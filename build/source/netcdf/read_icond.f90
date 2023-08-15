! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2020 NCAR/RAL; University of Saskatchewan; University of Washington
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module read_icond_actors_module
USE, intrinsic :: iso_c_binding
USE nrtype
USE netcdf
USE globalData,only: ixHRUfile_min,ixHRUfile_max
USE globalData,only: nTimeDelay   ! number of hours in the time delay histogram
implicit none
private
public::read_icond
public::read_icond_nlayers
! define single HRU restart file
integer(i4b), parameter :: singleHRU=1001
integer(i4b), parameter :: multiHRU=1002
integer(i4b), parameter :: restartFileType=multiHRU
contains

 ! ************************************************************************************************
 ! public subroutine read_icond_nlayers: read model initial conditions file for number of snow/soil layers
 ! ************************************************************************************************
subroutine read_icond_nlayers(iconFile,nGRU,indx_meta,err,message)
  ! --------------------------------------------------------------------------------------------------------
  ! modules
  USE nrtype
  USE var_lookup,only:iLookIndex                        ! variable lookup structure
  USE globalData,only:gru_struc                         ! gru-hru mapping structures
  USE netcdf_util_module,only:nc_file_close             ! close netcdf file
  USE netcdf_util_module,only:nc_file_open              ! close netcdf file
  USE netcdf_util_module,only:netcdf_err                ! netcdf error handling
  USE data_types,only:gru_hru_intVec                    ! actual data
  USE data_types,only:var_info                          ! metadata
  implicit none

  ! --------------------------------------------------------------------------------------------------------
  ! variable declarations
  ! dummies
  character(*)        ,intent(in)     :: iconFile       ! name of input (restart) file
  integer(i4b)        ,intent(in)     :: nGRU           ! total # of GRUs in run domain
  type(var_info)      ,intent(in)     :: indx_meta(:)   ! metadata
  integer(i4b)        ,intent(out)    :: err            ! error code
  character(*)        ,intent(out)    :: message        ! returned error message

  ! locals
  integer(i4b)                        :: ncID                       ! netcdf file id
  integer(i4b)                        :: dimID                      ! netcdf file dimension id
  integer(i4b)                        :: fileHRU                    ! number of HRUs in netcdf file
  integer(i4b)                        :: snowID, soilID             ! netcdf variable ids
  integer(i4b)                        :: iGRU, iHRU                 ! loop indexes
  integer(i4b)                        :: iHRU_global                ! index of HRU in the netcdf file
  integer(i4b),allocatable            :: snowData(:)                ! number of snow layers in all HRUs
  integer(i4b),allocatable            :: soilData(:)                ! number of soil layers in all HRUs  
  character(len=256)                  :: cmessage                   ! downstream error message

  ! --------------------------------------------------------------------------------------------------------
  ! initialize error message
  err=0
  message = 'read_icond_nlayers/'

  ! open netcdf file
  call nc_file_open(iconFile,nf90_nowrite,ncid,err,cmessage);
  if (err/=0) then; message=trim(message)//trim(cmessage); return; end if
  

  ! get number of HRUs in file (the GRU variable(s), if present, are processed at the end)
  err = nf90_inq_dimid(ncID,"hru",dimId);               if(err/=nf90_noerr)then; message=trim(message)//'problem finding hru dimension/'//trim(nf90_strerror(err)); return; end if
  err = nf90_inquire_dimension(ncID,dimId,len=fileHRU); if(err/=nf90_noerr)then; message=trim(message)//'problem reading hru dimension/'//trim(nf90_strerror(err)); return; end if

  ! allocate storage for reading from file (allocate entire file size, even when doing subdomain run)
  allocate(snowData(fileHRU))
  allocate(soilData(fileHRU))
  snowData = 0
  soilData = 0

  ! get netcdf ids for the variables holding number of snow and soil layers in each hru
  err = nf90_inq_varid(ncid,trim(indx_meta(iLookIndex%nSnow)%varName),snowid); call netcdf_err(err,message)
  err = nf90_inq_varid(ncid,trim(indx_meta(iLookIndex%nSoil)%varName),soilid); call netcdf_err(err,message)

  ! get nSnow and nSoil data (reads entire state file)
  err = nf90_get_var(ncid,snowid,snowData); call netcdf_err(err,message)
  err = nf90_get_var(ncid,soilid,soilData); call netcdf_err(err,message)

  ixHRUfile_min=huge(1)
  ixHRUfile_max=0
  ! find the min and max hru indices in the state file
  do iGRU = 1,nGRU
  do iHRU = 1,gru_struc(iGRU)%hruCount
  if(gru_struc(iGRU)%hruInfo(iHRU)%hru_nc < ixHRUfile_min) ixHRUfile_min = gru_struc(iGRU)%hruInfo(iHRU)%hru_nc
  if(gru_struc(iGRU)%hruInfo(iHRU)%hru_nc > ixHRUfile_max) ixHRUfile_max = gru_struc(iGRU)%hruInfo(iHRU)%hru_nc
  end do
  end do
  

  ! loop over grus in current run to update snow/soil layer information
  do iGRU = 1,nGRU
  do iHRU = 1,gru_struc(iGRU)%hruCount
  iHRU_global = gru_struc(iGRU)%hruInfo(iHRU)%hru_nc

  ! single HRU (Note: 'restartFileType' is hardwired above to multiHRU)
  if(restartFileType==singleHRU) then
      gru_struc(iGRU)%hruInfo(iHRU)%nSnow = snowData(1)
      gru_struc(iGRU)%hruInfo(iHRU)%nSoil = soilData(1)

  ! multi HRU
  else
      gru_struc(iGRU)%hruInfo(iHRU)%nSnow = snowData(iHRU_global)
      gru_struc(iGRU)%hruInfo(iHRU)%nSoil = soilData(iHRU_global)
  endif

  end do
  end do


  ! close file
  call nc_file_close(ncid,err,cmessage)
  if(err/=0)then;message=trim(message)//trim(cmessage);return;end if

  ! cleanup
  deallocate(snowData,soilData)

end subroutine read_icond_nlayers


! ************************************************************************************************
! public subroutine read_icond: read model initial conditions
! ************************************************************************************************
subroutine read_icond(&
                    indxGRU,                       & ! intent(in):    Index of GRU in gru_struc
                    indxHRU,                       & ! intent(in):    Index of HRU in gru_struc
                    mparData,                      & ! intent(in):    model parameters
                    progData,                      & ! intent(inout): model prognostic variables
                    bvarData,                      & ! intent(inout): model basin (GRU) variables
                    indxData,                      & ! intent(inout): model indices
                    err,message)                     ! intent(out):   error control
  ! --------------------------------------------------------------------------------------------------------
  ! modules
  USE nrtype
  USE var_lookup,only:iLookVarType                       ! variable lookup structure
  USE var_lookup,only:iLookPROG                          ! variable lookup structure
  USE var_lookup,only:iLookPARAM                         ! variable lookup structure
  USE var_lookup,only:iLookBVAR                          ! variable lookup structure
  USE var_lookup,only:iLookINDEX                         ! variable lookup structure
  USE globalData,only:prog_meta                          ! metadata for prognostic variables
  USE globalData,only:bvar_meta                          ! metadata for basin (GRU) variables
  USE globalData,only:gru_struc                          ! gru-hru mapping structures
  USE globaldata,only:iname_soil,iname_snow              ! named variables to describe the type of layer
  USE data_types,only:var_ilength                        ! full integer structure
  USE data_types,only:var_dlength                        ! double precision structure for a single HRU
  USE data_types,only:var_info                           ! metadata
  USE get_ixName_module,only:get_varTypeName             ! to access type strings for error messages
  USE updatState_module,only:updateSoil                  ! update soil states

  USE netcdf

  USE globalData,only:init_cond_prog
  USE globalData,only:init_cond_bvar


  implicit none

  ! --------------------------------------------------------------------------------------------------------
  ! variable declarations
  ! dummies
  integer(i4b)     ,intent(in)             :: indxGRU      ! index of GRU in gru_struc
  integer(i4b)     ,intent(in)             :: indxHRU      ! index of HRU in hru_struc
  type(var_dlength),intent(in)             :: mparData     ! model parameters
  type(var_dlength),intent(inout)          :: progData     ! model prognostic variables
  type(var_dlength),intent(inout)          :: bvarData     ! model basin (GRU) variables
  type(var_ilength),intent(inout)          :: indxData     ! model indices
  integer(i4b)     ,intent(out)            :: err          ! error code
  character(*)     ,intent(out)            :: message      ! returned error message

  ! locals
  character(len=256)                     :: cmessage     ! downstream error message
  integer(i4b)                           :: fileHRU      ! number of HRUs in file
  integer(i4b)                           :: fileGRU      ! number of GRUs in file
  integer(i4b)                           :: iVar, i      ! loop indices
  integer(i4b),dimension(1)              :: ndx          ! intermediate array of loop indices
  integer(i4b)                           :: dimID        ! varible dimension ids
  integer(i4b)                           :: ncVarID      ! variable ID in netcdf file
  character(256)                         :: dimName      ! not used except as a placeholder in call to inq_dim function
  integer(i4b)                           :: dimLen       ! data dimensions
  integer(i4b)                           :: ncID         ! netcdf file ID
  integer(i4b)                           :: ixFile       ! index in file
  integer(i4b)                           :: nSoil, nSnow, nToto ! # layers
  integer(i4b)                           :: nTDH          ! number of points in time-delay histogram
  integer(i4b)                           :: iLayer,jLayer ! layer indices
  integer(i4b),parameter                 :: nBand=2       ! number of spectral bands

  character(len=32),parameter            :: scalDimName   ='scalarv'  ! dimension name for scalar data
  character(len=32),parameter            :: midSoilDimName='midSoil'  ! dimension name for soil-only layers
  character(len=32),parameter            :: midTotoDimName='midToto'  ! dimension name for layered varaiables
  character(len=32),parameter            :: ifcTotoDimName='ifcToto'  ! dimension name for layered varaiables
  character(len=32),parameter            :: tdhDimName    ='tdh'      ! dimension name for time-delay basin variables

  ! --------------------------------------------------------------------------------------------------------

  ! Start procedure here
  err=0; message="read_icondActors.f90 - read_icond/"

  ! loop through prognostic variables
  do iVar = 1,size(prog_meta)

    ! skip variables that are computed later
    if(prog_meta(iVar)%varName=='scalarCanopyWat'          .or. &
      prog_meta(iVar)%varName=='spectralSnowAlbedoDiffuse' .or. &
      prog_meta(iVar)%varName=='scalarSurfaceTemp'         .or. &
      prog_meta(iVar)%varName=='mLayerVolFracWat'          .or. &
      prog_meta(iVar)%varName=='mLayerHeight'                   ) cycle


    ! get the number of layers
    nSnow = gru_struc(indxGRU)%hruInfo(indxHRU)%nSnow
    nSoil = gru_struc(indxGRU)%hruInfo(indxHRU)%nSoil
    nToto = nSnow + nSoil

    ! put the data into data structures and check that none of the values are set to nf90_fill_double
    select case (prog_meta(iVar)%varType)
      case (iLookVarType%scalarv)
        progData%var(iVar)%dat(1)       = init_cond_prog(iVar)%var_data(indxGRU,1)
        if(abs(progData%var(iVar)%dat(1) - nf90_fill_double) < epsilon(init_cond_prog(iVar)%var_data))then; err=20; endif
      case (iLookVarType%midSoil)
        progData%var(iVar)%dat(1:nSoil) = init_cond_prog(iVar)%var_data(indxGRU,1:nSoil)
        if(any(abs(progData%var(iVar)%dat(1:nSoil) - nf90_fill_double) < epsilon(init_cond_prog(iVar)%var_data)))then; err=20; endif
      case (iLookVarType%midToto)
        progData%var(iVar)%dat(1:nToto) = init_cond_prog(iVar)%var_data(indxGRU,1:nToto)
        if(any(abs(progData%var(iVar)%dat(1:nToto) - nf90_fill_double) < epsilon(init_cond_prog(iVar)%var_data)))then; err=20; endif
      case (iLookVarType%ifcToto)
        progData%var(iVar)%dat(0:nToto) = init_cond_prog(iVar)%var_data(indxGRU,1:nToto+1)
        if(any(abs(progData%var(iVar)%dat(0:nToto) - nf90_fill_double) < epsilon(init_cond_prog(iVar)%var_data)))then; err=20; endif
      case default
        message=trim(message)//"unexpectedVariableType[name='"//trim(prog_meta(iVar)%varName)//"';type='"//trim(get_varTypeName(prog_meta(iVar)%varType))//"']"
        print*,message
        err=20; return
    end select

    if(err==20)then; 
      message=trim(message)//"data set to the fill value (name='"//trim(prog_meta(iVar)%varName)//"')"; 
      print*, message
      return;
    endif

    ! fix the snow albedo
    if(progData%var(iLookPROG%scalarSnowAlbedo)%dat(1) < 0._dp)then
      progData%var(iLookPROG%scalarSnowAlbedo)%dat(1) = mparData%var(iLookPARAM%albedoMax)%dat(1)
    endif

    ! make sure canopy water is positive
    if(progData%var(iLookPROG%scalarCanopyWat)%dat(1) < 0.0001_rkind)then
      progData%var(iLookPROG%scalarCanopyliq)%dat(1) = 0.0001_rkind
    endif

    ! initialize the spectral albedo
    progData%var(iLookPROG%spectralSnowAlbedoDiffuse)%dat(1:nBand) = progData%var(iLookPROG%scalarSnowAlbedo)%dat(1)

  end do ! end looping through prognostic variables (iVar)

  ! --------------------------------------------------------------------------------------------------------
  ! (2) set number of layers
  ! --------------------------------------------------------------------------------------------------------

  ! save the number of layers
  indxData%var(iLookINDEX%nSnow)%dat(1)   = gru_struc(indxGRU)%hruInfo(indxHRU)%nSnow
  indxData%var(iLookINDEX%nSoil)%dat(1)   = gru_struc(indxGRU)%hruInfo(indxHRU)%nSoil
  indxData%var(iLookINDEX%nLayers)%dat(1) = gru_struc(indxGRU)%hruInfo(indxHRU)%nSnow + gru_struc(indxGRU)%hruInfo(indxHRU)%nSoil

  ! set layer type
  indxData%var(iLookINDEX%layerType)%dat(1:gru_struc(indxGRU)%hruInfo(indxHRU)%nSnow) = iname_snow
  indxData%var(iLookINDEX%layerType)%dat((gru_struc(indxGRU)%hruInfo(indxHRU)%nSnow+1):(gru_struc(indxGRU)%hruInfo(indxHRU)%nSnow+gru_struc(indxGRU)%hruInfo(indxHRU)%nSoil)) = iname_soil


  ! --------------------------------------------------------------------------------------------------------
  ! (3) update soil layers (diagnostic variables)
  ! --------------------------------------------------------------------------------------------------------
  ! loop through soil layers
  do iLayer = 1,indxData%var(iLookINDEX%nSoil)%dat(1)

    ! get layer in the total vector
    jLayer = iLayer+indxData%var(iLookINDEX%nSnow)%dat(1)

    ! update soil layers
    call updateSoil(&
                    ! input
                    progData%var(iLookPROG%mLayerTemp          )%dat(jLayer),& ! intent(in): temperature vector (K)
                    progData%var(iLookPROG%mLayerMatricHead    )%dat(iLayer),& ! intent(in): matric head (m)
                    mparData%var(iLookPARAM%vGn_alpha          )%dat(iLayer),& ! intent(in): van Genutchen "alpha" parameter
                    mparData%var(iLookPARAM%vGn_n              )%dat(iLayer),& ! intent(in): van Genutchen "n" parameter
                    mparData%var(iLookPARAM%theta_sat          )%dat(iLayer),& ! intent(in): soil porosity (-)
                    mparData%var(iLookPARAM%theta_res          )%dat(iLayer),& ! intent(in): soil residual volumetric water content (-)
                    1._dp - 1._dp/mparData%var(iLookPARAM%vGn_n)%dat(iLayer),& ! intent(in): van Genutchen "m" parameter (-)
                    ! output
                    progData%var(iLookPROG%mLayerVolFracWat    )%dat(jLayer),& ! intent(out): volumetric fraction of total water (-)
                    progData%var(iLookPROG%mLayerVolFracLiq    )%dat(jLayer),& ! intent(out): volumetric fraction of liquid water (-)
                    progData%var(iLookPROG%mLayerVolFracIce    )%dat(jLayer),& ! intent(out): volumetric fraction of ice (-)
                    err,message)                                                                   ! intent(out): error control
    if (err/=0) then; message=trim(message)//trim(cmessage); return; end if

  end do  ! looping through soil layers

  ! --------------------------------------------------------------------------------------------------------
  ! (2) now get the basin variable(s)
  ! --------------------------------------------------------------------------------------------------------

  ! get the index in the file: single HRU
  if(allocated(init_cond_bvar))then

    ! loop through specific basin variables (currently 1 but loop provided to enable inclusion of others)
    ndx = (/iLookBVAR%routingRunoffFuture/)   ! array of desired variable indices
    do i = 1,size(ndx)
      iVar = ndx(i)

      ! store data in basin var (bvar) structure

      ! put the data into data structures
      bvarData%var(iVar)%dat(1:nTDH) = init_cond_bvar(i)%var_data(indxGRU,1:nTDH)
      ! check whether the first values is set to nf90_fill_double
      if(any(abs(bvarData%var(iVar)%dat(1:nTDH) - nf90_fill_double) < epsilon(init_cond_bvar(i)%var_data)))then; err=20; endif
      if(err==20)then; message=trim(message)//"data set to the fill value (name='"//trim(bvar_meta(iVar)%varName)//"')"; return; endif

    end do ! end looping through basin variables
  endif  ! end if case for not being a singleHRU run - gaurded by the structure not being allocated


end subroutine read_icond

end module read_icond_actors_module
