module summaActors_initOutputStruct
  USE nrtype
  implicit none
  public::initalizeOutput
  contains

subroutine initalizeOutput(forcFileInfo, maxSteps, num_gru, err)
  USE globalData,only:outputStructure
  USE globalData,only:time_meta,forc_meta,attr_meta,type_meta ! metadata structures
  USE globalData,only:prog_meta,diag_meta,flux_meta,id_meta   ! metadata structures
  USE globalData,only:mpar_meta,indx_meta                     ! metadata structures
  USE globalData,only:bpar_meta,bvar_meta                     ! metadata structures
  USE globalData,only:statForc_meta                           ! child metadata for stats
  USE globalData,only:statProg_meta                           ! child metadata for stats
  USE globalData,only:statDiag_meta                           ! child metadata for stats
  USE globalData,only:statFlux_meta                           ! child metadata for stats
  USE globalData,only:statIndx_meta                           ! child metadata for stats
  USE globalData,only:statBvar_meta                           ! child metadata for stats
  USE globalData,only:gru_struc
  USE globalData,only:structInfo                              ! information on the data structures
  USE alloc_file_access,only:alloc_outputStruc
  USE multiconst,only:secprday               ! number of seconds in a day
  USE data_types,only:file_info_array
  USE var_lookup,only:maxvarFreq                ! maximum number of output files
  
  implicit none
  type(file_info_array), pointer        :: forcFileInfo
  integer(i4b), intent(in)              :: maxSteps
  integer(i4b), intent(in)              :: num_gru
  integer(i4b), intent(inout)           :: err 

  ! local variables
  integer(i4b)                          :: nVars
  integer(i4b)                          :: iGRU
  integer(i4b)                          :: iHRU
  integer(i4b)                          :: iStep
  integer(i4b)                          :: nSnow
  integer(i4b)                          :: nSoil
  integer(i4b)                          :: iStruct
  character(len=256)                    :: message
  integer(i4b)                          :: num_hru

  ! Allocate structure to hold output files
  if (.not.allocated(outputStructure))then
    allocate(outputStructure(1))
  end if


  ! Primary Data Structures (scalars)
  allocate(outputStructure(1)%mparStruct(1))
  allocate(outputStructure(1)%bparStruct(1))
  allocate(outputStructure(1)%dparStruct(1))
  allocate(outputStructure(1)%mparStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%bparStruct(1)%gru(num_gru))
  allocate(outputStructure(1)%dparStruct(1)%gru(num_gru))

  ! Finalize Stats for writing
  
  do iGRU = 1, num_gru
    num_hru = gru_struc(iGRU)%hruCount

    ! Primary Data Structures (scalars)
    allocate(outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(num_hru))
    allocate(outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(num_hru))

  end do

  do iGRU=1,num_gru
    do iHRU=1,gru_struc(iGRU)%hruCount

      ! Get the maximum number of steps needed to initalize the output structure
      nVars = maxval(forcFileInfo%ffile_list(:)%nVars)
      nSnow = gru_struc(iGRU)%hruInfo(iHRU)%nSnow
      nSoil = gru_struc(iGRU)%hruInfo(iHRU)%nSoil
      
      call alloc_outputStruc(attr_meta,outputStructure(1)%attrStruct(1)%gru(iGRU)%hru(iHRU), &
                  maxSteps,nSnow,nSoil,err,message);    ! local attributes for each HRU

      call alloc_outputStruc(type_meta,outputStructure(1)%typeStruct(1)%gru(iGRU)%hru(iHRU), &
                    maxSteps,nSnow,nSoil,err,message);    ! classification of soil veg etc.

      call alloc_outputStruc(id_meta,outputStructure(1)%idStruct(1)%gru(iGRU)%hru(iHRU), &
                    maxSteps,nSnow,nSoil,err,message);        ! local values of hru gru IDs

      call alloc_outputStruc(mpar_meta,outputStructure(1)%mparStruct(1)%gru(iGRU)%hru(iHRU), &
                    maxSteps,nSnow,nSoil,err,message); 

      call alloc_outputStruc(mpar_meta, outputStructure(1)%dparStruct(1)%gru(iGRU)%hru(iHRU), &
                    maxSteps,err=err,message=message)

      call alloc_outputStruc(bpar_meta,outputStructure(1)%bparStruct(1)%gru(iGRU), &
                    maxSteps,nSnow=0,nSoil=0,err=err,message=message);  ! basin-average params 
      ! check errors
      if(err/=0)then
        message=trim(message)//'initOutputStruc.f90 - [structure =  '//trim(structInfo(iStruct)%structName)//']'
        print*, "message"
        return
      endif
    end do ! Looping through GRUs
  end do


end subroutine initalizeOutput

end module