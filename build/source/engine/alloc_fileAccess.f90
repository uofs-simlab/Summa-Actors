module alloc_outputStructure
USE nrtype
USE data_types,only:var_time_dlength
USE data_types,only:var_time_ilength
USE data_types,only:var_time_i
USE data_types,only:var_time_d
USE data_types,only:var_time_i8
USE data_types,only:var_i8
USE data_types,only:var_d
USE data_types,only:var_i
USE data_types,only:var_dlength
USE data_types,only:var_info
USE globalData,only:nBand                 ! number of spectral bands
USE globalData,only:nTimeDelay            ! number of timesteps in the time delay histogram
USE var_lookup,only:maxvarFreq             ! allocation dimension (output frequency)
USE var_lookup,only:iLookVarType           ! look up structure for variable typed


implicit none
private
public::alloc_outputStruc
public::allocateDat_rkind
public::allocateDat_int
! public::allocateDat_flag
contains

subroutine alloc_outputStruc(metaStruct,dataStruct,nSteps,nSnow,nSoil,err,message)
  implicit none
  type(var_info),intent(in)        :: metaStruct(:)
  class(*),intent(inout)           :: dataStruct
  ! optional input
  integer(i4b),intent(in),optional :: nSteps
  integer(i4b),intent(in),optional :: nSnow          ! number of snow layers
  integer(i4b),intent(in),optional :: nSoil          ! number of soil layers
  ! output
  integer(i4b),intent(inout)       :: err            ! error code
  character(*),intent(out)         :: message        ! error message
  ! local
  logical(lgt)                     :: check          ! .true. if the variables are allocated
  integer(i4b)                     :: nVars          ! number of variables in the metadata structure
  integer(i4b)                     :: nLayers        ! total number of layers
  integer(i4b)                     :: iVar
  character(len=256)               :: cmessage       ! error message of the downwind routine
  ! initalize error control
  message='alloc_outputStruc'

  nVars = size(metaStruct)
    if(present(nSnow) .or. present(nSoil))then
        ! check both are present
        if(.not.present(nSoil))then; err=20; message=trim(message)//'expect nSoil to be present when nSnow is present'; return; end if
        if(.not.present(nSnow))then; err=20; message=trim(message)//'expect nSnow to be present when nSoil is present'; return; end if
        nLayers = nSnow+nSoil
    
        ! It is possible that nSnow and nSoil are actually needed here, so we return an error if the optional arguments are missing when needed
    else
        select type(dataStruct)
            ! class is (var_flagVec); err=20
            class is (var_time_ilength); err=20
            class is (var_time_dlength); err=20
        end select
        if(err/=0)then; message=trim(message)//'expect nSnow and nSoil to be present for variable-length data structures'; return; end if
    end if

    check=.false.
    ! allocate the dimension for model variables
    select type(dataStruct)

        class is (var_time_i)
            if(allocated(dataStruct%var))then
                check=.true.
            else 
                allocate(dataStruct%var(nVars),stat=err)
            end if
            
            do iVar=1, nVars
                allocate(dataStruct%var(iVar)%tim(nSteps))
            end do
            return

        class is (var_time_i8)
            if(allocated(dataStruct%var))then 
                check=.true.
            else 
                allocate(dataStruct%var(nVars),stat=err) 
            end if 
            do iVar=1, nVars
                allocate(dataStruct%var(iVar)%tim(nSteps))
            end do
            return

        class is (var_time_d)
            if(allocated(dataStruct%var))then
                check=.true.
            else
                allocate(dataStruct%var(nVars),stat=err)
            end if
            do iVar=1, nVars
                allocate(dataStruct%var(iVar)%tim(nSteps))
            end do
            return
        
        class is (var_d)
            if(allocated(dataStruct%var))then
                check=.true.
            else
                allocate(dataStruct%var(nVars),stat=err)
            end if
            return
        
        class is (var_i)
            if(allocated(dataStruct%var))then
                check=.true.
            else
                allocate(dataStruct%var(nVars),stat=err)
            end if
            return
        
        class is (var_i8)
            if(allocated(dataStruct%var))then
                check=.true.
            else
                allocate(dataStruct%var(nVars), stat=err)
            end if
            return
        
        class is (var_dlength)
            if(allocated(dataStruct%var))then
                check=.true.
            else
                allocate(dataStruct%var(nVars),stat=err)
            end if
        ! class is (var_flagVec);      if(allocated(dataStruct%var))then; check=.true.; else; allocate(dataStruct%var(nVars),stat=err); end if

        class is (var_time_ilength)
            if(allocated(dataStruct%var))then
                check=.true. 
            else 
                allocate(dataStruct%var(nVars),stat=err) 
            end if
            do iVar=1, nVars
                allocate(dataStruct%var(iVar)%tim(nSteps))
            end do

        class is (var_time_dlength)
            if(allocated(dataStruct%var))then
                check=.true.
            else 
                allocate(dataStruct%var(nVars),stat=err)
            end if
            do iVar=1, nVars
                allocate(dataStruct%var(iVar)%tim(nSteps))
            end do
      
      class default; err=20; message=trim(message)//'unable to identify derived data type for the variable dimension'; return
  end select
  ! check errors
  if(check) then; err=20; message=trim(message)//'structure was unexpectedly allocated already'; return; end if
  if(err/=0)then; err=20; message=trim(message)//'problem allocating'; return; end if

  ! allocate the dimension for model data
  select type(dataStruct)
      ! class is (var_flagVec); call allocateDat_flag(metaStruct,nSnow,nSoil,nLayers,dataStruct,err,cmessage)
      class is (var_time_ilength); call allocateDat_int(metaStruct,dataStruct,nSnow,nSoil,nSteps,err,cmessage)
      class is (var_time_dlength); call allocateDat_rkind_nSteps(metaStruct,dataStruct,nSnow,nSoil,nSteps,err,cmessage)
      class is (var_dlength);      call allocateDat_rkind(metaStruct,dataStruct,nSnow,nSoil,err,cmessage)
      class default; err=20; message=trim(message)//'unable to identify derived data type for the data dimension'; return
  end select

  ! check errors
  if(err/=0)then; message=trim(message)//trim(cmessage); return; end if



end subroutine


subroutine allocateDat_rkind_nSteps(metadata,varData,nSnow, nSoil, &
                                  nSteps,err,message)
              
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages
  
  implicit none
  type(var_info),intent(in)            :: metadata(:)
  ! output variables
  type(var_time_dlength),intent(inout)                :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)              :: nSteps
  integer(i4b),intent(in)              :: nSnow
  integer(i4b),intent(in)              :: nSoil
  
  integer(i4b),intent(inout)          :: err         ! error code
  character(*),intent(inout)          :: message     ! error message
  
  ! local variables
  integer(i4b)                        :: iStep 
  integer(i4b)                        :: nVars
  integer(i4b)                        :: iVar
  integer(i4b)                        :: nLayers
  message='allocateDat_rkindAccessActor'

  nVars = size(metaData)
  nLayers = nSnow+nSoil
  do iStep=1, nSteps
      do iVar=1, nVars
        select case(metadata(iVar)%vartype)
          case(iLookVarType%scalarv); allocate(varData%var(iVar)%tim(iStep)%dat(1),stat=err)
          case(iLookVarType%wLength); allocate(varData%var(iVar)%tim(iStep)%dat(nBand),stat=err)
          case(iLookVarType%midSnow); allocate(varData%var(iVar)%tim(iStep)%dat(nSnow),stat=err)
          case(iLookVarType%midSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
          case(iLookVarType%midToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers),stat=err)
          case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSnow),stat=err)
          case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSoil),stat=err)
          case(iLookVarType%ifcToto); allocate(varData%var(iVar)%tim(iStep)%dat(0:nLayers),stat=err)
          case(iLookVarType%parSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
          case(iLookVarType%routing); allocate(varData%var(iVar)%tim(iStep)%dat(nTimeDelay),stat=err)
          case(iLookVarType%outstat); allocate(varData%var(iVar)%tim(iStep)%dat(maxvarfreq*2),stat=err)
          case(iLookVarType%unknown); allocate(varData%var(iVar)%tim(iStep)%dat(0),stat=err)
          case default
              err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
              return
        end select
      end do
  end do

end subroutine allocateDat_rkind_nSteps

subroutine allocateDat_rkind(metadata,varData,nSnow,nSoil,err,message)

  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages
  
  implicit none
  type(var_info),intent(in)            :: metadata(:)
  ! output variables
  type(var_dlength),intent(inout)      :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)              :: nSnow
  integer(i4b),intent(in)              :: nSoil
  
  integer(i4b),intent(inout)          :: err         ! error code
  character(*),intent(inout)          :: message     ! error message
  
  ! local variables
  integer(i4b)                        :: nVars
  integer(i4b)                        :: iVar
  integer(i4b)                        :: nLayers
  message='allocateDat_rkindAccessActor'

  nVars = size(metaData)
  nLayers = nSnow+nSoil
  do iVar=1, nVars
      select case(metadata(iVar)%vartype)
      case(iLookVarType%scalarv); allocate(varData%var(iVar)%dat(1),stat=err)
      case(iLookVarType%wLength); allocate(varData%var(iVar)%dat(nBand),stat=err)
      case(iLookVarType%midSnow); allocate(varData%var(iVar)%dat(nSnow),stat=err)
      case(iLookVarType%midSoil); allocate(varData%var(iVar)%dat(nSoil),stat=err)
      case(iLookVarType%midToto); allocate(varData%var(iVar)%dat(nLayers),stat=err)
      case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%dat(0:nSnow),stat=err)
      case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%dat(0:nSoil),stat=err)
      case(iLookVarType%ifcToto); allocate(varData%var(iVar)%dat(0:nLayers),stat=err)
      case(iLookVarType%parSoil); allocate(varData%var(iVar)%dat(nSoil),stat=err)
      case(iLookVarType%routing); allocate(varData%var(iVar)%dat(nTimeDelay),stat=err)
      case(iLookVarType%outstat); allocate(varData%var(iVar)%dat(maxvarfreq*2),stat=err)
      case(iLookVarType%unknown); allocate(varData%var(iVar)%dat(0),stat=err)
      case default
          err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
          return
      end select
  end do

end subroutine allocateDat_rkind

subroutine allocateDat_int(metadata,varData,nSnow, nSoil, &
                          nSteps,err,message)
  USE get_ixName_module,only:get_varTypeName       ! to access type strings for error messages

  implicit none
  type(var_info),intent(in)         :: metadata(:)
  ! output variables
  type(var_time_ilength),intent(inout) :: varData     ! model variables for a local HRU
  integer(i4b),intent(in)           :: nSteps
  integer(i4b),intent(in)           :: nSnow
  integer(i4b),intent(in)           :: nSoil
  
  integer(i4b),intent(inout)          :: err         ! error code
  character(*),intent(inout)          :: message     ! error message
  
  ! local variables
  integer(i4b)                      :: iStep 
  integer(i4b)                      :: nVars
  integer(i4b)                      :: iVar
  integer(i4b)                      :: nLayers
  message='allocateDat_rkindAccessActor'
  
  nVars = size(metaData)
  nLayers = nSnow+nSoil
  do iStep=1, nSteps
      do iVar=1, nVars
          select case(metadata(iVar)%vartype)
          case(iLookVarType%scalarv); allocate(varData%var(iVar)%tim(iStep)%dat(1),stat=err)
          case(iLookVarType%wLength); allocate(varData%var(iVar)%tim(iStep)%dat(nBand),stat=err)
          case(iLookVarType%midSnow); allocate(varData%var(iVar)%tim(iStep)%dat(nSnow),stat=err)
          case(iLookVarType%midSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
          case(iLookVarType%midToto); allocate(varData%var(iVar)%tim(iStep)%dat(nLayers),stat=err)
          case(iLookVarType%ifcSnow); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSnow),stat=err)
          case(iLookVarType%ifcSoil); allocate(varData%var(iVar)%tim(iStep)%dat(0:nSoil),stat=err)
          case(iLookVarType%ifcToto); allocate(varData%var(iVar)%tim(iStep)%dat(0:nLayers),stat=err)
          case(iLookVarType%parSoil); allocate(varData%var(iVar)%tim(iStep)%dat(nSoil),stat=err)
          case(iLookVarType%routing); allocate(varData%var(iVar)%tim(iStep)%dat(nTimeDelay),stat=err)
          case(iLookVarType%outstat); allocate(varData%var(iVar)%tim(iStep)%dat(maxvarfreq*2),stat=err)
          case(iLookVarType%unknown); allocate(varData%var(iVar)%tim(iStep)%dat(0),stat=err)
          case default
              err=40; message=trim(message)//"1. unknownVariableType[name='"//trim(metadata(iVar)%varname)//"'; type='"//trim(get_varTypeName(metadata(iVar)%vartype))//"']"
              return
          end select
      end do
  end do


end subroutine




end module alloc_outputStructure