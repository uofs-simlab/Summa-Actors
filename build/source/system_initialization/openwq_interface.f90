module openwq_actor
    USE, intrinsic :: iso_c_binding

    implicit none
  public::defineGlobalData_openWQ_fortran
  public::openwq_start_step_fortran
  public::openwq_space_step_fortran
  public::openwq_space_step_end_fortran

  contains

subroutine defineGlobalData_openWQ_fortran(err, message_r) bind(C, name="defineGlobalData_openWQ_fortran")
    USE C_interface_module
    USE summa_openwq,only:openwq_init
    implicit none
    ! dummy variables
    integer(c_int),intent(out)                :: err
    type(c_ptr),intent(out)                   :: message_r ! message to return to the caller
    ! local variables
    character(len=256)                        :: message = "" 
  
    ! Convert strings
    call f_c_string_ptr(trim(message), message_r)
    print*, '21\n'
  
    ! Define global data
    call openwq_init(err)
    if (err /=0) then; message = "problem allocating openWQ progStruct for saving state information"; endif
    if (err /= 0) then; call f_c_string_ptr(trim(message), message_r); return; endif
  
  end subroutine defineGlobalData_openWQ_fortran

subroutine openwq_start_step_fortran(timestep) bind(C, name="openwq_start_step_fortran")
    USE C_interface_module
    USE summa_openwq,only:openwq_run_time_start

    integer(c_int), intent(in) :: timestep

    print*, timestep
    call openwq_run_time_start(timestep)

    end subroutine openwq_start_step_fortran

  !subroutine openwq_run_time_start_fortran()
  
    subroutine openwq_space_step_fortran(timestep) bind(C, name="openwq_space_step_fortran")
        USE C_interface_module
        USE summa_openwq,only:openwq_run_space_step
    
        integer(c_int), intent(in) :: timestep
    
        print*, timestep
        call openwq_run_space_step(timestep)
    
        end subroutine openwq_space_step_fortran
    
        subroutine openwq_space_step_end_fortran(timestep) bind(C, name="openwq_space_step_end_fortran")
            USE C_interface_module
            USE summa_openwq,only:openwq_run_time_end
        
            integer(c_int), intent(in) :: timestep
        
            print*, timestep
            call openwq_run_time_end(timestep)
        
            end subroutine openwq_space_step_end_fortran
  
end module