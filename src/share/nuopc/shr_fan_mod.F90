!================================================================================
module shr_fan_mod

  !================================================================================
  ! Namelist for handling of FAN (Flow of Agricultureal Nitrogen) for nitrogen
  ! fields that will be exchanged between the land surface and atmosphere
  !================================================================================
  use shr_kind_mod,only : r8 => shr_kind_r8
  use shr_kind_mod,only : CL => SHR_KIND_CL, CX => SHR_KIND_CX, CS => SHR_KIND_CS
  use shr_sys_mod, only : shr_sys_abort
  use shr_log_mod, only : loglev  => shr_log_Level
  use shr_log_mod, only : logunit => shr_log_Unit
  use shr_mpi_mod,    only : shr_mpi_bcast

  implicit none
  private

  public shr_fan_readnl   ! Reads the fan_inparm namelist

  logical, save, public :: shr_fan_to_atm = .false.             ! If nitrogen from FAN will go to the atmosphere
  character(len=CS), save, public :: shr_fan_fields_token = ''  ! Specific fields to exchange

!-------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------

  subroutine shr_fan_readnl(nlfilename, fan_fields, have_fields)
    !-------------------------------------------------------------------------
    !
    ! This reads the fan_inparm namelist group in drv_flds_in and parses the
    ! namelist information for the driver, CTSM, and CAM.
    !
    ! Namelist variables:
    !         fan_nh3_to_atm
    !  fan_nh3_to_atm (logical) -- if FAN fields will be sent to the atmosphere
    !
    !-------------------------------------------------------------------------
    use shr_mpi_mod, only : shr_mpi_bcast
    use shr_nl_mod   , only : shr_nl_find_group_name
    use ESMF, only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet

    ! input/output variables
    character(len=*), intent(in)  :: nlfilename
    character(len=*), intent(out) :: fan_fields
    logical, intent(out) :: have_fields
    
    ! local variables
    type(ESMF_VM) :: vm
    integer :: localPet
    integer :: mpicomm, iostat, fileunit, rc
    logical :: exists, fan_nh3_to_atm
    character(*),parameter :: subname = '(shr_fan_reanl) '
    !------------------------------------------------------------------

    namelist /fan_inparm/ fan_nh3_to_atm
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localpet=localpet, mpiCommunicator=mpicomm, rc=rc)
    if (localpet==0) then
       inquire(file=trim(nlfilename), exist=exists)
       if (exists) then
          open(newunit=fileunit, file=trim(nlfilename), status='old' )
          call shr_nl_find_group_name(fileunit, 'fan_inparm', iostat)
          if (iostat /= 0) then
             write(logunit, *) subname, 'FAN/CAM coupling not specified'
             fan_nh3_to_atm = .false.
             !call shr_sys_abort(subName//'Error reading namelist')
          else          
             read(fileunit, fan_inparm, iostat=iostat)
             if (iostat /= 0) then
                call shr_sys_abort(subName//'Error reading namelist')
             end if
          end if
          close(fileunit)
       end if
    
    end if ! root
    call shr_mpi_bcast(fan_nh3_to_atm, mpicomm)
    have_fields = fan_nh3_to_atm
    if (fan_nh3_to_atm) then
       fan_fields = 'Fall_FANNH3'
    else
       fan_fields = ''
    end if
    shr_fan_to_atm = have_fields
    shr_fan_fields_token = fan_fields
    
  end subroutine shr_fan_readnl

  !-------------------------------------------------------------------------
  
endmodule shr_fan_mod
