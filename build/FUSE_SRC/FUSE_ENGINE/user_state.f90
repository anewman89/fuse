module user_state
  use nrtype
  implicit none

  public

  logical(lgt)               :: init_flag                      ! flag to check if user specified input is being used
  logical(lgt)               :: state_out                      ! flag to check if FUSE state will be output at user specified date
  character(len=2000)        :: input_state_file               ! name of user specified input model state file
  character(len=2000)        :: output_state_file              ! name of user specified output model state file
  character(len=2000)        :: state_out_date                 ! date to output model state

  contains

  SUBROUTINE GET_USER_STATE_INFO(USER_STATE_NAMELIST)

    ! ---------------------------------------------------------------------------------------
    ! Creator:
    ! --------
    ! Andy Newman, 2018 
    !
    ! ---------------------------------------------------------------------------------------
    ! Purpose:
    ! --------
    !             Read a namelist that has information about initializing
    !              a user specified state and outputting the model state
    !             at a user specified time
    !
    ! ---------------------------------------------------------------------------------------
    USE nrtype                          ! variable types, etc.

    IMPLICIT NONE

    ! arguments
    ! input
    character(len=*),intent(in)         :: user_state_namelist            ! name of namelist file to be read

    ! internal
    integer(I4B) :: ierr

    namelist / USER_STATE_INFO / input_state_file, output_state_file, state_out, init_flag, state_out_date

    ! ---------------------------------------------------------------------------------------

    open(UNIT=30, file=trim(user_state_namelist),form="FORMATTED")

    read(UNIT=30, NML=USER_STATE_INFO, iostat=ierr)
    if (ierr /= 0) then
      write(*,'(/," ***** ERROR: Problem reading namelist USER_STATE_INFO",/)')
      rewind(UNIT=30)
      read(UNIT=30, NML=USER_STATE_INFO)
      stop " ***** ERROR: Problem reading namelist USER_STATE_INFO"
    endif

    close(UNIT=30)
    return

  END SUBROUTINE GET_USER_STATE_INFO

end module user_state
