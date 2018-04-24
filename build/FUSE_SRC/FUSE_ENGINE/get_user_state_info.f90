SUBROUTINE GET_USER_STATE_INFO(USER_STATE_NAMELIST,INIT_FLAG,USER_STATE_FILE,OUTPUT_STATE_FILE,STATE_OUT)

! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Andy Newman, 2018 
!
! ---------------------------------------------------------------------------------------
! Purpose:
! --------
!             Read a namelist that has information about initializing
!             a user specified state and outputting the model state
!             at a user specified time
!
! ---------------------------------------------------------------------------------------
USE nrtype                          ! variable types, etc.

IMPLICIT NONE

! arguments
! input
  character(len=5000),intent(in)         :: user_state_namelist            ! name of namelist file to be read

! output
  logical(lgt),    intent(out)        :: init_flag                      ! flag to check if user specified input is being used
  character(len=5000),intent(out)        :: user_state_file                ! name of user specified input model state file
  character(len=5000),intent(out)        :: output_state_file              ! name of user specified output model state file
  INTEGER(I4B),    intent(out)        :: state_out                      ! timestep to output model state at

! internal
  integer(I4B) :: ierr

  namelist / USER_STATE_INFO / user_state_file, output_state_file, state_out, init_flag

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
