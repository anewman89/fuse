SUBROUTINE INIT_USER_STATE(USER_FILE_NAME)
! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Andy Newman, 2018 - essentially a copy of M. Clark INIT_STATE
!   Martyn Clark, 2007
!   Modified by Brian Henn to include snow model, 6/2013
! ---------------------------------------------------------------------------------------
! Purpose:
! --------
! Initialize model states using a user defined file
! ---------------------------------------------------------------------------------------
! Modules Modified:
! -----------------
! Model states in MODULE multistate
! ---------------------------------------------------------------------------------------
USE multiparam                                          ! model parameters
USE multistate                                          ! model states
USE multibands                                          ! model snow bands
USE multiroute                                          ! routed runoff
IMPLICIT NONE

! arguments
CHARACTER(LEN=*), INTENT(IN)   :: USER_FILE_NAME ! name of user specified state initialization file

! internal
CHARACTER(LEN=100)                    :: dummy_string   ! used for temporary data storage
INTEGER(I4B)                          :: ISNW           ! snow band index
INTEGER(I4B)                          :: ERR            ! error integer
INTEGER(I4B)                          :: IR             ! index for routed flow

! ---------------------------------------------------------------------------------------

!open user state file
OPEN(33,FILE=TRIM(USER_FILE_NAME),STATUS='OLD',ACTION='READ',IOSTAT=ERR)
if(ERR /= 0) then
  print *,'OPEN ERROR: ', ERR
  stop
end if

! (upper layer)
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%TENS_1A
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' TENS_1A'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%TENS_1B
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' TENS_1B'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%TENS_1
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' TENS_1'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%FREE_1
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' FREE_1'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%WATR_1
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' WATR_1'
  stop
end if

! (lower layer)
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%TENS_2
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' TENS_2'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%FREE_2
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' FREE_2'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%FREE_2A
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' FREE_2A'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%FREE_2B
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' FREE_2B'
  stop
end if
READ(33,*,IOSTAT=ERR) dummy_string, FSTATE%WATR_2
if(ERR /= 0) then
  print *,'READ ERROR: ', ERR, ' WATR_2'
  stop
end if

! snow model
DO ISNW=1,N_BANDS
  READ(33,*,IOSTAT=ERR) dummy_string,MBANDS(ISNW)%SWE
  if(ERR /= 0) then
    print *,'READ ERROR: ', ERR, ' Snow Band: ',ISNW
    stop
  end if

END DO

! (routed runoff)
!READ(33,*,IOSTAT=ERR) dummy_string
!READ(33,*,IOSTAT=ERR) dummy_string, FUTURE
!if(ERR /= 0) then
!  print *,'READ ERROR: ', ERR, ' Routed Runoff'
!  stop
!end if

do IR=1,size(FUTURE)
  READ(33,*,IOSTAT=ERR) dummy_string,FUTURE(IR)
  if(ERR /= 0) then
    print *,'READ ERROR: ', ERR, ' Routed Runoff, index: ', IR
    stop
  end if
end do 


!FUTURE         = 0._sp
! ---------------------------------------------------------------------------------------
END SUBROUTINE INIT_USER_STATE
