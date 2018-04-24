SUBROUTINE OUTPUT_STATE(OUTPUT_STATE_NAME)
! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Andy Newman, 2018 - essentially a copy of M. Clark INIT_STATE
!   Martyn Clark, 2007
!   Modified by Brian Henn to include snow model, 6/2013
! ---------------------------------------------------------------------------------------
! Purpose:
! --------
! Output model states using a user defined file
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
CHARACTER(LEN=*), INTENT(IN)          :: OUTPUT_STATE_NAME   ! name of user specified state initialization file

! internal
INTEGER(I4B)                          :: ISNW                ! snow band index
INTEGER(I4B)                          :: ERR                 ! error integer
INTEGER(I4B)                          :: IR                  ! routed runoff index

! ---------------------------------------------------------------------------------------

!WRITE user state file
OPEN(33,FILE=TRIM(OUTPUT_STATE_NAME),ACTION='WRITE',ACCESS='SEQUENTIAL',IOSTAT=ERR)
IF(ERR /= 0) THEN
  print *,'OPEN ERROR: ', ERR
  stop
END IF

! (upper layer)

WRITE(33,*,IOSTAT=ERR) 'tension-water-layer-1A', FSTATE%TENS_1A
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' TENS_1A'
  stop 
END IF

WRITE(33,*,IOSTAT=ERR) 'tension-water-layer-1B', FSTATE%TENS_1B
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' TENS_1B'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'tension-water-layer-1', FSTATE%TENS_1
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' TENS_1'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'free-water-layer-1', FSTATE%FREE_1
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' FREE_1'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'total-water-layer-1', FSTATE%WATR_1
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' WATR_1'
  stop
END IF

! (lower layer)
WRITE(33,*,IOSTAT=ERR) 'tension-water-layer-2', FSTATE%TENS_2
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' TENS_2'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'free-water-layer-2', FSTATE%FREE_2
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' FREE_2'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'free-water-layer-2A', FSTATE%FREE_2A
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' FREE_2A'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'free-water-layer-2B', FSTATE%FREE_2B
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' FREE_2B'
  stop
END IF

WRITE(33,*,IOSTAT=ERR) 'total-water-layer-2', FSTATE%WATR_2
IF(ERR /= 0) THEN
  print *,'WRITE ERROR: ', ERR, ' WATR_2'
  stop
END IF

! snow model
DO ISNW=1,N_BANDS
  WRITE(33,*,IOSTAT=ERR) 'snow-water-equivalent',MBANDS(ISNW)%SWE
  IF(ERR /= 0) THEN
    print *,'WRITE ERROR: ', ERR, ' Snow band: ',ISNW
    stop
  END IF
END DO

! (routed runoff)
DO IR=1,size(FUTURE,1)
  WRITE(33,*,IOSTAT=ERR) 'routed-runoff', FUTURE(IR)
  IF(ERR /= 0) THEN
    print *,'WRITE ERROR: ', ERR, ' Routed runoff, index: ',IR
    stop
  END IF
END DO

! ---------------------------------------------------------------------------------------
END SUBROUTINE OUTPUT_STATE
