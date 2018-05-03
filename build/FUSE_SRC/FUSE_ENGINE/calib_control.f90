module calib_control

  use nrtype
  implicit none

  public

  integer(I4B)        ::   MAXN = 10000               ! maximum number of trials before optimization is terminated
  integer(I4B)        ::   KSTOP = 3                  ! number of shuffling loops the value must change by PCENTO (MAX=9)
  REAL(MSP)           ::   PCENTO = 0.001             ! the percentage
  integer(I4B)        ::   NGS = 7                    ! number of complexes in the initial population
  integer(I4B)        ::   MINGS = 2                  ! minimum number of complexes required
  integer(I4B)        ::   INIFLG = 1                 ! 1 = include initial point in the population
  integer(I4B)        ::   IPRINT = 1                 ! 0 = supress printing
  integer(I4B)        ::   ISEED = 1                  ! integer used in start of initial random SCE parameter value draw

  character(len=100)  ::   calib_type = "daily"       ! calibration type - compute metrics on daily series or series of maximums in a specified interval
  character(len=100)  ::   calib_metric = "rmse"      ! calibration metric
  integer(I4B)        ::   smooth_window = 1          ! window to perform smoothing over
  integer(I4B)        ::   interval_window = 365      ! window for generating series of maximum values
  real(SP)            ::   kge_a_corr = 1.            ! prefactor to weight correlation in kge
  real(SP)            ::   kge_a_alpha = 1.           ! prefactor to weight variance bias(alpha) in kge
  real(SP)            ::   kge_a_beta = 1.            ! prefactor to weight volume bias(beta) in kge

  character(len=52)   ::   obj_func_desc              ! description of objective function
  character(len=13)   ::   obj_func_unit              ! objective function units
  
  contains

  subroutine read_calib_control()

    use nrtype
    use fuse_fileManager

    implicit none

    !local variables
    character(len=5000)   :: calib_file   ! full calibration control path
    integer(I4B)          :: ierr         ! error variable

    namelist / SCE_CONTROL / MAXN, KSTOP, PCENTO, NGS, MINGS, INIFLG, IPRINT, ISEED

    namelist / METRIC_CONTROL / calib_metric, interval_window, calib_type, smooth_window, &
                                kge_a_corr, kge_a_alpha, kge_a_beta

! ---------------------------------------------------------------------------------------

    calib_file = trim(setngs_path)//trim(calib_nmlist)

    open(UNIT=30, file=trim(calib_file),form="FORMATTED")

    read(UNIT=30, NML=SCE_CONTROL, iostat=ierr)
    if (ierr /= 0) then
      write(*,'(/," ***** ERROR: Problem reading namelist SCE_CONTROL",/)')
      rewind(UNIT=30)
      read(UNIT=30, NML=SCE_CONTROL)
      stop " ***** ERROR: Problem reading namelist SCE_CONTROL"
    endif

    read(UNIT=30, NML=METRIC_CONTROL, iostat=ierr)
    if (ierr /= 0) then
      write(*,'(/," ***** ERROR: Problem reading namelist METRIC_CONTROL",/)')
      rewind(UNIT=30)
      read(UNIT=30, NML=METRIC_CONTROL)
      stop " ***** ERROR: Problem reading namelist METRIC_CONTROL"
    endif

    close(UNIT=30)

    return

  end subroutine read_calib_control

  subroutine set_obj_func_description
    use nrtype
    use fuse_fileManager

    implicit none

    if(trim(calib_type) == "daily" .or. trim(calib_type) == "DAILY") then
        if(trim(calib_metric) == "RMSE" .or. trim(calib_metric) == "rmse") then
          obj_func_desc = 'root-mean-squared-error of flow                    '
          obj_func_unit = 'mm timestep-1'
        elseif(trim(calib_metric) == "KGE" .or. trim(calib_metric) == "kge") then
          obj_func_desc = 'Kling-Gupta score                                  '
          obj_func_unit = '-            '
        endif
    elseif(trim(calib_type) == "INTERVAL" .or. trim(calib_type) == "interval") then
        if(trim(calib_metric) == "RMSE" .or. trim(calib_metric) == "rmse") then
          obj_func_desc = 'root-mean-squared-error of interval maximums     '
          obj_func_unit = 'mm interval-1'
        elseif(trim(calib_metric) == "KGE" .or. trim(calib_metric) == "kge") then
          obj_func_desc = 'Kling-Gupta score using interval maximums          '
          obj_func_unit = '-            '
        endif
      endif

    
  end subroutine set_obj_func_description

end module calib_control
