MODULE eval_metrics
  IMPLICIT NONE
  CONTAINS

  ! ---------------------------------------------------------------------------------------
  ! Creator:
  ! --------
  ! Andy Newman, 04/2018
  ! ---------------------------------------------------------------------------------------
  ! Purpose:
  ! --------
  !  Module that contains various metrics computed from timeseries
  ! ---------------------------------------------------------------------------------------


  SUBROUTINE simple_moving_average(timeseries,windowSize,average_timeseries)
    ! ---------------------------------------------------------------------------------------
    ! Creator:
    ! --------
    ! Andy Newman, 04/2018
    ! ---------------------------------------------------------------------------------------
    ! Purpose:
    ! --------
    ! Calculate the simple moving average of a timeseries
    !   input: some timeseries of values
    !  output: root mean squared error
    ! ---------------------------------------------------------------------------------------

    USE nrtype                                               ! variable types, etc.

    IMPLICIT NONE
    
    !input
    REAL(SP),DIMENSION(:),INTENT(IN)       :: timeseries           ! input timeseries
    INTEGER(I4B),INTENT(IN)                :: windowSize           ! length of window to average over
    
    !output
    REAL(SP),DIMENSION(:),INTENT(OUT)      :: average_timeseries   ! output averaged timeseries

    !local variables
    INTEGER(I4B)                          :: i                     ! looping variable
    INTEGER(I4B)                          :: nval                  ! length of timeseries


    !code below

    !define number of values
    nval = size(timeseries,1)

    !loop through raw timeseries, create average series using windowSize
    do i = 1,nval
      if(i < windowSize) then !if window isn't full, use all available
        average_timeseries(i) = sum(timeseries(1:i))/real(i)
      else               !else average over window
        average_timeseries(i) = sum(timeseries(i-windowSize+1:i))/real(windowSize)
      endif
    enddo

  END SUBROUTINE simple_moving_average

  subroutine interval_max_vals(timeseries,interval,interval_maxes,calibration_mask)

    USE nrtype                              ! variable types, etc.

    implicit none


    !input variables
    real(SP), dimension(:), intent(in)  :: timeseries                          ! input timeseries
    integer(I4B), intent(in)            :: interval                            ! interval (timesteps) to find max value over

    !output variables
    real(SP), dimension(:), allocatable, intent(out)      :: interval_maxes    ! output max values
    integer(I4B), dimension(:), allocatable, intent(out)  :: calibration_mask  ! mask for calibration of interval max values

 
    !local variables
    integer(I4B)                            :: i                               ! looping variable
    integer(I4B)                            :: nval                            ! length of timeseries
    integer(I4B)                            :: nintervals                      ! number of intervals
    integer(I4B)                            :: start_pos                       ! start position of current interval
    integer(I4B)                            :: end_pos                         ! end position of current interval
    integer(I4B)                            :: cal_intervals                   ! number of max values to indentify and use for calibration    
    integer(I4B), dimension(:), allocatable :: reset                           ! mask for calibration of interval max values
    real(SP)                                :: min_value                       ! minimum value of interval_maxes

    !code below

    !length of timeseries
    nval = size(timeseries)

    !determine number of complete intervals we have
    !only use complete intervals starting at the first timestep
    nintervals = nval/interval

    !allocate space for output
    allocate(interval_maxes(nintervals),calibration_mask(nintervals),reset(nintervals))

    !loop through timeseries
    !initialize start position
    start_pos = 1
    do i = 1,nintervals

      !set end_position
      end_pos = start_pos + interval - 1
 
      interval_maxes(i) = maxval(timeseries(start_pos:end_pos))
      
      !increment start position
      start_pos = end_pos + 1
    enddo

    !identify top N values
    !initialize variables
    cal_intervals = 10
    calibration_mask = 0
    reset = 0
    min_value = 0

    !loop through array and identifiy N values
    do i = 1,nintervals
      !if current value is larger than minimum
      if(interval_maxes(i) > min_value) then

        !if we have less than cal_intervals values, set mask
        if(i .lt. cal_intervals-1) then
          calibration_mask(i) = 1
        else if(i .eq. cal_intervals) then
          calibration_mask(i) = 1
          min_value = minval(interval_maxes,calibration_mask==1)
          where(interval_maxes .EQ. min_value) reset = 1
          where(reset .EQ. 1) calibration_mask = 0
          reset = 0
        else                           !if more than 10, change mask to new value location
          calibration_mask(i) = 1
          where(interval_maxes .EQ. min_value) reset = 1
          where(reset .EQ. 1) calibration_mask = 0
          reset = 0
          min_value = minval(interval_maxes,calibration_mask==1)
        endif !end 

      endif   !end min_value if statement
    enddo

!    print *,'intervals ',interval_maxes
!    print *,'mask ',calibration_mask

  end subroutine interval_max_vals

  subroutine calc_kge(model,observation,kge)
    ! ---------------------------------------------------------------------------------------
    ! Creator:
    ! --------
    ! Andy Newman, 04/2018
    ! ---------------------------------------------------------------------------------------
    ! Purpose:
    ! --------
    !  This subroutine calculates the negative of the Kling-Gupta Efficiency (KGE)
    !  in order to solve a minimization problem
    !  input: some timeseries of model and observation values
    !  output: KGE
    ! ---------------------------------------------------------------------------------------

    USE nrtype                                               ! variable types, etc.

    implicit none

    !input variables
    real(SP), dimension(:),     intent(in)  :: model                 ! input model variable
    real(SP), dimension(:),     intent(in)  :: observation           ! input observation variable

    !output variables
    real(SP),                   intent(out) :: kge                   ! output KGE value

    !local variables
    integer(I4B)      :: itime                                       ! counter for looping through series
    integer(I4B)      :: nval                                        ! number of elements in series
    real(SP)          :: linear_corr                                 ! linear pearson correlation
    real(SP)          :: alpha                                       ! alpha in KGE (multiplicative variance bias)
    real(SP)          :: beta                                        ! beta  in KGE (multiplicative volume bias)
    real(SP)          :: mean_model                                  ! model mean value
    real(SP)          :: mean_obs                                    ! observation mean value
    real(SP)          :: std_dev_model                               ! model standard deviation
    real(SP)          :: std_dev_obs                                 ! observation standard deviation
  
    !initialize variables to zero
    mean_model = 0.0
    mean_obs   = 0.0
    std_dev_model = 0.0
    std_dev_obs   = 0.0

    !number of elements
    nval = size(model,1)

    !We first compute the mean
    mean_model = sum(model)/real(nval)
    mean_obs = sum(observation)/real(nval)
    beta = mean_model/mean_obs

    !Now we compute the standard deviation
    do itime = 1, nval
      std_dev_model = std_dev_model + (model(itime)-mean_model)**2
      std_dev_obs = std_dev_obs + (observation(itime)-mean_obs)**2
    enddo
    std_dev_model = sqrt(std_dev_model/real(nval-1))
    std_dev_obs   = sqrt(std_dev_obs/real(nval-1))
    alpha = std_dev_model/std_dev_obs

    !Compute linear correlation coefficient
    call pearson_corr(model,observation,linear_corr)

    !compute KGE
    kge = -( 1.0 - sqrt((linear_corr-1.0)**2 + (alpha-1.0)**2 + (beta-1.0)**2) )

    return
  end subroutine

  subroutine pearson_corr(x,y,r)
    USE nrtype                                        ! variable types, etc.

    implicit none

    !input variables
    real(SP), dimension(:),     intent(in)  :: x      ! first timeseries
    real(SP), dimension(:),     intent(in)  :: y      ! second timeseries

    !output variables
    real(SP),                   intent(out) :: r      ! output linear correlation coefficient

    !local variables
    real(SP), dimension(size(x)) :: xt                ! deviations of x from mean of x
    real(SP), dimension(size(x)) :: yt                ! deviations of y from mean of y
   
    real(SP) :: tiny = 1.0e-20                        ! numerical tiny value for numerical stability
    real(SP) :: mean_x                                ! mean of x series
    real(SP) :: mean_y                                ! mean of y series
    real(SP) :: var_x                                 ! variance of x
    real(SP) :: var_y                                 ! variance of y
    real(SP) :: covar_xy                              ! covariance of x-y
    integer(I4B) :: nval                              ! number of elements

    !number of elements
    nval=size(x)

    !mean values
    mean_x=sum(x)/nval
    mean_y=sum(y)/nval

    !deviations of x and y from mean values
    xt(:)=x(:)-mean_x
    yt(:)=y(:)-mean_y

    !variance of x and y
    var_x=dot_product(xt,xt)
    var_y=dot_product(yt,yt)

    !covariance of x-y
    covar_xy=dot_product(xt,yt)

    !correlation coefficient
    r=covar_xy/(sqrt(var_x*var_y)+tiny)

  end subroutine pearson_corr

END MODULE eval_metrics

