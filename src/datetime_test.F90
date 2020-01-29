program datetime_test

  use unit_test
  use datetime

  implicit none

  type(datetime_type) a, b
  type(timedelta_type) dt

  call test_suite_init('Test datetime')

  call test_basic_functions()

  call test_constructors()

  call test_operators()

  call test_suite_report()

  call test_suite_final()

contains

  subroutine test_basic_functions()

    call test_case_create('Test basic functions')

    ! Test leap year judgement.
    call assert_false(is_leap_year(2017), __FILE__, __LINE__)
    call assert_true(is_leap_year(2000), __FILE__, __LINE__)
    call assert_true(is_leap_year(2004), __FILE__, __LINE__)
    call assert_true(is_leap_year(2008), __FILE__, __LINE__)
    call assert_true(is_leap_year(2012), __FILE__, __LINE__)
    call assert_true(is_leap_year(2016), __FILE__, __LINE__)

    ! Test days_of_month.
    call assert_equal(days_of_month(2019,  1, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  2, datetime_gregorian_calendar), 28, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  3, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  4, datetime_gregorian_calendar), 30, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  5, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  6, datetime_gregorian_calendar), 30, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  7, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  8, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019,  9, datetime_gregorian_calendar), 30, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019, 10, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019, 11, datetime_gregorian_calendar), 30, __FILE__, __LINE__)
    call assert_equal(days_of_month(2019, 12, datetime_gregorian_calendar), 31, __FILE__, __LINE__)
    call assert_equal(days_of_month(1984,  2, datetime_gregorian_calendar), 29, __FILE__, __LINE__)

    ! Test accum_days.
    call assert_equal(accum_days(2019, 11, 0, datetime_gregorian_calendar), 304, __FILE__, __LINE__)
    call assert_equal(accum_days(2019, 11, 1, datetime_gregorian_calendar), 305, __FILE__, __LINE__)
    call assert_equal(accum_days(2019, 12, 31, datetime_gregorian_calendar), 365, __FILE__, __LINE__)

    ! Test days_of_year.
    call assert_equal(days_of_year(2017, datetime_gregorian_calendar), 365, __FILE__, __LINE__)

  end subroutine test_basic_functions

  subroutine test_constructors()

    type(datetime_type) a

    call test_case_create('Test constructors')

    a = create_datetime(2017, 10, 6, 12, 31, 23)
    call assert_equal(a%year, 2017, __FILE__, __LINE__)
    call assert_equal(a%month, 10, __FILE__, __LINE__)
    call assert_equal(a%day, 6, __FILE__, __LINE__)
    call assert_equal(a%hour, 12, __FILE__, __LINE__)
    call assert_equal(a%minute, 31, __FILE__, __LINE__)
    call assert_equal(a%second, 23, __FILE__, __LINE__)
    call assert_equal(a%millisecond, 0.0d0, __FILE__, __LINE__)
    call assert_equal(a%timezone, 0.0d0, __FILE__, __LINE__)
    call assert_equal(a%isoformat(), '2017-10-06T12:31:23Z', __FILE__, __LINE__)

    a = create_datetime(year=2019, julday=365, hour=24)
    call assert_equal(a%year, 2020, __FILE__, __LINE__)
    call assert_equal(a%month, 1, __FILE__, __LINE__)
    call assert_equal(a%day, 1, __FILE__, __LINE__)
    call assert_equal(a%hour, 0, __FILE__, __LINE__)
    call assert_equal(a%minute, 0, __FILE__, __LINE__)
    call assert_equal(a%second, 0, __FILE__, __LINE__)

    a = create_datetime(timestamp=1532755828.266736d0)
    call assert_approximate(a%timestamp(), 1532755828.266736d0, __FILE__, __LINE__)

    a = create_datetime('2018041401', '%Y%m%d%H')
    call assert_equal(a%isoformat(), '2018-04-14T01:00:00Z', __FILE__, __LINE__)
    call assert_equal(trim(a%format('%Y')), '2018', __FILE__, __LINE__)
    call assert_equal(a%format('%y%j%H%M'), '181040100', __FILE__, __LINE__)

    a = create_datetime('2018-01-18T11:51:10Z')
    call assert_equal(a%year, 2018, __FILE__, __LINE__)
    call assert_equal(a%month, 1, __FILE__, __LINE__)
    call assert_equal(a%day, 18, __FILE__, __LINE__)
    call assert_equal(a%hour, 11, __FILE__, __LINE__)
    call assert_equal(a%minute, 51, __FILE__, __LINE__)
    call assert_equal(a%second, 10, __FILE__, __LINE__)
    call assert_equal(a%millisecond, 0.0d0, __FILE__, __LINE__)
    call assert_equal(a%timezone, 0.0d0, __FILE__, __LINE__)

    a = create_datetime(days=120)
    call assert_equal(a%year, 1, __FILE__, __LINE__)
    call assert_equal(a%month, 5, __FILE__, __LINE__)
    call assert_equal(a%day, 1, __FILE__, __LINE__)
    call assert_equal(a%hour, 0, __FILE__, __LINE__)
    call assert_equal(a%minute, 0, __FILE__, __LINE__)

  end subroutine test_constructors

  subroutine test_operators()

    type(datetime_type) a, b
    type(timedelta_type) dt

    call test_case_create('Test operators')

    a = create_datetime('2018-01-18T11:51:10Z')

    b = a
    call assert_true(a == b, __FILE__, __LINE__)

    dt = create_timedelta(minutes=5)
  
    b = a + dt
    call assert_true(b >  a, __FILE__, __LINE__)
    call assert_true(b >= a, __FILE__, __LINE__)
    call assert_true(a <  b, __FILE__, __LINE__)
    call assert_true(a <= b, __FILE__, __LINE__)
    call assert_true(a /= b, __FILE__, __LINE__)
    call assert_equal(a%minute + 5, b%minute, __FILE__, __LINE__)
  
    b = a - dt
    call assert_true(b <  a, __FILE__, __LINE__)
    call assert_true(b <= a, __FILE__, __LINE__)
    call assert_true(a >  b, __FILE__, __LINE__)
    call assert_true(a >= b, __FILE__, __LINE__)
    call assert_true(a /= b, __FILE__, __LINE__)
    call assert_equal(a%minute - 5, b%minute, __FILE__, __LINE__)

    a = create_datetime(2018, 1, 18, 13, 14, 12)
    b = create_datetime(2018, 1, 13, 12, 45, 13)
    call assert_true(a > b, __FILE__, __LINE__)

    ! Test add_* subroutines.
    a = create_datetime(2017, 2, 1)
    call a%add_months(-6)
    call assert_equal(a%year, 2016, __FILE__, __LINE__)
    call assert_equal(a%month, 8, __FILE__, __LINE__)
    call assert_equal(a%day, 1, __FILE__, __LINE__)

    a = create_datetime(minute=6)
    b = create_datetime(hour=1)
    call assert_false(a > b, __FILE__, __LINE__)
  
    a = create_datetime(minute=56)
  
    b = a + dt
    call assert_equal(b%hour, 1, __FILE__, __LINE__)
    call assert_equal(b%minute, 1, __FILE__, __LINE__)
  
    a = create_datetime(second=45)
    dt = create_timedelta(seconds=30)
    b = a + dt
    call assert_equal(b%minute, 1, __FILE__, __LINE__)
    call assert_equal(b%second, 15, __FILE__, __LINE__)
  
    dt = create_timedelta(days=31)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 12, __FILE__, __LINE__)
    call assert_equal(b%day, 1, __FILE__, __LINE__)
    call assert_equal(b%hour, 0, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 0, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(days=37)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 11, __FILE__, __LINE__)
    call assert_equal(b%day, 25, __FILE__, __LINE__)
    call assert_equal(b%hour, 0, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 0, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(hours=25)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 12, __FILE__, __LINE__)
    call assert_equal(b%day, 30, __FILE__, __LINE__)
    call assert_equal(b%hour, 23, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 0, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(hours=24)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 12, __FILE__, __LINE__)
    call assert_equal(b%day, 31, __FILE__, __LINE__)
    call assert_equal(b%hour, 0, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 0, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(minutes=60)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 12, __FILE__, __LINE__)
    call assert_equal(b%day, 31, __FILE__, __LINE__)
    call assert_equal(b%hour, 23, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 0, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(seconds=21600)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 12, __FILE__, __LINE__)
    call assert_equal(b%day, 31, __FILE__, __LINE__)
    call assert_equal(b%hour, 18, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 0, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(milliseconds=2200)
    a = create_datetime(millisecond=300)
    b = a + dt
    call assert_equal(b%year, 1, __FILE__, __LINE__)
    call assert_equal(b%month, 1, __FILE__, __LINE__)
    call assert_equal(b%day, 1, __FILE__, __LINE__)
    call assert_equal(b%hour, 0, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)
    call assert_equal(b%second, 2, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 500.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(milliseconds=1000)
    a = create_datetime()
    b = a - dt
    call assert_equal(b%year, 0, __FILE__, __LINE__)
    call assert_equal(b%month, 12, __FILE__, __LINE__)
    call assert_equal(b%day, 31, __FILE__, __LINE__)
    call assert_equal(b%hour, 23, __FILE__, __LINE__)
    call assert_equal(b%minute, 59, __FILE__, __LINE__)
    call assert_equal(b%second, 59, __FILE__, __LINE__)
    call assert_equal(b%millisecond, 0.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2018, 1, 1, 0, 0, 0)
    b = create_datetime(2018, 1, 1, 0, 0, 0)
    dt = a - b  
    call assert_equal(dt%days, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2018, 1, 18, 13, 14, 12)
    b = create_datetime(2018, 1, 13, 12, 45, 13)
    dt = a - b
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, 59.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, 28.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%days, 5.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2018, 1, 18, 0, 0, 0)
    b = create_datetime(2018, 1, 13, 0, 0, 0)
    dt = a - b
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%days, 5.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2017, 2, 18, 13, 37, 20)
    b = create_datetime(2018, 1, 13, 0, 0, 0)
    dt = a - b
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, -40.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, -22.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, -10.0d0, __FILE__, __LINE__)
    call assert_equal(dt%days, -328.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2018, 4, 18, 13, 37, 20)
    b = create_datetime(2018, 4, 18, 13, 37, 10)
    dt = a - b
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, 10.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%days, 0.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2018, 4, 18, 13, 37, 0)
    b = create_datetime(2018, 4, 18, 13, 34, 0)
    dt = a - b
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, 3.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%days, 0.0d0, __FILE__, __LINE__)
  
    a = create_datetime(2018, 4, 18, 13, 0, 0)
    b = create_datetime(2018, 4, 18, 12, 0, 0)
    dt = a - b
    call assert_equal(dt%milliseconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%seconds, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, 1.0d0, __FILE__, __LINE__)
    call assert_equal(dt%days, 0.0d0, __FILE__, __LINE__)
  
    a = create_datetime(year=2017, month=10, day=6, hour=14)
    b = create_datetime(year=2018, month=4, day=16, hour=23, minute=51)
    dt = b - a
    call assert_equal(dt%total_seconds(), 16624260.0d0, __FILE__, __LINE__)
    call assert_equal(dt%total_minutes(), 16624260.0d0 / 60.0d0, __FILE__, __LINE__)
    call assert_equal(dt%total_hours(), 16624260 / 3600.0d0, __FILE__, __LINE__)
    call assert_equal(dt%total_days(), 16624260 / 86400.0d0, __FILE__, __LINE__)
  
    a = create_datetime(year=2015, month=8, day=5, hour=21)
    b = create_datetime(year=2015, month=8, day=5, hour=19, minute=31)
    dt = b - a
    call assert_equal(dt%days, 0.0d0, __FILE__, __LINE__)
    call assert_equal(dt%hours, -1.0d0, __FILE__, __LINE__)
    call assert_equal(dt%minutes, -29.0d0, __FILE__, __LINE__)
  
    dt = create_timedelta(hours=0.5)
    a = create_datetime(2018, 9, 4, 14, 30)
    b = a + dt
    call assert_equal(b%year, 2018, __FILE__, __LINE__)
    call assert_equal(b%month, 9, __FILE__, __LINE__)
    call assert_equal(b%day, 4, __FILE__, __LINE__)
    call assert_equal(b%hour, 15, __FILE__, __LINE__)
    call assert_equal(b%minute, 0, __FILE__, __LINE__)

  end subroutine test_operators

end program datetime_test
