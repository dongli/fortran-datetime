program datetime_test

  use unit_test
  use datetime

  implicit none

  type(datetime_type) a, b
  type(timedelta_type) dt

  call test_case_init()

  call test_case_create('Test datetime type')

  ! Test constructor function.
  a = create_datetime(2017, 10, 6, 12, 31, 23)
  call assert_equal(a%year, 2017, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%month, 10, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%day, 6, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%hour, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%minute, 31, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%second, 23, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%timezone, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%isoformat(), '2017-10-06T12:31:23Z', file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(timestamp=1532755828.266736d0)

  call assert_approximate(a%timestamp(), 1532755828.266736d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime('2018041401', '%Y%m%d%H')
  call assert_equal(a%isoformat(), '2018-04-14T01:00:00Z', file_name=__FILE__, line_number=__LINE__)
  call assert_equal(trim(a%format('%Y')), '2018', file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%format('%y%j%H%M'), '181040100', file_name=__FILE__, line_number=__LINE__)

  ! Test parse isoformat.
  a = create_datetime('2018-01-18T11:51:10Z')
  call assert_equal(a%year, 2018, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%month, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%day, 18, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%hour, 11, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%minute, 51, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%second, 10, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%timezone, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  ! Test assignment and equal judgement.
  b = a
  call assert_true(a == b, file_name=__FILE__, line_number=__LINE__)

  ! Test timedelta operators and judgements.
  dt = timedelta(minutes=5)

  b = a + dt
  call assert_true(b > a, file_name=__FILE__, line_number=__LINE__)
  call assert_true(b >= a, file_name=__FILE__, line_number=__LINE__)
  call assert_true(a < b, file_name=__FILE__, line_number=__LINE__)
  call assert_true(a <= b, file_name=__FILE__, line_number=__LINE__)
  call assert_true(a /= b, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%minute + 5, b%minute, file_name=__FILE__, line_number=__LINE__)

  b = a - dt
  call assert_true(b < a, file_name=__FILE__, line_number=__LINE__)
  call assert_true(b <= a, file_name=__FILE__, line_number=__LINE__)
  call assert_true(a > b, file_name=__FILE__, line_number=__LINE__)
  call assert_true(a >= b, file_name=__FILE__, line_number=__LINE__)
  call assert_true(a /= b, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%minute - 5, b%minute, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 1, 18, 13, 14, 12)
  b = create_datetime(2018, 1, 13, 12, 45, 13)
  call assert_true(a > b, file_name=__FILE__, line_number=__LINE__)

  ! Test construction from minute and hour.
  a = create_datetime(minute=6)
  b = create_datetime(hour=1)
  call assert_false(a > b, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(minute=56)

  b = a + dt
  call assert_equal(b%hour, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 1, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(second=45)
  dt = timedelta(seconds=30)
  b = a + dt
  call assert_equal(b%minute, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 15, file_name=__FILE__, line_number=__LINE__)

  ! Test timedelta days.
  dt = timedelta(days=31)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  dt = timedelta(days=37)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 11, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 25, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  ! Test timedelta hours.
  dt = timedelta(hours=25)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 30, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 23, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  dt = timedelta(hours=24)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 31, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  ! Test timedelta minutes.
  dt = timedelta(minutes=60)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 31, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 23, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  ! Test timedelta seconds.
  dt = timedelta(seconds=21600)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 31, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 18, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  ! Test timedelta milliseconds.
  dt = timedelta(milliseconds=2200)
  a = create_datetime(millisecond=300)
  b = a + dt
  call assert_equal(b%year, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 2, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 500.0d0, file_name=__FILE__, line_number=__LINE__)

  dt = timedelta(milliseconds=1000)
  a = create_datetime()
  b = a - dt
  call assert_equal(b%year, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 12, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 31, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 23, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 59, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%second, 59, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%millisecond, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  ! Test leap year judgement.
  call assert_false(is_leap_year(2017), file_name=__FILE__, line_number=__LINE__)
  call assert_true(is_leap_year(2000), file_name=__FILE__, line_number=__LINE__)
  call assert_true(is_leap_year(2004), file_name=__FILE__, line_number=__LINE__)
  call assert_true(is_leap_year(2008), file_name=__FILE__, line_number=__LINE__)
  call assert_true(is_leap_year(2012), file_name=__FILE__, line_number=__LINE__)
  call assert_true(is_leap_year(2016), file_name=__FILE__, line_number=__LINE__)

  ! Test construction from days.
  a = create_datetime(days=120)
  call assert_equal(a%year, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%month, 5, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%day, 1, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%hour, 0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%minute, 0, file_name=__FILE__, line_number=__LINE__)

  ! Test add_* subroutines.
  a = create_datetime(2017, 2, 1)
  call a%add_months(-6)
  call assert_equal(a%year, 2016, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%month, 8, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(a%day, 1, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 1, 1, 0, 0, 0)
  b = create_datetime(2018, 1, 1, 0, 0, 0)
  dt = a - b  
  call assert_equal(dt%days, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 1, 18, 13, 14, 12)
  b = create_datetime(2018, 1, 13, 12, 45, 13)
  dt = a - b
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, 59.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, 28.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%days, 5.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 1, 18, 0, 0, 0)
  b = create_datetime(2018, 1, 13, 0, 0, 0)
  dt = a - b
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%days, 5.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2017, 2, 18, 13, 37, 20)
  b = create_datetime(2018, 1, 13, 0, 0, 0)
  dt = a - b
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, -40.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, -22.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, -10.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%days, -328.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 4, 18, 13, 37, 20)
  b = create_datetime(2018, 4, 18, 13, 37, 10)
  dt = a - b
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, 10.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%days, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 4, 18, 13, 37, 0)
  b = create_datetime(2018, 4, 18, 13, 34, 0)
  dt = a - b
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, 3.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%days, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(2018, 4, 18, 13, 0, 0)
  b = create_datetime(2018, 4, 18, 12, 0, 0)
  dt = a - b
  call assert_equal(dt%milliseconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%seconds, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, 1.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%days, 0.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(year=2017, month=10, day=6, hour=14)
  b = create_datetime(year=2018, month=4, day=16, hour=23, minute=51)
  dt = b - a
  call assert_equal(dt%total_seconds(), 16624260.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%total_minutes(), 16624260.0d0 / 60.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%total_hours(), 16624260 / 3600.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%total_days(), 16624260 / 86400.0d0, file_name=__FILE__, line_number=__LINE__)

  a = create_datetime(year=2015, month=8, day=5, hour=21)
  b = create_datetime(year=2015, month=8, day=5, hour=19, minute=31)
  dt = b - a
  call assert_equal(dt%days, 0.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%hours, -1.0d0, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(dt%minutes, -29.0d0, file_name=__FILE__, line_number=__LINE__)

  dt = timedelta(hours=0.5)
  a = create_datetime(2018, 9, 4, 14, 30)
  b = a + dt
  call assert_equal(b%year, 2018, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%month, 9, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%day, 4, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%hour, 15, file_name=__FILE__, line_number=__LINE__)
  call assert_equal(b%minute, 0, file_name=__FILE__, line_number=__LINE__)

  call test_case_report('Test datetime type')

  call test_case_final()

end program datetime_test
