program datetime_test

  use unit_test
  use datetime_mod
  use timedelta_mod

  implicit none

  type(datetime_type) a, b
  type(timedelta_type) dt

  call test_case_init()

  call test_case_create('Test datetime type')

  a = datetime(2017, 10, 6, 12, 31, 23)

  call assert_equal(a%year, 2017)
  call assert_equal(a%month, 10)
  call assert_equal(a%day, 6)
  call assert_equal(a%hour, 12)
  call assert_equal(a%minute, 31)
  call assert_equal(a%second, 23)
  call assert_equal(a%millisecond, 0)
  call assert_approximate(a%timezone, 0.0d0)
  call assert_equal(a%isoformat(), '2017-10-06T12:31:23Z')

  b = a

  call assert_true(a == b)

  dt = timedelta(minutes=5)

  b = a + dt

  call assert_true(b > a)
  call assert_true(b >= a)
  call assert_true(a < b)
  call assert_true(a <= b)
  call assert_true(a /= b)
  call assert_equal(a%minute + 5, b%minute)

  b = a - dt

  call assert_true(b < a)
  call assert_true(b <= a)
  call assert_true(a > b)
  call assert_true(a >= b)
  call assert_true(a /= b)
  call assert_equal(a%minute - 5, b%minute)

  a = datetime(minute=6)
  b = datetime(hour=1)
  call assert_false(a > b)

  a = datetime(minute=56)

  b = a + dt

  call assert_equal(b%hour, 1)
  call assert_equal(b%minute, 1)

  a = datetime(second=45)

  dt = timedelta(seconds=30)

  b = a + dt

  call assert_equal(b%minute, 1)
  call assert_equal(b%second, 15)

  dt = timedelta(seconds=21600)

  a = datetime()

  b = a - dt

  call assert_equal(b%year, 0)
  call assert_equal(b%month, 12)
  call assert_equal(b%day, 31)
  call assert_equal(b%hour, 18)
  call assert_equal(b%minute, 0)
  call assert_equal(b%second, 0)
  call assert_equal(b%millisecond, 0)

  call assert_false(is_leap_year(2017))
  call assert_true(is_leap_year(2000))
  call assert_true(is_leap_year(2004))
  call assert_true(is_leap_year(2008))
  call assert_true(is_leap_year(2012))
  call assert_true(is_leap_year(2016))

  a = datetime(days=120)

  call assert_equal(a%year, 1)
  call assert_equal(a%month, 5)
  call assert_equal(a%day, 1)
  call assert_equal(a%hour, 0)
  call assert_equal(a%minute, 0)

  call test_case_report('Test datetime type')

  call test_case_final()

end program datetime_test
