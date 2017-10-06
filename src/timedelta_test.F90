program timedelta_test

  use unit_test
  use timedelta_mod

  implicit none

  type(timedelta_type) dt

  call test_case_init()

  call test_case_create('Test timedelta type')

  dt = timedelta(days=2)

  call assert_equal(dt%total_seconds(), 2.0d0 * 86400.0d0)

  dt = timedelta(hours=3, minutes=30)

  call assert_equal(dt%total_seconds(), 3.5d0 * 3600.0d0)

  dt = timedelta(minutes=6)

  call assert_equal(dt%total_seconds(), 360.0d0)

  dt = timedelta(seconds=60, milliseconds=103)

  call assert_equal(dt%total_seconds(), 60.103d0)

  dt = timedelta(seconds=33.1d0)

  call assert_equal(dt%seconds, 33)
  call assert_equal(dt%milliseconds, 100)
  call assert_equal(dt%total_seconds(), 33.1d0)

  dt = timedelta(minutes=3.24d0)

  call assert_equal(dt%minutes, 3)
  call assert_equal(dt%seconds, 14)
  call assert_equal(dt%milliseconds, 400)
  call assert_equal(dt%total_seconds(), 194.4d0)

  dt = timedelta(hours=1.43d0, seconds=13d0)

  call assert_equal(dt%hours, 1)
  call assert_equal(dt%minutes, 25)
  call assert_equal(dt%seconds, 60)
  call assert_approximate(dt%milliseconds * 1.0d0, 1000.0d0, eps=2.0d0)
  call assert_approximate(dt%total_seconds(), 5161d0, eps=0.1d0)

  call test_case_report('Test timedelta type')

  call test_case_final()

end program timedelta_test
