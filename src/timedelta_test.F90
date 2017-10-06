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

  call test_case_report('Test timedelta type')

  call test_case_final()

end program timedelta_test
