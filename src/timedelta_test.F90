program timedelta_test

  use unit_test
  use timedelta_mod

  implicit none

  type(timedelta_type) dt1, dt2

  call test_case_init()

  call test_case_create('Test timedelta type')

  dt1 = timedelta(days=2)

  call assert_equal(dt1%total_seconds(), 2.0d0 * 86400.0d0)

  dt1 = timedelta(hours=3, minutes=30)

  call assert_equal(dt1%total_seconds(), 3.5d0 * 3600.0d0)

  dt1 = timedelta(minutes=6)

  call assert_equal(dt1%total_seconds(), 360.0d0)

  dt1 = timedelta(seconds=60, milliseconds=103)

  call assert_equal(dt1%total_seconds(), 60.103d0)

  dt1 = timedelta(seconds=33.1d0)

  call assert_equal(dt1%seconds, 33)
  call assert_equal(dt1%milliseconds, 100)
  call assert_equal(dt1%total_seconds(), 33.1d0)

  dt1 = timedelta(minutes=3.24d0)

  call assert_equal(dt1%minutes, 3)
  call assert_equal(dt1%seconds, 14)
  call assert_equal(dt1%milliseconds, 400)
  call assert_equal(dt1%total_seconds(), 194.4d0)

  dt1 = timedelta(hours=1.43d0, seconds=13d0)

  call assert_equal(dt1%hours, 1)
  call assert_equal(dt1%minutes, 25)
  call assert_equal(dt1%seconds, 60)
  call assert_approximate(dt1%milliseconds * 1.0d0, 1000.0d0, eps=2.0d0)
  call assert_approximate(dt1%total_seconds(), 5161d0, eps=0.1d0)

  dt2 = dt1

  call assert_equal(dt2%hours, 1)
  call assert_equal(dt2%minutes, 25)
  call assert_equal(dt2%seconds, 60)
  call assert_approximate(dt2%milliseconds * 1.0d0, 1000.0d0, eps=2.0d0)
  call assert_approximate(dt2%total_seconds(), 5161d0, eps=0.1d0)

  call test_case_report('Test timedelta type')

  call test_case_final()

end program timedelta_test
