program timedelta_test

  use unit_test
  use timedelta_mod

  implicit none

  type(timedelta_type) dt1, dt2

  call test_case_init()

  call test_case_create('Test timedelta type')

  dt1 = timedelta(days=2)

  call assert_equal(dt1%total_seconds(), 2.0 * 86400.0)
  call assert_equal(dt1%total_minutes(), 2.0 * 1440.0)
  call assert_equal(dt1%total_hours(), 2.0 * 24.0)
  call assert_equal(dt1%total_days(), 2.0)

  dt1 = timedelta(hours=3, minutes=30)

  call assert_equal(dt1%total_seconds(), 3.5 * 3600.0)
  call assert_equal(dt1%total_minutes(), 3.5 * 60.0)
  call assert_equal(dt1%total_hours(), 3.5)
  call assert_equal(dt1%total_days(), 3.5 / 24.0)

  dt1 = timedelta(minutes=6)

  call assert_equal(dt1%total_seconds(), 360.0)
  call assert_equal(dt1%total_minutes(), 6.0)
  call assert_equal(dt1%total_hours(), 6.0 / 60.0)
  call assert_equal(dt1%total_days(), 6.0 / 1440.0)

  dt1 = timedelta(seconds=60, milliseconds=103)

  call assert_equal(dt1%total_seconds(), 60.103)
  call assert_equal(dt1%total_minutes(), 60.103 / 60.0)
  call assert_equal(dt1%total_hours(), 60.103 / 3600.0)
  call assert_equal(dt1%total_days(), 60.103 / 86400.0)

  dt1 = timedelta(seconds=33.1)

  call assert_equal(dt1%seconds, 33.1)
  call assert_equal(dt1%total_seconds(), 33.1)

  dt1 = timedelta(minutes=3.24)

  call assert_equal(dt1%minutes, 3.24)
  call assert_equal(dt1%total_seconds(), 194.4)

  dt1 = timedelta(hours=1.43, seconds=13)

  call assert_equal(dt1%hours, 1.43)
  call assert_equal(dt1%seconds, 13.0)
  call assert_approximate(dt1%total_seconds(), 5161.0, eps=0.1)

  dt2 = dt1

  call assert_equal(dt2%hours, 1.43)
  call assert_equal(dt2%seconds, 13.0)
  call assert_approximate(dt2%total_seconds(), 5161.0, eps=0.1)

  call test_case_report('Test timedelta type')

  call test_case_final()

end program timedelta_test
