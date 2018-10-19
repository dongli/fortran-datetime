module timedelta_mod

  implicit none

  type timedelta_type
    real(8) :: days = 0.0d0
    real(8) :: hours = 0.0d0
    real(8) :: minutes = 0.0d0
    real(8) :: seconds = 0.0d0
    integer :: milliseconds = 0
  contains
    procedure :: total_seconds
    procedure :: total_minutes
    procedure :: total_hours
    procedure :: total_days
    procedure :: negate
  end type timedelta_type

contains

  type(timedelta_type) function timedelta(days, hours, minutes, seconds, milliseconds) result(res)

    class(*), intent(in), optional :: days
    class(*), intent(in), optional :: hours
    class(*), intent(in), optional :: minutes
    class(*), intent(in), optional :: seconds
    integer, intent(in), optional :: milliseconds

    real(8) remainder

    remainder = 0.0d0

    if (present(days)) then
      select type (days)
      type is (integer)
        res%days = days
      type is (real(4))
        res%days = days
      type is (real(8))
        res%days = days
      end select
    end if

    if (present(hours)) then
      select type (hours)
      type is (integer)
        res%hours = hours
      type is (real(4))
        res%hours = hours
      type is (real(8))
        res%hours = hours
      end select
    end if

    if (present(minutes)) then
      select type (minutes)
      type is (integer)
        res%minutes = minutes
      type is (real(4))
        res%minutes = minutes
      type is (real(8))
        res%minutes = minutes
      end select
    end if

    if (present(seconds)) then
      select type (seconds)
      type is (integer)
        res%seconds = seconds
      type is (real(4))
        res%seconds = seconds
      type is (real(8))
        res%seconds = seconds
      end select
    end if

    if (present(milliseconds)) then
      res%milliseconds = milliseconds
    end if

  end function timedelta

  real(8) function total_seconds(this)

    class(timedelta_type), intent(in) :: this

    total_seconds = this%days * 86400 + this%hours * 3600 + this%minutes * 60 + this%seconds + this%milliseconds * 1.0d-3

  end function total_seconds

  real(8) function total_minutes(this)

    class(timedelta_type), intent(in) :: this

    total_minutes = this%days * 1440 + this%hours * 60 + this%minutes + (this%seconds + this%milliseconds * 1.0d-3) / 60.0d0

  end function total_minutes

  real(8) function total_hours(this)

    class(timedelta_type), intent(in) :: this

    total_hours = this%days * 24 + this%hours + (this%minutes + (this%seconds + this%milliseconds * 1.0d-3) / 60.0d0) / 60.0d0

  end function total_hours

  real(8) function total_days(this)

    class(timedelta_type), intent(in) :: this

    total_days = this%days + (this%hours + (this%minutes + (this%seconds + this%milliseconds * 1.0d-3) / 60.0d0) / 60.0d0) / 24.0d0

  end function total_days

  pure elemental type(timedelta_type) function negate(this) result(res)

    class(timedelta_type), intent(in) :: this

    res%days = - this%days
    res%hours = - this%hours
    res%minutes = - this%minutes
    res%seconds = - this%seconds
    res%milliseconds = - this%milliseconds

  end function negate

end module timedelta_mod
