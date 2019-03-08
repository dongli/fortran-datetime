module timedelta_mod

  implicit none

  type timedelta_type
    integer :: months = 0.0d0
    real(8) :: days = 0.0d0
    real(8) :: hours = 0.0d0
    real(8) :: minutes = 0.0d0
    real(8) :: seconds = 0.0d0
    real(8) :: milliseconds = 0.0d0
  contains
    procedure :: total_seconds
    procedure :: total_minutes
    procedure :: total_hours
    procedure :: total_days
    procedure :: negate
  end type timedelta_type

contains

  type(timedelta_type) function timedelta(months, days, hours, minutes, seconds, milliseconds) result(res)

    class(*), intent(in), optional :: months
    class(*), intent(in), optional :: days
    class(*), intent(in), optional :: hours
    class(*), intent(in), optional :: minutes
    class(*), intent(in), optional :: seconds
    class(*), intent(in), optional :: milliseconds

    real(8) remainder

    remainder = 0.0d0

    if (present(months)) then
      select type (months)
      type is (integer)
        res%months = months
      type is (real(4))
        res%months = months
      type is (real(8))
        res%months = months
      end select
    end if

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
      select type (milliseconds)
      type is (integer)
        res%milliseconds = milliseconds
      type is (real(4))
        res%milliseconds = milliseconds
      type is (real(8))
        res%milliseconds = milliseconds
      end select
    end if

  end function timedelta

  real(8) function total_seconds(this)

    class(timedelta_type), intent(in) :: this

    if (this%months == 0.0d0) then
      total_seconds = this%days * 86400 + this%hours * 3600 + this%minutes * 60 + this%seconds + this%milliseconds * 1.0d-3
    else
      write(*, *) '[Error]: timedelta has nonzero months value!'
      total_seconds = -1
    end if

  end function total_seconds

  real(8) function total_minutes(this)

    class(timedelta_type), intent(in) :: this

    if (this%months == 0.0d0) then
      total_minutes = this%days * 1440 + this%hours * 60 + this%minutes + (this%seconds + this%milliseconds * 1.0d-3) / 60.0d0
    else
      write(*, *) '[Error]: timedelta has nonzero months value!'
      total_minutes = -1
    end if

  end function total_minutes

  real(8) function total_hours(this)

    class(timedelta_type), intent(in) :: this

    if (this%months == 0.0d0) then
      total_hours = this%days * 24 + this%hours + (this%minutes + (this%seconds + this%milliseconds * 1.0d-3) / 60.0d0) / 60.0d0
    else
      write(*, *) '[Error]: timedelta has nonzero months value!'
      total_hours = -1
    end if

  end function total_hours

  real(8) function total_days(this)

    class(timedelta_type), intent(in) :: this

    if (this%months == 0.0d0) then
      total_days = this%days + (this%hours + (this%minutes + (this%seconds + this%milliseconds * 1.0d-3) / 60.0d0) / 60.0d0) / 24.0d0
    else
      write(*, *) '[Error]: timedelta has nonzero months value!'
      total_days = -1
    end if

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
