module timedelta_mod

  implicit none

  type timedelta_type
    integer :: days = 0
    integer :: hours = 0
    integer :: minutes = 0
    integer :: seconds = 0
    integer :: milliseconds = 0
  contains
    procedure :: total_seconds
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
      type is (real(8))
        res%days = floor(days)
        remainder = days - res%days
      end select
    end if

    if (present(hours)) then
      select type (hours)
      type is (integer)
        res%hours = hours
      type is (real(8))
        res%hours = floor(hours + remainder * 24)
        remainder = hours + remainder * 24 - res%hours
      end select
    else
      res%hours = floor(remainder * 24)
      remainder = remainder * 24 - res%hours
    end if

    if (present(minutes)) then
      select type (minutes)
      type is (integer)
        res%minutes = minutes
      type is (real(8))
        res%minutes = floor(minutes + remainder * 60)
        remainder = minutes + remainder * 60 - res%minutes
      end select
    else
      res%minutes = floor(remainder * 60)
      remainder = remainder * 60 - res%minutes
    end if

    if (present(seconds)) then
      select type (seconds)
      type is (integer)
        res%seconds = seconds
      type is (real(8))
        res%seconds = floor(seconds + remainder * 60)
        remainder = seconds + remainder * 60 - res%seconds
      end select
    else
      res%seconds = floor(remainder * 60)
      remainder = remainder * 60 - res%seconds
    end if

    if (present(milliseconds)) then
      res%milliseconds = milliseconds + remainder * 1000
    else
      res%milliseconds = remainder * 1000
    end if

  end function timedelta

  real(8) function total_seconds(this)

    class(timedelta_type), intent(in) :: this

    total_seconds = this%days * 86400 + this%hours * 3600 + this%minutes * 60 + this%seconds + this%milliseconds * 1.0d-3

  end function total_seconds

end module timedelta_mod