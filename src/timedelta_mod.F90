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

  elemental type(timedelta_type) function timedelta(days, hours, minutes, seconds, milliseconds)

    integer, intent(in), optional :: days
    integer, intent(in), optional :: hours
    integer, intent(in), optional :: minutes
    integer, intent(in), optional :: seconds
    integer, intent(in), optional :: milliseconds

    if (present(days)) timedelta%days = days
    if (present(hours)) timedelta%hours = hours
    if (present(minutes)) timedelta%minutes = minutes
    if (present(seconds)) timedelta%seconds = seconds
    if (present(milliseconds)) timedelta%milliseconds = milliseconds

  end function timedelta

  real(8) function total_seconds(this)

    class(timedelta_type), intent(in) :: this

    total_seconds = this%days * 86400 + this%hours * 3600 + this%minutes * 60 + this%seconds + this%milliseconds * 1.0d-3

  end function total_seconds

end module timedelta_mod