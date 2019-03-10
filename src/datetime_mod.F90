module datetime_mod

  use timedelta_mod

  implicit none

  private

  public create_datetime
  public set_datetime
  public datetime_type
  public days_of_month
  public days_of_year
  public is_leap_year

  type datetime_type
    integer :: year = 1
    integer :: month = 1
    integer :: day = 1
    integer :: hour = 0
    integer :: minute = 0
    integer :: second = 0
    real(8) :: millisecond = 0
    real(8) :: timezone = 0.0d0
  contains
    procedure :: init
    procedure :: isoformat
    procedure :: timestamp
    procedure :: format
    procedure :: add_months
    procedure :: add_days
    procedure :: add_hours
    procedure :: add_minutes
    procedure :: add_seconds
    procedure :: add_milliseconds
    procedure :: days_in_year
    procedure, private :: assign
    procedure, private :: add
    procedure, private :: sub_datetime
    procedure, private :: sub_timedelta
    procedure, private :: eq
    procedure, private :: neq
    procedure, private :: gt
    procedure, private :: ge
    procedure, private :: lt
    procedure, private :: le
    generic :: assignment(=) => assign
    generic :: operator(+) => add
    generic :: operator(-) => sub_datetime
    generic :: operator(-) => sub_timedelta
    generic :: operator(==) => eq
    generic :: operator(/=) => neq
    generic :: operator(>) => gt
    generic :: operator(>=) => ge
    generic :: operator(<) => lt
    generic :: operator(<=) => le
  end type datetime_type

  interface create_datetime
    module procedure datetime_1
    module procedure datetime_2
  end interface create_datetime

  interface set_datetime
    module procedure datetime_1
    module procedure datetime_2
  end interface set_datetime

contains
  
  subroutine init(this)
  
    class(datetime_type), intent(inout) :: this
    
    integer  values(8)
  
    call date_and_time(VALUES=values)
   
    this%year = values(1)
    this%month = values(2)
    this%day = values(3)
    this%hour = values(5)
    this%minute = values(6)
    this%second = values(7)
    this%millisecond = values(8)
  
  end subroutine init

  type(datetime_type) function datetime_1( &
      year,  month,  day,  hour,  minute, second, millisecond, &
                     days, hours, minutes, &
      timestamp, &
      timezone) result(res)

    integer, intent(in), optional :: year
    integer, intent(in), optional :: month
    integer, intent(in), optional :: day
    integer, intent(in), optional :: hour
    integer, intent(in), optional :: minute
    integer, intent(in), optional :: second
    integer, intent(in), optional :: millisecond
    integer, intent(in), optional :: days
    integer, intent(in), optional :: hours
    integer, intent(in), optional :: minutes
    class(*), intent(in), optional :: timestamp
    class(*), intent(in), optional :: timezone

    real(8) residue_seconds

    if (present(timestamp)) then
      ! Assume the start date time is UTC 1970-01-01 00:00:00.
      res%year = 1970
      res%month = 1
      res%day = 1
      res%hour = 0
      res%minute = 0
      res%second = 0
      res%millisecond = 0
      select type (timestamp)
      type is (integer)
        residue_seconds = timestamp
      type is (real(4))
        residue_seconds = timestamp
      type is (real(8))
        residue_seconds = timestamp
      end select
      call res%add_days(int(residue_seconds / 86400.0))
      residue_seconds = mod(residue_seconds, 86400.0)
      call res%add_hours(int(residue_seconds / 3600.0))
      residue_seconds = mod(residue_seconds, 3600.0)
      call res%add_minutes(int(residue_seconds / 60.0))
      residue_seconds = mod(residue_seconds, 60.0)
      call res%add_seconds(int(residue_seconds))
      call res%add_milliseconds((residue_seconds - int(residue_seconds)) * 1000)
    else
      if (present(year))        res%year        = year
      if (present(month))       res%month       = month
      if (present(day))         res%day         = day
      if (present(hour))        res%hour        = hour
      if (present(minute))      res%minute      = minute
      if (present(second))      res%second      = second
      if (present(millisecond)) res%millisecond = millisecond
      if (present(days))        call res%add_days(days)
      if (present(hours))       call res%add_hours(hours)
      if (present(minutes))     call res%add_minutes(minutes)
    end if
    if (present(timezone)) then
      select type (timezone)
      type is (integer)
        res%timezone = timezone
      type is (real(4))
        res%timezone = timezone
      type is (real(8))
        res%timezone = timezone
      end select
    end if

  end function datetime_1

  type(datetime_type) function datetime_2(datetime_str, format_str) result(res)

    character(*), intent(in) :: datetime_str
    character(*), intent(in), optional :: format_str

    integer i, j, num_spec
    character(1), allocatable :: specs(:) ! Date time element specifiers (e.g. 'Y', 'm', 'd')

    if (present(format_str)) then
      num_spec = 0
      i = 1
      do while (i <= len_trim(format_str))
        if (format_str(i:i) == '%') then
          ! % character consumes 1 character specifier.
          num_spec = num_spec + 1
          i = i + 2
        else
          num_spec = num_spec + 1
          i = i + 1
        end if
      end do

      allocate(specs(num_spec))

      i = 1
      j = 0
      do while (i <= len_trim(format_str))
        if (format_str(i:i) == '%') then
          i = i + 1
          j = j + 1
          specs(j:j) = format_str(i:i)
        else
          j = j + 1
          specs(j:j) = format_str(i:i)
        end if
        i = i + 1
      end do

      i = 1
      do j = 1, num_spec
        select case (specs(j))
        case ('Y')
          read(datetime_str(i:i+3), '(I4)') res%year
          i = i + 4
        case ('m')
          read(datetime_str(i:i+1), '(I2)') res%month
          i = i + 2
        case ('d')
          read(datetime_str(i:i+1), '(I2)') res%day
          i = i + 2
        case ('H')
          read(datetime_str(i:i+1), '(I2)') res%hour
          i = i + 2
        case ('M')
          read(datetime_str(i:i+1), '(I2)') res%minute
          i = i + 2
        case ('S')
          read(datetime_str(i:i+1), '(I2)') res%second
          i = i + 2
        case default
          i = i + 1
        end select
      end do
    else
      ! TODO: I assume UTC time for the time being.
      read(datetime_str(1:4), '(I4)') res%year
      read(datetime_str(6:7), '(I2)') res%month
      read(datetime_str(9:10), '(I2)') res%day
      read(datetime_str(12:13), '(I2)') res%hour
      read(datetime_str(15:16), '(I2)') res%minute
      read(datetime_str(18:19), '(I2)') res%second
    end if

  end function datetime_2

  function isoformat(this) result(res)

    class(datetime_type), intent(in) :: this
    character(30) res

    write(res, "(I4.4, '-', I2.2, '-', I2.2, 'T', I2.2, ':', I2.2, ':', I2.2, 'Z')") &
      this%year, this%month, this%day, this%hour, this%minute, this%second

  end function isoformat

  function timestamp(this)

    class(datetime_type), intent(in) :: this
    real(8) timestamp

    type(timedelta_type) dt

    dt = this - create_datetime(1970)
    timestamp = dt%total_seconds()

  end function timestamp

  function format(this, format_str) result(res)

    class(datetime_type), intent(in) :: this
    character(*), intent(in) :: format_str
    character(:), allocatable :: res

    character(100) tmp
    integer i, j

    tmp = ''
    i = 1
    j = 1
    do while (i <= len_trim(format_str))
      if (format_str(i:i) == '%') then
        i = i + 1
        select case (format_str(i:i))
        case ('Y')
          write(tmp(j:j+3), '(I4.4)') this%year
          j = j + 4
        case ('y')
          write(tmp(j:j+1), '(I2.2)') mod(this%year, 100)
          j = j + 2
        case ('j')
          write(tmp(j:j+2), '(I3.3)') this%days_in_year()
          j = j + 3
        case ('m')
          write(tmp(j:j+1), '(I2.2)') this%month
          j = j + 2
        case ('d')
          write(tmp(j:j+1), '(I2.2)') this%day
          j = j + 2
        case ('H')
          write(tmp(j:j+1), '(I2.2)') this%hour
          j = j + 2
        case ('M')
          write(tmp(j:j+1), '(I2.2)') this%minute
          j = j + 2
        case ('S')
          write(tmp(j:j+1), '(I2.2)') this%second
          j = j + 2
        end select
      else
        write(tmp(j:j), '(A1)') format_str(i:i)
        j = j + 1
      end if
      i = i + 1
    end do
    res = trim(tmp)

  end function format

  pure subroutine add_months(this, months)

    class(datetime_type), intent(inout) :: this
    integer, intent(in) :: months

    this%month = this%month + months

    if (this%month > 12) then
      this%year = this%year + this%month / 12
      this%month = mod(this%month, 12)
    else if (this%month < 1) then
      this%year = this%year + this%month / 12 - 1
      this%month = 12 + mod(this%month, 12)
    end if

  end subroutine add_months

  pure subroutine add_days(this, days)

    class(datetime_type), intent(inout) :: this
    class(*), intent(in) :: days

    real(8) residue_days
    integer month_days

    select type (days)
    type is (integer)
      residue_days = 0
      this%day = this%day + days
    type is (real(4))
      residue_days = days - int(days)
      this%day = this%day + days
    type is (real(8))
      residue_days = days - int(days)
      this%day = this%day + days
    end select

    if (residue_days /= 0) then
      call this%add_hours(residue_days * 24)
    end if

    do
      if (this%day < 1) then
        call this%add_months(-1)
        month_days = days_of_month(this%year, this%month)
        this%day = this%day + month_days
      else
        month_days = days_of_month(this%year, this%month)
        if (this%day > month_days) then
          call this%add_months(1)
          this%day = this%day - month_days
        else
          exit
        end if
      end if
    end do

  end subroutine add_days

  pure subroutine add_hours(this, hours)

    class(datetime_type), intent(inout) :: this
    class(*), intent(in) :: hours

    real(8) residue_hours

    select type (hours)
    type is (integer)
      residue_hours = 0
      this%hour = this%hour + hours
    type is (real(4))
      residue_hours = hours - int(hours)
      this%hour = this%hour + hours
    type is (real(8))
      residue_hours = hours - int(hours)
      this%hour = this%hour + hours
    end select

    if (residue_hours /= 0) then
      call this%add_minutes(residue_hours * 60)
    end if

    if (this%hour >= 24) then
      call this%add_days(this%hour / 24)
      this%hour = mod(this%hour, 24)
    else if (this%hour < 0) then
      if (mod(this%hour, 24) == 0) then
        call this%add_days(this%hour / 24)
        this%hour = 0
      else
        call this%add_days(this%hour / 24 - 1)
        this%hour = mod(this%hour, 24) + 24
      end if
    end if

  end subroutine add_hours

  pure subroutine add_minutes(this, minutes)

    class(datetime_type), intent(inout) :: this
    class(*), intent(in) :: minutes

    real(8) residue_minutes

    select type (minutes)
    type is (integer)
      residue_minutes = 0
      this%minute = this%minute + minutes
    type is (real(4))
      residue_minutes = minutes - int(minutes)
      this%minute = this%minute + minutes
    type is (real(8))
      residue_minutes = minutes - int(minutes)
      this%minute = this%minute + minutes
    end select

    if (residue_minutes /= 0) then
      call this%add_seconds(residue_minutes * 60)
    end if

    if (this%minute >= 60) then
      call this%add_hours(this%minute / 60)
      this%minute = mod(this%minute, 60)
    else if (this%minute < 0) then
      if (mod(this%minute, 60) == 0) then
        call this%add_hours(this%minute / 60)
        this%minute = 0
      else
        call this%add_hours(this%minute / 60 - 1)
        this%minute = mod(this%minute, 60) + 60
      end if
    end if

  end subroutine add_minutes

  pure subroutine add_seconds(this, seconds)

    class(datetime_type), intent(inout) :: this
    class(*), intent(in) :: seconds

    real(8) residue_seconds

    select type (seconds)
    type is (integer)
      residue_seconds = 0
      this%second = this%second + seconds
    type is (real(4))
      residue_seconds = seconds - int(seconds)
      this%second = this%second + seconds
    type is (real(8))
      residue_seconds = seconds - int(seconds)
      this%second = this%second + seconds
    end select

    if (residue_seconds /= 0) then
      call this%add_milliseconds(residue_seconds * 1000)
    end if

    if (this%second >= 60) then
      call this%add_minutes(this%second / 60)
      this%second = mod(this%second, 60)
    else if (this%second < 0) then
      if (mod(this%second, 60) == 0) then
        call this%add_minutes(this%second / 60)
        this%second = 0
      else
        call this%add_minutes(this%second / 60 - 1)
        this%second = mod(this%second, 60) + 60
      end if
    end if

  end subroutine add_seconds

  pure subroutine add_milliseconds(this, milliseconds)

    class(datetime_type), intent(inout) :: this
    class(*), intent(in) :: milliseconds

    select type (milliseconds)
    type is (integer)
      this%millisecond = this%millisecond + milliseconds
    type is (real(4))
      this%millisecond = this%millisecond + milliseconds
    type is (real(8))
      this%millisecond = this%millisecond + milliseconds
    end select

    if (this%millisecond >= 1000) then
      call this%add_seconds(int(this%millisecond / 1000))
      this%millisecond = mod(this%millisecond, 1000.0)
    else if (this%millisecond < 0) then
      if (mod(this%millisecond, 1000.0) == 0) then
        call this%add_seconds(int(this%millisecond / 1000))
        this%millisecond = 0
      else
        call this%add_seconds(int(this%millisecond / 1000) - 1)
        this%millisecond = mod(this%millisecond, 1000.0d0) + 1000
      end if
    end if

  end subroutine add_milliseconds

  pure integer function days_in_year(this) result(res)

    class(datetime_type), intent(in) :: this

    integer month

    res = 0
    do month = 1, this%month - 1
      res = res + days_of_month(this%year, month)
    end do
    res = res + this%day

  end function days_in_year

  pure elemental subroutine assign(this, other)

    class(datetime_type), intent(inout) :: this
    class(datetime_type), intent(in) :: other

    this%year        = other%year
    this%month       = other%month
    this%day         = other%day
    this%hour        = other%hour
    this%minute      = other%minute
    this%second      = other%second
    this%millisecond = other%millisecond

  end subroutine assign

  elemental type(datetime_type) function add(this, td) result(res)

    class(datetime_type), intent(in) :: this
    type(timedelta_type), intent(in) :: td

    res = this
    call res%add_milliseconds(td%milliseconds)
    call res%add_seconds(td%seconds)
    call res%add_minutes(td%minutes)
    call res%add_hours(td%hours)
    call res%add_days(td%days)
    call res%add_months(td%months)

  end function add

  pure elemental type(datetime_type) function sub_timedelta(this, td) result(res)

    class(datetime_type), intent(in) :: this
    type(timedelta_type), intent(in) :: td

    res = this
    call res%add_milliseconds(-td%milliseconds)
    call res%add_seconds(-td%seconds)
    call res%add_minutes(-td%minutes)
    call res%add_hours(-td%hours)
    call res%add_days(-td%days)

  end function sub_timedelta

  type(timedelta_type) recursive function sub_datetime(this, other) result(res)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    integer year, month, days, hours, minutes, seconds
    real(8) milliseconds

    days = 0
    hours = 0
    minutes = 0
    seconds = 0
    milliseconds = 0

    if (this >= other) then
      if (this%year == other%year) then
        if (this%month == other%month) then
          if (this%day == other%day) then
            if (this%hour == other%hour) then
              if (this%minute == other%minute) then
                if (this%second == other%second) then
                  milliseconds = milliseconds + this%millisecond - other%millisecond
                else
                  seconds = seconds + this%second - other%second - 1
                  milliseconds = milliseconds + 1000 - other%millisecond
                  milliseconds = milliseconds + this%millisecond
                end if
              else
                minutes = minutes + this%minute - other%minute - 1
                seconds = seconds + 60 - other%second - 1
                seconds = seconds + this%second
                milliseconds = milliseconds + 1000 - other%millisecond
                milliseconds = milliseconds + this%millisecond
              end if
            else
              hours = hours + this%hour - other%hour - 1
              minutes = minutes + 60 - other%minute - 1
              minutes = minutes + this%minute
              seconds = seconds + 60 - other%second - 1
              seconds = seconds + this%second
              milliseconds = milliseconds + 1000 - other%millisecond
              milliseconds = milliseconds + this%millisecond
            end if
          else
            days = days + this%day - other%day - 1
            hours = hours + 24 - other%hour - 1
            hours = hours + this%hour
            minutes = minutes + 60 - other%minute - 1
            minutes = minutes + this%minute
            seconds = seconds + 60 - other%second - 1
            seconds = seconds + this%second
            milliseconds = milliseconds + 1000 - other%millisecond
            milliseconds = milliseconds + this%millisecond
          end if
        else
          do month = other%month + 1, this%month - 1
            days = days + days_of_month(this%year, month)
          end do
          days = days + days_of_month(other%year, other%month) - other%day - 1
          days = days + this%day
          hours = hours + 24 - other%hour - 1
          hours = hours + this%hour
          minutes = minutes + 60 - other%minute - 1
          minutes = minutes + this%minute
          seconds = seconds + 60 - other%second - 1
          seconds = seconds + this%second
          milliseconds = milliseconds + 1000 - other%millisecond
          milliseconds = milliseconds + this%millisecond
        end if
      else
        do year = other%year + 1, this%year - 1
          days = days + 365 + merge(1, 0, is_leap_year(year))
        end do
        do month = other%month + 1, 12
          days = days + days_of_month(other%year, month)
        end do
        do month = 1, this%month - 1
          days = days + days_of_month(this%year, month)
        end do
        days = days + days_of_month(other%year, other%month) - other%day - 1
        days = days + this%day
        hours = hours + 24 - other%hour - 1
        hours = hours + this%hour
        minutes = minutes + 60 - other%minute - 1
        minutes = minutes + this%minute
        seconds = seconds + 60 - other%second - 1
        seconds = seconds + this%second
        milliseconds = milliseconds + 1000 - other%millisecond
        milliseconds = milliseconds + this%millisecond
      end if
      ! Carry over.
      if (milliseconds >= 1000) then
        milliseconds = milliseconds - 1000
        seconds = seconds + 1
      end if
      if (seconds >= 60) then
        seconds = seconds - 60
        minutes = minutes + 1
      end if
      if (minutes >= 60) then
        minutes = minutes - 60
        hours = hours + 1
      end if
      if (hours >= 24) then
        hours = hours - 24
        days = days + 1
      end if
      res = timedelta(days=days, hours=hours, minutes=minutes, seconds=seconds, milliseconds=milliseconds)
    else
      res = sub_datetime(other, this)
      res = res%negate()
    end if

  end function sub_datetime

  pure elemental logical function eq(this, other)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    eq = this%year        == other%year   .and. &
         this%month       == other%month  .and. &
         this%day         == other%day    .and. &
         this%hour        == other%hour   .and. &
         this%minute      == other%minute .and. &
         this%second      == other%second .and. &
         this%millisecond == other%millisecond

  end function eq

  pure elemental logical function neq(this, other)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    neq = .not. this == other

  end function neq

  pure elemental logical function gt(this, other)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    if (this%year < other%year) then
      gt = .false.
      return
    else if (this%year > other%year) then
      gt = .true.
      return
    end if

    if (this%month < other%month) then
      gt = .false.
      return
    else if (this%month > other%month) then
      gt = .true.
      return
    end if

    if (this%day < other%day) then
      gt = .false.
      return
    else if (this%day > other%day) then
      gt = .true.
      return
    end if

    if (this%hour < other%hour) then
      gt = .false.
      return
    else if (this%hour > other%hour) then
      gt = .true.
      return
    end if

    if (this%minute < other%minute) then
      gt = .false.
      return
    else if (this%minute > other%minute) then
      gt = .true.
      return
    end if

    if (this%second < other%second) then
      gt = .false.
      return
    else if (this%second > other%second) then
      gt = .true.
      return
    end if

    if (this%millisecond < other%millisecond) then
      gt = .false.
      return
    else if (this%millisecond < other%millisecond) then
      gt = .true.
      return
    end if

    gt = this /= other

  end function gt

  pure elemental logical function ge(this, other)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    ge = this > other .or. this == other

  end function ge

  pure elemental logical function lt(this, other)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    lt = other > this

  end function lt

  pure elemental logical function le(this, other)

    class(datetime_type), intent(in) :: this
    class(datetime_type), intent(in) :: other

    le = other > this .or. this == other

  end function le

  pure integer function days_of_month(year, month) result(res)

    integer, intent(in) :: year
    integer, intent(in) :: month

    integer, parameter :: days(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    if (month == 2 .and. is_leap_year(year)) then
      res = 29
    else
      res = days(month)
    end if

  end function days_of_month

  pure integer function days_of_year(year) result(res)

    integer, intent(in) :: year

    if (is_leap_year(year)) then
      res = 366
    else
      res = 365
    end if

  end function days_of_year

  pure logical function is_leap_year(year) result(res)

    integer, intent(in) :: year

    res = (mod(year, 4) == 0 .and. .not. mod(year, 100) == 0) .or. (mod(year, 400) == 0)

  end function is_leap_year

end module datetime_mod
