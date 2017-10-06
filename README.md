Introduction
============

This is a Fortran datetime library for doing tedious datetime operations.

Examples
========

```
use datetime_mod
use timedelta_mod

type(datetime_type) a, b
type(timedelta_type) dt

a = datetime(year=2017, month=10, day=6, hour=14)

write(6, *) a%isoformat() ! => 2017-10-06T14:00:00Z

dt = timedelta(minutes=6)
b = a + dt

write(6, *) a%isoformat() ! => 2017-10-06T14:06:00Z

```