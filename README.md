<a name="top"></a>
# Fortran Date Time Library

## Content

+ [Overview](#overview)

+ [Installation](#installation)

+ [Example](#example)

+ [Compiler Support](#compiler-support)

+ [License](#license)

## Overview
This is a Fortran datetime library for doing tedious datetime operations.

<sub>Go to [Top](#top)</sub>

## Installation
A CMake-Setup is provided.

<sub>Go to [Top](#top)</sub>

## Example

```
use datetime

type(datetime_type) a, b
type(timedelta_type) dt

a = datetime(year=2017, month=10, day=6, hour=14)

write(6, *) a%isoformat() ! => 2017-10-06T14:00:00Z

dt = timedelta(minutes=6)
b = a + dt

write(6, *) a%isoformat() ! => 2017-10-06T14:06:00Z

b = datetime(year=2018, month=4, day=16, hour=23, minute=51)

dt = b - a

write(6, *) dt%total_seconds() ! => 16624260.0
write(6, *) dt%total_minutes() ! => 277071.0
write(6, *) dt%total_hours()   ! => 4617.85
write(6, *) dt%total_days()    ! => 192.4104166666667

```

## Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v17.0.2.187+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()

<sub>Go to [Top](#top)</sub>

## License
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg)]()

<sub>Go to [Top](#top)</sub>
