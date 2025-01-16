## Overview

`uintN_t` is a type used to represent a large unsigned integer with a fixed number of bits.
Implement all base operations for integers (`+`, `-`, `*` and etc.).

## Using in project

Just include the file `uintN_t.hpp`.  
Create a variable with type uintN_t<*bits*, *digit*>
or use base size aliases (uint*X*_t, *X* = 128, 256, 512, 1024)
where:
- *bits* - integer width (8, 16, 32, 64, 128, 256, etc.)
- *digit* - unsigned integer type for represent digit (width equal 8, 16, 32 and 64[^1])

Ways of create object:
- using literal suffix `_Ui` + *bits* from namespace `uintN_t_literals`
- initialize with `{0, 0, ...}` i.e. array of digits (C++14 and later)
- create from 'short' number or array of digits with use `create_uintN_t` with template parameter of *bits* and *digit*
- using string [conversions](#Conversions)

## Example

``` cpp
#include "uintN_t.hpp"
#include <iostream>
using namespace uintN_t_literals;

int main() {
    auto n = 12345_Ui128;
    // or n = uintN_t<128, uint32_t>{12345}
    // or n = create_uintN_t<128, uint32_t>(12345)
    std::cout << n << '\n';
}
```

## Standard library support

- `std::numeric_limits`
- `std::hash`
- `std::to_string`
- `std::strtoumax`[^2]
- `std::to_chars`
- `std::from_chars`
- `std::ostream.operator<<`
- `std::istream.operator>>`
- `std::swap`

## Conversions

- to `bool`
- to `digit_t` (explicit)
- to `extend_digit_t` (explicit)
- to `uintN_t` with greater width
- to `uintN_t` with less width (explicit)
- to `uintN_t` with same width and other digit type by `to_another_digits`
- to string by `std::to_string` or `std::to_chars` or `std::ostream`
- from string by `std::strtoumax` or `std::from_chars` or `std::istream`

## TODO

- [x] Add support for C++11
- [x] Add literal check in operator
- [ ] Full implement Toom-Cook algorithm with k = 4
- [x] Add conversion from string or istream
- [x] Add support of different digit type

[^1]: If your compiler support `__uint128_t` and macros `__SIZEOF_INT128__`
[^2]: Overloaded by template parameter