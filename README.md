## Overview

`uintN_t` is a type used to represent a large unsigned integer with a fixed number of bits.
Implement all base operations for integers (`+`, `-`, `*` and etc.).

## Using in project

Just include the file `uintN_t.hpp`.  
Create a variable with type uintN_t<*bits*> where *bits* - integer width (32, 64, 128, ...).

Ways of create object:
- using literal suffix `_Ui` + *bits* from namespace `uintN_t_literals`
- initialize with `{0, 0, ...}` i.e. array of digits (C++14 and later)
- create from 'short' number or array of digits with use `create_uintN_t` with template parameter of *bits*

## Example

``` cpp
#include "uintN_t.hpp"
#include <iostream>
using namespace uintN_t_literals;

int main() {
    uintN_t<128> n = 12345_Ui128;
    // or n = {12345}
    // or n = create_uintN_t<128>(12345)
    std::cout << n << '\n';
}
```

## STL support

- `std::numeric_limits`
- `std::hash`
- `std::to_string`
- `std::to_chars`
- `std::ostream.operator<<`
- `std::istream.operator>>`
- `std::swap`

## Conversions

- to `bool`
- to `digit_t` (explicit)
- to `extend_digit_t` (explicit)
- to `uintN_t` with greater width
- to `uintN_t` with less width (explicit)
- to `std::string` from `std::to_string` or `std::to_chars` or ostream

## TODO

- [x] Add support for C++11
- [x] Add literal check in operator
- [ ] Full implement Toom-Cook algorithm with k = 4
- [ ] Add conversion from string or istream (P.S.: istream is done)