/* uintN_t - integer type for large numbers. C++11 and later */
#ifndef UINTN_T_HPP
#define UINTN_T_HPP

#include <cstddef>
#include <cstdint>

#if __cpp_impl_three_way_comparison >= 201907L
#include <compare>
#endif

#if __cplusplus >= 201402L
#define CONSTEXPR_GREATER_CXX11 constexpr
#else
#define CONSTEXPR_GREATER_CXX11
#endif

/* Info: for macros use namespace prefix 'evs' */
/* Comment: I don't want write full for-loop */
#define evsIRANGE(i_var, max_val) \
for (size_t i_var = 0; i_var < max_val; ++i_var)

namespace detail {

/* Comment: I don't want include all header <type_traits> */
template <bool B, class T> struct enable_if {};
template <class T>
struct enable_if<true, T> { using type = T; };
template <bool B, class T>
using enable_if_t = typename enable_if<B, T>::type;

#define evsENABLE(cond) \
::detail::enable_if_t<(cond), int> = 0

constexpr uint64_t merge_32_to_64(uint32_t low, uint32_t high) noexcept {
    return static_cast<uint64_t>(high) << 32 | low;
}

CONSTEXPR_GREATER_CXX11 void split_64_to_32(
    uint64_t num, uint32_t& low, uint32_t& high) noexcept {
    low  = static_cast<uint32_t>(num);
    high = static_cast<uint32_t>(num >> 32);
}

} // namespace detail

/**
 * @brief  Integer type for large numbers, is aggregate type
 * @tparam bits count of bits in number
 */
template <size_t bits>
struct uintN_t {
    using        digit_t = uint32_t;
    using extend_digit_t = uint64_t;
    static constexpr size_t digit_width = sizeof(digit_t) * 8;
    static constexpr size_t digit_count = bits / digit_width;

    static_assert(bits >= digit_width && !(bits & (bits - 1)),
        "Incorrect bit count argument, need power of 2");

    digit_t digits[digit_count] {};

    /* Conversions */

    CONSTEXPR_GREATER_CXX11 operator bool() const noexcept {
        evsIRANGE(i, digit_count)
            if (digits[i])
                return true;
        return false;
    }

    constexpr explicit operator digit_t() const noexcept {
        return digits[0];
    }
    constexpr explicit operator extend_digit_t() const noexcept {
        return detail::merge_32_to_64(digits[0], digits[1]);
    }

    /* Widening conversion (1 -> 01) */
    template <size_t other_bits, evsENABLE(other_bits > bits)>
    CONSTEXPR_GREATER_CXX11 operator uintN_t<other_bits>() const noexcept {
        uintN_t<other_bits> out;
        evsIRANGE(i, digit_count)
            out.digits[i] = digits[i];
        return out;
    }

    /* Narrowing conversion (12 -> 2) */
    template <size_t other_bits, evsENABLE(other_bits < bits)>
    CONSTEXPR_GREATER_CXX11 explicit operator uintN_t<other_bits>() const noexcept {
        uintN_t<other_bits> out;
        evsIRANGE(i, out.digit_count)
            out.digits[i] = digits[i];
        return out;
    }

    /* Assign functions */

    /**
     * @brief  Add `rhs` to number and save result in number
     * @param  rhs second number for addition
     * @return Carrying from addition
     */
    CONSTEXPR_GREATER_CXX11 bool assign_add(const uintN_t& rhs) noexcept {
        bool carry = false;
        evsIRANGE(i, digit_count) {
            digit_t old_lhs_i_val = digits[i];
            digits[i] = old_lhs_i_val + rhs.digits[i] + carry;
            carry = carry
                ? (digits[i] <= old_lhs_i_val)
                : (digits[i] <  old_lhs_i_val);
        }
        return carry;
    }

    /**
     * @brief  Add `rhs` to number and save result in number
     * @param  rhs second number for addition
     * @return Carrying from addition
     */
    CONSTEXPR_GREATER_CXX11 bool assign_add(digit_t rhs) noexcept {
        digit_t old_lhs_val = digits[0];
        digits[0] += rhs;
        bool carry = digits[0] < old_lhs_val;
        for (size_t i = 1; carry && i < digit_count; i++) {
            old_lhs_val = digits[i];
            carry = ++digits[i] < old_lhs_val;
        }
        return carry;
    }

    /**
     * @brief Shift bits to left by `shift`
     * @param shift bit shift, must be in range [`1`, `digit_width` - `1`]
     */
    CONSTEXPR_GREATER_CXX11 void small_shift_left(size_t shift) noexcept {
        if (!shift || shift >= digit_width) return;
        for (size_t i = digit_count - 1; i > 0; i--)
            digits[i] = digits[i] << shift | digits[i - 1] >> (digit_width - shift);
        digits[0] <<= shift;
    }
    /**
     * @brief Shift bits to right by `shift`
     * @param shift bit shift, must be in range [`1`, `digit_width` - `1`]
     */
    CONSTEXPR_GREATER_CXX11 void small_shift_right(size_t shift) noexcept {
        if (!shift || shift >= digit_width) return;
        evsIRANGE(i, digit_count - 1)
            digits[i] = digits[i] >> shift | digits[i + 1] << (digit_width - shift);
        digits[digit_count - 1] >>= shift;
    }

    /**
     * @brief Shift digit to left by `shift`,
     *        equal shift by `digit_width` * `shift`
     * @param shift digit shift, must be in range [`1`, `digit_count` - `1`]
     */
    CONSTEXPR_GREATER_CXX11 void digit_shift_left(size_t shift) noexcept {
        if (shift >= digit_count) return clear();
        for (size_t i = digit_count - 1; i > shift - 1; i--)
            digits[i] = digits[i - shift];
        evsIRANGE(i, shift) digits[i] = 0;
    }
    /**
     * @brief Shift digit to right by `shift`,
     *        equal shift by `digit_width` * `shift`
     * @param shift digit shift, must be in range [`1`, `digit_count` - `1`]
     */
    CONSTEXPR_GREATER_CXX11 void digit_shift_right(size_t shift) noexcept {
        if (shift >= digit_count) return clear();
        evsIRANGE(i, digit_count - shift)
            digits[i] = digits[i + shift];
        for (size_t i = digit_count - 1; i > digit_count - shift - 1; i--)
            digits[i] = 0;
    }

    /* Unary operators */

    constexpr uintN_t operator+() const noexcept { return *this; }
    CONSTEXPR_GREATER_CXX11 uintN_t operator-() const noexcept {
        uintN_t out = ~(*this);
        out.assign_add(1);
        return out;
    }
    CONSTEXPR_GREATER_CXX11 uintN_t operator~() const noexcept {
        uintN_t out = *this;
        evsIRANGE(i, digit_count)
            out.digits[i] = ~out.digits[i];
        return out;
    }

    /* Increment/Decrement */

    CONSTEXPR_GREATER_CXX11 uintN_t& operator++() noexcept { return *this += 1; }
    CONSTEXPR_GREATER_CXX11 uintN_t& operator--() noexcept {
#if __cplusplus >= 201402L
        return *this -= {1};
#else
        uintN_t one; one.digits[0] = 1;
        return *this -= one;
#endif
    }

    CONSTEXPR_GREATER_CXX11 uintN_t operator++(int) noexcept {
        uintN_t out = *this; ++(*this);
        return out;
    }
    CONSTEXPR_GREATER_CXX11 uintN_t operator--(int) noexcept {
        uintN_t out = *this; --(*this);
        return out;
    }

    /* Binary-assign operators */

    CONSTEXPR_GREATER_CXX11 uintN_t& operator+=(const uintN_t& rhs) noexcept {
        assign_add(rhs);
        return *this;
    }
    CONSTEXPR_GREATER_CXX11 uintN_t& operator+=(digit_t rhs) noexcept {
        assign_add(rhs);
        return *this;
    }
    CONSTEXPR_GREATER_CXX11 uintN_t& operator-=(const uintN_t& rhs) noexcept {
        return *this += -rhs;
    }

#define evsBITWISE_ASGOPER_TMPL(op)                                             \
    CONSTEXPR_GREATER_CXX11 uintN_t& operator op(const uintN_t& rhs) noexcept { \
        evsIRANGE(i, digit_count) digits[i] op rhs.digits[i];                   \
        return *this; }
    
    evsBITWISE_ASGOPER_TMPL(&=)
    evsBITWISE_ASGOPER_TMPL(|=)
    evsBITWISE_ASGOPER_TMPL(^=)

#undef evsBITWISE_ASGOPER_TMPL

    CONSTEXPR_GREATER_CXX11 uintN_t& operator<<=(size_t shift) noexcept {
        digit_shift_left(shift / digit_width);
        small_shift_left(shift % digit_width);
        return *this;
    }
    CONSTEXPR_GREATER_CXX11 uintN_t& operator>>=(size_t shift) noexcept {
        digit_shift_right(shift / digit_width);
        small_shift_right(shift % digit_width);
        return *this;
    }

    CONSTEXPR_GREATER_CXX11 uintN_t& operator<<=(int shift) noexcept {
        if (shift < 0) return *this >>= -shift;
        return *this <<= static_cast<size_t>(shift);
    }
    CONSTEXPR_GREATER_CXX11 uintN_t& operator>>=(int shift) noexcept {
        if (shift < 0) return *this <<= -shift;
        return *this >>= static_cast<size_t>(shift);
    }

    CONSTEXPR_GREATER_CXX11 uintN_t& operator*=(const uintN_t&) noexcept;
    CONSTEXPR_GREATER_CXX11 uintN_t& operator*=(uint16_t) noexcept;

    CONSTEXPR_GREATER_CXX11 uintN_t& operator/=(const uintN_t&) noexcept;
    CONSTEXPR_GREATER_CXX11 uintN_t& operator%=(const uintN_t&) noexcept;

    /* Binary operators */

#define evsBINOP_VIA_BINASGOP(op, param_type)                         \
    CONSTEXPR_GREATER_CXX11 uintN_t operator op(param_type rhs) const \
    noexcept { return uintN_t(*this) op ## = rhs; }

    evsBINOP_VIA_BINASGOP(+, const uintN_t&)
    evsBINOP_VIA_BINASGOP(+, digit_t)
    evsBINOP_VIA_BINASGOP(-, const uintN_t&)

    evsBINOP_VIA_BINASGOP(*, const uintN_t&)
    evsBINOP_VIA_BINASGOP(*, uint16_t)

    evsBINOP_VIA_BINASGOP(/, const uintN_t&)
    evsBINOP_VIA_BINASGOP(%, const uintN_t&)

    evsBINOP_VIA_BINASGOP(&, const uintN_t&)
    evsBINOP_VIA_BINASGOP(|, const uintN_t&)
    evsBINOP_VIA_BINASGOP(^, const uintN_t&)

    evsBINOP_VIA_BINASGOP(<<, size_t)
    evsBINOP_VIA_BINASGOP(>>, size_t)
    evsBINOP_VIA_BINASGOP(<<, int)
    evsBINOP_VIA_BINASGOP(>>, int)

#undef evsBINOP_VIA_BINASGOP

    /* Comparison operators */

    /**
     * @brief  Three-way comparison of a number with `rhs`
     * @param  rhs the number to be compared with
     * @return Int value as less (< 0), greater (> 0) and equal (== 0)
     */
    CONSTEXPR_GREATER_CXX11 int compare(const uintN_t& rhs) const noexcept {
        size_t i = digit_count - 1;
        while (i > 0 && digits[i] == rhs.digits[i]) --i;
        return (rhs.digits[i] < digits[i]) - (digits[i] < rhs.digits[i]);
    }

#define evsCMP_OPER_TMPL(op)                                     \
    CONSTEXPR_GREATER_CXX11 bool operator op(const uintN_t& rhs) \
    const noexcept { return compare(rhs) op 0; }

    evsCMP_OPER_TMPL(==)
#if !(__cpp_impl_three_way_comparison >= 201907L)
    evsCMP_OPER_TMPL(!=)
    evsCMP_OPER_TMPL(< )
    evsCMP_OPER_TMPL(> )
    evsCMP_OPER_TMPL(<=)
    evsCMP_OPER_TMPL(>=)
#else
    constexpr auto operator<=>(const uintN_t& rhs) const noexcept {
        const int cmp = compare(rhs);
        if (cmp < 0) return std::strong_ordering::less;
        if (cmp > 0) return std::strong_ordering::greater;
        return std::strong_ordering::equal;
    }
#endif

#undef evsCMP_OPER_TMPL

    /* Other functions */

    /**
     * @brief  Get sign of number
     * @return Int value as `-1` (negative),
     *         `1` (positive) and `0` (equal zero)
     */
    CONSTEXPR_GREATER_CXX11 int sign() const noexcept {
        const bool sign_bit =
            (digits[digit_count - 1] >> (digit_width - 1)) & 1;
        if (sign_bit) return -1;
        evsIRANGE(i, digit_count)
            if (digits[i]) return 1;
        return 0;
    }

    /// Set all digits equal zero, same as `*this = {0}`
    CONSTEXPR_GREATER_CXX11 void clear() noexcept {
        evsIRANGE(i, digit_count) digits[i] = 0;
    }

    /**
     * @brief  Get bit value from `pos`
     * @param  pos index of bit
     * @return Bit value in `pos`
     */
    CONSTEXPR_GREATER_CXX11 bool bit(size_t pos) const noexcept {
        if (pos >= bits) return false;
        const size_t digit_index = pos / digit_width;
        const size_t   bit_index = pos % digit_width;
        return digits[digit_index] >> bit_index & 1;
    }
    /**
     * @brief Set bit to passed value in `pos`
     * @param pos index of bit
     * @param value new value of bit
     */
    CONSTEXPR_GREATER_CXX11 void bit(size_t pos, bool value) noexcept {
        if (pos >= bits) return;
        const size_t digit_index = pos / digit_width;
        const size_t   bit_index = pos % digit_width;
        const digit_t mask = ~(1 << bit_index);
        (digits[digit_index] &= mask) |= digit_t{value} << bit_index;
    }

    /**
     * @brief  Get hex digit from `pos`
     * @param  pos index of hex digit
     * @return Hex digit in `pos`
     */
    CONSTEXPR_GREATER_CXX11 uint8_t hex_digit(size_t pos) const noexcept {
        if (pos >= bits / 4) return 0;
        const size_t digit_index = pos * 4 / digit_width;
        const size_t   hex_index = pos * 4 % digit_width;
        return digits[digit_index] >> hex_index & 15;
    }
    /**
     * @brief Set hex digit to passed value in `pos`
     * @param pos index of hex digit
     * @param value new value of hex digit
     */
    CONSTEXPR_GREATER_CXX11 void hex_digit(size_t pos, uint8_t value) noexcept {
        if (pos >= bits / 4) return;
        const size_t digit_index = pos * 4 / digit_width;
        const size_t   hex_index = pos * 4 % digit_width;
        const digit_t mask = ~(15 << hex_index);
        (digits[digit_index] &= mask) |= digit_t{value & 15U} << hex_index;
    }

    /**
     * @brief Split number to two numbers with width 2 times less
     * @param low lower part of original number
     * @param high higher part of original number
     */
    CONSTEXPR_GREATER_CXX11 void split(
        uintN_t<bits/2>& low,
        uintN_t<bits/2>& high
    ) const noexcept {
        static_assert(bits / 2 >= 32,
            "Impossible split 32-bit number to two 16-bit");
        size_t i = 0;
        for (; i < digit_count / 2; i++)
            low.digits[i] = digits[i];
        for (size_t base = i; i < digit_count; i++)
            high.digits[i - base] = digits[i];
    }

    /**
     * @brief Merge two numbers to number with width 2 times greater
     * @param low lower part of original number
     * @param high higher part of original number
     */
    CONSTEXPR_GREATER_CXX11 void merge(
        const uintN_t<bits/2>& low,
        const uintN_t<bits/2>& high
    ) noexcept {
        static_assert(bits / 2 >= 32,
            "Impossible merge two 16-bit number to 32-bit");
        size_t base = 0;
        evsIRANGE(i, low.digit_count)
            digits[base++] = low.digits[i];
        evsIRANGE(i, high.digit_count)
            digits[base++] = high.digits[i];
    }
}; // struct uintN_t

template <>
constexpr uintN_t<32>::operator uintN_t<32>::extend_digit_t()
const noexcept { return digits[0]; }

// Base size aliases
using uint128_t  = uintN_t<128>;
using uint256_t  = uintN_t<256>;
using uint512_t  = uintN_t<512>;
using uint1024_t = uintN_t<1024>;

namespace detail {

namespace multiplication {

template <size_t B>
CONSTEXPR_GREATER_CXX11 uintN_t<B*2> karatsuba(
    const uintN_t<B>& lhs,
    const uintN_t<B>& rhs
) noexcept {
    using half_num_t = uintN_t<B/2>;
    using doub_num_t = uintN_t<B*2>;

    half_num_t x0, x1; // lhs = x1 * 2^(B/2) + x0
    half_num_t y0, y1; // rhs = y1 * 2^(B/2) + y0
    lhs.split(x0, x1);
    rhs.split(y0, y1);

    /*
    out = z2 * 2^B + z1 * 2^(B/2) + z0
    z0 = x0 * y0
    z2 = x1 * y1
    z3 = (x1 + x0) * (y1 + y0)
             ^- x2       ^- y2
    z1 = z3 - z0 - z2
    */

    half_num_t x2 = x1;
    half_num_t y2 = y1;
    bool xc = x2.assign_add(x0);
    bool yc = y2.assign_add(y0);

    doub_num_t z0 = karatsuba(x0, y0);
    doub_num_t z2 = karatsuba(x1, y1);
    doub_num_t z3 = karatsuba(x2, y2);
    //              ^- return with width = B

    // if-blocks need because has overflow in x2 and y2
    if (xc) z3 += static_cast<doub_num_t>(y2) << B/2;
    if (yc) z3 += static_cast<doub_num_t>(x2) << B/2;
#if __cplusplus >= 201402L
    if (xc && yc) z3 += doub_num_t{1} << B;
#else
    doub_num_t one; one.digits[0] = 1;
    if (xc && yc) z3 += one << B;
#endif
    doub_num_t z1 = z3 - z2 - z0;
    
    doub_num_t out = z0;
    z1 <<= B/2; z2 <<= B;
    (out += z1) += z2;

    return out;
}

template <> // base variant for recursion
CONSTEXPR_GREATER_CXX11 uintN_t<64> karatsuba(
    const uintN_t<32>& lhs,
    const uintN_t<32>& rhs
) noexcept {
    uintN_t<32>::extend_digit_t res = 
        static_cast<uintN_t<32>::extend_digit_t>(lhs.digits[0]) *
        static_cast<uintN_t<32>::extend_digit_t>(rhs.digits[0]);
#if __cplusplus >= 201402L
    uintN_t<32>::digit_t l = 0, h = 0;
    detail::split_64_to_32(res, l, h);
    return {l, h};
#else
    uintN_t<64> out;
    detail::split_64_to_32(res,
        out.digits[0], out.digits[1]);
    return out;
#endif
}

/* Toom-4 algorithm explain
Math base:
P(t) = p_3*t^3 + p_2*t^2 + p_1*t + p_0
Q(t) = q_3*t^3 + q_2*t^2 + q_1*t + q_0
R(t) = r_6*t^6 + r_5*t^5 + r_4*t^4 + r_3*t^3 + r_2*t^2 + r_1*t + r_0
where
R(t) = P(t) * Q(t)

whereas
P(B/4) = lhs
Q(B/4) = rhs
therefore
R(B/4) = P(B/4) * Q(B/4) = lhs * rhs

1) Find values of R in 7 points (use recursive multiplication):
R(0) = P(0) * Q(0)
R(1) = P(1) * Q(1)
...
R(6) = P(6) * Q(6)

2) Calculation delta values of y
Δy^k_i = Δy^{k-1}_{i+1} - Δy^{k-1}_i
Δy^0_i = R(x_i) i.e. x_i = i, where i = [0, 6]
as triangle
y_0
    y_1 - y_0 = y^1_0
y_1                   y^1_1 - y^1_0 = y^2_0
    y_2 - y_1 = y^1_1                       ...
y_2                            ...
           ...
...

3) Calculation coefficients of Newton polynomial
c_i = y^i_0 / i!, where i = [0, 6]

N(x) =
    c_0 +
    c_1 * x +
    c_2 * x * (x - 1) +
    c_3 * x * (x - 1) * (x - 2) +
    c_4 * x * (x - 1) * (x - 2) * (x - 3) +
    c_5 * x * (x - 1) * (x - 2) * (x - 3) * (x - 4) +
    c_6 * x * (x - 1) * (x - 2) * (x - 3) * (x - 4) * (x - 5)

4) Calculation coefficients of R

c_6  c_5
     -5 * c_6
-------------
c_6  m0_0      c_4
     -4 * c_6  -4 * m0_0
------------------------
c_6  m1_0      m1_0       c_3
     -3 * c_6  -3 * m1_0  -3 * m1_1
-----------------------------------
c_6  m2_0      m2_1       m2_2       c_2
     -2 * c_6  -2 * m2_0  -2 * m2_1  -2 * m2_2
----------------------------------------------
c_6  m3_0      m3_1       m3_2       m3_3       c_1
     -1 * c_6  -1 * m3_0  -1 * m3_1  -1 * m3_2  -1 * m3_3
---------------------------------------------------------  c_0
r_6  r_5       r_4        r_3        r_2        r_1        r_0

5) Substitute B/4 in R and get result

literature: https://ido.tsu.ru/iop_res1/teorcrypto/text/1_34.html
*/

template <size_t B>
CONSTEXPR_GREATER_CXX11 uintN_t<B*2> _fast_mul( // same as russian_peasant
    const uintN_t<B>& lhs, uint16_t rhs) noexcept {
    uintN_t<B*2> out, left = lhs;
    while (rhs) {
        if (rhs & 1) out += left;
        left.small_shift_left(1);
        rhs >>= 1;
    }
    return out;
}

CONSTEXPR_GREATER_CXX11 uint16_t _ipow(uint16_t base, uint16_t exp) noexcept {
    uint16_t out = 1;
    evsIRANGE(i, exp) out *= base;
    return out;
}

#if __cplusplus >= 201402L
template <size_t B>
static constexpr uintN_t<B> fact_vals[7] = {
    {1}, {1}, {2}, {6}, {24}, {120}, {720}
};
#define FACT_VALUE(b, i) fact_vals<b>[i]
#else
template <size_t B>
uintN_t<B> fact_vals(size_t i) {
    uintN_t<B> out;
    switch (i) {
        default: out.digits[0] =   1; break;
        case 2:  out.digits[0] =   2; break;
        case 3:  out.digits[0] =   6; break;
        case 4:  out.digits[0] =  24; break;
        case 5:  out.digits[0] = 120; break;
        case 6:  out.digits[0] = 720; break;
    }
    return out;
}
#define FACT_VALUE(b, i) fact_vals<b>(i)
#endif

template <size_t B>
CONSTEXPR_GREATER_CXX11 uintN_t<B*2> toom_4(
    const uintN_t<B>& lhs,
    const uintN_t<B>& rhs
) noexcept {
    using doub_num_t = uintN_t<B*2>;
    using half_num_t = uintN_t<B/2>;
    using quar_num_t = uintN_t<B/4>;    

    half_num_t p01, p23, q01, q23;
    quar_num_t p[4] {}, q[4] {};

    // 0) Split numbers by coefficients
    lhs.split(p01, p23); rhs.split(q01, q23);
    p01.split(p[0], p[1]); p23.split(p[2], p[3]);
    q01.split(q[0], q[1]); q23.split(q[2], q[3]);

    // 1) Find values of R in 7 points
    half_num_t P_y[7] {}, Q_y[7] {};
    evsIRANGE(i, 7) {
        evsIRANGE(j, 4) {
            P_y[i] += _fast_mul(p[j], _ipow(i, j));
            Q_y[i] += _fast_mul(p[j], _ipow(i, j));
        }
    }
    doub_num_t R_y[7] {};
    evsIRANGE(i, 7) {
        quar_num_t P_y_low = static_cast<quar_num_t>(P_y[i]);
        quar_num_t Q_y_low = static_cast<quar_num_t>(Q_y[i]);
        uint32_t P_y_carrying = P_y[i].digits[half_num_t::digit_count / 2]; // max 16 bit
        uint32_t Q_y_carrying = Q_y[i].digits[half_num_t::digit_count / 2]; // max 16 bit
        R_y[i] = toom_4(P_y_low, Q_y_low);
        R_y[i] += static_cast<doub_num_t>(_fast_mul(Q_y_low, P_y_carrying)) << (B/4);
        R_y[i] += static_cast<doub_num_t>(_fast_mul(P_y_low, Q_y_carrying)) << (B/4);
#if __cplusplus >= 201402L
        R_y[i] += doub_num_t{P_y_carrying * Q_y_carrying} << (B/2);
#else
        doub_num_t carry; carry.digits[0] = P_y_carrying * Q_y_carrying;
        R_y[i] += carry << (B/2);
#endif
    }

    // 2,3) Calculation deltas and Newton coefficients
    doub_num_t Newton_coefs[7] {};
    doub_num_t y_deltas[7][7] {};
    evsIRANGE(i, 7) y_deltas[0][i] = R_y[i];
    for (size_t i = 1; i < 7; i++)
        for (size_t j = 0; j < 7 - i; j++)
            y_deltas[i][j] = y_deltas[i - 1][j + 1] - y_deltas[i - 1][j];
    evsIRANGE(i, 7)
        Newton_coefs[i] = y_deltas[i][0] / FACT_VALUE(B*2, i);

    // 4) Calculation coefficients of R
    doub_num_t intermed_calc[7] {};
    for (uint16_t base = 5, i = 2; base > 0; base--, i++) {
        for (size_t j = 1; j > i; j++)
            intermed_calc[6 - j] = static_cast<doub_num_t>(
                _fast_mul(Newton_coefs[7 - j], base));
        for (size_t j = 1; j > i; j++)
            Newton_coefs[6 - j] -= intermed_calc[6 - j];
    }

    // 5) Substitute B/4 in R and get result
    doub_num_t out;
    for (size_t i = 0, shift = 0; i < 7; i++, shift += B/4)
        out += Newton_coefs[i] << shift;

    return out;
}

template <> // base variant for recursion if 32
CONSTEXPR_GREATER_CXX11 uintN_t<64> toom_4(
    const uintN_t<32>& lhs,
    const uintN_t<32>& rhs
) noexcept {
    return karatsuba(lhs, rhs);
}

template <> // base variant for recursion if 64
CONSTEXPR_GREATER_CXX11 uintN_t<128> toom_4(
    const uintN_t<64>& lhs,
    const uintN_t<64>& rhs
) noexcept {
    return karatsuba(lhs, rhs);
}

template <size_t B>
CONSTEXPR_GREATER_CXX11 uintN_t<B*2> russian_peasant(
    const uintN_t<B>& lhs,
    const uintN_t<B>& rhs
) noexcept {
    uintN_t<B*2> out, left = lhs, right = rhs;
    while (right) {
        if (right.digits[0] & 1)
            out += left;
        left.small_shift_left(1);
        right.small_shift_right(1);
    }
    return out;
}

} // namespace multiplication

namespace division {

template <size_t B>
struct div_result_t {
    uintN_t<B> quotient;
    uintN_t<B> remainder;
};

// https://clck.ru/3FBwXQ (Wikipedia)
template <size_t B>
CONSTEXPR_GREATER_CXX11 div_result_t<B> prime(
    const uintN_t<B>& N,
    const uintN_t<B>& D
) noexcept {
    // Division by zero => return {0, 0}
    if (!D) return {};

    uintN_t<B> Q, R;
    for (size_t i = B; i--;) {
        R.small_shift_left(1);
        R.bit(0, N.bit(i));
        if (R >= D) {
            R -= D;
            Q.bit(i, 1);
        }
    }

    return {Q, R};
}

} // namespace division

CONSTEXPR_GREATER_CXX11 size_t cexpr_strlen(const char* str) noexcept {
    size_t count = 0;
    while (*str) ++str, ++count;
    return count;
}

CONSTEXPR_GREATER_CXX11 uint32_t char_to_digit(char ch) noexcept {
    if ('0' <= ch && ch <= '9')
        return ch - '0';
    if ('a' <= ch && ch <= 'z')
        return 10 + ch - 'a';
    if ('A' <= ch && ch <= 'Z')
        return 10 + ch - 'A';
    return -1;
}

static constexpr auto log10_2 = 0.3010299956639812L;

template <size_t B>
CONSTEXPR_GREATER_CXX11 uintN_t<B> from_literal(const char* literal) noexcept {
    enum class literal_base {
        bin = 1, oct = 3,
        dec = 0, hex = 4
    } base = literal_base::dec;

    if (*literal == '0') {
        base = literal_base::oct;
        ++literal;

        if (*literal == 'x' || *literal == 'X') {
            base = literal_base::hex;
            ++literal;
        } else if (*literal == 'b' || *literal == 'B') {
            base = literal_base::bin;
            ++literal;
        }
    }

    const size_t lit_len = cexpr_strlen(literal);
    bool exit_cond = false;
    switch (base) {
        case literal_base::bin: exit_cond = lit_len > B; break;
        case literal_base::hex: exit_cond = lit_len > B/4; break;
        case literal_base::dec:
        exit_cond = lit_len > static_cast<size_t>(B * log10_2) + 1;
        break;
        case literal_base::oct:
        exit_cond = lit_len > B/3 + 1 ||
            (lit_len == B/3 + 1 && *literal > '3');
        break;
    }
    if (exit_cond)
        return ~uintN_t<B>{};

    uintN_t<B> out, out_c;
    while (*literal) {
        const uint32_t next_digit = char_to_digit(*literal++);
        switch (base) {
            case literal_base::dec: {
                bool carry = false;
                out_c = out << 3;
                carry |= out.bit(B - 1) || out.bit(B - 2) || out.bit(B - 3);
                carry |= out_c.assign_add(out << 1);
                carry |= out_c.assign_add(next_digit);
                out = out_c;
                if (carry) return ~uintN_t<B>{};
            } break;
            default: {
                out.small_shift_left(static_cast<size_t>(base));
                out.digits[0] |= next_digit;
            } break;
        }
    }

    return out;
}

class uint64_with_carry {
public:
    CONSTEXPR_GREATER_CXX11 uint64_with_carry() : m_r(0), m_c(0) {}
    CONSTEXPR_GREATER_CXX11 uint64_with_carry(uint64_t n) : m_r(n), m_c(0) {}

    CONSTEXPR_GREATER_CXX11 uint8_t carry() const noexcept { return m_c; }
    CONSTEXPR_GREATER_CXX11 void carry(uint8_t v) noexcept { m_c = v & 1; }
    CONSTEXPR_GREATER_CXX11 void reg(uint64_t v) noexcept { m_r = v; }
    CONSTEXPR_GREATER_CXX11 uint32_t low() const noexcept {
        return static_cast<uint32_t>(m_r);
    }
    CONSTEXPR_GREATER_CXX11 uint32_t high() const noexcept {
        return static_cast<uint32_t>(m_r >> 32);
    }

    CONSTEXPR_GREATER_CXX11 uint64_with_carry&
    operator+=(const uint64_with_carry& rhs) noexcept {
        uint64_t res = m_r + rhs.m_r;
        m_c ^= rhs.m_c ^ (uint8_t)(res < m_r);
        m_r = res;
        return *this;
    }
    CONSTEXPR_GREATER_CXX11 uint64_with_carry
    operator+(const uint64_with_carry& rhs) const noexcept {
        return uint64_with_carry(*this) += rhs;
    }

private:
    uint64_t m_r;
    uint8_t  m_c : 1;
};

} // namespace detail

// Define uintN_t multiplication operators
template <size_t bits> // general variant
CONSTEXPR_GREATER_CXX11 uintN_t<bits>&
    uintN_t<bits>::operator*=(const uintN_t<bits>& rhs) noexcept {
    return (*this = static_cast<uintN_t<bits>>(
        detail::multiplication::karatsuba(*this, rhs)
    ));
}
template <size_t bits> // specefic variant for rhs < 2^16
CONSTEXPR_GREATER_CXX11 uintN_t<bits>&
    uintN_t<bits>::operator*=(uint16_t rhs) noexcept {
    auto copy = *this;
    clear();
    while (rhs) {
        if (rhs & 1) *this += copy;
        copy.small_shift_left(1);
        rhs >>= 1;
    }
    return *this;
}

// Define uintN_t division operators
template <size_t bits>
CONSTEXPR_GREATER_CXX11 uintN_t<bits>&
    uintN_t<bits>::operator/=(const uintN_t<bits>& rhs) noexcept {
    return (*this = detail::division::prime(*this, rhs).quotient);
}
template <size_t bits>
CONSTEXPR_GREATER_CXX11 uintN_t<bits>&
    uintN_t<bits>::operator%=(const uintN_t<bits>& rhs) noexcept {
    return (*this = detail::division::prime(*this, rhs).remainder);
}

namespace uintN_t_literals {

#define evsDEFINE_LITERAL_SUFFUX(BITS)                      \
CONSTEXPR_GREATER_CXX11 uintN_t<BITS> operator""_Ui ## BITS \
    (const char* literal) noexcept {                        \
    return detail::from_literal<BITS>(literal); }

evsDEFINE_LITERAL_SUFFUX(128)
evsDEFINE_LITERAL_SUFFUX(256)
evsDEFINE_LITERAL_SUFFUX(512)
evsDEFINE_LITERAL_SUFFUX(1024)

#undef evsDEFINE_LITERAL_SUFFUX

} // namespace uintN_t_literals

namespace uintN_t_alg {

using ::detail::multiplication::karatsuba;
using ::detail::multiplication::russian_peasant;

template <size_t B>
CONSTEXPR_GREATER_CXX11 uintN_t<B*2> sqr(const uintN_t<B>& x) noexcept {
    // from https://ido.tsu.ru/iop_res1/teorcrypto/text/1_3.html
    using ed_t = typename uintN_t<B>::extend_digit_t;
    using uiwc = detail::uint64_with_carry;
    uintN_t<B*2> out;
    uiwc cuv;

    const size_t n = x.digit_count;
    evsIRANGE(i, n) {
        cuv.reg(ed_t{out.digits[i * 2]} +
            ed_t{x.digits[i]} * ed_t{x.digits[i]});
        cuv.carry(0);
        out.digits[i * 2] = cuv.low();

        for (size_t j = i + 1; j < n; j++) {
            cuv = uiwc(detail::merge_32_to_64(cuv.high(), cuv.carry()))
                + uiwc(ed_t{x.digits[i]} * ed_t{x.digits[j]})
                + uiwc(ed_t{x.digits[i]} * ed_t{x.digits[j]})
                + uiwc(out.digits[i + j]);
            out.digits[i + j] = cuv.low();
        }

#if __cplusplus >= 201402L
        uintN_t<B*2> cu = {cuv.high(), cuv.carry()};
#else
        uintN_t<B*2> cu;
        cu.digits[0] = cuv.high();
        cu.digits[1] = cuv.carry();
#endif
        cu.digit_shift_left(i + n);
        out += cu;
    }

    return out;
}

} // namespace uintN_t_alg

#include <algorithm>
#include <version>
#include <ostream>
#include <utility>
#include <string>
#include <limits>

#if __cpp_lib_to_chars >= 201611L
#include <charconv>
#endif

namespace detail {

static constexpr char digit_set[] = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8',
    '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
    'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
};

template <class T>
constexpr void reverse(T* first, T* last) noexcept {
    if (first == last) return;
    for (--last; first < last; ++first, --last) {
        T tmp = *last;
        *last = *first;
        *first = tmp;
    }
}

template <size_t B>
constexpr char* to_chars_i(
    char* first, char* last,
    uintN_t<B> number,
    bool* overflow, int base
) noexcept {
    reverse(number.digits, number.digits + number.digit_count);
    uint64_t d, r;
    char* it = first;

    do { // algorithm from https://stackoverflow.com/a/8023937
        if (it == last) {
            if (overflow)
                *overflow = true;
            return last;
        }

        r = number.digits[0];
        evsIRANGE(i, number.digit_count) {
            d = r / base;
            r = r - d * base;
            if (i != number.digit_count - 1)
                r = (r << 32) + number.digits[i + 1];
            number.digits[i] = d;
        }

        *it++ = digit_set[r];
    } while (number);

    reverse(first, it);
    if (overflow)
        *overflow = false;
    return it;
}

template <size_t B>
constexpr char* to_chars_pow2(
    char* first, char* last,
    uintN_t<B> number,
    bool* overflow, int pow
) noexcept {
    const typename uintN_t<B>::digit_t mask = (1 << pow) - 1;
    char* it = first;

    do {
        if (it == last) {
            if (overflow)
                *overflow = true;
            return last;
        }

        *it++ = digit_set[number.digits[0] & mask];
        number.small_shift_right(pow);
    } while (number);

    reverse(first, it);
    if (overflow)
        *overflow = false;
    return it;
}

} // namespace detail

namespace std {

template <size_t B>
struct numeric_limits<uintN_t<B>>
    : numeric_limits<typename uintN_t<B>::digit_t> {
    static constexpr int digits = B;
    static constexpr int digits10 =
        static_cast<int>(digits * detail::log10_2);
    /* Other variables same for unsigned int */
    
    static constexpr uintN_t<B> min() noexcept { return {}; }
    static constexpr uintN_t<B> max() noexcept { return ~min(); }

    static constexpr uintN_t<B> lowest()        noexcept { return {}; }
    static constexpr uintN_t<B> epsilon()       noexcept { return {}; }
    static constexpr uintN_t<B> infinity()      noexcept { return {}; }
    static constexpr uintN_t<B> quiet_NaN()     noexcept { return {}; }
    static constexpr uintN_t<B> denorm_min()    noexcept { return {}; }
    static constexpr uintN_t<B> round_error()   noexcept { return {}; }
    static constexpr uintN_t<B> signaling_NaN() noexcept { return {}; }
};

template <size_t B>
string to_string(const uintN_t<B>& value) {
    char buffer[std::numeric_limits<uintN_t<B>>::digits10 + 1] {};
    const char* buf_end = detail::to_chars_i(
        buffer, buffer + sizeof(buffer), value, nullptr, 10);
    return string(buffer, buf_end - buffer);
}

#if __cpp_lib_to_chars >= 201611L
template <size_t B>
constexpr to_chars_result to_chars(
    char* first, char* last, const uintN_t<B>& value, int base = 10) noexcept {
    if (base < 2 || base > 36)
        return {last, errc::invalid_argument};

    bool overflow = false;
    char* end;
    switch (base) {
        case  2: end = detail::to_chars_pow2(first, last, value, &overflow, 1); break;
        case  4: end = detail::to_chars_pow2(first, last, value, &overflow, 2); break;
        case  8: end = detail::to_chars_pow2(first, last, value, &overflow, 3); break;
        case 16: end = detail::to_chars_pow2(first, last, value, &overflow, 4); break;
        case 32: end = detail::to_chars_pow2(first, last, value, &overflow, 5); break;
        default: end = detail::to_chars_i(first, last, value, &overflow, base); break;
    }

    if (overflow)
        return {last, errc::value_too_large};
    return {end, errc{}};
}
#endif // __cpp_lib_to_chars >= 201611L

template <size_t B, class CharT, class Traits>
basic_ostream<CharT, Traits>& operator<<(
    basic_ostream<CharT, Traits>& os, const uintN_t<B>& n) {
    const auto fls = os.flags();
    char buffer[B / 3 + 1 + 2 + 1] {};
    // addition digit --'   |   |
    // base (0x/0) ---------'   |
    // pos (+) -----------------'

    size_t offset = 0;
    char* buf_end;

    if (fls & os.showpos)
        buffer[offset++] = '+';
    if (fls & os.showbase) {
        if (fls & (os.hex | os.oct))
            buffer[offset++] = '0';
        if (fls & os.hex)
            buffer[offset++] = 'x';
    }

    if (fls & os.hex)
        buf_end = detail::to_chars_pow2(
            buffer + offset, buffer + sizeof(buffer), n, nullptr, 4);
    else if (fls & os.oct)
        buf_end = detail::to_chars_pow2(
            buffer + offset, buffer + sizeof(buffer), n, nullptr, 3);
    else // os.dec branch
        buf_end = detail::to_chars_i(
            buffer + offset, buffer + sizeof(buffer), n, nullptr, 10);

    const ptrdiff_t nl = buf_end - buffer;
    const streamsize w = os.width(0);
    const ptrdiff_t lw = w > nl ? w - nl : 0;
    const CharT fillch = os.fill();

    CharT out_number[sizeof(buffer)] {};
    transform(buffer, buf_end, out_number, [&os, &fls] (char c) {
        if (fls & os.uppercase && (('a' <= c && c <= 'f') || c == 'x'))
            return os.widen(c - 32);
        return os.widen(c);
    });

    if (!(fls & os.internal)) {
        if ((fls & os.right) || (fls & os.adjustfield) == 0)
            for (ptrdiff_t i = 0; i < lw; i++) os << fillch;
        os.write(out_number, nl);
        if (fls & os.left)
            for (ptrdiff_t i = 0; i < lw; i++) os << fillch;
    } else { // os.internal
        if (offset) os.write(out_number, offset);
        for (ptrdiff_t i = 0; i < lw; i++) os << fillch;
        os.write(out_number + offset, nl - offset);
    }
    return os;
}

template <size_t B>
constexpr void swap(
    uintN_t<B>& lhs,
    uintN_t<B>& rhs
) noexcept {
    swap(lhs.digits, rhs.digits);
}

} // namespace std

#endif // UINTN_T_HPP