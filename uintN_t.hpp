/* uintN_t - integer type for large numbers. C++14 and later */
#ifndef UINTN_T_HPP
#define UINTN_T_HPP

#include <cstddef>
#include <cstdint>

#ifdef __cpp_impl_three_way_comparison
#include <compare>
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
    constexpr operator bool() const noexcept {
        evsIRANGE(i, digit_count)
            if (digits[i])
                return true;
        return false;
    }
    constexpr explicit operator digit_t()
        const noexcept { return digits[0]; }

    /* Widening conversion (1 -> 01) */
    template <size_t other_bits, evsENABLE(other_bits > bits)>
    constexpr operator uintN_t<other_bits>() const noexcept {
        uintN_t<other_bits> out;
        evsIRANGE(i, digit_count)
            out.digits[i] = digits[i];
        return out;
    }

    /* Narrowing conversion (12 -> 2) */
    template <size_t other_bits, evsENABLE(other_bits < bits)>
    constexpr explicit operator uintN_t<other_bits>() const noexcept {
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
    constexpr bool assign_add(const uintN_t& rhs) noexcept {
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
     * @brief Shift bits to left by `shift`
     * @param shift bit shift, must be in range [`1`, `digit_width` - `1`]
     */
    constexpr void small_shift_left(size_t shift) noexcept {
        if (!shift || shift >= digit_width) return;
        for (size_t i = digit_count - 1; i > 0; i--)
            digits[i] = digits[i] << shift | digits[i - 1] >> (digit_width - shift);
        digits[0] <<= shift;
    }
    /**
     * @brief Shift bits to right by `shift`
     * @param shift bit shift, must be in range [`1`, `digit_width` - `1`]
     */
    constexpr void small_shift_right(size_t shift) noexcept {
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
    constexpr void digit_shift_left(size_t shift) noexcept {
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
    constexpr void digit_shift_right(size_t shift) noexcept {
        if (shift >= digit_count) return clear();
        evsIRANGE(i, digit_count - shift)
            digits[i] = digits[i + shift];
        for (size_t i = digit_count - 1; i > digit_count - shift - 1; i--)
            digits[i] = 0;
    }

    /* Unary operators */

    constexpr uintN_t operator+() const noexcept { return *this; }
    constexpr uintN_t operator-() const noexcept { return ~(*this) += {1}; }
    constexpr uintN_t operator~() const noexcept {
        uintN_t out = *this;
        evsIRANGE(i, digit_count)
            out.digits[i] = ~out.digits[i];
        return out;
    }

    /* Increment/Decrement */

    constexpr uintN_t& operator++() noexcept { return *this += {1}; }
    constexpr uintN_t& operator--() noexcept { return *this -= {1}; }

    constexpr uintN_t operator++(int) noexcept {
        uintN_t out = *this; ++(*this);
        return out;
    }
    constexpr uintN_t operator--(int) noexcept {
        uintN_t out = *this; --(*this);
        return out;
    }

    /* Binary-assign operators */

    constexpr uintN_t& operator+=(const uintN_t& rhs) noexcept {
        assign_add(rhs);
        return *this;
    }
    constexpr uintN_t& operator-=(const uintN_t& rhs) noexcept {
        return *this += -rhs;
    }

#define evsBITWISE_ASGOPER_TMPL(op)                               \
    constexpr uintN_t& operator op(const uintN_t& rhs) noexcept { \
        evsIRANGE(i, digit_count) digits[i] op rhs.digits[i];     \
        return *this; }
    
    evsBITWISE_ASGOPER_TMPL(&=)
    evsBITWISE_ASGOPER_TMPL(|=)
    evsBITWISE_ASGOPER_TMPL(^=)

    constexpr uintN_t& operator<<=(size_t shift) noexcept {
        digit_shift_left(shift / digit_width);
        small_shift_left(shift % digit_width);
        return *this;
    }
    constexpr uintN_t& operator>>=(size_t shift) noexcept {
        digit_shift_right(shift / digit_width);
        small_shift_right(shift % digit_width);
        return *this;
    }

    constexpr uintN_t& operator<<=(int shift) noexcept {
        if (shift < 0) return *this >>= -shift;
        return *this <<= static_cast<size_t>(shift);
    }
    constexpr uintN_t& operator>>=(int shift) noexcept {
        if (shift < 0) return *this <<= -shift;
        return *this >>= static_cast<size_t>(shift);
    }

    constexpr uintN_t& operator*=(const uintN_t&) noexcept;
    constexpr uintN_t& operator*=(uint16_t) noexcept;

    constexpr uintN_t& operator/=(const uintN_t&) noexcept;
    constexpr uintN_t& operator%=(const uintN_t&) noexcept;

    /* Binary operators */

#define evsBINOP_VIA_BINASGOP(op)                           \
    constexpr uintN_t operator op(const uintN_t& rhs) const \
    noexcept { return uintN_t(*this) op ## = rhs; }

    evsBINOP_VIA_BINASGOP(+)
    evsBINOP_VIA_BINASGOP(-)
    evsBINOP_VIA_BINASGOP(*)
    evsBINOP_VIA_BINASGOP(/)
    evsBINOP_VIA_BINASGOP(%)
    evsBINOP_VIA_BINASGOP(&)
    evsBINOP_VIA_BINASGOP(|)
    evsBINOP_VIA_BINASGOP(^)

    constexpr uintN_t operator*(uint16_t rhs) const noexcept {
        return uintN_t(*this) *= rhs;
    }

    constexpr uintN_t operator<<(size_t shift) const noexcept {
        return uintN_t(*this) <<= shift;
    }
    constexpr uintN_t operator>>(size_t shift) const noexcept {
        return uintN_t(*this) >>= shift;
    }

    constexpr uintN_t operator<<(int shift) const noexcept {
        return uintN_t(*this) <<= shift;
    }
    constexpr uintN_t operator>>(int shift) const noexcept {
        return uintN_t(*this) >>= shift;
    }

    /* Comparison operators */

    /**
     * @brief  Three-way comparison of a number with `rhs`
     * @param  rhs the number to be compared with
     * @return Int value as less (< 0), greater (> 0) and equal (== 0)
     */
    constexpr int compare(const uintN_t& rhs) const noexcept {
        size_t i = digit_count - 1;
        while (i > 0 && digits[i] == rhs.digits[i]) --i;
        return (rhs.digits[i] < digits[i]) - (digits[i] < rhs.digits[i]);
    }

#define evsCMP_OPER_TMPL(op)                       \
    constexpr bool operator op(const uintN_t& rhs) \
    const noexcept { return compare(rhs) op 0; }

    evsCMP_OPER_TMPL(==)
#ifndef __cpp_impl_three_way_comparison
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

    /* Other functions */

    /// Set all digits equal zero, same as `*this = {0}`
    constexpr void clear() noexcept {
        evsIRANGE(i, digit_count) digits[i] = 0;
    }

    /**
     * @brief  Get bit value from `pos`
     * @param  pos index of bit
     * @return Bit value in `pos`
     */
    constexpr bool bit(size_t pos) const noexcept {
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
    constexpr void bit(size_t pos, bool value) noexcept {
        if (pos >= bits) return;
        const size_t digit_index = pos / digit_width;
        const size_t   bit_index = pos % digit_width;
        if (value) digits[digit_index] |=   digit_t{1} << bit_index;
        else       digits[digit_index] &= ~(digit_t{1} << bit_index);
    }

    /**
     * @brief Split number to two numbers with width 2 times less
     * @param low lower part of original number
     * @param high higher part of original number
     */
    constexpr void split(
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
    constexpr void merge(
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

    /**
     * @brief  Change order of elements (digits) in inner
     *         array (need for to_string algorithm)
     * @return new number with reverse digit order
     */
    constexpr uintN_t reverse_digits() const noexcept {
        uintN_t out = *this;
        digit_t* i = out.digits;
        digit_t* j = out.digits + out.digit_count;
        for (--j; i < j; ++i, --j) {
            digit_t tmp = *i;
            *i = *j;
            *j = tmp;
        }
        return out;
    }
}; // struct uintN_t

// Base size aliases
using uint128_t  = uintN_t<128>;
using uint256_t  = uintN_t<256>;
using uint512_t  = uintN_t<512>;
using uint1024_t = uintN_t<1024>;

namespace detail {

namespace multiplication {

template <size_t B>
constexpr uintN_t<B*2> karatsuba(
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
    if (xc && yc) z3 += doub_num_t{1} << B;
    doub_num_t z1 = z3 - z2 - z0;
    
    doub_num_t out = z0;
    z1 <<= B/2; z2 <<= B;
    (out += z1) += z2;

    return out;
}

template <> // base variant for recursion
constexpr uintN_t<64> karatsuba(
    const uintN_t<32>& lhs,
    const uintN_t<32>& rhs
) noexcept {
    using  nt = uintN_t<32>;
    using  dt = nt::digit_t;
    using edt = nt::extend_digit_t;
    edt res = 
        static_cast<edt>(lhs.digits[0]) *
        static_cast<edt>(rhs.digits[0]);
    return {
        static_cast<dt>(res), static_cast<dt>(res >> nt::digit_width)
    };
}

template <size_t B>
constexpr uintN_t<B*2> russian_peasant(
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

template <size_t bits>
struct div_result_t {
    uintN_t<bits> quotient;
    uintN_t<bits> remainder;
};

// https://clck.ru/3FBwXQ (Wikipedia)
template <size_t B>
constexpr div_result_t<B> prime(
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

constexpr size_t cexpr_strlen(const char* str) noexcept {
    size_t count = 0;
    while (*str) ++str, ++count;
    return count;
}

#define evsTHREEWAY_CMP(l, r) (r < l) - (l < r)

constexpr int cexpr_strcmp(const char* lhs, const char* rhs) noexcept {
    const size_t lhs_len = cexpr_strlen(lhs);
    const size_t rhs_len = cexpr_strlen(rhs);
    if (lhs_len != rhs_len)
        return evsTHREEWAY_CMP(lhs_len, rhs_len);
    while (*lhs && *lhs == *rhs) ++lhs, ++rhs;
    return evsTHREEWAY_CMP(*lhs, *rhs);
}

static constexpr auto max_128_value =
    "340282366920938463463374607431768211455";
static constexpr auto max_256_value =
    "1157920892373161954235709850086879078532"
    "69984665640564039457584007913129639935";
static constexpr auto max_512_value =
    "1340780792994259709957402499820584612747"
    "9365820592393377723561443721764030073546"
    "9768018742981669034276900318581864860508"
    "53753882811946569946433649006084095";
static constexpr auto max_1024_value =
    "1797693134862315907729305190789024733617"
    "9769789423065727343008115773267580550096"
    "3132708477322407536021120113879871393357"
    "6587897688144166224928474306394741243777"
    "6789342486548527630221960124609411945308"
    "2952085005768838150682342462881473913110"
    "5408272371633505106845862982399472459384"
    "79716304835356329624224137215";

static constexpr auto log10_2 = 0.3010299956639812L;

} // namespace detail

// Define uintN_t multiplication operators
template <size_t bits> // general variant
constexpr uintN_t<bits>& uintN_t<bits>::operator*=(const uintN_t<bits>& rhs) noexcept {
    return (*this = static_cast<uintN_t<bits>>(
        detail::multiplication::karatsuba(*this, rhs)
    ));
}
template <size_t bits> // specefic variant for rhs < 2^16
constexpr uintN_t<bits>& uintN_t<bits>::operator*=(uint16_t rhs) noexcept {
    return (*this = static_cast<uintN_t<bits>>(
        detail::multiplication::russian_peasant(*this, {rhs})
    ));
}

// Define uintN_t division operators
template <size_t bits>
constexpr uintN_t<bits>& uintN_t<bits>::operator/=(const uintN_t<bits>& rhs) noexcept {
    return (*this = detail::division::prime(*this, rhs).quotient);
}
template <size_t bits>
constexpr uintN_t<bits>& uintN_t<bits>::operator%=(const uintN_t<bits>& rhs) noexcept {
    return (*this = detail::division::prime(*this, rhs).remainder);
}

namespace uintN_t_literals {

#define evsDEFINE_LITERAL_SUFFUX(Bits)                                                 \
constexpr uintN_t<Bits> operator""_Ui ## Bits (const char* literal) noexcept {         \
    if (detail::cexpr_strcmp(literal, detail::max_ ## Bits ## _value) > 0) return {0}; \
    uintN_t<Bits> out;                                                                 \
    do {                                                                               \
        out = (out << 3) + (out << 1); /* multiplication by 10 */                      \
        out += {static_cast<uintN_t<Bits>::digit_t>(*literal - '0')};                  \
    } while (*++literal);                                                              \
    return out; }                                                                      \

evsDEFINE_LITERAL_SUFFUX(128)
evsDEFINE_LITERAL_SUFFUX(256)
evsDEFINE_LITERAL_SUFFUX(512)
evsDEFINE_LITERAL_SUFFUX(1024)

} // namespace uintN_t_literals

#include <ostream>
#include <utility>
#include <string>
#include <limits>

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

template <size_t bits>
string to_string(const uintN_t<bits>& number, bool is_signed = false) {
    string out;
    uintN_t<bits> n;
    uint64_t d, r;

    bool sign = number.digits[number.digit_count - 1] >> 31;
    if (is_signed && sign)
        n = (-number).reverse_digits();
    else n = number.reverse_digits();

    do { // https://stackoverflow.com/a/8023937
        r = n.digits[0];

        evsIRANGE(i, n.digit_count - 1) {
            d = r / 10;
            r = ((r - d * 10) << 32) + n.digits[i + 1];
            n.digits[i] = d;
        }

        d = r / 10;
        r = r - d * 10;
        n.digits[n.digit_count - 1] = d;

        out.insert(out.begin(), r + '0');
    } while (n);

    if (is_signed && sign) out.insert(out.begin(), '-');
    return out;
}

template <size_t bits>
ostream& operator<<(ostream& os, const uintN_t<bits>& n) {
    if (os.flags() & os.hex) {
        char prev = os.fill('0');
        os << n.digits[n.digit_count - 1];
        for (size_t i = n.digit_count - 1; i > 0; i--) {
            os.width(8);
            os << n.digits[i - 1];
        }
        os.fill(prev);
    } else os << to_string(n, false);
    return os;
}

template <size_t bits>
constexpr void swap(
    uintN_t<bits>& lhs,
    uintN_t<bits>& rhs
) noexcept {
    swap(lhs.digits, rhs.digits);
}

} // namespace std

#endif // UINTN_T_HPP