// =============================================================
//  big_integer.cpp -- BigInteger class implementation
//
//  TASK: Implement all methods declared in big_integer.h
//  This stub file exists only so the project structure is clear.
//  Replace its contents with your implementation.
// =============================================================

#include "big_integer.h"
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>
using namespace std;

class BigInteger {
private:
    vector<int> digits_;  // Store digits in reverse order (least significant first)
    bool negative_;     // Sign flag

    // Helper function to remove leading zeros
    void removeLeadingZeros() {
        while (digits_.size() > 1 && digits_.back() == 0) {
            digits_.pop_back();
        }
        if (digits_.size() == 1 && digits_[0] == 0) {
            negative_ = false;
        }
    }

    // Helper function to compare absolute values
    // Returns: 1 if this > other, -1 if this < other, 0 if equal
    int compareAbs(const BigInteger& other) const {
        if (digits_.size() != other.digits_.size()) {
            return digits_.size() > other.digits_.size() ? 1 : -1;
        }
        for (int i = digits_.size() - 1; i >= 0; i--) {
            if (digits_[i] != other.digits_[i]) {
                return digits_[i] > other.digits_[i] ? 1 : -1;
            }
        }
        return 0;
    }

    // Addition of absolute values
    BigInteger addAbs(const BigInteger& other) const {
        BigInteger result;
        result.digits_.clear();
        int carry = 0;
        size_t maxSize = max(digits_.size(), other.digits_.size());

        for (size_t i = 0; i < maxSize || carry; i++) {
            int sum = carry;
            if (i < digits_.size()) sum += digits_[i];
            if (i < other.digits_.size()) sum += other.digits_[i];
            result.digits_.push_back(sum % 10);
            carry = sum / 10;
        }
        return result;
    }

    // Subtraction of absolute values (assumes this >= other)
    BigInteger subtractAbs(const BigInteger& other) const {
        BigInteger result;
        result.digits_.clear();
        int borrow = 0;

        for (size_t i = 0; i < digits_.size(); i++) {
            int diff = digits_[i] - borrow;
            if (i < other.digits_.size()) {
                diff -= other.digits_[i];
            }

            if (diff < 0) {
                diff += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.digits_.push_back(diff);
        }
        result.removeLeadingZeros();
        return result;
    }

    // Multiplication by single digit
    BigInteger multiplyByDigit(int digit_) const {
        if (digit_ == 0) return BigInteger(0);
        BigInteger result;
        result.digits_.clear();
        int carry = 0;

        for (int d : digits_) {
            int product = d * digit_ + carry;
            result.digits_.push_back(product % 10);
            carry = product / 10;
        }
        while (carry) {
            result.digits_.push_back(carry % 10);
            carry /= 10;
        }
        return result;
    }

public:
    // Constructors
    BigInteger() : negative_(false) {
        digits_.push_back(0);
    }

    BigInteger(long long num) : negative_(num < 0) { //Ошибка связана с ???
        if (num < 0) num = -num;
        if (num == 0) {
            digits_.push_back(0);
            negative_ = false;
        } else {
            while (num > 0) {
                digits_.push_back(num % 10);
                num /= 10;
            }
        }
    }

    BigInteger(const string& str) : negative_(false) {
        digits_.clear();
        size_t start = 0;
        if (!str.empty() && str[0] == '-') {
            negative_ = true;
            start = 1;
        }
        if (start >= str.length()) {
            digits_.push_back(0);
            negative_ = false;
            return;
        }
        for (int i = str.length() - 1; i >= (int)start; i--) {
            if (str[i] >= '0' && str[i] <= '9') {
                digits_.push_back(str[i] - '0');
            }
        }
        if (digits_.empty()) digits_.push_back(0);
        removeLeadingZeros();
    }

    // Convert to string
    string to_string() const {
        string result;
        for (int i = digits_.size() - 1; i >= 0; i--) {
            result += (char)('0' + digits_[i]);
        }
        if (negative_ && !(digits_.size() == 1 && digits_[0] == 0)) {
            result = "-" + result;
        }
        return result;
    }

    BigInteger operator+(const BigInteger& other) const {
        if (negative_ == other.negative_) {
            // Same sign: add absolute values
            BigInteger result = addAbs(other);
            result.negative_ = negative_;
            return result;
        } else {
            int cmp = compareAbs(other);
            BigInteger result;
            if (cmp >= 0) {
                result = subtractAbs(other);
                result.negative_ = negative_;
            } else {
                result = other.subtractAbs(*this);
                result.negative_ = other.negative_;
            }
            return result;
        }
    }

    // Subtraction
    BigInteger operator-(const BigInteger& other) const {
        BigInteger negOther = other;
        negOther.negative_ = !negOther.negative_;
        return *this + negOther;
    }

    // Multiplication
    BigInteger operator*(const BigInteger& other) const {
        if ((digits_.size() == 1 && digits_[0] == 0) ||
            (other.digits_.size() == 1 && other.digits_[0] == 0)) {
            return BigInteger(0);
        }

        BigInteger result(0);
        result.digits_.clear();
        result.digits_.resize(digits_.size() + other.digits_.size(), 0);

        for (size_t i = 0; i < digits_.size(); i++) {
            for (size_t j = 0; j < other.digits_.size(); j++) {
                result.digits_[i + j] += digits_[i] * other.digits_[j];
                result.digits_[i + j + 1] += result.digits_[i + j] / 10;
                result.digits_[i + j] %= 10;
            }
        }

        result.removeLeadingZeros();
        result.negative_ = (negative_ != other.negative_) && // Опять какой-то negative. Единственный негативный тут ты, нега
                           !(result.digits_.size() == 1 && result.digits_[0] == 0);
        return result;
    }

    // Division
    BigInteger operator/(const BigInteger& other) const {
        if (other.digits_.size() == 1 && other.digits_[0] == 0) {
            throw runtime_error("Division by zero");
        }

        if (compareAbs(other) < 0) {
            return BigInteger(0);
        }

        BigInteger dividend = *this;
        dividend.negative_ = false;
        BigInteger divisor = other;
        divisor.negative_ = false;

        BigInteger quotient(0);
        quotient.digits_.clear();
        BigInteger current(0);

        for (int i = digits_.size() - 1; i >= 0; i--) {
            current.digits_.insert(current.digits_.begin(), dividend.digits_[i]);
            current.removeLeadingZeros();

            int count = 0;
            while (current.compareAbs(divisor) >= 0) {
                current = current - divisor;
                count++;
            }
            quotient.digits_.insert(quotient.digits_.begin(), count);
        }

        quotient.removeLeadingZeros();
        quotient.negative_ = (negative_ != other.negative_) &&
                             !(quotient.digits_.size() == 1 && quotient.digits_[0] == 0);
        return quotient;
    }

    // Modulo
    BigInteger operator%(const BigInteger& other) const {
        if (other.digits_.size() == 1 && other.digits_[0] == 0) {
            throw runtime_error("Modulo by zero");
        }
        BigInteger quotient = *this / other;
        BigInteger result = *this - (quotient * other);
        return result;
    }

    // Comparison operators
    bool operator==(const BigInteger& other) const {
        if (negative_ != other.negative_) return false;
        return digits_ == other.digits_;
    }

    bool operator!=(const BigInteger& other) const {
        return !(*this == other);
    }

    bool operator<(const BigInteger& other) const {
        if (negative_ != other.negative_) return negative_;
        int cmp = compareAbs(other);
        return negative_ ? cmp > 0 : cmp < 0;
    }

    bool operator>(const BigInteger& other) const {
        return other < *this;
    }

    bool operator<=(const BigInteger& other) const {
        return !(*this > other);
    }

    bool operator>=(const BigInteger& other) const {
        return !(*this < other);
    }

    // Stream operators
    friend ostream& operator<<(ostream& os, const BigInteger& num) {
        os << num.to_string();
        return os;
    }

    friend istream& operator>>(istream& is, BigInteger& num) {
        string str;
        is >> str;
        num = BigInteger(str);
        return is;
    }

    // Additional useful functions
    BigInteger abs() const {
        BigInteger result = *this;
        result.negative_ = false;
        return result;
    }

    // Power function (a^b where b is a small integer)
    BigInteger power(int exp) const {
        if (exp < 0) throw runtime_error("Negative exponents not supported");
        if (exp == 0) return BigInteger(1);

        BigInteger result(1);
        BigInteger base = *this;

        while (exp > 0) {
            if (exp % 2 == 1) {
                result = result * base;
            }
            base = base * base;
            exp /= 2;
        }
        return result;
    }

    // Greatest common divisor
    BigInteger gcd(const BigInteger& other) const {
        BigInteger a = abs();
        BigInteger b = other.abs();

        while (!(b.digits_.size() == 1 && b.digits_[0] == 0)) {
            BigInteger temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    bool is_zero() const {
        return digits_.size() == 1 && digits_[0] == 0;
    }

    bool is_negative() const {
        return negative_;
    }

    BigInteger& operator+=(const BigInteger& rhs) {
        *this = *this + rhs;
        return *this;
    }

    BigInteger& operator-=(const BigInteger& rhs) {
        *this = *this - rhs;
        return *this;
    }

    BigInteger& operator*=(const BigInteger& rhs) {
        *this = *this * rhs;
        return *this;
    }

    BigInteger& operator/=(const BigInteger& rhs) {
        *this = *this / rhs;
        return *this;
    }

    BigInteger& operator%=(const BigInteger& rhs) {
        *this = *this % rhs;
        return *this;
    }

    BigInteger& operator++() {
        *this += BigInteger(1);
        return *this;
    }

    BigInteger operator++(int) {
        BigInteger temp = *this;
        ++(*this);
        return temp;
    }

    BigInteger& operator--() {
        *this -= BigInteger(1);
        return *this;
    }

    BigInteger operator--(int) {
        BigInteger temp = *this;
        --(*this);
        return temp;
    }

    explicit operator bool() const {
        return !is_zero();
    }

    BigInteger(int value) : BigInteger((long long)value) {}

        // Унарный минус
    BigInteger operator-() const {
        BigInteger r = *this;
        if (!r.is_zero()) r.negative_ = !r.negative_;
        return r;
    }

    // Количество цифр и сумма цифр
    size_t digit_count() const { return digits_.size(); }

    int digit_sum() const {
        int s = 0;
        for (int d : digits_) s += d;
        return s;
    }

    // Чётность
    bool is_even() const { return digits_[0] % 2 == 0; }

    // Факториал
    static BigInteger factorial(int n) {
        if (n < 0) throw runtime_error("Negative factorial");
        BigInteger r(1);
        for (int i = 2; i <= n; i++) r *= BigInteger(i);
        return r;
    }

    // Число Фибоначчи
    static BigInteger fibonacci(int n) {
        BigInteger a(0), b(1);
        for (int i = 0; i < n; i++) {
            BigInteger t = a + b;
            a = b;
            b = t;
        }
        return a;
    }

    // Возведение в степень по модулю (a^exp mod m), полезно для криптографии
    BigInteger mod_pow(BigInteger exp, const BigInteger& mod) const {
        if (exp.is_negative()) throw runtime_error("Negative exponent");
        BigInteger result(1), base = *this % mod;
        const BigInteger two(2);
        while (!exp.is_zero()) {
            if (!exp.is_even()) result = (result * base) % mod;
            base = (base * base) % mod;
            exp /= two;
        }
        return result;
    }

    // Целочисленный квадратный корень (метод Ньютона)
    BigInteger isqrt() const {
        if (negative_) throw runtime_error("Sqrt of negative");
        if (is_zero()) return BigInteger(0);
        BigInteger x = *this, y = (x + BigInteger(1)) / BigInteger(2);
        while (y < x) {
            x = y;
            y = (x + *this / x) / BigInteger(2);
        }
        return x;
    }

    // НОК
    BigInteger lcm(const BigInteger& other) const {
        if (is_zero() || other.is_zero()) return BigInteger(0);
        return (abs() / gcd(other)) * other.abs();
    }

    // Проверка на простоту (простая, для небольших чисел)
    bool is_prime() const {
        if (negative_ || *this < BigInteger(2)) return false;
        BigInteger limit = isqrt();
        for (BigInteger i(2); i <= limit; ++i)
            if ((*this % i).is_zero()) return false;
        return true;
    }
};
