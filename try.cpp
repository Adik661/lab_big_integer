#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>
using namespace std;

class BigInteger {
private:
    vector<int> digits;  // Store digits in reverse order (least significant first)
    bool isNegative;     // Sign flag

    // Helper function to remove leading zeros
    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.size() == 1 && digits[0] == 0) {
            isNegative = false;
        }
    }

    // Helper function to compare absolute values
    // Returns: 1 if this > other, -1 if this < other, 0 if equal
    int compareAbs(const BigInteger& other) const {
        if (digits.size() != other.digits.size()) {
            return digits.size() > other.digits.size() ? 1 : -1;
        }
        for (int i = digits.size() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] > other.digits[i] ? 1 : -1;
            }
        }
        return 0;
    }

    // Addition of absolute values
    BigInteger addAbs(const BigInteger& other) const {
        BigInteger result;
        result.digits.clear();
        int carry = 0;
        size_t maxSize = max(digits.size(), other.digits.size());

        for (size_t i = 0; i < maxSize || carry; i++) {
            int sum = carry;
            if (i < digits.size()) sum += digits[i];
            if (i < other.digits.size()) sum += other.digits[i];
            result.digits.push_back(sum % 10);
            carry = sum / 10;
        }
        return result;
    }

    // Subtraction of absolute values (assumes this >= other)
    BigInteger subtractAbs(const BigInteger& other) const {
        BigInteger result;
        result.digits.clear();
        int borrow = 0;

        for (size_t i = 0; i < digits.size(); i++) {
            int diff = digits[i] - borrow;
            if (i < other.digits.size()) {
                diff -= other.digits[i];
            }

            if (diff < 0) {
                diff += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.digits.push_back(diff);
        }
        result.removeLeadingZeros();
        return result;
    }

    // Multiplication by single digit
    BigInteger multiplyByDigit(int digit) const {
        if (digit == 0) return BigInteger(0);
        BigInteger result;
        result.digits.clear();
        int carry = 0;

        for (int d : digits) {
            int product = d * digit + carry;
            result.digits.push_back(product % 10);
            carry = product / 10;
        }
        while (carry) {
            result.digits.push_back(carry % 10);
            carry /= 10;
        }
        return result;
    }

public:
    // Constructors
    BigInteger() : isNegative(false) {
        digits.push_back(0);
    }

    BigInteger(long long num) : isNegative(num < 0) {
        if (num < 0) num = -num;
        if (num == 0) {
            digits.push_back(0);
            isNegative = false;
        } else {
            while (num > 0) {
                digits.push_back(num % 10);
                num /= 10;
            }
        }
    }

    BigInteger(const string& str) : isNegative(false) {
        digits.clear();
        size_t start = 0;
        if (!str.empty() && str[0] == '-') {
            isNegative = true;
            start = 1;
        }
        if (start >= str.length()) {
            digits.push_back(0);
            isNegative = false;
            return;
        }
        for (int i = str.length() - 1; i >= (int)start; i--) {
            if (str[i] >= '0' && str[i] <= '9') {
                digits.push_back(str[i] - '0');
            }
        }
        if (digits.empty()) digits.push_back(0);
        removeLeadingZeros();
    }

    // Convert to string
    string toString() const {
        string result;
        for (int i = digits.size() - 1; i >= 0; i--) {
            result += (char)('0' + digits[i]);
        }
        if (isNegative && !(digits.size() == 1 && digits[0] == 0)) {
            result = "-" + result;
        }
        return result;
    }

    // Addition
    BigInteger operator+(const BigInteger& other) const {
        if (isNegative == other.isNegative) {
            // Same sign: add absolute values
            BigInteger result = addAbs(other);
            result.isNegative = isNegative;
            return result;
        } else {
            // Different signs: subtract absolute values
            int cmp = compareAbs(other);
            BigInteger result;
            if (cmp >= 0) {
                result = subtractAbs(other);
                result.isNegative = isNegative;
            } else {
                result = other.subtractAbs(*this);
                result.isNegative = other.isNegative;
            }
            return result;
        }
    }

    // Subtraction
    BigInteger operator-(const BigInteger& other) const {
        BigInteger negOther = other;
        negOther.isNegative = !negOther.isNegative;
        return *this + negOther;
    }

    // Multiplication
    BigInteger operator*(const BigInteger& other) const {
        if ((digits.size() == 1 && digits[0] == 0) ||
            (other.digits.size() == 1 && other.digits[0] == 0)) {
            return BigInteger(0);
        }

        BigInteger result(0);
        result.digits.clear();
        result.digits.resize(digits.size() + other.digits.size(), 0);

        for (size_t i = 0; i < digits.size(); i++) {
            for (size_t j = 0; j < other.digits.size(); j++) {
                result.digits[i + j] += digits[i] * other.digits[j];
                result.digits[i + j + 1] += result.digits[i + j] / 10;
                result.digits[i + j] %= 10;
            }
        }

        result.removeLeadingZeros();
        result.isNegative = (isNegative != other.isNegative) &&
                           !(result.digits.size() == 1 && result.digits[0] == 0);
        return result;
    }

    // Division
    BigInteger operator/(const BigInteger& other) const {
        if (other.digits.size() == 1 && other.digits[0] == 0) {
            throw runtime_error("Division by zero");
        }

        if (compareAbs(other) < 0) {
            return BigInteger(0);
        }

        BigInteger dividend = *this;
        dividend.isNegative = false;
        BigInteger divisor = other;
        divisor.isNegative = false;

        BigInteger quotient(0);
        quotient.digits.clear();
        BigInteger current(0);

        for (int i = digits.size() - 1; i >= 0; i--) {
            current.digits.insert(current.digits.begin(), dividend.digits[i]);
            current.removeLeadingZeros();

            int count = 0;
            while (current.compareAbs(divisor) >= 0) {
                current = current - divisor;
                count++;
            }
            quotient.digits.insert(quotient.digits.begin(), count);
        }

        quotient.removeLeadingZeros();
        quotient.isNegative = (isNegative != other.isNegative) &&
                             !(quotient.digits.size() == 1 && quotient.digits[0] == 0);
        return quotient;
    }

    // Modulo
    BigInteger operator%(const BigInteger& other) const {
        if (other.digits.size() == 1 && other.digits[0] == 0) {
            throw runtime_error("Modulo by zero");
        }
        BigInteger quotient = *this / other;
        BigInteger result = *this - (quotient * other);
        return result;
    }

    // Comparison operators
    bool operator==(const BigInteger& other) const {
        if (isNegative != other.isNegative) return false;
        return digits == other.digits;
    }

    bool operator!=(const BigInteger& other) const {
        return !(*this == other);
    }

    bool operator<(const BigInteger& other) const {
        if (isNegative != other.isNegative) return isNegative;
        int cmp = compareAbs(other);
        return isNegative ? cmp > 0 : cmp < 0;
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
        os << num.toString();
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
        result.isNegative = false;
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

        while (!(b.digits.size() == 1 && b.digits[0] == 0)) {
            BigInteger temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }
};

// Example usage and tests
int main() {
    // Test creation and basic operations
    BigInteger a("123456789012345678901234567890");
    BigInteger b("987654321098765432109876543210");

    cout << "a = " << a << endl;
    cout << "b = " << b << endl;

    cout << "\nArithmetic Operations:" << endl;
    cout << "a + b = " << (a + b) << endl;
    cout << "a - b = " << (a - b) << endl;
    cout << "b - a = " << (b - a) << endl;
    cout << "a * b = " << (a * b) << endl;
    cout << "b / a = " << (b / a) << endl;
    cout << "b % a = " << (b % a) << endl;

    cout << "\nNegative Numbers:" << endl;
    BigInteger c("-12345");
    BigInteger d("67890");
    cout << "c = " << c << endl;
    cout << "d = " << d << endl;
    cout << "c + d = " << (c + d) << endl;
    cout << "c * d = " << (c * d) << endl;

    cout << "\nComparison:" << endl;
    cout << "a == b: " << (a == b) << endl;
    cout << "a < b: " << (a < b) << endl;
    cout << "a > b: " << (a > b) << endl;

    cout << "\nPower and GCD:" << endl;
    BigInteger base(2);
    cout << "2^100 = " << base.power(100) << endl;

    BigInteger x("48"), y("18");
    cout << "gcd(48, 18) = " << x.gcd(y) << endl;

    return 0;
}
