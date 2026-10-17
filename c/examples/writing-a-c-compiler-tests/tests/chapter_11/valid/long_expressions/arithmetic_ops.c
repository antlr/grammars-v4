/* Test basic arithmetic operations on long integers
 * when one or both operands and the result are outside the range of int */

long a;
long b;

int addition(void) {
    // a == 4294967290l, i.e. 2^32 - 6
    // b = 5
    return (a + b == 4294967295l);
}

int subtraction(void) {
    // a = -4294967290l;
    // b = 90l;
    return (a - b == -4294967380l);
}

int multiplication(void) {
    // a = 4294967290l;
    return (a * 4l == 17179869160l);
}

int division(void) {
    /* The first operand can't fit in an int; this requires us to store the operand in RDX:RAX
    * using the 'cqo' instruction, instead of in EDX:EAX using 'cdq'
    */
    // a = 4294967290l;
    b = a / 128l;
    return (b == 33554431l);
}

int remaind(void) {
    // a = 8589934585l, i.e. 2^33 - 7
    b = -a % 4294967290l;
    return (b == -5l);
}

int complement(void) {
    // a = 9223372036854775806l, i.e. LONG_MAX - 1
    return (~a == -9223372036854775807l);
}

int main(void) {

    /* Addition */
    a = 4294967290l; // 2^32 - 6
    b = 5l;
    if (!addition()) {
        return 1;
    }

    /* Subtraction */
    a = -4294967290l;
    b = 90l;
    if (!subtraction()) {
        return 2;
    }

    /* Multiplication */
    a = 4294967290l;
    if (!multiplication()) {
        return 3;
    }

    /* Division */
    a = 4294967290l;
    if (!division()) {
        return 4;
    }

    /* Remainder */
    a = 8589934585l; // 2^33 - 7
    if (!remaind()) {
        return 5;
    }

    /* Complement */
    a = 9223372036854775806l; //LONG_MAX - 1
    if (!complement()) {
        return 6;
    }

    return 0;
}