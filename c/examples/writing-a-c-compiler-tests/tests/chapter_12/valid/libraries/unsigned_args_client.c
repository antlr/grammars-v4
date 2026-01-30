/* make sure we pass (and if needed, type convert) unsigned args correctly */
int accept_unsigned(unsigned int a, unsigned int b, unsigned long c, unsigned long d,
                 unsigned int e, unsigned int f, unsigned long g, unsigned int h,
                 unsigned long i);

int main(void) {
    return accept_unsigned(1, -1, -1, 9223372036854775808ul, 2147483648ul, 0, 123456, 2147487744u, 9223372041149743104ul);
}