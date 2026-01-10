/* Test comparisons to several null pointer constants */

#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
// we have to suppress warning for non_null != 0u
// this warning is bogus: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110238
#pragma GCC diagnostic ignored "-Wpointer-compare"
#endif
#endif

double *get_null_pointer(void) {
    return 0;
}

int main(void)
{
    double x;
    double *null = get_null_pointer();
    double *non_null = &x;

    if (non_null == 0) {
        return 1;
    }

    if (!(null == 0l)) {
        return 2;
    }

    if (!(non_null != 0u)) {
        return 3;
    }

    if (null != 0ul) {
        return 4;
    }

    return 0;
}