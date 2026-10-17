#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
/* Test that naming scheme does not result in conflicting variable names after alpha conversion */

int main(void) {
    int a; // a0
    int result;
    int a1 = 1; // a10
    {
        int a = 2; //a1
        int a1 = 2; // a11
        {
            int a; // a2
            {
                int a; // a3
                {
                    int a; // a4
                    {
                        int a; // a5
                        {
                            int a; // a6
                            {
                                int a; // a7
                                {
                                    int a; // a8
                                    {
                                        int a; // a9
                                        {
                                            int a = 20; // a10
                                            result = a;
                                            {
                                                int a; // a11
                                                a = 5;
                                                result = result + a;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        result = result + a1;
    }
    return result + a1;
}