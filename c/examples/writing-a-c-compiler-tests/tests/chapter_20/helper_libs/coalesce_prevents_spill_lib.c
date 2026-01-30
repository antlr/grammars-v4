// validation function for coalesce_prevents_spill
#include <stdio.h>
#include <stdlib.h>

int validate(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m) {
    int args[13] = {a, b, c, d, e, f, g, h, i, j, k, l, m};
    for (int i = 0; i < 13; i ++) {
        if (args[i] != 10) {
            printf("Expected argument %d to have value 10, actual value was %d\n", i, args[i]);
            exit(-1);
        }
    }
    /*
    if (m != 1){
        printf("Expected last argument to have value 1, actual value was %d\n", m);
        exit(-1);
    }
    */
    return 0;
}