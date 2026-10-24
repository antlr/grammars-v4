#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]){
    int i = 0;
    do {
        i++;
        if (i == 5){
            goto label;
        }
        printf("%d\n", i);
label:
    } while (i < 10);
}

