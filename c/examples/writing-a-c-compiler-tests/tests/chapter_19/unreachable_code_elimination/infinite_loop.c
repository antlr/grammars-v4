/* make sure we don't choke on programs that never terminate
 * This program _does_ terminate because it indirectly calls exit()
 * but the compiler doesn't know that.
 * */

int exit_wrapper(int status); // defined in chapter_19/libraries/exit.c

int main(void) {
    int i = 0;
    do {
        i = i + 1;
        if (i > 10) {
            exit_wrapper(i);
        }
    } while(1);
}