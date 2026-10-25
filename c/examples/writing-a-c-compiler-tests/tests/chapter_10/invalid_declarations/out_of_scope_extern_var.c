int main(void) {
    {
        /* This declares a variable 'a'
         * with external linkage
         */
        extern int a;
    }
    /* a is no longer in scope after the end of the block */
    return a;
}

int a = 1;