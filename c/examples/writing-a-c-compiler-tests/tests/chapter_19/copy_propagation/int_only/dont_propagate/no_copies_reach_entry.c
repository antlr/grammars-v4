/* Make sure we track that the set of reaching copies from ENTRY is empty */
int target(int a, int flag) {
    if (flag) {
        // if we initialized ENTRY with the set of all copies in target,
        // we'll think that a = 10 reaches this return statement,
        // and incorrectly rewrite it as 'return 10'
        return a;
    }

    a = 10;  // initialize ENTRY w/ empty set of copies, not including this one
    return a;
}

int main(void) {
    return target(4, 1);
}