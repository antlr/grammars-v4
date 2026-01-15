/* Load instruction generates all aliased variables */
long *pass_and_return(long *ptr) {
    return ptr;
}

int main(void) {
    long l;
    long *ptr = &l;
    long *other_ptr = pass_and_return(ptr);  // now other_ptr points to l
    l = 10;  // not a dead store b/c l is aliased and this is followed by load
             // from memory
    return *other_ptr;  // this makes all aliased vars (i.e. l) live
}