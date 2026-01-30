// When we return a value in memory, make sure we pass a pointer
// to the returned value in RAX.
// The main function, defined in validate_return_pointer_<PLATFORM>.s,
// will call return_in_mem, then read through RAX to access/validate the result


// This struct will be passed in memory
struct s {
    long l1;
    long l2;
    long l3;
};


struct s return_in_mem(void) {
    struct s result = {1, 2, 3};
    return result;
}