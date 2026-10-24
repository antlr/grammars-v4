struct s;
extern struct s *ptr;

// can't subscript a pointer to an incomplete type
// this is equivalent to dereferencing a pointer to an incomplete type
int main(void) { ptr[0]; }