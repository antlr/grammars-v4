void exit(int status);

// In our implementation, this fails tag resolution because it specifies an
// undeclared type. In a fully conforming implementation, it fails because you
// can't define a function with incomplete return type
struct s foo(void) {
    exit(0);
}