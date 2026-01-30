int main(void) {
    // In our implementation, this fails tag resolution because it specifies an
    // undeclared type. In a fully conforming implementation, it fails because
    // it casts to a structure type
    (struct s)0;
    return 0;
}