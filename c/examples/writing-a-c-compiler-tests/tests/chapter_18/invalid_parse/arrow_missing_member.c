struct s {
    int y;
};

int main(void) {
    struct s *ptr = 0;
    return ptr->;  // arrow must be followed by a member name
}