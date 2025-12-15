// Because a structure declaration isn't a statement, it can't appear right
// after a case statement. NOTE: this is valid as of C23
int main(void) {
    switch (0) {
        case 0:
            struct s {
                int a;
            };
            return 0;
    }
    return 0;
}