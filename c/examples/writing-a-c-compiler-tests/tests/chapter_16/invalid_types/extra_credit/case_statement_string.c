// Can't use strings as labels for case statements
int main(void) {
    switch (0) {
        case "foo":
            return 1;
        default:
            return 0;
    }
}