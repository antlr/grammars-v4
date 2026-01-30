int main(void) {
    for (int i = 0; i < 10; i = i + 1) {
        // case statements can only appear inside switch statements
        case 0: return 1;
    }
    return 9;
}