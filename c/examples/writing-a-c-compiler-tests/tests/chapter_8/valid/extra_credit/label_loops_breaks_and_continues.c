// any of the new statements added in this chapter (do, while, etc)
// may be labeled

int main(void) {
    int sum = 0;
    goto do_label;
    return 0;

do_label:
    do {
        sum = 1;
        goto while_label;
    } while (1);

while_label:
    while (1) {
        sum = sum + 1;
        goto break_label;
        return 0;
    break_label:
        break;
    };
    goto for_label;
    return 0;

for_label:
    for (int i = 0; i < 10; i = i + 1) {
        sum = sum + 1;
        goto continue_label;
        return 0;
    continue_label:
        continue;
        return 0;
    }
    return sum;
}