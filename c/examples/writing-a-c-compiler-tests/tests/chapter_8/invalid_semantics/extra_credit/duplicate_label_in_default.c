/* Make sure our validation of goto/label also traverses default statements */

int main(void) {
        int a = 1;
label:

    switch (a) {
        case 1:
            return 0;
        default:
        label:
            return 1;
    }
    return 0;
}