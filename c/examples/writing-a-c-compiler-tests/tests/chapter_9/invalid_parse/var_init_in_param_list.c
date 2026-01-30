/* Variable initializers aren't permitted in parameter lists */
int bad_params(int a = 3) {
    return 1;
}

int main(void) {
    return 0;
}