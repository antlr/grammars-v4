int main(void) {
    // invalid abstract declarator syntax: pointer declarator can't follow
    // array size declarator
    return (int[3] *)0;
}