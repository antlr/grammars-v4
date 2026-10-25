int main(void) {
    // pointer declarator can't follow array size declarator
    return (int[3](*))0;
}