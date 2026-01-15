int main(void)
{
    long *l = 0;
    // It's illegal to compare a pointer to any integer type
    // without explicitly casting to the same type
    return l <= 100ul;
}