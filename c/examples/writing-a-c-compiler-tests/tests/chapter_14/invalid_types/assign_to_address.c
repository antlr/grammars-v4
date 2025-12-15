int main(void)
{
    int x = 0;
    &x = 10; /* An address-of expression is not an lvalue, so you can't assign to it */
}