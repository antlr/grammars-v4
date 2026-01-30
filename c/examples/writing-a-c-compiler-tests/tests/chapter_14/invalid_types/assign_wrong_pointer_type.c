int main(void)
{
    double *d = 0;
    long *l = 0;
    /* It's illegal to assign one pointer type to another */
    l = d;
    return 0;
}