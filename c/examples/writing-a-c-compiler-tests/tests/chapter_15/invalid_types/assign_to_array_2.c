int main(void)
{
    int dim2[1][2] = {{1, 2}};
    int dim[2] = {3, 4};
    // dim2 has array type, so it can't be assigned to
    dim2[0] = dim;
    return dim[0];
}