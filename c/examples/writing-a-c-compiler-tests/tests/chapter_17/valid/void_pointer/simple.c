/* A simple test of allocating and freeing memory */

void *malloc(unsigned long size);
void free(void *ptr);

int main(void) {
    int *array = malloc(10 * sizeof (int));
    array[2] = 100;
    int result = array[2];
    free(array);
    return result;
}