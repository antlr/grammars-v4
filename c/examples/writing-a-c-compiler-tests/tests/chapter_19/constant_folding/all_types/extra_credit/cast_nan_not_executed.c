// Make sure the compiler doesn't complain if you try to cast NaN to an int
// in code that isn't executed. (If it actually did execute it would be
// undefined behavior)

static int flse = 0;

int main(void) {
    int retval = 0;
    if (flse) {
        retval = (int) (0.0/0.0);
    }
    return retval;
}