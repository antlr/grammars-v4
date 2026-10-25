extern double *d_ptr;
int update_thru_ptr(double new_val);

int main(void) {
    double d = 0.0;
    d_ptr = &d;
    update_thru_ptr(10.0);
    return (d == 10.0);

}