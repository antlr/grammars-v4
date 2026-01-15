double *d_ptr;

int update_thru_ptr(double new_val) {
    *d_ptr = new_val;
    return 0;
}