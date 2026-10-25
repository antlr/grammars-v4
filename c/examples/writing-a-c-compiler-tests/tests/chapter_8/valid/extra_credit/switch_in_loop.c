int main(void) {
    int acc = 0;
    int ctr = 0;
    for (int i = 0; i < 10; i = i + 1)  {
        // make sure break statements here break out of switch but not loop
        switch(i) {
            case 0:
                acc = 2;
                break;
            case 1:
                acc = acc * 3;
                break;
            case 2:
                acc = acc * 4;
                break;
            default:
                acc = acc + 1;
        }
        ctr = ctr + 1;
    }

    return ctr == 10 && acc == 31;
}
