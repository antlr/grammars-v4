// A fun use of fallthrough - see https://en.wikipedia.org/wiki/Duff%27s_device
int main(void) {
    int count = 37;
    int iterations = (count + 4) / 5;
    switch (count % 5) {
        case 0:
            do {
                count = count - 1;
                case 4:
                    count = count - 1;
                case 3:
                    count = count - 1;
                case 2:
                    count = count - 1;
                case 1:
                    count = count - 1;
            } while ((iterations = iterations - 1) > 0);
    }
    return (count == 0 && iterations == 0);
}
