int main(void) {
    switch(4) {
        case 0:
            return 0;
        case 4: {
            int acc = 0;
            // make sure we can use continue inside a loop
            // inside a switch
            for (int i = 0; i < 10; i = i + 1) {
                if (i % 2)
                    continue;
                acc = acc + 1;
            }
            return acc;
        }
    }
    return 0;
}