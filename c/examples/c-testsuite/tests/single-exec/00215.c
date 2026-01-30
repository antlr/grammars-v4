extern int printf(const char *format, ...);
static void kb_wait_1(void)
{
    unsigned long timeout = 2;
    do {
        if (1) printf("timeout=%ld\n", timeout);
        else
        {
            while (1)
                printf("error\n");
        }
        timeout--;
    } while (timeout);
}
static void kb_wait_2(void)
{
    unsigned long timeout = 2;
    do {
        if (1) printf("timeout=%ld\n", timeout);
        else
        {
            for (;;)
                printf("error\n");
        }
        timeout--;
    } while (timeout);
}
static void kb_wait_2_1(void)
{
    unsigned long timeout = 2;
    do {
        if (1) printf("timeout=%ld\n", timeout);
        else
        {
            do {
                printf("error\n");
            } while (1);
        }
        timeout--;
    } while (timeout);
}
static void kb_wait_2_2(void)
{
    unsigned long timeout = 2;
    do {
        if (1) printf("timeout=%ld\n", timeout);
        else
        {
            label:
                printf("error\n");
            goto label;
        }
        timeout--;
    } while (timeout);
}
static void kb_wait_3(void)
{
    unsigned long timeout = 2;
    do {
        if (1) printf("timeout=%ld\n", timeout);
        else
        {
            int i = 1;
            goto label;
            i = i + 2;
        label:
            i = i + 3;
        }
        timeout--;
    } while (timeout);
}
static void kb_wait_4(void)
{
    unsigned long timeout = 2;
    do {
	if (1) printf("timeout=%ld\n", timeout);
        else
        {
            switch(timeout) {
                case 2:
                    printf("timeout is 2");
                    break;
                case 1:
                    printf("timeout is 1");
                    break;
                default:
                    printf("timeout is 0?");
                    break;
            };
            /* return; */
        }
        timeout--;
    } while (timeout);
}
int main()
{
    printf("begin\n");
    kb_wait_1();
    kb_wait_2();
    kb_wait_2_1();
    kb_wait_2_2();
    kb_wait_3();
    kb_wait_4();
    printf("end\n");
    return 0;
}
