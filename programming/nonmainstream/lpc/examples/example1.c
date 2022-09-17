inherit "/inc/global.h";

int x = 0;

int init(){
    int a = 0;
    int b = a;
    int c = "/actor/adder"->act(a, b);
    return c;
}

