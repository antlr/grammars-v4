#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <xmmintrin.h>


//note: directAbstractDeclarator not used
extern int ext;
/* comment
 *
 * */
//line comment

/*
//

staticAssertDeclaration*/

const int tab[] = {1,2,3};

struct empty{};

//sample typeQualifierList
struct TypeQualifierListSample {
    int val;
    struct elem * const right;
    struct elem * restrict r2;
    struct elem * volatile v;
    struct elem * _Atomic a;
    struct elem * const volatile cv;
    int bits1:4;
    int bits2:03;
    int (*g) //directDeclarator |   '(' declarator ')'
    //without ending ;
};

struct TwoSemi {
  int a;;
  int bb;;
};

//int bits1:4; int bits2:03; can't compile
int gener0() __asm("ret");
int gener1(a,b,c){return 0;}; //empty list: directDeclarator |   directDeclarator '(' identifierList? ')'
int gener2() __attribute__((__error__("message")));


struct elem {
    int val;
    struct elem * right,
       * left; //must be 'struct' before struct name
};

void access() {
   struct elem el;
   el.val = 0;
   int tab[10];
   tab[4] = 5;
}

//designation,designatorList,designator
int table[40] = {[3]=5,[6]=7};
struct elem designated = {.val=46};


struct Gener {
    int val;
    //must be 'struct' before struct name
    struct elem * right, // __asm ("ret"), = not compile
       * left; //__attribute__((__error__("message"))); only for functions
};


typedef struct elem Node;
typedef struct elem* NodePtr;

Node node1;
Node* nodePtr;
NodePtr nodePtr; //variable redefinition with the same type is allowed in C

void insert(Node ** tree, struct elem * item) {
    if(!(*tree)) {
        *tree = item;
        return;
    }
    if(item->val < (*tree)->val)
        insert(&(*tree)->left, item);
}

void printout(Node * tree) {
    if(tree->left) printout(tree->left);
    printf("%d\n",tree->val);
}

_Thread_local int threadVar;

int spec() {
    static int staticVar = 5;
    /*
     stackoverflow.com:
        Auto is just implicit in C, but due to how rarely (read never) it is used
        in actual code explicitly its meaning has changed in C++11.
        Auto just specifies automatic storage, meaning the variable will go away when
        it goes out of scope.
     * */
    auto int a101;
    register int reg1;
    return 0;
}

int types() {
    char c;
    short sh;
    int i;
    long l;
    float f;
    double d;
    signed sg;
    unsigned u;
    _Bool b;
    _Complex double z;
    _Complex zdefault;

    //not in language, but defined in xmmintrin.h
    __m128 m128;
    __m128d m128d;
    __m128i m128i;
    return 0;
}

union u{
    int a;
    float f;
};

enum {FIRST, SECOND};
enum A {A1, A2};

inline int funcSpecifier0() {return 0;}
_Noreturn int funcSpecifier1() {}
__inline__ int funcSpecifier2() {return 0;}
//__stdcall int funcSpecifier3(int b) {return b;} //is not supported for this target
//__declspec(dllexport) int funcSpecifier4(int b){return b;} //only when -fms-extensions

int sum()
{
    int i, sum = 0;
    int _Alignas(int) g;
    //int* _Alignas(int*) gp;; //* abstractDeclarator , not compile
    const int LAST1 = 4;
    const LAST2 = 3+LAST1;
    const LAST3 = 10,be=4;
    const int LAST = 20;
    for ( i = 1; i <= LAST; i++ ) {
        sum += i;
    } /*-for-*/
    printf("sum = %d\n", sum);

    return 0;
}

void returnByPointer(int *n) {
    *n = 5;
}

void numberLiterals() {
    double d = 11.4;
    double d1 = 12.4l;
    double d2 = 12.4L;
    float fl1 = 12.4;
    float fl2 = 11.4f;
    float fl3 = 13.4F;
    int bin1 = 0b0011;
    int bin2 = 0B1010;
}

/*
Func call with various arg numbers.
And func forward declarations.
And func call as func call argument.
*/

void aX(void);
int a102(int param1);
int a102(int param1){return 6;};
int a2(int param1, int param2); //params must have type: int a2(int param1, param2); param2 is bad
void params(a,b,c); //allowed all without types
void a3();
void a3(void); //not prefered style
void a4(int, ...);
void a4(int param1, ...);

int f(int arg1, char arg2)
{
    a102(arg1);
    a2(arg1, arg2);
    a3();
    return 5;
}
int a11(int param1){ return 64;}
int a2(int param1, int param2){return 50;}
void a3(){}

int oldParams(block,maxn) //old style params, must be all in new or old style
        char *block; int maxn;{
            return 100;
        }

void params(a,b,c) {

}

//function pointer
typedef int MyType;

typedef
void *
(*F1)(
        const MyType        *param1,
        long             param2,
        void              *param3,
        short             param4
);

void *f1(
        const MyType        *param1,
        long             param2,
        void              *param3,
        short             param4
){
    return malloc(100);
};


typedef
int
(*F2)(
        const MyType        *param1,
        long             param2,
        char              *param3,
        int             param4
);

int f2(
        const MyType        *param1,
        long             param2,
        char              *param3,
        int             param4
){
    return 100;
};


typedef struct elem MyStruct;

typedef
MyStruct*
( *F3 ) (
        const MyType        *param1,
        double             param2,
        float              *param3,
        long             param4
);

MyStruct  *f3(
        const MyType        *param1,
        double             param2,
        float              *param3,
        long             param4
){
    MyStruct myStruct;
    return &myStruct;//syntex correct, but bad behavior returning stack variable
};

MyStruct  f4(
        const MyType        *param1,
        double             param2,
        float              *param3,
        long             param4
){
    MyStruct myStruct;
    return myStruct;//return not pointer but whole structure
};

void * /*__cdecl*/ ff1(unsigned int param1){ return NULL;}; //can't be cdecl
int tab1 [20][34];

void directDeclarator(int tab2 [static 20], int tab3 [const restrict volatile _Atomic static 20],
                        int tab4 [const restrict volatile _Atomic]) { //without *
    int tab1 [20];
    int (tab);
    //int k:4;

    struct Bits{
        int g:4;
    } bits;
}

float fpow(float x)
{
    float x0,xn,h,y[20],so,se,ans,xarr[20];
    char* p = (char*) 0; //* abstractDeclarator
    return (float)(1/(1+pow(x,2)));
}

typecast() //default return type is int
{
    float b1 = 4;
    long l = 100;
    int a1 = (int)(b1);
    int* a3 = (int *)(l);
    int **a4 = (int **)(l);
    return 0;
}


struct A1
{
    const char *p;
};

struct B
{
    char p[10];
    struct A1 a;
};

void builtin_offsetof ()
{
    //not used
    __builtin_offsetof(struct A1, p); // OK
    //__builtin_offsetof(struct A, p[0]); // { dg-error "non constant address" }
    __builtin_offsetof(struct B, p[0]); // OK
    __builtin_offsetof(struct B, p[9]); // OK
    __builtin_offsetof(struct B, p[10]); // OK
    __builtin_offsetof(struct B, p[11]); // { dg-warning "greater than size" }
    __builtin_offsetof(struct B, a.p); // OK
    __builtin_offsetof(struct B, p[0]); // OK
    //__builtin_offsetof(struct B, a.p[0]); // { dg-error "non constant address" }

    va_list p;
    __builtin_va_arg(p, struct A1); // OK

    /* below is not supported by grammar
    __extension__ printf("abc"); //expression (with variable declaration), not compound statement
    __extension__ 2+3;
    __extension__ int n = 5;*/
    //__extension__ if (n>4) n=5; //bad
    int n2 = _Alignof(int);
}

void caseSample() {
    int f = 4, n=5;
    switch (f) {
        case 1:n=4;break;
        case 2:n=5; n++; break;
        default: return;
    }
    if (f!=4) if (f>3) n=5; else n=6;
}

/* compiler not uses this
void qsort_b(void *base, size_t nel, size_t width, int (^compar)(const void *, const void *));
char ^other;
*/

void loops() {

    for (int i=0; i<10; i++) {}
    for (;;) {break;}
    int n = 0;
a:
    while (n<10) n++;
    do n--; while(n>0);
    if (n>4) goto a;
}

//this is not C, but test is without preprocessor, it must be temporary
/*#if defined(__cplusplus)
extern "C" {}
#endif*/
int main() {
    sum();
    Node * curr, * root;
    int i;
    F1 f1ptr = f1;
    F2 f2ptr = f2;
    F3 f3ptr = f3;
    void* ptr = f1(&i,111,NULL,1);
    int nn = f2(&i, 10000, "abc",100);
    float f = 312.345f;
    Node* node1 = f3(&i, 12.345,&f,1000);//bad behavior local variable pointer
    Node node2 = f4(&i, 12.345,&f,1000);

    int n = 100;

    volatile int vol;
//    __volatile__ int vol2; //declaration , not statement? - not suuported by grammar
    //__asm ("");
    // gcc's extended inline assembly
    /*__asm__ ("leal (%0,%0,4),%0"
    : "=r" (n)
    : "0" (n));*/
    printf("100*5 = %d\n", n);
    fflush(stdout); // flush is intentional


    /*this is denied
    asm
    {
    mfspr x, 286;
    }
    */

    int n1 = i = 7; //transitive assignment
    returnByPointer(&n1);

    root = NULL;

    for(i=1;i<=10;i++) {
        curr = (Node *)malloc(sizeof(struct elem));
        curr->left = curr->right = NULL;
        curr->val = rand();
        insert(&root, curr);
    }
    root->left->left = NULL;

    printout(root);

    printf ("\
Usage: %s [ignored command line arguments]\n\
  or:  %s OPTION\n\
", "vv");

    typedef int type;
    int gg=100;
    _Generic (gg+2,default : gg=3,int : gg=4);
    _Atomic(int) atomicVar;
    _Static_assert(2<3,"2<3");
// standard inline assembly in C++ , grammar: must be volatile
    __asm__ __volatile__("movq $60, %rax\n\t" // the exit syscall number on Linux
             "movq $2,  %rdi\n\t" // this program returns 2
             "syscall");
    return 0;
}
