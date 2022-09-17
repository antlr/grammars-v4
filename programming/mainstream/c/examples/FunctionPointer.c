//function pointer
typedef
void *
(*f1)(
          const MyType        *param1,
          long             param2,
    void              *param3,
          short             param4
  );


typedef
int
(*f2)(
          const MyType        *param1,
          long             param2,
    char              *param3,
          int             param4
  );


typedef
MyStruct
( *f3 ) (
          const MyType        *param1,
          double             param2,
    float              *param3,
          long             param4
  );