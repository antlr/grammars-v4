typedef long long int sqlite_int64;
typedef sqlite_int64 sqlite3_int64;
typedef sqlite_int64 i64; /* 8-byte signed integer */

static void inttoptrFunc(
){
  sqlite3_int64 i64;
  i64 = 1;
}

struct Point { int x; int y; };   // declares struct tag, no variable
union Value { int i; float f; };  // declares union tag, no variable
enum Color { Red, Green, Blue };  // declares enum, no variable
