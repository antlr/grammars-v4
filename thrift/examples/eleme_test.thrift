typedef map<string, map<string, i16>> T1

typedef map<S1, string> T2

exception E1 {
  1: required string name,
  2: required string message
}

struct S1 {
  1: required i32 a
}

struct Args {
  1: required list<S1> list1,
  2: required T1 map1,
  3: required T2 map2
}

struct DefResArg {
  1: i32 i = 233,
  2: string s = "hehe"
}

struct I64Data {
  1: i64 data
}

service Test {

  Args test(1: list<S1> list1, 2: T1 map1, 3: T2 map2) throws (1: E1 exception1);

  void void_call();

  oneway void oneway_set_hehe(1: double hehe);
  double get_hehe();

  binary bin(1: binary data);

  i64 bignumber(1: i64 data);

  bool unknown();

  void required_a(1: required i32 a);

  void arr(1: list<i32> arr);

  S1 response_a();

  map<i32,string> def_req_arg(1: i32 i = 233, 2: string s = "hehe");

  DefResArg def_res_arg();

  map<string,i32> zero(1: i32 zero);

  I64Data readi64(1: i64 data);

}