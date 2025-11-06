local ex = regexp("[a-zA-Z]+");
local string = "123 Test; strlen(str);";
local res = ex.search(string);
print(string.slice(res.begin,res.end)); //prints "Test"
print("\n");
ex = regexp(@"\m()");
string = "123 Test; doSomething(str, getTemp(), (a+(b/c)));";
res = ex.search(string);
print(string.slice(res.begin,res.end)); //prints "(...)"
print("\n");
