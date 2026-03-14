using IO = System.IO;

class Foo<T> { public void method() {} }
class C<T>
{
	public void M<T2>(int a, T b)
	{
	}
}
class @yield
{
	void Foo(object u, __arglist)
	{
		C<object> c = null;
		c.M<int>(5, u);
		TypedReference tr = __makeref(c);
		Type t = __reftype(tr);
		dynamic a = t;
		dynamic b;
		Params(ref a, out b);
		dynamic d = c;
		dynamic e = null;
		Params(ref e, out e);
	}

	void Params(ref dynamic a, out dynamic b, params dynamic[] c) { b = null; }
	void Params(out dynamic a, ref dynamic c, params dynamic[][] d) { a = null; }
	public override string ToString() { return base.ToString(); } 

	public void method()
	{
		int?[] a = new int?[5];/*[] bug*/ // YES []
		int[] var = { 1, 2, 3, 4, 5 };/*,;*/
		int i = (int)a[0];/*[]*/
		Foo<int> fObj = new Foo<int>();/*<> ()*/
		fObj.method();/*().*/
		i = i + i - i * i / i % i & i | i ^ i;/*+ - * / % & | ^*/
		bool b = true & false | true ^ false;/*& | ^*/
		b = !b;/*!*/
		i = ~i;/*~i*/
		b = i < i && i > i;/*< && >*/
		int? ii = 5;/*? bug*/ // NO ?
		int f = true ? 1 : 0;/*? :*/   // YES :
		i++;/*++*/
		i--;/*--*/
		b = true && false || true;/*&& ||*/
		_ = i << 5;/*<<*/
		_ = i >> 5;/*>>*/
		b = i == i && i != i && i <= i && i >= i;/*= == && != <= >=*/
		i += 5;/*+=*/
		i -= i;/*-=*/
		i *= i;/**=*/
		i /= i;/*/=*/
		i %= i;/*%=*/
		i &= i;/*&=*/
		i |= i;/*|=*/
		i ^= i;/*^=*/
		i <<= i;/*<<=*/
		i >>= i;/*>>=*/
		Func<int, int> s = y => y + 1;/*=>*/
		double d = .3;
		Point point;
		unsafe
		{
			Point* p = &point;/** &*/
			p->x = 10;/*->*/
		}
		IO::BinaryReader br = null;
		dynamic x = null;
		x[i: 1] = 3;
		x[i: 1, j: 5] = "str";
	}

	struct Point { public int x; public int y; public void ThisAccess() { this = this; } }
}