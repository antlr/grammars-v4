#include <iostream> 
#include <regex> 
#include<string.h>
#include<stack>
#include<conio.h>
using namespace std;
int main()
{
	int i = 0;
	int y = 1;
	int&const icr = i; //const reference
	int& const icr1 = i; // const reference 
	int &const icr2 = i; //const reference
	int & const icr3 = i; //const reference
	icr = y;          
	icr = 99;         
	int x = 9;
	icr = x;
	cout << "icr: " << icr << ", y:" << y << endl;


	_getch();
	return 0;
}
