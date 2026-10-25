function factorial(n)
{   if (n == 0)
        return 1;
    else
        return n * factorial(n-1);
}
var i;
document.clear();
for (i = 0; i <= 16; i++)
    document.write(i + "! = " + factorial(i) + "<br />");
    