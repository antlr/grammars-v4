function min(x,y)
    return x<y?x:y;

function max(x,y)
    return x>y?x:y;

if(min(100,200)>max(50,20))
    print("I'm useless statement just to show up the if/else\n");
else
    print("squirrel!!\n");

print("\n")

function typy(obj)
{
    switch(typeof obj)
    {
        case "integer":
        case "float":
            return "is a number";
        case "table":
        case "array":
            return "is a container";
        default:
            return "is other stuff"
    }
}

local a=1,b={},c=function(a,b){return a+b;}

print("a "+typy(a)+"\n");
print("b "+typy(b)+"\n");
print("c "+typy(c)+"\n");
