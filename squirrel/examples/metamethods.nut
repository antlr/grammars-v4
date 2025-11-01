
local base_vec={
    function _add(n)
    {
        return {
            x=x+n.x,
            y=y+n.y,
            z=z+n.z,
        }
    }
    function _sub(n)
    {
        return {
            x=x-n.x,
            y=y-n.y,
            z=z-n.z,
        }
    }
    function _div(n)
    {
        return {
            x=x/n.x,
            y=y/n.y,
            z=z/n.z,
        }
    }
    function _mul(n)
    {
        return {
            x=x*n.x,
            y=y*n.y,
            z=z*n.z,
        }
    }
    function _modulo(n)
    {
        return {
            x=x%n,
            y=y%n,
            z=z%n,
        }
    }
    function _typeof() {return "vector";}
    function _get(key)
    {
        if(key==100)
        {
            return test_field;
        }
    },
    function _set(key,val)
    {
        ::print("key = "+key+"\n");
        ::print("val = "+val+"\n")
        if(key==100)
        {
            return test_field=val;
        }
    }
    test_field="nothing"
}

function vector(_x,_y,_z)
{
    return {x=_x,y=_y,z=_z }.setdelegate(base_vec);
}
////////////////////////////////////////////////////////////

local v1=vector(1.5,2.5,3.5);
local v2=vector(1.5,2.5,3.5);

local r=v1+v2;


foreach(i,val in r)
{
    print(i+" = "+val+"\n");
}

r=v1*v2;

foreach(i,val in r)
{
    print(i+" = "+val+"\n");
}

r=v1/v2;

foreach(i,val in r)
{
    print(i+" = "+val+"\n");
}

r=v1-v2;

foreach(i,val in r)
{
    print(i+" = "+val+"\n");
}

r=v1%2;

foreach(i,val in r)
{
    print(i+" = "+val+"\n");
}

print(v1[100]+"\n");
v1[100]="set SUCCEEDED";
print(v1[100]+"\n");

if(typeof v1=="vector")
    print("<SUCCEEDED>\n");
else
    print("<FAILED>\n");
