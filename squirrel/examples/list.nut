/*translation of the list test from The Great Computer Language Shootout
*/

function compare_arr(a1,a2)
{
    foreach(i,val in a1)
        if(val!=a2[i])return null;
    return 1;
}

function test()
{
    local size=10000
    local l1=[]; l1.resize(size);
    for(local i=0;i<size;i+=1) l1[i]=i;
    local l2=clone l1;
    local l3=[]

    l2.reverse();
    while(l2.len()>0)
        l3.append(l2.pop());
    while(l3.len()>0)
        l2.append(l3.pop());
    l1.reverse();

    if(compare_arr(l1,l2))
        return l1.len();
    return null;
}

local n = vargv.len()!=0?vargv[0].tointeger():1
for(local i=0;i<n;i+=1)
    if(!test())
    {
        print("failed");
        return;
    }

print("oki doki");

