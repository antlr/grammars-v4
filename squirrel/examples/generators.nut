/*
*Random number function from The Great Computer Language shootout
*converted to a generator func
*/

function gen_random(max) {
    local last=42
    local IM = 139968;
    local IA = 3877;
    local IC = 29573;
    for(;;){  //loops forever
        yield (max * (last = (last * IA + IC) % IM) / IM);
    }
}

local randtor=gen_random(100);

print("RAND NUMBERS \n")

for(local i=0;i<10;i+=1)
    print(">"+resume randtor+"\n");

print("FIBONACCI \n")
function fiboz(n)
{
    local prev=0;
    local curr=1;
    yield 1;

    for(local i=0;i<n-1;i+=1)
    {
        local res=prev+curr;
        prev=curr;
        yield curr=res;
    }
    return prev+curr;
}

foreach(val in fiboz(10))
{
    ::print(">"+val+"\n");
}
