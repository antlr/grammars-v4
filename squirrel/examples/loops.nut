local arr=["one","two","three"]

::print("FOREACH\n");

foreach(i,val in arr)
{
    ::print("index ["+i+"]="+val+"\n");
}

::print("FOR\n");

for(local i=0;i<arr.len();i+=1)
{
    ::print("index ["+i+"]="+arr[i]+"\n");
}

::print("WHILE\n");

local i=0;
while(i<arr.len())
{
    ::print("index ["+i+"]="+arr[i]+"\n");
    i+=1;
}
::print("DO WHILE\n");

local i=0;
do
{
    ::print("index ["+i+"]="+arr[i]+"\n");
    i+=1;
}while(i<arr.len());
