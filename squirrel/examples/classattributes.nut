class Foo {
    //constructor
    constructor(a)
    {
        testy = ["stuff",1,2,3];
    }
    //attributes of PrintTesty
    </ test = "freakin attribute"/>
    function PrintTesty()
    {
        foreach(i,val in testy)
        {
            ::print("idx = "+i+" = "+val+" \n");
        }
    }
    //attributes of testy
    </ flippy = 10 , second = [1,2,3] />
    testy = null;

}

foreach(member,val in Foo)
{
    ::print(member+"\n");
    local attr;
    if((attr = Foo.getattributes(member)) != null) {
        foreach(i,v in attr)
        {
            ::print("\t"+i+" = "+(typeof v)+"\n");
        }
    }
    else {
        ::print("\t<no attributes>\n")
    }
}
