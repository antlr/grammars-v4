function state1()
{
    ::suspend("state1");
    return state2();
}

function state2()
{
    ::suspend("state2");
    return state3();
}

function state3()
{
    ::suspend("state3");
    return state1();
}

local statethread = ::newthread(state1)

::print(statethread.call()+"\n");

for(local i = 0; i < 10000; i++)
    ::print(statethread.wakeup()+"\n");
