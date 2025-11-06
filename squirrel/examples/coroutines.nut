function coroutine_test(a,b)
{
    ::print(a+" "+b+"\n");
    local ret = ::suspend("suspend 1");
    ::print("the coroutine says "+ret+"\n");
    ret = ::suspend("suspend 2");
    ::print("the coroutine says "+ret+"\n");
    ret = ::suspend("suspend 3");
    ::print("the coroutine says "+ret+"\n");
    return "I'm done"
}

local coro = ::newthread(coroutine_test);

local susparam = coro.call("test","coroutine"); //starts the coroutine

local i = 1;
do
{
    ::print("suspend passed ["+susparam+"]\n")
    susparam = coro.wakeup("ciao "+i);
    ++i;
}while(coro.getstatus()=="suspended")

::print("return passed ["+susparam+"]\n")
