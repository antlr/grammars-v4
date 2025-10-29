/*translation of the methcall test from The Great Computer Language Shootout
*/

class Toggle {
    bool=null
}

function Toggle::constructor(startstate) {
    bool = startstate
}

function Toggle::value() {
    return bool;
}

function Toggle::activate() {
    bool = !bool;
    return this;
}

class NthToggle extends Toggle {
    count_max=null
    count=0
}

function NthToggle::constructor(start_state,max_counter)
{
    base.constructor(start_state);
    count_max = max_counter
}

function NthToggle::activate ()
{
    ++count;
    if (count >= count_max ) {
      base.activate();
      count = 0;
    }
    return this;
}


function main() {
    local n = vargv.len()!=0?vargv[0].tointeger():1



    local val = 1;
    local toggle = Toggle(val);
    local i = n;
    while(i--) {
      val = toggle.activate().value();

    }
    print(toggle.value() ? "true\n" : "false\n");

    val = 1;
    local ntoggle = NthToggle(val, 3);
    i = n;
    while(i--) {
      val = ntoggle.activate().value();
    }
    print(ntoggle.value() ? "true\n" : "false\n");

}
local start=clock();
main();
print("TIME="+(clock()-start)+"\n");
