#! /usr/bin/env node
// class field proposal, stage3 (2019/11/03)
class CF {
    #x = 12345;
    y = id;
    #priv_method(){
        this.#x++;
    }
    async * #method() {
    }
    #priv_var = 9961;

}

var a = b??c;