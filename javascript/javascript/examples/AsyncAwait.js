async function f(){}

class C {
    async method(){}
    static async method1(){}
    static async #method2(){}
    async *gen(){}
}
async ()=>{};

async ()=>{await f();}
async ()=>{await promise;}
async ()=>{await 1;}

async function* gen(){}