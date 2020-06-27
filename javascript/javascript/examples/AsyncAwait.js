async function f(){}

class C {
    async method(){}
    static async method1(){}
    async static #method2(){}
    async *gen(){}
    async get v(){return 1};
}
async ()=>{};

async ()=>{await f();}
async ()=>{await promise;}
async ()=>{await 1;}

async function* gen(){}