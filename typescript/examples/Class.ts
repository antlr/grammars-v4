
interface IPerson {
    name: string;
}

class Person implements IPerson {
    public publicString: string;
    private privateString: string;
    protected protectedString: string;
    readonly readonlyString: string;
    name: string;

    constructor(name: string) {
        this.name = name;
    }
}

class Employee extends Person {
    empCode: number;
    currentUser: any;
    static pi: number = 3.14;

    constructor(empcode: number, name:string) {
        super(name);
        this.empCode = empcode;
    }

    get user() {
        return this.currentUser;
    }

    set user(usr: any) {
        this.currentUser = usr;
    }

    displayName():void {
        console.log("Name = " + this.name +  ", Employee Code = " + this.empCode);
    }
}

let emp = new Employee(100,"Steve");