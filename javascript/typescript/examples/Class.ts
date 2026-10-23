
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


export class myClass {
    public async foo(    ): Promise<any> {
    }
}


let emp = new Employee(100,"Steve");

import {Controller, Get, Post} from '@nestjs/common';

export class NotController {
    @Post()
    notControllerPost(body) {
        return 'This is not an api method';
    }
}

export default class CustomerModel {
    constructor(data) {
        this.cardAccountId = data.cardAccountId;
        this.accountHolderId = data.accountHolderId;
        this.firstName = data.firstName;
        this.lastName = data.lastName;
    }
}