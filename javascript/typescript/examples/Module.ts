export let age : number = 20;
export class Employee {
    empCode: number;
    empName: string;
    constructor(name: string, code: number) {
        this.empName = name;
        this.empCode = code;
    }
    displayEmployee() {
        console.log ("Employee Code: " + this.empCode + ", Employee Name: " + this.empName );
    }
}
let companyName:string = "XYZ";


module.exports.routes = {
    "GET /lookup/:lookupId": {
        controller: "LookupReferenceController"
    },
    "POST /lookup": {
        controller: "LookupReferenceController"
    },

'/health': function(req, res) {
    return res.json({status: 'UP', version: sails.config.version});
}
}