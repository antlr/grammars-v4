// CData sample

<!--//--><![CDATA[//><!--
//--><!]]>

// Arrow functions, https://strongloop.com/strongblog/an-introduction-to-javascript-es6-arrow-functions/

var func = function (parameters) { return expression; }
var func = () => { return 1; }
var func = () => 1

var func = function (a) { return a * 2; }
var func = (a) => { return a * 2; }
var func = (a) => a * 2
var func = a => a * 2

var func = function (a, b) { return a * b; }
var func = (a, b) => { return a * b; }
var func = (a, b) => a * b

var func = function () { return arguments[0]; }
var func = (...args) => args[0]