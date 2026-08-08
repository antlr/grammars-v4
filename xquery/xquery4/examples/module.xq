xquery version "4.0";
declare namespace ex = "http://example.com";
declare variable $base as xs:string := "http://example.com/";
declare function ex:greet($name as xs:string) as xs:string {
    concat("Hello, ", $name, "!")
};
ex:greet("World")
