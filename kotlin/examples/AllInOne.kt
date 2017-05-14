// Nullability

fun main(args : Array<String>) { 
    val firstName : String = "Adam"
    val name : String? = firstName 
    print("$name") 
}

// Type inference

fun main(args : Array<String>) { 
    val firstName = "Adam" 
    val middle = 'c' 
    val lastName = "Brown" 
    val age = 15 
    println("$firstName $middle $lastNameis $age") 
}

// Functions

fun main(args : Array<String>) { 
    greet(englishGreeting()) 
    greet(italianGreeting())
} 

fun greet(msg : String){ 
    println(msg) 
} 
    
fun englishGreeting() : String = "Hello world" 
    
fun italianGreeting() : String{ 
    return "bon giorno" 
}

// Single expression

fun main(args : Array<String>) {
   val res = add(1,1)
   show("$res")
}

fun add(a : Int, b : Int) = a + b
fun show(msg : String) = println("$msg")

// Optional parameters

fun main(args : Array<String>) {
  show()
  show("Good morning")
}

fun show (msg : String = "Hello World"){
    println("$msg") 
}

// Arguments

fun main(args : Array<String>) { 
    greet(firstName = "Frasensco", lastName = "Merini") 
    greet(lastName = "John", firstName = "Stamos") 
    greet("Borat", "Ismail") 
    greet("Crystal", lastName = "Stamos") 
    call("Xavier", age = 20, location = "Portugal") 
} 
    
fun greet(firstName : String, lastName : String){
    println("Good morning $firstName $lastName") 
} 
    
fun call(name : String, location : String, age : Int){ 
    println("Call $name who lives at $location and he is $age old") 
}

// Variable arguments
 
fun main(args : Array<String>) {
  names("John", "Adam", "Joy", age = 20)
}

/* TODO: vararg does not work properly.
fun names(vararg  names : String, age : Int){
  for(n in names){
    println("$n is $age old")
  }
}*/

// Using array to supply variable arguments

fun main(args : Array<String>) {
  val n = array("John", "Adam", "Joy")
  names(*n)
}

/* TODO: vararg does not work properly.
fun names(vararg  names : String){
  println("Argument length is ${names.size}")
  println("${names[0]}")
  val nns : Array<String> = names
  println("${nns[1]}")
}*/

// Passing one varargs argument to another
fun main(args : Array<String>) {
  val n = array("John", "Adam", "Joy")
  fugitives(*n)
}
,  
fun fugitives(vararg escapees: String){
  names(*escapees) 
}

fun names(vararg  names : String){
  println("Argument length is ${names.size}")
  println("${names[0]}")
  val nns : Array<String> = names
  println("${nns[1]}")
}

// Function Types and Function Literals

// Different ways to write function literals

val m = { (x : String) -> println("$x") } 
val n : (String) -> Unit = { x -> println("$x") } 
val o : (String) -> Unit = { (x : String) -> println("$x") } 

fun main(args : Array<String>) { 
    m("good morning")
    n("good morning") 
    o("good morning") 
}

// Below is a function that returns a function type

fun main(args : Array<String>) { 
    val greet = greetingFrom("Cairo, Egypt") 
    greet("Brown") 
} 

fun greetingFrom(location : String) : (String) -> Unit{ 
    return { name -> println ("Hello $name from $location")}
}

fun evening(): String = "Good Evening" 
fun main(args : Array<String>){ 
    say({ "good morning"}) 
    say { val msg = "good afternoon" msg } 
    say({evening()})
} 

fun say(greet : () -> String){ 
    println("${greet()}") 
}

// Callable references

fun main(args : Array<String>) {
 calcAndShow(10,10, ::add) //20
 calcAndShow(10,10, ::multiply) /100
 calcAndShow(10,19, { x, y -> x - y }) //-9
}

fun calcAndShow(a : Int, b : Int,  func : (a : Int, b : Int) -> Int){
 val result = func (a, b)
 println("$result")
}

fun add(a : Int, b : Int) : Int = a + b
fun multiply (a : Int, b : Int) : Int = a * b

// Function expansion

fun main(args : Array<String>) {
    val a =  calculate(1) { x -> 10 + x } //11
    val b = calculate(2) { x -> 20 * x } //40

    println("a = $a, b = $b")
}

fun calculate(a : Int,  calc : (Int) -> Int) : Int{
    return calc(a)
}

// Closure

fun main(args : Array<String>) {
    val total = add(1)(2)
    println("Total value is $total")
}

fun add(a : Int) : (Int) -> Int{
    return { x -> a + x }
}

// Local function

fun main(args : Array<String>){ 
    accumulate() 
} 

fun accumulate(){
    var i = 0 

    fun add(){ 
        i++ 
    } 

    for (i in 1..10){
        add() 
    } 

    println("i is now $i")  //It prints "i is now 10"
}

// Extension function

fun Int.show(){
    println("This number is $this")
}
    
fun main(args : Array<String>){
    3.show()
}

fun Int?.show(){
    println("This number is $this")
}
 
fun Int.show2(){
    println("This number is $this")
}

fun main(args : Array<String>){
    var number : Int? = null
    number.show()
    5.show()
    //number.show2() will not compile
}

// Extension function expressed in function literals

val show = { Int.() -> println("This is number $this") }
val add = { Int.(number : Int) : Int -> 
    val now = this + number
    now
}

fun main(args : Array<String>){
    5.add(10).show()
}

// Extension function in infix form

fun main(args : Array<String>) {
   val res = 1 add 2
   println("$res")
}

fun Int.add (one : Int) : Int = this + one

// Variable arguments and function type argument

fun main(args : Array<String>) {
  names("John", "Adam", "Joy"){ 
    name  -> println ("$name")
  }
}

fun names(vararg  names : String, print : (String) -> Unit){
  for(n in names){
   print(n)
  }
}

// Control Structures

// If statement

fun main(args : Array<String>) {
  val total = 10
  
  if (total > 5){
      println("$total is greater than 5") 
  }else if (total > 10){
      println("$total is greater than 10")
  }else{
      println("$total is less than 6")
  }
}





