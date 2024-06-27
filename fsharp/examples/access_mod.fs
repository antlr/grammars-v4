namespace Messages
 
type public MessageFuncType = unit -> unit
 
module internal Texts = 
    let public hello = "Hello"                  // доступно везде в проекте
    let internal goodMorning = "Good Morning"   // доступно только в пределах проекта Messages
    let private goodEvening = "Good Evening"    // доступно только в пределах модуля Text
 
module public Funcs = 
    let public sayHello() = printfn $"{Texts.hello}"            // доступно везде
    let internal sayMorning() = printfn $"{Texts.goodMorning}"  // доступно только в пределах проекта Messages
    //let public sayEvening() = printfn $"{Texts.goodEvening}"  // так нельзя - не доступа к Texts.hello