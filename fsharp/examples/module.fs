module Hello
 
module Messages = 
    let hello = "Hello"
    let goodMorning = "Good Morning"
    let goodEvening = "Good Evening"
 
    module RusMessages = 
        let hello = "Привет"
        let goodMorning = "Добрый день"
        let goodEvening = "Добрый вечер"
 
let printHello() = printfn $"{Messages.RusMessages.hello}"