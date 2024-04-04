let countryCode = "de"
 
let country = 
    if countryCode = "fr" then
        printfn "Выбрана страна: Франция"
        printfn "Язык: французский"
        "France"
    elif countryCode = "de"  then
        printfn "Выбрана страна: Германия"
        printfn "Язык: немецкий"
        "Germany"
    else
        printfn "Страна по умолчанию: США"
        printfn "Язык: английский"
        "USA"
         
printfn $"country: {country}"   // Germany