let divide a b = 
    try
      Some(a / b)
    with
      | ex when a = 0 -> printfn "Both numbers are zero"; None
      | ex  -> printfn "%s" ex.Message; None