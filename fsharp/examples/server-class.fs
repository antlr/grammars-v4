[<Sealed>]
type WebServerBuilder(settings : Settings.T) =
    let mutable engine = Unchecked.defaultof<IWebHost>
    let mutable thread = Unchecked.defaultof<Task>
    
    // Port on which server should start (Defaults to 9800)
    let port = 
        let conf = settings.ConfigurationSource.[Settings.ServerKey + ":" + Settings.HttpPort]
        if conf = null then "9800" else conf
    
    //  Indicates if FlexSearch should run over HTTPS or not.
    let useHttps = settings.GetBool(Settings.SecurityKey, Settings.UseHttps, false)

    let webHostBuilder =
        let config = settings.ConfigurationSource
        let webAppBuilder = new WebHostBuilder()
        webAppBuilder.UseConfiguration(config)
                     .UseKestrel(fun o -> 
                        if useHttps then 
                            let certPass = Constants.CertificatePassPath
                                           |> File.ReadAllBytes 
                                           |> decrypt
                            o.UseHttps(Constants.CertificatePath, certPass) |> ignore)
                     .ConfigureServices(fun s -> s.AddSingleton<IConfiguration>(config) |> ignore)
                     .UseStartup<WebServer>()
    do
            // The reason we need to reset the performance counters is that sometimes 
            // the counters cache in the registry becomes corrupted.
            // http://stackoverflow.com/questions/17980178/cannot-load-counter-name-data-because-an-invalid-index-exception
            // The performance counters are used for the /memory endpoint.
            if settings.GetBool(Settings.ServerKey, Settings.ResetPerformanceCounters, true)
            then Installers.resetPerformanceCounters()

    member _.Stop() = shutdown()