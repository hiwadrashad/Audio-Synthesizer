namespace Web.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open System.Diagnostics
open System.Web
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open System.Net.Mime;
open Web.Models

type HomeController (logger : ILogger<HomeController>) =
    inherit Controller()

    member this.MusicMaker() =
     this.View()

     member this.MusicMaker(score : string) =
      match Assembler.aseembleToPackAndStream score with
       | Choice1Of2 ms ->
        this.Response.Headers("Content-Disposition",
        ContentDisposition(FileName = "ringring.wav",Inline=false).ToString())
        ms.Postion <- 0L
        this.File(ms,"audio/x-wav")
       | Choice2Of2 err -> failwith err


    [<ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)>]
    member this.Error () =
        let reqId = 
            if isNull Activity.Current then
                this.HttpContext.TraceIdentifier
            else
                Activity.Current.Id

        this.View({ RequestId = reqId })
