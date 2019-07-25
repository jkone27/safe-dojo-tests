module Api

open FSharp.Data.UnitSystems.SI.UnitNames
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn
open Shared
open FSharp.Control.Tasks
open System.Collections.Generic
open Microsoft.Extensions.Logging

let namesDb = ["John"; "Babe"; "Hoffman"] |> List 

let postName (next:HttpFunc) (ctx: HttpContext) = task {
    let log = ctx.GetLogger()
    log.Log(LogLevel.Information, EventId(1), "post")
    let! name = ctx.BindModelAsync<MyRequest>()
    namesDb.Add name.MyName
    return! json name next ctx
}

let getNames (next:HttpFunc) (ctx: HttpContext) = task {
    let log = ctx.GetLogger()
    log.Log(LogLevel.Information, EventId(2), "get")
    return! json (namesDb) next ctx
}

let apiRouter = router {
    pipe_through (pipeline { set_header "x-pipeline-type" "Api" })
    //forward "/name" controller to unify get and post
    get "/name" getNames
    post "/name" postName
    get "/health" (fun (n:HttpFunc) (c:HttpContext) -> task { return! text "alive" n c })
    not_found_handler (setStatusCode 404 >=> text "Api 404")
    }