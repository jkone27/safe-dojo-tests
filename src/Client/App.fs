module App

open Elmish
open Fable.FontAwesome
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.Recharts
open Fable.Recharts.Props
open Fulma
open Shared
open Thoth.Json

type ServerState = Idle | Loading | ServerError of string

/// The overall data model driving the view.
type Model =
    { 
      ServerState : ServerState
      Name: string
      Names : string list }

/// The different types of messages in the system.
type Msg =
    | ErrorMsg of exn
    | PostYourName of string
    | PostOk of unit
    | GetNames of string list
    | NameChanged of string

/// The init function is called to start the message pump with an initial view.


let namesDecoder = Decode.Auto.generateDecoder<string list> ()

let getNames (_:unit) = promise {
    return! Fetch.fetchAs "api/name" namesDecoder []
}

let postName name = promise {
    let! postResult = Fetch.postRecord "api/name" { MyName = name } []
    if not postResult.Ok then 
        failwith "unable to post!"
    else
        ()
}

let init () =
    { 
      ServerState = Idle
      Name = ""
      Names = []
    }, Cmd.ofPromise getNames () GetNames ErrorMsg


/// The update function knows how to update the model given a message.
let update msg model =
    match model, msg with
    | _, ErrorMsg e -> { model with ServerState = ServerError e.Message }, Cmd.none
    | _, NameChanged name -> { model with Name = name }, Cmd.none
    |_, PostYourName name -> 
        model, Cmd.ofPromise postName name PostOk ErrorMsg
    |_, PostOk _ -> model, Cmd.none
    |_, GetNames names -> { model with Names = names}, Cmd.none

[<AutoOpen>]
module ViewParts =
    let basicTile title options content =
        Tile.tile options [
            Notification.notification [ Notification.Props [ Style [ Height "100%"; Width "100%" ] ] ]
                (Heading.h2 [] [ str title ] :: content)
        ]
    let childTile title content =
        Tile.child [ ] [
            Notification.notification [ Notification.Props [ Style [ Height "100%"; Width "100%" ] ] ]
                (Heading.h2 [ ] [ str title ] :: content)
        ]

    let nameInput model dispatch = 
        Field.div [] [
                    Label.label [] [ str "Name" ]
                    Control.div [ Control.HasIconLeft; Control.HasIconRight ] [
                        Input.text
                            [ Input.Placeholder "Ex: whatever"
                              Input.Value model.Name
                              Input.Props [ OnChange (fun ev -> dispatch (NameChanged !!ev.target?value))] ]
                        Fulma.Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ Fa.Solid.Home ] [] ]
                    ]
                ]

    let postButton model dispatch =
        Field.div [ Field.IsGrouped ] [
                    Level.level [ ] [
                        Level.left [] [
                            Level.item [] [
                                Button.button
                                    [ Button.IsFullWidth
                                      Button.Color IsPrimary
                                      Button.OnClick (fun _ -> dispatch (PostYourName(model.Name)))
                                      Button.IsLoading (model.ServerState = ServerState.Loading) ]
                                    [ str "Post" ]]
                        ]
                    ]
                ]

    let serverError error =
        Field.div [] [
                        Tag.list [ Tag.List.HasAddons; Tag.List.IsCentered ] [
                            Tag.tag [ Tag.Color Color.IsDanger; Tag.Size IsMedium ] [
                                str error
                            ]
                        ]
                    ]

    let header model =
        Heading.h1 [ ] [
                        (let name = 
                            match model.Name with
                            |"" -> "we don't know your name"
                            |n -> sprintf "your name is %s" n
                        str name)
                    ]

    let loadNames model =
        let namesElements = model.Names |> Seq.map (fun n -> Field.p [] [ str n ])
        Field.div [] namesElements



/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view model dispatch =
    div [] [
        Hero.hero [ Hero.Color Color.IsInfo ] [
            Hero.body [ ] [
                Container.container [ Container.IsFluid
                                      Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                    header model 
                ]
            ]
        ]

        Container.container [] [
            yield loadNames model
            yield nameInput model dispatch
            yield postButton model dispatch 
            match model with
            | { ServerState = ServerError error } ->
                yield serverError error 
            |_ -> yield Field.div [] []
        ]

        br [ ]

        Footer.footer [] [
            Content.content
                [ Content.Modifiers [ Fulma.Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ]
                [ safeComponents ]
        ]
    ]