module App 

open Elmish
open Elmish.WPF
open System
open FsXaml
open System.Collections.ObjectModel

module ToDoItem =
      
    type ToDoItem = {
        itemId : int
        content : string
    }

    type Msg =
        | DeleteItem
    
    let update (model:ToDoItem) (msg:Msg) =
        match msg with
        | _ -> model,Cmd.none

    let bindings() = [
        "Content" |> Binding.oneWay (fun m -> m.content)
        "Delete" |> Binding.cmd (fun _ -> DeleteItem)
    ]


open ToDoItem

type State = { 
    currentId : int
    items : ToDoItem list
    currentBuffer : string
} 

type Msg = 
    | BufferChanged of string
    | AddItem
    | DoNothing
    | TdMessages of (int*ToDoItem.Msg)
        


let init() = { currentBuffer = "";currentId = 0;items = []}, Cmd.none

let timeout (n, msg) = async {
    do! Async.Sleep n 
    return msg 
}

let update msg state =  
    match msg with 
    | AddItem -> 
        let newState = {currentBuffer = "";currentId = state.currentId + 1;items = {itemId = state.currentId;content = state.currentBuffer} :: state.items}
        newState,Cmd.none
    | BufferChanged v ->
        {state with currentBuffer = v},Cmd.none

    | TdMessages (id,ToDoItem.Msg.DeleteItem) ->
        {state with items = state.items |> List.filter (fun e -> e.itemId <> id)}, Cmd.none

    | TdMessages (id,msg) ->
        let (newItems,cmds) =
            state.items 
            |> List.map (fun m -> if id = m.itemId then ToDoItem.update m msg else m,Cmd.none)
            |> List.unzip

        {state with items = newItems}, Cmd.batch cmds
    | DoNothing -> 
        state, Cmd.none

let bindings model dispatch = [
    "currentBuffer" |> Binding.twoWay (fun state -> state.currentBuffer) (fun v _ -> v |> BufferChanged)
    "items" |> Binding.subModelSeq (fun x -> x.items) (fun e -> e.itemId) (fun () -> ToDoItem.bindings()) TdMessages
    "AddItem" |> Binding.cmd (fun _ -> AddItem)
]
type MainWindow = XAML<"MainWindow.xaml"> 

[<EntryPoint; STAThread>]
let main argv =  
    Program.mkProgram init update bindings
    |> Program.runWindow (MainWindow())