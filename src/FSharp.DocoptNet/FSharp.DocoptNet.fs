module FSharp.DocoptNet

open System
open System.Linq
open DocoptNet
open System.Collections.Generic

type ArgValue = MultiValue of string list
              | Value of string
              | NoValue
              | Counted of int
              | Present
              | NotPresent

type Option = LongOption of string
            | ShortOption of char
            | Command of string
            | Argument of string
            | AnyOption

type NamedOption = NamedOption of string * Option

type Arguments = IDictionary<NamedOption, ArgValue>

let private asOptionString name = sprintf "<%s>" name

let command name = NamedOption (name, Command name)
let longOption name = 
  let name = "--" + name
  NamedOption (name, LongOption name)
let shortOption (name:char) = 
  let optionName = "-" + name.ToString()
  NamedOption (optionName, ShortOption name)
let argument name = 
  let name = asOptionString name
  NamedOption (name, Argument name)

let isPresent (args: Arguments) (namedOption:NamedOption) = 
  let hasKey, v = args.TryGetValue(namedOption)  
  match hasKey, namedOption, v with
  | false, _, _ -> false
  | true, NamedOption (name, _), Present -> true
  | true, NamedOption (name, LongOption n), Value v -> true
  | _ -> false

let getValue (args: Arguments) (option:NamedOption) = 
  match args.TryGetValue(option) with
  | false,_ -> None
  | true,v -> Some v

let getAllArgs<'a when 'a :> IConvertible> (args: Arguments) (NamedOption (name, option)) =
  let valuesAsString =
    match getValue args (NamedOption (name, option)) with
    | Some (MultiValue vs) -> vs
    | Some (Value v) -> [v]
    | _ -> failwithf "no argument given: %s" name
  valuesAsString
  |> Seq.map (fun v -> System.Convert.ChangeType(v, typeof<'a>) :?> 'a)
  |> Seq.toList

let getArg<'a when 'a :> IConvertible> (args: Arguments) (NamedOption (name, option)) = 
  let valueAsString =
    match getValue args (NamedOption (name, option)) with
    | Some (MultiValue (v::vs)) -> v
    | Some (Value v) -> v
    | _ -> failwithf "no argument given: %s" name
  System.Convert.ChangeType(valueAsString, typeof<'a>) :?> 'a

let parseArguments usage (args: string array)  : Arguments =
  let d = new Docopt()
  let schemaDict =
    Docopt.GetFlatPatterns(usage)
    |> Seq.map (fun p -> (p.Name, p))
    |> dict

  let result = d.Apply(usage, args)

  let toOption (pattern:Pattern) = 
    NamedOption (
      pattern.Name, 
      match pattern with
      | :? DocoptNet.Command
        -> Command pattern.Name
      | :? DocoptNet.Option as o
        -> 
          if String.IsNullOrEmpty(o.LongName) 
          then ShortOption o.ShortName.[1]
          else LongOption o.LongName
      | :? DocoptNet.Argument as a
        -> Argument a.Name
      | _ -> failwithf "unhandled toOption %O" pattern
    )
  let toValue (NamedOption (optionName, option)) (v:ValueObject) =
    if v = null then ArgValue.NotPresent
    else
      match option with
      | Command n -> if v.IsTrue then ArgValue.Present else ArgValue.NotPresent
      | LongOption o
        -> 
        match (v.IsTrue, v.IsFalse) with
        | true, false -> ArgValue.Present
        | false, true -> ArgValue.NotPresent
        | _, _ ->
          ArgValue.Value (v.Value.ToString())
      | Argument a
        ->
          if v = null then ArgValue.NotPresent
          else
            match v.Value with
            | :? System.Collections.ArrayList as l 
              -> ArgValue.MultiValue (l.Cast<ValueObject>() |> Seq.map (fun vo -> vo.Value.ToString()) |> Seq.toList)
            | _
              -> ArgValue.Value (v.Value.ToString())
      | ShortOption o 
        ->
        match (v.IsTrue, v.IsFalse) with
        | true, false -> ArgValue.Present
        | false, true -> ArgValue.NotPresent
        | _, _ -> ArgValue.Value (v.Value.ToString())
      | _ -> failwithf "unhandled toValue %s %O" (option.GetType().Name) (option,v)

  let parsedResult = 
    result
    |> Seq.map (
      fun i -> 
      (
        let name = i.Key
        let o = toOption schemaDict.[name]
        let v = toValue o i.Value
        (o,v)
      )
    )
    |> dict
  parsedResult
  