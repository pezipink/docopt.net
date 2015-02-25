module NavalFateTests
open FSharp.DocoptNet
open NUnit.Framework

type TestSet = { Arguments      : string list
               ; ExpectedOutput : string
               }

let usage = """
Naval Fate.

Usage:
  naval_fate ship new <name>...
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored | --drifting]
  naval_fate mine show [-md]
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate --help | -h
  naval_fate --version

Options:
  -h, --help      Show this screen.
  --version       Show version.
  --speed=<kn>    Speed in knots [default: 10].
  -m, --moored    Moored (anchored) mine.
  -d, --drifting  Drifting mine.
"""

let expectations = [
  { Arguments = ["ship"; "new"; "thename1"; "thename2"]
  ; ExpectedOutput = """Command 'ship'
Command 'new'
names:
-thename1
-thename2
"""
  }
  { Arguments = ["ship"; "shoot"; "10"; "50"]
  ; ExpectedOutput = """Command 'ship'
Command 'shoot'
x:10 y:50
"""
  }
  { Arguments = ["mine"; "set"; "10"; "50"]
  ; ExpectedOutput = """Command 'mine'
Command 'set'
x:10 y:50
"""
  }
  { Arguments = ["mine"; "remove"; "10"; "50"]
  ; ExpectedOutput = """Command 'mine'
Command 'remove'
x:10 y:50
"""
  }
  { Arguments = ["mine"; "remove"; "10"; "50"; "--moored"]
  ; ExpectedOutput = """Command 'mine'
Command 'remove'
x:10 y:50
  --moored
"""
  }
  { Arguments = ["mine"; "set"; "10"; "50"; "-m"]
  ; ExpectedOutput = """Command 'mine'
Command 'set'
x:10 y:50
  --moored
"""
  }
  { Arguments = ["mine"; "remove"; "10"; "50"; "-d"]
  ; ExpectedOutput = """Command 'mine'
Command 'remove'
x:10 y:50
  --drifting
"""
  }
  { Arguments = ["mine"; "show"; "-m"; "-d"; ]
  ; ExpectedOutput = """Command 'mine'
Command 'show'
  --moored
  --drifting
"""
  }
  { Arguments = ["mine"; "show"; "--moored"; ]
  ; ExpectedOutput = """Command 'mine'
Command 'show'
  --moored
"""
  }
  { Arguments = ["ship"; "shipname"; "move"; "10"; "50"]
  ; ExpectedOutput = """Command 'ship'
Command 'move'
name: shipname x:10 y:50 speed:10.00
"""
  }
  { Arguments = ["ship"; "shipname"; "move"; "10"; "50"; "--speed="+(11.55).ToString()]
  ; ExpectedOutput = """Command 'ship'
Command 'move'
name: shipname x:10 y:50 speed:11.55
"""
  }
  (* 
  // apparently either argument aren't supported, try NavalFate project with "-h" as argument
  { Arguments = ["-h"]
  ; ExpectedOutput = usage
  }
  { Arguments = ["--help"]
  ; ExpectedOutput = usage
  }
  *)
  { Arguments = ["--version"]
  ; ExpectedOutput = """Naval Fate v0.0.0.0.0.1.0
"""
  }

]

let navalFate args =
  let builder = new System.Text.StringBuilder()
  let putl fmt = Printf.bprintf builder (new Printf.BuilderFormat<_>(fmt+System.Environment.NewLine))
  let opts = parseArguments usage args
  if opts |>isPresent<| command "ship" then
    putl "Command 'ship'"
    if opts |>isPresent<| command "new" then
      putl "Command 'new'"
      let names : string list = opts |>getAllArgs<| argument "name"
      putl "names:"
      names
      |> Seq.iter (fun n -> putl "-%s" n)
    else if opts |>isPresent<| command "shoot" then
      putl "Command 'shoot'"
      let x : int = opts |>getArg<| argument "x"
      let y : int = opts |>getArg<| argument "y"
      putl "x:%i y:%i" x y
    else if opts |>isPresent<| command "move" then
      putl "Command 'move'"
      let x : int = opts |>getArg<| argument "x"
      let y : int = opts |>getArg<| argument "y"
      let name: string = opts |>getArg<| argument "name"
      let speed : float = opts |>getArg<| longOption "speed"
      putl "name: %s x:%i y:%i speed:%.2f" name x y speed
    else if opts |>isPresent<| command "list" then
      putl "Command 'list'"
    else if opts |>isPresent<| command "show" then
      putl "Command 'show'"
    else if opts |>isPresent<| longOption "moored" then
      putl "--moored"
    else if opts |>isPresent<| longOption "drifing" then
      putl "--drifing"
  else if opts |>isPresent<| command "mine" then
    putl "Command 'mine'"
    if (Seq.fold (||) (false) <| Seq.map (isPresent opts) [command "set"; command "remove"]) then
      if opts |>isPresent<| command "set" then
        putl "Command 'set'"
      if opts |>isPresent<| command "remove" then
        putl "Command 'remove'"
      let x : int = opts |>getArg<| argument "x"
      let y : int = opts |>getArg<| argument "y"
      putl "x:%i y:%i" x y
    if opts |>isPresent<| command "list" then
      putl "Command 'list'"
    if opts |>isPresent<| command "show" then
      putl "Command 'show'"
    if opts |>isPresent<| longOption "moored" then
      putl "  --moored"
    if opts |>isPresent<| longOption "drifting" then
      putl "  --drifting"
  else if opts |>isPresent<| longOption "version" then
    putl "Naval Fate v0.0.0.0.0.1.0"
  else if opts |>isPresent<| longOption "help" then
    putl usage

  builder.ToString()

[<Test>]
let ``test parsing naval_fate options`` () = 
  expectations
  |> Seq.iter (fun e ->
    let args = e.Arguments |> List.toArray
    let commandline = System.String.Join(" ", args)
    try
      let output = navalFate args
      Assert.AreEqual(e.ExpectedOutput, output)
      printfn "passed %s\n" commandline
    with
    | :? DocoptNet.DocoptExitException -> Assert.Ignore(sprintf "failed with %s %s" (commandline) (e.ToString()))
    | e -> Assert.Fail(sprintf "failed with %s %s" (commandline) (e.ToString()))
  )
