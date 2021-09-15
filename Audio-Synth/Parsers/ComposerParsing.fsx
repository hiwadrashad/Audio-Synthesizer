#r @"C:\Users\itv-admin\.nuget\packages\fparsec\1.1.1\lib\netstandard2.0\FParsec.dll"
#r @"C:\Users\itv-admin\.nuget\packages\fparsec\1.1.1\lib\netstandard2.0\FParsecCS.dll"

open FParsec

let TST p str = 
 match run p str with
 |Success(result, _,  _) -> printfn "Success: %A" result
 | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type MeasureFraction = Half | Quarter | Eighth | Sixteenth | ThirthySeconth
type Length = {fraction: MeasureFraction; extended: bool}
type Note = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp
type Octave = One | Two | Three
type Sound = Rest | Tone of note: Note * octave: Octave
type Token = {length: Length; sound: Sound}

let aspriation = "32.#d3"

let parsemeasurefraction = 
 (stringReturn "2" Half) <|> (stringReturn "4" Quarter) <|> (stringReturn "8" Eighth) <|> (stringReturn "16" Sixteenth) <|> (stringReturn "32" ThirthySeconth)

let parseextendedparser = (stringReturn "." true) <|> (stringReturn "" false)

let parselength = 
 pipe2
  parsemeasurefraction
  parseextendedparser
  (fun t e -> {fraction = t; extended = e})

let parsenotsharpablenote = anyOf "be" |>>(function
 | 'b' -> B
 | 'e' -> E)
 | unknown -> sprintf "Unknown note %c" unknown |> failwith

TST parselength aspriation