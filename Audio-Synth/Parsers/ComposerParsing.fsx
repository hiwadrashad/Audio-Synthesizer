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
 | 'e' -> E
 | unknown -> sprintf "Unknown note %c" unknown |> failwith)

let parsesharp = (stringReturn "#" true) <|> (stringReturn "" false)

let parsesharpnote = 
 pipe2
  parsesharp
  (anyOf "acdfg")
  (fun isSharp note ->
     match (isSharp,note) with
        (false, 'a') -> A
      | (true, 'a') -> ASharp
      | (false, 'c') -> C
      | (true, 'c') -> CSharp
      | (false, 'd') -> D
      | (true, 'd') -> DSharp
      | (false, 'f') -> F
      | (true, 'f') -> FSharp
      | (false, 'g') -> G
      | (true, 'g') -> GSharp
      | (_,unknown) -> sprintf "Unknown note %c" unknown |> failwith
      )
let parsenote = parsenotsharpablenote <|> parsesharpnote

let parseoctave = anyOf "123" |>> 
 (function
  | '1' -> One
  | '2' -> Two
  | '3' -> Three
  | unknown -> sprintf "unknown octave %c" unknown |> failwith)

let parsetone = pipe2 parsenote parseoctave (fun n o -> Tone(note = n, octave = o))

let parserest = stringReturn "-" Rest

let parsetoken = pipe2 parselength (parserest <|> parsetone) (fun l t -> {length = l; sound = t})

let parsescore = sepBy parsetoken (pstring " ")

TST parsemeasurefraction "2"
TST parsenote "#a"
TST parseoctave "2"
TST parsetone "#d3"
TST parsescore aspriation