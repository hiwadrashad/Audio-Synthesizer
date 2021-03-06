
module ComposerParsing
#r @"C:\Users\itv-admin\.nuget\packages\fparsec\1.1.1\lib\netstandard2.0\FParsec.dll"
#r @"C:\Users\itv-admin\.nuget\packages\fparsec\1.1.1\lib\netstandard2.0\FParsecCS.dll"
open FParsec

let TST p str = 
 match run p str with
 |Success(result, _,  _) -> printfn "Success: %A" result
 | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type MeasureFraction = Full | Half | Quarter | Eighth | Sixteenth | ThirthySeconth
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

let durationFromToken token = 
 let bpm = 120.
 let secondsPerBeat = 60./bpm
 (match token.length.fraction with
 | Full -> 4.*1000.*secondsPerBeat
 | Half -> 2.*1000.*secondsPerBeat
 | Quarter -> 1.*1000.*secondsPerBeat
 | Eighth -> 1./2.*1000.*secondsPerBeat
 | Sixteenth -> 1./4.*1000.*secondsPerBeat
 | ThirthySeconth -> (1./8.)*1000.*secondsPerBeat) *
 (if token.length.extended then 1.5 else 1.0)

let parse (score:string) =    
     match score.Trim() |> run parsescore with
         | Success(result, _, _)   -> Choice2Of2 result
         | Failure(errorMsg, _, _) -> Choice1Of2 errorMsg

let octaveNumeric = function 
 | One -> 1
 | Two -> 2
 | Three -> 3

let semitonesBetween lower upper = 
 let noteSquence = [A;ASharp;B;CSharp;D;DSharp;E;F;FSharp;G;GSharp]
 let overAllIndex (note,octave) =
  let noteIndex = List.findIndex(fun n -> n=note) noteSquence
  noteIndex + ((octaveNumeric octave - 1) * 12)
 (overAllIndex upper) - (overAllIndex lower)

let frequency {sound=sound} = 
 match sound with
 | Rest -> 0.
 | Tone (note,octave) -> 
  let gap = semitonesBetween (A,One) (note,octave)
  220. * ((2. ** (1./12.)) ** (float gap))