module Assembler
#load "..\Parsers\ComposerParsing.fsx"
#load "..\Audio\SampleGenerator.fsx"
#load "..\Audio\Wave-Packer.fsx"
open SampleGenerator
open ComposerParsing
open WavePacker

let TokenToSound token = generateSamples(durationFromToken token)(frequency token)

let assemble tokens = 
 List.map TokenToSound tokens |> Seq.concat

let assembleToPackedStream(score:string) = 
 match parse score with
  | Choice1Of2 errorMsg -> Choice1Of2 errorMsg
  | Choice2Of2 tokens -> 
   assemble tokens 
   |> Array.ofSeq 
   |> pack 
   |> Choice2Of2