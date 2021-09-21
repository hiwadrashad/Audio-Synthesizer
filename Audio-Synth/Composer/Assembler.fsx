module Assembler
#load "..\Audio\SampleGenerator.fsx"
#load "..\Parsers\ComposerParsing.fsx"
open SampleGenerator
open ComposerParsing

let TokenToSound token = generateSamples(durationFromToken token)(frequency token)
