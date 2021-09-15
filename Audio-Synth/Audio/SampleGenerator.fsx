module SampleGenerator

let generateSamples milliseconds frequency = 
 let sampleRate = 44100.
 let sixteenBitsampleLimit = 32767.
 let VOLUME = 0.8
 let toAmplitude x = 
   x 
   |> (*) (2. * System.Math.PI * frequency / sampleRate) 
   |> sin
   |> (*) sixteenBitsampleLimit
   |> (*) VOLUME
   |> int16
 let numOfSamples = milliseconds / 1000. * sampleRate
 let requiredSamples = seq {1.0..numOfSamples}
 let result = Seq.map toAmplitude requiredSamples 
 result