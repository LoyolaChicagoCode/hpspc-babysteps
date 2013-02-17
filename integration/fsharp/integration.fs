module sigcse

type Fx = double -> double

let xSquared x : double = x * x

let FxValues (a : double)(b : double)(g : int)(f : Fx) =
   let interval = (b - a) / (double) g
   seq { for n in 1 .. g-1 do yield f(a + (double)n * interval) }

let integrate( a : double)(b : double)(g : int)(f : Fx) : double = 
   let FxValues =
      let interval = (b - a) / (double) g
      seq { for n in 1 .. g-1 do yield f(a + (double)n * interval) }
   let fxSum = Seq.fold (+) 0.0 FxValues
   (b - a) / (double) g * (f(a) / 2.0 + f(b) / 2.0 + fxSum)


let area = integrate 0.0 10.0 1000 xSquared
printfn "Area = %f" area

