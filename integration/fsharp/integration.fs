module integration = 
  type Fx = double -> double

  let integrate( a : double)(b : double)(g : int)(f : Fx) : double = 
    let interval = (b - a) / (double) g
    let fxValues = seq { for n in 1 .. g-1 -> f(a + (double)n * interval) }
    let fxSum = Seq.fold (+) 0.0 fxValues
    (b - a) / (double) g * (f(a) / 2.0 + f(b) / 2.0 + fxSum)

module test1 = 

  let xSquared x : double = x * x
  open integration
  let area = integrate 0.0 10.0 1000 xSquared
  printfn "Area = %f" area

module test = 

  let xSquared x : double = x * x
  open integration
  let area = integrate 0.0 100.0 1000 xSquared
  printfn "Area = %f" area
