let myAbs (x : float) =
    if x < 0 then
        -x
    else x

let rec iterFor f i a b = 
    if a <= b then
        f (iterFor f i (a+1) b) a
    else i

let rec iterWhile f acc   m i e  x  =
    let el = f i x e
    if  (myAbs el) >= m then
        iterWhile f (acc + el) m (i + 1) el x
    else acc


let power x = iterFor (fun acc _ -> acc*x) 1. 1

let eps = 0.000000000000001


let tailorPower a x =
    let log_a = log a
    iterWhile (fun  i x el -> (el * log_a * x / (float i)))  (1.) eps (1) (1) x

let f1 x = 0.6 * (tailorPower 3 x) - 2.3 * x - 3.

let f2 x  = (power x 2) - (log (1. + x)) - 3.

let f3 x = 2. * x * (sin x) - (cos x) 

let rec dichotomy f a b =
    let eps = 0.0001
    let c = (a + b) / 2. 
    let f1 : float = f a
    let f2 : float = f b
    let f3 : float = f c
    if ((a < 0.) <> (c < 0.)) && (myAbs (a - c) >= eps) then
        dichotomy f a c
    else if ((b < 0.) <> (c < 0.)) && (myAbs (b - c) >= eps) then
        dichotomy f b c
    else
        f3

    

// let iterations phi x0 = 0.

// let newthon f f' x0 = 0.
// // используйте функцию 'iterations'

// // Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
// let f1 = ...
// let f2 = ...
// let f3 = ...

// let f1' = ...
// let f2' = ...
// let f3' = ...

// let phi1 = ...
// let phi2 = ...
// let phi3 = ...

// let main = 
    printfn "%10.5f" (dichotomy f1 0. 1.) //(iterations phi1 0.) (newthon f1 f1' 1.)
    printfn "%10.5f" (dichotomy f2 0. 1.) //(iterations phi2 0.) (newthon f2 f2' 1.)
    printfn "%10.5f" (dichotomy f3 0. 1.) //(iterations phi3 0.) (newthon f3 f3' 1.)

 