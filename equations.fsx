let myAbs (x : float) =
    if x < 0 then
        -x
    else x

let rec iterFor f i a b = 
    if a <= b then
        f (iterFor f i (a+1) b) a
    else i

let rec iterWhile f acc   m i e ia x  =
    let el = f a i x e
    if  (myAbs el) >= m then
        iterWhile f (acc + el) m el a x
    else acc


let power x = iterFor (fun acc _ -> acc*x) 1. 1

let eps = 0.000000000000001


let tailorPower a x=
    let log_a = log a
    iterWhile (fun a x el -> (el * (2. * x / 5.)))  (1.)) eps (1.) x
let f1 (x : float) = 0.6 * (power x 3) - 2.3 * x - 3

let f1 x = 

let f1 x = 
let dichotomy f a b = 0.

let iterations phi x0 = 0.

let newthon f f' x0 = 0.
// используйте функцию 'iterations'

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 = ...
let f2 = ...
let f3 = ...

let f1' = ...
let f2' = ...
let f3' = ...

let phi1 = ...
let phi2 = ...
let phi3 = ...

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 0. 1.) (iterations phi1 0.) (newthon f1 f1' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 0. 1.) (iterations phi2 0.) (newthon f2 f2' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 0. 1.) (iterations phi3 0.) (newthon f3 f3' 1.)

 