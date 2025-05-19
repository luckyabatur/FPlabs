// Print a table of a given function f, computed by taylor series

// function to compute
let f x = 1. / (2. * x - 5.)

let a = 0.0
let b = 2.0
let n = 10

let myAbs (x : float) =
    if x < 0 then
        -x
    else x

let rec iterFor f i a b = 
    if a <= b then
        f (iterFor f i (a+1) b) a
    else i

let rec iterWhile f acc   m i e x  =
    let el = f i x e
    if  (myAbs el) >= m then
        iterWhile f (acc + el) m (i+1) el x
    else (acc, i)


let power x = iterFor (fun acc _ -> acc*x) 1. 1

let eps = 0.000000000000001
// let eps = -0.001


let taylor_naive x = 
    iterWhile  (fun i x _ -> (-1. / 5.) * (power (2.*x/5.) (i))) (-1. / 5.) eps 1 1 x

// степень

// Define a function to do the same in a more efficient way
let taylor x =
    iterWhile  (fun _ x el -> (el * (2. * x / 5.)))  (-1. / 5.) eps 1 (-1. / 5.) x

let main =
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     let (naive_val, naive_iter) = taylor_naive x
     let (smart_val, smart_iter) = taylor x
     printfn "%5.2f  %10.6f  %10.6f  %d  %10.6f  %d" x (f x) (naive_val) (naive_iter) (smart_val) (smart_iter)
// make sure to improve this table to include the required number of iterations
// for each of the methods

main