// Print a table of a given function f, computed by taylor series

// function to compute
let f x = 1. / (2. * x - 5.)

let a = 0.0
let b = 2.0
let n = 10


let rec iterFor f i a b = 
    if a <= b then
        f (iterFor f i (a+1) b) a
    else i

let rec iterWhile f acc   m i x  =
    let el = f i x
    if el <= m then
        iterWhile f (acc + el) m (i+1) x
    else (acc + el, i)


let power x = iterFor (fun acc _ -> acc*x) 1. 1

let eps = -0.00000000000001


let taylor_naive x = 
    iterWhile  (fun i x -> (-1. / 5.) * (power (2.*x/5.) (i-1))) (0.) eps 1 x

// степень

// Define a function to do the same in a more efficient way
let taylor = f

let main =
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     let (naive_val, iter_naive) = taylor_naive x
     printfn "%5.2f  %10.6f  %10.6f %d   %10.6f" x (f x) (naive_val) (iter_naive) (taylor x)
// make sure to improve this table to include the required number of iterations
// for each of the methods

main