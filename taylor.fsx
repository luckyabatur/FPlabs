// Print a table of a given function f, computed by taylor series

// function to compute
let f x = 1. / (2. * x - 5.)

let a = 0.0
let b = 2.0
let n = 10


let rec iter f acc   m i =
    let el = f i
    if el >= m then
        iter f (acc + el) m (i+1)
    else acc + el
// Define a function to compute f using naive taylor series method


let taylor_naive x = iter  (fun i -> (-1 / 5)) (-1. / 5.) 0.001 1


// Define a function to do the same in a more efficient way
let taylor = f

let main =
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     printfn "%5.2f  %10.6f  %10.6f   %10.6f" x (f x) (taylor_naive x) (taylor x)
// make sure to improve this table to include the required number of iterations
// for each of the methods

main