
//1: Sum Types

open System

type Sum<'a, 'b> =
| Left of 'a
| Right of 'b


// 1.1

//Write two values of type Sum<int list, bool option>,
//one should be defined using Left and
//the other should be defined using Right.

let first = Left [1;2;3]

let second = Right (Some true)


//Create a function sumMap : ('a -> 'c) -> ('b -> 'c) -> Sum<'a, 'b> -> 'c that given functions f, g and the sum s
//returns f x when s = Left x and g y when s = Right y

let sumMap f g s =
    match s with
    | Left x -> f x
    | Right y -> g y
    
//1.2

type SumColl<'a, 'b> =
| Nil
| CLeft of 'a * SumColl<'a, 'b>
| CRight of 'b * SumColl<'a, 'b>

let value = CLeft ([true;false],CRight(3,Nil))

let rec ofList (lst: Sum<'a,'b> list) =
    match lst with
    | [] -> Nil
    | Left x :: xs -> CLeft(x,ofList xs)
    | Right y :: ys -> CRight(y, ofList ys)
    
//1.3

let reverse (coll: SumColl<'a,'b>) =
    let rec aux acc coll' =
        match coll' with
        | Nil -> acc
        | CLeft(value, next) -> aux (CLeft(value,acc)) next
        | CRight(value, next) -> aux (CRight(value,acc)) next
    aux Nil coll



let tailList (lst: Sum<'a,'b> list) =
   let rec aux acc lst' =
        match lst' with
        | [] -> acc
        | Left x :: xs -> CLeft(x,aux acc xs)
        | Right y :: ys -> CRight(y,aux acc ys)
   aux Nil lst

//1.4
let ofList2 lst =
    List.foldBack(fun elem acc ->
    match elem with
    | Left x -> CLeft(x,acc)
    | Right y-> CRight(y,acc)) lst Nil
    
//1.5

let coll = CLeft ("Hello", (CRight ([1; 2; 3], (CRight ([42], Nil)))))
let rec foldBackSumColl f g (coll: SumColl<'a,'b>) acc =
    match coll with
    | Nil -> acc
    | CLeft(elem, next) -> f elem (foldBackSumColl f g next acc)
    | CRight(elem, next) -> g elem (foldBackSumColl f g next acc)
    
//2: Code Comprehension

let f s =
    let l = String.length s
    let rec aux =
        function
        | i when i = l -> []
        | i -> s.[i] :: aux (i + 1)

    aux 0

let g s = 
    s |> f |>
    List.filter System.Char.IsLetter |>
    List.map System.Char.ToLower |>
    fun lst -> lst = List.rev lst

//2.1

(* Q: What are the types of functions f and g?
   A:
        f: string -> char list
        
        g: string -> bool
   
   Q: What do functions f and g do? Focus on what they do rather than how they do it.
   A:
        f: converts a string to a char list
        
        g: Checks if a string is a palindrome (eg. equal when spelled backwards).
   
   Q: What would be appropriate names for functions f and g?
   A: 
   
       f: toCharList
       
       g: isPalindrome
    
    *)

//2.2 Create a function f2 that behaves the same as f but which uses list comprehension.

let f2 (s: string) = List.ofSeq s

//2.3
//The function g uses the piping operator (|>) to join the building blocks of the function.
//With very minor changes the function can be rewritten to use function composition (>>) instead.

//Write a function g2 that behaves the same as g, with no uses of the |>-operator, but using the >>-operator instead.
let g2 = 
    f >>
    List.filter System.Char.IsLetter >> 
    List.map System.Char.ToLower >>
    fun lst -> lst = List.rev lst

//2.4

(*

let f s =
    let l = String.length s
    let rec aux =
        function
        | i when i = l -> []
        | i -> s.[i] :: aux (i + 1)

    aux 0
    
The function f is not tail-recursive. Explain why. 
To make a compelling argument you should evaluate a function call, similarly to what is done in Chapter 1.4 of HR, 
and reason about that evaluation. 
You need to make clear what aspects of the evaluation tell you that the function is tail recursive and you need to 
have at least one evaluation step per function call, recursive or otherwise.

The function f is not tail recursive because it needs to wait on the recursive calls to the aux function, 
before it can compute any values.

Lets call f with the string "Lol"

s.[0] :: aux(0 + 1)

s.[0] :: aux(0+1(s.[1] :: aux( 1 + 1)

s.[0] :: aux(0+1(s.[1] :: aux( 1 + 1(s.[2] :: aux( 2 + 1)

s.[0] :: aux(0+1(s.[1] :: aux( 1 + 1(s.[2] :: []

[L;o;l]







Create a tail-recursive version of f called fTail using continuations.
*)

(*let f s =
    let l = String.length s
    let rec aux =
        function
        | i when i = l -> []
        | i -> s.[i] :: aux (i + 1)

    aux 0
    *)

let fTail (s: string) =
    let l = String.length s
    let rec aux c counter =
        match counter with
        |i when i = l -> c []
        | i -> aux (fun result -> c(s.[i] :: result)) (i + 1)
    aux id 0
    



//2.5
let gOpt (s : string) =
        let rec aux i =
            function 
            | j when i >= j -> true
            | j when not (System.Char.IsLetter s.[i]) -> 
                 aux (i + 1) j
            | j when not (System.Char.IsLetter s.[j]) ->
                aux i (j - 1)
            | j when System.Char.ToLower s.[i] = System.Char.ToLower s.[j] ->
                aux (i + 1) (j - 1)
            | _ -> false
        aux 0 (String.length s - 1)
        

//3 : Golden ratio

let rec calculateGoldenRatio (n: int) =
    let fib x =
        let rec aux x' a b =
            match x' with
            | 0 -> a
            | n -> aux (n-1) b (a+b)
        aux x 1 1 |>  float
    (fib (n + 1)) /( fib(n) )


let grSeq = Seq.unfold(fun (a,b) -> Some(b/a, (b,a+b))) (1.0,1.0)

let goldenRectangleSeq (x: float) =
    Seq.map(fun ratio -> (ratio * x) * x) grSeq
let goldenTriangleSeq (x: float) =
    Seq.map(fun ratio ->
        let height = x * sqrt((ratio* ratio) - (1.0/4.0))
        (x * height) / 2.0
        ) grSeq 

let goldenRectangleTriangle (x: float) =
   seq {
    
        for ratio in grSeq do
            let height = x * sqrt((ratio* ratio) - (1.0/4.0))
            yield (((ratio * x) * x),(x * height) / 2.0)                                                          
    }





   


                              
    
    

    

