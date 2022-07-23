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
    

                              
    
    

    

