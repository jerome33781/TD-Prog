
type 'a my_list = |Nil |Cons of 'a * 'a my_list

let rec string_of_list f l =
  match l with
    | Nil -> ""
    | Cons(t,q) ->(f t)^(string_of_list f q)

let hd l =
  match l with
    | Nil -> None
    | Cons(t,q) -> Some t

let tl l =
  match l with
    | Nil -> None
    | Cons(t,q) -> Some q

let rec length l =
  match l with
    | Nil -> 0
    | Cons(t,q) -> 1+(length q)

let rec map f l =
  match l with
    | Nil -> Nil
    | Cons(t,q) -> Cons((f t),(map f q))

