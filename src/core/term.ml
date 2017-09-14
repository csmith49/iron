open CCFun

(* terms are rose trees with a different constructor for leaves *)
type 'a t =
    | Leaf of 'a
    | Node of 'a * ('a t) list

(* sometimes there will be some accessing needed *)
let value : 'a t -> 'a = function
    | Leaf x -> x
    | Node (x, _) -> x

(* and we can wrap making so we choose the right constructor *)
let make (x : 'a) (xs : ('a t) list) : 'a t = match xs with
    | [] -> Leaf x
    | _ -> Node (x, xs)

(* terms are instances of functors *)
module Functor = struct
  let rec fmap (f : 'a -> 'b) : 'a t -> 'b t = function
    | Leaf x -> Leaf (f x)
    | Node (x, xs) -> Node (f x, CCList.map (fmap f) xs)
  let (<$>) = fmap
end

(* we will often want a very particular kind of constructor *)
let rec string_term_of_sexp : CCSexp.t -> string t = function
    | `Atom x -> Leaf x
    | `List ((`Atom x) :: []) -> Leaf x
    | `List ((`Atom x) :: xs) ->
        let subterms = CCList.map string_term_of_sexp xs in
            Node (x, subterms)
    | _ as s -> failwith ("invalid term: " ^ (CCSexp.to_string s))

(* to handle input/output, we parameterize expected functions by string conversion behavior *)
(* so that we can make a functor to wrap the behavior for us *)
module IO = functor (A : sig
    type t
    val to_string : t -> string
    val of_string : string -> t
end) -> struct
    (* we don't need anything fancy, just a way to see basic tree structure *)
    (* prints as a simple sexpr *)
    let rec to_string : A.t t -> string = function
        | Leaf x -> A.to_string x
        | Node (x, xs) ->
            let x' = A.to_string x in
            let xs' = CCList.map to_string xs in
                "(" ^ (CCString.concat " " (x' :: xs')) ^ ")"
    (* and so of course we can read in terms through sexps *)
    let rec of_string : string -> A.t t =
        function s -> s
            |> CCSexp.parse_string
            |> CCResult.to_opt
            |> CCOpt.get_exn
            |> string_term_of_sexp
            |> Functor.fmap A.of_string
end
