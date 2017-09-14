(* note: left list is backwards *)
type 'a t = 'a * ('a Term.t) list * ('a Term.t) list

(* basic accessing *)
let value : 'a t -> 'a = function (x, _, _) -> x
let left : 'a t -> ('a Term.t) list = function (_, l, _) -> l
let right : 'a t -> ('a Term.t) list = function (_, _, r) -> r

(* we need to be able to plug terms back in *)
(* left list has to be reversed before being concatted *)
let collapse (xt : 'a Term.t) (b : 'a t) : 'a Term.t =
    let x = value b in
    let xs = CCList.rev_append (left b) (xt :: (right b)) in
        Term.make x xs

(* helpers for movement functions in Zipper *)
let push_left (xt : 'a Term.t) (b : 'a t) : ('a Term.t * 'a t) option = match b with
    | (yt, l :: ls, rs) ->
        let b' = (yt, ls, xt :: rs) in
            Some (l, b')
    | _ -> None
let push_right (xt : 'a Term.t) (b : 'a t) : ('a Term.t * 'a t) option = match b with
    | (yt, ls, r :: rs) ->
        let b' = (yt, xt :: ls, rs) in
            Some (r, b')
    | _ -> None

(* and we'll make branches from terms *)
let rec push_down (xt : 'a Term.t) : ('a Term.t * 'a t) option = match xt with
    | Term.Leaf _ -> None
    | Term.Node (_, []) -> None
    | Term.Node (x, y :: ys) ->
        let b' = (x, [], ys) in
            Some (y, b')
