(* of course, maneuvering will fail often *)
open CCOpt.Infix
open CCFun

(* so our zipper.t maintains current content and the rest of the term in branch form *)
type 'a t = ('a Term.t) * ('a Branch.t) list

(* basic zipper construction *)
let of_term (xt : 'a Term.t) : 'a t = (xt, [])

(* value-level getters and setters *)
let get : 'a t -> 'a = function
    (xt, _) -> Term.value xt
let set (x : 'a) : 'a t -> 'a t =
    CCPair.map1 (fun lt ->
        match lt with
            | Term.Leaf _ -> Term.Leaf x
            | Term.Node (_, xs) -> Term.Node (x, xs))

(* term-level getters and setters *)
let get_term : 'a t -> 'a Term.t = fst
let set_term (xt : 'a Term.t) : 'a t -> 'a t = CCPair.map1 (fun lt -> xt)

(* and now we can encode basic motion *)
let up : 'a t -> 'a t option = function
    | (xt, b :: bs) -> Some (Branch.collapse xt b, bs)
    | _ -> None
let left : 'a t -> 'a t option = function
    | (xt, b :: bs) -> begin match Branch.push_left xt b with
        | Some (yt, b') -> Some (yt, b' :: bs)
        | None -> None
    end
    | _ -> None
let right : 'a t -> 'a t option = function
    | (xt, b :: bs) -> begin match Branch.push_right xt b with
        | Some (yt, b') -> Some (yt, b' :: bs)
        | None -> None
    end
    | _ -> None
let down : 'a t -> 'a t option = function
    (xt, bs) -> match Branch.push_down xt with
        | None -> None
        | Some (yt, b) -> Some (yt, b :: bs)

(* and then fancy motion *)
let rec next (z : 'a t) : ('a t) option = (right z) <+> (up z) >>= next
let preorder (z : 'a t) : ('a t) option = (down z) <+> (next z)
let rec preorder_until (p : 'a -> bool) (z : 'a t) : ('a t) option =
    (CCOpt.if_ (p % get) z) <+> (preorder z) >>= (preorder_until p)
