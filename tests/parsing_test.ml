module StringNode = struct
    type t = string
    let to_string = CCFun.id
    let of_string = CCFun.id
end

module StringTermIO = Term.IO(StringNode)

let test1 = "(x (y z a) b (c d e f g))"
let term1 = StringTermIO.of_string test1

let _ = print_endline (StringTermIO.to_string term1)
