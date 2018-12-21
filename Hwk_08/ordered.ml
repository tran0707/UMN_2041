module type OrderedSig = sig
    type t
    val eq : t -> t -> bool
    val lt : t -> t -> bool
    val leq : t -> t -> bool
end

module Int : (OrderedSig with type t = int) = struct
    type t = int
    let eq t1 t2 = (t1 = t2)
    let lt t1 t2 = (t1 < t2)
    let leq t1 t2 = (t1 <= t2) 
end
