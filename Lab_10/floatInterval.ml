open Intervals

module Float_comparable : (Comparable with type t = float) = struct
    type t = float
    let compare = compare
    let to_string = string_of_float
end

module Float_interval = Make_interval(Float_comparable)
