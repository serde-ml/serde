type t =
  | Int of Int.t
  | Bool of bool
  | Float of float
  | String of string
  | Char of char
  | Tuple of { tup_size : int; tup_elements : t list }
  | Unit
  | Variant_unit of { vu_type : string; vu_name : string; vu_idx : int }
  | Variant_tuple of {
      vt_type : string;
      vt_name : string;
      vt_idx : int;
      vt_size : int;
      vt_fields : t list;
    }
  | Variant_record of {
      vr_type : string;
      vr_name : string;
      vr_idx : int;
      vr_size : int;
      vr_fields : (string * t) list;
    }
  | Record of {
      rec_type : string;
      rec_size : int;
      rec_fields : (string * t) list;
    }
  | Sequence of { seq_type : string; seq_elements : t list }
