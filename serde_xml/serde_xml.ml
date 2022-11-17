open Serde

let ( let* ) = Result.bind

module Ser : Ser.Intf with type output = Tyxml.Xml.elt = Ser.Make (struct
  type output = Tyxml.Xml.elt
  type error = unit

  let initial_output () = Ok (Tyxml.Xml.empty ())

  let serialize_int _ser _output int =
    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "type" "int";
             Tyxml.Xml.string_attrib "value" (Int.to_string int);
           ]
         "prim" [])

  let serialize_bool _ser _output bool =
    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "type" "bool";
             Tyxml.Xml.string_attrib "value" (Bool.to_string bool);
           ]
         "prim" [])

  let serialize_unit _ser _output () =
    Ok (Tyxml.Xml.node ~a:[ Tyxml.Xml.string_attrib "type" "unit" ] "prim" [])

  let serialize_char _ser _output char =
    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "type" "char";
             Tyxml.Xml.string_attrib "value" (String.make 1 char);
           ]
         "prim" [])

  let serialize_float _ser _output float =
    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "type" "float";
             Tyxml.Xml.string_attrib "value" (Float.to_string float);
           ]
         "prim" [])

  let serialize_string _ser _output string =
    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "type" "string";
             Tyxml.Xml.string_attrib "value" string;
           ]
         "prim" [])

  let serialize_tuple
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok
      (Tyxml.Xml.node
         ~a:[ Tyxml.Xml.string_attrib "type" "tuple" ]
         "prim" parts)

  let serialize_unit_variant _ser _output ~type_name ~variant_name
      ~variant_index:_ =
    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "name" type_name;
             Tyxml.Xml.string_attrib "variant" variant_name;
             Tyxml.Xml.string_attrib "kind" "unit_variant";
           ]
         "variant" [])

  let serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index ~variant_name ~variant_size ~fields =
    let* fields = Ser.map fields in

    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "name" type_name;
             Tyxml.Xml.string_attrib "variant" variant_name;
             Tyxml.Xml.string_attrib "kind" "tuple_variant";
             Tyxml.Xml.string_attrib "idx" (Int.to_string variant_index);
             Tyxml.Xml.string_attrib "size" (Int.to_string variant_size);
           ]
         "variant" fields)

  let serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index ~variant_name ~variant_size ~fields =
    let* fields = Ser.map_field fields in

    let fields =
      List.map (fun (name, xml) -> Tyxml.Xml.node name [ xml ]) fields
    in

    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "name" type_name;
             Tyxml.Xml.string_attrib "variant" variant_name;
             Tyxml.Xml.string_attrib "kind" "record_variant";
             Tyxml.Xml.string_attrib "idx" (Int.to_string variant_index);
             Tyxml.Xml.string_attrib "size" (Int.to_string variant_size);
           ]
         "variant" fields)

  let serialize_record
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~record_size ~fields =
    let* fields = Ser.map_field fields in

    let fields =
      List.map (fun (name, xml) -> Tyxml.Xml.node name [ xml ]) fields
    in

    Ok
      (Tyxml.Xml.node
         ~a:
           [
             Tyxml.Xml.string_attrib "name" type_name;
             Tyxml.Xml.string_attrib "size" (Int.to_string record_size);
           ]
         "record" fields)
end)

let to_string_pretty fn t =
  let* t = fn t in
  let* xml = Serde.serialize (module Ser) t in
  Ok (Format.asprintf "%a" (Tyxml.Xml.pp ~indent:true ()) xml)
