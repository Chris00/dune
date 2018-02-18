open Import

module Atom = Sexp.Atom

module Syntax = struct
  type t = OCaml | Reason
end

module File = struct
  type t =
    { name : Atom.t
    ; syntax : Syntax.t
    }

  let to_ocaml t =
    match t.syntax with
    | OCaml -> code_errorf "to_ocaml: can only convert reason Files" ()
    | Reason ->
      { syntax = OCaml
      ; name =
          let base, ext = Filename.split_extension (Atom.to_string t.name) in
          (base ^ ".re" ^
             (match Filename.extension (Atom.to_string t.name) with
              | ".re" -> ".ml"
              | ".rei" -> ".mli"
              | _ -> code_errorf "to_ocaml: unrecognized extension %s" ext ()))
          |> Atom.unsafe_of_string
      }
end

type t =
  { name     : Atom.t
  ; impl     : File.t option
  ; intf     : File.t option
  ; obj_name : string
  }

let name t = t.name

let real_unit_name t =
  String.capitalize_ascii (Filename.basename t.obj_name)

let has_impl t = Option.is_some t.impl

let file t ~dir (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> Path.relative dir (Atom.to_string f.name))

let obj_file t ~obj_dir ~ext = Path.relative obj_dir (t.obj_name ^ ext)

let cm_source t ~dir kind = file t ~dir (Cm_kind.source kind)

let cm_file_unsafe t ~obj_dir kind =
  obj_file t ~obj_dir ~ext:(Cm_kind.ext kind)

let cm_file t ~obj_dir (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  | _ -> Some (cm_file_unsafe t ~obj_dir kind)

let cmt_file t ~obj_dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Option.map t.impl ~f:(fun _ -> obj_file t ~obj_dir ~ext:".cmt" )
  | Intf -> Option.map t.intf ~f:(fun _ -> obj_file t ~obj_dir ~ext:".cmti")

let odoc_file t ~doc_dir = obj_file t ~obj_dir:doc_dir~ext:".odoc"

let cmti_file t ~obj_dir =
  match t.intf with
  | None   -> obj_file t ~obj_dir ~ext:".cmt"
  | Some _ -> obj_file t ~obj_dir ~ext:".cmti"

let iter t ~f =
  Option.iter t.impl ~f:(f Ml_kind.Impl);
  Option.iter t.intf ~f:(f Ml_kind.Intf)

let set_obj_name t ~wrapper =
  match wrapper with
  | Some s ->
     { t with obj_name = sprintf "%s__%s" (Atom.to_string s)
                           (Atom.to_string t.name) }
  | None ->
    let fn =
      match t.impl with
      | Some f -> f.name
      | None -> (Option.value_exn t.intf).name
    in
    let obj_name  =
      match String.index (Atom.to_string fn) '.' with
      | None -> fn
      | Some i -> Atom.sub fn ~pos:0 ~len:i
    in
    { t with obj_name = Atom.to_string obj_name }
