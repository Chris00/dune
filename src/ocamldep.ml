open Import
open Build.O

module SC = Super_context
module Atom = Sexp.Atom
module Atom_map = Sexp.Atom_map

module Dep_graph = struct
  type t =
    { dir        : Path.t
    ; per_module : (unit, Module.t list) Build.t Atom_map.t
    }

  let deps_of t (m : Module.t) =
    match Atom_map.find m.name t.per_module with
    | Some x -> x
    | None ->
      let a = Atom.unsafe_of_string in
      Sexp.code_error "Ocamldep.Dep_graph.deps_of"
        [ a "dir", Path.sexp_of_t t.dir
        ; a "modules", Sexp.To_sexp.(list atom) (Atom_map.keys t.per_module)
        ; a "module", Atom m.name
        ]

  module Dep_closure =
    Top_closure.Make(String)(struct
      type t = Module.t
      type graph = t list Atom_map.t
      let key (t : t) = Atom.to_string t.name
      let deps (t: t) map = Option.value_exn (Atom_map.find t.name map)
    end)

  let top_closed t modules =
    Build.all
      (List.map (Atom_map.bindings t.per_module) ~f:(fun (unit, deps) ->
         deps >>^ fun deps -> (unit, deps)))
    >>^ fun per_module ->
    let per_module = Atom_map.of_alist_exn per_module in
    match Dep_closure.top_closure per_module modules with
    | Ok modules -> modules
    | Error cycle ->
      die "dependency cycle between modules in %s:\n   %s" (Path.to_string t.dir)
        (String.concat ~sep:"\n-> "
           (List.map cycle ~f:(fun m -> Atom.to_string(Module.name m))))

  let top_closed_implementations t modules =
    Build.memoize "top sorted implementations" (
      let filter_out_intf_only = List.filter ~f:Module.has_impl in
      top_closed t (filter_out_intf_only modules)
      >>^ filter_out_intf_only)

  let dummy (m : Module.t) =
    { dir = Path.root
    ; per_module = Atom_map.singleton m.name (Build.return [])
    }
end

module Dep_graphs = struct
  type t = Dep_graph.t Ml_kind.Dict.t

  let dummy m =
    Ml_kind.Dict.make_both (Dep_graph.dummy m)
end

let parse_deps ~dir ~file ~(unit : Module.t)
      ~modules ~alias_module ~lib_interface_module lines =
  let invalid () =
    die "ocamldep returned unexpected output for %s:\n\
         %s"
      (Path.to_string_maybe_quoted file)
      (String.concat ~sep:"\n"
         (List.map lines ~f:(sprintf "> %s")))
  in
  match lines with
  | [] | _ :: _ :: _ -> invalid ()
  | [line] ->
    match String.index line ':' with
    | None -> invalid ()
    | Some i ->
      let basename =
        String.sub line ~pos:0 ~len:i
        |> Filename.basename
      in
      if basename <> Path.basename file then invalid ();
      let deps =
        String.extract_blank_separated_words (String.sub line ~pos:(i + 1)
                                                ~len:(String.length line - (i + 1)))
        |> List.filter_map ~f:(fun m ->
          let m = Atom.of_string m in
          if m = unit.name then
            None
          else
            Atom_map.find m modules)
      in
      (match lib_interface_module with
       | None -> ()
       | Some (m : Module.t) ->
         let is_alias_module =
           match alias_module with
           | None -> false
           | Some (m : Module.t) -> unit.name = m.name
         in
         if unit.name <> m.name && not is_alias_module &&
            List.exists deps ~f:(fun x -> Module.name x = m.name) then
           die "Module %s in directory %s depends on %s.\n\
                This doesn't make sense to me.\n\
                \n\
                %s is the main module of the library and is the only module exposed \n\
                outside of the library. Consequently, it should be the one depending \n\
                on all the other modules in the library."
             (Atom.to_string unit.name) (Path.to_string dir)
             (Atom.to_string m.name) (Atom.to_string m.name));
      let deps =
        match alias_module with
        | None -> deps
        | Some m -> m :: deps
      in
      deps

let rules sctx ~(ml_kind:Ml_kind.t) ~dir ~modules ~alias_module ~lib_interface_module =
  let per_module =
    Atom_map.map modules ~f:(fun unit ->
      match Module.file ~dir unit ml_kind with
      | None -> Build.return []
      | Some file ->
        let ocamldep_output = Path.extend_basename file ~suffix:".d" in
        let context = SC.context sctx in
        SC.add_rule sctx
          (Build.run ~context (Ok context.ocamldep)
             [A "-modules"; Ml_kind.flag ml_kind; Dep file]
             ~stdout_to:ocamldep_output);
        Build.memoize (Path.to_string ocamldep_output)
          (Build.lines_of ocamldep_output
           >>^ parse_deps ~dir ~file ~unit ~modules ~alias_module ~lib_interface_module))
  in
  let per_module =
    match alias_module with
    | None -> per_module
    | Some m -> Atom_map.add per_module ~key:m.name ~data:(Build.return [])
  in
  { Dep_graph.
    dir
  ; per_module
  }

let rules sctx ~dir ~modules ~alias_module ~lib_interface_module =
  Ml_kind.Dict.of_func (rules sctx ~dir ~modules ~alias_module ~lib_interface_module)
