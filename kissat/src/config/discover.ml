module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
      let default : C.Pkg_config.package_conf =
        {
          libs = [ "-Lkissat/src"; "-lkissat_c" ];
          cflags = [ "-fPIC"; "-pedantic"; "-O3"; "-std=c99"; "-Ikissat_c" ];
        }
      in
      let conf : C.Pkg_config.package_conf =
        match C.ocaml_config_var c "system" with
        | Some "freebsd" | Some "netbsd" | Some "openbsd" ->
            {
              libs = default.libs @ [ "-L/usr/local/lib" ];
              cflags = "-I/usr/local/include" :: default.cflags;
            }
        | Some _ | None -> default
      in
      C.Flags.write_sexp "libs.sexp" conf.libs;
      C.Flags.write_sexp "flags.sexp" conf.cflags)
