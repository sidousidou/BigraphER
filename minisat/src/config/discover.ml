module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
      let default : C.Pkg_config.package_conf =
        {
          libs = [ "-lstdc++"; "-lminisat" ];
          cflags =
            [
              "-pedantic";
              "-fpermissive";
              "-fPIC";
              "-O3";
              "-std=c++11";
              "-Wno-reserved-user-defined-literal";
              "-Wno-zero-length-array";
            ];
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
