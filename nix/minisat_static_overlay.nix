self: super: {
  minisat = super.minisat.overrideAttrs (o: {
    patches = [ ./minisat_static.patch ./minisat_static_cmake.patch ];
  });
}
