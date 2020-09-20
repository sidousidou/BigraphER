#include "src/kissat.h"

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#define solver_val(v) (*((kissat**) Data_abstract_val(v)))


CAMLprim value ocaml_kissat_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(block);

  kissat *solver = kissat_init();

  block = caml_alloc_small(sizeof(kissat*), Abstract_tag);
  solver_val(block) = solver;

  CAMLreturn (block);
}

void ocaml_kissat_delete(value block) {
  CAMLparam1 (block);

  if (solver_val(block)==0) {
    goto exit;
  }

  kissat *solver = solver_val(block);
  kissat_release(solver);

 exit:
  CAMLreturn0;
}

CAMLprim value ocaml_kissat_solve(value block) {
  CAMLparam1 (block);

  kissat *solver = solver_val(block);

  CAMLreturn (Val_int (kissat_solve(solver)));
}

void ocaml_kissat_add(value block, value lit) {
  CAMLparam2(block, lit);

  kissat *solver = solver_val(block);
  kissat_add(solver, Int_val(lit));

  CAMLreturn0;
}

CAMLprim value ocaml_kissat_value(value block, value lit) {
  CAMLparam2(block, lit);

  kissat *solver = solver_val(block);

  CAMLreturn(Val_int(kissat_value(solver, Int_val(lit))));
}

CAMLprim value ocaml_kissat_get_option(value block, value name) {
  CAMLparam2(block, name);

  kissat *solver = solver_val(block);

  CAMLreturn(Val_int(kissat_get_option(solver, String_val(name))));
}

CAMLprim value ocaml_kissat_set_option(value block, value name, value v) {
  CAMLparam3(block, name, v);

  kissat *solver = solver_val(block);

  CAMLreturn(Val_int(kissat_set_option(solver, String_val(name), Int_val(v))));
}


void ocaml_kissat_print_statistics(value block) {
  CAMLparam1(block);

  kissat *solver = solver_val(block);
  kissat_print_statistics(solver);

  CAMLreturn0;
}
