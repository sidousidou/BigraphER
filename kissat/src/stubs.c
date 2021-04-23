#include "src/kissat.h"

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define solver_val(v) (*((kissat **)Data_abstract_val(v)))

CAMLprim value ocaml_kissat_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  kissat *_solver = kissat_init();
  result = caml_alloc_small(sizeof(kissat *), Abstract_tag);
  solver_val(result) = _solver;

  CAMLreturn(result);
}

void ocaml_kissat_delete(value solver) {
  CAMLparam1(solver);

  if (solver_val(solver) == 0) {
    goto exit;
  }

  kissat *_solver = solver_val(solver);
  kissat_release(_solver);

exit:
  CAMLreturn0;
}

CAMLprim value ocaml_kissat_solve(value solver) {
  CAMLparam1(solver);
  CAMLlocal1(result);

  kissat *_solver = solver_val(solver);
  result = Val_int(kissat_solve(_solver));

  CAMLreturn(result);
}

void ocaml_kissat_add_clause_empty (value solver) {
  CAMLparam1(solver);

  kissat *_solver = solver_val(solver);
  kissat_add(_solver, Int_val(0));

  CAMLreturn0;
}

void ocaml_kissat_add_clause_unit (value solver, value l) {
  CAMLparam2(solver, l);

  kissat *_solver = solver_val(solver);
  kissat_add(_solver, Int_val(l));
  kissat_add(_solver, Int_val(0));

  CAMLreturn0;
}

void ocaml_kissat_add_clause_binary (value solver, value l0, value l1) {
  CAMLparam3(solver, l0, l1);

  kissat *_solver = solver_val(solver);
  kissat_add(_solver, Int_val(l0));
  kissat_add(_solver, Int_val(l1));
  kissat_add(_solver, Int_val(0));

  CAMLreturn0;
}

void ocaml_kissat_add_clause_ternary (value solver, value l0, value l1, value l2) {
  CAMLparam4(solver, l0, l1, l2);

  kissat *_solver = solver_val(solver);
  kissat_add(_solver, Int_val(l0));
  kissat_add(_solver, Int_val(l1));
  kissat_add(_solver, Int_val(l2));
  kissat_add(_solver, Int_val(0));

  CAMLreturn0;
}

void ocaml_kissat_add_clause_quaternary (value solver,value l0, value l1, value l2, value l3) {
  CAMLparam5(solver, l0, l1, l2, l3);

  kissat *_solver = solver_val(solver);
  kissat_add(_solver, Int_val(l0));
  kissat_add(_solver, Int_val(l1));
  kissat_add(_solver, Int_val(l2));
  kissat_add(_solver, Int_val(l3));
  kissat_add(_solver, Int_val(0));

  CAMLreturn0;
}

void ocaml_kissat_add_clause(value solver, value lits) {
  CAMLparam2(solver, lits);

  kissat *_solver = solver_val(solver);

  while (lits != Val_emptylist) {
    kissat_add(_solver, Int_val(Field(lits, 0)));
    lits = Field(lits, 1);
  }

  kissat_add(_solver, Int_val(0));

  CAMLreturn0;
}

CAMLprim value ocaml_kissat_value(value solver, value lit) {
  CAMLparam2(solver, lit);
  CAMLlocal1(result);

  kissat *_solver = solver_val(solver);
  result = Val_int(kissat_value(_solver, Int_val(lit)));

  CAMLreturn(result);
}

CAMLprim value ocaml_kissat_get_option(value solver, value name) {
  CAMLparam2(solver, name);
  CAMLlocal1(result);

  kissat *_solver = solver_val(solver);
  result = Val_int(kissat_get_option(_solver, String_val(name)));

  CAMLreturn(result);
}

CAMLprim value ocaml_kissat_set_option(value solver, value name, value v) {
  CAMLparam3(solver, name, v);
  CAMLlocal1(result);

  kissat *_solver = solver_val(solver);
  result = Val_int(kissat_set_option(_solver, String_val(name), Int_val(v)));

  CAMLreturn(result);
}

void ocaml_kissat_print_statistics(value solver) {
  CAMLparam1(solver);

  kissat *_solver = solver_val(solver);
  kissat_print_statistics(_solver);

  CAMLreturn0;
}
