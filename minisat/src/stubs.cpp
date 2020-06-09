#define __STDC_LIMIT_MACROS
#define __STDC_FORMAT_MACROS

#include <minisat/core/Solver.h>
#include <minisat/utils/System.h>


extern "C"
{
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
}

/* Declaring the functions which should be accessible on the C side. */
extern "C"
{
  CAMLprim value ocaml_minisat_new(value unit);
  void ocaml_minisat_set_verbosity(value solver, value verb);
  CAMLprim value ocaml_minisat_new_var(value solver);
  CAMLprim value ocaml_minisat_pos_lit(value v);
  CAMLprim value ocaml_minisat_neg_lit(value v);
  CAMLprim value ocaml_minisat_negate(value v);
  CAMLprim value ocaml_minisat_add_clause(value solver, value c);
  CAMLprim value ocaml_minisat_simplify(value solver);
  CAMLprim value ocaml_minisat_solve(value solver);
  CAMLprim value ocaml_minisat_value_of(value solver, value v);
  CAMLprim value ocaml_minisat_n_vars(value solver);
  CAMLprim value ocaml_minisat_n_clauses(value solver);
  CAMLprim value ocaml_minisat_mem_used(value unit);
  CAMLprim value ocaml_minisat_cpu_time(value unit);
}

#define solver_val(v) (*((Solver**) Data_custom_val(v)))

using namespace Minisat;

static inline void convert_literals(value l, vec<Lit> &r) {
  while(Int_val(l) != 0) {
    Lit lit = toLit(Int_val(Field(l, 0)));
    r.push(lit);
    l = Field(l, 1);
  }
}

static inline void fin_solver(value solver){
  Solver *_solver = solver_val(solver);
  delete _solver;
}

static struct custom_operations minisat_ops = {
  (char*)"ocaml_minisat",
  fin_solver,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

CAMLprim value ocaml_minisat_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  Solver *solver = new Solver();

  result = caml_alloc_custom(&minisat_ops, sizeof(Solver*), 1024*1024, 50*1024*1024);
  solver_val(result) = solver;

  CAMLreturn(result);
}

void ocaml_minisat_set_verbosity(value solver, value verb) {
  CAMLparam2 (solver,verb);

  Solver* _solver = solver_val(solver);
  _solver->verbosity = Int_val(verb);

  CAMLreturn0;
}

CAMLprim value ocaml_minisat_new_var(value solver) {
  CAMLparam1(solver);

  Solver* _solver = solver_val(solver);

  CAMLreturn(Val_int(_solver->newVar()));
}

CAMLprim value ocaml_minisat_pos_lit(value v) {
  CAMLparam1(v);

  Lit lit = mkLit(Int_val(v), false);

  CAMLreturn(Val_int(toInt(lit)));
}

CAMLprim value ocaml_minisat_neg_lit(value v) {
  CAMLparam1(v);

  Lit lit = mkLit(Int_val(v), true);

  CAMLreturn(Val_int(toInt(lit)));
}

CAMLprim value ocaml_minisat_negate(value l) {
  CAMLparam1(l);

  Lit lit = toLit(Int_val(l));

  CAMLreturn(Val_int(toInt(~lit)));
}

CAMLprim value ocaml_minisat_add_clause(value solver, value c) {
  CAMLparam2(solver, c);

  Solver* _solver = solver_val(solver);
  vec<Lit> clause;
  convert_literals(c, clause);

  CAMLreturn(Val_bool(_solver->addClause_(clause)));
}

CAMLprim value ocaml_minisat_simplify(value solver) {
  CAMLparam1 (solver);

  Solver* _solver = solver_val(solver);

  CAMLreturn (Val_bool(_solver->simplify()));
}

CAMLprim value ocaml_minisat_solve(value solver) {
  CAMLparam1 (solver);
  CAMLlocal1 (result);

  Solver* _solver = solver_val(solver);
  if(_solver->solve()) {
    result = Val_int(0);
  } else {
    result = Val_int(1);
  }

  CAMLreturn (result);
}

CAMLprim value ocaml_minisat_value_of(value solver, value v) {
  CAMLparam2 (solver,v);
  CAMLlocal1 (result);

  Var var = Int_val(v);
  Solver* _solver = solver_val(solver);

  if (var >= _solver->model.size()){
    caml_invalid_argument("index out of bounds");
  }
  lbool val = _solver->model[var];

  if(val == l_False) {
    result = Val_int(0);
  } else if(val == l_True) {
    result = Val_int(1);
  } else if (val == l_Undef) {
    result = Val_int(2);
  } else {
    assert(0);
  }

  CAMLreturn(result);
}

CAMLprim value ocaml_minisat_n_vars(value solver) {
  CAMLparam1 (solver);

  Solver* _solver = solver_val(solver);

  CAMLreturn(Val_int(_solver->nVars()));
}

CAMLprim value ocaml_minisat_n_clauses(value solver) {
  CAMLparam1 (solver);

  Solver* _solver = solver_val(solver);

  CAMLreturn(Val_int(_solver->nClauses()));
}

CAMLprim value ocaml_minisat_mem_used(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(ml_f);

  double d;
  d = memUsedPeak();
  ml_f = caml_copy_double(d);

  CAMLreturn(ml_f);
}

CAMLprim value ocaml_minisat_cpu_time(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(ml_f);

  double d;
  d = cpuTime();
  ml_f = caml_copy_double(d);

  CAMLreturn(ml_f);
}
