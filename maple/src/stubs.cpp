#define __STDC_LIMIT_MACROS
#define __STDC_FORMAT_MACROS

#include "core/Solver.h"
#include "utils/System.h"

extern "C" {
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

/* Declaring the functions which should be accessible on the C side. */
extern "C" {
CAMLprim value ocaml_maple_new(value unit);
void ocaml_maple_set_verbosity(value solver, value verb);
CAMLprim value ocaml_maple_new_var(value solver);
CAMLprim value ocaml_maple_pos_lit(value v);
CAMLprim value ocaml_maple_neg_lit(value v);
CAMLprim value ocaml_maple_negate(value v);
CAMLprim value ocaml_maple_add_clause(value solver, value c);
CAMLprim value ocaml_maple_simplify(value solver);
CAMLprim value ocaml_maple_solve(value solver);
CAMLprim value ocaml_maple_value_of(value solver, value v);
CAMLprim value ocaml_maple_n_vars(value solver);
CAMLprim value ocaml_maple_n_clauses(value solver);
CAMLprim value ocaml_maple_mem_used(value unit);
CAMLprim value ocaml_maple_cpu_time(value unit);
CAMLprim value ocaml_maple_solve_all_true(value solver, value vars);
}

#define solver_val(v) (*((Solver **)Data_custom_val(v)))

using namespace MapleLCMDiscChronoBT; /*CHECK*/

static inline void convert_literals(value l, vec<Lit> &r) {
  CAMLparam1(l);

  while (Int_val(l) != 0) {
    r.push(toLit(Int_val(Field(l, 0))));
    l = Field(l, 1);
  }

  CAMLreturn0;
}

static inline void fin_solver(value solver) {
  CAMLparam1(solver);

  Solver *_solver = solver_val(solver);
  delete _solver;

  CAMLreturn0;
}

static struct custom_operations maple_ops = {
    (char *)"ocaml_maple",     fin_solver,
    custom_compare_default,    custom_hash_default,
    custom_serialize_default,  custom_deserialize_default,
    custom_compare_ext_default};

CAMLprim value ocaml_maple_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  Solver *solver = new Solver();

  result = caml_alloc_custom(&maple_ops, sizeof(Solver *), 1024 * 1024,
                             50 * 1024 * 1024);
  solver_val(result) = solver;

  CAMLreturn(result);
}

void ocaml_maple_set_verbosity(value solver, value verb) {
  CAMLparam2(solver, verb);

  Solver *_solver = solver_val(solver);
  _solver->verbosity = Int_val(verb);

  CAMLreturn0;
}

CAMLprim value ocaml_maple_new_var(value solver) {
  CAMLparam1(solver);
  CAMLlocal1(result);

  Solver *_solver = solver_val(solver);
  result = Val_int(_solver->newVar());

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_pos_lit(value v) {
  CAMLparam1(v);
  CAMLlocal1(result);

  Lit lit = mkLit(Int_val(v), false);
  result = Val_int(toInt(lit));

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_neg_lit(value v) {
  CAMLparam1(v);
  CAMLlocal1(result);

  Lit lit = mkLit(Int_val(v), true);
  result = Val_int(toInt(lit));

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_negate(value l) {
  CAMLparam1(l);
  CAMLlocal1(result);

  Lit lit = toLit(Int_val(l));
  result = Val_int(toInt(~lit));

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_add_clause(value solver, value c) {
  CAMLparam2(solver, c);
  CAMLlocal1(result);

  Solver *_solver = solver_val(solver);
  vec<Lit> clause;
  convert_literals(c, clause);
  result = Val_bool(_solver->addClause_(clause));

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_simplify(value solver) {
  CAMLparam1(solver);
  CAMLlocal1(result);

  Solver *_solver = solver_val(solver);
  result = Val_bool(_solver->simplify());

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_solve(value solver) {
  CAMLparam1(solver);
  CAMLlocal1(result);

  Solver *_solver = solver_val(solver);
  if (_solver->solve()) {
    result = Val_int(0);
  } else {
    result = Val_int(1);
  }

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_value_of(value solver, value v) {
  CAMLparam2(solver, v);
  CAMLlocal1(result);

  Var var = Int_val(v);
  Solver *_solver = solver_val(solver);

  if (var >= _solver->model.size()) {
    caml_invalid_argument("index out of bounds");
  }
  lbool val = _solver->model[var];

  if (val == l_False) {
    result = Val_int(0);
  } else if (val == l_True) {
    result = Val_int(1);
  } else if (val == l_Undef) {
    result = Val_int(2);
  } else {
    assert(0);
  }

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_n_vars(value solver) {
  CAMLparam1(solver);
  CAMLlocal1(result);

  Solver *_solver = solver_val(solver);
  result = Val_int(_solver->nVars());

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_n_clauses(value solver) {
  CAMLparam1(solver);
  CAMLlocal1(result);

  Solver *_solver = solver_val(solver);
  result = Val_int(_solver->nClauses());

  CAMLreturn(result);
}

CAMLprim value ocaml_maple_mem_used(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(ml_f);

  double d;
  d = memUsedPeak();
  ml_f = caml_copy_double(d);

  CAMLreturn(ml_f);
}

CAMLprim value ocaml_maple_cpu_time(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(ml_f);

  double d;
  d = cpuTime();
  ml_f = caml_copy_double(d);

  CAMLreturn(ml_f);
}

static inline CAMLprim value tuple(value a, value b) {
  CAMLparam2(a, b);
  CAMLlocal1(tuple);

  tuple = caml_alloc(2, 0);

  Store_field(tuple, 0, a);
  Store_field(tuple, 1, b);

  CAMLreturn(tuple);
}

static inline CAMLprim value append(value hd, value tl) {
  CAMLparam2(hd, tl);
  CAMLlocal1(result);

  result = tuple(hd, tl);

  CAMLreturn(result);
}

CAMLprim value build_solution(Solver *_solver) {
  CAMLparam0();
  CAMLlocal1(result);

  result = Val_emptylist;

  for (int i = 0; i < _solver->nVars(); i++) {
    if (_solver->modelValue(i) == l_True) {
      result = append(Val_int(i), result);
    }
  }

  CAMLreturn(result);
}

// Only return true vars in each solution
CAMLprim value ocaml_maple_solve_all_true(value solver, value vars) {
  CAMLparam2(solver, vars);
  CAMLlocal3(x, result, block_sol);

  vec<Lit> blocking_clause;
  Solver *_solver = solver_val(solver);

  result = Val_emptylist;

  if (vars == Val_emptylist) {
    while (_solver->solve()) {
      blocking_clause.clear();
      for (int i = 0; i < _solver->nVars(); i++) {
        blocking_clause.push(mkLit(i, _solver->modelValue(i) == l_True));
      }
      _solver->addClause(blocking_clause);
      block_sol = build_solution(_solver);
      result = append(block_sol, result);
    }
  } else {
    while (_solver->solve()) {
      x = vars;
      blocking_clause.clear();
      while (x != Val_emptylist) {
        int i = Int_val(Field(x, 0));
        blocking_clause.push(mkLit(i, _solver->modelValue(i) == l_True));
        x = Field(x, 1);
      }
      _solver->addClause(blocking_clause);
      block_sol = build_solution(_solver);
      result = append(block_sol, result);
    }
  }

  CAMLreturn(result);
}
