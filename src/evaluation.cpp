#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <cstring>
#include <map>
#include <vector>

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

Value Let::eval(Assoc &env) {} // let expression

Value Lambda::eval(Assoc &env) {} // lambda expression

Value Apply::eval(Assoc &e) {
  // Assoc clos_env =
  Value mid_fun = rator->eval(e);
  Closure *clos_ptr = dynamic_cast<Closure *>(mid_fun.get());
  // Closure* clos_ptr = dynamic_cast<Closure*>(rator->eval(e).get());
  Assoc cur_env = clos_ptr->env;
  std::vector<std::pair<std::string, Value>> tobind;
  for (int i = 0; i < clos_ptr->parameters.size(); i++) {
    tobind.push_back({clos_ptr->parameters[i], rand[i]->eval(e)});
  }
  for (auto binded_pair : tobind) {
    // std::cout<<"!!binding "<<binded_pair.first<<" to ";
    // binded_pair.second->show(std::cout);
    // std::cout<<std::endl;
    cur_env = extend(binded_pair.first, binded_pair.second, cur_env);
  }
  return clos_ptr->e->eval(cur_env);
} // for function calling

Value Letrec::eval(Assoc &env) {} // letrec expression

Value Var::eval(Assoc &e) {} // evaluation of variable

Value Fixnum::eval(Assoc &e) { return IntegerV(n); } // evaluation of a fixnum

Value If::eval(Assoc &e) {} // if expression

Value True::eval(Assoc &e) { return BooleanV(true); } // evaluation of #t

Value False::eval(Assoc &e) { return BooleanV(false); } // evaluation of #f

Value Begin::eval(Assoc &e) {} // begin expression

Value Quote::eval(Assoc &e) {
  auto bool_f = dynamic_cast<FalseSyntax *>(s.get());
  if (bool_f)
    return BooleanV(false);

  auto bool_t = dynamic_cast<TrueSyntax *>(s.get());
  if (bool_t)
    return BooleanV(true);

  auto num = dynamic_cast<Number *>(s.get());
  if (num)
    return IntegerV(num->n);

  auto iden = dynamic_cast<Identifier *>(s.get());
  if (iden) {
    return SymbolV(iden->s);
  }

  auto list = dynamic_cast<List *>(s.get());
  if (list) {
    if (list->stxs.size() == 0) {
      return NullV();
    } else {
      size_t sz = list->stxs.size();
      if (sz == 3) {
        auto isDot = dynamic_cast<Identifier *>(list->stxs[1].get());
        if (isDot && isDot->s == ".") {
          return PairV((Expr(new Quote(list->stxs[0]))).get()->eval(e),
                       (Expr(new Quote(list->stxs[2]))).get()->eval(e));
        }
      }
      Value res = NullV();
      for (int i = sz - 1; i >= 0; --i)
        res = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), res);
      return res;
    }
  }

  return NullV();
} // quote expression

Value MakeVoid::eval(Assoc &e) { return VoidV(); } // (void)

Value Exit::eval(Assoc &e) { return TerminateV(); } // (exit)

Value Binary::eval(Assoc &e) {
  return evalRator(rand1->eval(e), rand2->eval(e));
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e) {
  return evalRator(rand->eval(e));

} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return IntegerV((dynamic_cast<Integer *>(rand1.get())->n) *
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError("Wrong Typename"));
} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return IntegerV((dynamic_cast<Integer *>(rand1.get())->n) +
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError("Wrong Typename"));
} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return IntegerV((dynamic_cast<Integer *>(rand1.get())->n) -
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError("Wrong Typename"));
} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return BooleanV((dynamic_cast<Integer *>(rand1.get())->n) <
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError(" "));
} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return BooleanV((dynamic_cast<Integer *>(rand1.get())->n) <=
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError(" "));
} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return BooleanV((dynamic_cast<Integer *>(rand1.get())->n) ==
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError(" "));
} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return BooleanV((dynamic_cast<Integer *>(rand1.get())->n) >=
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError(" "));
} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
    return BooleanV((dynamic_cast<Integer *>(rand1.get())->n) >
                    (dynamic_cast<Integer *>(rand2.get())->n));
  }
  throw(RuntimeError(" "));
} // >

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2) {
  return Value(new Pair(rand1, rand2));
} // cons

Value IsBoolean::evalRator(const Value &rand) {
  bool is_bool = dynamic_cast<Boolean *>(rand.get()) != nullptr;
  return BooleanV(is_bool);
} // boolean?

Value IsFixnum::evalRator(const Value &rand) {
  bool is_fixnum = dynamic_cast<Fixnum *>(rand.get()) != nullptr;
  return IntegerV(is_fixnum);
} // fixnum?

Value IsSymbol::evalRator(const Value &rand) {
  bool is_symbol = dynamic_cast<Symbol *>(rand.get()) != nullptr;
  return BooleanV(is_symbol);
} // symbol?

Value IsNull::evalRator(const Value &rand) {
  bool is_null = dynamic_cast<Null *>(rand.get()) != nullptr;
  return BooleanV(is_null);
} // null?

Value IsPair::evalRator(const Value &rand) {
  // 检查 rand 是否是 Pair 类型
  bool is_pair = dynamic_cast<Pair *>(rand.get()) != nullptr;
  // 返回布尔值
  return BooleanV(is_pair);
} // pair?

Value IsProcedure::evalRator(const Value &rand) {} // procedure?

Value Not::evalRator(const Value &rand) {} // not

Value Car::evalRator(const Value &rand) {
  // 检查 rand 是否是 Pair 类型
  auto *pair_obj = dynamic_cast<Pair *>(rand.get());
  if (!pair_obj) {
    throw RuntimeError("car expects a Pair");
  }
  // 返回 Pair 的 car 部分
  return pair_obj->car;
} // car

Value Cdr::evalRator(const Value &rand) {
  // 检查 rand 是否是 Pair 类型
  auto *pair_obj = dynamic_cast<Pair *>(rand.get());
  if (!pair_obj) {
    throw RuntimeError("cdr expects a Pair");
  }
  // 返回 Pair 的 cdr 部分
  return pair_obj->cdr;
} // cdr
