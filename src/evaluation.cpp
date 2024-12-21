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

Value Apply::eval(Assoc &e) {} // for function calling

Value Letrec::eval(Assoc &env) {} // letrec expression

Value Var::eval(Assoc &e) { throw RuntimeError(" "); } // evaluation of variable

Value Fixnum::eval(Assoc &e) { return IntegerV(n); } // evaluation of a fixnum

Value If::eval(Assoc &e) {
  Value res = cond->eval(e);
  // 先算出结果？
  auto bool_res = dynamic_cast<Boolean *>(res.get()); // 判断结果真假
  if (bool_res && bool_res->b == false)
    return alter->eval(e);
  else
    return conseq->eval(e);
}
// if expression

Value True::eval(Assoc &e) { return BooleanV(true); } // evaluation of #t

Value False::eval(Assoc &e) { return BooleanV(false); } // evaluation of #f

Value Begin::eval(Assoc &e) {
  if (es.size() == 0)
    return NullV();
  for (int i = 0; i < es.size(); i++) {
    es[i]->eval(e); // 直接调用对应版本的eval，如果有错就会报RE
  }

  return es[es.size() - 1]->eval(e);
} // begin expression

Value Quote::eval(Assoc &e) {
  // 检查是否是 FalseSyntax
  auto bool_f = dynamic_cast<FalseSyntax *>(s.get());
  if (bool_f)
    return BooleanV(false);

  // 检查是否是 TrueSyntax
  auto bool_t = dynamic_cast<TrueSyntax *>(s.get());
  if (bool_t)
    return BooleanV(true);

  // 检查是否是 Number
  auto num = dynamic_cast<Number *>(s.get());
  if (num)
    return IntegerV(num->n);

  // 检查是否是 Identifier
  auto iden = dynamic_cast<Identifier *>(s.get());
  if (iden) {
    return SymbolV(iden->s);
  }

  // 检查是否是 List
  auto list = dynamic_cast<List *>(s.get());
  if (list) {
    if (list->stxs.size() == 0) {
      return NullV(); // 空列表返回 NullV
    } else {
      size_t size = list->stxs.size();
      if (size == 3) {
        auto isDot = dynamic_cast<Identifier *>(list->stxs[1].get());
        if (isDot && isDot->s == ".") {
          return PairV((Expr(new Quote(list->stxs[0]))).get()->eval(e),
                       (Expr(new Quote(list->stxs[2]))).get()->eval(e));
        }
      }

      auto isDot = dynamic_cast<Identifier *>(list->stxs[size - 2].get());
      if (isDot && isDot->s == ".") {
        Value tail = (Expr(new Quote(list->stxs[size - 1]))).get()->eval(e);
        for (int i = size - 3; i >= 0; --i) {
          tail = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), tail);
        }
        return tail;
      }

      // 处理普通列表 (a b c ...)
      Value res = NullV();
      for (int i = size - 1; i >= 0; --i) {
        res = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), res);
        // std::cout << "Quote: " << res << std::endl;
      }
      return res;
    }
  }

  return NullV(); // 默认返回 NullV
}

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

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {
  if (rand1->v_type == V_INT and rand2->v_type == V_INT)
    return BooleanV((dynamic_cast<Integer *>(rand1.get())->n) ==
                    (dynamic_cast<Integer *>(rand2.get())->n));
  else if (rand1->v_type == V_BOOL and rand2->v_type == V_BOOL)
    return BooleanV((dynamic_cast<Boolean *>(rand1.get())->b) ==
                    (dynamic_cast<Boolean *>(rand2.get())->b));
  else if (rand1->v_type == V_SYM and rand2->v_type == V_SYM)
    return BooleanV((dynamic_cast<Symbol *>(rand1.get())->s) ==
                    (dynamic_cast<Symbol *>(rand2.get())->s));

  if ((dynamic_cast<Null *>(rand1.get()) ||
       dynamic_cast<Void *>(rand1.get())) &&
      (dynamic_cast<Null *>(rand2.get()) ||
       dynamic_cast<Void *>(rand2.get()))) {
    return BooleanV(true);
  }

  else {
    return BooleanV(rand1.get() == rand2.get());
  }
} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2) {
  return Value(new Pair(rand1, rand2));
} // cons

Value IsBoolean::evalRator(const Value &rand) {
  bool is_bool = dynamic_cast<Boolean *>(rand.get()) != nullptr;
  return BooleanV(is_bool);
} // boolean?

Value IsFixnum::evalRator(const Value &rand) {
  bool is_fixnum = dynamic_cast<Integer *>(rand.get()) != nullptr;
  return BooleanV(is_fixnum);
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

Value Not::evalRator(const Value &rand) {
  // auto bool_f = dynamic_cast<FalseSyntax *>(s.get());

  if (rand->v_type == V_BOOL and
      (dynamic_cast<Boolean *>(rand.get())->b == false)) {
    return BooleanV(true);
  } else {
    return BooleanV(false);
  }
} // not

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
