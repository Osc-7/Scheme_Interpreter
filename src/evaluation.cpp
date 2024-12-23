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

Value Let::eval(Assoc &env) {
  Assoc env1 = env;
  // 1. 对每个绑定进行评估，扩展环境
  for (auto &i : bind) {
    env1 = extend(i.first, i.second.get()->eval(env), env1);
  }
  // 2. 对 let 体进行求值
  return body.get()->eval(env1);
}
Value Letrec::eval(Assoc &env) {
  Assoc env1 = env;
  // 1. 先为所有绑定创建递归环境，所有绑定的值初始化为 NullV()
  for (auto &i : this->bind) {
    env1 = extend(i.first, NullV(), env1);
  }

  Assoc env2 = env;

  // 2. 评估每个绑定的右侧表达式，并将值存入环境
  for (auto &i : this->bind) {
    Value val = i.second.get()->eval(env1);
    if (val.get()->v_type == V_NULL)
      throw RuntimeError("Unusable variable");
    env2 = extend(i.first, val, env2);
  }

  // 3. 递归绑定的值修改
  for (auto &i : this->bind) {
    Value val = find(i.first, env2);
    modify(i.first, i.second.get()->eval(env2), env2);
  }

  // 4. 求值 Letrec 表达式的体
  return body.get()->eval(env2);
}

Value Lambda::eval(Assoc &env) {
  Assoc new_env = env;
  return ClosureV(x, e, new_env);
} // lambda expression

Value Apply::eval(Assoc &e) {
  Value rator = this->rator.get()->eval(e);

  auto clos = dynamic_cast<Closure *>(rator.get());
  if (clos) {
    if (clos->parameters.size() != this->rand.size()) {
      throw RuntimeError(" ");
    }

    Assoc env1 = Assoc(clos->env);
    // std::cout << clos->parameters[0] << std::endl;
    for (size_t i = 0; i < clos->parameters.size(); ++i) {
      env1 = extend(clos->parameters[i], this->rand[i].get()->eval(e), env1);
    }

    return clos->e.get()->eval(env1);
  }

  throw RuntimeError("fuction calling wrong");
  return clos->e.get()->eval(e);
} // for function calling

Value Var::eval(Assoc &e) {
  Value res = find(x, e);
  if (res.get())
    return res;
  else
    throw RuntimeError(" ");
} // evaluation of variable

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

  // 检查是否是 List
  auto list = dynamic_cast<List *>(s.get());
  if (list) {
    if (list->stxs.size() == 0) {
      return NullV(); // 空列表返回 NullV
    } else {
      size_t size = list->stxs.size();
      if (size == 3) { // 检查是否是形如 (a . b) 的列表
        auto isDot = dynamic_cast<Identifier *>(list->stxs[1].get());
        if (isDot && isDot->s == ".") {
          return PairV((Expr(new Quote(list->stxs[0]))).get()->eval(e),
                       (Expr(new Quote(list->stxs[2]))).get()->eval(e));
        }
      }

      if (size >= 3) {
        auto isDot = dynamic_cast<Identifier *>(list->stxs[size - 2].get());
        if (isDot && isDot->s == ".") {
          Value tail = (Expr(new Quote(list->stxs[size - 1]))).get()->eval(e);
          for (int i = size - 3; i >= 0; --i) {
            tail = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), tail);
          }
          return tail;
        }
      }
      // 处理普通列表 (a b c ...)
      Value res = NullV();
      for (int i = size - 1; i >= 0; --i) {
        res = PairV((Expr(new Quote(list->stxs[i]))).get()->eval(e), res);
      }
      return res;
    }
  }

  return NullV();
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

  if (dynamic_cast<Null *>(rand1.get()) &&
      (dynamic_cast<Null *>(rand2.get()))) {
    return BooleanV(true);
  }
  if (dynamic_cast<Void *>(rand1.get()) && dynamic_cast<Void *>(rand2.get())) {
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

Value IsProcedure::evalRator(const Value &rand) {
  auto ptr = dynamic_cast<Closure *>(rand.get());
  if (ptr)
    return BooleanV(true);
  else
    return BooleanV(false);
} // procedure?

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
