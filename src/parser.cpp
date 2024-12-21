#ifndef PARSER
#define PARSER

// parser of myscheme

#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <cstring>
#include <iostream>
#include <map>
#define mp make_pair
using std ::pair;
using std ::string;
using std ::vector;

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

Expr Syntax ::parse(Assoc &env) {
  if (get() == nullptr)
    throw RuntimeError("unexpected EOF");
  return get()->parse(env);
}

Expr Number::parse(Assoc &env) { return Expr(new Fixnum(n)); }

Expr Identifier::parse(Assoc &env) {
  Value res = find(s, env);
  if (res.get()) {
    return Expr(new Var(s));
  }
  switch (primitives[s]) {
  case E_VOID:
  case E_EXIT: {
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));

    st->stxs.push_back(new List());

    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    st->stxs.push_back(stx);

    return st->parse(env);
  }

  case E_MUL:
  case E_MINUS:
  case E_PLUS:
  case E_LT:
  case E_LE:
  case E_EQ:
  case E_GE:
  case E_GT:
  case E_EQQ:
  case E_CONS: {
    List *args = new List();
    args->stxs.push_back(Syntax(new Identifier("x")));
    args->stxs.push_back(Syntax(new Identifier("y")));
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    stx->stxs.push_back(Syntax(new Identifier("x")));
    stx->stxs.push_back(Syntax(new Identifier("y")));
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));
    st->stxs.push_back(args);
    st->stxs.push_back(stx);

    return st->parse(env);
  }

  case E_BOOLQ:
  case E_INTQ:
  case E_NULLQ:
  case E_PAIRQ:
  case E_PROCQ:
  case E_SYMBOLQ:
  case E_NOT:
  case E_CAR:
  case E_CDR: {
    List *args = new List();
    args->stxs.push_back(Syntax(new Identifier("x")));
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    stx->stxs.push_back(Syntax(new Identifier("x")));
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));
    st->stxs.push_back(args);
    st->stxs.push_back(stx);

    return st->parse(env);
  }

  default:
    break;
  }

  return Expr(new Var(s));
}

Expr TrueSyntax::parse(Assoc &env) { return Expr(new True()); }

Expr FalseSyntax::parse(Assoc &env) { return Expr(new False()); }

Expr List::parse(Assoc &env) {
  if (stxs.empty()) {
    throw RuntimeError("Empty expression");
  }

  auto *id = dynamic_cast<Identifier *>(stxs[0].get());
  if (!id) {
    // 处理应用表达式
    Expr opexpr = stxs[0]->parse(env);
    vector<Expr> to_expr;
    for (size_t i = 1; i < stxs.size(); ++i) {
      to_expr.push_back(stxs[i]->parse(env));
    }
    return Expr(new Apply(opexpr, to_expr));
  }

  string op = id->s;
  // std::cout << "Here" << std::endl;

  // 检查是否是基本运算符
  if (primitives.count(op)) { // 这里应该一个一个地处理！ 12.16
    // std::cout << "Here" << std::endl;
    switch (primitives[op]) {
    case E_PLUS:
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number"));
      else
        return Expr(new Plus(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    case E_MINUS:
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number"));
      else
        return Expr(new Minus(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    case E_MUL:
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number"));
      else
        return Expr(new Mult(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    case E_EXIT:
      if (stxs.size() != 1) {
        throw(RuntimeError("wrong"));
      }
      return Expr(new Exit());
      break;

    case E_CONS:
      if (stxs.size() != 3) {
        throw RuntimeError("Wrong parameter number for: cons");
      }
      return Expr(new Cons(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    case E_CAR:
      if (stxs.size() != 2) {
        throw RuntimeError("Wrong parameter number for: car");
      }
      return Expr(new Car(stxs[1]->parse(env)));
      break;
    case E_CDR:
      if (stxs.size() != 2) {
        throw RuntimeError("Wrong parameter number for: cdr");
      }
      return Expr(new Cdr(stxs[1]->parse(env)));
      break;
    case E_VOID:
      if (stxs.size() != 1) {
        throw RuntimeError(" ");
      }
      return Expr(new MakeVoid());
      break;
    case E_LT: { // 小于
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number for: <"));
      return Expr(new Less(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    }
    case E_LE: { // 小于等于
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number for: <="));
      return Expr(new LessEq(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    }
    case E_EQ: { // 等于
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number for: =="));
      return Expr(new Equal(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    }
    case E_GE: { // 大于等于
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number for: >="));
      return Expr(new GreaterEq(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    }
    case E_GT: { // 大于
      if (stxs.size() != 3)
        throw(RuntimeError("wrong parameter number for: >"));
      return Expr(new Greater(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    }
    case E_EQQ:
      if (stxs.size() != 3)
        throw RuntimeError("");
      return Expr(new IsEq(stxs[1]->parse(env), stxs[2]->parse(env)));
      break;
    case E_NOT:
      if (stxs.size() != 2)
        throw RuntimeError("");
      return Expr(new Not(stxs[1]->parse(env)));
      break;
    case E_INTQ:
      if (stxs.size() == 2) {
        return Expr(new IsFixnum(stxs[1]->parse(env)));
      }
      throw(RuntimeError(" "));
      break;
    case E_BOOLQ:
      if (stxs.size() == 2) {
        return Expr(new IsBoolean(stxs[1]->parse(env)));
      }
      throw(RuntimeError(" "));
      break;
    case E_NULLQ:
      if (stxs.size() == 2) {
        return Expr(new IsNull(stxs[1]->parse(env)));
      }
      throw(RuntimeError(" "));
      break;
    case E_PAIRQ:
      if (stxs.size() == 2) {
        return Expr(new IsPair(stxs[1]->parse(env)));
      }
      throw(RuntimeError(" "));
      break;
    case E_SYMBOLQ:
      if (stxs.size() == 2) {
        return Expr(new IsSymbol(stxs[1]->parse(env)));
      }
      throw(RuntimeError(" "));
      break;
    case E_PROCQ:
      if (stxs.size() != 2) {
        // std::cout << "here" << std::endl;
        throw(RuntimeError("wrong parameter number"));
      } else
        return Expr(new IsProcedure(stxs[1]->parse(env)));
      break;
    default:
      throw(RuntimeError("what"));
    }
    // std::cout << "Here" << std::endl;
  }
  // 检查是否是保留字
  auto it = reserved_words.find(op);
  if (it == reserved_words.end()) {
    throw RuntimeError("Unknown reserved word: " + op);
  }

  // 获取对应的 ExprType
  ExprType op_type = it->second;

  switch (op_type) {
  case E_IF: {
    if (stxs.size() != 4) {
      throw RuntimeError("Wrong parameter number for: " + op);
    }
    return Expr(
        new If(stxs[1]->parse(env), stxs[2]->parse(env), stxs[3]->parse(env)));
  }
  case E_BEGIN: {
    std::vector<Expr> next_exprs;
    for (size_t i = 1; i < stxs.size(); ++i) {
      next_exprs.push_back(stxs[i]->parse(env));
    }
    return Expr(new Begin(next_exprs));
  }
  case E_QUOTE: {
    if (stxs.size() != 2) {
      throw RuntimeError("Wrong parameter number for: " + op);
    }
    // std::cout << "Here" << std::endl;

    return Expr(new Quote(stxs[1]));
  }
  case E_LAMBDA: {
    if (stxs.size() != 3) {
      throw RuntimeError("Wrong parameter number for: " + op);
    }
    // std::cout << "fucking here" << std::endl;
    auto args = (dynamic_cast<List *>(stxs[1].get()))->stxs;
    vector<string> transformedArgs;

    Assoc env1 = env;

    for (auto &syn : args) {
      string s = dynamic_cast<Identifier *>(syn.get())->s;
      transformedArgs.push_back(s);
      if (!find(s, env).get())
        env1 = extend(s, VoidV(), env1);
    }

    return Expr(new Lambda(transformedArgs, stxs[2].parse(env1)));
  }
  case E_LET: {
    // 检查参数数量是否正确
    if (stxs.size() != 3) {
      throw RuntimeError("Line " + std::to_string(__LINE__) +
                         " expect 2 argument(s), found " +
                         std::to_string(stxs.size() - 1));
    }

    // 存储绑定的变量名和值对
    std::vector<std::pair<std::string, Expr>> binded_vector;
    auto *binder_list_ptr = dynamic_cast<List *>(stxs[1].get());

    if (!binder_list_ptr) {
      throw RuntimeError("Line " + std::to_string(__LINE__) +
                         ": Invalid binding list for let");
    }

    // 遍历绑定列表中的每一项
    for (const auto &stx_tobind_raw : binder_list_ptr->stxs) {
      auto *stx_tobind = dynamic_cast<List *>(stx_tobind_raw.get());
      if (!stx_tobind || stx_tobind->stxs.size() != 2) {
        throw RuntimeError("Line " + std::to_string(__LINE__) +
                           ": Invalid binding in let: each binding must have "
                           "exactly two elements");
      }

      // 解析绑定的变量名
      auto *temp_id = dynamic_cast<Identifier *>(stx_tobind->stxs[0].get());
      if (!temp_id) {
        throw RuntimeError("Line " + std::to_string(__LINE__) +
                           ": Invalid variable name in let binding");
      }

      std::string var_name = temp_id->s;

      // 为绑定的变量名在环境中预留空间
      env = extend(var_name, NullV(), env);

      // 解析绑定的表达式
      Expr temp_store = stx_tobind->stxs[1]->parse(env);
      binded_vector.emplace_back(var_name, temp_store);
    }

    // 构造 Let 表达式并返回
    return Expr(new Let(binded_vector, stxs[2]->parse(env)));
  }

  case E_LETREC: {
    // 检查参数数量是否正确
    if (stxs.size() != 3) {
      // std::cout << "I supposed" << std::endl;
      throw RuntimeError("Line " + std::to_string(__LINE__) +
                         " expect 2 argument(s), found " +
                         std::to_string(stxs.size() - 1));
    }

    // 存储绑定的变量名和值对
    std::vector<std::pair<std::string, Expr>> binded_vector;
    auto *binder_list_ptr = dynamic_cast<List *>(stxs[1].get());

    if (!binder_list_ptr) {
      // std::cout << "????" << std::endl;
      throw RuntimeError("Line " + std::to_string(__LINE__) +
                         ": Invalid binding list for letrec");
    }

    // 遍历绑定列表中的每一项
    for (const auto &stx_tobind_raw : binder_list_ptr->stxs) {
      auto *stx_tobind = dynamic_cast<List *>(stx_tobind_raw.get());
      if (!stx_tobind || stx_tobind->stxs.size() != 2) {
        throw RuntimeError("Line " + std::to_string(__LINE__) +
                           ": Invalid binding in letrec: each binding must "
                           "have exactly two elements");
      }

      // 解析绑定的变量名
      auto *temp_id = dynamic_cast<Identifier *>(stx_tobind->stxs[0].get());
      if (!temp_id) {
        throw RuntimeError("Line " + std::to_string(__LINE__) +
                           ": Invalid variable name in letrec binding");
      }

      std::string var_name = temp_id->s;

      // 为绑定的变量名在环境中预留空间
      env = extend(var_name, NullV(), env);

      // 解析绑定的表达式
      Expr temp_store = stx_tobind->stxs[1]->parse(env);
      binded_vector.emplace_back(var_name, temp_store);
    }

    // 构造 Letrec 表达式并返回
    return Expr(new Letrec(binded_vector, stxs[2]->parse(env)));
  }

  default:
    throw RuntimeError("Unsupported operation: " + op);
  }
  vector<Expr> rands;
  for (size_t i = 1; i < stxs.size(); ++i)
    rands.push_back(stxs[i].parse(env));
  return Expr(new Apply(new Var(op), rands));

  auto list = dynamic_cast<List *>(stxs[0].get());
  if (list) {

    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i) {
      Expr e = stxs[i].parse(env);
      rands.push_back(e);
    }
    return Expr(new Apply(stxs[0].parse(env), rands));
  }
  // std::cout << "we end up here???" << std::endl;
  throw RuntimeError(" what ");
}

#endif