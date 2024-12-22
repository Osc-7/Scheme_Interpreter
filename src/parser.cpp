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
  // 首先查找是否是变量，如果是变量，则返回对应的 Expr
  Value res = find(s, env);
  if (res.get()) {
    return Expr(new Var(s));
  }

  /*
  // 检查是否是保留字
  auto it = reserved_words.find(s);
  if (it != reserved_words.end()) {
    // 如果是保留字，则根据 ExprType 进行不同的处理
    ExprType op_type = it->second;
  }
  */

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
    return Expr(new MakeVoid());
  }

  auto id = dynamic_cast<Identifier *>(stxs[0].get());
  if (id) {

    string op = id->s;
    auto prim = primitives.find(op);
    auto reserved = reserved_words.find(op);

    Value res = find(op, env);
    // std::cout << res.ptr << std::endl;
    if (res.get()) {
      // std::cout << "0" << std::endl; // 如果为真，跳过以下逻辑
    } else {
      // std::cout << "Here" << std::endl;

      // 检查是否是基本运算符
      if (prim == primitives.end() && !stxs.size()) {
        vector<Expr> rands;
        // std::cout << "1" << std::endl;

        return Expr(new Apply(Expr(new Var(op)), rands));
      } else if (prim != primitives.end()) {
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
            throw RuntimeError("void");
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
            throw RuntimeError("eqq");
          return Expr(new IsEq(stxs[1]->parse(env), stxs[2]->parse(env)));
          break;
        case E_NOT:
          if (stxs.size() != 2)
            throw RuntimeError("not");
          return Expr(new Not(stxs[1]->parse(env)));
          break;
        case E_INTQ:
          if (stxs.size() == 2) {
            return Expr(new IsFixnum(stxs[1]->parse(env)));
          }
          throw(RuntimeError("intq"));
          break;
        case E_BOOLQ:
          if (stxs.size() == 2) {
            return Expr(new IsBoolean(stxs[1]->parse(env)));
          }
          throw(RuntimeError("boolq"));
          break;
        case E_NULLQ:
          if (stxs.size() == 2) {
            return Expr(new IsNull(stxs[1]->parse(env)));
          }
          throw(RuntimeError("nullq"));
          break;
        case E_PAIRQ:
          if (stxs.size() == 2) {
            return Expr(new IsPair(stxs[1]->parse(env)));
          }
          throw(RuntimeError("pairq"));
          break;
        case E_SYMBOLQ:
          if (stxs.size() == 2) {
            return Expr(new IsSymbol(stxs[1]->parse(env)));
          }
          throw(RuntimeError("symbolq"));
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
      if (reserved != reserved_words.end()) {
        switch (reserved_words[op]) {
        case E_IF: {
          if (stxs.size() != 4) {
            throw RuntimeError("Wrong parameter number for: " + op);
          }
          return Expr(new If(stxs[1]->parse(env), stxs[2]->parse(env),
                             stxs[3]->parse(env)));
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
          // std::cout << "cnt" << std::endl;
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
          if (stxs.size() != 3) {
            throw RuntimeError("Line " + std::to_string(__LINE__) +
                               " expect 2 argument(s), found " +
                               std::to_string(stxs.size() - 1));
          }
          auto header = (dynamic_cast<List *>(stxs[1].get()))->stxs;
          vector<std::pair<string, Expr>> transformedHeader;

          Assoc env1 = env;

          for (auto &syn : header) {
            auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

            if (syn_v.size() != 2) {
              throw RuntimeError("Line " + std::to_string(__LINE__) +
                                 ": Invalid binding in letrec");
            }
            string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;
            Expr parsed = syn_v[1].parse(env);

            transformedHeader.push_back(std::make_pair(bind, parsed));
            env1 = extend(bind, ExpressionV(parsed), env1);
          }

          return Expr(new Let(transformedHeader, stxs[2].parse(env1)));
        }

        case E_LETREC: {
          if (stxs.size() != 3) {
            throw RuntimeError("Line " + std::to_string(__LINE__) +
                               " expect 2 argument(s), found " +
                               std::to_string(stxs.size() - 1));
          }

          auto header = (dynamic_cast<List *>(stxs[1].get()))->stxs;
          vector<std::pair<string, Expr>> transformedHeader;

          Assoc env1 = env;

          for (auto &syn : header) {
            auto syn_v = (dynamic_cast<List *>(syn.get()))->stxs;

            if (syn_v.size() != 2) {
              throw RuntimeError("Line " + std::to_string(__LINE__) +
                                 ": Invalid binding in letrec");
            }
            string bind = (dynamic_cast<Identifier *>(syn_v[0].get()))->s;
            Expr parsed = syn_v[1].parse(env);

            transformedHeader.push_back(std::make_pair(bind, parsed));
            env1 = extend(bind, ExpressionV(parsed), env1);
          }

          return Expr(new Letrec(transformedHeader, stxs[2].parse(env1)));
        }
        default:
          throw RuntimeError("Unsupported operation: " + op);
        }
      }
    }
    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i)
      rands.push_back(stxs[i].parse(env));
    // std::cout << "2" << std::endl;

    return Expr(new Apply(new Var(op), rands));
  }
  auto list = dynamic_cast<List *>(stxs[0].get());
  // std::cout << (bool)list << std::endl;
  if (list) {

    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i) {
      Expr e = stxs[i].parse(env);
      rands.push_back(e);
    }
    // std::cout << "3" << std::endl;
    return Expr(new Apply(stxs[0].parse(env), rands));
  }
  // std::cout << "we end up here???" << std::endl;
  throw RuntimeError(" what ");
}

#endif