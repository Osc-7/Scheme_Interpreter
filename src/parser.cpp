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
  // 1. 首先查找是否是已定义的变量
  Value res = find(s, env); // 查找变量名对应的值
  if (res.get()) {
    // 如果变量已经在环境中定义，则返回对应的表达式
    return Expr(new Var(s));
  }

  // 2. 如果是一个原始操作符（如 +、- 等），我们会将其转换为 lambda 表达式
  switch (primitives[s]) {
  case E_VOID:
  case E_EXIT: {
    // 对于 E_VOID 和 E_EXIT 操作符，创建一个 `lambda` 函数
    List *st = new List();
    st->stxs.push_back(
        Syntax(new Identifier("lambda"))); // 添加 `lambda` 标识符

    st->stxs.push_back(new List()); // 参数列表为空

    // 将操作符作为 lambda 的函数体
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    st->stxs.push_back(stx);

    // 返回 `lambda` 表达式的解析结果
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
    // 对于这些操作符（如乘法、加法等），我们需要构造一个 lambda 表达式
    List *args = new List();
    args->stxs.push_back(Syntax(new Identifier("x"))); // 参数 x
    args->stxs.push_back(Syntax(new Identifier("y"))); // 参数 y

    // 创建一个列表，表示操作符及其参数
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    stx->stxs.push_back(Syntax(new Identifier("x")));
    stx->stxs.push_back(Syntax(new Identifier("y")));

    // 构造 lambda 函数
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));
    st->stxs.push_back(args); // 添加参数列表
    st->stxs.push_back(stx);  // 添加操作符的表达式

    // 返回构造的 lambda 表达式
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
    // 对于一些布尔操作符和类型检查操作符，构造类似的 lambda 表达式
    List *args = new List();
    args->stxs.push_back(Syntax(new Identifier("x"))); // 参数 x

    // 创建一个列表，表示操作符及其参数
    List *stx = new List();
    stx->stxs.push_back(Syntax(new Identifier(s)));
    stx->stxs.push_back(Syntax(new Identifier("x")));

    // 构造 lambda 函数
    List *st = new List();
    st->stxs.push_back(Syntax(new Identifier("lambda")));
    st->stxs.push_back(args); // 添加参数列表
    st->stxs.push_back(stx);  // 添加操作符的表达式

    // 返回构造的 lambda 表达式
    return st->parse(env);
  }

  default:
    break;
  }

  // 3. 如果既不是变量、保留字，也不是原始操作符，返回一个普通的标识符表达式
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
    // 获取操作符字符串
    string op = id->s;

    // 查找操作符是否在基本操作符和保留字中
    auto prim = primitives.find(op);
    auto reserved = reserved_words.find(op);

    // 查找该变量在环境中的值
    Value res = find(op, env);
    if (res.get()) {
      // 如果环境中已找到变量，跳过以下处理逻辑
    } else {
      // 如果没有找到，开始进一步解析操作符

      // 如果操作符是基本操作符，并且没有操作数，则构造 Apply 表达式
      if (prim == primitives.end() && !stxs.size()) {
        vector<Expr> rands;
        return Expr(new Apply(Expr(new Var(op)), rands));
      } else if (prim != primitives.end()) {
        // 根据操作符类型执行不同的操作
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
            throw(RuntimeError("wrong parameter number"));
          } else
            return Expr(new IsProcedure(stxs[1]->parse(env)));
          break;
        default:
          throw(RuntimeError("what"));
        }
      }

      // 检查是否是保留字
      if (reserved != reserved_words.end()) {
        switch (reserved_words[op]) {
        case E_IF: {
          // 检查 if 表达式的参数数量是否正确
          if (stxs.size() != 4) {
            throw RuntimeError("Wrong parameter number for: " + op);
          }
          // 返回 If 表达式，解析条件、真分支和假分支
          return Expr(new If(stxs[1]->parse(env), stxs[2]->parse(env),
                             stxs[3]->parse(env)));
        }
        case E_BEGIN: {
          // 处理 begin 表达式：按顺序解析多个子表达式
          std::vector<Expr> next_exprs;
          for (size_t i = 1; i < stxs.size(); ++i) {
            next_exprs.push_back(stxs[i]->parse(env));
          }
          // 返回 Begin 表达式，包含所有子表达式
          return Expr(new Begin(next_exprs));
        }
        case E_QUOTE: {
          // 检查 quote 表达式的参数数量
          if (stxs.size() != 2) {
            throw RuntimeError("Wrong parameter number for: " + op);
          }
          // 返回 Quote 表达式，包含单一的参数
          return Expr(new Quote(stxs[1]));
        }
        case E_LAMBDA: {
          // 检查 lambda 表达式的参数数量
          if (stxs.size() != 3) {
            throw RuntimeError("Wrong parameter number for: " + op);
          }
          // 获取 lambda 参数列表
          auto args = (dynamic_cast<List *>(stxs[1].get()))->stxs;
          vector<string> t_args;

          // 构造新的环境
          Assoc env1 = env;

          // 为每个参数创建新的环境绑定
          for (auto &syn : args) {
            string s = dynamic_cast<Identifier *>(syn.get())->s;
            t_args.push_back(s);
            if (!find(s, env).get()) {
              env1 = extend(s, VoidV(), env1);
            }
          }

          // 返回 Lambda 表达式
          return Expr(new Lambda(t_args, stxs[2].parse(env1)));
        }
        case E_LET: {
          // 检查 let 表达式的参数数量
          if (stxs.size() != 3) {
            throw RuntimeError("Line " + std::to_string(__LINE__) +
                               " expect 2 argument(s), found " +
                               std::to_string(stxs.size() - 1));
          }
          // 获取 let 绑定部分
          auto header = (dynamic_cast<List *>(stxs[1].get()))->stxs;
          vector<std::pair<string, Expr>> bindings;

          Assoc env1 = env;

          // 解析每个绑定，并扩展环境
          for (auto &syn : header) {
            auto sync = (dynamic_cast<List *>(syn.get()))->stxs;
            if (sync.size() != 2) {
              throw RuntimeError("Line " + std::to_string(__LINE__) +
                                 ": Invalid binding in let");
            }
            string bind = (dynamic_cast<Identifier *>(sync[0].get()))->s;
            Expr parsed = sync[1].parse(env);
            bindings.push_back(std::make_pair(bind, parsed));
            env1 = extend(bind, ExpressionV(parsed), env1);
          }

          // 返回 Let 表达式
          return Expr(new Let(bindings, stxs[2].parse(env1)));
        }
        case E_LETREC: {
          // 检查 letrec 表达式的参数数量
          if (stxs.size() != 3) {
            throw RuntimeError("Line " + std::to_string(__LINE__) +
                               " expect 2 argument(s), found " +
                               std::to_string(stxs.size() - 1));
          }

          // 获取 letrec 绑定部分
          auto header = (dynamic_cast<List *>(stxs[1].get()))->stxs;
          vector<std::pair<string, Expr>> bindings;

          Assoc env1 = env;

          // 解析每个绑定，并扩展环境
          for (auto &syn : header) {
            auto sync = (dynamic_cast<List *>(syn.get()))->stxs;
            if (sync.size() != 2) {
              throw RuntimeError("Line " + std::to_string(__LINE__) +
                                 ": Invalid binding in letrec");
            }
            string bind = (dynamic_cast<Identifier *>(sync[0].get()))->s;
            Expr parsed = sync[1].parse(env);
            bindings.push_back(std::make_pair(bind, parsed));
            env1 = extend(bind, ExpressionV(parsed), env1);
          }

          // 返回 Letrec 表达式
          return Expr(new Letrec(bindings, stxs[2].parse(env1)));
        }
        default:
          // 如果是未支持的保留字，抛出异常
          throw RuntimeError("Unsupported operation: " + op);
        }
      }
    }
    // 如果不是保留字，则继续解析
    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i)
      rands.push_back(stxs[i].parse(env));

    // 返回 Apply 表达式，将操作符和所有操作数传递进去
    return Expr(new Apply(new Var(op), rands));
  }
  // 处理列表类型的操作符
  auto list = dynamic_cast<List *>(stxs[0].get());
  if (list) {
    vector<Expr> rands;
    for (size_t i = 1; i < stxs.size(); ++i) {
      Expr e = stxs[i].parse(env);
      rands.push_back(e);
    }
    // 返回 Apply 表达式，将列表的第一个元素作为函数，其余元素作为参数
    return Expr(new Apply(stxs[0].parse(env), rands));
  }

  // 如果都没有匹配到，抛出异常
  throw RuntimeError("Unsupported operation");
}

#endif