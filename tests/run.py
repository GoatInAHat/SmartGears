#!/usr/bin/env python3
"""Minimal AutoLISP subset interpreter to run SmartGears unit tests locally.

Supports the core functions used by math/geom code and current tests so agents
can iterate without AutoCAD. CAD-facing calls (entmake/command/tblsearch) are
stubbed to nil and only evaluated when invoked.
"""

import math
import os
import sys
from pathlib import Path

global_env = None
current_env = None


###############################################################################
# Reader (tokenize + parse into Python lists/atoms)
###############################################################################


def tokenize(src: str):
    tokens = []
    i = 0
    N = len(src)
    while i < N:
        ch = src[i]
        # Comments
        if ch == ";":
            while i < N and src[i] != "\n":
                i += 1
            continue
        # Strings
        if ch == '"':
            i += 1
            out = []
            while i < N:
                if src[i] == '"':
                    break
                if src[i] == "\\" and i + 1 < N:
                    i += 1
                    out.append(src[i])
                else:
                    out.append(src[i])
                i += 1
            tokens.append("\"" + "".join(out) + "\"")
            i += 1
            continue
        # Quote shorthand
        if ch == "'":
            tokens.append("'")
            i += 1
            continue
        # Parens
        if ch in "()":
            tokens.append(ch)
            i += 1
            continue
        # Whitespace
        if ch.isspace():
            i += 1
            continue
        # Symbols / numbers
        j = i
        while j < N and not src[j].isspace() and src[j] not in "()'":
            j += 1
        tokens.append(src[i:j])
        i = j
    return tokens


def parse(tokens):
    def parse_expr(idx):
        if idx >= len(tokens):
            raise ValueError("Unexpected EOF")
        tok = tokens[idx]
        if tok == "'":
            expr, next_idx = parse_expr(idx + 1)
            return ["quote", expr], next_idx
        if tok == "(":
            lst = []
            idx += 1
            while idx < len(tokens) and tokens[idx] != ")":
                expr, idx = parse_expr(idx)
                lst.append(expr)
            if idx >= len(tokens) or tokens[idx] != ")":
                raise ValueError("Unmatched (")
            return lst, idx + 1
        if tok == ")":
            raise ValueError("Unexpected )")
        # Atom
        atom = atom_from_token(tok)
        return atom, idx + 1

    def transform(obj):
        if isinstance(obj, list):
            if len(obj) == 3 and obj[1] == ".":
                return (transform(obj[0]), transform(obj[2]))
            return [transform(x) for x in obj]
        return obj

    exprs = []
    idx = 0
    while idx < len(tokens):
        expr, idx = parse_expr(idx)
        exprs.append(transform(expr))
    return exprs


class StrLiteral(str):
    pass


def atom_from_token(tok):
    if tok.startswith('"') and tok.endswith('"'):
        return StrLiteral(tok[1:-1])
    try:
        if "." in tok or "e" in tok or "E" in tok:
            return float(tok)
        return int(tok)
    except ValueError:
        return tok  # symbol


###############################################################################
# Interpreter core
###############################################################################


class Env(dict):
    def __init__(self, parent=None):
        super().__init__()
        self.parent = parent

    def find(self, name):
        if name in self:
            return self
        if self.parent:
            return self.parent.find(name)
        return None


def is_true(val):
    return val is not None and val is not False and val != []


def ensure_list(x):
    if x is None:
        return []
    if isinstance(x, list):
        return x
    return [x]


class Lambda:
    def __init__(self, params, body, env):
        self.params = params
        self.body = body
        self.env = env

    def __call__(self, args, eval_fn):
        call_env = Env(self.env)
        for p, a in zip(self.params, args):
            call_env[p] = a
        res = None
        for form in self.body:
            res = eval_fn(form, call_env)
        return res


###############################################################################
# Built-ins
###############################################################################


def fn_equal(*args):
    if len(args) == 2:
        a, b = args
        tol = None
    elif len(args) == 3:
        a, b, tol = args
    else:
        return None
    if tol is not None and all(isinstance(v, (int, float)) for v in (a, b)):
        return abs(a - b) <= tol
    return a == b


def fn_cons(a, d):
    if isinstance(d, list):
        return [a] + d
    elif d is None:
        return [a]
    else:
        return (a, d)  # dotted pair


def fn_car(x):
    if x is None:
        return None
    if isinstance(x, list):
        return x[0] if x else None
    if isinstance(x, tuple) and len(x) == 2:
        return x[0]
    return None


def fn_cdr(x):
    if x is None:
        return None
    if isinstance(x, list):
        return x[1:] if len(x) > 1 else None
    if isinstance(x, tuple) and len(x) == 2:
        return x[1]
    return None


def fn_last(lst):
    lst = ensure_list(lst)
    if not lst:
        return None
    return [lst[-1]]


def fn_assoc(key, alist):
    if alist is None:
        return None
    for item in alist:
        if isinstance(item, tuple) and len(item) == 2 and item[0] == key:
            return item
        if isinstance(item, list) and len(item) >= 2 and item[0] == key:
            if len(item) == 2:
                return (item[0], item[1])
            return (item[0], item[1:])
    return None


def fn_distance(p1, p2):
    p1 = ensure_list(p1)
    p2 = ensure_list(p2)
    n = min(len(p1), len(p2), 3)
    return math.sqrt(sum((p1[i] - p2[i]) ** 2 for i in range(n)))


def fn_angle(p1, p2):
    p1 = ensure_list(p1)
    p2 = ensure_list(p2)
    dx = p2[0] - p1[0]
    dy = p2[1] - p1[1]
    return math.atan2(dy, dx)


def fn_polar(pt, ang, dist):
    return [pt[0] + dist * math.cos(ang), pt[1] + dist * math.sin(ang)]


def fn_rem(a, b):
    return a % b


def fn_fix(x):
    return math.trunc(x)


def fn_ascii(ch):
    return ord(ch[0]) if ch else 0


def fn_substr(s, start, length=None):
    # start is 1-based
    i = max(0, int(start) - 1)
    if length is None:
        return s[i:]
    return s[i : i + int(length)]


def fn_strlen(s):
    return len(s)


def fn_strcase(s, flag=None):
    text = "" if s is None else str(s)
    return text.lower() if flag else text.upper()


def fn_rtos(val, mode=None, prec=None):
    try:
        p = 0 if prec is None else int(prec)
    except Exception:
        p = 6
    fmt = "{0:." + str(p) + "f}"
    return fmt.format(val)


def fn_logand(a, b):
    return int(a) & int(b)


def fn_logxor(a, b):
    return int(a) ^ int(b)


def fn_findfile(path):
    if Path(path).exists():
        return path
    return None


def fn_princ(*args):
    if args:
        print("".join(str(a) for a in args), end="")
    return None


def fn_mapcar(fn_val, *lists):
    if isinstance(fn_val, list) and fn_val and fn_val[0] == "lambda":
        fn_val = Lambda(fn_val[1], fn_val[2:], current_env)
    lists = [ensure_list(lst) for lst in lists]
    if not lists:
        return []
    length = min(len(lst) for lst in lists)
    out = []
    for i in range(length):
        args = [lst[i] for lst in lists]
        if isinstance(fn_val, Lambda):
            out.append(fn_val(args, eval_expr))
        elif callable(fn_val):
            out.append(fn_val(*args))
        else:
            raise TypeError("mapcar needs a function")
    return out


def fn_apply(fn_val, arg_list):
    if isinstance(fn_val, list) and fn_val and fn_val[0] == "lambda":
        fn_val = Lambda(fn_val[1], fn_val[2:], current_env)
    args = ensure_list(arg_list)
    if isinstance(fn_val, Lambda):
        return fn_val(args, eval_expr)
    if callable(fn_val):
        return fn_val(*args)
    raise TypeError("apply needs a function")


def fn_numberp(val):
    return True if isinstance(val, (int, float)) else None


def fn_stringp(val):
    return True if isinstance(val, str) else None


def fn_listp(val):
    return True if isinstance(val, list) else None


def _num(x):
    return 0 if x is None else x


BUILT_INS = {
    "+": lambda *args: sum(_num(a) for a in args),
    "-": lambda a, *rest: _num(a) - sum(_num(r) for r in rest) if rest else -_num(a),
    "*": lambda *args: math.prod(_num(a) for a in args) if args else 1,
    "/": lambda a, b: _num(a) / _num(b),
    "sin": math.sin,
    "cos": math.cos,
    "atan": lambda *args: math.atan2(args[0], args[1]) if len(args) == 2 else math.atan(args[0]),
    "acos": math.acos,
    "sqrt": math.sqrt,
    "abs": abs,
    "max": max,
    "min": min,
    "=": lambda a, b: a == b,
    "<": lambda a, b: a < b,
    ">": lambda a, b: a > b,
    "<=": lambda a, b: a <= b,
    ">=": lambda a, b: a >= b,
    "equal": fn_equal,
    "cons": fn_cons,
    "car": fn_car,
    "cdr": fn_cdr,
    "cadr": lambda x: fn_car(fn_cdr(x)),
    "caddr": lambda x: fn_car(fn_cdr(fn_cdr(x))),
    "list": lambda *args: list(args) if args else [],
    "append": lambda *args: sum((ensure_list(a) for a in args if a is not None), []),
    "length": lambda x: len(ensure_list(x)),
    "nth": lambda n, lst: ensure_list(lst)[int(n)] if len(ensure_list(lst)) > int(n) else None,
    "reverse": lambda lst: list(reversed(ensure_list(lst))),
    "last": fn_last,
    "assoc": fn_assoc,
    "distance": fn_distance,
    "angle": fn_angle,
    "polar": fn_polar,
    "rem": fn_rem,
    "fix": fn_fix,
    "float": float,
    "1+": lambda x: x + 1,
    "1-": lambda x: x - 1,
    "not": lambda x: None if is_true(x) else True,
    "null": lambda x: None if is_true(x) else True,
    "and": lambda *args: all(is_true(a) for a in args),
    "or": lambda *args: next((a for a in args if is_true(a)), None),
    "substr": fn_substr,
    "strlen": fn_strlen,
    "strcase": fn_strcase,
    "strcat": lambda *args: "".join(str(a) for a in args),
    "rtos": fn_rtos,
    "ascii": fn_ascii,
    "logand": fn_logand,
    "logxor": fn_logxor,
    "findfile": fn_findfile,
    "princ": fn_princ,
    "mapcar": fn_mapcar,
    "apply": fn_apply,
    "numberp": fn_numberp,
    "stringp": fn_stringp,
    "listp": fn_listp,
    # CAD stubs
    "entmake": lambda *_: None,
    "entmakex": lambda *_: None,
    "command": lambda *_: None,
    "tblsearch": lambda *_: None,
}


###############################################################################
# Evaluator
###############################################################################


def eval_expr(expr, env):
    global current_env
    current_env = env
    # Atom
    if isinstance(expr, (int, float, StrLiteral)):
        return expr

    if expr is None:
        return None

    if isinstance(expr, str):  # symbol
        if expr == "nil":
            return None
        if expr in ("T", "t"):
            return True
        if expr.startswith(":"):
            return expr
        found = env.find(expr)
        if found:
            return found[expr]
        raise NameError(f"Undefined symbol: {expr}")

    # List evaluation
    if not expr:
        return None
    op = expr[0]

    # Special forms
    if op == "quote":
        return expr[1] if len(expr) > 1 else None
    if op == "if":
        # (if test then [else])
        test = expr[1]
        conseq = expr[2] if len(expr) > 2 else None
        alt = expr[3] if len(expr) > 3 else None
        return eval_expr(conseq, env) if is_true(eval_expr(test, env)) else eval_expr(alt, env)
    if op == "cond":
        for clause in expr[1:]:
            if clause and is_true(eval_expr(clause[0], env)):
                res = None
                for part in clause[1:]:
                    res = eval_expr(part, env)
                return res
        return None
    if op == "progn":
        res = None
        for sub in expr[1:]:
            res = eval_expr(sub, env)
        return res
    if op == "setq":
        res = None
        for i in range(1, len(expr), 2):
            sym = expr[i]
            val = eval_expr(expr[i + 1], env)
            env[sym] = val
            res = val
        return res
    if op == "defun":
        _, name, params, *body = expr
        env[name] = Lambda(params, body, env)
        return name
    if op == "lambda":
        _, params, *body = expr
        return Lambda(params, body, env)
    if op == "while":
        _, test, *body = expr
        res = None
        while is_true(eval_expr(test, env)):
            for b in body:
                res = eval_expr(b, env)
        return res
    if op == "foreach":
        _, sym, lst, *body = expr
        res = None
        for item in ensure_list(eval_expr(lst, env)):
            env[sym] = item
            for b in body:
                res = eval_expr(b, env)
        return res
    if op == "and":
        for sub in expr[1:]:
            val = eval_expr(sub, env)
            if not is_true(val):
                return None
        return val if expr[1:] else True
    if op == "or":
        for sub in expr[1:]:
            val = eval_expr(sub, env)
            if is_true(val):
                return val
        return None
    if op == "load":
        path = eval_expr(expr[1], env)
        return load_file(path, env)

    # Function application
    fn_val = eval_expr(op, env)
    args = [eval_expr(a, env) for a in expr[1:]]

    # Built-in
    if callable(fn_val) and not isinstance(fn_val, Lambda):
        try:
            return fn_val(*args)
        except TypeError as e:
            raise TypeError(f"call {fn_val} with args {args} from expr {expr}") from e
    if isinstance(fn_val, Lambda):
        return fn_val(args, eval_expr)
    raise TypeError(f"{op} is not callable")


###############################################################################
# Loader & runner
###############################################################################


def load_file(path, env):
    with open(path, "r", encoding="utf-8") as f:
        src = f.read()
    tokens = tokenize(src)
    forms = parse(tokens)
    res = None
    for form in forms:
        res = eval_expr(form, env)
    return res


def collect_tests(env):
    return [name for name in env.keys() if isinstance(name, str) and name.startswith("sg-test-")]


def run_tests(env, test_names):
    failures = []
    for name in sorted(test_names):
        fn = env[name]
        try:
            res = fn([], eval_expr) if isinstance(fn, Lambda) else fn()
        except Exception as e:
            failures.append((name, f"exception {e}"))
            continue
        if res == "pass" or (isinstance(res, str) and res.lower() == "pass"):
            continue
        if isinstance(res, list):
            if res and res[0] == "fail":
                failures.append((name, " ".join(map(str, res[1:]))))
            elif all(is_true(v) for v in res):
                continue
            else:
                failures.append((name, f"unexpected result {res}"))
        elif res is True or res is None:
            continue
        else:
            failures.append((name, f"unexpected result {res}"))
    return failures


def main():
    root = Path(__file__).resolve().parents[1]
    env = Env()
    env.update(BUILT_INS)
    global global_env
    global_env = env

    # Load bundle (ensures single source of truth for functions).
    load_file(root / "SmartGears.lsp", env)

    # Load tests
    test_paths = []
    if len(sys.argv) > 1:
        test_paths = [Path(p) for p in sys.argv[1:]]
    else:
        test_paths = sorted(Path(root / "tests" / "unit").glob("*.lsp"))
    for path in test_paths:
        load_file(path, env)

    tests = collect_tests(env)
    failures = run_tests(env, tests)

    if failures:
        print("FAIL", len(failures))
        for name, reason in failures:
            print(f" - {name}: {reason}")
        sys.exit(1)
    else:
        print(f"PASS {len(tests)} tests")


if __name__ == "__main__":
    main()
