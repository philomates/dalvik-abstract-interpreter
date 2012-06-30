package com.android.demo.lambad;

// Compositional evaluator as visitor
// Based heavily on dvanhorn's gist: https://gist.github.com/1321466

import java.math.BigInteger;

// Syntax

interface Exp {
  <X> X accept(Visitor<X> v);
  String toString();
}

interface Visitor<X> {
  <X> X visit(String x);
  <X> X visit(BigInteger x);
  <X> X visit(String x, Exp e);
  <X> X visit(Exp l, Exp r);
}

class Var implements Exp {
  String x;
  Var(String x) {
    this.x = x;
  }
  public <X> X accept(Visitor<X> v) {
    return v.visit(this.x);
  }
  public String toString() {
    return this.x;
  }
}

class Int implements Exp {
  BigInteger x;
  Int(BigInteger x) {
    this.x = x;
  }
  public <X> X accept(Visitor<X> v) {
    return v.visit(this.x);
  }

  public String toString() {
    return "(const " + this.x.toString() + ")";
  }
}

class Lam implements Exp {
  String x;
  Exp e;
  Lam(String x, Exp e) {
    this.x = x;
    this.e = e;
  }
  public <X> X accept(Visitor<X> v) {
    return v.visit(this.x, this.e);
  }
  public String toString() {
    return "(lambda (" + this.x + ") " + this.e.toString() + ")";
  }
}

class App implements Exp {
  Exp left;
  Exp right;
  App(Exp left, Exp right) {
    this.left = left;
    this.right = right;
  }
  public <X> X accept(Visitor<X> v) {
    return v.visit(this.left, this.right);
  }
  public String toString() {
    return "(" + this.left.toString() + " " + this.right.toString() + ")";
  }
}


// Values

// A Value is a function or an integer
interface Value {
  Value apply(Value v);
  BigInteger getConst();
}

abstract class Function implements Value {
  public BigInteger getConst() {
    throw new Error("Not an integer");
  }
}

class Const implements Value {
  BigInteger i;
  Const(BigInteger i) {
    this.i = i;
  }
  public BigInteger getConst() { return this.i; }
  public Value apply(Value v) {
    throw new Error("Not a function");
  }
}

// Environments

abstract class Env {
  abstract public Value lookup(String x);

  public Env extend(String x, Value v) {
    return new Cons_Env(x, v, this);
  }
}

class MT_Env extends Env {
  public Value lookup(String x) {
    throw new Error("Unbound variable: " + x);
  }
}

class Cons_Env extends Env {
  String x;
  Value v;
  Env rest;
  Cons_Env(String x, Value v, Env rest) {
    this.x = x;
    this.v = v;
    this.rest = rest;
  }
  public Value lookup(String y) {
    return this.x.equals(y) ? this.v : rest.lookup(y);
  }
}

// Semantics

class Eval implements Visitor<Value> {
  Env env;
  Eval(Env env) {
    this.env = env;
  }

  public Value visit(BigInteger x) {
    return new Const(x);
  }

  public Value visit(String x) {
    return env.lookup(x);
  }

  public Value visit(final String x, final Exp e) {
    return new Function() {
      public Value apply(Value v) {
        return e.accept(new Eval(env.extend(x, v)));
      }
    };
  }

  public Value visit(Exp l, Exp r) {
    return l.accept(this).apply(r.accept(this));
  }
}

/**
// Split environment into compile-time and run-time lists
interface List<X> {
  int lookup(X x);
  X get(int i);
  List<X> cons(X x);
}

class MT<X> implements List<X> {
  public int lookup(X x) {
    throw new Error("Unbound: " + x.toString());
  }
  public X get(int i) {
    throw new Error("Invalid index");
  }
  public List<X> cons(X x) {
    return new Cons<X>(x, this);
  }
}

class Cons<X> implements List<X> {
  X first;
  List<X> rest;
  Cons(X first, List<X> rest) {
    this.first = first;
    this.rest = rest;
  }
  public int lookup(X x) {
    return x.equals(this.first) ? 0 : 1 + this.rest.lookup(x);
  }
  public X get(int i) {
    return i==0 ? this.first : this.rest.get(i-1);
  }
  public List<X> cons(X x) {
    return new Cons<X>(x, this);
  }
}

interface Computation {
  Value run(List<Value> renv);
}

class Compile implements Visitor<Computation> {
  List<String> env;
  Compile(List<String> env) {
    this.env = env;
  }

  public Computation visit(BigInteger x) {
    final BigInteger i = x;
    return new Computation() {
      public Value run(List<Value> renv) {
        return new Const(i);
      }
    };
  }

  public Computation visit(String x) {
    final int i = env.lookup(x);
    return new Computation() {
      public Value run(List<Value> renv) {
        return renv.get(i);
      }
    };
  }

  public Computation visit(String x, Exp e) {
    final Computation c = e.accept(new Compile(this.env.cons(x)));
    return new Computation() {
      public Value run(final List<Value> renv) {
        return new Function() {
          public Value apply(Value v) {
            return c.run(renv.cons(v));
          }
        };
      }
    };
  }

  public Computation visit(Exp l, Exp r) {
    final Computation cl = l.accept(this);
    final Computation cr = r.accept(this);
    return new Computation() {
      public Value run(List<Value> renv) {
        return cl.run(renv).apply(cr.run(renv));
      }
    };
  }
}
**/

// An example

/**
  public class Evaluator {
  public static void main(String [] args) {

  // lambda (f) . lambda (x) . (f (f x))
  Exp prog =
    new Lam("f",
      new Lam("x",
        new App(new Var("f"),
          new App(new Var("f"),
            new Var("x")))));

  Value twice = prog.accept(new Eval(new MT_Env()));

  Value add1 = new Function () {
    public Value apply(Value v) {
      return new Const(v.getConst().add(BigInteger.ONE));
    }
  };

  Value res =
  // Don't even *think* about applying twice another time.
  twice.apply(twice).apply(twice).apply(twice).apply(add1)
  .apply(new Const(BigInteger.ZERO));

  System.out.println(res.getConst()); // 65536

  Computation cprog =
  new App(new App(new App(prog,prog), prog), prog)
  .accept(new Compile(new MT<String>()));

  Value cres =
  cprog.run(new MT<Value>()).apply(add1)
  .apply(new Const(BigInteger.ZERO));

  System.out.println(cres.getConst()); // 65536

  }
  }
 **/
