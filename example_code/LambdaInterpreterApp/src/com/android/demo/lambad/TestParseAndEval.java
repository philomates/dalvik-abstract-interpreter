/**
package com.android.demo.lambad;

import java.math.BigInteger;

public class TestParseAndEval {
  public static void main(String[] args) {
    // Parser tests
    testApp();
    testConst();
    testLambda();
    testBig();
    testIdentity();

    // Example Evaluation
    // (((lambda (f) (lambda (x) (f (f x)))) add1) (const 1))
    Parser p = new Parser("(((lambda (f) (lambda (x) (f (f x)))) add1) (const 1))");
    Exp parsedProg = p.parse();
    Exp hardcodedProg =
      new App(
        new App(
          new Lam("f",
            new Lam("x",
              new App(new Var("f"),
                new App(new Var("f"),
                  new Var("x"))))),
          new Var("add1")),
        new Int(BigInteger.ONE));

    // adder function to add to env
    Value add1 = new Function () {
      public Value apply(Value v) {
        return new Const(v.getConst().add(BigInteger.ONE));
      }
    };

    Env add1Env = new Cons_Env("add1", add1, new MT_Env());

    Value parsedOut = parsedProg.accept(new Eval(add1Env));
    Value hardcodedOut = hardcodedProg.accept(new Eval(add1Env));

    System.out.println("Program Evaluation Example:\n-----------------");
    System.out.println("parsedProg:\t\t" + parsedProg.toString());
    System.out.println("hardcodedProg:\t" + hardcodedProg.toString());
    System.out.println("Output of parsedProg:\t" + parsedOut.getConst());
    System.out.println("Output of hardcodedProg:\t" + hardcodedOut.getConst());
  }

  public static void printResults(String testName, Exp result, Exp expected) {
    System.out.println("\n" + testName + " Test:\n-----------------");
    System.out.println("expected string: " + expected.toString());
    System.out.println("result string: " + result.toString());
    System.out.println("are they the same?: " + (result.toString().equals(expected.toString())));
    System.out.println("");
  }

  public static void testLambda() {
    Parser p = new Parser("(lambda (x) (x x))");
    Exp result = p.parse();
    Exp expected = new Lam("x", new App(new Var("x"), new Var("x")));
    printResults("Self-Application", result, expected);
  }

  public static void testConst() {
    Parser p = new Parser("(const 1)");
    Exp result = p.parse();
    Exp expected = new Int(BigInteger.ONE);
    printResults("Constant", result, expected);
  }

  public static void testApp() {
    Parser p = new Parser("(f x)");
    Exp result = p.parse();
    Exp expected = new App(new Var("f"), new Var("x"));
    printResults("Application", result, expected);
  }

  public static void testIdentity() {
    Parser p = new Parser("(lambda (x) x)");
    Exp result = p.parse();
    Exp expected = new Lam("x", new Var("x"));
    printResults("Identity", result, expected);
  }

  public static void testBig() {
    Parser p = new Parser("(((lambda (f) (lambda (x) (f (f x)))) add1) (const 1))");
    Exp result = p.parse();
    Exp expected =
      new App(
        new App(
          new Lam("f",
            new Lam("x",
              new App(new Var("f"),
                new App(new Var("f"),
                  new Var("x"))))),
          new Var("add1")),
        new Int(BigInteger.ONE));
    printResults("Big", result, expected);
  }
}
**/
