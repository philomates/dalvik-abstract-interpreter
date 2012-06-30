package com.android.demo.lambad;

import java.math.BigInteger;

// λ-calc ::= v
//         |  (const n)
//         |  (lambda (x) exp)
//         |  (exp1 exp2)

class Parser {
  String toParse;
  public Parser(String toParse) {
    this.toParse = toParse;
  }

  public Exp parse() { return this.parseConst(); }

  // Peeking & Eating
  private String peek() {
    return this.toParse.substring(0,1);
  }

  private String peekWord() {
    int index = Math.min(this.toParse.indexOf(' '), this.toParse.indexOf(')'));
    if (index == -1) {
      // beware of not finding ' ' or ')'
      index = Math.max(this.toParse.indexOf(' '), this.toParse.indexOf(')'));
      if (index == -1) {
        throw new Error ("unknown spacing/paren related error while parsing");
      }
    }
    return this.toParse.substring(0, index);
  }

  private void eatWord(String word) {
    if (word.equals(this.peekWord())) {
      this.toParse = this.toParse.substring(word.length());
    } else {
      throw new Error ("eating wrong word");
    }
  }

  private void eat(String c) {
    if (c.equals(this.peek())) {
      toParse = toParse.substring(1);
    } else {
      throw new Error ("eating wrong char");
    }
  }

  // return everything before ')' and eat everything including ')'
  private String eatTillClosingParen() {
    // grab string before closing paren
    String v = this.toParse.substring(0, toParse.indexOf(')'));
    // get rid of closing paren
    this.toParse = this.toParse.substring(toParse.indexOf(')') + 1);
    return v;
  }

  private void eatSpaces() {
    while(" ".equals(this.peek())) { this.eat(" "); }
  }

  // Recursive Parsing
  public Exp parseConst() {
    this.eatSpaces();
    if ("(const".equals(this.peekWord())) {
      this.eatWord("(const");
      this.eatSpaces();
      String tmp = this.eatTillClosingParen();
      BigInteger i = new BigInteger(tmp);
      return new Int(i);
    } else {
      return this.parseAbs();
    }
  }

  private Exp parseAbs() {
    if ("(lambda".equals(this.peekWord())) {
      this.eatWord("(lambda");
      this.eatSpaces();
      this.eat("(");
      String v = this.eatTillClosingParen();
      Exp body = this.parseConst();
      this.eat(")");
      return new Lam(v, body);
    } else {
      return parseApp();
    }
  }

  private Exp parseApp() {
    if("(".equals(this.peek())) {
      this.eat("(");
      Exp exp1 = parseConst();
      Exp exp2 = parseConst();
      this.eat(")");
      return new App(exp1, exp2);
    } else {
      return parseVar();
    }
  }

  private Exp parseVar() {
    String v = this.peekWord();
    this.eatWord(v);
    return new Var(v);
  }

  // Debugging helpers
  public void showRest() {
    System.out.println("Still to parse: " + this.toParse);
  }
}
