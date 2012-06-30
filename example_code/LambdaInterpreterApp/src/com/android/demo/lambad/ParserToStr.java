package com.android.demo.lambad;

import java.math.BigInteger;

// Parser used for debugging that returns a String instead of a Exp object.


// λ-calc ::= v
//         |  (const n)
//         |  (lambda (x) exp)
//         |  (exp1 exp2)

class ParserToStr {
  String toParse;
  public ParserToStr(String toParse) {
    this.toParse = toParse;
  }

  public String parse() { return this.parseConst(); }

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
  public String parseConst() {
    this.eatSpaces();
    if ("(const".equals(this.peekWord())) {
      this.eatWord("(const");
      this.eatSpaces();
      String tmp = this.eatTillClosingParen();
      BigInteger i = new BigInteger(tmp);
      return "(const " + i + ")";
    } else {
      return "";
    }
  }

  private String parseAbs() {
    if ("(lambda".equals(this.peekWord())) {
      this.eatWord("(lambda");
      this.eatSpaces();
      this.eat("(");
      String v = this.eatTillClosingParen();
      String body = this.parseConst();
      this.eat(")");
      return "(lambda ("+v+") " + body + ")";
    } else {
      return parseApp();
    }
  }

  private String parseApp() {
    if("(".equals(this.peek())) {
      this.eat("(");
      String exp1 = parseConst();
      String exp2 = parseConst();
      this.eat(")");
      return "(" + exp1 + " " + exp2 + ")";
    } else {
      return parseVar();
    }
  }

  private String parseVar() {
    String v = this.peekWord();
    this.eatWord(v);
    return "(var " + v + ")";
  }

  // Debugging helpers
  public void showRest() {
    System.out.println("Still to parse: " + this.toParse);
  }
}
