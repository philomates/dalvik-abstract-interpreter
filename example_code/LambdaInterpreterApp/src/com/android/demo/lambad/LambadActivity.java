package com.android.demo.lambad;

import android.app.Activity;
import android.database.Cursor;
import android.os.Bundle;
import android.provider.Browser;

import android.view.View;
import android.widget.EditText;
import android.widget.RadioButton;
import android.widget.TextView;
import android.widget.Toast;

import android.os.StrictMode;

import java.math.BigInteger;
import java.net.URLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.io.InputStream;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.BufferedInputStream;
import java.io.InputStreamReader;


public class LambadActivity extends Activity {
  private EditText text;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.main);
    text = (EditText) findViewById(R.id.editText1);

    // XXX: Thread hack to make network IO happen on main thread
    StrictMode.ThreadPolicy policy = new StrictMode.ThreadPolicy.Builder().permitAll().build();
    StrictMode.setThreadPolicy(policy);
  }

  // This method is called at button click because we assigned the name to the
  // "On Click property" of the button
  public void myClickHandler(View view) {
    switch (view.getId()) {
      case R.id.button1:
        if (text.getText().length() == 0) {
          Toast.makeText(this, "Please enter a valid expression", Toast.LENGTH_LONG).show();
          return;
        }

        TextView tv = (TextView) findViewById(R.id.demoShow);
        tv.setText(evalSomeLambdaExpr(text.getText().toString()));
        break;
    }
  }

  public static String readStream(InputStream in) {
    BufferedReader reader = new BufferedReader(new InputStreamReader(in));
    String result = "";
    try {
      String line = reader.readLine();
      result = line;
      while((line=reader.readLine())!=null){
          result+=line;
      }
    } catch (Exception e) {
      result = "";
    }
    return result;
  }

  public static Value buildHTTPRandomNumberGen() {
    final URL url;
    final Value errorReturn = new Const(new BigInteger("-1"));

    try {
      url = new URL("http://www.random.org/integers/?num=1&min=1&max=42&col=1&base=10&format=plain&rnd=new");
    } catch (MalformedURLException e) {
      return new Function () {
        public Value apply(Value v) { return errorReturn; }
      };
    }

    Value func = new Function() {
      public Value apply(Value v) {
        Value result;
        InputStream in;
        // open url connection
        try {
          URLConnection urlConnection = url.openConnection();
          in = new BufferedInputStream(urlConnection.getInputStream());
        } catch (IOException e) { return errorReturn; }

        try {
          // read number from request result and create BigInteger
          result = (new Const(new BigInteger(readStream(in))));
          in.close();
        } catch (Exception e) {
          result = errorReturn;
        }
        return result;
      }
    };

    return func;
  }

  public static String evalSomeLambdaExpr(String exprString) {
    Parser p = new Parser(exprString);

    Exp parsedProg;
    try {
      parsedProg = p.parse();
    } catch (Exception e) {
      return ("Error parsing " + exprString);
    }

    //adder function to add to env
    //Value add1 = new Function () {
      //public Value apply(Value v) {
        //return new Const(v.getConst().add(BigInteger.ONE));
      //}
    //};

    Env randomGenEnv = new Cons_Env("rand", buildHTTPRandomNumberGen(), new MT_Env());
    //Env add1Env = new Cons_Env("add1", add1, randomGenEnv);

    try {
      Value parsedOut = parsedProg.accept(new Eval(randomGenEnv));
      return parsedOut.getConst().toString();
    } catch (Exception e) {
      return ("Error evaluating " + parsedProg.toString());
    }
  }

  public static String fixedEval() {
    String exprString = "(rand (const 1))";
    Parser p = new Parser(exprString);

    Exp parsedProg;
    try {
      parsedProg = p.parseConst();
    } catch (Exception e) {
      return ("Error parsing " + exprString);
    }
    return parsedProg.toString();

    //adder function to add to env
    //Value add1 = new Function () {
      //public Value apply(Value v) {
        //return new Const(v.getConst().add(BigInteger.ONE));
      //}
    //};

    //Env randomGenEnv = new Cons_Env("rand", buildHTTPRandomNumberGen(), new MT_Env());
    //Env add1Env = new Cons_Env("add1", add1, randomGenEnv);

    //try {
      //Value parsedOut = parsedProg.accept(new Eval(randomGenEnv));
      //return parsedOut.getConst().toString();
    //} catch (Exception e) {
      //return ("Error evaluating " + parsedProg.toString());
    //}
  }
}
