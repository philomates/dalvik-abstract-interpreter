/*
 * Copyright (C) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.android.demo.notepad3;

import android.app.Activity;
import android.database.Cursor;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

public class NoteEdit extends Activity {

  private EditText mTitleText;
  private EditText mBodyText;
  private int[] anArray;
  public int index;
  private Long mRowId;
  private NotesDbAdapter mDbHelper;

  public static int min(int x, int y) {
    if (x < y) {
      return x;
    }
    return y;
  }

  public static int max(int x, int y) {
    if (x > y) {
      return x;
    }
    return y;
  }

  // run tests
  public static void runMe() {
    // class/inheritance tests
    //NoteEdit foo = new NoteEdit();
    //NoteEditChild bar = new NoteEditChild();

    //foo.anOverridenMethod();
    //foo.dontOverrideMe();

    //bar.anOverridenMethod();
    //bar.dontOverrideMe();
    //bar.saveState();

    // recursion/maths tests
    int result = fib(3);
    //int result2 = factorial(4);
    //int result = 0;
    //for (int i=0; i<10; i++) {
      //result += isEven(i);
    //}
  }

  public static int isEven(int i) {
    if (i%2 == 0)
      return 1;
    return 0;
  }

  public void anOverridenMethod() {
    index++;
  }

  public void dontOverrideMe() {
    index = index*2;
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    index = 0;
    anArray = new int[10];
    anArray[0] = 0;
    anArray[1] = 1;
    super.onCreate(savedInstanceState);
    mDbHelper = new NotesDbAdapter(this);
    mDbHelper.open();

    setContentView(R.layout.note_edit);
    setTitle(R.string.edit_note);

    mTitleText = (EditText) findViewById(R.id.title);
    mBodyText = (EditText) findViewById(R.id.body);

    Button confirmButton = (Button) findViewById(R.id.confirm);

    mRowId = (savedInstanceState == null) ? null :
      (Long) savedInstanceState.getSerializable(NotesDbAdapter.KEY_ROWID);
    if (mRowId == null) {
      Bundle extras = getIntent().getExtras();
      mRowId = extras != null ? extras.getLong(NotesDbAdapter.KEY_ROWID)
        : null;
    }

    populateFields();

    confirmButton.setOnClickListener(new View.OnClickListener() {

      public void onClick(View view) {
        setResult(RESULT_OK);
        finish();
      }

    });
  }

  private void populateFields() {
    if (mRowId != null) {
      Cursor note = mDbHelper.fetchNote(mRowId);
      startManagingCursor(note);
      mTitleText.setText(note.getString(
            note.getColumnIndexOrThrow(NotesDbAdapter.KEY_TITLE)));
      mBodyText.setText(note.getString(
            note.getColumnIndexOrThrow(NotesDbAdapter.KEY_BODY)));
    }
  }

  @Override
  protected void onSaveInstanceState(Bundle outState) {
    super.onSaveInstanceState(outState);
    saveState();
    outState.putSerializable(NotesDbAdapter.KEY_ROWID, mRowId);
  }

  @Override
  protected void onPause() {
    super.onPause();
    saveState();
  }

  @Override
  protected void onResume() {
    super.onResume();
    populateFields();
  }

  public static int factorial(int n) {
    if (n==0) {
      return 1;
    } else {
      return n*factorial(n-1);
    }
  }

  public static int fib(int n) {
        if (n <= 1)
          return n;
        return fib(n-1) + fib(n-2);
  }


  public static void runFib() {
    int result = fib(10);
    int result2 = factorial(10);
  }

  private void saveState() {
    String title = mTitleText.getText().toString();
    String body = mBodyText.getText().toString();
    anOverridenMethod();
    runFib();
    if (index >= 2) { index = 0; }

    if (mRowId == null) {
      long id = mDbHelper.createNote(title + anArray[index], body);
      if (id > 0) {
        mRowId = id;
      }
    } else {
      mDbHelper.updateNote(mRowId, title + anArray[index], body);
    }
    index++;
  }

}
