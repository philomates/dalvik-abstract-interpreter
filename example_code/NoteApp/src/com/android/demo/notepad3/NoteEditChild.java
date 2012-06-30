package com.android.demo.notepad3;

public class NoteEditChild extends NoteEdit {
  public int child_field;
  public void anOverridenMethod() {
    index--;
    child_field = index*index;
  }

  public void saveState() {
    anOverridenMethod();
    if (child_field >= 2) {
      index = 0;
      runFib();
    }
  }

}
