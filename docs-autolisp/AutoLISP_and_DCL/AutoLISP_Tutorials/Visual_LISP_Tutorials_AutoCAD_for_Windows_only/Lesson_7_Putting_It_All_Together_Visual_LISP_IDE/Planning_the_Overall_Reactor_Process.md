---
title: Planning the Overall Reactor Process
guid: "GUID-A33CFACE-CEA1-43D7-9F3C-48E56760FD29"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A33CFACE-CEA1-43D7-9F3C-48E56760FD29.htm"
generated: "2025-11-28T19:07:05.154034Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Planning the Overall Reactor Process

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A33CFACE-CEA1-43D7-9F3C-48E56760FD29.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A33CFACE-CEA1-43D7-9F3C-48E56760FD29.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You need to define several new functions in this lesson. Rather than present you with details on all aspects of the new code, this lesson presents an overview and points out the concepts behind the code. At the end of the lesson, you will have all the source code necessary to create a garden path application identical to the sample program you ran in Lesson 1.

Note:
 When you are in the midst of developing and debugging reactor-based applications, there is always the potential of leaving AutoCAD
®
 in an unstable state. This can be caused by several situations, such as failing to remove a reactor from deleted entities. For this reason, it is recommended that before beginning Lesson 7, you should close Visual LISP, save any open files as you do so, exit AutoCAD, then restart both applications.

Begin by loading the project as it existed at the end of Lesson 6.

Two obvious pieces of work remain to be done in the garden path application:

-  Writing the object reactor callbacks.
-  Writing the editor reactor callbacks.

You also need to consider how to handle the global variables in your program. Often, it is desirable to have global variables retain a value throughout an AutoCAD drawing session. In the case of reactors, however, this is not the case. To illustrate this, imagine a user of your garden path application has drawn several garden paths in a single drawing. After doing this, the user erases them, first one at a time, then two at a time, and so on, until all but one path is erased.

Lesson 5 introduced a global variable `*reactorsToRemove*`, responsible for storing pointers to the reactors for the polylines about to be erased. When `*reactorsToRemove*`  is declared in `gp:outline-erased`, the event lets you know the polyline is about to be erased. The polyline is not actually removed until the `gp:command-ended`  event fires.

The first time the user deletes a polyline, things work just as you would expect. In `gp:outline-erased`, you store a pointer to the reactor. When `gp:command-ended`  fires, you remove the tiles associated with the polyline to which the reactor is attached, and all is well. Then, the user decides to erase two paths. As a result, your application will get two calls to `gp:outline-erased`, one for each polyline about to be erased. There are two potential problems you must anticipate:

- When you
  setq
   the
  *reactorsToRemove*
   variable, you must add a pointer to a reactor to the global, making sure not to overwrite any values already stored there. This means
  *reactorsToRemove*
   must be a list structure, so you can append reactor pointers to it. You can then accumulate several reactor pointers corresponding to the number of paths the user is erasing within a single erase command.
-  Every time
  gp:command-will-start
   fires, indicating a new command sequence is beginning, you should reinitialize the
  *reactorsToRemove*
   variable to
  nil.
   This is necessary so that the global is not storing reactor pointers from the previous erase command.

  If you do not reinitialize the global variable or use the correct data structure (in this case, a list), you will get unexpected behavior. In the case of reactors, unexpected behavior can be fatal to your AutoCAD session.

Here is the chain of events that needs to occur for users to erase two garden paths with a single erase command. Note how global variables are handled:

- Initiate the ERASE command. This triggers the
  gp:command-will-start
   function. Set
  *reactorsToRemove*
   to
  nil
  .
- Select two polylines; your application is not yet notified.
-  Press Enter to erase the two selected polylines.

  Your application gets a callback to `gp:outline-erased`  for one of the polylines. Add its reactor pointer to the null global, `*reactorsToRemove*`.

  Your application gets a callback to `gp:outline-erased`  for the second of the polylines. Append its reactor pointer to the `*reactorsToRemove*`  global that already contains the first reactor pointer.
-  AutoCAD deletes the polylines.
- Your callback function
  gp:command-ended
   fires. Eliminate any tiles associated with the reactor pointers stored in
  *reactorsToRemove*
  .

In addition to the `*reactorsToRemove*`  global, your application also includes a `*polyToChange*`  global, which stores a pointer to any polyline that will be modified. Two additional global variables for the application will be introduced later in this lesson.
