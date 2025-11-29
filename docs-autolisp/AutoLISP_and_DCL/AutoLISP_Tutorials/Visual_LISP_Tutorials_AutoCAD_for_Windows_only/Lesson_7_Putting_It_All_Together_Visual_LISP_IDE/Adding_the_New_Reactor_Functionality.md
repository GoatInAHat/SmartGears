---
title: Adding the New Reactor Functionality
guid: "GUID-9C3CE8AA-3D59-4608-B370-9CB357A393E9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9C3CE8AA-3D59-4608-B370-9CB357A393E9.htm"
generated: "2025-11-28T19:07:05.459011Z"
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

# Adding the New Reactor Functionality

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9C3CE8AA-3D59-4608-B370-9CB357A393E9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9C3CE8AA-3D59-4608-B370-9CB357A393E9.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

In Lesson 6, you hooked up callback function `gp:command-will-start`  to the reactor event `:vlr-commandWillStart`. As it currently exists, the function displays some messages and initializes two global variables, `*polyToChange*`  and `*reactorsToRemove*`, to `nil`.

## To add functionality to the gp:command-will-start callback function

1.  Open your
   gpreact.lsp
    file.
2. In the
   gp:command-will-start
    function, add two variables to the
   setq
    function call by modifying it as follows:

   ```lisp
   ;; Reset all four reactor globals to nil.
   (setq   *lostAssociativity* nil
           *polyToChange* nil
           *reactorsToChange* nil
           *reactorsToRemove* nil)
   ```
3. Replace the remaining code in
   gp:command-will-start
   , up to the last
   princ
    function call, with the following code:

   ```lisp
   (if (member (setq currentCommandName (car command-list))
               '("U"          "UNDO"      "STRETCH"
               "MOVE"       "ROTATE"    "SCALE"
               "BREAK"      "GRIP_MOVE" "GRIP_ROTATE"
               "GRIP_SCALE" "GRIP_MIRROR")
       ) ;_ end of member
     (progn
       (setq *lostAssociativity* T)
       (princ "\nNOTE: The ")
       (princ currentCommandName)
       (princ " command will break a path's associativity .")
     ) ;_ end of progn
   ) ;_ end of if
   ```

   This code checks to see if the user issued a command that breaks the associativity between the tiles and the path. If the user issued such a command, the program sets the `*lostAssociativity*`  global variable and warns the user.

   As you experiment with the garden path application, you may discover additional editing commands that can modify the garden path and cause the loss of associativity. Add these commands to the quoted list so that the user is aware of what will happen. When this function fires, the user has started a command but has not selected any entities to modify. The user could still cancel the command, leaving things unchanged.
