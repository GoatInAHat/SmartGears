---
title: To set and retrieve variables from a document namespace (AutoLISP)
guid: "GUID-A61FAC68-AAA4-4242-9349-811071D53DA4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A61FAC68-AAA4-4242-9349-811071D53DA4.htm"
generated: "2025-11-28T19:05:58.627311Z"
description: Values can be stored and retrieved from AutoLISP variables while a drawing remains open.
topic_type: "task-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# To set and retrieve variables from a document namespace (AutoLISP)

> Values can be stored and retrieved from AutoLISP variables while a drawing remains open.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A61FAC68-AAA4-4242-9349-811071D53DA4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A61FAC68-AAA4-4242-9349-811071D53DA4.htm)
- Topic Type: task-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

1. At the AutoCAD Command prompt or in an AutoLISP program, enter an AutoLISP statement that uses the
   setq
    function and press Enter.
2. Enter the name of the variable you assigned a value to and prefix it with an
   !
    (exclamation point) to return the value assigned to the variable and press Enter.

## Example

1. In the AutoCAD drawing environment, create or open two new drawings.
2. Do one of the following:
   - In Windows, on the ribbon, click View tab
      User Interface panel
      Tile Vertically.

     You should see two open document windows shown side by side.
   - In Mac OS, resize each drawing window so they can be seen side by side.
3. At the AutoCAD Command prompt, enter
   (setq draw1foo "I am drawing 1")
    and press Enter.

   Returns:

   ```lisp
   "I am drawing 1"
   ```
4. Activate the second drawing by clicking in the window's title bar.
5. At the AutoCAD Command prompt, enter
   !draw1foo
    and press Enter.

   Returns:

   ```lisp
   nil
   ```

   The variable is `nil`  because it has not been set in this document’s.
6. At the AutoCAD Command prompt, enter
   (setq draw2foo "I too am a drawing, but number 2")
    and press Enter.

   Returns:

   ```lisp
   "I too am a drawing, but number 2"
   ```
7. Activate the previous drawing.
8. At the AutoCAD Command prompt, enter
   !draw1foo
    and press Enter.

   Returns:

   ```lisp
   "I am drawing 1"
   ```
9. At the AutoCAD Command prompt, enter
   !draw2foo
    and press Enter.

   Returns:

   ```lisp
   nil
   ```

   The `draw1foo`  variable contains the value you set in Step 3, but `draw2foo`  is nil because you did not set it to a value in the current document; you set a different variable of the same name in the second drawing's namespace.
