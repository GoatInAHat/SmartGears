---
title: To set and retrieve variables in the blackboard namespace (AutoLISP)
guid: "GUID-4E317058-1E6A-45E2-A9BE-71D63D2D0A33"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4E317058-1E6A-45E2-A9BE-71D63D2D0A33.htm"
generated: "2025-11-28T19:05:59.049658Z"
description: Variables can be stored and retrieved across multiple open drawings using the blackboard namespace.
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

# To set and retrieve variables in the blackboard namespace (AutoLISP)

> Variables can be stored and retrieved across multiple open drawings using the blackboard namespace.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4E317058-1E6A-45E2-A9BE-71D63D2D0A33.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4E317058-1E6A-45E2-A9BE-71D63D2D0A33.htm)
- Topic Type: task-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

1. At the AutoCAD Command prompt or from an AutoLISP program, use the
   vl-bb-set
    function to set the value of a variable in the blackboard.
2. Use the
   vl-bb-ref
    function to retrieve the value of a variable from the blackboard.

## Example

1. At the AutoCAD Command prompt, enter
   (vl-bb-set '*example* 0)
    and press Enter.

   Returns:

   ```lisp
   0
   ```

   The `*example*`  variable is set to 0 in the blackboard namespace.
2. At the AutoCAD Command prompt, enter
   (vl-bb-ref '*example*)
    and press Enter.

   Returns:

   ```lisp
   0
   ```
3. At the AutoCAD Command prompt, enter
   !*example*
    and press Enter.

   Returns:

   ```lisp
   nil
   ```

   The `*example*`  variable returns nil because it has not been set in the document namespace.
4. At the AutoCAD Command prompt, enter
   (setq *example* -1)
    and press Enter.

   Returns:

   ```lisp
   -1
   ```

   The `*example*`  variable is set to -1 in the document namespace.
5. At the AutoCAD Command prompt, enter
   (vl-bb-ref '*example*)
    and press Enter.

   Returns:

   ```lisp
   0
   ```

   The blackboard variable named `*example*`  is still set to the value assigned in Step 1; setting the document variable of the same name in Step 4 had no effect on the variable in the blackboard.

   You can also the `vl-doc-set`  and `vl-doc-ref`  functions to set and retrieve document namespace variables from a separate-namespace VLX, and `vl-propagate`  to set the value of a variable in all open document namespaces.
