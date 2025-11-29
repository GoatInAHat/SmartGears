---
title: Getting Help With a Function (AutoLISP)
guid: "GUID-88EEAE20-9DD3-4774-8654-164BC8997FD0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-88EEAE20-9DD3-4774-8654-164BC8997FD0.htm"
generated: "2025-11-28T19:07:02.647708Z"
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

# Getting Help With a Function (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-88EEAE20-9DD3-4774-8654-164BC8997FD0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-88EEAE20-9DD3-4774-8654-164BC8997FD0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The code that adds a lightweight polyline to the drawing calls a function named `vla-addLightweightPolyline`. Not only is that a lengthy term to write, but there are several functions whose names begin with `vla-add`  that you will use to create entities. Rather than consulting a manual to look up the function name every time you create a program, let Visual LISP help.

## To get help with using a function

1. Enter the following on a blank line:

   ```lisp
   (vla-add
   ```
2. Press Ctrl+Shift+Spacebar.
3. Scroll through the list until you find
   vla-addLightweightPolyline
   .
4. Double-click on
   vla-addLightweightPolyline
   .

   Visual LISP displays the Symbol Service dialog box for the selected function.
5.  Clicking the Help button in the Symbol Service dialog box.
6.  Delete the changes you made to
   gpdraw.lsp
   ; these were for demonstration purposes only. Also, close the Symbol Service and Apropos windows.
