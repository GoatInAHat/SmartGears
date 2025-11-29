---
title: Matching Parentheses
guid: "GUID-4AC7C3B7-E22E-4358-A972-40FBAC73DB4A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-4AC7C3B7-E22E-4358-A972-40FBAC73DB4A.htm"
generated: "2025-11-28T19:07:02.367616Z"
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

# Matching Parentheses

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-4AC7C3B7-E22E-4358-A972-40FBAC73DB4A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-4AC7C3B7-E22E-4358-A972-40FBAC73DB4A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Visual LISP provides a parenthesis matching feature to help you find the close parenthesis that corresponds to an open parenthesis.

## To match an open parenthesis with its corresponding close parenthesis

1. Place your cursor in front of the opening parenthesis that precedes the
   setq
    function call.
2. Press Ctrl+Shift+]. (Double-clicking also does the trick.)

Visual LISP finds the closing parenthesis that matches the one you chose, and selects all the code in between. Not only does this ensure you typed in the correct number of parentheses, it also makes it easy to copy or cut the selected text. This might have come in handy when you updated this call at the end of Lesson 4.

Why else might you want to do this? You can copy some code to the Visual LISP Console window, paste it there, and try it out. Or maybe you have figured out how to replace 50 lines of code with three really marvelous lines of much better code. You can quickly select the old code using the parentheses matching tool, then eliminate it with a single keystroke. It is a lot quicker to let Visual LISP find an entire block than for you to hunt down every last closing parenthesis.

There is a corresponding key command for matching and selecting backward. To try this, put your cursor after a closing parenthesis, then either double-click or press Ctrl+Shift+[. Visual LISP searches for the corresponding opening parenthesis, and selects it along with the enclosed code.

Both commands are also available by clicking Edit ![](../../../../_assets/ac_menuaro-f6601f42.gif)  Parentheses Matching from the Visual LISP menu.
