---
title: "About Wild-Card Patterns in Selection Set Filter Lists (AutoLISP)"
guid: "GUID-11EA0ABD-AE29-4DD7-92A5-DFF6EC1C9799"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-11EA0ABD-AE29-4DD7-92A5-DFF6EC1C9799.htm"
generated: "2025-11-28T19:06:11.033781Z"
description: "Symbol names specified in filtering lists can include wild-card patterns."
topic_type: concept
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

# About Wild-Card Patterns in Selection Set Filter Lists (AutoLISP)

> Symbol names specified in filtering lists can include wild-card patterns.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-11EA0ABD-AE29-4DD7-92A5-DFF6EC1C9799.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-11EA0ABD-AE29-4DD7-92A5-DFF6EC1C9799.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The wild-card patterns recognized by `ssget`  are the same as those recognized by the `wcmatch`  function. When filtering for anonymous blocks, you must precede the * character with a reverse single quotation mark (**`````**), also known as an escape character, because the **`*`**  is read by `ssget`  as a wild-card character.

For example, you can retrieve an anonymous block named *U2 with the following:

```lisp
(ssget "X" '((2 . "`*U2")))
```
