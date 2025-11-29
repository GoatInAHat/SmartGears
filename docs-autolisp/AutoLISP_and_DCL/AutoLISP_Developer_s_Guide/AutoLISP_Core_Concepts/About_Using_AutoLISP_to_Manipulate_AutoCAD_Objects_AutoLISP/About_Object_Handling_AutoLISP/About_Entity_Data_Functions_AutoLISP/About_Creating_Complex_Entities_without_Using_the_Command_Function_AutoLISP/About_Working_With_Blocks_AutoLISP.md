---
title: About Working With Blocks (AutoLISP)
guid: "GUID-D0E1B3EC-FA5B-4820-83C0-EDEE0BE540E0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-D0E1B3EC-FA5B-4820-83C0-EDEE0BE540E0.htm"
generated: "2025-11-28T19:06:12.748130Z"
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

# About Working With Blocks (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-D0E1B3EC-FA5B-4820-83C0-EDEE0BE540E0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-D0E1B3EC-FA5B-4820-83C0-EDEE0BE540E0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

There is no direct method for an application to check whether a block listed in the BLOCK table is actually referenced by an insert object in the drawing. You can use the following code to scan the drawing for instances of a block reference:

```lisp
(ssget "x" '((2 . "BLOCKNAME")))
```

You must also scan each block definition for instances of nested blocks.
