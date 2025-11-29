---
title: Using ActiveX Methods in Reactor Callback Functions
guid: "GUID-748D014B-FB64-4D56-BD2B-7B1BD6FBC3E9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-748D014B-FB64-4D56-BD2B-7B1BD6FBC3E9.htm"
generated: "2025-11-28T19:07:05.823025Z"
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

# Using ActiveX Methods in Reactor Callback Functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-748D014B-FB64-4D56-BD2B-7B1BD6FBC3E9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-748D014B-FB64-4D56-BD2B-7B1BD6FBC3E9.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The second detail appearing in the pseudo-code shows up near the end, at the step for redrawing the tiles. Here is the pseudo-code statement:

```lisp
Redraw the tiles (force ActiveX drawing)
```

The parenthetical phrase says it all: force ActiveX drawing. Why is this required? Why can't the application use the object creation preference stored in the association sublist?

The answer is you cannot use the `command`  function for entity creation within a reactor callback function. This has to do with some internal workings of AutoCAD. You need to force the tile drawing routine to use ActiveX. You will hear more about this issue later in this lesson.
