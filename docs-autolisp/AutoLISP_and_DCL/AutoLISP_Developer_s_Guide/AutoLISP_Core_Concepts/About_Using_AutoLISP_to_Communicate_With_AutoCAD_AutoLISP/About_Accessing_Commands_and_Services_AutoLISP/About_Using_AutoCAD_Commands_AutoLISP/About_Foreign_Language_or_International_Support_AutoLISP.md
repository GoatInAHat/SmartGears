---
title: About Foreign Language or International Support (AutoLISP)
guid: "GUID-D156B6ED-B1B4-42CB-BE1D-CA251BFC08C3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-D156B6ED-B1B4-42CB-BE1D-CA251BFC08C3.htm"
generated: "2025-11-28T19:06:06.415650Z"
description: AutoLISP programs can be used in an AutoCAD release that supports a language other than the original language the program was developed for.
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

# About Foreign Language or International Support (AutoLISP)

> AutoLISP programs can be used in an AutoCAD release that supports a language other than the original language the program was developed for.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-D156B6ED-B1B4-42CB-BE1D-CA251BFC08C3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-D156B6ED-B1B4-42CB-BE1D-CA251BFC08C3.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The standard AutoCAD commands and keywords used in an AutoLISP program can be automatically translated if you precede each command or keyword with an underscore (_).

```lisp
(command "_line" pt1 pt2 pt3 "_c")
```

If you are using the dot prefix (to avoid using redefined commands), you can place the dot and underscore in either order. Both `"._line"`  and `"_.line"`  are valid.

Note:
 It is recommended to always add an underscore (_) in front of a command name or keyword when using the
command
 or
command-s
 functions; this will help your program to work as expected when executed in a language other than it was originally targeted for.
