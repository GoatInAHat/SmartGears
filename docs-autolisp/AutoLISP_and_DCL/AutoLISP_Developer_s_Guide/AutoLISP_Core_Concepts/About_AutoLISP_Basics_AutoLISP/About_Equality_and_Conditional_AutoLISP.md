---
title: About Equality and Conditional (AutoLISP)
guid: "GUID-A77594CA-D8AF-4550-8EAA-3D3EED0248AC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A77594CA-D8AF-4550-8EAA-3D3EED0248AC.htm"
generated: "2025-11-28T19:06:03.612374Z"
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

# About Equality and Conditional (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A77594CA-D8AF-4550-8EAA-3D3EED0248AC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A77594CA-D8AF-4550-8EAA-3D3EED0248AC.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

AutoLISP includes functions that provide equality verification as well as conditional branching and looping. The equality and conditional functions are listed in AutoLISP Function Synopsis (AutoLISP), under the heading Equality and Conditional Functions (AutoLISP).

When writing code that checks string and symbol table names, keep in mind that AutoLISP automatically converts symbol table names to upper case in some instances. When testing symbol names for equality, you need to make the comparison insensitive to the case of the names. Use the `strcase`  function to convert strings to the same case before testing them for equality.
