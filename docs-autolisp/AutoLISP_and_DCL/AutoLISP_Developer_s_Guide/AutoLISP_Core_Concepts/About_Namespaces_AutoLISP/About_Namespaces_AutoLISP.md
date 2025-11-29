---
title: About Namespaces (AutoLISP)
guid: "GUID-4E949651-A881-4E0F-9DBA-A4645FE473B3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4E949651-A881-4E0F-9DBA-A4645FE473B3.htm"
generated: "2025-11-28T19:05:58.284045Z"
description: A namespace is a LISP environment containing a set of symbols (for example, variables and functions).
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

# About Namespaces (AutoLISP)

> A namespace is a LISP environment containing a set of symbols (for example, variables and functions).

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4E949651-A881-4E0F-9DBA-A4645FE473B3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4E949651-A881-4E0F-9DBA-A4645FE473B3.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The concept of namespaces was introduced to prevent applications running in one drawing window from unintentionally affecting applications running in other windows. Each open AutoCAD drawing document has its own namespace. Variables and functions defined in one document namespace are isolated from variables and functions defined in other namespaces.

You can see how this works by trying a simple example.
