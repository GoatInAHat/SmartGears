---
title: arx (AutoLISP)
guid: "GUID-D74F3A2B-F506-415F-8F5A-258764C2E26D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D74F3A2B-F506-415F-8F5A-258764C2E26D.htm"
generated: "2025-11-28T19:06:23.210798Z"
description: Returns a list of the currently loaded ObjectARX applications
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
  - function
---

# arx (AutoLISP)

> Returns a list of the currently loaded ObjectARX ® applications

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D74F3A2B-F506-415F-8F5A-258764C2E26D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D74F3A2B-F506-415F-8F5A-258764C2E26D.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(arx)
```

 No arguments.

## Return Values

**Type:**  String

A list of ObjectARX application file names; the path is not included in the file name.

## Examples

```lisp
(arx)
```

- **Windows and Web:** ("acapp.arx" "acmted.arx" ...)
- **Mac OS:** ("acapp.bundle" "acmted.crx" ...)
