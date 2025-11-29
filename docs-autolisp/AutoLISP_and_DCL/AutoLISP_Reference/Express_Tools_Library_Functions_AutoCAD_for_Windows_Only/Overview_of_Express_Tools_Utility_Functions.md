---
title: Overview of Express Tools Utility Functions
guid: "GUID-66179117-AF23-441B-A990-3266FAF7EA38"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-66179117-AF23-441B-A990-3266FAF7EA38.htm"
generated: "2025-11-28T19:06:53.805986Z"
description: The acetutil modules (acetutil.arx and acetutil*.fas) provide a number of utility functions which can be called from AutoLISP on Windows for AutoCAD only.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 02/04/2024
topic_subtype:
  - autolisp
---

# Overview of Express Tools Utility Functions

> The acetutil modules ( acetutil.arx and acetutil*.fas ) provide a number of utility functions which can be called from AutoLISP on Windows for AutoCAD only.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-66179117-AF23-441B-A990-3266FAF7EA38.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-66179117-AF23-441B-A990-3266FAF7EA38.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 02/04/2024

This documentation covers all the functionality present in version 1.38 of *acetutil.arx*, along with a few functions provided in *acetutil.fas*. Functions that do not have a specific library designation appear in *acetutil.arx*.

Starting with AutoCAD 2025, you might need to initialize the Express Tools Utility functions before they can be used by your AutoLISP programs. The `acet-load-expresstools`  function is used to initialize the Express Tools Utility functions, which is similar to initializing the ActiveX/COM library for use with the `vl-load-com`  function.

Note:
 The
acet-util-ver
 function can be called to identify the module version of the Express Tools Utility functions. This function returns a REAL value containing the current version number of the
acetutil.arx
 library.

```lisp
(acet-util-ver)
1.38
```
