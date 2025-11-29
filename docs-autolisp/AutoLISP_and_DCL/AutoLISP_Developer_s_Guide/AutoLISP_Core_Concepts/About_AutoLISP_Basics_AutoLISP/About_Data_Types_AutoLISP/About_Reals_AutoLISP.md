---
title: About Reals (AutoLISP)
guid: "GUID-F2BFD097-295B-4499-BBD9-0A785C377070"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F2BFD097-295B-4499-BBD9-0A785C377070.htm"
generated: "2025-11-28T19:06:01.116659Z"
description: "A real is a number containing a decimal point. Numbers between -1 and 1 must contain a leading zero."
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# About Reals (AutoLISP)

> A real is a number containing a decimal point. Numbers between -1 and 1 must contain a leading zero.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F2BFD097-295B-4499-BBD9-0A785C377070.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F2BFD097-295B-4499-BBD9-0A785C377070.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

Real numbers are stored in double-precision floating-point format, providing at least 14 significant digits of precision. Note that AutoLISP does not show you all the significant digits.

Reals can be expressed in scientific notation, which has an optional e or E followed by the exponent of the number (for example, 0.0000041 is the same as 4.1e-6). Numbers such as 3.1, 0.23, -56.123, and 21,000,000.0 are all valid AutoLISP real numbers.
