---
title: Externally Defined Commands (AutoLISP)
guid: "GUID-50C54945-6760-4428-8181-417E2C79A945"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-50C54945-6760-4428-8181-417E2C79A945.htm"
generated: "2025-11-28T19:06:53.132952Z"
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
---

# Externally Defined Commands (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-50C54945-6760-4428-8181-417E2C79A945.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-50C54945-6760-4428-8181-417E2C79A945.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

AutoCAD ^®^  commands defined by ObjectARX ^®^  or AutoLISP ^®^  applications are called externally defined. AutoLISP applications may need to access externally defined commands differently from the way they access built-in AutoLISP functions. Many externally defined commands have their own programming interfaces that allow AutoLISP applications to take advantage of their functionality.

| Externally defined functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(c:3dsin *mode [multimat create] file*)](3dsin_AutoLISP_External_Function.md) | Imports a 3D Studio (.3ds) file | ✓ | -- | -- | -- | -- |
| [(align *args ...*)](align_AutoLISP_External_Function.md) | Translates and rotates objects, allowing them to be aligned with other objects | ✓ | ✓ | ✓ | -- | -- |
| [(c:cal *expression*)](cal_AutoLISP_External_Function.md) | Invokes the on-line geometry calculator and returns the value of the evaluated expression | ✓ | -- | ✓ | -- | -- |
| [(mirror3d *args ...*)](mirror3d_AutoLISP_External_Function.md) | Reflects selected objects about a user-specified plane | ✓ | -- | ✓ | -- | -- |
| [(rotate3d *args ...*)](rotate3d_AutoLISP_External_Function.md) | Rotates an object about an arbitrary 3D axis | ✓ | -- | ✓ | -- | -- |
| [(c:solprof *args ...*)](solprof_AutoLISP_External_Function.md) | Creates profile images of three-dimensional solids | ✓ | -- | ✓ | -- | -- |
