---
title: regapp (AutoLISP)
guid: "GUID-D331A88A-9B6E-49C9-B745-D0A063F38FD2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D331A88A-9B6E-49C9-B745-D0A063F38FD2.htm"
generated: "2025-11-28T19:06:39.769677Z"
description: Registers an application name with the current AutoCAD drawing in preparation for using extended object data
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

# regapp (AutoLISP)

> Registers an application name with the current AutoCAD drawing in preparation for using extended object data

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D331A88A-9B6E-49C9-B745-D0A063F38FD2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D331A88A-9B6E-49C9-B745-D0A063F38FD2.htm)
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
(regapp
application
)
```

- ***application*:** **Type:**  String  Application name. The name must be a valid symbol table name.

## Return Values

**Type:**  String

If an application of the same name has already been registered, this function returns `nil`; otherwise it returns the name of the application.

If registered successfully, the application name is entered into the APPID symbol table. This table maintains a list of the applications that are using extended data in the drawing.

## Examples

```lisp
(regapp "ADESK_4153322344")

"ADESK_4153322344"

(regapp "DESIGNER-v2.1-124753")

"DESIGNER-v2.1-124753"
```

Note:
 It is recommended that you pick a unique application name. One way of ensuring this is to adopt a naming scheme that uses the company or product name and a unique number (like your telephone number or the current date/time). The product version number can be included in the application name or stored by the application in a separate integer or real-number field; for example,
(1040 2.1)
.
