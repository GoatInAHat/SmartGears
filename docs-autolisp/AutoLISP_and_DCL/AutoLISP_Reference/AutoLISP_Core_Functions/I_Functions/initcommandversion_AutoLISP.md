---
title: initcommandversion (AutoLISP)
guid: "GUID-6176FC98-DC5D-433E-8D76-F481BE68D46A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6176FC98-DC5D-433E-8D76-F481BE68D46A.htm"
generated: "2025-11-28T19:06:32.896561Z"
description: Forces the next command to run with the specified version
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

# initcommandversion (AutoLISP)

> Forces the next command to run with the specified version

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6176FC98-DC5D-433E-8D76-F481BE68D46A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6176FC98-DC5D-433E-8D76-F481BE68D46A.htm)
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
(initcommandversion
[version]
)
```

- ***version*:** **Type:**  Integer  This argument specifies the version of the command to be used. If this argument is not present, the next use (and next use only) of a supported command will initialize to the latest version.

## Return Values

**Type:**  T

Always returns `T`.

## Remarks

This function makes it possible to force a specific behavior for a supported command regardless of how it is being run. This only affects commands that have been updated to support a command version. In such commands, a test for an initialized command version replaces the legacy test for whether the command is being run from AutoLISP or a script. When a supported command is being run manually, the default version is 2 (or the latest version). When a command is being run from automation, the default version is 1.

## Examples

Initializing a specific command version may affect each supported command differently. For example, here is the AutoCAD FILLET command with and without an initialized version:

Command: **FILLET**

Current settings: Mode = TRIM, Radius = 0.0000

Select first object or [Undo/Polyline/Radius/Trim/Multiple]: *Cancel*

Command: **(initcommandversion 1)**

Command: **FILLET**

Current settings: Mode = TRIM, Radius = 0.0000

Select first object or [uNdo/Polyline/Radius/Trim/mUltiple]: *Cancel*

Another typical example is the AutoCAD COLOR command. Run normally, COLOR displays the Select Color dialog; but by running `(initcommandversion 1)`  before the COLOR command, it is forced to prompt from color from the Command prompt.
