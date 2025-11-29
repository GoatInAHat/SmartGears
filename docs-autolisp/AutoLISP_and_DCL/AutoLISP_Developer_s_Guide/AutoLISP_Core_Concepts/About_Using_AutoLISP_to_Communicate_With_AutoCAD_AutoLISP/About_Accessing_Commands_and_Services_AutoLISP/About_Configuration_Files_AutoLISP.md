---
title: About Configuration Files (AutoLISP)
guid: "GUID-11BEB9BE-2E76-4180-AB1F-1D05CEFCA9C8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-11BEB9BE-2E76-4180-AB1F-1D05CEFCA9C8.htm"
generated: "2025-11-28T19:06:06.900448Z"
description: AutoCAD uses a configuration file with the name acadxxxx.cfg to store device and application information.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About Configuration Files (AutoLISP)

> AutoCAD uses a configuration file with the name acadxxxx.cfg to store device and application information.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-11BEB9BE-2E76-4180-AB1F-1D05CEFCA9C8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-11BEB9BE-2E76-4180-AB1F-1D05CEFCA9C8.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 The
getcfg
 and
setcfg
 functions are obsolete, and it is recommended to store application information using the
vl-registry-read
 and
vl-registry-write
 functions.

The *xxxx*  in the file name refers to the AutoCAD release number. The AppData section of this file is provided for users and developers to store configuration information pertaining to their applications. The `getcfg`  and `setcfg`  functions allow AutoLISP applications to inspect and change the value of parameters in the AppData section.

The `setcfg`  function requires two strings that represent the section and parameter, and the value to assign. The value returned by `setcfg`  is `nil`  if the value could not be stored or the value that was being assigned to the parameter. The `getcfg`  function requires the section and parameter to retrieve a value from and returns the value if the parameter exists.

The following code creates a section under AppData named ArchStuff with a parameter titled WallThk. The value of ”8” is then assigned to WallThk.

```lisp
(setcfg "AppData/ArchStuff/WallThk" "8")

"8"
```

The following code returns the value assigned to the specified section and parameter.

```lisp
(getcfg "AppData/ArchStuff/WallThk")

"8"
```

Note:
 It is recommend to store values in the Windows Registry or the AutoCAD property list (
HKCU.plist
 and
HKLM.plist
) files on Mac OS. This can be done using the
vl-registry-read
 and
vl-registry-write
 functions.
