---
title: "vl-list-loaded-vlx (AutoLISP)"
guid: "GUID-0F0C7A45-41AA-410F-AEF0-EECAE812FCF3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0F0C7A45-41AA-410F-AEF0-EECAE812FCF3.htm"
generated: "2025-11-28T19:06:48.163178Z"
description: "Returns a list of all separate-namespace VLX files associated with the current document"
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

# vl-list-loaded-vlx (AutoLISP)

> Returns a list of all separate-namespace VLX files associated with the current document

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0F0C7A45-41AA-410F-AEF0-EECAE812FCF3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0F0C7A45-41AA-410F-AEF0-EECAE812FCF3.htm)
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
(vl-list-loaded-vlx)
```

No arguments.

## Return Values

**Type:**  List or nil

A list of symbols identifying separate-namespace VLX applications associated with the current AutoCAD document; otherwise `nil`, if there are no VLX applications associated with the current document.

The `vl-list-loaded-vlx`  function does not identify VLX applications that are loaded in the current document's namespace.

## Remarks

VLX files are supported on Windows only.

Note:
 This function is supported on Mac OS and Web, but does not affect the program.

## Examples

Test for loaded VLX files associated with the current AutoCAD document:

```lisp
(vl-list-loaded-vlx)

nil
```

No VLX files are associated with the current document.

Load two VLX files; both VLX applications have been compiled to run in their own namespace:

```lisp
(load "c:/my documents/visual lisp/examples/foo1.vlx")

nil

(load "c:/my documents/visual lisp/examples/foo2.vlx")

nil
```

Test for loaded VLX files associated with the current AutoCAD document:

```lisp
(vl-list-loaded-vlx)

(FOO1 FOO2)
```

The two VLX files just loaded are identified by `vl-list-loaded-vlx`.

Load a VLX that was compiled to run in a document's namespace:

```lisp
(load "c:/my documents/visual lisp/examples/foolocal.vlx")

nil
```

Test for loaded VLX files:

```lisp
(vl-list-loaded-vlx)

(FOO1 FOO2))
```

The last VLX loaded (*foolocal.vlx*) is not returned by `vl-list-loaded-vlx`  because the application was loaded into the document's namespace; the VLX does not have its own namespace.
