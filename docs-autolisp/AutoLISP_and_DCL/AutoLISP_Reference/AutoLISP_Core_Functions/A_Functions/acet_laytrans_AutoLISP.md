---
title: "acet-laytrans (AutoLISP)"
guid: "GUID-4BD312D1-1850-4DE4-A63D-35367C5F7F5D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4BD312D1-1850-4DE4-A63D-35367C5F7F5D.htm"
generated: "2025-11-28T19:06:22.181165Z"
description: Translates drawing layers to standards defined in another drawing or standards file
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

# acet-laytrans (AutoLISP)

> Translates drawing layers to standards defined in another drawing or standards file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4BD312D1-1850-4DE4-A63D-35367C5F7F5D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4BD312D1-1850-4DE4-A63D-35367C5F7F5D.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows only; not available in AutoCAD LT for Windows

## Signature

```lisp
(acet-laytrans
filename [settings]
)
```

- ***filename*:** **Type:**  String  A string specifying a file containing layer mappings to be used for translation.
- ***settings*:** **Type:**  Integer  A bit-coded integer specifying Layer Translator processing options. The bits can be added together in any combination to form a value between 0 and 15. If the *settings*  argument is omitted, a value of 15 (all options selected) is assumed. The bit values are as follows:  **0**  -- No options  **1**  -- Force entity color to BYLAYER  **2**  -- Force entity linetype to BYLAYER  **4**  -- Translate objects in blocks  **8**  -- Write transaction log

## Return Values

**Type:**  T or nil

`T`  if translation is successful; otherwise `nil`.

## Examples

The following command translates the current drawing using layer mappings saved in *LayMap.dwg*. No transaction log will be produced, but all other processing options will be in effect.

```lisp
(acet-laytrans "c:/my documents/cad drawings/LayMap.dwg" 7)

T
```
