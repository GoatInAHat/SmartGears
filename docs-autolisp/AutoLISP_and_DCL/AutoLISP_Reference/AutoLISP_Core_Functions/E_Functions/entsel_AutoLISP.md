---
title: entsel (AutoLISP)
guid: "GUID-9D4CF74D-8B8B-4D66-A952-564AFBA254E7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9D4CF74D-8B8B-4D66-A952-564AFBA254E7.htm"
generated: "2025-11-28T19:06:28.483046Z"
description: Prompts the user to select a single object (entity) by specifying a point
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

# entsel (AutoLISP)

> Prompts the user to select a single object (entity) by specifying a point

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9D4CF74D-8B8B-4D66-A952-564AFBA254E7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9D4CF74D-8B8B-4D66-A952-564AFBA254E7.htm)
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
(entsel
[msg]
)
```

- ***msg*:** **Type:**  String  A prompt string to be displayed to users. If omitted, `entsel`  prompts with the message, "Select object."

## Return Values

**Type:**  List or nil

A list whose first element is the entity name of the chosen object and whose second element is the coordinates (in terms of the current UCS) of the point used to pick the object.

The pick point returned by `entsel`  does not represent a point that lies on the selected object. The point returned is the location of the crosshairs at the time of selection. The relationship between the pick point and the object will vary depending on the size of the pickbox and the current zoom scale.

## Remarks

When operating on objects, you may want to simultaneously select an object and specify the point by which it was selected. Examples of this can be found with Object Snaps and in the AutoCAD BREAK, TRIM, and EXTEND commands. The `entsel`  function allows AutoLISP programs to perform this operation. It selects a single object, requiring the selection to be a pick point. The current Osnap setting is ignored by this function unless you specifically request it while you are in the function. The `entsel`  function honors keywords from a preceding call to `initget`.

## Examples

The following AutoCAD command sequence illustrates the use of the `entsel`  function and the list returned:

Command: **line**

From point: **1,1**

To point: **6,6**

To point: *Press Enter*

Command: **(setq e (entsel "\nSelect an object: "))**

Select an object: **3,3**

(<Entity name: 60000014> (3.0 3.0 0.0))
