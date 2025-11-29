---
title: dictnext (AutoLISP)
guid: "GUID-8596E2E9-FEB5-4995-ADE4-31287864C0CB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8596E2E9-FEB5-4995-ADE4-31287864C0CB.htm"
generated: "2025-11-28T19:06:26.965104Z"
description: Finds the next item in a dictionary
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

# dictnext (AutoLISP)

> Finds the next item in a dictionary

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8596E2E9-FEB5-4995-ADE4-31287864C0CB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8596E2E9-FEB5-4995-ADE4-31287864C0CB.htm)
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
(dictnext
ename [rewind]
)
```

Arguments

- ***ename*:** **Type:**  Ename (entity name)  Name of the dictionary being viewed.
- ***rewind*:** **Type:**  T or nil  If this argument is present and is not `nil`, the dictionary is rewound and the first entry in it is retrieved.

## Return Values

**Type:**  Ename (entity name)

The next entry in the specified dictionary; otherwise `nil`, when the end of the dictionary is reached. Entries are returned as lists of dotted pairs of DXF-type codes and values. Deleted dictionary entries are not returned.

The `dictsearch`  function specifies the initial entry retrieved.

Use `namedobjdict`  to obtain the master dictionary entity name.

Note:
 Once you begin stepping through the contents of a dictionary, passing a different dictionary name to
dictnext
 will cause the place to be lost in the original dictionary. In other words, only one global iterator is maintained for use in this function.

## Examples

Create a dictionary and an entry as shown in the example for `dictadd`. Then make another Xrecord object:

```lisp
(setq xname (entmakex datalist))

<Entity name: 1b62d60>
```

Add this Xrecord object to the dictionary, as the second record in the dictionary:

```lisp
(dictadd newdict "DATA_RECORD_2" xname)

<Entity name: 1b62d60>
```

Return the entity name of the next entry in the dictionary:

```lisp
(cdr (car (dictnext newdict)))

<Entity name: 1bac958>
```

`dictnext`  returns the name of the first entity added to the dictionary.

Return the entity name of the next entry in the dictionary:

```lisp
(cdr (car (dictnext newdict)))

<Entity name: 1bac960>
```

`dictnext`  returns the name of the second entity added to the dictionary.

Return the entity name of the next entry in the dictionary:

```lisp
(cdr (car (dictnext newdict)))

nil
```

There are no more entries in the dictionary, so `dictnext`  returns `nil`.

Rewind to the first entry in the dictionary and return the entity name of that entry:

```lisp
(cdr (car (dictnext newdict T)))

<Entity name: 1bac958>
```

Specifying `T`  for the optional *rewind*  argument causes `dictnext`  to return the first entry in the dictionary.
