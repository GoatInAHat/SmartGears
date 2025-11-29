---
title: dictadd (AutoLISP)
guid: "GUID-5931D6D8-7F6E-4773-B08C-DEC5F9C4A22E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5931D6D8-7F6E-4773-B08C-DEC5F9C4A22E.htm"
generated: "2025-11-28T19:06:26.797827Z"
description: Adds a nongraphical object to the specified dictionary
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

# dictadd (AutoLISP)

> Adds a nongraphical object to the specified dictionary

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5931D6D8-7F6E-4773-B08C-DEC5F9C4A22E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5931D6D8-7F6E-4773-B08C-DEC5F9C4A22E.htm)
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
(dictadd
ename symbol newobj
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the dictionary the object is being added to.
- ***symbol*:** **Type:**  String  The key name of the object being added to the dictionary; *symbol*  must be a unique name that does not already exist in the dictionary.
- ***newobj*:** **Type:**  Ename (entity name)  A nongraphical object to be added to the dictionary.

## Return Values

**Type:**  Ename (entity name)

The entity name of the object added to the dictionary.

## Remarks

As a general rule, each object added to a dictionary must be unique to that dictionary. This is specifically a problem when adding group objects to the group dictionary. Adding the same group object using different key names results in duplicate group names, which can send the `dictnext`  function into an infinite loop.

Drawing properties such as Title, Subject, Author, and Keywords, can be accessed only with the `IAcadSummaryInfo`  interface that is part of the AutoCAD ActiveX/COM library, accessible as a property of the `Document`  object in the AutoCAD object model. The AutoCAD ActiveX/COM library is available only in AutoCAD for Windows; not available in AutoCAD LT for Windows.

Note:
 The
entmakex
 function doesn't support the creation of
AcDbXrecord
 (Xrecord) objects in AutoCAD LT.

## Examples

The examples that follow create objects and add them to the named object dictionary.

Create a dictionary entry list:

```lisp
(setq dictionary (list '(0 . "DICTIONARY") '(100 . "AcDbDictionary")))

((0 . "DICTIONARY") (100 . "AcDbDictionary"))
```

Create a dictionary object using the `entmakex`  function:

```lisp
(setq xname (entmakex dictionary))
(setq xname (entmakex dictionary))

<Entity name: 1d98950>
```

Add the dictionary to the named object dictionary:

```lisp
(setq newdict (dictadd (namedobjdict) "MY_WAY_COOL_DICTIONARY" xname))

<Entity name: 1d98950>
```

Create an Xrecord list:

```lisp
(setq datalist (append (list '(0 . "XRECORD")
                                 '(100 . "AcDbXrecord"))
                                 '((1 . "This is my data") (10 1. 2. 3.) (70 . 33))))

((0 . "XRECORD") (100 . "AcDbXrecord") (1 . "This is my data") (10 1.0 2.0 3.0) (70 . 33))
```

Make an Xrecord object:

```lisp
(setq xname (entmakex datalist))

<Entity name: 1d98958>
```

Add the Xrecord object to the dictionary:

```lisp
(dictadd newdict "DATA_RECORD_1" xname)

<Entity name: 1d98958>
```
