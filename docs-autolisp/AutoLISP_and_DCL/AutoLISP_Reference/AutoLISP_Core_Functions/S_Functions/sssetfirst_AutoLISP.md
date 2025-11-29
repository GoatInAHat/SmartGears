---
title: sssetfirst (AutoLISP)
guid: "GUID-4076CD9D-1C66-4C73-ACC0-3A6CD0B0D2D8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4076CD9D-1C66-4C73-ACC0-3A6CD0B0D2D8.htm"
generated: "2025-11-28T19:06:42.566866Z"
description: Sets which objects are selected and gripped
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

# sssetfirst (AutoLISP)

> Sets which objects are selected and gripped

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4076CD9D-1C66-4C73-ACC0-3A6CD0B0D2D8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4076CD9D-1C66-4C73-ACC0-3A6CD0B0D2D8.htm)
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
(sssetfirst
gripset [pickset]
)
```

- ***gripset*:** **Type:**  nil  AutoCAD no longer supports grips on unselected objects, so this argument is ignored. However, if *gripset*  is `nil`  and no *pickset*  is specified, `sssetfirst`  turns off the grip handles and selections it previously turned on.
- ***pickset*:** **Type:**  Pickset (selection set)  A selection set to be selected.

## Return Values

**Type:**  List

The selection set or sets specified.

## Remarks

The *gripset*  argument is ignored; the selection set of objects specified by *pickset*  are selected and gripped.

You are responsible for creating a valid selection set. For example, you may need to verify that a background paper space viewport (DXF group code 69) is not included in the selection set. You may also need to ensure that selected objects belong to the current layout, as in the following code:

```lisp
(setq ss (ssget (list (cons 410 (getvar "ctab")))))
```

## Examples

First, draw a square and build three selection sets. Begin by drawing side 1 and creating a selection set to include the line drawn:

```lisp
(entmake (list (cons 0 "line") '(10 0.0 0.0 0.0)'(11 0.0 10.0 0.0)))

((0 . "line") (10 0.0 0.0 0.0) (11 0.0 10.0 0.0))

(setq pickset1 (ssget "_l"))

<Selection set: a5>
```

Variable `pickset1`  points to the selection set created.

Draw side 2 and add it to the `pickset1`  selection set:

```lisp
(entmake (list (cons 0 "line") '(10 0.0 10.0 0.0)'(11 10.0 10.0 0.0)))

((0 . "line") (10 0.0 10.0 0.0) (11 10.0 10.0 0.0))

(ssadd (entlast) pickset1)

<Selection set: a5>
```

Create another selection set to include only side 2:

```lisp
(setq 2onlyset (ssget "_l"))

<Selection set: a8>
```

Draw side 3 and add it to the `pickset1`  selection set:

```lisp
(entmake (list (cons 0 "line") '(10 10.0 10.0 0.0)'(11 10.0 0.0 0.0)))

((0 . "line") (10 10.0 10.0 0.0) (11 10.0 0.0 0.0))

(ssadd (entlast) pickset1)

<Selection set: a5>
```

Create another selection and include side 3 in the selection set:

```lisp
(setq pickset2 (ssget "_l"))

<Selection set: ab>
```

Variable `pickset2`  points to the new selection set.

Draw side 4 and add it to the `pickset1`  and `pickset2`  selection sets:

```lisp
(entmake (list (cons 0 "line") '(10 10.0 0.0 0.0)'(11 0.0 0.0 0.0)))

((0 . "line") (10 10.0 0.0 0.0) (11 0.0 0.0 0.0))

(ssadd (entlast) pickset1)

<Selection set: a5>

(ssadd (entlast) pickset2)

<Selection set: ab>
```

At this point, `pickset1`  contains sides 1-4, `pickset2`  contains sides 3 and 4, and `2onlyset`  contains only side 2.

Turn grip handles on and select all objects in `pickset1`:

```lisp
(sssetfirst nil pickset1)

(nil <Selection set: a5>)
```

Turn grip handles on and select all objects in `pickset2`:

```lisp
(sssetfirst nil pickset2)

(nil <Selection set: ab>)
```

Turn grip handles on and select all objects in `2onlyset`:

```lisp
(sssetfirst nil 2onlyset)

(nil <Selection set: a8>)
```

Each `sssetfirst`  call replaces the gripped and selected selection set from the previous `sssetfirst`  call.

Note:
 Do
not
 call
sssetfirst
 when AutoCAD is in the middle of executing a command.
