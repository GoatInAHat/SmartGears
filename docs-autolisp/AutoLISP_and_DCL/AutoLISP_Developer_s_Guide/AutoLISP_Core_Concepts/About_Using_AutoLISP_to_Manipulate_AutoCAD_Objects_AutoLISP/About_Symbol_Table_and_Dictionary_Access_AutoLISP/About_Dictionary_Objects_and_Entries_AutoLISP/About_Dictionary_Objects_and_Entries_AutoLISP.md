---
title: About Dictionary Objects and Entries (AutoLISP)
guid: "GUID-24E52678-513E-4322-8070-B23C8945DC3D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-24E52678-513E-4322-8070-B23C8945DC3D.htm"
generated: "2025-11-28T19:06:15.532195Z"
description: A dictionary is a container object, similar to symbol tables.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Dictionary Objects and Entries (AutoLISP)

> A dictionary is a container object, similar to symbol tables.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-24E52678-513E-4322-8070-B23C8945DC3D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-24E52678-513E-4322-8070-B23C8945DC3D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Dictionaries are stored in the drawing’s named object dictionary, which is the root of all non-graphical objects in a drawing, or as an extension dictionary attached to an object. Each drawing can contain different dictionaries, so a routine should not expect that a specific dictionary might exist in a drawing. The dictionaries in a drawing’s named object dictionary can be accessed using `namedobjdict`, which returns an entity name. Use `entget`  to access the entity list that represents all of the dictionaries in the drawing.

The following rules apply to dictionary objects:

- Dictionary objects can be examined with
  entget
   and their xdata modified with
  entmod
  . Their entries cannot be altered with
  entmod
  . All access to their entries are made through the
  dictsearch
   and
  dictnext
   functions.
- Dictionary entry contents cannot be modified through
  entmod
  , although xdata can be modified.
- Dictionary entries that begin with ACAD* cannot be renamed.

Dictionary entries can be queried with the `dictsearch`  and `dictnext`  functions. Each dictionary entry consists of a text name key plus a hard ownership handle reference to the entry object. Dictionary entries may be removed by directly passing entry object names to the `entdel`  function. The text name key uses the same syntax and valid characters as symbol table names. A key name can be changed using the `dictrename`  function.

The following example code lists each of the dictionaries in the drawing’s named object dictionary and their entries:

```lisp
(defun c:ListDictionaries ( / ed ed1)
  (prompt "\nDictionaries in current drawing: ")
  (foreach ed (entget (namedobjdict))
    (progn
      (cond ((= (car ed) 3)
        (prompt (strcat "\n" (cdr ed))))
            ((= (car ed) 350)
        (progn
          (foreach ed1 (entget (cdr ed))
            (if (= (car ed1) 3)
              (prompt (strcat "\n  " (cdr ed1)))
            )
          )
        ))
      )
    )
  )
 (princ)
)
```

The following is an example of the output you might see in Text History window after `c:ListDictionaries`  is executed.

```lisp
Dictionaries in current drawing:
ACAD_CIP_PREVIOUS_PRODUCT_INFO
ACAD_COLOR
ACAD_DETAILVIEWSTYLE
  Imperial24
ACAD_GROUP
ACAD_LAYOUT
  Layout1
  Layout2
  Model
ACAD_MATERIAL
  ByBlock
  ByLayer
  Global
ACAD_MLEADERSTYLE
  Annotative
  Standard
ACAD_MLINESTYLE
  Standard
ACAD_PLOTSETTINGS
ACAD_PLOTSTYLENAME
  Normal
ACAD_SCALELIST
  A0
  A1
  A2
  A3
  A4
  A5
  A6
  A7
  A8
  A9
  B0
  B1
  B2
  B3
  B4
  B5
  B6
  B7
  B8
  B9
  C0
  C1
  C2
  C3
  C4
  C5
  C6
  C7
  C8
  C9
  D0
  D1
  D2
ACAD_SECTIONVIEWSTYLE
  Imperial24
ACAD_TABLESTYLE
  Standard
ACAD_VISUALSTYLE
  2dWireframe
  Basic
  Brighten
  ColorChange
  Conceptual
  Dim
  EdgeColorOff
  Facepattern
  Flat
  FlatWithEdges
  Gouraud
  GouraudWithEdges
  Hidden
  JitterOff
  Linepattern
  OverhangOff
  Realistic
  Shaded
  Shaded with edges
  Shades of Gray
  Sketchy
  Thicken
  Wireframe
  X-Ray
AcDbVariableDictionary
  CANNOSCALE
  CMLEADERSTYLE
  CTABLESTYLE
  CVIEWDETAILSTYLE
  CVIEWSECTIONSTYLE
  DIMASSOC
  HIDETEXT
  LAYEREVAL
  LAYERNOTIFY
```
