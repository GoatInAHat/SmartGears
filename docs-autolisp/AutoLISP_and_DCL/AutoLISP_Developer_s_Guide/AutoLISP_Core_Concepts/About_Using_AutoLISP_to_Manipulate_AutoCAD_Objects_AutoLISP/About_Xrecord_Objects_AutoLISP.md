---
title: About Xrecord Objects (AutoLISP)
guid: "GUID-FA5F2E08-24F5-4947-A470-6CA84E404F2A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-FA5F2E08-24F5-4947-A470-6CA84E404F2A.htm"
generated: "2025-11-28T19:06:15.120296Z"
description: Xrecord objects are used to store and manage arbitrary data.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# About Xrecord Objects (AutoLISP)

> Xrecord objects are used to store and manage arbitrary data.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-FA5F2E08-24F5-4947-A470-6CA84E404F2A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-FA5F2E08-24F5-4947-A470-6CA84E404F2A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

They are composed of DXF group codes with normal object group codes (that is, non-xdata group codes), ranging from 1 through 369 for supported ranges. These objects are similar in concept to xdata but are not limited by size or order.

The following code examples create and list xrecord data in a custom dictionary named XRECLIST.

```lisp
(defun C:MAKEXRECORD( / xrec xname )
  ; create the xrecord's data list.
  (setq xrec '((0 . "XRECORD")(100 . "AcDbXrecord")
    (1 . "This is a test xrecord list")
    (10 1.0 2.0 0.0) (40 . 3.14159) (50 . 3.14159)
    (62 . 1) (70 . 180))
  )

  ; use entmakex to create the xrecord with no owner.
  (setq xname (entmakex xrec))

  ; add the new xrecord to the named object dictionary.
  (dictadd (namedobjdict) "XRECLIST" xname)
 (princ)
)

(defun C:LISTXRECORD ( / xlist )
  ; find the xrecord in the named object dictionary.
  (setq xlist (dictsearch (namedobjdict) "XRECLIST"))

  ; print out the xrecord's data list.
  (princ xlist)
 (princ)
)
```

The results of the LISTXRECORD command will look similar to the following:

```lisp
((-1 . <Entity name: 7ffffb05ee0>) (0 . XRECORD) (5 . 1E6) (102 . {ACAD_REACTORS) (330 . <Entity name: 7ffffb038c0>) (102 . }) (330 . <Entity name: 7ffffb038c0>) (100 . AcDbXrecord) (280 . 1) (1 . This is a test xrecord list) (10 1.0 2.0 0.0) (40 . 3.14159) (50 . 3.14159) (62 . 1) (70 . 180))
```
