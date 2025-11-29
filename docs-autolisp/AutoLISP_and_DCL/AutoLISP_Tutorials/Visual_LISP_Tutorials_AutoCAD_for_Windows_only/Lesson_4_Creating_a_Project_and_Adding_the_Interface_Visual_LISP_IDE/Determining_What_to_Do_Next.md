---
title: Determining What to Do Next
guid: "GUID-188FDF62-3036-4446-B0F4-11CBFFB5EB07"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-188FDF62-3036-4446-B0F4-11CBFFB5EB07.htm"
generated: "2025-11-28T19:07:01.212443Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 13/01/2021
topic_subtype:
  - autolisp
---

# Determining What to Do Next

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-188FDF62-3036-4446-B0F4-11CBFFB5EB07.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-188FDF62-3036-4446-B0F4-11CBFFB5EB07.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 13/01/2021

If the user clicked OK, you must build a list containing the values set by the user's interaction with the dialog. This list is what `gp:getDialogInput`  will return to its calling function. If the user clicked Cancel, the function returns `nil`:

```lisp
(if UserClick          ; User clicked Ok
  ;; Build the resulting data
  (progn
    (setq Result (list
        (cons 42 tileRad)
        (cons 43 TileSpace)
        (cons 3 objectCreateMethod)
        (cons 4 plineStyle)
        )
     )
  )
)
```
