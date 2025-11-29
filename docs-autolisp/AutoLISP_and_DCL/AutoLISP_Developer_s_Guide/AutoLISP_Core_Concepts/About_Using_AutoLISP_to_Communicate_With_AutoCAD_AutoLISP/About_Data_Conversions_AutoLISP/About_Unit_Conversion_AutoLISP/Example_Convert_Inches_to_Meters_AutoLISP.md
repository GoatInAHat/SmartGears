---
title: "Example: Convert Inches to Meters (AutoLISP)"
guid: "GUID-A42448EB-3E46-4902-9980-255DBA1EF9CB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A42448EB-3E46-4902-9980-255DBA1EF9CB.htm"
generated: "2025-11-28T19:06:09.297227Z"
description: "This example demonstrates how a user-specified distance in inches can be converted to meters with the cvunit function."
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

# Example: Convert Inches to Meters (AutoLISP)

> This example demonstrates how a user-specified distance in inches can be converted to meters with the cvunit function.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A42448EB-3E46-4902-9980-255DBA1EF9CB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A42448EB-3E46-4902-9980-255DBA1EF9CB.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

```lisp
(defun C:I2M ( / eng_len metric_len eng metric)
  (princ "\nConverting inches to meters. ")
  (setq eng_len
  (getdist "\nEnter a distance in inches: "))
  (setq metric_len (cvunit eng_len "inches" "meters"))
  (setq eng (rtos eng_len 2 4)
           metric (rtos metric_len 2 4))
  (princ
    (strcat "\n\t" eng " inches = " metric " meters."))
 (princ)
)
```
