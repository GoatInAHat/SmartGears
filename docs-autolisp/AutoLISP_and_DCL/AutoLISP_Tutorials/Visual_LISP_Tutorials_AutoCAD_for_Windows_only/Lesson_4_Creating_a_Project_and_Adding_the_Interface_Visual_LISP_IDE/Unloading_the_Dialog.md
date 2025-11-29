---
title: Unloading the Dialog
guid: "GUID-8BECBD3B-9260-4A3B-A10F-A26DEE90EC14"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8BECBD3B-9260-4A3B-A10F-A26DEE90EC14.htm"
generated: "2025-11-28T19:07:01.112121Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Unloading the Dialog

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8BECBD3B-9260-4A3B-A10F-A26DEE90EC14.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8BECBD3B-9260-4A3B-A10F-A26DEE90EC14.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

When a user clicks OK or Cancel, you need to unload the dialog. Like `start_dialog`  and `unload_dialog`  is another simple function.

```lisp
(unload_dialog dcl_id)
```
