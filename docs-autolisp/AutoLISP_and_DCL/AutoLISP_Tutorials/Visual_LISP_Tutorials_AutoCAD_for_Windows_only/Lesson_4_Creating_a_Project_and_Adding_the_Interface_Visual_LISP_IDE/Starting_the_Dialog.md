---
title: Starting the Dialog
guid: "GUID-9D72BBBB-090F-4845-8D6B-57AB556ED420"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9D72BBBB-090F-4845-8D6B-57AB556ED420.htm"
generated: "2025-11-28T19:07:01.032372Z"
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

# Starting the Dialog

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9D72BBBB-090F-4845-8D6B-57AB556ED420.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9D72BBBB-090F-4845-8D6B-57AB556ED420.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The `start_dialog`  function displays a dialog box and accepts user input. The `start_dialog`  function requires no arguments.

```lisp
(start_dialog)
```

Control passes to users when you issue `start_dialog`. Users can make choices within the dialog box, until they click OK or Cancel.
