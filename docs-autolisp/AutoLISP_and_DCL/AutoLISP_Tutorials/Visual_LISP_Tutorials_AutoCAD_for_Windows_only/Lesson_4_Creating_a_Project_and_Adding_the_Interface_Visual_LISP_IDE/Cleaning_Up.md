---
title: Cleaning Up
guid: "GUID-506738AF-7068-4FA4-A0F8-CD93FD94272E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-506738AF-7068-4FA4-A0F8-CD93FD94272E.htm"
generated: "2025-11-28T19:07:01.680519Z"
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

# Cleaning Up

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-506738AF-7068-4FA4-A0F8-CD93FD94272E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-506738AF-7068-4FA4-A0F8-CD93FD94272E.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

If you have not done so already, delete the following code from the `C:GPath`  function in *gpmain.lsp*:

```lisp
(princ "\nThe gp:drawOutline function returned <")
(princ PolylineName)
(princ ">")
(alert "Congratulations - your program is complete!")
```

You had been using this code as a placeholder, but now that `gp:drawOutline`  is functioning, you no longer need it.
