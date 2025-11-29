---
title: Ensuring That ActiveX Is Loaded
guid: "GUID-E6D5173F-58D0-4CD3-B666-D5B727C1499D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E6D5173F-58D0-4CD3-B666-D5B727C1499D.htm"
generated: "2025-11-28T19:06:58.952110Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 25/02/2021
topic_subtype:
  - autolisp
---

# Ensuring That ActiveX Is Loaded

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E6D5173F-58D0-4CD3-B666-D5B727C1499D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E6D5173F-58D0-4CD3-B666-D5B727C1499D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 25/02/2021

ActiveX functionality is not automatically enabled when you start AutoCAD or Visual LISP, so your programs must ensure that ActiveX is loaded. The following function call accomplishes this:

```lisp
(vl-load-com)
```

If ActiveX support is not yet available, executing `vl-load-com`  initializes the AutoLISP ActiveX environment. If ActiveX is already loaded, `vl-load-com`  does nothing.
