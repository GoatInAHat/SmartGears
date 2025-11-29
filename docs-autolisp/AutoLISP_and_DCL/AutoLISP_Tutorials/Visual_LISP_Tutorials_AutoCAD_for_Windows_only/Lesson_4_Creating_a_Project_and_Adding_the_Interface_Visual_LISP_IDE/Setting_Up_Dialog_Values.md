---
title: Setting Up Dialog Values
guid: "GUID-51A56BDF-D579-4C5A-8B84-8522D7C26F16"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-51A56BDF-D579-4C5A-8B84-8522D7C26F16.htm"
generated: "2025-11-28T19:07:00.649396Z"
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

# Setting Up Dialog Values

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-51A56BDF-D579-4C5A-8B84-8522D7C26F16.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-51A56BDF-D579-4C5A-8B84-8522D7C26F16.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

When you run the complete garden path application, notice that the dialog box always starts up with ActiveX as the default object creation method and Lightweight as the polyline style. Something more interesting occurs with the default tile size—the values change depending on the width of the path. The following code fragment shows how to set up the default values to be displayed in the dialog box:

```lisp
(setq objectCreateMethod "ACTIVEX"
      plineStyle "LIGHT"
      tilerad (/ pathWidth 15.0)
      tilespace (/ tilerad 5.0)
      dialogLoaded T
      dialogShow T
) ;_ end of setq
```

For the moment, don't worry about what purpose the `dialogLoaded`  and `dialogShow`  variables serve. This becomes apparent in the next two sections.
