---
title: Memory Management Functions Reference (AutoLISP)
guid: "GUID-0EA35DD5-0C92-4AF8-B3AE-EFA35A5FDF55"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0EA35DD5-0C92-4AF8-B3AE-EFA35A5FDF55.htm"
generated: "2025-11-28T19:06:18.422175Z"
topic_type: concept
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
---

# Memory Management Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0EA35DD5-0C92-4AF8-B3AE-EFA35A5FDF55.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0EA35DD5-0C92-4AF8-B3AE-EFA35A5FDF55.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP memory management functions.

| Memory management functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(alloc *int*)](../A_Functions/alloc_AutoLISP.md) | Sets the segment size to a given number of nodes | ✓ | ✓ | ✓ | -- | ✓ |
| [(expand *number*)](../E_Functions/expand_AutoLISP.md) | Allocates node space by requesting a specified number of segments | ✓ | ✓ | ✓ | -- | ✓ |
| [(gc)](../G_Functions/gc_AutoLISP.md) | Forces a garbage collection, which frees up unused memory | ✓ | ✓ | ✓ | -- | ✓ |
| [(mem)](../M_Functions/mem_AutoLISP.md) | Displays the current state of memory in AutoLISP | ✓ | ✓ | ✓ | -- | ✓ |
