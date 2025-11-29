---
title: "FAQ: What Symbol Table Entries Cannot Be Renamed or Modified? (AutoLISP)"
guid: "GUID-726335A9-C1D4-445B-B395-51098A03F7FD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-726335A9-C1D4-445B-B395-51098A03F7FD.htm"
generated: "2025-11-28T19:06:15.929635Z"
description: Most entries in the symbol tables can be renamed or modified with a few exceptions.
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

# FAQ: What Symbol Table Entries Cannot Be Renamed or Modified? (AutoLISP)

> Most entries in the symbol tables can be renamed or modified with a few exceptions.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-726335A9-C1D4-445B-B395-51098A03F7FD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-726335A9-C1D4-445B-B395-51098A03F7FD.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following table shows which symbol table entries cannot be modified or renamed, except that most LAYER symbol table entries can be renamed and xdata can be modified on all symbol table entries.

| Symbol table entries that cannot be modified or renamed |  |
| --- | --- |
| Table | Entry name |
| VPORT | *ACTIVE |
| LINETYPE | CONTINUOUS |
| LAYER | Entries cannot be modified, except for xdata, but renaming is allowed |

The following entries cannot be renamed, but are otherwise modifiable:

| Symbol table entries that cannot be renamed |  |
| --- | --- |
| Table | Entry name |
| STYLE | STANDARD |
| DIMSTYLE | STANDARD |
| BLOCKS | *MODEL_SPACE or *PAPER_SPACE |
| APPID | No entries can be renamed |
