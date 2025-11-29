---
title: Namespace Communication Functions Reference (AutoLISP)
guid: "GUID-F5DD5472-1695-4CA3-9110-E6008E87BB89"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F5DD5472-1695-4CA3-9110-E6008E87BB89.htm"
generated: "2025-11-28T19:06:18.494235Z"
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

# Namespace Communication Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F5DD5472-1695-4CA3-9110-E6008E87BB89.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F5DD5472-1695-4CA3-9110-E6008E87BB89.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The namespace communication functions consist of blackboard addressing and multi-document-loading functions.

| Namespace communication functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(vl-bb-ref *'variable*)](../V_Functions/vl_bb_ref_AutoLISP.md) | Returns the value of a variable from the blackboard namespace | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-bb-set *'variable value*)](../V_Functions/vl_bb_set_AutoLISP.md) | Sets the value of a variable in the blackboard namespace | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-load-all *"filename"*)](../V_Functions/vl_load_all_AutoLISP.md) | Loads a file into all open AutoCAD documents, and into any document subsequently opened during the current AutoCAD session | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-propagate *'variable*)](../V_Functions/vl_propagate_AutoLISP.md) | Copies the value of a variable into all open AutoCAD documents, and into any document subsequently opened during the current AutoCAD session | ✓ | ✓ | ✓ | -- | ✓ |
