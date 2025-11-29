---
title: About Management of Extended Data Memory Use (AutoLISP)
guid: "GUID-48A12BAA-E785-4E2B-B7A7-E3B976E75891"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-48A12BAA-E785-4E2B-B7A7-E3B976E75891.htm"
generated: "2025-11-28T19:06:14.914000Z"
description: Extended data is limited to 16K per entity.
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

# About Management of Extended Data Memory Use (AutoLISP)

> Extended data is limited to 16K per entity.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-48A12BAA-E785-4E2B-B7A7-E3B976E75891.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-48A12BAA-E785-4E2B-B7A7-E3B976E75891.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Because the xdata of an entity can be created and maintained by multiple applications, problems can result when the size of the xdata approaches its limit. The following functions can be used to help manage the memory that xdata occupies.

- xdsize
   - Returns the amount of memory (in bytes) that the xdata in a list occupies.
- xdroom
   - Returns the remaining number of free bytes that can still be appended to the entity.

The `xdsize`  function can be slow when reading a large extended data list, so it is not recommended that you call it frequently. A better approach is to use it (in conjunction with `xdroom`) in an error handler. If a call to `entmod`  fails, you can use `xdsize`  and `xdroom`  to find out whether the call failed because the entity did not have enough room for the xdata.
