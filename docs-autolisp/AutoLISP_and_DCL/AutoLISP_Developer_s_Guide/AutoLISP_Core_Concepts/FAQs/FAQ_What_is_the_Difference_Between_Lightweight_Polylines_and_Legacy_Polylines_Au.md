---
title: "FAQ: What is the Difference Between Lightweight Polylines and \"Legacy\" Polylines? (AutoLISP)"
guid: "GUID-0A3004D1-1BF6-468A-9F69-4D0BA88857F2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0A3004D1-1BF6-468A-9F69-4D0BA88857F2.htm"
generated: "2025-11-28T19:06:15.847903Z"
description: A lightweight polyline (LWPOLYLINE) is defined in the drawing database as a single graphic entity unlike the "legacy" polyline (POLYLINE), which is defined as a group of subentities. A "legacy" polyline can be either a 2D or 3D polyline, while a lightweight polyline can only be a 2D polyline.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 12/08/2024
topic_subtype:
  - autolisp
---

# FAQ: What is the Difference Between Lightweight Polylines and "Legacy" Polylines? (AutoLISP)

> A lightweight polyline (LWPOLYLINE) is defined in the drawing database as a single graphic entity unlike the "legacy" polyline (POLYLINE), which is defined as a group of subentities. A "legacy" polyline can be either a 2D or 3D polyline, while a lightweight polyline can only be a 2D polyline.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0A3004D1-1BF6-468A-9F69-4D0BA88857F2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0A3004D1-1BF6-468A-9F69-4D0BA88857F2.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 12/08/2024

Lightweight polylines display faster and consume less disk space and RAM than "legacy" 2D polylines. As of AutoCAD Release 14 and later, 3D polylines are always created as "legacy" polyline entities, and 2D polylines are created as lightweight polylines, unless they are curved or fitted with the AutoCAD PEDIT command. When a drawing from an earlier release is opened in AutoCAD Release 14 or a later release, all 2D polylines are convert to lightweight polylines automatically, unless they have been curved or fitted or contain extended data (xdata).
