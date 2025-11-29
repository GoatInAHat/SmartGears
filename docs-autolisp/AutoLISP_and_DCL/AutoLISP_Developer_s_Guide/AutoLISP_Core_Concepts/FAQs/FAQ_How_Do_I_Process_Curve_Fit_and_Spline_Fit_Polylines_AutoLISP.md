---
title: "FAQ: How Do I Process Curve-Fit and Spline-Fit Polylines? (AutoLISP)"
guid: "GUID-AC65F02A-8AFA-416A-831A-4D7C04E2A06C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-AC65F02A-8AFA-416A-831A-4D7C04E2A06C.htm"
generated: "2025-11-28T19:06:16.006212Z"
description: A "legacy" polyline might contain vertices that were not created explicitly; these auxiliary vertices were inserted automatically by the AutoCAD PEDIT command's Fit and Spline options.
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

# FAQ: How Do I Process Curve-Fit and Spline-Fit Polylines? (AutoLISP)

> A "legacy" polyline might contain vertices that were not created explicitly; these auxiliary vertices were inserted automatically by the AutoCAD PEDIT command's Fit and Spline options.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-AC65F02A-8AFA-416A-831A-4D7C04E2A06C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-AC65F02A-8AFA-416A-831A-4D7C04E2A06C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 12/08/2024

You can safely ignore these additional vertices when stepping through a polyline with `entnext`  because changes to these vertices will be discarded the next time the AutoCAD PEDIT command is used to fit or convert the polyline to a spline. The group code 70 flags of a "legacy" polyline entity indicate whether the polyline has been curve-fit (bit value 2) or spline-fit (bit value 4). If neither bit is set, all the polyline's vertices are regular user-defined vertices.

However, if the curve-fit bit (2) is set, alternating vertices of the polyline have the bit value 1 set in their 70 group code to indicate that they were inserted by the curve-fitting process. If you use `entmod`  to move the vertices of such a polyline with the intent of refitting the curve by means of the AutoCAD PEDIT command, ignore these vertices.

Likewise, if the "legacy" polyline entity's spline-fit flag bit (bit 4) is set, an assortment of vertices will be found—some with flag bit 1 (inserted by curve fitting if the AutoCAD SPLINESEGS system variable was negative), some with bit value 8 (inserted by spline fitting), and all others with bit value 16 (spline frame-control point). Here again, if you use `entmod`  to move the vertices and you intend to refit the spline afterward, move only the control-point vertices.
