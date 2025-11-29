---
title: Examining Reactor Behavior in Detail
guid: "GUID-51A2B285-5319-4BDD-B7C0-4C20D604A9CC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-51A2B285-5319-4BDD-B7C0-4C20D604A9CC.htm"
generated: "2025-11-28T19:07:04.798708Z"
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

# Examining Reactor Behavior in Detail

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-51A2B285-5319-4BDD-B7C0-4C20D604A9CC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-51A2B285-5319-4BDD-B7C0-4C20D604A9CC.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

With a stack of scrap paper, start tracing the reactor events within the application. Here is an example of the kinds of things you should track:

Draw ten garden paths, then track the following Command/Object combinations, selecting the polylines in succession:

- Erase/Polyline border (path 1)
- Erase/Circle within a polyline (path 2)
- Erase/Two polylines (paths 3 and 4)
- Move/Polyline border (path 5)
- Move/Circle within a polyline (path 6)
- Move/Two polylines and several circles (paths 7 and 8)
- Move Vertex (through grips)/Polyline border (path 9)
- Stretch/Polyline border (path 10)

This exercise will give you a good understanding of what is happening behind the scenes. At any time throughout Lesson 7 when the reactor functionality becomes confusing, refer to your “reactor-trace sheets.”
