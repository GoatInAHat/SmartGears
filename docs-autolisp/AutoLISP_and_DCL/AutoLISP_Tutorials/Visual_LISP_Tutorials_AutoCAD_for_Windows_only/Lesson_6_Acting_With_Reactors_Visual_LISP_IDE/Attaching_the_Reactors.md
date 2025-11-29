---
title: Attaching the Reactors
guid: "GUID-F6B71F40-1430-42AB-A7F3-1AB3E492DA8A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F6B71F40-1430-42AB-A7F3-1AB3E492DA8A.htm"
generated: "2025-11-28T19:07:04.076672Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# Attaching the Reactors

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F6B71F40-1430-42AB-A7F3-1AB3E492DA8A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F6B71F40-1430-42AB-A7F3-1AB3E492DA8A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

The next step in planning a reactor-based application is to determine how and when to attach reactors. You need to attach two object reactors for the polyline border of any garden path (one to respond to a modification event, the other to respond to erasure), and one editor reactor to alert your application when users complete their modification to the polyline. Object reactors are attached to entities, while editor reactors are registered with AutoCAD.

There is another consideration to account for. To recalculate the polyline outline—return it to a rectilinear shape—after the user modifies it, you must know what the vertex configuration was before the modification. This information cannot be determined once the polyline has been modified. By that time you can only retrieve information on what the new configuration is. So how do you solve this? You could keep this information in a global variable, but there is a major problem with that idea. Users can draw as many garden paths as they want, and every new path would require a new global variable.
