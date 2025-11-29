---
title: Using the Command Function
guid: "GUID-52F378BE-4D44-419C-812A-199A19509C38"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-52F378BE-4D44-419C-812A-199A19509C38.htm"
generated: "2025-11-28T19:06:58.010708Z"
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

# Using the Command Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-52F378BE-4D44-419C-812A-199A19509C38.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-52F378BE-4D44-419C-812A-199A19509C38.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

When AutoLISP first appeared in AutoCAD, the only available means for entity creation was the `command`  function. This allows an AutoLISP programmer to code just about any command that can be executed from the AutoCAD Command prompt. This is reliable, but it is not as fast as ActiveX methods and does not provide the flexibility of `entmake`.
