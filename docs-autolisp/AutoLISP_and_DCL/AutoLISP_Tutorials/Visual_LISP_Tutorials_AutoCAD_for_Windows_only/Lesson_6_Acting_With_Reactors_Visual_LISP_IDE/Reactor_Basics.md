---
title: Reactor Basics
guid: "GUID-4725B475-15DE-4CBC-BD0B-E25B137601C4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-4725B475-15DE-4CBC-BD0B-E25B137601C4.htm"
generated: "2025-11-28T19:07:03.595634Z"
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

# Reactor Basics

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-4725B475-15DE-4CBC-BD0B-E25B137601C4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-4725B475-15DE-4CBC-BD0B-E25B137601C4.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

A reactor is an object you attach to the drawing editor, or to specific entities within a drawing. Extending the metaphor of the pager, the reactor object is an automatic dialer that knows how to call your pager when something significant happens. The pager within your application is an AutoLISP ®  function called by the reactor; such a function is known as a *callback function*.

Note:
 The complexity of the application code and the level of expertise required for these final two lessons is much higher than Lessons 1 through 5. There is a great deal of information presented, but it is not all explained at the same level of detail as in the previous lessons. If you are a beginner, do not worry if you don't get it the first time. Consider this just a first taste of some of the very powerful but more technically difficult features of Visual LISP
®
.
