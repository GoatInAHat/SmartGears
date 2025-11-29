---
title: Wrapping Up Lesson 6
guid: "GUID-25A67B1B-F582-48CB-9762-F0197035A11A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-25A67B1B-F582-48CB-9762-F0197035A11A.htm"
generated: "2025-11-28T19:07:04.965177Z"
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
tags:
  - programs. See code; functions
  - crashing AutoCAD, reactors and
  - reactors crashing AutoCAD and
  - reactors transient versus persistent
  - boundaries modifying reactors and
  - changing boundaries reactors and
  - erasing tiles and redrawing when boundaries change, reactors and
  - tiles (garden path) redrawing when boundary is modified
  - reference works. See resources
---

# Wrapping Up Lesson 6

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-25A67B1B-F582-48CB-9762-F0197035A11A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-25A67B1B-F582-48CB-9762-F0197035A11A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019
- Keywords: programs. See code; functions, crashing AutoCAD, reactors and, reactors crashing AutoCAD and, reactors transient versus persistent, boundaries modifying reactors and, changing boundaries reactors and, erasing tiles and redrawing when boundaries change, reactors and, tiles (garden path) redrawing when boundary is modified, reference works. See resources

In this lesson, you were introduced to reactors and how to implement them using Visual LISP. You designed a plan for adding reactors to the garden path application, and added some of the code your program will need to implement the plan.

Reactors can add a great deal of functionality to an application, but remember—the more powerful your programs can be, the harder they can crash.

Another thing to keep in mind is that the way your application is designed, the reactor functionality is not persistent from one drawing session to the next. If you save a drawing that contains a garden path hooked up to reactors, the reactors will not be there the next time you open the drawing.
