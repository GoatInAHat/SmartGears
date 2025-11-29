---
title: Storing Data With a Reactor
guid: "GUID-831137D8-85DB-4CC4-A069-A8CBA0BD7862"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-831137D8-85DB-4CC4-A069-A8CBA0BD7862.htm"
generated: "2025-11-28T19:07:04.165362Z"
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

# Storing Data With a Reactor

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-831137D8-85DB-4CC4-A069-A8CBA0BD7862.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-831137D8-85DB-4CC4-A069-A8CBA0BD7862.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You can solve the problem of saving the original configuration by taking advantage of another feature of Visual LISP reactors—the ability to store data within a reactor. When the user first draws a path boundary, you attach a reactor to the boundary, along with the data you need to save. This entails modifying your main program function, `C:GPath`, as follows:

```lisp
Defun C:GPath
   Do everything that is already done in the garden path
   (and don't break anything)
   Attach an object reactor to the polyline using these parameters:
      A pointer to the polyline just drawn,
      A list of data that you want the reactor to record,
      A list of the specific polyline object events to be tracked,
      along with the LISP callback functions to be invoked
   End of the object reactor setup
   Attach editor reactor to the drawing editor using the
   following parameters:
      Any data you want attached to the reactor (in this case, none)
      A list of the specific editor reactor events to be tracked,
      along with the LISP callback functions to be invoked
   End of the editor reactor setup
End function
```
