---
title: Planning for Multiple Reactors
guid: "GUID-2DEA0D9C-D877-4584-8DCA-C557E92A6D39"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-2DEA0D9C-D877-4584-8DCA-C557E92A6D39.htm"
generated: "2025-11-28T19:07:03.973929Z"
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

# Planning for Multiple Reactors

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-2DEA0D9C-D877-4584-8DCA-C557E92A6D39.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-2DEA0D9C-D877-4584-8DCA-C557E92A6D39.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Users may have several garden paths on the screen, and may be erasing more than one. You need to plan for this possibility.

The reactor associated with an entity is an object reactor. If there are several entities in the drawing, there may also be several object reactors, one for each entity. A specific editing event, such as the `erase`  command, can trigger many callbacks, depending on how many of the selected entities have reactors attached. Editor reactors, on the other hand, are singular in nature. Your application should only attach a single `:vlr-commandEnded`  event reactor.

The event sequence for both modifications—changing a vertex location and erasing a polyline—ends up with actions that need to be performed within the `gp:command-ended`  function. Determine which set of actions to perform for each condition. The following pseudo-code outlines the logic:

```lisp
Defun gp:command-ended (2nd version)
  Retrieve the pointer to the polyline (from a global variable)
      Conditional:
        If the polyline has been modified then:
           Erase the tiles
           Get information on the previous polyline vertex locations
           Get information on the new polyline vertex locations
           Redefine the polyline (straighten it up)
           Redraw the tiles
        End conditional expression
        If the polyline has been erased then:
           Erase the tiles
        End conditional expression
      End Conditional
End function
```
