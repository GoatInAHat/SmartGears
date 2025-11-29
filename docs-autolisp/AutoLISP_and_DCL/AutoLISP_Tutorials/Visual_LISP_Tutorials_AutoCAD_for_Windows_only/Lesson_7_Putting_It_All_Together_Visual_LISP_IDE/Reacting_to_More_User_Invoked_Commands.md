---
title: "Reacting to More User-Invoked Commands"
guid: "GUID-6D1C9D68-5353-435E-9F8C-AFF3497ED2EE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-6D1C9D68-5353-435E-9F8C-AFF3497ED2EE.htm"
generated: "2025-11-28T19:07:05.282091Z"
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

# Reacting to More User-Invoked Commands

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-6D1C9D68-5353-435E-9F8C-AFF3497ED2EE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-6D1C9D68-5353-435E-9F8C-AFF3497ED2EE.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

When writing a reactor-based application, you need to handle any command that affects your objects in a significant way. One of your program design activities should be to review all possible AutoCAD editing commands and determine how your application should respond to each one. The format of the reactor-trace sheet shown near the end of Lesson 6 is very good for this purpose. Invoke the commands you expect your user to use, and write down the kind of behavior with which your application should respond. Other actions to plan for include

- Determine what to do when users issue UNDO and REDO commands.
- Determine what to do when users issue the OOPS command after erasing entities linked with reactors.

To prevent a very complex subject from becoming very, *very*  complex, the tutorial does not try to cover all the possibilities that should be covered, and the functionality within this lesson is kept to an absolute minimum.

Even though you won't be building in the complete functionality for these extra commands, examine what a few additional editing functions would require you to do:

-  If users stretch a polyline boundary (using the STRETCH command) several things should happen. It could be stretched in any direction, not just on the major or minor axis, so the boundary may end up in a very odd shape. In addition, you need to take into consideration how many vertices have been stretched. A situation where only one vertex is stretched will result in a polyline quite different from one in which two vertices are moved. In any case, the tiles must be erased and new positions recalculated once you determine the adjustments needed to the boundary.
-  If users move a polyline boundary, all the tiles should be erased, then redrawn in the new location. This is a fairly simple operation, because the polyline boundary did not change its size or shape.
- If users scale a polyline boundary, you need to make a decision. Should the tiles be scaled up as well, so that the path contains the same number of tiles? Or, should the tile size remain the same and the application add or remove tiles, depending on whether the polyline was scaled up or down?
- If users rotate a polyline boundary, all the tiles should be erased, then redrawn in the new orientation.

To begin, though, just plan for the following:

- Warn the user upon command-start that the selected edit command (such as
  stretch
  ,
  move
  , or
  rotate
  ) will have detrimental effects on a garden path.
- If the user proceeds, erase the tiles and do not redraw them.
-  Remove the reactors from the path outline.

Note:
 In addition to user-invoked AutoCAD commands, entities may also be modified or deleted through AutoLISP or ObjectARX
®
 applications. The example provided in the Garden Path tutorial does not cover programmatic manipulation of the garden path polyline boundary, such as through (
entdel
 <
polyline entity
>). In this case, the editor reactor events
:vlr-commandWillStart
 and
:vlr-commandEnded
 will not be triggered.
