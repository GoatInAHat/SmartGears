---
title: Planning the Callback Functions
guid: "GUID-BB44D330-083B-45F7-B1FA-06B47C2465D7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BB44D330-083B-45F7-B1FA-06B47C2465D7.htm"
generated: "2025-11-28T19:07:03.904662Z"
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

# Planning the Callback Functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BB44D330-083B-45F7-B1FA-06B47C2465D7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BB44D330-083B-45F7-B1FA-06B47C2465D7.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

For each reactor event, you must plan the function that will be invoked when the event occurs. The following pseudo-code outlines the logical sequence of events that should occur when users drag one of the polyline vertices to a new location:

```lisp
Defun gp:outline-changed
       Erase the tiles.
       Determine how the boundary changed.
       Straighten up the boundary.
       Redraw new tiles.
End function
```

There is a complication, though. When the user begins dragging the outline of a polyline vertex, AutoCAD notifies your application by issuing a `:vlr-modified`  event. However, at this point the user has just begun dragging one of the polyline vertices. If you immediately invoke the `gp:outline-changed`  function, you will interrupt the action that the user is in the midst of. You would not know where the new vertex location will be, because the user has not yet selected its position. And finally, AutoCAD will not allow your function to modify the polyline object while the user is still dragging it. AutoCAD has the polyline object open for modification, and leaves it open until the user finishes repositioning the object.

You need to change your approach. Here is the updated logic:

```lisp
When the user begins repositioning a polyline vertex,
  Invoke the gp:outline-changed function
  Defun gp:outline-changed
    Set a global variable that stores a pointer to the polyline
    being modified by the user
  End function

When the command completes,
  Invoke the gp:command-ended function
  Defun gp:command-ended
           Erase the tiles
           Get information on the previous polyline vertex locations
           Get information on the new polyline vertex locations
           Redefine the polyline (straighten it up)
           Redraw the tiles
  End function
```

When a user completes modifying a path outline, AutoCAD notifies your application by issuing a `:vlr-commandEnded`  event, if you have established an editor reactor.

The use of a global variable to identify the polyline the user changed is necessary because there is no continuity between the `gp:outline-changed`  and `gp:command-ended`  functions. In your application, both functions are invoked and executed independently of one another. The global variable stores important information set up in one function and accessed in the other.

Now consider what to do if the user erases the garden path boundary. The ultimate objective is to erase all the tiles. The following pseudo-code outlines the logic:

```lisp
When the user begins to erase the boundary,
  Invoke the gp:outline-erased function
  Defun gp:outline-erased
     Set a global variable that stores a pointer to the reactor
     attached to the polyline currently being erased
  End function

When the erase is completed,
  Invoke the gp:command-ended function
  Defun gp:command-ended
     Erase the tiles that belonged to the now-deleted polyline
  End function
```
