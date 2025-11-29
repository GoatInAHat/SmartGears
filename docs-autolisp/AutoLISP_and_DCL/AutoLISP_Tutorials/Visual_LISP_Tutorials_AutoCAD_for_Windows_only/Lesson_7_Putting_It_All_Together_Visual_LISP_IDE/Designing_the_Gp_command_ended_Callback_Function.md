---
title: "Designing the Gp:command-ended Callback Function"
guid: "GUID-CF559004-EC0C-4600-A344-C53D85741590"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-CF559004-EC0C-4600-A344-C53D85741590.htm"
generated: "2025-11-28T19:07:05.619278Z"
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

# Designing the Gp:command-ended Callback Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-CF559004-EC0C-4600-A344-C53D85741590.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-CF559004-EC0C-4600-A344-C53D85741590.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The `gp:command-ended`  editor reactor callback function is where most action takes place. Until this function is called, the garden path border polylines are “ *open for modify*;” that is, users may still be manipulating the borders in AutoCAD. Within the reactor sequence, you have to wait until AutoCAD has done its part of the work before you are free to do what you want to do.

The following pseudo-code illustrates the logic of the `gp:command-ended`  function:

```lisp
Determine the condition of the polyline.
  CONDITION 1 - POLYLINE ERASED (Erase command)
    Erase the tiles.
  CONDITION 2 - LOST ASSOCIATIVITY (Move, Rotate, etc.)
    Erase the tiles.
  CONDITION 3 - GRIP_STRETCH - REDRAW AND RE-TILE
    Erase the tiles.
    Get the current boundary data from the polyline.
    If it is a lightweight polyline,
       Process boundary data as 2D
    Else
       Process boundary data as 3D
    End if
  Redefine the polyline border (pass in parameters of the current
         boundary configuration, as well as the old).
  Get the new boundary information and put it into the format
         required for setting back into the polyline entity.
  Regenerate the polyline.
  Redraw the tiles (force ActiveX drawing).
  Put the revised boundary information back into the reactor
         named in *reactorsToChange*.
End function
```

The pseudo-code is relatively straightforward, but there are several important details buried in the pseudo-code, and they are things you would not be expected to know at this point.
