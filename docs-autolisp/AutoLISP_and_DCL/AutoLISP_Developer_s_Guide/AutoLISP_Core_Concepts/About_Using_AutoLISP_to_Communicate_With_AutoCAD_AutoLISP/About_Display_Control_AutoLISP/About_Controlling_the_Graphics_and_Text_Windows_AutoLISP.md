---
title: About Controlling the Graphics and Text Windows (AutoLISP)
guid: "GUID-CC491170-1CBD-4AE0-824D-8F425580CED6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-CC491170-1CBD-4AE0-824D-8F425580CED6.htm"
generated: "2025-11-28T19:06:07.378329Z"
description: You can control the display of the graphics and text windows from an application.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 12/08/2024
topic_subtype:
  - autolisp
---

# About Controlling the Graphics and Text Windows (AutoLISP)

> You can control the display of the graphics and text windows from an application.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-CC491170-1CBD-4AE0-824D-8F425580CED6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-CC491170-1CBD-4AE0-824D-8F425580CED6.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 12/08/2024

Early releases of AutoCAD could be configured to use two different screens; one displayed the graphics screen while the other displayed the contents of the text window. In those early releases, AutoCAD could be installed using a single-screen. With single-screen AutoCAD installations, a call to `graphscr`  displayed the graphics window, and a call to `textscr`  displayed the text window. Using these functions was equivalent to toggling the Flip Screen function key. The function `textpage`  is equivalent to `textscr`.

In later and the most recent release, the text window is a floating window that can be independently displayed and resized instead of being a separate screen. A call to `graphscr`  hides the text window and ensures that the graphics window is displayed, while a call to `textscr`  displays the text window. If the text window is already displayed when `textscr`  is called, the window is moved to the foreground in front of the AutoCAD application window.

The `redraw`  function is similar to the AutoCAD REDRAW command, but provides more control over what is displayed. It not only redraws the entire graphics area, but can also specify a single object to be redrawn or undrawn (that is, blanked out). If the object is a complex object such as a "legacy" polyline or a block, `redraw`  can draw (or undraw) either the entire object or its header. The `redraw`  function can also highlight and unhighlight specified objects.
