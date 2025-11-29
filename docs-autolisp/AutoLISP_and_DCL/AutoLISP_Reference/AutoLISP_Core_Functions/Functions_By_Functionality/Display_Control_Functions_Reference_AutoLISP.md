---
title: Display Control Functions Reference (AutoLISP)
guid: "GUID-9C2C56A3-0BEF-45AB-A7FE-5825339DCB41"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C2C56A3-0BEF-45AB-A7FE-5825339DCB41.htm"
generated: "2025-11-28T19:06:16.717460Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# Display Control Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C2C56A3-0BEF-45AB-A7FE-5825339DCB41.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C2C56A3-0BEF-45AB-A7FE-5825339DCB41.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP display control functions.

| Display control functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(graphscr)](../G_Functions/graphscr_AutoLISP.md) | Displays the AutoCAD graphics screen | ✓ | ✓ | / - supported, but doesn't do anything | -- | / - supported, but doesn't do anything |
| [(grdraw *from to color [highlight]*)](../G_Functions/grdraw_AutoLISP.md) | Draws a vector between two points, in the current viewport | ✓ | ✓ | ✓ | -- | ✓ |
| [(grtext *[box text [highlight]]*)](../G_Functions/grtext_AutoLISP.md) | Writes text to the status line or to screen menu areas | ✓ | ✓ | ✓ | -- | ✓ |
| [(grvecs *vlist [trans]*)](../G_Functions/grvecs_AutoLISP.md) | Draws multiple vectors on the graphics screen | ✓ | ✓ | ✓ | -- | ✓ |
| [(menucmd *string*)](../M_Functions/menucmd_AutoLISP.md) | Issues menu commands, or sets and retrieves menu item status | ✓ | ✓ | / - supported, but limited | -- | / - supported, but limited |
| [(menugroup *groupname*)](../M_Functions/menugroup_AutoLISP.md) | Verifies that a menu group is loaded | ✓ | ✓ | / - supported, but always returns *groupname* | -- | / - supported, but always returns *groupname* |
| [(prin1 *[expr [file-desc]]*)](../P_Functions/prin1_AutoLISP.md) | Prints an expression to the command line or writes an expression to an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(princ *[expr [file-desc]]*)](../P_Functions/princ_AutoLISP.md) | Prints an expression to the command line, or writes an expression to an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(print *[expr [file-desc]]*)](../P_Functions/print_AutoLISP.md) | Prints an expression to the command line, or writes an expression to an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(prompt *msg*)](../P_Functions/prompt_AutoLISP.md) | Displays a string on your screen's prompt area | ✓ | ✓ | ✓ | -- | ✓ |
| [(redraw *[ename [mode]]*)](../R_Functions/redraw_AutoLISP.md) | Redraws the current viewport or a specified object (entity) in the current viewport | ✓ | ✓ | ✓ | -- | ✓ |
| [(terpri)](../T_Functions/terpri_AutoLISP.md) | Prints a newline to the Command line | ✓ | ✓ | ✓ | -- | ✓ |
| [(textpage)](../T_Functions/textpage_AutoLISP.md) | Switches from the graphics screen to the text screen | ✓ | ✓ | / - supported, but doesn't do anything | -- | / - supported, but doesn't do anything |
| [(textscr)](../T_Functions/textscr_AutoLISP.md) | Switches from the graphics screen to the text screen (like the AutoCAD Flip Screen function key) | ✓ | ✓ | / - supported, but doesn't do anything | -- | / - supported, but doesn't do anything |
| [(vports)](../V_Functions/vports_AutoLISP.md) | Returns a list of viewport descriptors for the current viewport configuration | ✓ | ✓ | ✓ | -- | ✓ |
