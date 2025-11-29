---
title: Initializing the Default Dialog Values
guid: "GUID-A8F592EE-AA6E-4EB0-8D73-70B8F7C448D9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A8F592EE-AA6E-4EB0-8D73-70B8F7C448D9.htm"
generated: "2025-11-28T19:07:00.889340Z"
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

# Initializing the Default Dialog Values

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A8F592EE-AA6E-4EB0-8D73-70B8F7C448D9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A8F592EE-AA6E-4EB0-8D73-70B8F7C448D9.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

If everything worked successfully in loading the dialog, you are ready to start setting up the values that will be displayed to users. A successful load is indicated if the flag variables `dialogLoaded`  and `dialogShow`  are both `T`  (true).

Now set the initial values for the tile radius and spacing. The `set_tile`  function assigns a value to a tile. An edit box deals with strings rather than numbers, so you need to use the `rtos`  (convert Real TO String) function to convert your tile size variable values into strings in decimal format with a precision of two digits. The following code handles this conversion:

```lisp
(if (and dialogLoaded dialogShow)
  (progn
    ;; Set the initial state of the tiles
    (set_tile "gp_trad" (rtos tileRad 2 2))
    (set_tile "gp_spac" (rtos tileSpace 2 2))
  )
)
```
