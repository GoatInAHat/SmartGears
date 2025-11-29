---
title: menucmd (AutoLISP)
guid: "GUID-20357966-71D4-4D55-881B-06AFDECD24DB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-20357966-71D4-4D55-881B-06AFDECD24DB.htm"
generated: "2025-11-28T19:06:36.944152Z"
description: Issues menu commands, or sets and retrieves menu item status
topic_type: "reference-adsk"
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
  - function
---

# menucmd (AutoLISP)

> Issues menu commands, or sets and retrieves menu item status

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-20357966-71D4-4D55-881B-06AFDECD24DB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-20357966-71D4-4D55-881B-06AFDECD24DB.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(menucmd
str
)
```

- ***str*:** **Type:**  String  Menu area and the value to assign to that menu area. The *string*  argument has the following syntax:  `"menu_area=value"`  The allowed values of *menu_area*, shown in the following list, are the same as they are in menu file submenu references.  **B1-B4**  -- BUTTONS menus 1 through 4  **A1-A4**  -- AUX menus 1 through 4  **P0-P16**  -- Pull-down (POP) menus 0 through 16  **I**  -- Image tile menus  **S**  -- SCREEN menu (Obsolete)  **T1-T4**  -- TABLET menus 1 through 4  **M**  -- DIESEL string expressions  ***Gmenugroup.nametag***  -- A menugroup and name tag.  Note:  Only Pull-down (POP) menus and DIESEL string expressions are supported on Mac OS.  Only DIESEL string expressions are supported on Web.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

The `menucmd`  function can switch between subpages in an AutoCAD menu. This function can also force the display of menus. This allows AutoLISP programs to use image tile menus and to display other menus from which the user can make selections. AutoLISP programs can also enable, disable, and place marks in menu or ribbon items.

## Examples

The following code displays the image tile menu `MOREICONS`:

```lisp
(menucmd "I=moreicons")

Loads the MOREICONS image tile menu

(menucmd "I=*")

Displays the menu
```

The following code checks the status of the third menu item in the pull-down menu `POP11`. If the menu item is currently enabled, the `**menucmd**`  function disables it.

```lisp
(setq s (menucmd "P11.3=?"))

Gets the status of the menu item

(if (= s "")

If the status is an empty string,

  (menucmd "P11.3=~")

disable the menu item

)
```

The previous code is not foolproof. In addition to being enabled or disabled, menu items can also receive marks. The code `(menucmd "P11.3=?")`  could return `"!."`, indicating that the menu item is currently checked. This code would assume that the menu item is disabled and continue without disabling it. If the code included a call to the `wcmatch`  function, it could check the status for an occurrence of the tilde (`~)`  character and then take appropriate action.

The `menucmd`  function also allows AutoLISP programs to take advantage of the DIESEL string expression language. Some things can be done more easily with DIESEL than with the equivalent AutoLISP code. The following code returns a string containing the current day and date:

```lisp
(menucmd "M=$(edtime,$(getvar,date),DDDD\",\" D MONTH YYYY)")

"Sunday, 16 July 1995"
```
