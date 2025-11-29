---
title: Assigning Actions to Tiles
guid: "GUID-8715618D-FB61-4428-A4A5-32165A3D8E2F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8715618D-FB61-4428-A4A5-32165A3D8E2F.htm"
generated: "2025-11-28T19:07:00.963085Z"
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

# Assigning Actions to Tiles

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8715618D-FB61-4428-A4A5-32165A3D8E2F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8715618D-FB61-4428-A4A5-32165A3D8E2F.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

A DCL definition does nothing more than define a lifeless dialog box. You connect this lifeless dialog box to your dynamic AutoLISP code with the `action_tile`  function, as demonstrated by the following code:

```lisp
;; Assign actions (the functions to be invoked) to dialog buttons
(action_tile
  "gp_lw"
  "(setq plineStyle \"Light\")"
)
(action_tile
  "gp_hw"
  "(setq plineStyle \"Pline\")"
)
(action_tile
  "gp_actx"
  "(setq objectCreateMethod \"ActiveX\")"
)
(action_tile
  "gp_emake"
  "(setq objectCreateMethod \"Entmake\")"
)
(action_tile
  "gp_cmd"
  "(setq objectCreateMethod \"Command\")"
)
(action_tile "cancel" "(done_dialog) (setq UserClick nil)")
(action_tile
  "accept"
  (strcat "(progn (setq tileRad (atof (get_tile \"gp_trad\")))"
       "(setq tileSpace (atof (get_tile \"gp_spac\")))"
       "(done_dialog) (setq UserClick T))"
  )
)
```

Notice all the quotes around the AutoLISP code. When you write an AutoLISP `action_tile`  function, your code is essentially telling a tile, “here, remember this string, then pass it back to me when the user selects you.” The string (anything within double-quotation marks) is dormant until the user selects the tile. At that time, the tile passes the string to AutoCAD, which converts the string into functioning AutoLISP code and executes the code.

For example, consider the following `action_tile`  expression, which is connected to the lightweight polyline radio button:

```lisp
(action_tile
   "gp_lw"
   "(setq plineStyle \"Light\")"
)
```

The code assigns the string `"(setq plineStyle \"Light\")"`  to the radio button. When a user picks the button, the string is passed back to AutoCAD and transformed directly into the following AutoLISP expression:

```lisp
(setq plineStyle "Light")
```

Look at one more code fragment. The following is the `action_tile`  expression assigned to the OK button:

```lisp
(action_tile
    "accept"
    (strcat "(progn (setq tileRad (atof (get_tile \"gp_trad\")))"
      "(setq tileSpace (atof (get_tile \"gp_spac\")))"
      "(done_dialog) (setq UserClick T))"
)
```

When a user clicks the OK button, the lengthy string assigned to the button is passed to AutoCAD and turned into the following AutoLISP code:

```lisp
(progn
   (setq tileRad (atof (get_tile "gp_trad")))
   (setq tileSpace (atof (get_tile "gp_spac")))
   (done_dialog)
   (setq UserClick T)
)
```

This code does several things: It retrieves the current values from the tiles whose key values are `gp_trad`  (the tile radius) and `gp_spac`  (the tile spacing value). Then `atof`  converts the number string into a real number. The dialog is terminated with the `done_dialog`  function, and a value of `T`, or true, is assigned to the variable `UserClick`.

You're done assigning actions to the buttons. The next thing to do is to put it all in motion.
