---
title: About Dotted Pairs (AutoLISP)
guid: "GUID-877B87BA-6FA2-4894-9F94-184E7DF25B7D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-877B87BA-6FA2-4894-9F94-184E7DF25B7D.htm"
generated: "2025-11-28T19:06:04.021358Z"
description: Dotted pair lists must always contain two members and is the method AutoLISP uses to maintain entity definition data.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Dotted Pairs (AutoLISP)

> Dotted pair lists must always contain two members and is the method AutoLISP uses to maintain entity definition data.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-877B87BA-6FA2-4894-9F94-184E7DF25B7D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-877B87BA-6FA2-4894-9F94-184E7DF25B7D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

When representing a dotted pair, members of the list are separated by a period (**`.`**). Most list-handling functions do not accept a dotted pair as an argument, so you should be sure you are passing the right kind of list to a function.

Dotted pairs are an example of an "improper list." An improper list is one in which the last `cdr`  is not `nil`. In addition to adding an item to the beginning of a list, the `cons`  function can create a dotted pair. If the second argument to the `cons`  function is anything other than another list or `nil`, it creates a dotted pair.

```lisp
(setq sublist (cons 'lyr "WALLS"))

(LYR . "WALLS")
```

The following functions are useful for handling dotted pairs:

- car
   - Returns the first member of the specified dotted pair.
- cdr
   - Returns the second member of the specified dotted pair.
- assoc
   - Searches an association list for a member and returns that association list entry.
- nth
   - Returns the nth element of a list.

The following code creates an association list of dotted pairs:

```lisp
(setq wallinfo (list sublist (cons 'len 240.0) (cons 'hgt 96.0)))

((LYR . "WALLS") (LEN . 240.0) (HGT . 96.0))
```

The `assoc`  function returns a specified list from within an association list regardless of the specified list's location within the association list. The `assoc`  function searches for a specified key element in the lists and returns the first instance, as follows:

```lisp
(assoc 'len wallinfo)

(LEN . 240.0)

(cdr (assoc 'lyr wallinfo))

"WALLS"

(nth 1 wallinfo)

(LEN . 240.0)

(car (nth 1 wallinfo))

LEN
```
