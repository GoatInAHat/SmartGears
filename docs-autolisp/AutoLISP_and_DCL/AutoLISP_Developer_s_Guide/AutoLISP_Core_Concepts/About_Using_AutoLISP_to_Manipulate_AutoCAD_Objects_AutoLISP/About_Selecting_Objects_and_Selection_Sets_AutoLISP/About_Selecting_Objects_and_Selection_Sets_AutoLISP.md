---
title: About Selecting Objects and Selection Sets (AutoLISP)
guid: "GUID-1DABEF81-C431-442A-8916-7F9079AA7799"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1DABEF81-C431-442A-8916-7F9079AA7799.htm"
generated: "2025-11-28T19:06:10.777831Z"
description: Selection sets are groups of one or more selected objects (entities).
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 30/03/2020
topic_subtype:
  - autolisp
---

# About Selecting Objects and Selection Sets (AutoLISP)

> Selection sets are groups of one or more selected objects (entities).

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1DABEF81-C431-442A-8916-7F9079AA7799.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1DABEF81-C431-442A-8916-7F9079AA7799.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 30/03/2020

You can interactively add objects to, remove objects from, or list objects in a selection set. The following example code uses the `ssget`  function to return a selection set containing all the objects in a drawing.

```lisp
(ssget "X")
<Selection set: 1>
```

AutoLISP provides a number of functions for handling selection sets. The following lists some of the functions available for working with selection sets:

- ssget
   - Prompts the user to select objects (entities), and returns a selection set.
- ssadd
   - Adds an object (entity) to a selection set, or creates a new selection set.
- ssdel
   - Removes an object (entity) from a selection set.
- ssname
   - Returns the object (entity) name of the indexed element of a selection set.
- sslength
   - Returns an integer containing the number of objects (entities) in a selection set.

The `ssget`  function provides the most general means of creating a selection set. It can create a selection set in one of the following ways:

- Explicitly specifying the objects to select, using the Last, Previous, Window, Implied, Window Polygon, Crossing, Crossing Polygon, or Fence options
- Specifying a single point
- Selecting all objects in the database
- Prompting the user to select objects

With any option, you can use filtering to specify a list of properties and conditions that the selected objects must match.

Note:
 Selection set and entity names do not remain the same between drawing sessions.

The first argument to `ssget`  is a string that describes which selection option to use. The next two arguments, *pt1*  and *pt2*, specify point values for the relevant options (they should be left out if they do not apply). A point list, *pt-list*, must be provided as an argument to the selection methods that allow selection by polygons (that is, Fence, Crossing Polygon, and Window Polygon). The last argument, *filter-list*, is optional. If *filter-list*  is supplied, it specifies the list of entity field values used in filtering. For example, you can obtain a selection set that includes all objects of a given type, on a given layer, or of a given color.

The following table shows examples of calls to `ssget`:

| SSGET Examples |  |
| --- | --- |
| Function call | Effect |
| (setq pt1 '(0.0 0.0 0.0) pt2 '(5.0 5.0 0.0) pt3 '(4.0 1.0 0.0) pt4 '(2.0 6.0 0.0) ) | Sets pt1, pt2, pt3, and pt4 to point values |
| (setq ss1 (ssget)) | Prompts the user for a general object selection and places those items in a selection set |
| (setq ss1 (ssget "P")) | Creates a selection set from the most recently created selection set |
| (setq ss1 (ssget "L")) | Creates a selection set of the last object added to the database that is visible on the screen |
| (setq ss1 (ssget pt2)) | Creates a selection set of an object passing through point (5,5) |
| (setq ss1 (ssget "W" pt1 pt2)) | Creates a selection set of the objects inside the window from (0,0) to (5,5) |
| (setq ss1 (ssget "F" (list pt2 pt3 pt4))) | Creates a selection set of the objects crossing the fence and defined by the points (5,5), (4,1), and (2,6) |
| (setq ss1 (ssget "WP" (list pt1 pt2 pt3))) | Creates a selection set of the objects inside the polygon defined by the points (0,0), (5,5), and (4,1) |
| (setq ss1 (ssget "X")) | Creates a selection set of all objects in the database |

When an application has finished using a selection set, it is important to release it from memory. This can be done by setting it to `nil`:

```lisp
(setq ss1 nil)
```

Remember:
 You can also release the memory used by the values stored in a variable by defining it as a local variable in a function.

Attempting to manage a large number of selection sets simultaneously is not recommended. An AutoLISP application cannot have more than 128 selection sets open at once. (The limit may be lower on your system.) When the limit is reached, AutoCAD will not create more selection sets. Keep a minimum number of sets open at a time, and set unneeded selection sets to `nil`  as soon as possible. If the maximum number of selection sets is reached, you must call the `gc`  function to free unused memory before another `ssget`  will work.
