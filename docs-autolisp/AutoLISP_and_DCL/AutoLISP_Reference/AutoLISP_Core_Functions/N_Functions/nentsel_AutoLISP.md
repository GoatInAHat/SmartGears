---
title: nentsel (AutoLISP)
guid: "GUID-A7AC0917-66CE-4BAA-BBAF-D49F8ADB26B1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A7AC0917-66CE-4BAA-BBAF-D49F8ADB26B1.htm"
generated: "2025-11-28T19:06:37.519518Z"
description: Prompts the user to select an object (entity) by specifying a point, and provides access to the definition data contained within a complex object
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

# nentsel (AutoLISP)

> Prompts the user to select an object (entity) by specifying a point, and provides access to the definition data contained within a complex object

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A7AC0917-66CE-4BAA-BBAF-D49F8ADB26B1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A7AC0917-66CE-4BAA-BBAF-D49F8ADB26B1.htm)
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
(nentsel
[msg]
)
```

- ***msg*:** **Type:**  String  Message to be displayed as a prompt. If the *msg*  argument is omitted, the Select Object prompt is issued.

## Return Values

**Type:**  Ename (entity name)

When the selected object is not complex (that is, not a 3D polyline or block), `nentsel`  returns the same information as `entsel`. However, if the selected object is a 3D polyline, `nentsel`  returns a list containing the name of the subentity (vertex) and the pick point. This is similar to the list returned by `entsel`, except that the name of the selected vertex is returned instead of the polyline header. The `nentsel`  function always returns the starting vertex of the selected 3D polyline segment. Picking the third segment of the polyline, for example, returns the third vertex. The Seqend subentity is never returned by `nentsel`  for a 3D polyline.

Note:
 A lightweight polyline (lwpolyline entity) is defined in the drawing database as a single entity; it does not contain subentities.

Selecting an attribute within a block reference returns the name of the attribute and the pick point. When the selected object is a component of a block reference other than an attribute, `nentsel`  returns a list containing four elements.

The first element of the list returned from picking an object within a block is the selected entity's name.

The second element is a list containing the coordinates of the point used to pick the object.

The third element is called the Model to World Transformation Matrix. It is a list consisting of four sublists, each of which contains a set of coordinates. This matrix can be used to transform the entity definition data points from an internal coordinate system called the Model Coordinate System (MCS), to the World Coordinate System (WCS). The insertion point of the block that contains the selected entity defines the origin of the MCS. The orientation of the UCS when the block is created determines the direction of the MCS axes.

Note:

nentsel
 is the only AutoLISP function that uses a matrix of this type; the
nentselp
 function returns a matrix similar to those used by other AutoLISP and ObjectARX functions.

The fourth element is a list containing the entity name of the block that contains the selected object. If the selected object is in a nested block (a block within a block), the list also contains the entity names of all blocks in which the selected object is nested, starting with the innermost block and continuing outward until the name of the block that was inserted in the drawing is reported.

## Remarks

The `nentsel`  function prompts the user to select an object. The current Object Snap mode is ignored unless the user specifically requests it. To provide additional support at the Command prompt, `nentsel`  honors keywords defined by a previous call to `initget`.

## Examples

Draw a 3D polyline with multiple line segments; then load and run the following function and select different segments of the line. Pick off the line and then pick the same segments again to see the subentity handle. Try it with a lightweight polyline to see the difference.

```lisp
(defun c:subent ()
  (while
     (setq Ent (entsel "\nPick an entity: "))
     (print (strcat "Entity handle is: "
          (cdr (assoc 5 (entget (car Ent))))))
   )
   (while
      (setq Ent (nentsel "\nPick an entity or subEntity: "))
      (print (strcat "Entity or subEntity handle is:  "
          (cdr (assoc 5 (entget (car Ent))))))
   )
  (prompt "\nDone.")
 (princ)
)
```
