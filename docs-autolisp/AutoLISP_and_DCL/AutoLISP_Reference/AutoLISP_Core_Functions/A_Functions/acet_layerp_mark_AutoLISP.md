---
title: "acet-layerp-mark (AutoLISP)"
guid: "GUID-5A7A7482-8EDB-43C3-BDD1-A7AEF45986DE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5A7A7482-8EDB-43C3-BDD1-A7AEF45986DE.htm"
generated: "2025-11-28T19:06:22.001232Z"
description: Places beginning and ending marks for Layer Previous recording
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

# acet-layerp-mark (AutoLISP)

> Places beginning and ending marks for Layer Previous recording

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5A7A7482-8EDB-43C3-BDD1-A7AEF45986DE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5A7A7482-8EDB-43C3-BDD1-A7AEF45986DE.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(acet-layerp-mark
[status]
)
```

- ***status*:** **Type:**  T or nil  **`T`**  -- sets a begin mark  **`nil`**  -- sets an end mark, clearing the begin mark

## Return Values

**Type:**  T or nil

`T`  if a begin mark is in effect; otherwise `nil`.

If *status*  is omitted, `acet-layerp-mark`  returns the current mark status for layer settings.

## Remarks

The `acet-layerp-mark`  function allows you to group multiple layer commands into a single transaction so that they can be undone by issuing the AutoCAD LAYERP command a single time. The LAYERPMODE setting must be On in order to set marks.

## Examples

The following code changes layer 0 to blue, and then makes several additional layer changes between a set of begin and end marks. If you issue the AutoCAD LAYERP command after running this code, layer 0 reverts to blue.

```lisp
(defun TestLayerP ()
  ;; Turn LAYERPMODE on, if it is not already
  (if (not (acet-layerp-mode))
    (acet-layerp-mode T)
  )

  ;; Set layer 0 to the color blue
  (command "._layer" "_color" "blue" "0" "")

  ;; Set a begin mark
  (acet-layerp-mark T)

  ;; Issue a series of layer commands, and then set an end mark
  (command "._layer" "_color" "green" "0" "")
  (command "._layer" "_thaw" "*" "")
  (command "._layer" "_unlock" "*" "")
  (command "._layer" "_ltype" "hidden" "0" "")
  (command "._layer" "_color" "red" "0" "")

  ;; Set an end mark
  (acet-layerp-mark nil)
 (princ)
)
```
