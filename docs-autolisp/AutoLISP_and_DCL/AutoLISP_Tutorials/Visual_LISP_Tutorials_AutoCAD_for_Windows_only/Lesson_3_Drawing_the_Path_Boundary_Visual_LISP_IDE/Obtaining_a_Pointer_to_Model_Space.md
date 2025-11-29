---
title: Obtaining a Pointer to Model Space
guid: "GUID-0A1B6F6E-B686-4816-A8E0-618A197C49A5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0A1B6F6E-B686-4816-A8E0-618A197C49A5.htm"
generated: "2025-11-28T19:06:59.021211Z"
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

# Obtaining a Pointer to Model Space

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0A1B6F6E-B686-4816-A8E0-618A197C49A5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0A1B6F6E-B686-4816-A8E0-618A197C49A5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

When you add entities through ActiveX functions, you need to identify the model space or paper space in which the entity is to be inserted. (In ActiveX terminology, entities are *objects*, but this tutorial will continue using the term entity.) To tell AutoCAD which space the new entities should occupy, you need to obtain a pointer to that space. Unfortunately, obtaining a pointer to model space is not a simple, single-shot function. The following code fragment shows how the operation needs to be set up:

```lisp
(vla-get-ModelSpace (vla-get-ActiveDocument
  (vlax-get-Acad-Object)))
```

Working from the inside out, the `vlax-get-Acad-Object`  function retrieves a pointer to AutoCAD. This pointer is passed to the `vla-get-ActiveDocument`  function, which retrieves a pointer to the active drawing (document) within AutoCAD. The Active Document pointer is then passed to the `vla-get-ModelSpace`  function that retrieves a pointer to the model space of the current drawing.

This is not the kind of expression you want to type over and over. For example, look at how much more complicated the code for adding a polyline using ActiveX appears when the entire model space expression is used:

```lisp
(setq pline (vla-addLightweightPolyline
  (vla-get-ModelSpace
    (vla-get-ActiveDocument
      (vlax-get-Acad-Object)
    )
  )
  VLADataPts)
)
(vla-put-closed pline T)
```

The function is definitely less understandable. Not only that, but within every expression within your program where an entity is created, you repeat the same set of nested functions. This demonstrates one of the few excellent uses for global variables. The garden path application can add a lot of entities to model space (think of all the tiles in the path), so, set up a global variable to store the pointer to the model space, as in the following code:

```lisp
(setq *ModelSpace* (vla-get-ModelSpace (vla-get-ActiveDocument
                 (vlax-get-Acad-Object))))
```

You can use the variable `*ModelSpace*`  anytime you call an ActiveX entity creation function. The only tricky thing with this scheme is the `*ModelSpace*`  variable must be ready to go before you start drawing. For this reason, the `setq`  establishing this variable will be called at the time the application is loaded, immediately after the call to `vl-load-com`. These calls will be placed before any `defun`  in the program file. As a result, they are executed as soon as the file is loaded.
