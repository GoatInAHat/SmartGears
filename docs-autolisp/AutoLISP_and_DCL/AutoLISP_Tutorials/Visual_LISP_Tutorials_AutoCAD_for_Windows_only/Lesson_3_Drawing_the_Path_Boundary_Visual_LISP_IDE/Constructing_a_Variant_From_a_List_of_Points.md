---
title: Constructing a Variant From a List of Points
guid: "GUID-A3314A92-F974-4A3E-B673-5F074A7A3B2B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A3314A92-F974-4A3E-B673-5F074A7A3B2B.htm"
generated: "2025-11-28T19:06:59.243747Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 16/02/2020
topic_subtype:
  - autolisp
---

# Constructing a Variant From a List of Points

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A3314A92-F974-4A3E-B673-5F074A7A3B2B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A3314A92-F974-4A3E-B673-5F074A7A3B2B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 16/02/2020

So far, the data in the polypoints variable is in a list format suitable for many AutoLISP calls. However, the data is to be supplied as an input parameter to an ActiveX call that expects a variant array of doubles. You can use another utility function to make the required conversion from list to variant:

```lisp
(defun gp:list->variantArray (ptsList / arraySpace sArray)
  ; allocate space for an array of 2d points stored as doubles
  (setq arraySpace (vlax-make-safearray
              vlax-vbdouble ; element type
              (cons 0
                    (- (length ptsList) 1)
                    ) ; array dimension
              )
  )

  (setq sArray (vlax-safearray-fill arraySpace ptsList))

  ; return array variant
  (vlax-make-variant sArray)
)
```

The following actions take place in `gp:list->variantArray`:

-  The
  vlax-make-safearray
   function is called to allocate an array of doubles (
  vlax-vbdouble
  ). The
  vlax-make-safearray
   function also requires you to specify the lower and upper index boundaries of the array. In
  gp:list->variantArray
  , the call to
  vlax-make-safearray
   specifies a start index of 0 and sets the upper limit to one less than the number of elements passed to it (
  ptsList
  ).
-  The
  vlax-safearray-fill
   function is called to populate the array with the elements in the point list.
-  The
  vlax-make-variant
   is called to convert the safearray into a variant. As the last function call in
  gp:list->variantArray
  , the return value is passed to the calling function.

The following is an example of a function call that invokes `gp:list->variantArray`  to convert a list to a variant array of doubles:

```lisp
; data conversion from list to variant
(setq VLADataPts (gp:list->variantArray polypoints))
```
