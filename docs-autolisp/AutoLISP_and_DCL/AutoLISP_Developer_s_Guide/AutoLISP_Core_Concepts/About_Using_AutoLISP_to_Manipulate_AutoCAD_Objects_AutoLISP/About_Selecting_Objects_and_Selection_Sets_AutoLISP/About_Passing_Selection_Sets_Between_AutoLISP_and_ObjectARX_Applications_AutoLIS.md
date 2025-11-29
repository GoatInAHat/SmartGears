---
title: About Passing Selection Sets Between AutoLISP and ObjectARX Applications (AutoLISP)
guid: "GUID-87466915-82EC-4487-BAB1-9A1E49CADA8A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-87466915-82EC-4487-BAB1-9A1E49CADA8A.htm"
generated: "2025-11-28T19:06:11.550255Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About Passing Selection Sets Between AutoLISP and ObjectARX Applications (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-87466915-82EC-4487-BAB1-9A1E49CADA8A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-87466915-82EC-4487-BAB1-9A1E49CADA8A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 ObjectARX is not available in AutoCAD LT.

When passing selection sets between AutoLISP and ObjectARX applications, the following should be observed:

If a selection set is created in AutoLISP and stored in an AutoLISP variable, then overwritten by a value returned from an ObjectARX application, the original selection set is eligible for garbage collection (it is freed at the next automatic or explicit garbage collection).

This is true even if the value returned from the ObjectARX application was the original selection set. In the following example, if the `**adsfunc**`  ObjectARX function returns the same selection set it was fed as an argument, then this selection set will be eligible for garbage collection even though it is still assigned to the same variable.

```lisp
(setq var1 (ssget))
(setq var1 (adsfunc var1))
```

If you want the original selection set to be protected from garbage collection, then you must not assign the return value of the ObjectARX application to the AutoLISP variable that already references the selection set. Changing the previous example prevents the selection set referenced by `var1`  from being eligible for garbage collection.

```lisp
(setq var1 (ssget))
(setq var2 (adsfunc var1))
```
