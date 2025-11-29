---
title: "Function-Handling Functions Reference (AutoLISP)"
guid: "GUID-6804DF2D-E5C4-466A-9869-3CDD7A031FAA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6804DF2D-E5C4-466A-9869-3CDD7A031FAA.htm"
generated: "2025-11-28T19:06:17.917238Z"
topic_type: concept
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
---

# Function-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6804DF2D-E5C4-466A-9869-3CDD7A031FAA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6804DF2D-E5C4-466A-9869-3CDD7A031FAA.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP function-handling functions.

| Function-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(apply *function lst*)](../A_Functions/apply_AutoLISP.md) | Passes a list of arguments to a specified function | ✓ | ✓ | ✓ | -- | ✓ |
| [(defun *sym ([arguments] [/variables ...]) expr ...*)](../D_Functions/defun_AutoLISP.md) | Defines a function | ✓ | ✓ | ✓ | -- | ✓ |
| [(defun-q *sym ([arguments] [/ variables ...]) expr ...*)](../D_Functions/defun_q_AutoLISP.md) | Defines a function as a list (intended for backward-compatibility only) | ✓ | ✓ | ✓ | -- | ✓ |
| [(defun-q-list-ref *'function*)](../D_Functions/defun_q_list_ref_AutoLISP.md) | Displays the list structure of a function defined with `defun-q` | ✓ | ✓ | ✓ | -- | ✓ |
| [(defun-q-list-set *'sym list*)](../D_Functions/defun_q_list_set_AutoLISP.md) | Defines a function as a list (intended for backward-compatibility only) | ✓ | ✓ | ✓ | -- | ✓ |
| [(eval *expr*)](../E_Functions/eval_AutoLISP.md) | Returns the result of evaluating an AutoLISP expression | ✓ | ✓ | ✓ | -- | ✓ |
| [(lambda *arguments expr ...*)](../L_Functions/lambda_AutoLISP.md) | Defines an anonymous function | ✓ | ✓ | ✓ | -- | ✓ |
| [(progn *[expr ...]*)](../P_Functions/progn_AutoLISP.md) | Evaluates each expression sequentially, and returns the value of the last expression | ✓ | ✓ | ✓ | -- | ✓ |
| [(trace *function ...*)](../T_Functions/trace_AutoLISP.md) | Aids in AutoLISP debugging | ✓ | ✓ | ✓ | -- | ✓ |
| [(untrace *function ...*)](../U_Functions/untrace_AutoLISP.md) | Clears the trace flag for the specified functions | ✓ | ✓ | ✓ | -- | ✓ |
