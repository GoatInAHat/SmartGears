---
title: "Symbol-Handling Functions Reference (AutoLISP)"
guid: "GUID-796EFF94-E904-4F5B-B120-C6D2DCD8646A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-796EFF94-E904-4F5B-B120-C6D2DCD8646A.htm"
generated: "2025-11-28T19:06:19.015954Z"
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

# Symbol-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-796EFF94-E904-4F5B-B120-C6D2DCD8646A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-796EFF94-E904-4F5B-B120-C6D2DCD8646A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP symbol-handling functions.

| Symbol-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(atom *item*)](../A_Functions/atom_AutoLISP.md) | Verifies that an item is an atom | ✓ | ✓ | ✓ | -- | ✓ |
| [(atoms-family *format [symlist]*)](../A_Functions/atoms_family_AutoLISP.md) | Returns a list of the currently defined symbols | ✓ | ✓ | ✓ | -- | ✓ |
| [(boundp *sym*)](../B_Functions/boundp_AutoLISP.md) | Verifies whether a value is bound to a symbol | ✓ | ✓ | ✓ | -- | ✓ |
| [(not *item*)](../N_Functions/not_AutoLISP.md) | Verifies that an item evaluates to nil | ✓ | ✓ | ✓ | -- | ✓ |
| [(null *item*)](../N_Functions/null_AutoLISP.md) | Verifies that an item is bound to nil | ✓ | ✓ | ✓ | -- | ✓ |
| [(numberp *item*)](../N_Functions/numberp_AutoLISP.md) | Verifies that an item is a real or an integer | ✓ | ✓ | ✓ | -- | ✓ |
| [(quote *expr*)](../Q_Functions/quote_AutoLISP.md) | Returns an expression without evaluating it | ✓ | ✓ | ✓ | -- | ✓ |
| [(set *sym expr*)](../S_Functions/set_AutoLISP.md) | Sets the value of a quoted symbol name to an expression | ✓ | ✓ | ✓ | -- | ✓ |
| [(setq *sym1 expr1 [sym2 expr2 ...]*)](../S_Functions/setq_AutoLISP.md) | Sets the value of a symbol or symbols to associated expressions | ✓ | ✓ | ✓ | -- | ✓ |
| [(type *item*)](../T_Functions/type_AutoLISP.md) | Returns the type of a specified item | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-symbol-name *symbol*)](../V_Functions/vl_symbol_name_AutoLISP.md) | Returns a string containing the name of a symbol | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-symbol-value *symbol*)](../V_Functions/vl_symbol_value_AutoLISP.md) | Returns the current value bound to a symbol | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-symbolp *object*)](../V_Functions/vl_symbolp_AutoLISP.md) | Identifies whether or not a specified object is a symbol | ✓ | ✓ | ✓ | -- | ✓ |
