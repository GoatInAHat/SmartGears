---
title: Equality and Conditional Functions Reference (AutoLISP)
guid: "GUID-C6A7FC58-7A07-4650-9D59-12CAFD194FDA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C6A7FC58-7A07-4650-9D59-12CAFD194FDA.htm"
generated: "2025-11-28T19:06:16.794811Z"
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

# Equality and Conditional Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C6A7FC58-7A07-4650-9D59-12CAFD194FDA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C6A7FC58-7A07-4650-9D59-12CAFD194FDA.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP equality and conditional functions.

| Equality and conditional functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(= *numstr [numstr ...]*)](../Non_alphabetic_Functions/equal_to_AutoLISP.md) | Returns `T`  if all arguments are numerically equal, and returns nil otherwise | ✓ | ✓ | ✓ | -- | ✓ |
| [(/= *numstr [numstr ...]*)](../Non_alphabetic_Functions/not_Equal_to_AutoLISP.md) | Returns `T`  if the arguments are not numerically equal, and nil if the arguments are numerically equal | ✓ | ✓ | ✓ | -- | ✓ |
| [(< *numstr [numstr ...]*)](../Non_alphabetic_Functions/less_than_AutoLISP.md) | Returns `T`  if each argument is numerically less than the argument to its right, and returns nil otherwise | ✓ | ✓ | ✓ | -- | ✓ |
| [(<= *numstr [numstr ...]*)](../Non_alphabetic_Functions/less_than_or_equal_to_AutoLISP.md) | Returns `T`  if each argument is numerically less than or equal to the argument to its right, and returns nil otherwise | ✓ | ✓ | ✓ | -- | ✓ |
| [(> *numstr [numstr ...]*)](../Non_alphabetic_Functions/greater_than_AutoLISP.md) | Returns `T`  if each argument is numerically greater than the argument to its right, and returns nil otherwise | ✓ | ✓ | ✓ | -- | ✓ |
| [(>= *numstr [numstr ...]*)](../Non_alphabetic_Functions/greater_than_or_equal_to_AutoLISP.md) | Returns `T`  if each argument is numerically greater than or equal to the argument to its right, and returns nil otherwise | ✓ | ✓ | ✓ | -- | ✓ |
| [(and *[expr ...]*)](../A_Functions/and_AutoLISP.md) | Returns the logical `AND`  of a list of expressions | ✓ | ✓ | ✓ | -- | ✓ |
| [(boole *func int1 [int2 ...]*)](../B_Functions/boole_AutoLISP.md) | Serves as a general bitwise Boolean function | ✓ | ✓ | ✓ | -- | ✓ |
| [(cond *[(test result ...) ...]*)](../C_Functions/cond_AutoLISP.md) | Serves as the primary conditional function for AutoLISP | ✓ | ✓ | ✓ | -- | ✓ |
| [(eq *expr1 expr2*)](../E_Functions/eq_AutoLISP.md) | Determines whether two expressions are identical | ✓ | ✓ | ✓ | -- | ✓ |
| [(equal *expr1 expr2 [fuzz]*)](../E_Functions/equal_AutoLISP.md) | Determines whether two expressions are equal | ✓ | ✓ | ✓ | -- | ✓ |
| [(if *testexpr thenexpr [elseexpr]*)](../I_Functions/if_AutoLISP.md) | Conditionally evaluates expressions | ✓ | ✓ | ✓ | -- | ✓ |
| [(or *[expr ...]*)](../O_Functions/or_AutoLISP.md) | Returns the logical `OR`  of a list of expressions | ✓ | ✓ | ✓ | -- | ✓ |
| [(repeat *int [expr ...]*)](../R_Functions/repeat_AutoLISP.md) | Evaluates each expression a specified number of times, and returns the value of the last expression | ✓ | ✓ | ✓ | -- | ✓ |
| [(while *testexpr [expr ...]*)](../W_Functions/while_AutoLISP.md) | Evaluates a test expression, and if it is not nil, evaluates other expressions; repeats this process until the test expression evaluates to nil | ✓ | ✓ | ✓ | -- | ✓ |
