---
title: Arithmetic Functions Reference (AutoLISP)
guid: "GUID-FC14467C-63B9-4400-8C6A-266D21C846AE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FC14467C-63B9-4400-8C6A-266D21C846AE.htm"
generated: "2025-11-28T19:06:16.317223Z"
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

# Arithmetic Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FC14467C-63B9-4400-8C6A-266D21C846AE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FC14467C-63B9-4400-8C6A-266D21C846AE.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP arithmetic functions.

| Arithmetic functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(+ *[number number ...]*)](../Non_alphabetic_Functions/add_AutoLISP.md) | Returns the sum of all numbers | ✓ | ✓ | ✓ | -- | ✓ |
| [(- *[number number ...]*)](../Non_alphabetic_Functions/subtract_AutoLISP.md) | Subtracts the second and following numbers from the first and returns the difference | ✓ | ✓ | ✓ | -- | ✓ |
| [(* *[number number ...]*)](../Non_alphabetic_Functions/multiply_AutoLISP.md) | Returns the product of all numbers | ✓ | ✓ | ✓ | -- | ✓ |
| [(/ *[number number ...]*)](../Non_alphabetic_Functions/divide_AutoLISP.md) | Divides the first number by the product of the remaining numbers and returns the quotient | ✓ | ✓ | ✓ | -- | ✓ |
| [(~ *int*)](../Non_alphabetic_Functions/bitwise_NOT_AutoLISP.md) | Returns the bitwise `NOT`  (1's complement) of the argument | ✓ | ✓ | ✓ | -- | ✓ |
| [(1+ *number*)](../Non_alphabetic_Functions/1_increment_AutoLISP.md) | Returns the argument increased by 1 (incremented) | ✓ | ✓ | ✓ | -- | ✓ |
| [(1- *number*)](../Non_alphabetic_Functions/1_decrement_AutoLISP.md) | Returns the argument reduced by 1 (decremented) | ✓ | ✓ | ✓ | -- | ✓ |
| [(abs *number*)](../A_Functions/abs_AutoLISP.md) | Returns the absolute value of the argument | ✓ | ✓ | ✓ | -- | ✓ |
| [(atan *num1 [num2]*)](../A_Functions/atan_AutoLISP.md) | Returns the arctangent of a number in radians | ✓ | ✓ | ✓ | -- | ✓ |
| [(cos *ang*)](../C_Functions/cos_AutoLISP.md) | Returns the cosine of an angle expressed in radians | ✓ | ✓ | ✓ | -- | ✓ |
| [(exp *number*)](../E_Functions/exp_AutoLISP.md) | Returns the constant e (a real) raised to a specified power (the natural antilog) | ✓ | ✓ | ✓ | -- | ✓ |
| [(expt *base power*)](../E_Functions/expt_AutoLISP.md) | Returns a number raised to a specified power | ✓ | ✓ | ✓ | -- | ✓ |
| [(fix *number*)](../F_Functions/fix_AutoLISP.md) | Returns the conversion of a real into the nearest smaller integer | ✓ | ✓ | ✓ | -- | ✓ |
| [(float *number*)](../F_Functions/float_AutoLISP.md) | Returns the conversion of a number into a real | ✓ | ✓ | ✓ | -- | ✓ |
| [(gcd *int1 int2*)](../G_Functions/gcd_AutoLISP.md) | Returns the greatest common denominator of two integers | ✓ | ✓ | ✓ | -- | ✓ |
| [(log *number*)](../L_Functions/log_AutoLISP.md) | Returns the natural log of a number as a real | ✓ | ✓ | ✓ | -- | ✓ |
| [(logand *[int int ...]*)](../L_Functions/logand_AutoLISP.md) | Returns the result of the logical bitwise `AND`  of a list of integers | ✓ | ✓ | ✓ | -- | ✓ |
| [(logior *[int int ...]*)](../L_Functions/logior_AutoLISP.md) | Returns the result of the logical bitwise inclusive `OR`  of a list of integers | ✓ | ✓ | ✓ | -- | ✓ |
| [(lsh *[int numbits]*)](../L_Functions/lsh_AutoLISP.md) | Returns the logical bitwise shift of an integer by a specified number of bits | ✓ | ✓ | ✓ | -- | ✓ |
| [(max *[number number ...]*)](../M_Functions/max_AutoLISP.md) | Returns the largest of the numbers given | ✓ | ✓ | ✓ | -- | ✓ |
| [(min *[number number ...]*)](../M_Functions/min_AutoLISP.md) | Returns the smallest of the numbers given | ✓ | ✓ | ✓ | -- | ✓ |
| [(minusp *number*)](../M_Functions/minusp_AutoLISP.md) | Verifies that a number is negative | ✓ | ✓ | ✓ | -- | ✓ |
| [(rem *[num1 num2 ...]*)](../R_Functions/rem_AutoLISP.md) | Divides the first number by the second, and returns the remainder | ✓ | ✓ | ✓ | -- | ✓ |
| [(sin *ang*)](../S_Functions/sin_AutoLISP.md) | Returns the sine of an angle as a real expressed in radians | ✓ | ✓ | ✓ | -- | ✓ |
| [(sqrt *number*)](../S_Functions/sqrt_AutoLISP.md) | Returns the square root of a number as a real | ✓ | ✓ | ✓ | -- | ✓ |
| [(zerop *number*)](../Z_Functions/zerop_AutoLISP.md) | Verifies that a number evaluates to zero | ✓ | ✓ | ✓ | -- | ✓ |
