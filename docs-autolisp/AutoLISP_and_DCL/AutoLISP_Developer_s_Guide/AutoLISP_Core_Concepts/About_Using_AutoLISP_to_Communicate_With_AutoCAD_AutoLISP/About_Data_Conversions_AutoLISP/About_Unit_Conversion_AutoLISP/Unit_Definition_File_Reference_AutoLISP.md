---
title: Unit Definition File Reference (AutoLISP)
guid: "GUID-EFC1FCF5-3CCF-4CDD-A205-1C2B3C747125"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EFC1FCF5-3CCF-4CDD-A205-1C2B3C747125.htm"
generated: "2025-11-28T19:06:09.441060Z"
description: The AutoCAD unit definition file, acad.unt, allows you to define the factors to convert data one set of units to another set of units.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# Unit Definition File Reference (AutoLISP)

> The AutoCAD unit definition file, acad.unt , allows you to define the factors to convert data one set of units to another set of units.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EFC1FCF5-3CCF-4CDD-A205-1C2B3C747125.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EFC1FCF5-3CCF-4CDD-A205-1C2B3C747125.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

The *acad.unt*  file is a plain ASCII text format file and is used by the unit-conversion function `cvunit`. You can add new and modify the unit definitions available by using a text editor. A definition consists of two lines in the file—the unit name and the unit definition. The first line must have an asterisk (*) in the first column, followed by the name of the unit. A unit name can have several abbreviations or alternate spellings, separated by commas. If a unit name has singular and plural forms, you can specify these using the following format:

```lisp
*[ [common] [ ( [singular.] plural) ] ]...
```

You can specify multiple expressions (singular and plural). They do not have to be located at the end of the word, and a plural form is not required. The following are examples of valid unit name definitions:

```lisp
*inch(es)
*milleni(um.a)
*f(oot.eet) or (foot.feet)
```

The line following the `*unit name`  line defines the unit as either fundamental or derived.

## Fundamental Units

A fundamental unit is an expression in constants. If the line following the `*unit name`  line begins with something other than an equal sign (=), it defines fundamental units. Fundamental units consist of five integers and two real numbers in the following form:

```lisp
c, e, h, k, m, r1, r2
```

The five integers correspond to the exponents of these five constants:

**c** Velocity of light in a vacuum

**e** Electron charge

**h** Planck's constant

**k** Boltzman's constant

**m** Electron rest mass

As a group, these exponents define the dimensionality of the unit: length, mass, time, volume, and so on.

The first real number (r1) is a multiplier, and the second (r2) is an additive offset (used only for temperature conversions). The fundamental unit definition allows for different spellings of the unit (for example, `meter`  and `metre`); the case of the unit is ignored. An example of a fundamental unit definition is as follows:

```lisp
*meter(s),metre(s),m
-1,0,1,0,-1,4.1214856408e11,0
```

In this example, the constants that make one meter are as follows:

## Derived Units

A derived unit is defined in terms of other units. If the line following the *unit name line begins with an equal sign (**`=`**), it defines derived units. Valid operators in these definitions are **`*`**  (multiplication), **`/`**  (division), **`+`**  (addition), **`-`**  (subtraction), and **`^`**  (exponentiation).

You can specify a predefined unit by naming it, and you can use abbreviations (if provided). The items in a formula are multiplied together unless some other arithmetic operator is specified. For example, the units database defines the dimensionless multiple and submultiple names, so you can specify a unit such as micro-inches by entering `micro inch`.

The following are examples of derived unit definitions.

```lisp
; Units of area
*township(s)
=93239571.456 meter^2
```

The definition of a township is given as 93,239,571.456 square meters.

```lisp
; Electromagnetic units
*volt(s),v
=watt/ampere
```

In this example, a volt is defined as a watt divided by an ampere. In the *acad.unt*, both watts and amperes are defined in terms of fundamental units.

## User Comments

Comments can be added to the file by placing a semicolon at the beginning of a line. The comment continues to the end of the line.

```lisp
; This entire line is a comment.
```
