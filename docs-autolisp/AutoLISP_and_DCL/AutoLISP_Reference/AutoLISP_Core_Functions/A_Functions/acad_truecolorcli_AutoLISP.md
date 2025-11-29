---
title: acad_truecolorcli (AutoLISP)
guid: "GUID-47F4C7F1-769B-4AA4-8AE9-C81C1C4D6FB9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-47F4C7F1-769B-4AA4-8AE9-C81C1C4D6FB9.htm"
generated: "2025-11-28T19:06:21.720420Z"
description: Prompts for colors at the Command prompt
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

# acad_truecolorcli (AutoLISP)

> Prompts for colors at the Command prompt

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-47F4C7F1-769B-4AA4-8AE9-C81C1C4D6FB9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-47F4C7F1-769B-4AA4-8AE9-C81C1C4D6FB9.htm)
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
(acad_truecolorcli
color [allowbylayer] [alternatePrompt]
)
```

- ***color*:** **Type:**  List  A dotted pair that describes the default color. The first element of the dotted pair must be one of the color-related DXF group codes (62, 420, or 430); for example, `(62 . *ColorIndex*)`, `(420 . *TrueColor*)`, or `(430 . *"colorbook$colorname"*)`.
- ***allowbylayer*:** **Type:**  T or nil  Omitting the *allowbylayer*  argument or setting it to a non- `nil`  value enables entering ByLayer or ByBlock to set the color. If set to `nil`, an error results if ByLayer or ByBlock is entered.
- ***alternateprompt*:** **Type:**  T or nil  An optional prompt string. If this string is omitted, the default value is “New color”.

## Return Values

**Type:**  List or nil

When the operation is successful, the function returns a list of one or more dotted pairs (depending on the tab on which the color is selected) describing the color selected. The last dotted pair in the list indicates the color selected. The function returns `nil`  if the user cancels the dialog box.

- **Color book color:** If the last item in the returned list is a 430 pair, then the specified color originates from a color book. This returned list will also contain a 420 pair that describes the corresponding true color and a 62 pair that describes the closest matching color index value.
- **True color:** If the returned list contains a 420 pair as the last item, then a true color was specified (as “Red,Green,Blue”). The list will also contain a 62 pair that indicates the closest matching color index. No 430 pair will be present.
- **Color index:** If the last item in the list is a 62 pair, then a color index was chosen. No other dotted pairs will be present in the returned list.

## Examples

Prompt for a color selection at the command line with a purple color index default selection and alternative text for the command prompt:

```lisp
(acad_truecolorcli '(62 . 215) 1 "Pick a color")
```

New Color [Truecolor/COlorbook] <215>:

```lisp
((62 . 256))
```

Prompt for a color selection at the command line with a yellow color index default selection, then set the color by layer:

```lisp
(acad_truecolorcli '(62 . 2))
```

New Color [Truecolor/COlorbook] <2 (yellow)>: bylayer

```lisp
((62 . 256))
```
