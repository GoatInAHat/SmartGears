---
title: User Input Functions Reference (AutoLISP)
guid: "GUID-49DBE7DD-D1D2-4B3A-9718-E3F4E2FD5E81"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-49DBE7DD-D1D2-4B3A-9718-E3F4E2FD5E81.htm"
generated: "2025-11-28T19:06:19.336957Z"
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

# User Input Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-49DBE7DD-D1D2-4B3A-9718-E3F4E2FD5E81.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-49DBE7DD-D1D2-4B3A-9718-E3F4E2FD5E81.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP user input functions.

| User input functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(entsel *[msg]*)](../E_Functions/entsel_AutoLISP.md) | Prompts the user to select a single object (entity) by specifying a point | ✓ | ✓ | ✓ | -- | ✓ |
| [(getangle *[pt] [msg]*)](../G_Functions/getangle_AutoLISP.md) | Pauses for user input of an angle, and returns that angle in radians | ✓ | ✓ | ✓ | -- | ✓ |
| [(getcorner *pt [msg]*)](../G_Functions/getcorner_AutoLISP.md) | Pauses for user input of a rectangle's second corner | ✓ | ✓ | ✓ | -- | ✓ |
| [(getdist *[pt] [msg]*)](../G_Functions/getdist_AutoLISP.md) | Pauses for user input of a distance | ✓ | ✓ | ✓ | -- | ✓ |
| [(getfiled *title default ext flags*)](../G_Functions/getfiled_AutoLISP.md) | Prompts the user for a file name with the standard AutoCAD file dialog box, and returns that file name | ✓ | ✓ | ✓ | -- | / - supported, but does not display the standard AutoCAD file dialog box and always returns a value of 1 |
| [(getint *[msg]*)](../G_Functions/getint_AutoLISP.md) | Pauses for user input of an integer, and returns that integer | ✓ | ✓ | ✓ | -- | ✓ |
| [(getkword *[msg]*)](../G_Functions/getkword_AutoLISP.md) | Pauses for user input of a keyword, and returns that keyword | ✓ | ✓ | ✓ | -- | ✓ |
| [(getorient *[pt] [msg]*)](../G_Functions/getorient_AutoLISP.md) | Pauses for user input of an angle, and returns that angle in radians | ✓ | ✓ | ✓ | -- | ✓ |
| [(getpoint *[pt] [msg]*)](../G_Functions/getpoint_AutoLISP.md) | Pauses for user input of a point, and returns that point | ✓ | ✓ | ✓ | -- | ✓ |
| [(getreal *[msg]*)](../G_Functions/getreal_AutoLISP.md) | Pauses for user input of a real number, and returns that real number | ✓ | ✓ | ✓ | -- | ✓ |
| [(getstring *[cr] [msg]*)](../G_Functions/getstring_AutoLISP.md) | Pauses for user input of a string, and returns that string | ✓ | ✓ | ✓ | -- | ✓ |
| [(initget *[bits] [string]*)](../I_Functions/initget_AutoLISP.md) | Establishes keywords for use by the next user input function call | ✓ | ✓ | ✓ | -- | ✓ |
| [(nentsel *[msg]*)](../N_Functions/nentsel_AutoLISP.md) | Prompts the user to select an object (entity) by specifying a point, and provides access to the definition data contained within a complex object | ✓ | ✓ | ✓ | -- | ✓ |
| [(nentselp *[msg] [pt]*)](../N_Functions/nentselp_AutoLISP.md) | Provides similar functionality to that of the `nentsel`  function without the need for user input | ✓ | ✓ | ✓ | -- | ✓ |
