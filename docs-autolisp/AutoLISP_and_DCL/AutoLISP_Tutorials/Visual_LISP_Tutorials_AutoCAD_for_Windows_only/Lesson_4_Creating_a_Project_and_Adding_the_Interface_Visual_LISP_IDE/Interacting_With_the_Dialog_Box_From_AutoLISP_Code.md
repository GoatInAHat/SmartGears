---
title: Interacting With the Dialog Box From AutoLISP Code
guid: "GUID-C8EAF94C-DF0E-4DB2-A8F5-4361873F0957"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-C8EAF94C-DF0E-4DB2-A8F5-4361873F0957.htm"
generated: "2025-11-28T19:07:00.563005Z"
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

# Interacting With the Dialog Box From AutoLISP Code

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-C8EAF94C-DF0E-4DB2-A8F5-4361873F0957.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-C8EAF94C-DF0E-4DB2-A8F5-4361873F0957.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You now need to program your AutoLISP function to interact with the dialog box. The stubbed-out function, `gp:getDialogInput`, is where this activity will take place. This function now lives in the *gp-io.lsp*  file, which you earlier extracted from *gpmain.lsp*.

Developing a dialog box interface can be confusing the first few times you do it. It involves planning ahead and asking yourself such questions as:

- Does the dialog box need to be set up with default values?
- What happens when the user chooses a button or enters a value?
- What happens when the user chooses Cancel?
- If the dialog (.
  dcl
  ) file is missing, what needs to occur?
