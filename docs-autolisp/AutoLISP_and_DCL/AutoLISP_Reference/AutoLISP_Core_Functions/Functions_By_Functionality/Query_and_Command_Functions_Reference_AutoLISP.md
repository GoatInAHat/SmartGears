---
title: Query and Command Functions Reference (AutoLISP)
guid: "GUID-F9ABE09D-9A71-4C00-8E5D-5BA70C3797B5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F9ABE09D-9A71-4C00-8E5D-5BA70C3797B5.htm"
generated: "2025-11-28T19:06:18.670099Z"
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

# Query and Command Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F9ABE09D-9A71-4C00-8E5D-5BA70C3797B5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F9ABE09D-9A71-4C00-8E5D-5BA70C3797B5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP query and command functions.

| Query and command functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(acad_colordlg *colornum [flag]*)](../A_Functions/acad_colordlg_AutoLISP.md) | Displays the standard AutoCAD Color Selection dialog box | ✓ | ✓ | -- | -- | -- |
| [(acad_helpdlg *helpfile topic*)](../A_Functions/acad_helpdlg_AutoLISP.md) | Invokes the Help facility (obsolete) | ✓ | -- | -- | -- | -- |
| [(command *[arguments] ...*)](../C_Functions/command_AutoLISP.md) | Executes an AutoCAD command | ✓ | ✓ | ✓ | -- | ✓ |
| [(command-s *[arguments] ...*)](../C_Functions/command_s_AutoLISP.md) | Executes an AutoCAD command and the supplied input | ✓ | ✓ | ✓ | -- | ✓ |
| [(getcfg *cfgname*)](../G_Functions/getcfg_AutoLISP.md) | Retrieves application data from the AppData section of the *acadXXXX.cfg*  file (obsolete) | ✓ | ✓ | ✓ | -- | ✓ |
| [(getcname *cname*)](../G_Functions/getcname_AutoLISP.md) | Retrieves the localized or English name of an AutoCAD command | ✓ | ✓ | ✓ | -- | ✓ |
| [(getenv *"variable-name"*)](../G_Functions/getenv_AutoLISP.md) | Returns the string value assigned to an environment variable | ✓ | ✓ | ✓ | -- | ✓ |
| [(getvar *"varname"*)](../G_Functions/getvar_AutoLISP.md) | Retrieves the value of an AutoCAD system variable | ✓ | ✓ | ✓ | -- | ✓ |
| [(help *[helpfile [topic [command]]]*)](../H_Functions/help_AutoLISP.md) | Invokes the Help facility | ✓ | ✓ | ✓ | -- | / - supported, but doesn't do anything |
| [(setcfg *cfgname cfgval*)](../S_Functions/setcfg_AutoLISP.md) | Writes application data to the AppData section of the *acadXXXX.cfg*  file (obsolete) | ✓ | ✓ | ✓ | -- | ✓ |
| [(setenv *"varname" "value"*)](../S_Functions/setenv_AutoLISP.md) | Sets an environment variable to a specified value | ✓ | ✓ | ✓ | -- | ✓ |
| [(setfunhelp *"c:fname" ["helpfile" ["topic" ["command"]]]*)](../S_Functions/setfunhelp_AutoLISP.md) | Registers a user-defined command with the Help facility so the appropriate help file and topic are called when the user requests help on that command | ✓ | ✓ | ✓ | -- | / - supported, but doesn't do anything |
| [(setvar *"varname" value*)](../S_Functions/setvar_AutoLISP.md) | Sets an AutoCAD system variable to a specified value | ✓ | ✓ | ✓ | -- | ✓ |
| [(ver)](../V_Functions/ver_AutoLISP.md) | Returns a string that contains the current AutoLISP version number | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-cmdf *[arguments] ...*)](../V_Functions/vl_cmdf_AutoLISP.md) | Executes an AutoCAD command after evaluating *arguments* | ✓ | ✓ | ✓ | -- | ✓ |
| [(vlax-add-cmd *global-name func-sym [local-name cmd-flags]*)](GUID-42392DA4-4A2B-4D34-AA7B-A5ACAF727E54.htm) | Adds commands to a group | ✓ | ✓ | ✓ | -- | ✓ |
| [(vlax-remove-cmd *global-name*)](GUID-EA671EF1-4EF9-49FE-9166-E241657198A1.htm) | Removes a single command or command group | ✓ | ✓ | ✓ | -- | ✓ |
