---
title: "Application-Handling Functions Reference (AutoLISP)"
guid: "GUID-E2B683FE-43FB-4F5D-A9AE-809772FE8D3B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E2B683FE-43FB-4F5D-A9AE-809772FE8D3B.htm"
generated: "2025-11-28T19:06:16.223850Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/07/2024
topic_subtype:
  - autolisp
---

# Application-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E2B683FE-43FB-4F5D-A9AE-809772FE8D3B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E2B683FE-43FB-4F5D-A9AE-809772FE8D3B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/07/2024

The following table provides summary descriptions of the AutoLISP application-handling functions.

| Application-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(arx)](../A_Functions/arx_AutoLISP.md) | Returns a list of the currently loaded ObjectARX applications | ✓ | ✓ | ✓ | -- | ✓ |
| [(arxload *application [onfailure]*)](../A_Functions/arxload_AutoLISP.md) | Loads an ObjectARX application | ✓ | ✓ | ✓ | -- | -- |
| [(arxunload *application [onfailure]*)](../A_Functions/arxunload_AutoLISP.md) | Unloads an ObjectARX application | ✓ | ✓ | ✓ | -- | -- |
| [(autoarxload *filename cmdlist*)](../A_Functions/autoarxload_AutoLISP.md) | Predefines command names to load an associated ObjectARX file | ✓ | ✓ | ✓ | -- | -- |
| [(autoload *filename cmdlist*)](../A_Functions/autoload_AutoLISP.md) | Predefines command names to load an associated AutoLISP file | ✓ | ✓ | ✓ | -- | -- |
| [(initdia *[dialogflag]*)](../I_Functions/initdia_AutoLISP.md) | Forces the display of the next command's dialog box | ✓ | ✓ | ✓ | -- | -- |
| [(load *filename [onfailure]*)](../L_Functions/load_AutoLISP.md) | Evaluates the AutoLISP expressions in a file | ✓ | ✓ | ✓ | -- | ✓ |
| [(showhtmlmodalwindow *uri*)](../S_Functions/showhtmlmodalwindow_AutoLISP.md) | Displays a modal dialog box with a specified URI (Uniform Resource Identifier) | ✓ | ✓ | -- | -- | -- |
| [(startapp *appcmd file*)](../S_Functions/startapp_AutoLISP.md) | Starts a Windows application | ✓ | ✓ | ✓ | -- | -- |
| [(vl-load-all *filename*)](../V_Functions/vl_load_all_AutoLISP.md) | Loads a file into all open AutoCAD documents | ✓ | ✓ | ✓ | -- | -- |
| [(vl-vbaload *"filename"*)](../V_Functions/vl_vbaload_AutoLISP.md) | Loads a VBA project | ✓ | -- | -- | -- | -- |
| [(vl-vbarun *"macroname"*)](../V_Functions/vl_vbarun_AutoLISP.md) | Runs a VBA macro | ✓ | -- | -- | -- | -- |
| [(vlax-add-cmd *"global-name" 'func-sym [“local-name" cmd-flags]*)](GUID-42392DA4-4A2B-4D34-AA7B-A5ACAF727E54.htm) | Adds commands to the AutoCAD built-in command set | ✓ | ✓ | ✓ | -- | / - supported, but doesn't affect the program |
