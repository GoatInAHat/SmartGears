---
title: "Error-Handling Functions Reference (AutoLISP)"
guid: "GUID-D114F1C4-DE8E-422D-8F3F-C43707B8212B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D114F1C4-DE8E-422D-8F3F-C43707B8212B.htm"
generated: "2025-11-28T19:06:16.876559Z"
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

# Error-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D114F1C4-DE8E-422D-8F3F-C43707B8212B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D114F1C4-DE8E-422D-8F3F-C43707B8212B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP error-handling functions.

| Error-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(*error* *string*)](../Non_alphabetic_Functions/error_AutoLISP.md) | A user-definable error-handling function | ✓ | ✓ | ✓ | -- | ✓ |
| [(*pop-error-mode*)](../Non_alphabetic_Functions/pop_error_mode_AutoLISP.md) | Error-handling function that ends the previous call to `*push-error-using-command*`  or `*push-error-using-stack*` | ✓ | ✓ | ✓ | -- | ✓ |
| [(*push-error-using-command*)](../Non_alphabetic_Functions/push_error_using_command_AutoLISP.md) | Error-handling function that indicates the use of the command function within a custom `*error*`  handler | ✓ | ✓ | ✓ | -- | ✓ |
| [(*push-error-using-stack*)](../Non_alphabetic_Functions/push_error_using_stack_AutoLISP.md) | Error-handling function that indicates the use of variables from the AutoLISP stack within a custom `*error*`  handler | ✓ | ✓ | ✓ | -- | ✓ |
| [(alert *string*)](../A_Functions/alert_AutoLISP.md) | Displays an alert dialog box with the error or warning message passed as a string | ✓ | ✓ | ✓ | -- | / - supported, but doesn't display the alert box |
| [(exit)](../E_Functions/exit_AutoLISP.md) | Forces the current application to quit | ✓ | ✓ | ✓ | -- | ✓ |
| [(quit)](../Q_Functions/quit_AutoLISP.md) | Forces the current application to quit | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-catch-all-apply *'function list*)](../V_Functions/vl_catch_all_apply_AutoLISP.md) | Passes a list of arguments to a specified function and traps any exceptions | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-catch-all-error-message *error-obj*)](../V_Functions/vl_catch_all_error_message_AutoLISP.md) | Returns a string from an error object | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-catch-all-error-p *arg*)](../V_Functions/vl_catch_all_error_p_AutoLISP.md) | Determines whether an argument is an error object returned from `vl-catch-all-apply` | ✓ | ✓ | ✓ | -- | ✓ |
