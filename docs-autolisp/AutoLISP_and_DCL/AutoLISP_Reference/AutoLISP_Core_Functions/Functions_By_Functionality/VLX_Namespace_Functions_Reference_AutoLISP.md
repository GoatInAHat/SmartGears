---
title: VLX Namespace Functions Reference (AutoLISP)
guid: "GUID-5784FC6F-82DD-4459-879B-6EC3BD5E88D1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5784FC6F-82DD-4459-879B-6EC3BD5E88D1.htm"
generated: "2025-11-28T19:06:19.414373Z"
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

# VLX Namespace Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5784FC6F-82DD-4459-879B-6EC3BD5E88D1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5784FC6F-82DD-4459-879B-6EC3BD5E88D1.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The VLX namespace functions listed below apply to separate-namespace VLX applications. These functions allow separate-namespace VLX functions to be accessible from a document namespace, enable the retrieval and updating of variables in the associated document namespace, and provide error-handling routines for separate-namespace VLX functions.

| VLX namespace functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(vl-arx-import *[ function | application]*)](../V_Functions/vl_arx_import_AutoLISP.md) | Imports ADS-DEFUN functions into a separate-namespace VLX | ✓ | ✓ | / - supported, but behavior is different than on Windows | -- | / - supported, but behavior is different than on Windows |
| [(vl-doc-export *'function*)](../V_Functions/vl_doc_export_AutoLISP.md) | Makes a function loaded in a VLX namespace available to the current document | ✓ | ✓ | / - supported, but behavior is different than on Windows | -- | / - supported, but behavior is different than on Windows |
| [(vl-doc-import *['function | application]*)](../V_Functions/vl_doc_import_AutoLISP.md) | Imports a function that was previously exported from another separate-namespace VLX | ✓ | ✓ | / - supported, but behavior is different than on Windows | -- | / - supported, but behavior is different than on Windows |
| [(vl-doc-ref *symbol*)](../V_Functions/vl_doc_ref_AutoLISP.md) | Retrieves the value of a variable from the namespace of the associated document | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-doc-set *symbol value*)](../V_Functions/vl_doc_set_AutoLISP.md) | Sets the value of a variable in the associated document's namespace | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-exit-with-error *"msg"*)](../V_Functions/vl_exit_with_error_AutoLISP.md) | Passes control from a VLX error handler to the `*error*`  function of the associated document namespace | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-exit-with-value *value*)](../V_Functions/vl_exit_with_value_AutoLISP.md) | Returns a value to the document namespace from which the VLX was invoked | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-list-exported-functions *[“appname”]*)](../../Visual_LISP_IDE_Related_Functions_AutoCAD_for_Windows_Only/vl_list_exported_functions_AutoLISP_Visual_LISP_IDE.md) | Lists all functions exported by the specified application, or all exported functions if no application is specified | ✓ | ✓ | / - supported, but does not affect the program | -- | / - supported, but does not affect the program |
| [(vl-list-loaded-vlx)](../V_Functions/vl_list_loaded_vlx_AutoLISP.md) | Returns a list of all separate-namespace VLX files associated with the current document | ✓ | ✓ | / - supported, but does not affect the program | -- | / - supported, but does not affect the program |
| [(vl-unload-vlx *"appname"*)](../V_Functions/vl_unload_vlx_AutoLISP.md) | Unloads a VLX that is loaded in its own namespace (a separate-namespace VLX) | ✓ | ✓ | / - supported, but does not affect the program | -- | / - supported, but does not affect the program |
| [(vl-vlx-loaded-p *"appname"*)](../V_Functions/vl_vlx_loaded_p_AutoLISP.md) | Determines whether a VLX is loaded in its own namespace | ✓ | ✓ | / - supported, but does not affect the program | -- | / - supported, but does not affect the program |
