---
title: Windows Registry and Property List File Functions Reference (AutoLISP)
guid: "GUID-E3D3DA29-AF63-41D3-865C-39E50FF31596"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E3D3DA29-AF63-41D3-865C-39E50FF31596.htm"
generated: "2025-11-28T19:06:19.488512Z"
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

# Windows Registry and Property List File Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E3D3DA29-AF63-41D3-865C-39E50FF31596.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E3D3DA29-AF63-41D3-865C-39E50FF31596.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

Windows Registry and property list (PLIST) file functions query and update the Windows Registry on Windows or property list files on Mac OS.

| Windows Registry and Property List File functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(vl-registry-delete *reg-key [val-name]*)](../V_Functions/vl_registry_delete_AutoLISP.md) | Deletes the specified key from the Windows Registry or property list file on Mac OS | ✓ | ✓ | ✓ | -- | -- |
| [(vl-registry-descendents *reg-key [val-names]*)](../V_Functions/vl_registry_descendents_AutoLISP.md) | Returns a list of subkeys or value names for the specified key of the Windows Registry or property list file on Mac OS | ✓ | ✓ | ✓ | -- | -- |
| [(vl-registry-read *reg-key [val-name]*)](../V_Functions/vl_registry_read_AutoLISP.md) | Returns data stored by a specific key/value pair in the Windows Registry or property list file on Mac OS | ✓ | ✓ | ✓ | -- | -- |
| [(vl-registry-write *reg-key [val-name val-data]*)](../V_Functions/vl_registry_write_AutoLISP.md) | Creates a key in the Windows Registry or property list file on Mac OS | ✓ | ✓ | ✓ | -- | -- |
| [(vlax-machine-product-key)](GUID-2EB68815-FDF9-4235-918B-025FB9A42220.htm) | Returns the AutoCAD Windows Registry path in the HKLM (HKEY_LOCAL_MACHINE)  Note:  Available on Windows only and requires a call to the `vl-load-com`  function. | ✓ | ✓ | -- | -- | -- |
| [(vlax-product-key)](GUID-237CEDFA-A02E-443E-BB21-5BD86E5E353E.htm) | Returns the AutoCAD Windows Registry path (Obsolete)  Note:  Use the `vlax-machine-product-key`  function instead. | ✓ | ✓ | -- | -- | -- |
| [(vlax-user-product-key)](GUID-42C8BADD-C5BC-4913-A8AF-ECF2ADF42FB6.htm) | Returns the AutoCAD Windows registry path in the HKCU (HKEY_CURRENT_USER)  Note:  Available on Windows only and requires a call to the `vl-load-com`  function. | ✓ | ✓ | -- | -- | -- |
