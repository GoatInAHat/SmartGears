---
title: "Symbol Table and Dictionary-Handling Functions Reference (AutoLISP)"
guid: "GUID-7AACB386-1AA1-4BDF-B5D7-A1E23A51CA15"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7AACB386-1AA1-4BDF-B5D7-A1E23A51CA15.htm"
generated: "2025-11-28T19:06:19.257089Z"
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

# Symbol Table and Dictionary-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7AACB386-1AA1-4BDF-B5D7-A1E23A51CA15.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7AACB386-1AA1-4BDF-B5D7-A1E23A51CA15.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP symbol table and dictionary-handling functions.

| Symbol table and dictionary-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(dictadd *ename symbol newobj*)](../D_Functions/dictadd_AutoLISP.md) | Adds a non-graphical object to the specified dictionary | ✓ | ✓ | ✓ | -- | ✓ |
| [(dictnext *ename symbol [rewind]*)](../D_Functions/dictnext_AutoLISP.md) | Finds the next item in a dictionary | ✓ | ✓ | ✓ | -- | ✓ |
| [(dictremove *ename symbol*)](../D_Functions/dictremove_AutoLISP.md) | Removes an entry from the specified dictionary | ✓ | ✓ | ✓ | -- | ✓ |
| [(dictrename *ename oldsym newsym*)](../D_Functions/dictrename_AutoLISP.md) | Renames a dictionary entry | ✓ | ✓ | ✓ | -- | ✓ |
| [(dictsearch *ename symbol [setnext]*)](../D_Functions/dictsearch_AutoLISP.md) | Searches a dictionary for an item | ✓ | ✓ | ✓ | -- | ✓ |
| [(layoutlist)](../L_Functions/layoutlist_AutoLISP.md) | Returns a list of all paper space layouts in the current drawing | ✓ | ✓ | ✓ | -- | -- |
| [(namedobjdict)](../N_Functions/namedobjdict_AutoLISP.md) | Returns the entity name of the current drawing's named object dictionary, which is the root of all non-graphical objects in the drawing | ✓ | ✓ | ✓ | -- | ✓ |
| [(setview *view_description [vport_id]*)](../S_Functions/setview_AutoLISP.md) | Establishes a view for a specified viewport | ✓ | ✓ | ✓ | -- | ✓ |
| [(snvalid *sym_name*)](../S_Functions/snvalid_AutoLISP.md) | Checks the symbol table name for valid characters | ✓ | ✓ | ✓ | -- | ✓ |
| [(tblnext *table-name [rewind]*)](../T_Functions/tblnext_AutoLISP.md) | Finds the next item in a symbol table | ✓ | ✓ | ✓ | -- | ✓ |
| [(tblobjname *table-name symbol*)](../T_Functions/tblobjname_AutoLISP.md) | Returns the entity name of a specified symbol table entry | ✓ | ✓ | ✓ | -- | ✓ |
| [(tblsearch *table-name symbol [setnext]*)](../T_Functions/tblsearch_AutoLISP.md) | Searches a symbol table for a symbol name | ✓ | ✓ | ✓ | -- | ✓ |
| [(vlax-ldata-delete *dict key*)](GUID-06F6C8B7-A55C-42DE-BD51-A9FAB68260D3.htm) | Erases AutoLISP data from a drawing dictionary  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-ldata-get *dict key [default-data]*)](GUID-C279216F-9568-4176-A9E7-BCB45E3F59C7.htm) | Retrieves AutoLISP data from a drawing dictionary  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-ldata-list *dict*)](GUID-A124867A-FBB8-4B36-9BB0-B50D971C91A1.htm) | Lists AutoLISP data in a drawing dictionary  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-ldata-put *dict key data*)](GUID-D2459C22-223C-4C8E-A69C-F288B8FBFA28.htm) | Stores AutoLISP data in a drawing dictionary  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-ldata-test *data*)](GUID-CD72D160-25BF-43F5-BD30-AC715F4A2426.htm) | Determines whether data can be saved over a session boundary  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
