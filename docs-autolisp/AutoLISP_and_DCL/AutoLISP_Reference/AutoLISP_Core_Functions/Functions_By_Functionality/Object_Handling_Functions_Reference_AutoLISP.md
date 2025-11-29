---
title: "Object-Handling Functions Reference (AutoLISP)"
guid: "GUID-1197B695-C73C-4B4D-AD5D-8F8EB64FB253"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1197B695-C73C-4B4D-AD5D-8F8EB64FB253.htm"
generated: "2025-11-28T19:06:18.585548Z"
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

# Object-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1197B695-C73C-4B4D-AD5D-8F8EB64FB253.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1197B695-C73C-4B4D-AD5D-8F8EB64FB253.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP object-handling functions.

| Object-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(dumpallproperties *ename [context]*)](../D_Functions/dumpallproperties_AutoLISP.md) | Retrieves an entity’s supported properties | ✓ | ✓ | ✓ | -- | -- |
| [(entdel *ename*)](../E_Functions/entdel_AutoLISP.md) | Deletes objects (entities) or restores previously deleted objects | ✓ | ✓ | ✓ | -- | ✓ |
| [(entget *ename [applist]*)](../E_Functions/entget_AutoLISP.md) | Retrieves an object's definition data | ✓ | ✓ | ✓ | -- | ✓ |
| [(entlast)](../E_Functions/entlast_AutoLISP.md) | Returns the name of the last non-deleted main object in the drawing | ✓ | ✓ | ✓ | -- | ✓ |
| [(entmake *[elist]*)](../E_Functions/entmake_AutoLISP.md) | Creates a new entity (graphical object) in the drawing | ✓ | / - supported objects are limited | ✓ | -- | ✓ |
| [(entmakex *[elist]*)](../E_Functions/entmakex_AutoLISP.md) | Makes a new object, gives it a handle and entity name (but does not assign an owner), and then returns the new entity name | ✓ | / - supported objects are limited | ✓ | -- | ✓ |
| [(entmod *elist*)](../E_Functions/entmod_AutoLISP.md) | Modifies the definition data of an object | ✓ | / - supported objects are limited | ✓ | -- | ✓ |
| [(entnext *[ename]*)](../E_Functions/entnext_AutoLISP.md) | Returns the name of the next object in the drawing | ✓ | ✓ | ✓ | -- | ✓ |
| [(entupd *ename*)](../E_Functions/entupd_AutoLISP.md) | Updates the screen image of an object | ✓ | ✓ | ✓ | -- | ✓ |
| [(getpropertyvalue *ename propertyname [or collectionName index name]*)](../G_Functions/getpropertyvalue_AutoLISP.md) | Returns the current value of an entity’s property | ✓ | ✓ | ✓ | -- | -- |
| [(handent *handle*)](../H_Functions/handent_AutoLISP.md) | Returns an object name based on its handle | ✓ | ✓ | ✓ | -- | ✓ |
| [(ispropertyreadonly *ename propertyname [or collectionName index name]*)](../I_Functions/ispropertyreadonly_AutoLISP.md) | Returns the read-only state of an entity’s property | ✓ | ✓ | ✓ | -- | -- |
| [(setpropertyvalue *ename propertyname value [or collectionname index name val]*)](../S_Functions/setpropertyvalue_AutoLISP.md) | Sets the property value for an entity | ✓ | ✓ | ✓ | -- | -- |
| [(vlax-dump-object *obj*)](GUID-BCE56B30-54A6-42F9-8910-81AF2B7B9AA8.htm) | Lists an object's methods and properties  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-erased-p *obj*)](GUID-423F047A-F8B8-4DD0-ABEC-52E3C7B336B5.htm) | Determines whether an object was erased  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-get-acad-object)](GUID-53DB599B-641D-45DD-A201-604942A4596C.htm) | Retrieves the top-level AutoCAD application object for the current AutoCAD session  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-method-applicable-p *obj method*)](GUID-63B81424-11BA-4CB3-A783-514031A2271D.htm) | Determines whether an object supports a particular method  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-object-released-p *obj*)](GUID-23DA7B30-FBB5-40DC-A93E-E1FC2A5D8B6F.htm) | Determines whether an object has been released  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-read-enabled-p *obj*)](GUID-8BBBF683-BA93-4942-9B8C-AD43D490E52A.htm) | Determines whether an object can be read  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-release-object *obj*)](GUID-85FA59E7-9EC9-4690-8CEE-318BF542C0E6.htm) | Releases a drawing object  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-typeinfo-available-p *obj*)](GUID-1DAB4433-A0FA-43EA-9022-68C8E468B1EA.htm) | Determines whether type library information is present for the specified type of object  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
| [(vlax-write-enabled-p *obj*)](GUID-67DAD30A-B359-4566-9B11-C6203EB71247.htm) | Determines whether an AutoCAD drawing object can be modified  Note:  Extended AutoLISP extension: requires `vl-load-com` | ✓ | ✓ | -- | -- | -- |
