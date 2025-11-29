---
title: entmakex (AutoLISP)
guid: "GUID-3F9A2EB2-082D-49DD-8EFD-DAD8F6E9AA6A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F9A2EB2-082D-49DD-8EFD-DAD8F6E9AA6A.htm"
generated: "2025-11-28T19:06:28.213940Z"
description: Makes a new object or entity, gives it a handle and entity name (but does not assign an owner), and then returns the new entity name
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

# entmakex (AutoLISP)

> Makes a new object or entity, gives it a handle and entity name (but does not assign an owner), and then returns the new entity name

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F9A2EB2-082D-49DD-8EFD-DAD8F6E9AA6A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F9A2EB2-082D-49DD-8EFD-DAD8F6E9AA6A.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(entmakex
[elist]
)
```

- ***elist*:** **Type:**  List  Entity definition data in a format similar to that returned by the `entget`  function. The *elist*  argument must contain all of the information necessary to define the entity. If any required definition data is omitted, `entmakex`  returns `nil`  and the entity is rejected. If you omit optional definition data (such as the layer), `entmakex`  uses the default value.

## Return Values

**Type:**  Ename (entity name)

If successful, `entmakex`  returns the name of the entity created. If `entmakex`  is unable to create the entity, the function returns `nil`.

## Remarks

The `entmakex`  function can define both graphical and nongraphical entities.

A number of objects are not supported by `entmakex`  in AutoCAD LT; see the Objects Not Supported By `entmakex`  in AutoCAD LT section for a list of the objects that are not supported.

## Examples

```lisp
(entmakex '((0 . "CIRCLE") (62 . 1) (10 4.0 3.0 0.0) (40 . 1.0)))

<Entity name: 1d45558>
```

Caution:
 Objects and entities without owners are not written out to
DWG
 or
DXF
 files. Be sure to set an owner at some point after using
entmakex
. For example, you can use
dictadd
 to set a dictionary to own an object.

## Objects Not Supported By `entmakex`  in AutoCAD LT

| Graphical Objects | Nongraphgical Objects |  |  |
| --- | --- | --- | --- |
| Class Name | DXF Name | Class Name | DXF Name |
| AcDb3dSolid | 3DSOLID | AcDbIBLBackground | RAPIDRTRENDERENVIRONMENT |
| AcDbAssocExternalPersSubentIdHolder | ACDBASSOCEXTERNALPERSSUBENTIDHOLDER | AcDbLightList | LIGHTLIST |
| AcDbCamera | CAMERA | AcDbMotionPath | ACDBMOTIONPATH |
| AcDbExtrudedSurface | EXTRUDEDSURFACE | AcDbMaterial | MATERIAL |
| AcDbFace | 3DFACE | AcDbMentalRayRenderSettings | MENTALRAYRENDERSETTINGS |
| AcDbHelix | HELIX | AcDbMlineStyle | MLINESTYLE |
| AcDbLight | LIGHT | AcDbNavisworksModelDef | AcDbNavisworksModelDef |
| AcDbLoftedSurface | LOFTEDSURFACE | AcDbRapidRTRenderSettings | RAPIDRTRENDERSETTINGS |
| AcDbMInsertBlock | INSERT | AcDbRenderEnvironment | RENDERENVIRONMENT |
| AcDbMline | MLINE | AcDbRenderGlobal | RENDERGLOBAL |
| AcDbNavisworksModel | Coordination Model | AcDbRenderSettings | RENDERSETTINGS |
| AcDbNurbSurface | NURBSURFACE | AcDbSectionManager | SECTION_MANAGER |
| AcDbPlaneSurface | PLANESURFACE | AcDbSectionSettings | SECTION_SETTINGS |
| AcDbPointCloudEx | ACDBPOINTCLOUDEX | AcDbSectionViewStyle | ACDBSECTIONVIEWSTYLE |
| AcDbPolyFaceMesh | POLYLINE | AcDbSun | SUN |
| AcDbPolygonMesh | POLYLINE | AcDbSolidBackground | SOLID_BACKGROUND |
| AcDbRevolvedSurface | REVOLVEDSURFACE | AcDbSkyBackground | SKYLIGHT_BACKGROUND |
| AcDbSection | SECTIONOBJECT | AcDbVbaProject | XRECORD |
| AcDbShape | SHAPE | AcDbXrecord | XRECORD |
| AcDbSubDMesh | MESH | AcDbPointCloudDefEx | ACDBPOINTCLOUDDEF_EX |
| AcDbSurface | SURFACE | AcDbPointCloudDefReactorEx | ACDBPOINTCLOUDDEF_REACTOR_EX |
| AcDbSweptSurface | SWEPTSURFACE | AcDbPointCloudColorMap | ACDBPOINTCLOUDCOLORMAP |
|  |  | AcDbPersSubentManager | ACDBPERSSUBENTMANAGER |
|  |  | AcDbEvalGraph | ACAD_EVALUATION_GRAPH |
|  |  | AcDbDictionaryVar | DICTIONARYVAR |
|  |  | AcDbAssocPersSubentManager | ACDBASSOCPERSSUBENTMANAGER |
|  |  | AcDbAssocNamespace | ACDBASSOCNAMESPACE |
|  |  | AcDbAssocManager | ACDBASSOCMANAGER |
|  |  | AcDbAssocDependency | ACDBASSOCDEPENDENCY |
|  |  | AcDbAssocValueDependency | ACDBASSOCVALUEDEPENDENCY |
|  |  | AcDbAssocGeomDependency | ACDBASSOCGEOMDEPENDENCY |
|  |  | AcDbAssocDimDependencyBody | ASSOCDIMDEPENDENCYBODY |
|  |  | AcDbAssocAction | ACDBASSOCACTION |
|  |  | AcDbAssocVariable | ACDBASSOCVARIABLE |
|  |  | AcDbAssocNetwork | ACDBASSOCNETWORK |
|  |  | AcDbAssoc2dConstraintGroup | ACDBASSOC2DCONSTRAINTGROUP |
|  |  | AcDbAssocSetObjectPropertyActionBody | ACDBASSOCSETOBJECTPROPERTYACTIONBODY |
|  |  | AcDbAssocRestoreEntityStateActionBody | ACDBASSOCRESTOREENTITYSTATEACTIONBODY |
|  |  | AcDbAssocPositionEntityActionBody | ACDBASSOCPOSITIONENTITYACTIONBODY |
|  |  | AcDbAssocImpliedSurfaceOrSolidActionBody | ACDBASSOCIMPLIEDSURFACEORSOLIACTIONBODY |
|  |  | AcDbAssocCloneAndPositionEntityActionBody | ACDBASSOCCLONEANDPOSITIONENTITYACTIONBODY |
|  |  | AcDbAssocBoolOperActionBody | ACDBASSOCBOOLOPERACTIONBODY |
|  |  | AcDbAssocRadialDimLargeActionBody | ACDBASSOCRADIALDIMLARGEACTIONBODY |
|  |  | AcDbAssocLeaderActionBody | ACDBASSOCLEADERACTIONBODY |
|  |  | AcDbAssocMLeaderActionBody | ACDBASSOCMLEADERACTIONBODY |
|  |  | AcDbAssocArcDimensionActionBody | ACDBASSOCARCDIMENSIONACTIONBODY |
|  |  | AcDbAssocAlignedDimActionBody | ACDBASSOCALIGNEDDIMACTIONBODY |
|  |  | AcDbAssocEntityCloneActionBody | ACDBASSOCENTITYCLONEACTIONBODY |
|  |  | AcDbAssocArrayActionBody | ACDBASSOCARRAYACTIONBODY |
|  |  | AcDbAssocArrayModifyActionBody | ACDBASSOCARRAYMODIFYACTIONBODY |
|  |  | AcDbAssocTrimSurfaceActionBody | ACDBASSOCTRIMSURFACEACTIONBODY |
|  |  | AcDbAssocSweptSurfaceActionBody | ACDBASSOCSWEPTSURFACEACTIONBODY |
|  |  | AcDbAssocRevolvedSurfaceActionBody | ACDBASSOCREVOLVEDSURFACEACTIONBODY |
|  |  | AcDbAssocPlaneSurfaceActionBody | ACDBASSOCPLANESURFACEACTIONBODY |
|  |  | AcDbAssocNetworkSurfaceActionBody | ACDBASSOCNETWORKSURFACEACTIONBODY |
|  |  | AcDbAssocLoftedSurfaceActionBody | ACDBASSOCLOFTEDSURFACEACTIONBODY |
|  |  | AcDbAssocExtrudedSurfaceActionBody | ACDBASSOCEXTRUDEDSURFACEACTIONBODY |
|  |  | AcDbAssocEdgeFilletActionBody | ACDBASSOCEDGEFILLETACTIONBODY |
|  |  | AcDbAssocEdgeChamferActionBody | ACDBASSOCEDGECHAMFERACTIONBODY |
|  |  | AcDbAssocBlendSurfaceActionBody | ACDBASSOCBLENDSURFACEACTIONBODY |
|  |  | AcDbAssocObjectActionParam | ACDBASSOCOBJECTACTIONPARAM |
|  |  | AcDbAssocFaceActionParam | ACDBASSOCFACEACTIONPARAM |
|  |  | AcDbAssocTrimmingBodyActionParam | ACDBASSOCTRIMMINGBODYACTIONPARAM |
|  |  | AcDbAssocEdgeActionParam | ACDBASSOCEDGEACTIONPARAM |
|  |  | AcDbAssocCompoundActionParam | ACDBASSOCCOMPOUNDACTIONPARAM |
|  |  | AcDbAssocOsnapPointRefActionParam | ACDBASSOCOSNAPPOINTREFACTIONPARAM |
|  |  | AcDbAssocPathActionParam | ACDBASSOCPATHACTIONPARAM |
|  |  | AcDbAssocTrimmingPathActionParam | ACDBASSOCTRIMMINGPATHACTIONPARAM |
|  |  | AcDbAssocCoordSystemActionParam | ACDBASSOCCOORDSYSTEMACTIONPARAM |
|  |  | AcDbAssocAsmBodyActionParam | ACDBASSOCASMBODYACTIONPARAM |
|  |  | AcDbAssocVertexActionParam | ACDBASSOCVERTEXACTIONPARAM |
|  |  | AcDbSectionViewStyle | ACDBSECTIONVIEWSTYLE |
