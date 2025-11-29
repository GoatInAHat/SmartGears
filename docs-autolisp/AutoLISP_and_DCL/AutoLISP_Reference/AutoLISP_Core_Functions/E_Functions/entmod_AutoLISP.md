---
title: entmod (AutoLISP)
guid: "GUID-C7D27797-247E-49B9-937C-0D8C58F4C832"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C7D27797-247E-49B9-937C-0D8C58F4C832.htm"
generated: "2025-11-28T19:06:28.303449Z"
description: Modifies the definition data of an object (entity)
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

# entmod (AutoLISP)

> Modifies the definition data of an object (entity)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C7D27797-247E-49B9-937C-0D8C58F4C832.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C7D27797-247E-49B9-937C-0D8C58F4C832.htm)
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
(entmod
elist
)
```

- ***elist*:** **Type:**  List  Entity definition data in a format similar to that returned by the `entget`  function.  For entity fields with floating-point values (such as thickness), `entmod`  accepts integer values and converts them to floating point. Similarly, if you supply a floating-point value for an integer entity field (such as color number), `entmod`  truncates it and converts it to an integer.

## Return Values

**Type:**  List or nil

If successful, `entmod`  returns the *elist*  supplied to it. If `entmod`  is unable to modify the specified entity, the function returns `nil`.

## Remarks

The `entmod`  function updates database information for the entity name specified by the -1 group in *elist*. The primary mechanism through which AutoLISP updates the database is by retrieving entities with `entget`, modifying the list defining an entity, and updating the entity in the database with `entmod`. The `entmod`  function can modify both graphical and nongraphical objects.

There are restrictions on the changes the `entmod`  function can make:

- An entity's type and handle cannot be changed. If you want to do this, use
  entdel
   to delete the entity, and then make a new entity with the
  command
   or
  entmake
   function.
- The
  entmod
   function cannot change internal fields, such as the entity name in the -2 group of a seqend entity. Attempts to change such fields are ignored.
-  You cannot use the
  entmod
   function to modify a viewport entity.
- A number of objects are not supported by
  entmod
   in AutoCAD LT; see the Objects Not Supported By
  entmod
   in AutoCAD LT section for a list of the objects that are not supported.

You can change an entity's space visibility field to 0 or 1 (except for viewport objects). If you use `entmod`  to modify an entity within a block definition, the modification affects all instances of the block in the drawing.

Before performing an `entmod`  on vertex entities, you should read or write the polyline entity's header. If the most recently processed polyline entity is different from the one to which the vertex belongs, width information (the 40 and 41 groups) can be lost.

Caution:
 You can use
entmod
 to modify entities within a block definition, but doing so can create a self-referencing block, which will cause AutoCAD to stop.

Note:
 In AutoCAD 2004 and later releases, the
entmod
 function has a new behavior in color operations. DXF group code 62 holds AutoCAD Color Index (ACI) values, but code 420 holds true color values. If the true color value and ACI value conflict, AutoCAD uses the 420 value, so the code 420 value should be removed before attempting to use the code 62 value.

## Examples

The following sequence of commands obtains the properties of an entity, and then modifies the entity.

Set the `en1`  variable to the name of the first entity in the drawing:

```lisp
(setq en1 (entnext))

<Entity name: 2c90520>
```

Set a variable named `ed`  to the entity data of entity `en1`:

```lisp
(setq ed (entget en1))

((-1 . <Entity name: 2c90520>) (0 . "CIRCLE") (5 . "4C") (100 . "AcDbEntity") (67 . 0) (8 . "0")
(100 . "AcDbCircle") (10 3.45373 6.21635 0.0) (40 . 2.94827) (210 0.0 0.0 1.0))
```

Changes the layer group in `ed`  from layer 0 to layer 1:

```lisp
(setq ed (subst (cons 8 "1") (assoc 8 ed) ed ))

((-1 . <Entity name: 2c90520>) (0 . "CIRCLE") (5 . "4C") (100 . "AcDbEntity") (67 . 0) (8 . "1")
(100 . "AcDbCircle") (10 3.45373 6.21635 0.0) (40 . 2.94827) (210 0.0 0.0 1.0))
```

Modify the layer of the `en1`  entity in the drawing:

```lisp
(entmod ed)
((-1 . <Entity name: 2c90520>) (0 . "CIRCLE") (5 . "4C")
(100 . "AcDbEntity") (67 . 0) (8 . "1") (100 . "AcDbCircle") (10 3.45373 6.21635 0.0) (40 . 2.94827) (210 0.0 0.0 1.0))
```

## Objects Not Supported By `entmod`  in AutoCAD LT

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
