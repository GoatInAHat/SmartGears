---
title: What's New or Changed with AutoLISP (AutoLISP)
guid: "GUID-037BF4D4-755E-4A5C-8136-80E85CCEDF3E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-037BF4D4-755E-4A5C-8136-80E85CCEDF3E.htm"
generated: "2025-11-28T19:05:58.147279Z"
description: The following is an overview of the changes made to AutoLISP in recent releases.
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/04/2024
topic_subtype:
  - autolisp
tags:
  - AutoLISP changes
  - API changes
  - new AutoLISP
  - what is new in AutoLISP
  - what's new in AutoLISP
  - changed functions
  - new functions
---

# What's New or Changed with AutoLISP (AutoLISP)

> The following is an overview of the changes made to AutoLISP in recent releases.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-037BF4D4-755E-4A5C-8136-80E85CCEDF3E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-037BF4D4-755E-4A5C-8136-80E85CCEDF3E.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/04/2024
- Keywords: AutoLISP changes, API changes, new AutoLISP, what is new in AutoLISP, what's new in AutoLISP, changed functions, new functions

## AutoCAD 2025

`acet-load-expresstools`  function was added to AutoCAD for Windows only. This function is used to initialize the Express Tools functions.

## AutoCAD 2024

- **New:** No new or changed functions  AutoCAD LT for Windows now supports AutoLISP and DCL Here are some of the known limitations or differences from AutoCAD:  Most `VL*`, `VLA*`, `VLAX*`, and `VLR*`  functions are supported, but the use of third-party automation libraries is not supported in to AutoCAD LT. Here is a high-level summary of the functions that are not supported:  `vlax-create-object`  `vlax-get-object`  `vlax-get-or-create-object`  `vlax-import-type-library`  `vla-GetInterfaceObject`  `VLA*`  functions related to creating and modifying 3D solid and surface, helix, material, multiline objects among others that can only be created in AutoCAD  `entmake`, `entmakex`, and `entmod`  functions only allow for the creation and modification of objects supported in AutoCAD LT  AutoLISP functions exposed by custom ObjectARX and Managed .NET programs can't be used  ActiveX limitations: `PreferencesProfiles`  object exists as part of the ActiveX implementation, but all of its methods and properties have been removed since profiles are not supported in AutoCAD LT for Windows  Creation of 3D meshes, surfaces, and solids is not supported, while support is limited for the modification of 3D objects  Developing AutoLISP program limitations: Visual LISP integrated development environment (IDE) and VLIDE/VLISP commands are not available in AutoCAD LT for Windows  Debugging with the AutoLISP Extension in Visual Studio Code is not supported in AutoCAD LT for Windows  Deploying AutoLISP program limitations: MNL files are not automatically loaded with CUIx files that have the same name, but the files can be loaded using the AutoLISP `LOAD`  function from another LISP file  Compiled LSP files are supported in AutoCAD LT for Windows, but compiling LSP files can only be done in AutoCAD for Windows only  Programs that utilize functions and commands that are limited to AutoCAD should check which product they are being loaded into to avoid compatibility problems. This can be done using the PROGRAM system variable, a value of `acadlt`  is returned for AutoCAD LT.  The following example restricts the loading of a DVB file and the definition of a command that runs a VBA macro based on if the code is loaded into AutoCAD or AutoCAD LT:  (if (/= (strcase (getvar "PROGRAM") T) "acadlt") (progn (vl-load-com) ;; Load a DVB file (setq retVal (vl-catch-all-apply 'vl-vbaload (list (findfile "sample/vba/drawline.dvb")))) ;; If the DVB file was found, then define the function to run the function (if (not (vl-catch-all-error-p retVal)) (defun c:DRAWLINE (/)(vl-vbarun "drawline")) (prompt "\ndrawline.dvb is missing") ) (prompt "\nEnter DRAWLINE to run the VBA macro.")(princ) ) (progn (prompt "\nVBA macros are not supported on AutoCAD LT.")(princ)) )

## AutoCAD 2023

- No new or changed functions
-  AutoLISP is now available on AutoCAD web for AutoCAD subscribers only

## AutoCAD 2022

No new or changed functions.

## AutoCAD 2021

- **New:** **AutoCAD AutoLISP Extension for Visual Studio (VS) Code**  - Allows for the editing and debugging of AutoLISP files with VS Code on Windows or Mac OS. The LISPSYS system variable must to set to 1 or 2 in order to debug AutoLISP files with the AutoCAD AutoLISP Extension. When LISPSYS is set to 0, the legacy AutoLISP engine and Visual LISP IDE are used for editing and debugging AutoLISP files.  **DCL support on Mac OS**  - Dialog boxes defined using DCL can now be displayed with AutoLISP. All DCL tiles supported on Windows are also supported on Mac OS, but not all tile attributes are supported on Mac OS.
- **Changed:** These functions were updated to support Unicode character strings/codes:  **[ascii](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-03E1B586-72CB-4AB6-A151-B581AE530318)**  - Returns the conversion of the first character of a string into its Unicode character code (an integer).  **[chr](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-5846BFDD-AE5F-4B2F-99E0-0B9DBA667739)**  - Converts an integer representing a Unicode character code into a single-character string.  **[load](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-F3639BAA-FD70-487C-AEB5-9E6096EC0255)**  - Evaluates the AutoLISP expressions in a file.  **[open](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-089A323F-21FF-4337-99A9-375758E23BA4)**  - Opens a file for access by the AutoLISP I/O functions. A new optional argument has been added which allows for the specification of the character encoding to be used when reading/writing the file. When the argument isn't provided, the file is assumed to contain a multibyte character set (MBCS) which is the legacy behavior.  **[read-char](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-7E94BD14-F018-47D0-88DA-2B08DE32DB2C)**  - Returns the integer representing the Unicode character read from the keyboard input buffer or from an open file.  **[read-line](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-AC74D827-0969-4888-91C0-C3149DEC3659)**  - Reads a string from the keyboard or from an open file, until an end-of-line marker is encountered.  **[strlen](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-F16AAC5F-5C87-4DC5-A7B9-BDCD25DC507A)**  - Returns an integer that is the number of characters in a string.  **[substr](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-36F8701B-7AB7-47BE-AC31-8508A2DF46A2)**  - Returns a substring of a string.  **[vl-directory-files](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-C28C0CB0-FBBE-4AA8-BAC4-2FF222772514)**  - Lists all files in a given directory.  **[vl-file-copy](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-12910252-217C-427D-8874-1323611FA74C)**  - Copies or appends the contents of one file to another file.  **[vl-file-delete](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-4CEDC718-EEBB-48A8-A9D3-387F216C12B8)**  - Deletes a file.  **[vl-file-directory-p](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-7EEA5563-33A7-453C-9E96-860F7565808B)**  - Determines if a file name refers to a directory.  **[vl-file-rename](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-188B957C-4CDF-41C7-99BE-8080FA587F74)**  - Renames a file.  **[vl-file-size](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-646032BF-CAC1-4166-9EBA-2C954DFF3005)**  - Determines the size of a file, in bytes.  **[vl-file-systime](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-7F6A31B2-7D51-4C4D-B239-206F398D67CA)**  - Returns last modification time of the specified file.  **[vl-filename-mktemp](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-F417A5EF-95BB-47EE-B60E-7C017635580D)**  - Calculates a unique file name to be used for a temporary file.  **[vl-list->string](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-02C7058A-F648-48F5-BAF6-2A62EABD4DF6)**  - Combines the Unicode characters associated with a list of integers into a string.  **[vl-mkdir](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-02D5CC9E-9394-4630-AE14-133F5F03D7B1)**  - Creates a directory.  **[vl-string->list](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-3C1B15F7-CF97-4783-A725-231C3A1ABB11)**  - Converts a string into a list of Unicode character codes.  **[vl-string-elt](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-5A68B31E-1EA7-434A-93FF-C05D9514A55B)**  - Returns the Unicode representation of the character at a specified position in a string.  **[vl-string-mismatch](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-D1EA6F47-146E-44BC-BBCA-40B7A874C0B8)**  - Returns the length of the longest common prefix for two strings, starting at specified positions.  **[vl-string-position](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-2B0BF48B-FA26-431B-8EC3-DD0BD9F147A0)**  - Looks for a character with the specified Unicode code in a string.  **[vl-string-search](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-4E7AE1DA-1ED5-4D96-A7D3-34241AA94AA6)**  - Searches for the specified pattern in a string.  **[vl-string-subst](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-D8EE91DC-D4DB-43E0-9AFE-5FA166C0896D)**  - Substitutes one string for another, within a string.  **[vl-string-translate](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-57060085-C79D-4613-B438-506AC443BCE7)**  - Replaces characters in a string with a specified set of characters.  **[vl-vbaload](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-54FD698E-8D94-4E27-9DF1-3F7E9D451E05)**  - Loads a VBA project.  **[vlisp-compile](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-8184531C-EDE5-4949-9EAE-25CE98BFF89C)**  - Compiles AutoLISP source code into a FAS file.  **[write-char](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-83AA4A55-C6A0-447B-A106-717E08D2EAF1)**  - Writes one Unicode character to the screen or an open file.  **[write-line](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-CB4F3ABC-F0F6-41DA-A911-75B90D9F974A)**  - Writes a string to the screen or to an open file.
- **Obsolete:** **Visual LISP IDE (AutoCAD for Windows Only)**  - The Visual LISP (VL) IDE has been retired and will be removed in a future release. It is recommended to use the AutoCAD AutoLISP Extension for Visual Studio (VS) Code creating new and updating existing AutoLISP programs. LISPSYS must be set to 0 before the VL IDE can be used to edit and debug AutoLISP files.

## AutoCAD 2020

No new or changed functions.

## AutoCAD 2019

No new or changed functions.

## AutoCAD 2018

No new or changed functions.

## AutoCAD 2017

No new or changed functions.

## AutoCAD 2016

- **Changed:** **[osnap](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-33498AA8-EDC0-45A3-9603-47A3F3510924)**  - Returns a 3D point that is the result of applying an Object Snap mode to a specified point. The function no longer accepts the `qui`  mode. Using the `qui`  mode results in a value of `nil`  to be returned, even if other modes are specified.
- **Obsolete:** **[getcfg](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-D8D46A8C-82DF-4193-A1B7-3DC3CAC70650)**  - Retrieves application data from the AppData section of the *acad20xx.cfg*  file.  **[setcfg](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-8BABF3B0-C8E1-49C9-A367-9823211C1F6B)**  - Writes application data to the AppData section of the *acad20xx.cfg*  file.

Note:

getcfg
 and
setcfg
 are still available for compatibility, but might be removed in a future release. It is recommended to use the
vl-registry-read
 and
vl-registry-write
 functions as replacements.

## AutoCAD 2015

No new or changed functions.

## AutoCAD 2014

- **New:** **[findtrustedfile](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-456F421A-DED1-4A87-9D4A-8434D1E997E1)**  - Searches the AutoCAD trusted file paths for the specified file.  **[showHTMLModalWindow](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-1330BB1E-866E-419A-8AE3-22B0C16E1F06)**  - Displays a modal window with a HTML document; use in conjunction with the new JavaScript API.
- **Changed:** **[findfile](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-D671F67D-F92B-41FF-B9FA-A48EF52CF607)**  - Searches the AutoCAD support and trusted file paths. Function was updated to search the new trusted applications paths.

## AutoCAD 2013

- **New:** **[vlax-machine-product-key](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-2EB68815-FDF9-4235-918B-025FB9A42220)**  - Returns the AutoCAD Windows registry path in the HKLM (HKEY_LOCAL_MACHINE).
- **Obsolete:** **[vlax-product-key](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-237CEDFA-A02E-443E-BB21-5BD86E5E353E)**  - Returns the AutoCAD Windows registry path.

## AutoCAD 2012

- **New:** **[command-s](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-5C9DC003-3DD2-4770-95E7-7E19A4EE19A1)**  - Executes an AutoCAD command and the supplied input.  **[*pop-error-mode*](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-60AFEFAD-1DF5-448F-A2E9-D7260DB279E4)**  - Error-handling function that ends the previous call to `*push-error-using-command*`  or `*push-error-using-stack*`.  **[*push-error-using-command*](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-620E034A-9151-427F-B6F5-B360D14DA925)**  - Error-handling function that indicates the use of the command function within a custom `*error*`  handler.  **[*push-error-using-stack*](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-C28420C9-2210-4EEC-AA73-2962999D1BC1)**  - Error-handling function that indicates the use of variables from the AutoLISP stack within a custom `*error*`  handler.

## AutoCAD 2011

- **New:** **[dumpallproperties](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-A65FBCCF-CC5D-47E8-9117-D577D9CB9D8A)**  - Retrieves an entity’s supported properties.  **[getpropertyvalue](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-8E5913FC-09ED-4C70-AFB7-2431C062E899)**  - Returns the current value of an entity’s property.  **[ispropertyreadonly](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-1DE8095D-5755-4889-BFB7-C13045B7BC81)**  - Returns the read-only state of an entity’s property.  **[setpropertyvalue](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-8F32FD8C-D81A-4DCA-B455-9D560CF17246)**  - Sets the property value for an entity.

## AutoCAD 2010

- **Changes:** **[help](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-4CB4A951-1507-4F3D-9F7B-93FF3A2C9850)**  - Invokes the Help facility. Function was updated to add support for HTML documentation.

## AutoCAD 2009

- **New:** **[initcommandversion](https://beehive.autodesk.com/community/service/rest/cloudhelp/resource/cloudhelpchannel/guidcrossbook/?v=2025&p=OARX&l=ENU&accessmode=live&guid=GUID-6176FC98-DC5D-433E-8D76-F481BE68D46A)**  - Forces the next command to run with the specified version.
