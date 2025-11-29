---
title: "Lesson 6: Acting With Reactors (Visual LISP IDE)"
guid: "GUID-EA2B760C-866D-46BB-B682-EEC46C8FF11B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EA2B760C-866D-46BB-B682-EEC46C8FF11B.htm"
generated: "2025-11-28T19:07:03.529671Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# Lesson 6: Acting With Reactors (Visual LISP IDE)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EA2B760C-866D-46BB-B682-EEC46C8FF11B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EA2B760C-866D-46BB-B682-EEC46C8FF11B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 29/03/2023

Attention:
 This lesson requires the Visual LISP Editor and applies to AutoCAD for Windows only.

In this lesson, you will learn about reactors and how to attach them to drawing events and entities. Reactors allow your application to be notified by AutoCAD ®  when particular events occur. For example, if a user moves an entity that your application has attached a reactor to, your application will receive notification that the entity has moved. You can program this to trigger additional operations, such as moving other entities associated with the one the user moved, or perhaps updating a text tag that records revision information on the altered drawing feature. In effect, it is like setting up your application with a pager and telling AutoCAD to beep the application when something happens.

## Topics in this Tutorial
