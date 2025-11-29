---
title: Reactor Types
guid: "GUID-76E0F572-8EE1-4FD5-A799-8CCD897D9C0C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-76E0F572-8EE1-4FD5-A799-8CCD897D9C0C.htm"
generated: "2025-11-28T19:07:03.678851Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Reactor Types

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-76E0F572-8EE1-4FD5-A799-8CCD897D9C0C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-76E0F572-8EE1-4FD5-A799-8CCD897D9C0C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

There are many types of AutoCAD ®  reactors. Each reactor type responds to one or more AutoCAD events. Reactors are grouped into the following categories:

- **Editor Reactors:** Notify your application each time an AutoCAD command is invoked.
- **Linker Reactors:** Notify your application every time an ObjectARX ®  application is loaded or unloaded.
- **Database Reactors:** Correspond to specific entities or objects within a drawing database.
- **Document Reactors:** Notify your application in MDI mode of a change to the current drawing document, such as opening of a new drawing document, activating a different document window, and changing a document's lock status.
- **Object Reactors:** Notify you each time a specific object is changed, copied, or deleted.

With the exception of editor reactors, there is one type of reactor for each reactor category. Editor reactors encompass a broad class of reactors: for example, DXF ™  reactors that notify an application when a DXF file is imported or exported, and Mouse reactors that notify of mouse events such as double-clicks.

Within the reactor categories, there are many specific events to which you can attach a reactor. AutoCAD allows users to perform many different kinds of actions, and it is up to you to determine the actions that you are interested in. Once you have done this, you can attach your reactor “auto-dialer” to the event, then write the callback function that is triggered when the event occurs.
