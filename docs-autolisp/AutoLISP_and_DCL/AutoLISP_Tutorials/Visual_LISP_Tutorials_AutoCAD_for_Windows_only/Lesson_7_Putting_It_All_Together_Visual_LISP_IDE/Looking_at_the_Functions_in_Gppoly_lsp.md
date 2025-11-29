---
title: Looking at the Functions in Gppoly.lsp
guid: "GUID-D4898DC6-DB76-42D9-842F-657DA937FA33"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4898DC6-DB76-42D9-842F-657DA937FA33.htm"
generated: "2025-11-28T19:07:06.490661Z"
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

# Looking at the Functions in Gppoly.lsp

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4898DC6-DB76-42D9-842F-657DA937FA33.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4898DC6-DB76-42D9-842F-657DA937FA33.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The file *gppoly.lsp*  contains a number of functions required for straightening a polyline when a single grip has been stretched. Only some of these functions will be explained in depth in this tutorial.

Note:
 This section of the Garden Path tutorial contains some of the most complex code and concepts in the entire lesson. If you are a beginner, you may want to jump ahead to the Building an Application section.

The functions within the *gppoly.lsp*  file are organized in a way that you may have noticed in other AutoLISP source code files. The highest-level function, often the main, or `C:`  function (in this case, `gp:Redefine-PolyBorder`), is located at the bottom of the file. The functions called within the main function are defined above it within the source file. This convention goes back to the old days of programming, when some development environments required that files be organized this way. With Visual LISP, this is a matter of personal style; there is no requirement that you organize your functions in any specific sequence.

Before diving into the details, step back and look at what needs to be done to recalculate and draw the garden path boundary. The following illustration shows an example of a garden path, along with the association list key points stored in the reactor data:

In this example, the 12 key point is the lower-left corner, 13 is lower-right, and so on. If the user moves the upper-right point (the 14 key point), the program will need to recalculate two existing points—the lower-right (13) and upper-left (15).
