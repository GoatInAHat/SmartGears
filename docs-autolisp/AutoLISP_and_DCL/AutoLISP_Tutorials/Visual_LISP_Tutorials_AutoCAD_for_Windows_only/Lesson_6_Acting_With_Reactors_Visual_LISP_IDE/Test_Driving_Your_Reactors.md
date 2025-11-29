---
title: Test Driving Your Reactors
guid: "GUID-D4641EEE-2C89-4A48-8BFD-C1C60A72576E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4641EEE-2C89-4A48-8BFD-C1C60A72576E.htm"
generated: "2025-11-28T19:07:04.614045Z"
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

# Test Driving Your Reactors

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4641EEE-2C89-4A48-8BFD-C1C60A72576E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4641EEE-2C89-4A48-8BFD-C1C60A72576E.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

By now, you should have all the necessary pieces in place to use some live reactors.

## To test the reactor code

1. Load all the source code from your project. (Click the Load Source Files button in the gpath project window.)
2. Start the
   C:GPath
    function.

   The program will draw a garden path for you, just as you were able to in Lesson 5. You will not see anything interesting at first.
3. Try the following actions after you draw the path:
   - Move a polyline vertex. Pick the polyline and turn on its grips, then drag a vertex to a new location.
   - Stretch the polyline.
   - Move the polyline.
   - Erase the polyline.

Examine the messages that appear. You are watching the behind-the-scenes activities of a powerful capability.

(If your application is not working correctly and you do not want to take the time to debug it right now, you can run the sample code provided in the *Tutorial\VisualLISP\Lesson6*  directory. Use the *Gpath6*  project in that directory.)

Note:
 Because of the reactor behavior, you may notice that after testing a reactor sequence in AutoCAD, you cannot return to Visual LISP by pressing Alt+Tab, or by clicking to activate the Visual LISP window. If this happens, simply enter
vlisp
 at the AutoCAD Command prompt to return to Visual LISP.
