---
title: Defining the Dialog Box With DCL
guid: "GUID-BAC2CA7F-EBFE-4F35-98B0-9F0722F53CC4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BAC2CA7F-EBFE-4F35-98B0-9F0722F53CC4.htm"
generated: "2025-11-28T19:07:00.206353Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 16/02/2020
topic_subtype:
  - autolisp
---

# Defining the Dialog Box With DCL

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BAC2CA7F-EBFE-4F35-98B0-9F0722F53CC4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BAC2CA7F-EBFE-4F35-98B0-9F0722F53CC4.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 16/02/2020

Begin by taking a look at the dialog box you need to create.

The dialog box contains the following elements:

-  Two sets of radio buttons.

  One set of buttons determines the polyline style of the boundary, and the other set of buttons specifies the tile entity creation method (ActiveX, `entmake`, or `command`). Only one radio button in a set can be selected at one time.
-  Edit boxes for specifying the radius of tiles and the spacing between tiles.
-  A standard set of OK and Cancel buttons.

Dialog box components are referred to as *tiles*  in DCL. Writing the complete contents of a dialog box DCL file may seem overwhelming. The trick is to sketch out what you want, break it down into sections, then write each section.

## To define the dialog box

1.  Open a new file in the Visual LISP text editor window.
2. Enter the following statement in the new file:

   ```lisp
   label = "Garden Path Tile Specifications";
   ```

   This DCL statement defines the title of the dialog box window.
3.  Define the radio buttons for specifying polyline type by adding the following code:

   ```lisp
   : boxed_radio_column {    // defines the radio button areas
     label = "Outline Polyline Type";
     : radio_button {        // defines the lightweight radio button
       label = "&Lightweight";
       key = "gp_lw";
       value = "1";
      }
   : radio_button {       // defines the old-style polyline radio button
     label = "&Old-style";
     key = "gp_hw";
    }
   }
   ```

   The `boxed_radio_column`  DCL directive defines a box boundary and allows you to specify a label for the set of buttons. Within the boundary, you specify the radio buttons you need by adding `radio_button`  directives. Each radio button requires a label and a *key*. The key is the name by which your AutoLISP code can refer to the button.

   Notice that the radio button labeled “lightweight” is given a value of 1. A value of 1 (a string, not an integer) assigned to a button makes it the default choice in a row of buttons. In other words, when you first display the dialog, this button will be selected. Also notice that in DCL files, double-slash characters, not semicolons as in AutoLISP, indicate a comment.
4.  Define the radio column for the selection of the entity creation style by adding the following code:

   ```lisp
   : boxed_radio_column {     // defines the radio button areas
     label = "Tile Creation Method";
     : radio_button {         // defines the ActiveX radio button
       label = "&ActiveX Automation";
       key = "gp_actx";
       value = "1";
      }
   : radio_button {          // defines the (entmake) radio button
     label = "&Entmake";
     key = "gp_emake";
    }
   : radio_button {          // defines the (command) radio button
     label = "&Command";
     key = "gp_cmd";
    }
   }
   ```
5.  Add the following code to define the edit box tiles that allow users to enter the numbers specifying tile size and spacing:

   ```lisp
   : edit_box {      // defines the Radius of Tile edit box
     label = "&Radius of tile";
     key = "gp_trad";
     edit_width = 6;
   }
   : edit_box {      // defines the Spacing Between Tiles edit box
     label = "S&pacing between tiles";
     key = "gp_spac";
     edit_width = 6;
   }
   ```

   Notice that this definition does not set any initial values for the edit boxes. You will set default values for each edit box in your AutoLISP program.
6.  Add the following code for the OK and Cancel buttons:

   ```lisp
   : row {          // defines the OK/Cancel button row
     : spacer { width = 1; }
     : button {    // defines the OK button
       label = "OK";
       is_default = true;
       key = "accept";
       width = 8;
       fixed_width = true;
     }
     : button {    // defines the Cancel button
       label = "Cancel";
       is_cancel = true;
       key = "cancel";
       width = 8;
       fixed_width = true;
     }
     : spacer { width = 1;}
   }
   ```

   Both buttons are defined within a row, so they line up horizontally.
7.  Scroll to the beginning of the text editor window and insert the following statement as the first line in your DCL:

   ```lisp
   gp_mainDialog : dialog {
   ```
8.  The
   dialog
    directive requires a closing brace, so scroll to the end of the file and add the brace as the last line of DCL code:

   ```lisp
   }
   ```
