---
title: "String-Handling Functions Reference (AutoLISP)"
guid: "GUID-8543549B-F0D7-43CD-87A3-F6B827FF0B88"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8543549B-F0D7-43CD-87A3-F6B827FF0B88.htm"
generated: "2025-11-28T19:06:18.864504Z"
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

# String-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8543549B-F0D7-43CD-87A3-F6B827FF0B88.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8543549B-F0D7-43CD-87A3-F6B827FF0B88.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP string-handling functions.

| String-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(read *[string]*)](../R_Functions/read_AutoLISP.md) | Returns the first list or atom obtained from a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(strcase *string [which]*)](../S_Functions/strcase_AutoLISP.md) | Returns a string where all alphabetic characters have been converted to uppercase or lowercase | ✓ | ✓ | ✓ | -- | ✓ |
| [(strcat *[string1 [string2 ...]*)](../S_Functions/strcat_AutoLISP.md) | Returns a string that is the concatenation of multiple strings | ✓ | ✓ | ✓ | -- | ✓ |
| [(strlen *[string ...]*)](../S_Functions/strlen_AutoLISP.md) | Returns an integer that is the number of characters in a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(substr *string start [length]*)](../S_Functions/substr_AutoLISP.md) | Returns a substring of a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-prin1-to-string *object*)](../V_Functions/vl_prin1_to_string_AutoLISP.md) | Returns the string representation of any LISP object as if it were output by the `prin1`  function | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-princ-to-string *object*)](../V_Functions/vl_princ_to_string_AutoLISP.md) | Returns the string representation of any LISP object as if it were output by the `princ`  function | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string->list *string*)](../V_Functions/vl_string_list_AutoLISP.md) | Converts a string into a list of Unicode character codes | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-elt *string position*)](../V_Functions/vl_string_elt_AutoLISP.md) | Returns the Unicode representation of the character at a specified position in a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-left-trim *character-set string*)](../V_Functions/vl_string_left_trim_AutoLISP.md) | Removes the specified characters from the beginning of a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-mismatch *str1 str2 [pos1 pos2 ignore-case-p]*)](../V_Functions/vl_string_mismatch_AutoLISP.md) | Returns the length of the longest common prefix for two strings, starting at specified positions | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-position *char-code str [ start-pos [from-end-p]]*)](../V_Functions/vl_string_position_AutoLISP.md) | Looks for a character with the specified Unicode code in a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-right-trim *character-set string*)](../V_Functions/vl_string_right_trim_AutoLISP.md) | Removes the specified characters from the end of a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-search *pattern* *string*  [ *start-pos*])](../V_Functions/vl_string_search_AutoLISP.md) | Searches for the specified pattern in a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-subst *new-str pattern string [start-pos]*)](../V_Functions/vl_string_subst_AutoLISP.md) | Substitutes one string for another, within a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-translate *source-set dest-set str*)](../V_Functions/vl_string_translate_AutoLISP.md) | Replaces characters in a string with a specified set of characters | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string-trim *char-set str*)](../V_Functions/vl_string_trim_AutoLISP.md) | Removes the specified characters from the beginning and end of a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(wcmatch *string pattern*)](../W_Functions/wcmatch_AutoLISP.md) | Performs a wild-card pattern match on a string | ✓ | ✓ | ✓ | -- | ✓ |
