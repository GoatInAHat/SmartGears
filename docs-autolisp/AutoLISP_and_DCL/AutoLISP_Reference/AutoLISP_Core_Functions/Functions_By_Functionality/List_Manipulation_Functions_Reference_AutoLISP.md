---
title: List Manipulation Functions Reference (AutoLISP)
guid: "GUID-00A75AFB-5708-4D0C-B18C-F74DDC4D65FA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-00A75AFB-5708-4D0C-B18C-F74DDC4D65FA.htm"
generated: "2025-11-28T19:06:18.250795Z"
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

# List Manipulation Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-00A75AFB-5708-4D0C-B18C-F74DDC4D65FA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-00A75AFB-5708-4D0C-B18C-F74DDC4D65FA.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP list manipulation functions.

| List manipulation functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(acad_strlsort *lst*)](../A_Functions/acad_strlsort_AutoLISP.md) | Sorts a list of strings by alphabetical order | ✓ | ✓ | ✓ | -- | ✓ |
| [(append *lst ...*)](../A_Functions/append_AutoLISP.md) | Takes any number of lists and runs them together as one list | ✓ | ✓ | ✓ | -- | ✓ |
| [(assoc *item alist*)](../A_Functions/assoc_AutoLISP.md) | Searches an association list for an element and returns that association list entry | ✓ | ✓ | ✓ | -- | ✓ |
| [(caddr *lst*)](../C_Functions/caddr_AutoLISP.md) | Returns the third element of a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(cadr *lst*)](../C_Functions/cadr_AutoLISP.md) | Returns the second element of a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(car *lst*)](../C_Functions/car_AutoLISP.md) | Returns the first element of a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(cdr *lst*)](../C_Functions/cdr_AutoLISP.md) | Returns the specified list, except for the first element of the list | ✓ | ✓ | ✓ | -- | ✓ |
| [(cons *new-first-element lst*)](../C_Functions/cons_AutoLISP.md) | The basic list constructor | ✓ | ✓ | ✓ | -- | ✓ |
| [(foreach *name lst [expr ...]*)](../F_Functions/foreach_AutoLISP.md) | Evaluates expressions for all members of a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(last *lst*)](../L_Functions/last_AutoLISP.md) | Returns the last element in a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(length *lst*)](../L_Functions/length_AutoLISP.md) | Returns an integer indicating the number of elements in a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(list *[expr ...]*)](../L_Functions/list_AutoLISP.md) | Takes any number of expressions and combines them into one list | ✓ | ✓ | ✓ | -- | ✓ |
| [(listp *item*)](../L_Functions/listp_AutoLISP.md) | Verifies that an item is a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(mapcar *function list1 ... listn*)](../M_Functions/mapcar_AutoLISP.md) | Returns a list of the result of executing a function with the individual elements of a list or lists supplied as arguments to the function | ✓ | ✓ | ✓ | -- | ✓ |
| [(member *expr lst*)](../M_Functions/member_AutoLISP.md) | Searches a list for an occurrence of an expression and returns the remainder of the list, starting with the first occurrence of the expression | ✓ | ✓ | ✓ | -- | ✓ |
| [(nth *n lst*)](../N_Functions/nth_AutoLISP.md) | Returns the nth element of a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(reverse *lst*)](../R_Functions/reverse_AutoLISP.md) | Returns a list with its elements reversed | ✓ | ✓ | ✓ | -- | ✓ |
| [(subst *newitem olditem lst*)](../S_Functions/subst_AutoLISP.md) | Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-consp *list-variable*)](../V_Functions/vl_consp_AutoLISP.md) | Determines whether or not a list is nil | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-every *predicate-function list [ more-lists ...]*)](../V_Functions/vl_every_AutoLISP.md) | Checks whether the predicate is true for every element combination | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-list* *object [more-objects ...]*)](../V_Functions/vl_list_AutoLISP.md) | Constructs and returns a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-list->string *char-codes-list*)](../V_Functions/vl_list_string_AutoLISP.md) | Combines the Unicode characters associated with a list of integers into a string | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-list-length *list-or-cons-object*)](../V_Functions/vl_list_length_AutoLISP.md) | Calculates list length of a true list | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-member-if *predicate-function list*)](../V_Functions/vl_member_if_AutoLISP.md) | Determines whether the predicate is true for one of the list members | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-member-if-not *predicate-function list*)](../V_Functions/vl_member_if_not_AutoLISP.md) | Determines whether the predicate is nil for one of the list members | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-position *symbol list*)](../V_Functions/vl_position_AutoLISP.md) | Returns the index of the specified list item | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-remove *element-to-remove list*)](../V_Functions/vl_remove_AutoLISP.md) | Removes elements from a list | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-remove-if *predicate-function list*)](../V_Functions/vl_remove_if_AutoLISP.md) | Returns all elements of the supplied list that fail the test function | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-remove-if-not *predicate-function list*)](../V_Functions/vl_remove_if_not_AutoLISP.md) | Returns all elements of the supplied list that pass the test function | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-some *predicate-function list [more-lists ...]*)](../V_Functions/vl_some_AutoLISP.md) | Checks whether the predicate is not nil for one element combination | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-sort *list comparison-function*)](../V_Functions/vl_sort_AutoLISP.md) | Sorts the elements in a list according to a given compare function | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-sort-i *list comparison-function*)](../V_Functions/vl_sort_i_AutoLISP.md) | Sorts the elements in a list according to a given compare function, and returns the element index numbers | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-string->list *string*)](../V_Functions/vl_string_list_AutoLISP.md) | Converts a string into a list of Unicode character codes | ✓ | ✓ | ✓ | -- | ✓ |
