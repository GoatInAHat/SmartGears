---
title: Example Using Local Variables (AutoLISP)
guid: "GUID-5ED11567-AFEE-421F-963E-6D7B55747A28"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-5ED11567-AFEE-421F-963E-6D7B55747A28.htm"
generated: "2025-11-28T19:06:05.322081Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Example Using Local Variables (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-5ED11567-AFEE-421F-963E-6D7B55747A28.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-5ED11567-AFEE-421F-963E-6D7B55747A28.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following example shows the use of local variables in a user-defined function (be certain there is at least one space between the slash and the local variables).

```lisp
(defun LOCAL ( / aaa bbb)
(setq aaa "A" bbb "B")
(princ (strcat "\naaa has the value " aaa ))
(princ (strcat "\nbbb has the value " bbb))
(princ) )

LOCAL
```

Before you test the new function, assign variables `aaa`  and `bbb`  to values other than those used in the `**LOCAL**`  function.

```lisp
(setq aaa 1 bbb 2)

2
```

You can verify that the variables `aaa`  and `bbb`  are actually set to those values.

```lisp
!aaa

1

!bbb

2
```

Now test the `LOCAL`  function.

```lisp
(local)

aaa has the value A
bbb has the value B
```

You will notice the function used the values for `aaa`  and `bbb`  that are local to the function. You can verify that the current values for `aaa`  and `bbb`  are still set to their nonlocal values.

```lisp
!aaa

1

!bbb

2
```

In addition to ensuring that variables are local to a particular function, this technique also ensures the memory used for those variables is available for other functions.
