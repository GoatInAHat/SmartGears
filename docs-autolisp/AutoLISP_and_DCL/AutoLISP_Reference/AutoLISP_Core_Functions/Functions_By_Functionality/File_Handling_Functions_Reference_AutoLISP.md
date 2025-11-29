---
title: "File-Handling Functions Reference (AutoLISP)"
guid: "GUID-F70DECFC-DBE1-4F04-A64C-B3F869A636A2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F70DECFC-DBE1-4F04-A64C-B3F869A636A2.htm"
generated: "2025-11-28T19:06:17.036221Z"
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

# File-Handling Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F70DECFC-DBE1-4F04-A64C-B3F869A636A2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F70DECFC-DBE1-4F04-A64C-B3F869A636A2.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP file-handling functions.

| File-handling functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(close *file-desc*)](../C_Functions/close_AutoLISP.md) | Closes an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(findfile *filename*)](../F_Functions/findfile_AutoLISP.md) | Searches the AutoCAD library path for the specified file | ✓ | ✓ | ✓ | -- | ✓ |
| [(findtrustedfile *filename*)](../F_Functions/findtrustedfile_AutoLISP.md) | Searches the AutoCAD trusted file paths for the specified file | ✓ | ✓ | ✓ | -- | ✓ |
| [(open *filename mode*)](../O_Functions/open_AutoLISP.md) | Opens a file for access by the AutoLISP I/O functions | ✓ | ✓ | ✓ | -- | ✓ |
| [(read-char *[file-desc]*)](../R_Functions/read_char_AutoLISP.md) | Returns the decimal ASCII code representing the character read from the keyboard input buffer or from an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(read-line *[file-desc]*)](../R_Functions/read_line_AutoLISP.md) | Reads a string from the keyboard or from an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-directory-files [ *directory pattern directories]*)](../V_Functions/vl_directory_files_AutoLISP.md) | Lists all files in a given directory | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-file-copy *"source-filename" "destination-filename" [append]*)](../V_Functions/vl_file_copy_AutoLISP.md) | Copies or appends the contents of one file to another file | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-file-delete *"filename"*)](../V_Functions/vl_file_delete_AutoLISP.md) | Deletes a file | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-file-directory-p *"filename"*)](../V_Functions/vl_file_directory_p_AutoLISP.md) | Determines if a file name refers to a directory | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-file-rename *"old-filename" "new-filename"*)](../V_Functions/vl_file_rename_AutoLISP.md) | Renames a file | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-file-size *"filename"*)](../V_Functions/vl_file_size_AutoLISP.md) | Determines the size of a file, in bytes | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-file-systime *"filename"*)](../V_Functions/vl_file_systime_AutoLISP.md) | Returns last modification time of the specified file | ✓ | ✓ | ✓ | -- | -- |
| [(vl-filename-base *"filename"*)](../V_Functions/vl_filename_base_AutoLISP.md) | Returns the name of a file, after stripping out the directory path and extension | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-filename-directory *"filename"*)](../V_Functions/vl_filename_directory_AutoLISP.md) | Returns the directory path of a file, after stripping out the name and extension | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-filename-extension *"filename"*)](../V_Functions/vl_filename_extension_AutoLISP.md) | Returns the extension from a file name, after stripping out the rest of the name | ✓ | ✓ | ✓ | -- | ✓ |
| [(vl-filename-mktemp *["pattern" "directory" "extension"]*)](../V_Functions/vl_filename_mktemp_AutoLISP.md) | Calculates a unique file name to be used for a temporary file | ✓ | ✓ | ✓ | -- | ✓ |
| [(write-char *num [file-desc]*)](../W_Functions/write_char_AutoLISP.md) | Writes one character to the screen or to an open file | ✓ | ✓ | ✓ | -- | ✓ |
| [(write-line *string [file-desc]*)](../W_Functions/write_line_AutoLISP.md) | Writes a string to the screen or to an open file | ✓ | ✓ | ✓ | -- | ✓ |
