## Problem
You have a directory containing an unknown set of subdirectories and files. For example:

```
a/a.png
a/a.html
b/b.html
b/c/c.html
c/d.png
d/
```

Return a list with the relative pathname (e.g. `"b/c/c.html"`) of each `.html` file in this tree.

* Do not include file names starting with a dot ("dotfiles").
* Do not visit subdirectories whose names start with a dot.
* The list does not have to be sorted.

No portable solution exists at the moment. SRFI 170 or implementation-specific libraries can be used.