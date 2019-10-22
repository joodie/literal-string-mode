# literal-string - emacs functions for editing long string literals

## Usage:

  - put `literal-string.el` somewhere on your `load-path` and `(require
    'literal-string)`

  - Enable `literal-string-mode` in your lisp modes and edit your
    markdown-formatted docstrings by hitting `C-c "` or `M-x
    literal-string-edit-string`

## Todo:

  - [X] Make the editing buffer's major mode customizable
  - [X] Make de/re-indentation customizable
  - [X] Finish packaging: autoloads, requires
  - [ ] Split doc-string editing and other literal string editing
  - [ ] Support non-lisp-style string delimiters

## Changes:

    - 0.5 customizable indentation
    - 0.4 fix major mode switch
    - 0.3 customize major mode for edit buffer
    - 0.2 use edit-indirect mode
    - 0.1 initial release
