;; -*- lexical-binding: t -*-

(defun literal-string-inside-string? ()
  "Returns non-nil if inside string, else nil.
  Result depends syntax table's string quote character."
  (nth 3 (syntax-ppss)))

(defun literal-string-current-string ()
  "list of start and end of content of current literal string."
  (when (literal-string-inside-string?)
    (save-excursion
      (search-forward-regexp "[^\\\\]\"")
      (backward-char 1)
      (let ((end (point-marker)))
        (search-backward-regexp "[^\\\\]\"")
        (forward-char 2)
        (let ((start (point-marker)))
          (list start end))))))

(defun literal-string-docstring-indent-level ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((indent-count nil))
      (while (not (eobp))
        (when (not (looking-at "[[:space:]]*$"))
          (setq indent-count (if indent-count
                                 (min indent-count (current-indentation))
                                 (current-indentation))))
        (forward-line 1))
      indent-count)))

(defun literal-string-docstring-deindent ()
  "Remove indentation of lines after the first one. Returns the
amount of indentation removed."
  (when-let (level (literal-string-docstring-indent-level))
    (when (not (zerop level))
      (indent-rigidly (point-min) (point-max) (- level))
      level)))

(defun literal-string-docstring-reindent ()
  (when-let (level literal-string-source-indent-level)
    (when (not (zerop level))
      (save-excursion
        (goto-char (point-min))
        (forward-line)
        (when (not (eobp))
          (indent-rigidly (point) (point-max) level))))))

(defun literal-string-replace-all (from to)
  (save-excursion
    (perform-replace from to nil nil nil nil nil (point-min) (point-max))))

(defun literal-string-unescape ()
  (literal-string-replace-all "\\\"" "\"")
  (literal-string-replace-all "\\\\" "\\"))

(defun literal-string-escape ()
  (literal-string-replace-all "\\" "\\\\")
  (literal-string-replace-all "\"" "\\\""))

(defvar-local literal-string-source-indent-level nil)
(defvar-local literal-string-source-region nil)

(defun literal-string-edit-string ()
  "Indent current string literal.
  Removes docstring indentation"
  (interactive)
  (when-let (region (literal-string-current-string))
    (let ((edit-buffer (get-buffer-create (format "*Edit Literal String <%s>*" (buffer-name)))))
      (apply #'copy-to-buffer edit-buffer region)
      (switch-to-buffer edit-buffer)
      (markdown-mode) ; first - changing major mode clears local vars!
      (setq literal-string-source-region region
            literal-string-source-indent-level (literal-string-docstring-deindent))
      (literal-string-unescape))))

(defun literal-string-edit-string-exit ()
  (interactive)
  (when-let (region literal-string-source-region) ; copy buffer-local var
    (let ((string-buffer (current-buffer))
          (source-buffer (marker-buffer (car region))))
      (literal-string-escape)
      (literal-string-docstring-reindent)
      (switch-to-buffer source-buffer)
      (apply #'delete-region region)
      (insert-buffer string-buffer)
      (set-marker (car region) nil nil)
      (set-marker (cadr region) nil nil)
      (kill-buffer string-buffer))))

(defun literal-string-edit-docstring ()
  (interactive)
  (when (literal-string-narrow-to-string)
    (markdown-mode)))

(defun literal-string-end-edit-docstring ()
  (interactive)
  (widen)
  (clojure-mode))

(provide 'literal-string)

