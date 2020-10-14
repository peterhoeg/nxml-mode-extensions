;;; nxml-mode-extensions.el --- nxml-mode extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Peter Hoeg

;; Author: Peter Hoeg <peter@hoeg.com>
;; Keywords: languages,
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defvar nxml-mode-extensions-maximum-file-size 1000000
  "Maximum file size to enable eldoc.")

;;;###autoload
(defvar nxml-mode-extensions-separator "/"
  "The character used to separate elements in the path.")

(defun nxml-mode-extensions-safe-to-enable-p (&rest args)
  "Safe to enable dynamic path update for current file?. Ignore ARGS."
  (and (buffer-name)
       (< (file-attribute-size (file-attributes (buffer-name) 'integer))) nxml-mode-extensions-maximum-file-size))

;; From: https://www.emacswiki.org/emacs/NxmlMode#toc11
;;;###autoload
(defun nxml-mode-extensions-get-path ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (let ((str (format "%s%s" nxml-mode-extensions-separator (mapconcat 'identity path nxml-mode-extensions-separator))))
          (when (called-interactively-p t)
            (message str))
          str)))))

;;;###autoload
(defun nxml-mode-extensions-enable-path-in-eldoc-maybe (&optional force enable-mode)
  "Enable current xpath with eldoc and optionally `ENABLE-MODE'."
  (when (or (nxml-mode-extensions-safe-to-enable-p)
            force)
    (set (make-local-variable 'eldoc-documentation-function)
         'nxml-mode-extensions-get-path)
    (when enable-mode
      (eldoc-mode 1))))

;;;###autoload
(defun nxml-mode-extensions-enable-path-in-modeline-maybe (&optional force enable-mode)
  "Enable current xpath in modeline and optionally `ENABLE-MODE'."
  (when (or (nxml-mode-extensions-safe-to-enable-p)
            force)
    (add-hook 'which-func-functions 'nxml-mode-extensions-get-path t t)
    (when enable-mode
      (which-function-mode 1))))

;;;###autoload
(defun nxml-mode-extensions-enable-current-path-maybe (&optional force enable-modes)
  "Conditionally enable current xpath with eldoc and modeline."
  (nxml-mode-extensions-enable-path-in-eldoc-maybe force enable-modes)
  (nxml-mode-extensions-enable-path-in-modeline-maybe force enable-modes))

;;;###autoload
(defun nxml-mode-extensions-enable-hideshow ()
  "Enable HideShow support for NXML."
  (when (require 'hideshow)
    (add-to-list
     'hs-special-modes-alist
     '(nxml-mode
       "<!--\\|<[^/>]*[^/]>"
       "-->\\|</[^/>]*[^/]>"

       "<!--"
       sgml-skip-tag-forward
       nil))))

;; http://stackoverflow.com/a/5198243/48082
;;;###autoload
(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region."
  (interactive "r")
  (save-excursion
    ;; (nxml-mode)
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (cl-incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (cl-incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (cl-incf end))
    (indent-region begin end nil)
    ;; since we are changing things anyway, we might as well clean up whitespace
    (delete-trailing-whitespace begin end)
    (normal-mode)))

;;;###autoload
(defun xml-pretty-print-region-or-buffer ()
  "Pretty format XML in region or buffer."
  (interactive)
  (if (region-active-p)
      (xml-pretty-print-region (region-beginning) (region-end))
    (xml-pretty-print-region (point-min) (point-max))))

(provide 'nxml-mode-extensions)
;;; nxml-mode-extensions.el ends here
