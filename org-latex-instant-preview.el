;;; org-latex-instant-preview.el --- Preview org-latex Fragments Instantly via MathJax -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Sheng Yang
;;
;; Author:  Sheng Yang <styang@fastmail.com>
;; Created: June 03, 2020
;; Modified: June 14, 2020
;; Version: 0.0.2
;; Keywords: tex,tools
;; Homepage: https://github.com/yangsheng6810/org-latex-instant-preview
;; Package-Requires: ((emacs "26") (names "0.5.2") (s "1.8.0") (posframe "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package provides instant preview of LaTeX snippets via MathJax outputed
;;  SVG.
;;
;;; Code:
;;;

(eval-when-compile (require 'names))
(require 'image-mode)
(require 's)
(require 'org)
(require 'posframe)
(require 'org-element)

;;;###autoload
(define-namespace org-latex-instant-preview-
;; (defgroup org-latex-instant-preview nil
;;   "Instant preview for org LaTeX snippets.")

(defcustom tex2svg-bin ""
  "Location of tex2svg executable."
  :group 'org-latex-instant-preview
  :type '(string))

(defcustom delay 0.3
  "Number of seconds to wait before a re-compilation."
  :group 'org-latex-instant-preview
  :type '(number))

(defconst -output-buffer "*org-latex-instant-preview-output-buffer*"
  "Buffer to hold the output.")

(defconst -posframe-buffer "*org-latex-instant-preview-posframe-buffer*"
  "Buffer to hold the preview.")

(defvar -need-update nil)
(defvar -timer nil)
(defvar -last-tex-string "")
(defvar -last-end-position nil)
(defvar -process nil)
(defvar -preview-visible nil)

:autoload
(defun stop ()
  "Stop instant preview of LaTeX snippets."
  (interactive)
  (posframe-hide -output-buffer)
  (remove-hook 'after-change-functions #'-prepare-render t)
  (remove-hook 'post-command-hook #'-prepare-render t)
  (when -timer
    (cancel-timer -timer))
  (setq -need-update nil))

(defun -prepare-render (&rest _)
  "Prepare timer to call re-compilation."
  (unless -need-update
    (setq -need-update t)
    (setq -timer
          (run-with-idle-timer delay nil #'start))))

(defun -remove-math-delimeter (ss)
  "Chop LaTeX delimeters from SS."
  (s-with ss
    (s-chop-prefixes '("$$" "\\(" "$" "\\["))
    (s-chop-suffixes '("$$" "\\)" "$" "\\]"))))

(defun -wrap-color (ss)
  "Wrap SS with color."
  (let ((color (face-foreground 'default)))
    (format "\\color{%s}{%s}" color ss)))

:autoload
(defun start (&rest _)
  "Start instant preview."
  (interactive)
  (unless tex2svg-bin
    (message "You need to set org-latex-instant-preview-tex2svg-bin
for instant preview to work!")
    (error "Org-latex-instant-preview-tex2svg-bin is not set"))

  (when (equal this-command #'start)
    (add-hook 'after-change-functions #'-prepare-render nil t))
  (get-buffer-create -output-buffer)
  (get-buffer-create -posframe-buffer)
  (let ((datum (org-element-context)))
    (if (or (memq (org-element-type datum) '(latex-environment latex-fragment))
            (and (memq (org-element-type datum) '(export-block))
                 (equal (org-element-property :type datum) "LATEX")))
	      (let ((ss (org-element-property :value datum))
              (end (org-element-property :end datum)))
          (when (memq (org-element-type datum) '(latex-fragment))
            (setq ss (-wrap-color
                      (-remove-math-delimeter ss))))
          (if (and -last-tex-string (equal ss -last-tex-string))
              (when (and -last-end-position (equal end -last-end-position))
                (-show end))
            (-render ss end)))
      (posframe-hide -posframe-buffer)))
  (setq -need-update nil))

(defun -render-old (tex-string end)
  "Render TEX-STRING to buffer, old version.

Showing at point END."
  (with-current-buffer -output-buffer
    (message "Instant LaTeX rendering")
    (let ((ss (shell-command-to-string
               (concat tex2svg-bin " "
                       (shell-quote-argument tex-string))))
          (inhibit-message t))
      (image-mode-as-text)
      (erase-buffer)
      (insert ss)
      (image-mode)
      (-show end))))

(defun -render (tex-string end)
  "Render TEX-STRING to buffer, async version.

Showing at point END"
  (message "Instant LaTeX rendering")
  (with-current-buffer -output-buffer
    (erase-buffer))
  (setq -last-tex-string tex-string)
  (setq -last-end-position end)
  (unless -process
    (setq -process
          (make-process
           :name "org-latex-instant-preview"
           :buffer -output-buffer
           :command (list tex2svg-bin
                          tex-string)
           ;; :stderr ::my-err-buffer
           :sentinel
           (lambda (&rest _)
             (let ((inhibit-message t))
               (with-current-buffer -posframe-buffer
                 (image-mode-as-text)
                 (erase-buffer)
                 (insert-buffer-substring -output-buffer)
                 (image-mode))
               (-show end)
               (setq -process nil)))))))

(defun -show (display-point)
  "Show preview posframe at DISPLAY-POINT."
  (when (posframe-workable-p)
    (posframe-show -posframe-buffer
                   :position display-point)))

(defun -clear-refresh-maybe (window &rest _)
  "Hide posframe buffer and refresh if needed.

WINDOW holds the window in which posframe resides."
  (posframe-hide -posframe-buffer)
  (when (eq window (selected-window))
    (posframe-show -posframe-buffer
                   :position -last-end-position))
  (setq -preview-visible nil))

:autoload
(define-minor-mode mode
  "Instant preview of LaTeX in org-mode"
  nil nil nil
  (if mode
      (progn
        (add-hook 'post-command-hook #'-prepare-render nil t)
        (add-hook 'window-state-change-functions #'-clear-refresh-maybe nil t))
    (remove-hook 'post-command-hook #'-prepare-render t)
    (remove-hook 'window-state-change-functions #'-clear-refresh-maybe t)
    (when -timer
      (cancel-timer -timer))
    (posframe-hide -posframe-buffer)
    (setq -process nil))))

(provide 'org-latex-instant-preview)
;;; org-latex-instant-preview.el ends here
