;;; org-latex-instant-preview.el --- Preview org-latex Fragments Instantly via MathJax -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Sheng Yang
;;
;; Author:  Sheng Yang <styang@fastmail.com>
;; Maintainer:  Sheng Yang <styang@fastmail.com>
;; Created: June 03, 2020
;; Modified: June 03, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yangsheng6810/org-latex-instant-preview
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:
;;;

(eval-when-compile (require 'names))
(require 'image-mode)
(require 's)

;;;###autoload
(define-namespace org-latex-instant-preview-
;; (defgroup org-latex-instant-preview nil
;;   "Instant preview for org LaTeX snippets.")

(defcustom tex2svg-bin ""
  "Location of tex2svg executable."
  :group 'org-latex-instant-preview
  :type '(string))

(defcustom delay 0.2
  "Number of seconds to wait before a re-compilation."
  :group 'org-latex-instant-preview
  :type '(number))

(defconst -output-buffer "* org-latex-instant-preview-output-buffer *"
  "Buffer to hold the preview.")

(defvar -need-update nil)
(defvar -timer nil)

:autoload
(defun stop ()
  "Stop instant preview of LaTeX snippets."
  (interactive)
  (posframe-hide -output-buffer)
  (remove-hook 'after-change-functions #'-prepare-render)
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

:autoload
(defun start (&rest _)
  "Start instant preview."
  (interactive)
  (unless tex2svg-bin
    (message "You need to set org-latex-instant-preview-tex2svg-bin
for instant preview to work!")
    (error "Org-latex-instant-preview-tex2svg-bin is not set"))

  (add-hook 'after-change-functions #'-prepare-render nil t)
  (get-buffer-create -output-buffer)
  (let ((datum (org-element-context)))
    (when (memq (org-element-type datum) '(latex-environment latex-fragment))
	    (let ((ss (org-element-property :value datum))
            (end (org-element-property :end datum)))
        (when (memq (org-element-type datum) '(latex-fragment))
          (setq ss (-remove-math-delimeter ss)))
	      (-render ss)
        (-show end))))
  (setq -need-update nil))

(defun -render (tex-string)
  "Render TEX-STRING to buffer. Old version."
  (with-current-buffer -output-buffer
    (image-mode-as-text)
    (erase-buffer)
    (let ((ss (shell-command-to-string
               (format "%s \"%s\"" tex2svg-bin tex-string))))
      (insert ss))
    (image-mode)))

(defun -show (display-point)
  "Show preview posframe at DISPLAY-POINT."
  (when (posframe-workable-p)
    (posframe-show -output-buffer
                   :position display-point)))
)

(provide 'org-latex-instant-preview)
;;; org-latex-instant-preview.el ends here
