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

;; Workaround for defvar-local problem in names.el
(unless (fboundp 'names--convert-defvar-local)
  (defalias 'names--convert-defvar-local 'names--convert-defvar
    "Special treatment for `defvar-local' FORM."))

;;;###autoload
(define-namespace org-latex-instant-preview-
;; (defgroup org-latex-instant-preview nil
;;   "Instant preview for org LaTeX snippets.")

(defcustom tex2svg-bin ""
  "Location of tex2svg executable."
  :group 'org-latex-instant-preview
  :type '(string))

(defcustom delay 0.1
  "Number of seconds to wait before a re-compilation."
  :group 'org-latex-instant-preview
  :type '(number))

(defcustom scale 1.0
  "Scale of preview."
  :group 'org-latex-instant-preview
  :type '(float))

(defconst -output-buffer-prefix "*org-latex-instant-preview-output-buffer*"
  "Prefix for buffer to hold the output.")

(defconst -posframe-buffer "*org-latex-instant-preview-posframe-buffer*"
  "Buffer to hold the preview.")

(defvar -process nil)
(defvar -timer nil)
(defvar-local -last-tex-string nil)
(defvar-local -last-end-position nil)
(defvar-local -current-window nil)
(defvar-local -output-buffer nil)
(defvar-local -is-inline nil)


(defun -clean-up ()
  "Clean up timer, process, and variables."
  (when -timer
    (cancel-timer -timer)
    (setq -timer nil))
  (posframe-hide -posframe-buffer)
  (when -process
    (kill-process -process))
  (setq -process nil
        -last-tex-string nil
        -last-end-position nil
        -current-window nil
        -output-buffer nil))


:autoload
(defun stop ()
  "Stop instant preview of LaTeX snippets."
  (interactive)
  (posframe-hide -output-buffer)
  (remove-hook 'after-change-functions #'-prepare-render t)
  (remove-hook 'post-command-hook #'-prepare-render t)
  (when -timer
    (cancel-timer -timer)
    (setq -timer nil)))

(defun -prepare-render (&rest _)
  "Prepare timer to call re-compilation."
  (when (and (not (string= tex2svg-bin ""))
             (executable-find tex2svg-bin)
             (-in-latex-p))
    (unless -timer
      (setq -timer
            (run-with-idle-timer delay nil #'-timer-wrapper)))))

(defun -remove-math-delimeter (ss)
  "Chop LaTeX delimeters from SS."
  (setq -is-inline
        (or (s-starts-with? "\\(" ss)
            (s-starts-with? "$" ss)))
  (s-with ss
    (s-chop-prefixes '("$$" "\\(" "$" "\\["))
    (s-chop-suffixes '("$$" "\\)" "$" "\\]"))))

(defun -wrap-color (ss)
  "Wrap SS with color from default face."
  (let ((color (face-foreground 'default)))
    (format "\\color{%s}{%s}" color ss)))

(defun -in-latex-p ()
  "Return t if DATUM is in a LaTeX fragment, nil otherwise."
  (let ((datum (org-element-context)))
    (or (memq (org-element-type datum) '(latex-environment latex-fragment))
        (and (memq (org-element-type datum) '(export-block))
             (equal (org-element-property :type datum) "LATEX")))))

(defun -prepare-buffers ()
  "Prepare buffers for output and posframe."
  (setq -output-buffer
        (concat -output-buffer-prefix (buffer-name)))
  (get-buffer-create -output-buffer)
  (get-buffer-create -posframe-buffer))

(defun -timer-wrapper ()
  "Wrapper function that timer call."
  (when -timer
    (cancel-timer -timer))
  (if -process
      (setq -timer
            (run-with-idle-timer delay nil #'-timer-wrapper))
    (setq -timer nil)
    (start)))

:autoload
(defun start (&rest _)
  "Start instant preview."
  (interactive)
  (unless (and (not (string= tex2svg-bin ""))
               (executable-find tex2svg-bin))
    (message "You need to set org-latex-instant-preview-tex2svg-bin
for instant preview to work!")
    (error "Org-latex-instant-preview-tex2svg-bin is not set correctly"))

  ;; Only used for manual start
  (when (equal this-command #'start)
    (add-hook 'after-change-functions #'-prepare-render nil t))

  (let ((datum (org-element-context)))
    (if (-in-latex-p)
        (progn
          (setq -current-window (selected-window))
	        (let ((ss (org-element-property :value datum))
                (end (org-element-property :end datum)))
            ;; the tex string from latex-fragment includes math delimeters like
            ;; $, $$, \(\), \[\], and we need to remove them.
            (when (memq (org-element-type datum) '(latex-fragment))
              (setq ss (-remove-math-delimeter ss)))
            ;; set forground color for LaTeX equations.
            (setq ss (-wrap-color ss))
            (if (and -last-tex-string
                     (equal ss -last-tex-string))
                ;; TeX string is the same, we only need to update posframe
                ;; position.
                (when (and -last-end-position
                           (equal end -last-end-position)
                           ;; do not force showing posframe when a render
                           ;; process is running.
                           (not -process))
                  (-show end))
              ;; A new rendering is needed.
              (-interrupt-rendering)
              (-render ss end))))
      ;; Hide posframe when not on LaTeX fragments.
      (posframe-hide -posframe-buffer))))

(defun -interrupt-rendering ()
  "Interrupt current running rendering."
  (when -process
    (condition-case nil
        (kill-process -process)
      (error nil)))
  (with-current-buffer -output-buffer
    (erase-buffer))
  (setq -process nil
        ;; last render for tex string is invalid, therefore need to invalid
        ;; it's cache
        -last-tex-string nil))

(defun -render (tex-string end)
  "Render TEX-STRING to buffer, async version.

Showing at point END"
  (message "Instant LaTeX rendering")
  (setq -last-tex-string tex-string)
  (setq -last-end-position end)
  (-interrupt-rendering)
  (setq -process
        (make-process
         :name "org-latex-instant-preview"
         :buffer -output-buffer
         :command (append (list tex2svg-bin
                                tex-string)
                          (when -is-inline
                            '("--inline")))
         ;; :stderr ::my-err-buffer
         :sentinel
         (lambda (&rest _)
           (condition-case nil
               (progn
                 (-fill-posframe-buffer)
                 (-show end))
             (error nil))
           ;; ensure -process is reset
           (setq -process nil)))))

(defun -fill-posframe-buffer ()
  "Write SVG in posframe buffer."
  (let ((inhibit-message t)
        ;; (image-auto-resize scale)
        ;; work around for the fact that -output-buffer is buffer local
        (ss (with-current-buffer -output-buffer
              (buffer-string))))
    (with-current-buffer -posframe-buffer
      (image-mode-as-text)
      (erase-buffer)
      (if (not (s-contains-p "svg" ss))
          ;; compile error
          (insert ss)
        ;; successfully compiled
        ;; ad-hoc for scaling
        (insert (format
                 "<svg height=\"110%%\"><g transform=\"scale(%s)\">" scale))
        (insert ss)
        (insert "</g></svg>"))
      (image-mode))))

(defun -show (display-point)
  "Show preview posframe at DISPLAY-POINT."
  (when (and -current-window
             (posframe-workable-p)
             (< (window-start) display-point (window-end)))
    (posframe-show -posframe-buffer
                   :position display-point
                   :parent-window -current-window)))

(defun -clear-refresh-maybe (window &rest _)
  "Hide posframe buffer and refresh if needed.

WINDOW holds the window in which posframe resides."
  (posframe-hide -posframe-buffer)
  (when (and -current-window
             (eq window (selected-window)))
    (-show -last-end-position)))

:autoload
(define-minor-mode mode
  "Instant preview of LaTeX in org-mode"
  nil nil nil
  (if mode
      (progn
        (-prepare-buffers)
        (add-hook 'post-command-hook #'-prepare-render nil t)
        (add-hook 'window-state-change-functions #'-clear-refresh-maybe nil t))
    (remove-hook 'post-command-hook #'-prepare-render t)
    (remove-hook 'window-state-change-functions #'-clear-refresh-maybe t)
    (-clean-up))))

(provide 'org-latex-instant-preview)
;;; org-latex-instant-preview.el ends here
