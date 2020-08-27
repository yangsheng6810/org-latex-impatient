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

(defcustom user-latex-definitions
  '("\\newcommand{\\ensuremath}[1]{#1}")
  "Custom LaTeX definitions used in preview."
  :group 'org-latex-instant-preview
  :type '(repeat string))

(defconst -output-buffer-prefix "*org-latex-instant-preview*"
  "Prefix for buffer to hold the output.")

(defconst -posframe-buffer "*org-latex-instant-preview*"
  "Buffer to hold the preview.")

(defvar -process nil)
(defvar -timer nil)
(defvar-local -last-tex-string nil)
(defvar-local -last-position nil)
(defvar-local -position nil)
(defvar-local -last-preview nil)
(defvar-local -current-window nil)
(defvar-local -output-buffer nil)
(defvar-local -is-inline nil)


(defun -clean-up ()
  "Clean up timer, process, and variables."
  (-hide)
  (when -process
    (kill-process -process))
  (when (get-buffer -output-buffer)
    (kill-buffer -output-buffer))
  (setq -process nil
        -last-tex-string nil
        -last-position nil
        -current-window nil))

:autoload
(defun stop ()
  "Stop instant preview of LaTeX snippets."
  (interactive)
  ;; only needed for manual start/stop
  (remove-hook 'after-change-functions #'-prepare-timer t)
  (-hide)
  (-interrupt-rendering))

(defun -prepare-timer (&rest _)
  "Prepare timer to call re-compilation."
  (when -timer
    (cancel-timer -timer)
    (setq -timer nil))
  (if (and (eq major-mode 'org-mode)
           (-in-latex-p))
      (setq -timer
            (run-with-idle-timer delay nil #'start))
    (-hide)))

(defun -remove-math-delimeter (ss)
  "Chop LaTeX delimeters from SS."
  (setq -is-inline
        (or (s-starts-with? "\\(" ss)
            (s-starts-with? "$" ss)))
  (s-with ss
    (s-chop-prefixes '("$$" "\\(" "$" "\\["))
    (s-chop-suffixes '("$$" "\\)" "$" "\\]"))))

(defun -add-color (ss)
  "Wrap SS with color from default face."
  (let ((color (face-foreground 'default)))
    (format "\\color{%s}{%s}" color ss)))

(defun -in-latex-p ()
  "Return t if DATUM is in a LaTeX fragment, nil otherwise."
  (let ((datum (org-element-context)))
    (or (memq (org-element-type datum) '(latex-environment latex-fragment))
        (and (memq (org-element-type datum) '(export-block))
             (equal (org-element-property :type datum) "LATEX")))))

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
    (add-hook 'after-change-functions #'-prepare-timer nil t))

  (if (and (eq major-mode 'org-mode)
           (-in-latex-p))
      (let ((datum (org-element-context)))
        (setq -current-window (selected-window))
	(let ((tex-string (org-element-property :value datum))
              (latex-header
               (concat (s-join "\n" user-latex-definitions)
                       "\n"
                       (plist-get (org-export-get-environment
                                   (org-export-get-backend 'latex))
                                  :latex-header))))
          ;; the tex string from latex-fragment includes math delimeters like
          ;; $, $$, \(\), \[\], and we need to remove them.
          (when (memq (org-element-type datum) '(latex-fragment))
            (setq tex-string (-remove-math-delimeter tex-string)))

          (setq -position (org-element-property :end datum)
                ;; set forground color for LaTeX equations.
                tex-string (concat latex-header (-add-color tex-string)))
          (if (and -last-tex-string
                   (equal tex-string -last-tex-string))
              ;; TeX string is the same, we only need to update posframe
              ;; position.
              (when (and -last-position
                         (equal -position -last-position)
                         ;; do not force showing posframe when a render
                         ;; process is running.
                         (not -process))
                (-show))
            ;; A new rendering is needed.
            (-interrupt-rendering)
            (-render tex-string))))
    ;; Hide posframe when not on LaTeX
    (-hide)))

(defun -interrupt-rendering ()
  "Interrupt current running rendering."
  (when -process
    (condition-case nil
        (kill-process -process)
      (error "Faild to kill process"))
    (setq -process nil
          ;; last render for tex string is invalid, therefore need to invalid
          ;; its cache
          -last-tex-string nil
          -last-preview nil))
  (when (get-buffer -output-buffer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer -output-buffer))))

(defun -render (tex-string)
  "Render TEX-STRING to buffer, async version.

Showing at point END"
  (message "Instant LaTeX rendering")
  (-interrupt-rendering)
  (setq -last-tex-string tex-string)
  (setq -last-position -position)
  (get-buffer-create -output-buffer)

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
                 (-show)
                 (kill-buffer -output-buffer))
             (error nil))
           ;; ensure -process is reset
           (setq -process nil)))))

(defun -insert-into-posframe-buffer (ss)
  "Insert SS into posframe buffer."
  (buffer-disable-undo -posframe-buffer)
  (let ((inhibit-message t))
    (with-current-buffer -posframe-buffer
      (image-mode-as-text)
      (erase-buffer)
      (insert ss)
      (image-mode))))

(defun -fill-posframe-buffer ()
  "Write SVG in posframe buffer."
  (let ((ss (with-current-buffer -output-buffer
              (buffer-string))))
    (unless (get-buffer -posframe-buffer)
      (get-buffer-create -posframe-buffer))
    ;; when compile error, ss is exactly the error message, so we do nothing.
    ;; Otherwise when compile succeed, do some hacks
    (when (s-contains-p "svg" ss)
      (setq ss
            (concat
             ;; 100% seems wierd
             "<svg height=\"110%%\">"
             ;; ad-hoc for scaling
             (format "<g transform=\"scale(%s)\">" scale)
             ss
             "</g></svg>")))
    (-insert-into-posframe-buffer ss)
    (setq -last-preview ss)))

(defun -show (&optional display-point)
  "Show preview posframe at DISPLAY-POINT."
  (unless display-point
    (setq display-point -position))
  (when (and -current-window
             (posframe-workable-p)
             (< (window-start) display-point (window-end)))
    (unless (get-buffer -posframe-buffer)
      (get-buffer-create -posframe-buffer)
      (when (and -last-preview
                 (not (string= "" -last-preview)))
        ;; use cached preview
        (-insert-into-posframe-buffer -last-preview)))
    (posframe-show -posframe-buffer
                   :position display-point
                   :parent-window -current-window)))

(defun -hide ()
  "Hide preview posframe."
  (posframe-hide -posframe-buffer)
  (when (get-buffer -posframe-buffer)
    (setq -last-preview
      (with-current-buffer -posframe-buffer
        (let ((inhibit-message t))
          (image-mode-as-text)
          (buffer-string))))
    (kill-buffer -posframe-buffer)))

(defun -clear-refresh-maybe (window &rest _)
  "Hide posframe buffer and refresh if needed.

WINDOW holds the window in which posframe resides."
  (if (and -current-window
           (eq window (selected-window))
           (eq major-mode 'org-mode)
           (-in-latex-p))
      (progn
        (-show -last-position))
    (-hide)
    (-interrupt-rendering)))

:autoload
(define-minor-mode mode
  "Instant preview of LaTeX in org-mode"
  nil nil nil
  (if mode
      (progn
        (setq -output-buffer
              (concat -output-buffer-prefix (buffer-name)))
        (add-hook 'post-command-hook #'-prepare-timer nil t)
        (add-hook 'window-state-change-functions #'-clear-refresh-maybe nil t))
    (remove-hook 'post-command-hook #'-prepare-timer t)
    (remove-hook 'window-state-change-functions #'-clear-refresh-maybe t)
    (stop)))

;; end of namespace
)
(provide 'org-latex-instant-preview)
;;; org-latex-instant-preview.el ends here
