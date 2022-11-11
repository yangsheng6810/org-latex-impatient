;;; org-latex-impatient.el --- Preview org-latex Fragments Instantly via MathJax -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Sheng Yang
;;
;; Author:  Sheng Yang <styang@fastmail.com>
;; Created: June 03, 2020
;; Modified: October 04, 2020
;; Version: 0.1.0
;; Keywords: tex,tools
;; Homepage: https://github.com/yangsheng6810/org-latex-instant-preview
;; Package-Requires: ((emacs "26") (s "1.8.0") (posframe "0.8.0") (org "9.3") (dash "2.17.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package provides instant preview of LaTeX snippets via MathJax outputed
;;  SVG.
;;
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Code:
;;;

(require 'image-mode)
(require 's)
(require 'dash)
(require 'org)
(require 'posframe)
(require 'org-element)

(defcustom org-latex-impatient-tex2svg-bin ""
  "Location of tex2svg executable."
  :group 'org-latex-impatient
  :type '(string))

(defcustom org-latex-impatient-delay 0.1
  "Number of seconds to wait before a re-compilation."
  :group 'org-latex-impatient
  :type '(number))

(defcustom org-latex-impatient-scale 1.0
  "Scale of preview."
  :group 'org-latex-impatient
  :type '(float))

(defcustom org-latex-impatient-border-color "black"
  "Color of preview border."
  :group 'org-latex-impatient
  :type '(color))

(defcustom org-latex-impatient-border-width 1
  "Width of preview border."
  :group 'org-latex-impatient
  :type '(integer))

(defcustom org-latex-impatient-user-latex-definitions
  '("\\newcommand{\\ensuremath}[1]{#1}"
    "\\renewcommand{\\usepackage}[2][]{}")
  "Custom LaTeX definitions used in preview.

\\usepackage redefined since MathJax does not support it"
  :group 'org-latex-impatient
  :type '(repeat string))

(defcustom org-latex-impatient-posframe-position-handler
  #'org-latex-impatient-poshandler
  "The handler for posframe position."
  :group 'org-latex-impatient
  :type '(function))

(defcustom org-latex-impatient-posframe-position
  'tex-end
  "The position that will be used by posframe handler."
  :group 'org-latex-impatient
  :type '(choice (const :tag "Current Point" point)
                 (const :tag "End of TeX String" tex-end)))

(defcustom org-latex-impatient-inhibit-envs
  '("tikzpicture")
  "List of environments that shall not preview"
  :group 'org-latex-impatient
  :type '(repeat string))

(defcustom org-latex-impatient-inhibit-commands
  '("\\tikz")
  "List of commands that shall not preview"
  :group 'org-latex-impatient
  :type '(repeat string))

(defconst org-latex-impatient--output-buffer-prefix "*org-latex-impatient*"
  "Prefix for buffer to hold the output.")

(defconst org-latex-impatient--posframe-buffer "*org-latex-impatient*"
  "Buffer to hold the preview.")

(defvar org-latex-impatient-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-g" #'org-latex-impatient-abort-preview)
    map)
  "Keymap for reading input.")

(defvar org-latex-impatient--process nil)
(defvar org-latex-impatient--timer nil)
(defvar-local org-latex-impatient--last-tex-string nil)
(defvar-local org-latex-impatient--last-position nil)
(defvar-local org-latex-impatient--position nil)
(defvar-local org-latex-impatient--last-preview nil)
(defvar-local org-latex-impatient--current-window nil)
(defvar-local org-latex-impatient--output-buffer nil)
(defvar-local org-latex-impatient--is-inline nil)
(defvar-local org-latex-impatient--force-hidden nil)

(defun org-latex-impatient-poshandler (info)
  "Default position handler for posframe.

Uses the end point of the current LaTeX fragment for inline math,
and centering right below the end point otherwise. Positions are
calculated from INFO."
  (if org-latex-impatient--is-inline
      (posframe-poshandler-point-bottom-left-corner info)
    (if (fboundp 'posframe-poshandler-point-window-center)
        (posframe-poshandler-point-window-center info)
      (org-latex-impatient--poshandler-point-window-center info))))

(defun org-latex-impatient--poshandler-point-window-center (info)
  "Posframe's position handler.

Get a position which let posframe stay right below current
position, centered in the current window. The structure of INFO
can be found in docstring of `posframe-show'.

This function will be removed once a similar poshandler is
available in upstream."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (ymax (plist-get info :parent-frame-height))
         (window (plist-get info :parent-window))
         (position-info (plist-get info :position-info))
         (header-line-height (plist-get info :header-line-height))
         (tab-line-height (plist-get info :tab-line-height))
         (y-top (+ (cadr (window-pixel-edges window))
                   tab-line-height
                   header-line-height
                   (- (or (cdr (posn-x-y position-info)) 0)
                      ;; Fix the conflict with flycheck
                      ;; https://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y position-info)) 0))
                   y-pixel-offset))
         (font-height (plist-get info :font-height))
         (y-bottom (+ y-top font-height)))
    (cons (+ window-left (/ (- window-width posframe-width) 2))
          (max 0 (if (> (+ y-bottom (or posframe-height 0)) ymax)
                     (- y-top (or posframe-height 0))
                   y-bottom)))))

(defun org-latex-impatient--clean-up ()
  "Clean up timer, process, and variables."
  (org-latex-impatient--hide)
  (when org-latex-impatient--process
    (kill-process org-latex-impatient--process))
  (when (get-buffer org-latex-impatient--output-buffer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer org-latex-impatient--output-buffer)))
  (setq org-latex-impatient--process nil
        org-latex-impatient--last-tex-string nil
        org-latex-impatient--last-position nil
        org-latex-impatient--current-window nil))

;;;###autoload
(defun org-latex-impatient-stop ()
  "Stop instant preview of LaTeX snippets."
  (interactive)
  ;; only needed for manual start/stop
  (remove-hook 'after-change-functions #'org-latex-impatient--prepare-timer t)
  (org-latex-impatient--hide)
  (org-latex-impatient--interrupt-rendering))

(defun org-latex-impatient--prepare-timer (&rest _)
  "Prepare timer to call re-compilation."
  (when org-latex-impatient--timer
    (cancel-timer org-latex-impatient--timer)
    (setq org-latex-impatient--timer nil))
  (if (and (or (derived-mode-p 'org-mode)
               (derived-mode-p 'latex-mode)
               (derived-mode-p 'markdown-mode))
           (org-latex-impatient--in-latex-p))
      (setq org-latex-impatient--timer
            (run-with-idle-timer org-latex-impatient-delay nil #'org-latex-impatient-start))
    (org-latex-impatient--hide)))

(defun org-latex-impatient--remove-math-delimeter (ss)
  "Chop LaTeX delimeters from SS."
  (unless org-latex-impatient--is-inline
    (setq org-latex-impatient--is-inline
          (or (s-starts-with? "\\(" ss)
              (s-starts-with? "$" ss))))
  (s-with ss
    (s-chop-prefixes '("$$" "\\(" "$" "\\["))
    (s-chop-suffixes '("$$" "\\)" "$" "\\]"))))

(defun org-latex-impatient--add-color (ss)
  "Wrap SS with color from default face."
  (let ((color (face-foreground 'default)))
    (format "\\color{%s}{%s}" color ss)))

(defun org-latex-impatient--equal-or-member (elm target)
  (if (listp target)
      (member elm target)
    (equal elm target)))

(defun org-latex-impatient--in-latex-p ()
  "Return t if current point is in a LaTeX fragment, nil otherwise."
  (cond ((derived-mode-p 'org-mode)
         (let ((datum (org-element-context)))
           (or (memq (org-element-type datum) '(latex-environment latex-fragment))
               (and (memq (org-element-type datum) '(export-block))
                    (equal (org-element-property :type datum) "LATEX")))))
        ((derived-mode-p 'latex-mode)
         (org-latex-impatient--tex-in-latex-p))
        ((derived-mode-p 'markdown-mode)
         (org-latex-impatient--equal-or-member 'markdown-math-face (get-text-property (point) 'face)))
        (t (message "We only support org-mode, latex-mode, and markdown-mode")
           nil)))

(defun org-latex-impatient--tex-in-latex-p ()
  "Return t if in LaTeX fragment in `latex-mode', nil otherwise."
  (let ((faces (face-at-point nil t)))
    (or (-contains? faces 'font-latex-math-face)
        (-contains? faces 'preview-face))))

(defun org-latex-impatient--has-latex-overlay ()
  "Return t if there is LaTeX overlay showing."
  (--first (or (overlay-get it 'xenops-overlay-type)
               (equal 'org-latex-overlay (overlay-get it 'org-overlay-type)))
           (append (overlays-at (point)) (overlays-at (1- (point))))))

(defun org-latex-impatient--get-tex-string ()
  "Return the string of LaTeX fragment."
  (cond ((derived-mode-p 'org-mode)
         (let ((datum (org-element-context)))
           (org-element-property :value datum)))
        ((derived-mode-p 'latex-mode)
         (let (begin end)
           (save-excursion
             (while (org-latex-impatient--tex-in-latex-p)
               (backward-char))
             (setq begin (1+ (point))))
           (save-excursion
             (while (org-latex-impatient--tex-in-latex-p)
               (forward-char))
             (setq end (point)))
           (let ((ss (buffer-substring-no-properties begin end)))
             (message "ss is %S" ss)
             ss)))
        ((derived-mode-p 'markdown-mode)
         (let (begin end)
           (save-excursion
             (setq begin (prop-match-beginning
                          (text-property--find-end-backward
                           (point) 'face 'markdown-math-face #'yang/equal-or-member))))
           (save-excursion
             (setq end (prop-match-end
                        (text-property--find-end-forward
                         (point) 'face 'markdown-math-face #'yang/equal-or-member)))
             (unless (looking-at (rx (or "$$" "\\]")))
               (setq org-latex-impatient--is-inline t)
               (message "setting is-line to %s" org-latex-impatient--is-inline)))
           (let ((ss (buffer-substring-no-properties begin end)))
             (message "ss is %S" ss)
             ss)))
        (t "")))

(defun org-latex-impatient--get-tex-position ()
  "Return the end position of LaTeX fragment."
  (cond ((eq org-latex-impatient-posframe-position 'tex-end)
         (cond ((derived-mode-p 'org-mode)
                (let ((datum (org-element-context)))
                  (org-element-property :end datum)))
               ((derived-mode-p 'latex-mode)
                (save-excursion
                  (while (org-latex-impatient--tex-in-latex-p)
                    (forward-char))
                  (point)))
               ((derived-mode-p 'markdown-mode)
                (save-excursion
                  (prop-match-end
                   (text-property--find-end-forward
                    (point) 'face 'markdown-math-face #'yang/equal-or-member))))
               (t (message "Only org-mode, latex-mode, and markdown-mode supported") nil)))
        ((eq org-latex-impatient-posframe-position 'point)
         (point))
        (t (message "org-latex-impatient-posframe-position set incorrectly"))))

(defun org-latex-impatient--need-remove-delimeters ()
  "Return t if need to remove delimeters."
  (cond ((derived-mode-p 'org-mode)
         (let ((datum (org-element-context)))
           (memq (org-element-type datum) '(latex-fragment))))
        ((derived-mode-p 'latex-mode)
         ;; (message "Not implemented.")
         t)
        ((derived-mode-p 'markdown-mode)
         ;; (message "Not implemented.")
         t)
        (t "")))

(defun org-latex-impatient--get-headers ()
  "Return a string of headers."
  (cond ((derived-mode-p 'org-mode)
         (plist-get (org-export-get-environment
                     (org-export-get-backend 'latex))
                    :latex-header))
        ((derived-mode-p 'latex-mode)
         ;; (message "Get header not supported in latex-mode yet.")
         "")
        ((derived-mode-p 'markdown-mode)
         ;; (message "Get header not supported in markdown-mode yet.")
         "")
        (t "")))

(defun org-latex-impatient-inhibit (tex-string)
  "Returns t if we do not want TEX-STRING to preview."
  ;; checks for tikz
  (or (s-matches-p
       (rx-to-string
        `(: "\\begin{" (or ,@org-latex-impatient-inhibit-envs)))
       tex-string)
      (s-matches-p
       (rx-to-string
        `(or ,@org-latex-impatient-inhibit-commands))
       tex-string)))

;;;###autoload
(defun org-latex-impatient-start (&rest _)
  "Start instant preview."
  (interactive)
  (unless (and (not (string= org-latex-impatient-tex2svg-bin ""))
               (executable-find org-latex-impatient-tex2svg-bin))
    (message "You need to set org-latex-impatient-tex2svg-bin
for instant preview to work!")
    (error "Variable org-latex-impatient-tex2svg-bin is not set correctly"))

  ;; Only used for manual start
  (when (equal this-command #'org-latex-impatient-start)
    (add-hook 'after-change-functions #'org-latex-impatient--prepare-timer nil t))

  (if (and (or (derived-mode-p 'org-mode)
               (derived-mode-p 'latex-mode)
               (derived-mode-p 'markdown-mode))
       (org-latex-impatient--in-latex-p)
       (not (org-latex-impatient--has-latex-overlay)))
      (let ((--dummy-- (setq org-latex-impatient--is-inline nil))
            (tex-string (org-latex-impatient--get-tex-string))
            (latex-header
             (concat (s-join "\n" org-latex-impatient-user-latex-definitions)
                     "\n"
                     (org-latex-impatient--get-headers))))
        (unless (org-latex-impatient-inhibit tex-string)
          (setq org-latex-impatient--current-window (selected-window))
          ;; the tex string from latex-fragment includes math delimeters like
          ;; $, $$, \(\), \[\], and we need to remove them.
          (when (org-latex-impatient--need-remove-delimeters)
            (setq tex-string (org-latex-impatient--remove-math-delimeter tex-string)))

          (setq org-latex-impatient--position (org-latex-impatient--get-tex-position)
                ;; set forground color for LaTeX equations.
                tex-string (concat latex-header
                                   (org-latex-impatient--add-color tex-string)))
          (if (and org-latex-impatient--last-tex-string
                   (equal tex-string
                          org-latex-impatient--last-tex-string))
              ;; TeX string is the same, we only need to update posframe
              ;; position.
              (when (and org-latex-impatient--last-position
                         (equal org-latex-impatient--position org-latex-impatient--last-position)
                         ;; do not force showing posframe when a render
                         ;; process is running.
                         (not org-latex-impatient--process)
                         (not org-latex-impatient--force-hidden))
                (org-latex-impatient--show))
            ;; reset `-force-hidden'
            (setq org-latex-impatient--force-hidden nil)
            ;; A new rendering is needed.
            (org-latex-impatient--interrupt-rendering)
            (org-latex-impatient--render tex-string))))
    ;; Hide posframe when not on LaTeX
    (org-latex-impatient--hide)))

(defun org-latex-impatient--interrupt-rendering ()
  "Interrupt current running rendering."
  (when org-latex-impatient--process
    (condition-case nil
        (kill-process org-latex-impatient--process)
      (error "Faild to kill process"))
    (setq org-latex-impatient--process nil
          ;; last render for tex string is invalid, therefore need to invalid
          ;; its cache
          org-latex-impatient--last-tex-string nil
          org-latex-impatient--last-preview nil))
  (when (get-buffer org-latex-impatient--output-buffer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer org-latex-impatient--output-buffer))))

(defun org-latex-impatient--render (tex-string)
  "Render TEX-STRING to buffer, async version.

Showing at point END"
  (let (message-log-max)
    (message "Instant LaTeX rendering"))
  (org-latex-impatient--interrupt-rendering)
  (setq org-latex-impatient--last-tex-string tex-string)
  (setq org-latex-impatient--last-position org-latex-impatient--position)
  (get-buffer-create org-latex-impatient--output-buffer)

  (setq org-latex-impatient--process
        (make-process
         :name "org-latex-impatient"
         :buffer org-latex-impatient--output-buffer
         :command (append (list org-latex-impatient-tex2svg-bin
                                tex-string)
                          (when org-latex-impatient--is-inline
                            '("--inline")))
         ;; :stderr ::my-err-buffer
         :sentinel
         (lambda (&rest _)
           (condition-case nil
               (progn
                 (org-latex-impatient--fill-posframe-buffer)
                 (org-latex-impatient--show)
                 (when (get-buffer org-latex-impatient--output-buffer)
                   (let ((kill-buffer-query-functions nil))
                     (kill-buffer org-latex-impatient--output-buffer))))
             (error nil))
           ;; ensure -process is reset
           (setq org-latex-impatient--process nil)))))

(defun org-latex-impatient--insert-into-posframe-buffer (ss)
  "Insert SS into posframe buffer."
  (buffer-disable-undo org-latex-impatient--posframe-buffer)
  (let ((inhibit-message t)
        (message-log-max nil))
    (with-current-buffer org-latex-impatient--posframe-buffer
      (image-mode-as-text)
      (erase-buffer)
      (insert ss)
      (image-mode))))

(defun org-latex-impatient--fill-posframe-buffer ()
  "Write SVG in posframe buffer."
  (let ((ss (with-current-buffer org-latex-impatient--output-buffer
              (buffer-string))))
    (unless (get-buffer org-latex-impatient--posframe-buffer)
      (get-buffer-create org-latex-impatient--posframe-buffer))
    ;; when compile error, ss is exactly the error message, so we do nothing.
    ;; Otherwise when compile succeed and need scaling, do some hacks
    (when (and (s-contains-p "svg" ss)
               (not (equal org-latex-impatient-scale 1.0)))
      (setq ss
            (concat
             ;; 100% seems wierd
             "<svg height=\"110%\">"
             ;; ad-hoc for scaling
             (format "<g transform=\"scale(%s)\">" org-latex-impatient-scale)
             ss
             "</g></svg>")))
    (org-latex-impatient--insert-into-posframe-buffer ss)
    (setq org-latex-impatient--last-preview ss)))

(defun org-latex-impatient--show (&optional display-point)
  "Show preview posframe at DISPLAY-POINT."
  (unless display-point
    (setq display-point org-latex-impatient--position))
  (when (and org-latex-impatient--current-window
             (posframe-workable-p)
             (<= (window-start) display-point (window-end))
             (not org-latex-impatient--force-hidden))
    (unless (get-buffer org-latex-impatient--posframe-buffer)
      (get-buffer-create org-latex-impatient--posframe-buffer)
      (when (and org-latex-impatient--last-preview
                 (not (string= "" org-latex-impatient--last-preview)))
        ;; use cached preview
        (org-latex-impatient--insert-into-posframe-buffer org-latex-impatient--last-preview)))
    (let ((temp org-latex-impatient--is-inline))
      (with-current-buffer org-latex-impatient--posframe-buffer
        (setq org-latex-impatient--is-inline temp)))

    ;; handle C-g
    (define-key org-latex-impatient-keymap (kbd "C-g") #'org-latex-impatient-abort-preview)
    (posframe-show org-latex-impatient--posframe-buffer
                   :position display-point
                   :poshandler org-latex-impatient-posframe-position-handler
                   :parent-window org-latex-impatient--current-window
                   :internal-border-width org-latex-impatient-border-width
                   :internal-border-color org-latex-impatient-border-color
                   :hidehandler #'posframe-hidehandler-when-buffer-switch)))

(defun org-latex-impatient--hide ()
  "Hide preview posframe."
  (define-key org-latex-impatient-keymap (kbd "C-g") nil)
  (posframe-hide org-latex-impatient--posframe-buffer)
  (when (get-buffer org-latex-impatient--posframe-buffer)
    (setq org-latex-impatient--last-preview
          (with-current-buffer org-latex-impatient--posframe-buffer
            (let ((inhibit-message t)
                  (message-log-max nil))
              (image-mode-as-text)
              (buffer-substring-no-properties (point-min) (point-max)))))
    (kill-buffer org-latex-impatient--posframe-buffer)))

(defun org-latex-impatient-abort-preview ()
  "Abort preview."
  (interactive)
  (org-latex-impatient--interrupt-rendering)
  (define-key org-latex-impatient-keymap (kbd "C-g") nil)
  (setq org-latex-impatient--force-hidden t)
  (org-latex-impatient--hide))

;;;###autoload
(define-minor-mode org-latex-impatient-mode
  "Instant preview of LaTeX in org-mode"
  :global nil 
  :init-value nil 
  :keymap org-latex-impatient-keymap
  (if org-latex-impatient-mode
      (progn
        (setq org-latex-impatient--output-buffer
              (concat org-latex-impatient--output-buffer-prefix (buffer-name)))
        (add-hook 'post-command-hook #'org-latex-impatient--prepare-timer nil t))
    (remove-hook 'post-command-hook #'org-latex-impatient--prepare-timer t)
    (org-latex-impatient-stop)))

(provide 'org-latex-impatient)
;;; org-latex-impatient.el ends here
