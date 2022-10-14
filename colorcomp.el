;;; colorcomp.el --- Color components display -*- lexical-binding: t; -*-

;; Copyright (C) 1990–1996, 1998–2022 Free Software Foundation, Inc.

;; Author: emacs-devel@gnu.org
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/colorcomp
;; Version: 0.1.0
;; Keywords: lisp tools
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Color components display

;; Here is a simple example using functions of the ewoc package to implement a color components display, an area in a buffer that represents a vector of three integers (itself representing a 24-bit RGB value) in various ways.

;;; Code:

(require 'ewoc)

(defvar colorcomp-ewoc nil)
(defvar colorcomp-labels ["[r]ed" "[g]reen" "[b]lue"])
(defvar colorcomp-data nil
  "A vector with RGB (red, green and blue) color levels.")

(defvar colorcomp-mode-map nil)
(defvar-local colorcomp-current-color nil)

(defun colorcomp-get-current-color ()
  "Format current color."
  (format "#%02X%02X%02X"
          (aref colorcomp-data 0)
          (aref colorcomp-data 1)
          (aref colorcomp-data 2)))

(defun colorcomp-get-bounds-by-chars (chars)
  "Return bounds of thing at point if it is match CHARS.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward chars)
                (point))))
      (if (string-blank-p
           (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun colorcomp-word-by-chars (chars)
  "Get thing at point matching CHARS.
Optional argument CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)"
  (when-let ((bounds (colorcomp-get-bounds-by-chars chars)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun colorcomp-pp (index)
  "Insert indicator of color's level in `colorcomp-data' at INDEX.
If INDEX is nil, create text sample."
  (if index
      (let* ((comp (aref colorcomp-data index))
             (title (aref colorcomp-labels index))
             (selected (and colorcomp-current-color
                            (= colorcomp-current-color index)))
             (current-color
              (pcase index
                (0 "red")
                (1 "green")
                (2 "blue"))))
        (insert
         (if selected
             (propertize title
                         'face
                         `(background-color .
                                            ,(colorcomp-get-current-color)))
           (progn (set-text-properties 0 (length title) nil title)
                  title))
         "\t: #x"
         (format "%02X" comp)
         " "
         (propertize (make-string (ash comp -2) ?#)
                     'face
                     `(foreground-color . ,current-color))
         "\n"))
    (let ((cstr (apply 'format "#%02X%02X%02X"
                       (mapcar (lambda (i)
                                 (aref colorcomp-data i))
                               '(0 1 2))))
          (samp " (sample text) "))
      (insert "Color\t: "
              (propertize samp 'face
                          `(foreground-color . ,cstr))
              (propertize samp 'face
                          `(background-color . ,cstr))
              "\n"))))

(defun colorcomp-hex-colors-in-buffer ()
  "Return list of hex colors in current buffer."
  (let ((colors))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[[:xdigit:]]\\{6\\}\\|#[[:xdigit:]]\\{3\\}"
                                nil t 1)
        (push (match-string-no-properties 0) colors)))
    colors))

(defun colorcomp-mark-region (beg end)
  "Mark region between BEG and END."
  (goto-char end)
  (push-mark end nil t)
  (goto-char beg))

(defun colorcomp-get-hex-color-at-point ()
  "Return hex color at point or nil."
  (when-let ((color
              (colorcomp-word-by-chars "#[[:xdigit:]]")))
    (when (string-match-p "#[[:xdigit:]]\\{6\\}\\|#[[:xdigit:]]\\{3\\}" color)
      color)))

(defvar colorcomp-color-buffer nil)
(defvar colorcomp-source-buffer nil)

;;;###autoload
(defun colorcomp (color)
  "Allow fiddling with COLOR in a new buffer."
  (interactive (list (or (colorcomp-get-hex-color-at-point)
                         (completing-read "Color (name or #RGB or #RRGGBB)"
                                          (colorcomp-get-hex-color-at-point) ))))
  (setq colorcomp-source-buffer (current-buffer))
  (when (string= "" color)
    (setq color "green"))
  (unless (color-values color)
    (error "No such color: %S" color))
  (setq colorcomp-color-buffer (generate-new-buffer (format "originally: %s"
                                                            color)))
  (switch-to-buffer-other-window
   colorcomp-color-buffer)
  (kill-all-local-variables)
  (setq major-mode 'colorcomp-mode
        mode-name "Color Components")
  (use-local-map colorcomp-mode-map)
  (erase-buffer)
  (buffer-disable-undo)
  (let ((data (apply #'vector (mapcar (lambda (n)
                                        (ash n -8))
                                      (color-values color))))
        (ewoc (ewoc-create 'colorcomp-pp
                           "\nColor Components\n\n"
                           "m - [m]ore
                             l - [l]ess
")))
    (set (make-local-variable 'colorcomp-data) data)
    (set (make-local-variable 'colorcomp-ewoc) ewoc)
    (ewoc-enter-last ewoc 0)
    (ewoc-enter-last ewoc 1)
    (ewoc-enter-last ewoc 2)
    (ewoc-enter-last ewoc nil)))

(defun colorcomp-mod (index limit delta)
  "Add DELTA to the color level in `colorcomp-data' at INDEX.
LIMIT is max value."
  (let ((cur (aref colorcomp-data index)))
    (unless (= limit cur)
      (aset colorcomp-data index (+ cur delta)))
    (ewoc-invalidate
     colorcomp-ewoc
     (ewoc-nth colorcomp-ewoc 0)
     (ewoc-nth colorcomp-ewoc 1)
     (ewoc-nth colorcomp-ewoc 2)
     (ewoc-nth colorcomp-ewoc -1))))

(defun colorcomp-update-level (index delta)
  "Add DELTA to the color level in `colorcomp-data' at INDEX."
  (setq colorcomp-current-color index)
  (colorcomp-mod index (if (> delta 0)
                           255 0)
                 delta))

;;;###autoload
(defun colorcomp-red-more (_a)
  "Increase red level."
  (interactive "p")
  (colorcomp-update-level 0 1))

;;;###autoload
(defun colorcomp-green-more (_a)
  "Increase green level."
  (interactive "p")
  (colorcomp-update-level 1 1))

;;;###autoload
(defun colorcomp-blue-more (_a)
  "Increase blue level."
  (interactive "p")
  (colorcomp-update-level 2 1))

;;;###autoload
(defun colorcomp-red-less (_a)
  "Decrease red level."
  (interactive "p")
  (colorcomp-update-level 0 -1))

;;;###autoload
(defun colorcomp-green-less (_a)
  "Decrease green level."
  (interactive "p")
  (colorcomp-update-level 1 -1))

;;;###autoload
(defun colorcomp-blue-less (_a)
  "Decrease blue level."
  (interactive "p")
  (colorcomp-update-level 2 -1))

;;;###autoload
(defun colorcomp-copy-as-kill-and-exit ()
  "Copy the color components into the kill ring and kill the buffer.
The string is formatted #RRGGBB (hash followed by six hex digits)."
  (interactive)
  (kill-new (colorcomp-get-current-color))
  (kill-buffer nil))

(defun colorcomp-defrepeatable (alist)
  "Make `colorcomp-defrepeatable' command from ALIST."
  (let ((keymap (make-sparse-keymap))
        (func (cdar alist)))
    (mapc (lambda (x)
            (define-key keymap (car x)
                        (cdr x)))
          alist)
    (lambda (arg)
      (interactive "p")
      (funcall func arg)
      (set-transient-map keymap t))))

(setq colorcomp-mode-map
      (let ((m (make-sparse-keymap)))
        (suppress-keymap m)
        (define-key m "r"
                    (colorcomp-defrepeatable
                     '(("m" . colorcomp-red-more)
                       ("l" . colorcomp-red-less))))
        (define-key m "b"
                    (colorcomp-defrepeatable
                     '(("m" . colorcomp-blue-more)
                       ("l" . colorcomp-blue-less))))
        (define-key m "g"
                    (colorcomp-defrepeatable
                     '(("m" . colorcomp-green-more)
                       ("l" . colorcomp-green-less))))
        (define-key m " " 'colorcomp-copy-as-kill-and-exit)
        (define-key m (kbd "RET") 'colorcomp-copy-as-kill-and-exit)
        m))

(provide 'colorcomp)
;;; colorcomp.el ends here