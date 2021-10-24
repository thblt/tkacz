;;; tkacz.el --- Emacs interface for Tkacz -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2020-2021 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintainer: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: bib
;; Homepage: https://github.com/thblt/tkacz
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.

;;; Code:

(require 'json)
(require 'tabulated-list)
(require 'wid-browse)
(require 'widget)

(defgroup tkacz nil
  "The strange document manager."
  :group 'applications)

;;;; Server process

(defvar tkacz--server-process
  nil
  "The Tkacz server process.")

;;;; Main view

(defun tkacz ()
  "Run Tkacz."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Tkacz*"))
  (tkacz-mode))

(define-derived-mode tkacz-mode widget-browse-mode "*Tkacz*"
  :group 'tkacz
  (setq-local mode-line-format tkacz-mode-line-format
              fill-column (window-body-width))
  (font-lock-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-minor-mode)
    (widget-insert (concat (propertize "Tkacz!\n" 'face 'bold)
                           (propertize "The strange reference manager" 'face 'italic)))
    (center-paragraph)
    (widget-insert "\n\n")

    (widget-create 'editable-field
                   :size (- fill-column 30)
                   :format "Query Tkacz: %v " ; Text after the field!
                   "")

    (widget-insert "\nSee also ")
    (use-local-map widget-keymap)
    (widget-setup)))

(defcustom tkacz-mode-line-format
  '(:eval
    (concat
     (propertize " *Tkacz*" 'face 'bold)
     (if (not tkacz-server-process)
         (propertize " Waiting for server…" 'face 'warning)
       " At your service.")))
  "Mode line definition for Tkacz's home view.")

;;;; List view

(define-derived-mode tkacz-list-mode tabulated-list-mode "*Tkacz*"
  :group 'tkacz
  (setq-local mode-line-format tkacz-mode-line-format)
  (hl-line-mode)
  (setq tabulated-list-format)
  )

;;;; Side views

(defun tkacz-side-view (buffer side action &optional noswap)
  "Control the display of BUFFER as a side view on SIDE.

BUFFER is a buffer, or nil.

SIDE is one of 'top, 'right, 'bottom or 'left.

ACTION can be:

 - nil to toggle the view (hide if visible, show if not)
 - 'hide to force hide
 - 'show or t to show the buffer.

By default, this will replace an existing side buffer, unless
NOSWAP is non-nil."
  )

;;;;; Customizations

(provide 'tkacz)

;;; tkacz.el ends here

;; Notes to self:
;; (display-buffer-in-side-window (get-buffer-create "*TZ collections*") '((side . top)))
;; (display-buffer-in-side-window (get-buffer-create "*TZ Notes*") '((side . right)))
;; (display-buffer-in-side-window (get-buffer-create "*TZ Details*") '((side . bottom)))
