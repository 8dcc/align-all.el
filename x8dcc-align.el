;;; x8dcc-align.el --- Set of functions for aligning text -*- lexical-binding: t; -*-

;; Author: 8dcc <8dcc.git@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/x8dcc/align.el

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

;;; Commentary:

;; This package is meant for personal use. For my full dotfiles, see:
;;
;;   <https://github.com/8dcc/emacs-dotfiles>

;;; Code:

(defun x8dcc-regexp-get-line-match (regex match-num paren-group)
  "Position of the MATCH-NUM-th match of the PAREN-GROUP subexpression in REGEX,
in the current line. PAREN-GROUP zero means the whole regexp, parentheses
indexes start at 1. MATCH-NUM starts at 1."
  (save-match-data
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
          (cur-pos 0)
          (ret nil))
      (dotimes (i match-num)
        (if (string-match regex line cur-pos)
            (setq ret (match-beginning paren-group)
                  cur-pos (1+ ret))))
      ret)))

(defun x8dcc-regexp-get-match-in-line (line regex match-num &optional
                                            paren-group absolute)
  "Position in the LINE of the MATCH-NUM-th match of REGEX. LINE is the line
number, and MATCH-NUM starts at 1. If ABSOLUTE, it will return the position
relative to the buffer, rather than the line."
  (unless paren-group (setq paren-group 0))
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (let ((match-line-pos (x8dcc-regexp-get-line-match
                           regex match-num paren-group)))
      (if (and absolute match-line-pos)
          (+ match-line-pos (line-beginning-position))
        match-line-pos))))

(defun x8dcc-regexp-rightmost-pos (line-start line-end regex regex-num
                                              &optional paren-group)
  "Position of the rightmost REGEX-NUM-th match of REGEX from LINE-START to
LINE-END. The returned position is relative to the start of the line, and
returns nil on failure. See `x8dcc-regexp-get-line-match'."
  (unless paren-group (setq paren-group 0))
  (let ((line-i line-start)
        (ret nil))
    (while (<= line-i line-end)
      (let ((match-line-pos (x8dcc-regexp-get-match-in-line
                             line-i regex regex-num paren-group)))
        (if (and match-line-pos
                 (or (null ret)
                     (< ret match-line-pos)))
            (setq ret match-line-pos)))
      (setq line-i (1+ line-i)))
    ret))

(defun x8dcc-add-padding (line-num regex regex-num rightmost-pos
                                   &optional paren-group)
  "Add spaces to the REGEX-NUM-th match of REGEX in LINE-NUM until it reaches
RIGHTMOST-POS. Returns RIGHTMOST-POS on success, and nil on failure."
  (unless paren-group (setq paren-group 0))
  (save-excursion
    (let ((match-line-pos (x8dcc-regexp-get-match-in-line
                           line-num regex regex-num paren-group nil))
          (match-abs-pos  (x8dcc-regexp-get-match-in-line
                           line-num regex regex-num paren-group 'absolute)))
      (unless (or (null match-line-pos)
                  (>= match-line-pos rightmost-pos))
        (goto-char match-abs-pos)
        (dotimes (i (- rightmost-pos match-line-pos))
          (insert " "))
        rightmost-pos))))

;;;###autoload
(defun x8dcc-align (line-start line-end regex &optional regex-num paren-group)
  "Align from LINE-START to LINE-END using the REGEX-NUM-th match of REGEX as
separator. If omited, REGEX-NUM defaults to 1 (the first match). Returns nil if
already aligned orno match found, or the last aligned position otherwise."
  (interactive
   (list (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end))
         (read-string "Regex: ")))
  (unless regex-num (setq regex-num 1))
  (unless paren-group (setq paren-group 0))
  (let ((rightmost-pos (x8dcc-regexp-rightmost-pos
                        line-start line-end regex regex-num paren-group))
        (line-i line-start)
        (ret nil))
    (unless (null rightmost-pos)
      (while (<= line-i line-end)
        (if (x8dcc-add-padding line-i regex regex-num rightmost-pos paren-group)
            (setq ret rightmost-pos))
        (setq line-i (1+ line-i)))
      ret)))

;;;###autoload
(defun x8dcc-align-all-regex (line-start line-end regex &optional
                                         regex-num-start regex-num-end
                                         paren-group)
  "Align from LINE-START to LINE-END, using all possible matches of REGEX as
separators, starting from match REGEX-NUM-START to REGEX-NUM-END. If
REGEX-NUM-END is omited, it will align until it finds an already aliged column,
or no match. See `x8dcc-align'"
  (interactive
   (list (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end))
         (read-string "Regex: ")))
  (unless regex-num-start (setq regex-num-start 1))
  (unless paren-group (setq paren-group 0))
  (if regex-num-end
      (while (<= regex-num-start regex-num-end)
        (x8dcc-align line-start line-end regex regex-num-start paren-group)
        (setq regex-num-start (1+ regex-num-start)))
    (let ((i regex-num-start))
      (while (x8dcc-align line-start line-end regex i paren-group)
        (setq i (1+ i)))
      i)))


;;;###autoload
(defun x8dcc-align-all (line-start line-end separator &optional
                                   col-num-start col-num-end)
  "Align from LINE-START to LINE-END, using SEPARATOR for delimiting columns. It
will align all columns from COL-NUM-START to COL-NUM-END. See
`x8dcc-align-all'."
  (interactive
   (list (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end))
         (read-string "Separator: ")))
  (x8dcc-align-all-regex line-start line-end
                         (concat "\\(" (regexp-quote separator) "\\)")
                         col-num-start col-num-end 1))

;;;###autoload
(defun x8dcc-align-all-left (line-start line-end separator &optional
                                        col-num-start col-num-end)
  "Like `x8dcc-align-all', but aligns the next space group instead of the actual
SEPARATOR."
  (interactive
   (list (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end))
         (read-string "Separator: ")
         1
         (read-number "Columns: ")))
  (x8dcc-align-all-regex line-start line-end
                         (concat (regexp-quote separator) " \\(\s-*?\\)")
                         col-num-start col-num-end 1))

(provide 'x8dcc-align-all)
;;; x8dcc-align-all.el ends here
