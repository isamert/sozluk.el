;;; sozluk.el --- An online Turkish dictonary  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/isamert/sozluk.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1") (dash "2.11.0"))

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

;; An online Turkish dictonary

;;; Code:

(require 'dash)
(require 'url)
(eval-when-compile (require 'subr-x))

(defgroup sozluk nil
  "An online Turkish dictonary"
  :group 'dictonary)

(defcustom sozluk-switch-to-buffer-fn #'switch-to-buffer-other-window
  "Which function to use while opening the result buffer."
  :type '(function)
  :group 'sozluk)

(defun sozluk--string-blank? (s)
  (or (null s) (string= "" s)))

(defun sozluk--request (url &optional url-params)
  (with-temp-buffer
    (let ((qstr (url-build-query-string url-params)))
      (url-insert-file-contents (format "%s%s%s" url (if (sozluk--string-blank? qstr) "" "?") qstr))
      (json-parse-buffer :object-type 'alist :array-type #'list :null-object nil))))

(defun sozluk--switch-to-buffer-for (input)
  (funcall sozluk-switch-to-buffer-fn (get-buffer-create (format "*sozluk: %s*" input))))

(defun sozluk--region-or-word ()
  (read-string
   "Kelime: "
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (thing-at-point 'word t))))

(defun sozluk--orgify-html (input)
  (string-trim
   (replace-regexp-in-string
    "\\(?:\\|</\\w+>\\)" ""
    (replace-regexp-in-string
     "<\\(\\w+\\)\\( [^>]+\\)?>\\(.+?\\)</\\(\\w+\\)>"
     (lambda (substr)
       (let* ((replacements
               '(("<b>" . "*")
                 ("</b>" . "*")
                 ("<i>" . "/")
                 ("</i>" . "/")))
              (orgified (replace-regexp-in-string
                         (regexp-opt (mapcar 'car replacements))
                         (lambda (it) (alist-get it replacements nil nil #'string-equal))
                         substr t t)))
         (cond
          ((string-prefix-p "<a " orgified)
           (save-match-data
             (string-match "\"/kelime/\\(\\w+\\)\"" orgified)
             (format "[[elisp:(sozluk \"%s\")][%s]]"
                     (match-string 1 orgified)
                     (match-string 1 orgified))))
          ((and
            (string-prefix-p "<span " orgified)
            (string-match-p "hidden" orgified))
           "")
          (t orgified))))
     input))))

;;;###autoload
(defun sozluk-etymology (input)
  (interactive (list (sozluk--region-or-word)))
  (sozluk--switch-to-buffer-for input)
  (insert "* Etimoloji\n")
  (--each (sozluk--request (format "https://api.etimolojiturkce.com/searchdetailed/%s" input))
    (insert
     (format
      "** %s\n%s\n\n"
      (alist-get 'kelime it)
      (sozluk--orgify-html (alist-get 'koken it)))))
  (org-mode)
  (goto-char (point-min)))

;;;###autoload
(defun sozluk (input)
  "Fetch the meaining of INPUT from sozluk.gov.tr and display it in
a nicely formatted org buffer."
  (interactive (list (sozluk--region-or-word)))
  (let ((result (sozluk--request "http://sozluk.gov.tr/gts" `(("ara" ,input)))))
    (when (alist-get 'error result)
      (user-error "Böyle bir kelime yok :("))
    (sozluk--switch-to-buffer-for input)
    (-each-indexed result
      (lambda (madde-index madde)
        (let-alist madde
          (insert (format "* %s (%s)\n" .madde (string-join (-repeat (1+ madde-index) "I"))))
          (--each-indexed .anlamlarListe
            (insert
             (format
              "%s. %s%s\n"
              (1+ it-index)
              (if-let* ((specs (string-join
                                (--map (format "/%s/" (alist-get 'tam_adi it)) (alist-get 'ozelliklerListe it))
                                ", "))
                        (_ (not (sozluk--string-blank? specs))))
                  (concat specs ". ") "")
              (alist-get 'anlam it)))
            (--each (alist-get 'orneklerListe it)
              (let-alist it
                (insert
                 (format
                  " : %s - *%s*\n" .ornek
                  (alist-get 'tam_adi (--find (string= (alist-get 'yazar_id it) .yazar_id) .yazar)))))))
          (unless (sozluk--string-blank? .birlesikler)
            (insert (format "\nBirleşikler: %s\n"
                            (string-join
                             (--map
                              (format "[[elisp:(sozluk \"%s\")][%s]]" it it)
                              (split-string .birlesikler ", "))
                             ", ")))))
        (insert "\n"))))
  (org-mode)
  (goto-char (point-min)))

(provide 'sozluk)
;;; sozluk.el ends here
