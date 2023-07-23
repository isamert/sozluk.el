;;; sozluk.el --- An online Turkish dictionary  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 1.0.0
;; Homepage: https://github.com/isamert/sozluk.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dash "2.11.0"))

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

;; An online Turkish dictionary.  Uses sozluk.gov.tr and
;; etimolojiturkce.com.

;;; Code:

(require 'dash)
(require 'url)
(require 'org)
(require 'turkish nil t)
(eval-when-compile (require 'subr-x))

(defgroup sozluk nil
  "An online Turkish dictionary."
  :group 'dictionary)

(defcustom sozluk-switch-to-buffer-fn #'switch-to-buffer-other-window
  "Which function to use while opening the result buffer."
  :type 'function
  :group 'sozluk)

(defcustom sozluk-fill-paragraph t
  "Whether to call `org-fill-paragraph' on buffer or not."
  :type 'boolean
  :group 'sozluk)

(defcustom sozluk-include-etymology-on-sozluk nil
  "Whether to append etymology results in `sozluk' calls."
  :type 'boolean
  :group 'sozluk)

(defcustom sozluk-deasciify-if-not-found nil
  "When a word is not found, de-asciify the word and try again.
This requires `turkish' package to be installed.  See
https://github.com/emres/turkish-mode"
  :type 'boolean
  :group 'sozluk)

(defun sozluk--string-blank? (s)
  "Check if S is nil or blank."
  (or (null s) (string= "" s)))

(defun sozluk--request (url &optional url-params)
  "Do a request for URL, optionally pass URL-PARAMS.
URL-PARAMS should be an alist in the form of
`url-build-query-string' requires."
  (with-temp-buffer
    (let ((qstr (url-build-query-string url-params)))
      (url-insert-file-contents (format "%s%s%s" url (if (sozluk--string-blank? qstr) "" "?") qstr))
      (json-parse-buffer :object-type 'alist :array-type #'list :null-object nil))))

(defun sozluk--switch-to-buffer-for (input)
  "Create and switch to buffer for INPUT.
For switching, `sozluk-switch-to-buffer-fn' is used."
  (let ((buffer (get-buffer-create (format "*sozluk: %s*" input))))
    (funcall sozluk-switch-to-buffer-fn buffer)
    (with-current-buffer buffer
      (erase-buffer))
    buffer))

(defun sozluk--region-or-word ()
  "Request a word from user.
If there is an active region, it will be used as the initial
input."
  (read-string
   "Kelime: "
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (thing-at-point 'word t))))

(defun sozluk--orgify-etymology-html (input)
  "Convert given html INPUT into `org-mode' format.
This is not a full-fledged HTML to `org-mode' converter, just
converts what it's needed for this package.  Mostly basic markup
and links are converted."
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
                         (regexp-opt (mapcar #'car replacements))
                         (lambda (it) (alist-get it replacements nil nil #'string-equal))
                         substr t t)))
         (cond
          ((string-prefix-p "<a " orgified)
           (save-match-data
             (string-match "\"/kelime/\\(\\w+\\)\"" orgified)
             (format "[[elisp:(sozluk-etymology \"%s\")][%s]]"
                     (match-string 1 orgified)
                     (match-string 1 orgified))))
          ((and
            (string-prefix-p "<span " orgified)
            (string-match-p "hidden" orgified))
           "")
          (t orgified))))
     input))))

(defun sozluk--finalize-buffer ()
  "Tidy up the result buffer."
  (org-mode)
  (when sozluk-fill-paragraph
    (goto-char (point-min))
    (org-fill-paragraph)
    (while (eq (org-forward-paragraph) 0)
      (org-fill-paragraph)))
  (org-fill-paragraph)
  (goto-char (point-min)))

;;;###autoload
(defun sozluk-etymology (input &optional use-current-buffer)
  "Fetch the etymology of INPUT from api.etimolojiturkce.com and display it.
If USE-CURRENT-BUFFER is non-nil then insert the meaning to the
current buffer."
  (interactive (list (sozluk--region-or-word)))
  (unless use-current-buffer
    (sozluk--switch-to-buffer-for input))
  (insert "* Etimoloji\n")
  (--each (sozluk--request (format "https://api.etimolojiturkce.com/searchdetailed/%s" input))
    (insert
     (format
      "** %s\n%s\n\n"
      (alist-get 'kelime it)
      (sozluk--orgify-etymology-html (alist-get 'koken it)))))
  (sozluk--finalize-buffer))

;;;###autoload
(defun sozluk (input)
  "Fetch the meaning of INPUT from sozluk.gov.tr and display it."
  (interactive (list (sozluk--region-or-word)))
  (catch 'return
    (let ((result (sozluk--request "https://sozluk.gov.tr/gts" `(("ara" ,input)))))
      (when (alist-get 'error result)
        (if-let* ((sozluk-deasciify-if-not-found)
                  (deascified-input (with-temp-buffer
                                      (insert input)
                                      (turkish-correct-buffer)
                                      (string-trim (buffer-string))))
                  (different? (not (string-equal deascified-input input))))
            (progn
              (message "Böyle bir kelime yok, '%s' olarak arıyorum..." deascified-input)
              (sozluk deascified-input)
              (throw 'return nil))
          (user-error "Böyle bir kelime yok :(")))

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
                          ((not (sozluk--string-blank? specs))))
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
          (insert "\n")))
      (when sozluk-include-etymology-on-sozluk
        (ignore-errors
          (sozluk-etymology input t)))
      (sozluk--finalize-buffer))))

(provide 'sozluk)
;;; sozluk.el ends here
