;;; gpicker-mb.el --- gpicker minibuffer integration for Emacs

;; Copyright (C) 2008 Aliaksey Kandratsenka
;; Copyright (C) 2013 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Version: 1.0
;; Keywords: autocompletion

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary
;;
;; This package replaces the gtk-based gpicker UI with an
;; iswitchb-like selection. This way, gpicker can be used even when
;; emacs is run inside of a terminal or on systems that don't support
;; GTK.
;;

;;; Usage
;;
;; To start a gpicker-mb session, call `gpicker-find-file',
;; `gpicker-find-file-other-window' or
;; `gpicker-find-file-other-frame' from a terminal window or set
;; `*gpicker-force-nogui*' to t to always use gpicker-mb instead of
;; opening a GTK window.
;;
;; When it is started, gpicker-mb displays the most interesting matches
;; in the minibuffer within { }. Gpicker-mb only displays up to
;; `gpicker-mb-max-result-count' results at a time. The file you're looking
;; for very likely doesn't appear on the initial list.
;;
;; To restrict the search, type some parts of the file you're looking
;; for. Gpicker-mb uses the exact same matching algorithm as the GTK
;; version.
;;
;; Once gpicker-mb displays the file you're looking for in the minibuffer,
;; type C-s or right to select the next match and C-r or left to select the
;; previous match. Repeat until the file you want is the first on the list
;; and press RET.
;;
;; This interface is very similar to iswitchb or IDO.
;;

;;; Customization
;;
;; To display more results when there are many matches, set
;; `gpicker-mb-max-result-count'.
;;
;; To modify the map used during a gpicker-mb session, modify
;; `gpicker-mb-map'.

;;; Acknowledgements
;;
;; Some of the minibuffer-handling code comes straight from iswitchb,
;; by Stephen Egler. It's been adapted to run with data coming from an
;; async process.
;;

(require 'tq)

;;; Code

(defvar *gpicker-mb-path* nil)

(defun gpicker-mb-get-path ()
  "Return a path to gpicker-daemon."
  (or *gpicker-mb-path* (concat *gpicker-path* "-daemon")))

(defvar gpicker-mb-max-result-count 8
  "Maximum number of results displayed in the minibuffer")

(defvar gpicker-mb-map
    (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [(control s)] 'gpicker-mb-next-match)
    (define-key map [(control r)] 'gpicker-mb-prev-match)
    (define-key map [(right)] 'gpicker-mb-next-match)
    (define-key map [(left)] 'gpicker-mb-prev-match)
    (define-key map [(return)] 'exit-minibuffer)
    (define-key map [(control m)] 'exit-minibuffer)
    (define-key map [(shift return)] 'gpicker-mb-open)
    (define-key map [(control x) ?4 (return)] 'gpicker-mb-open-other-window)
    (define-key map [(control x) ?5 (return)] 'gpicker-mb-open-other-frame)
    map)
  "Minibuffer keymap for `gpicker-mb-pick'.")

(defvar gpicker-mb-minibuf-depth nil
  "Value we expect to be returned by `minibuffer-depth' in the minibuffer.

This is an internal variable. It is set only while a gpicker
session is in progress.")

(defvar gpicker-mb-tq nil
  "Transaction queue created for the current gpicker session.

This is an internal variable. It is set only while a gpicker
session is in progress.")

(defvar gpicker-mb-matches nil
  "List of matching filenames, relative to `gpicker-mb-project-dir'.

The first match on this list is the active one.
`gpicker-mb-next-match' and `gpicker-mb-prev-match' manipulate
this list.

This is an internal variable. It is set only while a gpicker
session is in progress.")

(defvar gpicker-mb-project-dir nil
  "Root directory of the current gpicker session.

This is an internal variable. It is set only while a gpicker
session is in progress.")

;; inherit faces from iswitchb or icomplete
(if (or (> emacs-major-version 24)
	(and (= emacs-major-version 24) (>= emacs-minor-version 4)))
    (progn ;; emacs >= 24.4
      (require 'icomplete)
      (defface gpicker-mb-single-match
	'((t (:inherit icomplete-first-match)))
	"Face for the single matching file name.")
      (defface gpicker-mb-current-match
	'((t (:inherit icomplete-first-match)))
	"Face for the current matching file name."))
  (progn ;; emacs < 24.4
    (require 'iswitchb) 
    (defface gpicker-mb-single-match
      '((t (:inherit iswitchb-single-match)))
      "Face for the single matching file name.")
    (defface gpicker-mb-current-match
      '((t (:inherit iswitchb-current-match)))
      "Face for the current matching file name.")))

(defsubst gpicker-mb-update-matches (elem)
  "Call `gpicker-mb-chop' on `gpicker-mb-matches' with ELEM."
  (setq gpicker-mb-matches (gpicker-mb-chop gpicker-mb-matches elem)))

(defun gpicker-mb-next-match ()
  "Select the second matching file on the list.

Put the first matching file at the end of the list."
  (interactive)
  (gpicker-mb-update-matches (cadr gpicker-mb-matches)))

(defun gpicker-mb-prev-match ()
  "Select the last element on the list.

Put the last matching file at the beginning of the list."
  (interactive)
  (gpicker-mb-update-matches (car (last gpicker-mb-matches))))

(defun gpicker-mb-open (&optional open-func)
  "Open the current match using OPEN-FUNC.

This function calls OPEN-FUNC on the current match. If OPEN-FUNC
is nil, `open-file' is used.

The gpicker prompt continues after this call, so you can open
other files."
  (interactive)
  (let ((open-func (or open-func 'find-file))
	(match (car gpicker-mb-matches))
	(initial-window (selected-window)))
    (when match
      (funcall open-func
	       (expand-file-name match gpicker-mb-project-dir))
      (select-window initial-window))))

(defun gpicker-mb-open-other-window ()
  "Open the current match in another window.

See `gpicker-mb-open'."
  (interactive)
  (gpicker-mb-open 'find-file-other-window))

(defun gpicker-mb-open-other-frame ()
  "Open the current match in another frame.

See `gpicker-mb-open'."
  (interactive)
  (let ((initial-frame (selected-frame)))
    (gpicker-mb-open 'find-file-other-frame)
    (select-frame-set-input-focus initial-frame)))

(defun gpicker-mb-pick (project-dir project-type)
  "Run gpicker on PROJECT-DIR with the specified PROJECT-TYPE in the minibuffer.

Return a list of files to open, relative to PROJECT-DIR.

This function is normally called indirectly by
`gpicker-find-file', `gpicker-find-file-other-window' or
`gpicker-find-file-other-frame' when Emacs is running inside of a
terminal or when `*gpicker-force-nogui*' is t."
  (let ((process (start-process
		  "*gpicker*" nil
		  (gpicker-mb-get-path) "-t" project-type project-dir)))
    (unwind-protect
	(let ((gpicker-mb-tq (tq-create process))
	      (gpicker-mb-matches nil)
	      (gpicker-mb-last-search nil)
	      (gpicker-mb-minibuf-depth (1+ (minibuffer-depth)))
	      (gpicker-mb-project-dir project-dir)
	      (minibuffer-local-completion-map gpicker-mb-map)
	      ;; disable icomplete and iswitchb, to be sure they
	      ;; won't interfere.
	      (iswitchb-mode nil)
	      (icomplete-mode nil)
              (completing-read-function 'completing-read-default))
	  (completing-read "gpick " '(("dummy" . 1)))
	  (list (let ((selection (car gpicker-mb-matches)))
		  (and selection (expand-file-name selection project-dir)))))
      (when (eq 'run (process-status process))
	(delete-process process)))))

(defun gpicker-mb-run-async-query (query buffer)
  "Run the given QUERY asynchronously.

The result will eventually be displayed in the given BUFFER,
which should be the minibuffer."
  (gpicker-mb-set-text buffer "{...}")
  (setq gpicker-mb-matches nil)
  (tq-enqueue gpicker-mb-tq
	      (format "?%d:%s\n" gpicker-mb-max-result-count query) "\0\0\n"
	      (cons query buffer)
	      #'gpicker-mb-pick-collect-result)
  ;; if the answers comes back immediately, display the
  ;; result. Otherwise display {...} until the answers comes back.
  (accept-process-output nil 0.01))

(defun gpicker-mb-pick-collect-result (args answer)
  "Collect one response from gpicker-daemon and display the result.

This function is meant to be called from `tq-enqueue' with the
gpicker-daemon transaction queue.

ARGS should contain (query . buffer), the query that is run and
the minibuffer.

ANSWER should contain a complete answer from gpicker-daemon, including
the final \"\\0\\0\\n\""
  (condition-case err
      (let ((string (car args))
	    (buffer (cdr args)))
	(when (eq gpicker-mb-last-search string)
	  (let ((answers (split-string (substring answer 0 -3) "\0" t)))
	    (setq gpicker-mb-matches answers)
	    (gpicker-mb-set-text buffer (gpicker-mb-matches-as-string)))))
    (error (message "error collecting results: %s"
		    (error-message-string err)))))

(defun gpicker-mb-entryfn-p ()
  "Check whether we're currently in a gpicker-mb minibuffer."
  (eq gpicker-mb-minibuf-depth (minibuffer-depth)))

(defun gpicker-mb-minibuffer-setup ()
  "Setup a gpicker-mb minibuffer.

Called from `minibuffer-setup-hook'."
  (when (gpicker-mb-entryfn-p)
    (add-hook 'pre-command-hook 'gpicker-mb-clear-text nil t)
    (add-hook 'post-command-hook 'gpicker-mb-exhibit nil t)))
(add-hook 'minibuffer-setup-hook 'gpicker-mb-minibuffer-setup)

(defun gpicker-mb-exhibit ()
  "Run the typed query and displays the result in the minibuffer.

Should be called with the minibuffer as the current buffer."
  (gpicker-mb-clear-text)
  (let ((contents (buffer-substring (minibuffer-prompt-end) (point-max)))
	(buffer-undo-list t))
    (if (string= gpicker-mb-last-search contents)
	(gpicker-mb-set-text (current-buffer) (gpicker-mb-matches-as-string))
      (progn
	(setq gpicker-mb-last-search contents)
	(gpicker-mb-run-async-query contents (current-buffer))))))

(defun gpicker-mb-matches-as-string ()
  "Returns a string representation of a match list."
  (cond
   ((null gpicker-mb-matches)
    " [No match]")
   ;; single match
   ((null (cdr gpicker-mb-matches))
    (let ((text (concat "[" (car gpicker-mb-matches) "]")))
      (put-text-property 1 (1- (length text)) 'face 'gpicker-mb-single-match text)
      text))
   ;; multiple matches
   (t
    (let ((text-list (list))
	  (first (copy-seq (car gpicker-mb-matches)))
	  (rest (cdr gpicker-mb-matches)))
      (push "{" text-list)
      (put-text-property 0 (length first) 'face 'gpicker-mb-current-match first)
      (push first text-list)
      (dolist (filename (cdr gpicker-mb-matches))
	(push "," text-list)
	(push filename text-list))
      (when (equal (length gpicker-mb-matches) gpicker-mb-max-result-count)
	(push ",..." text-list))
      (push "}" text-list)
      (apply 'concat (nreverse text-list))))))

(defun gpicker-mb-set-text (minibuffer text)
  "Sets the completion text in to TEXT in the given MINIBUFFER

`gpicker-mb-clear-text' only deletes text inserted this way."
  (put-text-property 0 (length text) 'gpicker-mb t text)
  (with-current-buffer minibuffer
    (gpicker-mb-clear-text)
    (when (gpicker-mb-entryfn-p)
      (save-excursion
	(goto-char (point-max))
	(insert text)))))

(defun gpicker-mb-clear-text ()
  "Clear text inserted by `gpicker-mb-insert' from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (get-text-property (point) 'gpicker-mb)
	  (delete-region
	   (point) 
	   (next-single-property-change (point) 'gpicker-mb nil (point-max)))
	(goto-char (next-single-property-change
		    (point) 'gpicker-mb nil (point-max)))))))

(defun gpicker-mb-chop (list elem)
  "Remove all elements from LIST before ELEM and put them at the end."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car list))
      (if (equal next elem)
	  (setq ret (append list (nreverse sofar)))
	;; else
	(progn
	  (setq list (cdr list))
	  (setq sofar (cons next sofar)))))
    ret))


(provide 'gpicker-mb)
