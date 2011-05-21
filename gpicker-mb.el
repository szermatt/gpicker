;;; gpicker-mb.el --- gpicker minibuffer integration for Emacs

;; Copyright (C) 2008 Aliaksey Kandratsenka

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
;; Some of the minibuffer-handling code comes straight from iswitchb,
;; by Stephen Egler. It's been adapted to run with data coming from an
;; async process.

;;; Code

(defvar *gpicker-mb-path* nil)

(defun gpicker-mb-get-path ()
  "Return a path to gpicker-daemon."
  (or *gpicker-mb-path* (concat *gpicker-path* "-daemon")))

(defvar gpicker-md-max-result-count 8
  "Maximum number of results displayed in the minibuffer")

(defvar gpicker-mb-map
    (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-s" 'gpicker-mb-next-match)
    (define-key map "\C-r" 'gpicker-mb-prev-match)
    (define-key map "\C-m" 'exit-minibuffer)
    map)
  "Minibuffer keymap for `gpicker-mb-pick'.")

(defvar gpicker-mb-minibuf-depth
  "Value we expect to be returned by `minibuffer-depth' in the minibuffer.")

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

(defun gpicker-mb-pick (project-dir project-type)
  "Run gpicker on PROJECT-DIR with the specified PROJECT-TYPE in the minibuffer.

Return a list of files to open, relative to PROJECT-DIR.

Normally called by `gpicker-pick'."
  (let ((process (start-process
		  "*gpicker*" nil
		  (gpicker-mb-get-path) "-t" project-type project-dir)))
    (unwind-protect
	(let ((gpicker-mb-tq (tq-create process))
	      (gpicker-mb-matches nil)
	      (gpicker-mb-last-search nil)
	      (gpicker-mb-minibuf-depth (1+ (minibuffer-depth)))
	      (minibuffer-local-completion-map gpicker-mb-map))
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
	      (format "?%d:%s\n" gpicker-md-max-result-count query) "\0\0\n"
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
  (if (null gpicker-mb-matches)
      " [no match]"
    ;; matches
    (let ((text-list (list))
	  (first (copy-seq (car gpicker-mb-matches)))
	  (rest (cdr gpicker-mb-matches)))
      (push "{" text-list)
      (put-text-property 0 (length first) 'face 'iswitchb-current-match first)
      (push first text-list)
      (dolist (filename (cdr gpicker-mb-matches))
	(push " " text-list)
	(push filename text-list))
      (push "}" text-list)
      (apply 'concat (nreverse text-list)))))

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
