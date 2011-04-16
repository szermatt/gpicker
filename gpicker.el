;;; gpicker.el --- gpicker integration for Emacs

;; Copyright (C) 2008 Aliaksey Kandratsenka

;; Author: Aliaksey Kandratsenka <alk@tut.by>
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
;; Arrange loading of this file. One way is to add
;;
;;   (load "/usr/local/share/gpicker/gpicker.el")
;;
;; to your .emacs
;;
;; Run `gpicker-visit-project' to specify project directory.
;;
;; Open files by running `gpicker-find-file' or
;; `gpicker-find-file-other-window' or
;; `gpicker-find-file-other-frame'.
;;
;; I prefer to globally bind them to Super-f, C-x 4 Super-f and
;; C-x 5 Super-f correspondingly. You can do same by adding the following
;; elisp snippet to your .emacs
;;
;;   (global-set-key [8388710] 'gpicker-find-file)
;;   (global-set-key [?\C-x ?4 8388710] 'gpicker-find-file-other-window)
;;   (global-set-key [?\C-x ?5 8388710] 'gpicker-find-file-other-frame)
;;
;; On most keyboards Super is a key with flag between Ctrl and Alt. On
;; mac keyboards it's Command key.

;;; Code

(require 'ffap)
(require 'cl)

(defvar *gpicker-path* "gpicker")
(defvar *gpicker-simple-path* nil)
(defvar *gpicker-extra-args* '("--disable-bzr" ;; bzr builtin file listing is just too slow
                               "--load-stdin-too"
                               "--multiselect"))
(defvar *gpicker-project-dir* nil)
(defvar *gpicker-project-type* "guess")
(defvar *gpicker-errors-log* (expand-file-name "~/gpicker-errors.log"))
(defvar *gpicker-buffers-list* (expand-file-name "~/gpicker-buffers-list"))
(defvar *gpicker-gui* t)

(defun gpicker-delete-file (path)
  (condition-case e
      (delete-file path)
    (error nil)))

(defun gpicker-get-simple-path ()
  (or *gpicker-simple-path*
      (concat *gpicker-path* "-simple")))

(defun gpicker-grab-stdout (&rest call-process-args)
  (with-temp-buffer
    (unwind-protect (let ((status (apply #'call-process
                                         (car call-process-args)
                                         (and (file-exists-p *gpicker-buffers-list*)
                                              *gpicker-buffers-list*)
                                         (list (current-buffer) *gpicker-errors-log*)
                                         nil
                                         (cdr call-process-args))))
                      (if (eql status 0)
                          (buffer-string)
                        (message "%s exited with status %d" (car call-process-args) status)
                        (save-excursion
                          (set-buffer "*Messages*")
                          (goto-char (point-max))
                          (insert-file-contents *gpicker-errors-log*))
                        nil))
      (progn
        (gpicker-delete-file *gpicker-errors-log*)
        (gpicker-delete-file *gpicker-buffers-list*)))))

(defun gpicker-pick (dir)
  (unless *gpicker-project-dir*
    (error "visit gpicker project via 'gpicker-visit-project first!"))
  (let ((gpicker-args (append *gpicker-extra-args*
                               (list "-t"
                                     (or *gpicker-project-type* "default")
                                     dir)))
        (at-point (ffap-string-at-point)))
    (when (and at-point
               (> (string-bytes at-point) 0))
      (setq gpicker-args (list* "--init-filter" at-point gpicker-args)))
    (if *gpicker-gui*
	(gpicker-pick-gui gpicker-args)
      (gpicker-pick-nogui gpicker-args))))

(defun gpicker-pick-gui (gpicker-args)
  (with-temp-file *gpicker-buffers-list*
    (let ((standard-output (current-buffer)))
      (dolist (b (buffer-list))
	(let ((name (buffer-name b)))
	  (unless (or (eq (current-buffer) b)
		      (string= (substring name 0 1) " ")
		      (buffer-file-name b))
	    (princ (buffer-name b))
	    (princ "\0"))))))
  (unwind-protect (let ((rv (apply #'gpicker-grab-stdout
				   *gpicker-path*
				   gpicker-args)))
		    (and rv
			 (split-string rv "\0" t)))
    (discard-input)))

(defvar gpicker-nogui-map
    (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-s" 'gpicker-next-match)
    (define-key map "\C-r" 'gpicker-prev-match)
    (define-key map "\C-m" 'gpicker-exit-minibuffer)
    map)
  "Minibuffer keymap for the nogui version of `gpicker-pick'.")

(defun gpicker-exit-minibuffer ()
  (throw 'gpicker-return))

;; straight from iswitch
(defun gpicker-chop (list elem)
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

(defun gpicker-next-match ()
  (interactive)
  (gpicker-update-matches (cadr gpicker-matches)))

(defun gpicker-prev-match ()
  (interactive)
  (gpicker-update-matches (car (last gpicker-matches))))

(defun gpicker-update-matches (elem)
  (setq gpicker-matches (gpicker-chop gpicker-matches elem)))

(defun gpicker-pick-nogui (gpicker-args)
  (let* ((process (apply #'start-process
			 "*gpicker*" nil *gpicker-path* "--nogui" gpicker-args))
	 (gpicker-tq (tq-create process))
	 (gpicker-matches nil)
	 (gpicker-last-search nil)
	 (gpicker-minibuf-depth (1+ (minibuffer-depth)))
	 (minibuffer-local-completion-map gpicker-nogui-map))
    (unwind-protect
	(progn
	  (catch 'gpicker-return
	    (completing-read "gpick " '(("dummy" . 1))))
	  (list (let ((selection (car gpicker-matches)))
		  (and selection (expand-file-name selection *gpicker-project-dir*)))))
	(when (eq 'run (process-status process))
	  (delete-process process)))))

(defun gpicker-nogui-complete (string)
  (let ((dest (cons nil nil)))
    (tq-enqueue gpicker-tq (concat "?" string "\n") "\3" dest
		#'gpicker-pick-nogui--collect-result)
    (while (null (car dest))
      (accept-process-output))
    (cdr dest)))

(defun gpicker-pick-nogui--collect-result (dest answer)
  (setcar dest t)
  (setcdr dest (split-string answer "[\0\3]" t)))

(defvar gpicker-minibuf-depth
  "Value we expect to be returned by `minibuffer-depth' in the minibuffer.")

(defun gpicker-entryfn-p ()
  (eq gpicker-minibuf-depth (minibuffer-depth)))

(add-hook 'minibuffer-setup-hook 'gpicker-minibuffer-setup)

(defun gpicker-minibuffer-setup ()
  (when (gpicker-entryfn-p)
    (add-hook 'pre-command-hook 'gpicker-pre-command nil t)
    (add-hook 'post-command-hook 'gpicker-post-command nil t)))

(defun gpicker-pre-command ()
  (gpicker-tidy))

(defun gpicker-post-command ()
  (gpicker-exhibit))

(defun gpicker-tidy ()
  (if (and (boundp 'gpicker-eoinput)
	   gpicker-eoinput)
      (if (> gpicker-eoinput (point-max))
	  ;; Oops, got rug pulled out from under us - reinit:
	  (setq gpicker-eoinput (point-max))
	(let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	  (delete-region gpicker-eoinput (point-max)))))
  ;; Reestablish the local variable 'cause minibuffer-setup is weird:
  (make-local-variable 'gpicker-eoinput))

(defun gpicker-exhibit ()
  (let ((contents (buffer-substring (minibuffer-prompt-end) (point-max)))
	(buffer-undo-list t))
    (save-excursion
      (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
      (if (not (boundp 'gpicker-eoinput))
	  ;; In case it got wiped out by major mode business:
	  (make-local-variable 'gpicker-eoinput))
      (setq gpicker-eoinput (point)))
    (unless (string= gpicker-last-search contents)
      (setq gpicker-matches (gpicker-nogui-complete contents)))
    (setq gpicker-last-search contents)
    (gpicker-display-completion gpicker-matches)))

(defun gpicker-display-completion (completion)
  (save-excursion 
    (goto-char gpicker-eoinput)
    (if (null completion)
	(insert " [no match]")
      (progn
	(insert "{")
	(let ((start (point)))
	  (insert (car completion))
	  (put-text-property start (point) 'face 'iswitchb-current-match))
	(dolist (filename (cdr completion))
	  (insert " ")
	  (insert filename))
	(insert "}")))))

(defun gpicker-set-project-type (type)
  "Sets type of current gpicker project"
  (interactive (list (completing-read "Choose gpicker project type: "
                                      '("guess" "git" "hg"
                                        "bzr" "default" "mlocate")
                                      nil t nil nil "guess")))
  (setq *gpicker-project-type* type))

(defun gpicker-visit-project (dir)
  "Selects current project directory for gpicker"
  (interactive "DSelect directory:")
  (cd dir)
  (setq *gpicker-project-type* "guess")
  (setq *gpicker-project-dir* (expand-file-name dir)))

(defun gpicker-call-find-function (find-function original-file)
  (let ((file (expand-file-name file *gpicker-project-dir*)))
    (if (file-exists-p file)
        (let ((revert-without-query (list (regexp-quote (abbreviate-file-name file)))))
          (funcall find-function file))
    (let ((b (get-buffer original-file)))
      (and b (display-buffer b))))))

(defun gpicker-do-find (find-function)
  (let ((files (gpicker-pick *gpicker-project-dir*)))
    (if (and (eql find-function #'find-file)
             (eql (length files) 2))
        (progn
          (gpicker-call-find-function #'find-file (car files))
          (gpicker-call-find-function #'find-file-other-window (cadr files)))
      (dolist (file files)
        (gpicker-call-find-function find-function file)))))

(defun gpicker-find-file ()
  (interactive)
  (gpicker-do-find #'find-file))

(defun gpicker-find-file-other-window ()
  (interactive)
  (gpicker-do-find #'find-file-other-window))

(defun gpicker-find-file-other-frame ()
  (interactive)
  (gpicker-do-find #'find-file-other-frame))

;;; ido integration

(defvar *gpicker-hook-ido* t
  "*Use gpicker for filtering ido results")

(defadvice ido-set-matches-1 (around gpicker-ido-set-matches-1 activate)
  "Choose between the regular ido-set-matches-1 and gpicker-my-ido-match"
  (if *gpicker-hook-ido*
      (setq ad-return-value (gpicker-my-ido-match ido-text (ad-get-arg 0)))
    ad-do-it))

(defun gpicker-ido-toggle ()
  "Toggle gpicker-ido integration"
  (interactive)
  (setq *gpicker-hook-ido* (not *gpicker-hook-ido*)))

(defun gpicker-my-ido-match (str items)
  (setq items (reverse items))
  (with-temp-file *gpicker-buffers-list*
    (let ((standard-output (current-buffer)))
      (dolist (item items)
        (princ item)
        (princ "\0"))))
  (let ((args (list "-n\\0" str)))
    (when (or ido-rotate (string= str ""))
      (push "-S" args)) ;; dont_sort
    (let ((out (apply #'gpicker-grab-stdout
                      (gpicker-get-simple-path)
                      args)))
      (split-string out "\0" t))))

(defun gpicker-complete-list (list &optional init-filter)
  (with-temp-file *gpicker-buffers-list*
    (let ((standard-output (current-buffer)))
      (dolist (name list)
        (princ name)
        (princ "\0"))))
  (unwind-protect (let ((rv (gpicker-grab-stdout *gpicker-path* "-l"
                                                 "--init-filter" (or init-filter (ffap-string-at-point))
                                                 "-")))
                    (and rv
                         (split-string rv "\0" t)))
    (discard-input)))

(defun alk-obarray-to-list (obarray)
  (let ((rv))
    (mapatoms (lambda (sym)
                (setq rv (cons (symbol-name sym) rv)))
              obarray)
    rv))

(defun gpicker-goto-tag ()
  (interactive)
  (find-tag (car (gpicker-complete-list (alk-obarray-to-list (tags-completion-table))))))

(defun gpicker-imenu ()
  (interactive)
  (require 'imenu)
  (let* ((imenu-alist (imenu--make-index-alist))
         (imenu (apply #'append
                       (mapcar (lambda (pair)
                                 (let ((val (cdr pair)))
                                  (if (listp val)
                                      nil ;; (mapcar #'car val)
                                    (cons (car pair) nil))))
                               imenu-alist)))
         (selected (car (gpicker-complete-list imenu))))
    (when selected
      (imenu-default-goto-function selected (cdr (assoc selected imenu-alist)))
      (recenter-top-bottom))))

(defun gpicker-isearch ()
  (interactive)
  (let ((old-current-buffer (current-buffer)))
    (with-temp-file *gpicker-buffers-list*
      (let ((standard-output (current-buffer)))
        (princ (with-current-buffer old-current-buffer
                 (buffer-substring 1 (buffer-end 1)))))))
  (let ((rv (gpicker-grab-stdout *gpicker-path* "-IlP" "-d" "\\n" "-n" "\\n" "-")))
    (and (> (length rv) 0)
         (let ((line-num (1+ (string-to-number rv 10))))
           (goto-line line-num)))))


(provide 'gpicker)
