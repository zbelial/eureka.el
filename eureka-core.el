;;; eureka-core.el  --- AI powered Development Environment for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zbelial zjyzhaojiyang@gmail.com

;; Author: zbelial zjyzhaojiyang@gmail.com

;; Maintainer: zbelial zjyzhaojiyang@gmail.com

;; Homepage: https://github.com/zbelial/eureka.el.git
;; Version: 0.1.0
;; Keywords: AI LLM Emacs Coding


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:


(eval-when-compile
  (require 'cl-macs))
(require 'cl-lib)

;; Copied from ellama and modified
(require 'llm)
(require 'llm-provider-utils)
(require 'spinner)
(require 'compat)
(eval-when-compile (require 'rx))
(require 'markdown-mode nil t)

(defgroup eureka nil
  "Tool for interacting with LLMs."
  :group 'tools)

(defcustom eureka-user-nick "User"
  "User nick in logs."
  :group 'eureka
  :type 'string)

(defcustom eureka-assistant-nick "Eureka"
  "Assistant nick in logs."
  :group 'eureka
  :type 'string)

(defcustom eureka-provider nil
  "Backend LLM provider."
  :group 'eureka
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom eureka-spinner-type 'progress-bar
  "Spinner type for eureka."
  :group 'eureka
  :type `(choice ,@(mapcar
		    (lambda (type)
		      `(const ,(car type)))
		    spinner-types)))

(defcustom eureka-command-map
  (let ((map (make-sparse-keymap)))
    ;; code
    (define-key map (kbd "c c") 'eureka-code-complete)
    (define-key map (kbd "c a") 'eureka-code-add)
    (define-key map (kbd "c e") 'eureka-code-edit)
    (define-key map (kbd "c i") 'eureka-code-improve)
    (define-key map (kbd "c r") 'eureka-code-review)
    map)
  "Keymap for eureka commands."
  :group 'eureka
  :type 'keymap)

(defun eureka-setup-keymap ()
  "Set up the Eureka keymap and bindings."
  (interactive)
  (when (boundp 'eureka-keymap-prefix)
    (defvar eureka-keymap (make-sparse-keymap)
      "Keymap for Eureka Commands")

    (when eureka-keymap-prefix
      (define-key global-map (kbd eureka-keymap-prefix) eureka-command-map))))

(defcustom eureka-keymap-prefix nil
  "Key sequence for Eureka Commands."
  :type 'string
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (when value
	   (eureka-setup-keymap)))
  :group 'eureka)

(defcustom eureka-auto-scroll nil
  "If enabled eureka buffer will scroll automatically during generation."
  :type 'boolean
  :group 'eureka)

(defcustom eureka-fill-paragraphs '(text-mode)
  "When to wrap paragraphs."
  :group 'eureka
  :type `(choice
          (const :tag "Never fill paragraphs" nil)
          (const :tag "Always fill paragraphs" t)
          (function :tag "By predicate")
          (repeat :tag "In specific modes" (symbol))))

(defcustom eureka-naming-scheme 'eureka-generate-name-by-words
  "How to name sessions.
If you choose custom function, that function should accept PROVIDER, ACTION
and PROMPT arguments.

PROVIDER is an llm provider.

LABEL is a prefix string."
  :group 'eureka
  :type `(choice
          (const :tag "By first N words of prompt" eureka-generate-name-by-words)
          (const :tag "By current time" eureka-generate-name-by-time)
          (function :tag "By custom function")))

(defcustom eureka-long-lines-length 100
  "Long lines length for fill paragraph call.
Too low value can break generated code by splitting long comment lines."
  :group 'eureka
  :type 'integer)

(defcustom eureka-session-auto-save t
  "Automatically save eureka sessions if set."
  :group 'eureka
  :type 'boolean)

(defcustom eureka-instant-display-action-function nil
  "Display action function for `eureka-instant'."
  :group 'eureka
  :type 'function)

(define-minor-mode eureka-session-mode
  "Minor mode for eureka session buffers."
  :interactive nil
  (if eureka-session-mode
      (progn
        (add-hook 'after-save-hook 'eureka--save-session nil t)
        (add-hook 'kill-buffer-hook 'eureka--session-deactivate nil t))
    (remove-hook 'kill-buffer-hook 'eureka--session-deactivate)
    (remove-hook 'after-save-hook 'eureka--save-session)
    (eureka--session-deactivate)))

(define-minor-mode eureka-request-mode
  "Minor mode for eureka buffers with active request to llm."
  :interactive nil
  :keymap '(([remap keyboard-quit] . eureka--cancel-current-request-and-quit))
  (if eureka-request-mode
      (add-hook 'kill-buffer-hook 'eureka--cancel-current-request nil t)
    (remove-hook 'kill-buffer-hook 'eureka--cancel-current-request)
    (eureka--cancel-current-request)))

(defvar-local eureka--change-group nil)

(defvar-local eureka--current-request nil)

(defcustom eureka-enable-keymap t
  "Enable or disable Eureka keymap."
  :type 'boolean
  :group 'eureka
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (if value
	     (eureka-setup-keymap)
	   ;; If eureka-enable-keymap is nil, remove the key bindings
	   (define-key global-map (kbd eureka-keymap-prefix) nil))))

;;;; Session

(defcustom eureka-sessions-directory (file-truename
				      (file-name-concat
				       user-emacs-directory
				       "eureka"
                                       "sessions"))
  "Directory for saved eureka sessions."
  :type 'string
  :group 'eureka)

(defvar-local eureka--current-session nil)

(defvar eureka--current-session-id nil)

(defvar eureka--active-sessions (make-hash-table :test #'equal))

(cl-defstruct eureka-session
  "A structure represent eureka session.

ID is an unique identifier of session, string.

PROVIDER is an llm provider of session.

FILE is a path to file contains string representation of this session, string.

PROMPT is a variable contains last prompt in this session.

RESPONSE is a variable contains last response in this session. "
  id provider file buffer prompt response)

(defun eureka-get-session-buffer (id)
  "Return eureka session buffer by provided ID."
  (gethash id eureka--active-sessions))

(defun eureka-generate-name-by-words (llm-name label)
  "Generate name by LABEL (project root) and LLM-NAME."
  (let* ((cleaned-label (replace-regexp-in-string "/" "_" label)))
    (concat
     (string-join
      (flatten-tree
       (list "*eureka-session"
             cleaned-label))
      "-")
     (format "(%s)" llm-name))))

(defun eureka-get-current-time ()
  "Return string representation of current time."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%TT%3N" (current-time))))

(defun eureka-generate-name-by-time (llm-name label)
  "Generate name for eureka session by current time."
  (let* ((cleaned-label (replace-regexp-in-string "/" "_" label)))
    (concat
     (string-join
      (list "*eureka-session"
            cleaned-label
            (eureka-get-current-time))
      "-")
     (format "(%s)" llm-name))))

(defun eureka-generate-name (llm-name label)
  "Generate name for eureka ACTION by PROVIDER according to PROMPT."
  (replace-regexp-in-string "/" "_" (funcall eureka-naming-scheme llm-name label)))

(defun eureka-generate-temp-name (llm-name)
  "Generate name for eureka ACTION by PROVIDER according to PROMPT."
  (replace-regexp-in-string "/" "_" (eureka-generate-name-by-time llm-name "temp")))

(defun eureka-get-session-file-extension ()
  "Return file extension based om the current mode.
Defaults to md, but supports org.  Depends on \"eureka-major-mode.\""
  "md")

(defun eureka-new-session (provider label &optional ephemeral)
  "Create new eureka session with unique id.
Provided PROVIDER and NAME will be used in new session.
If EPHEMERAL non nil new session will not be associated with any file."
  (let* ((name (eureka-generate-name (llm-name provider) label))
	 (count 1)
	 (name-with-suffix (format "%s %d" name count))
	 (id (if (not (eureka-get-session-buffer name))
		 name
	       (while (eureka-get-session-buffer name-with-suffix)
		 (setq count (+ count 1))
		 (setq name-with-suffix (format "%s %d" name count)))
	       name-with-suffix))
	 (file-name (when (not ephemeral)
		      (file-name-concat
		       eureka-sessions-directory
		       (concat id "." (eureka-get-session-file-extension)))))
         (buffer (if file-name
		     (progn
		       (make-directory eureka-sessions-directory t)
		       (find-file-noselect file-name))
		   (get-buffer-create id)))
	 (session (make-eureka-session
		   :id id :provider provider :file file-name
                   :buffer buffer)))
    (setq eureka--current-session-id id)
    (puthash id buffer eureka--active-sessions)
    (with-current-buffer buffer
      (setq eureka--current-session session)
      (eureka-session-mode +1))
    session))

(defun eureka--cancel-current-request ()
  "Cancel current running request."
  (when eureka--current-request
    (llm-cancel-request eureka--current-request)
    (setq eureka--current-request nil)))

(defun eureka--cancel-current-request-and-quit ()
  "Cancel the current request and quit."
  (interactive)
  (eureka--cancel-current-request)
  (keyboard-quit))

(defun eureka--session-deactivate ()
  "Deactivate current session."
  (eureka--cancel-current-request)
  (when-let* ((session eureka--current-session)
              (id (eureka-session-id session)))
    (when (string= (buffer-name)
                   (buffer-name (eureka-get-session-buffer id)))
      (remhash id eureka--active-sessions)
      (when (equal eureka--current-session-id id)
	(setq eureka--current-session-id nil)))))

(defun eureka--get-session-file-name (file-name)
  "Get eureka session file name for FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
	 (dir (file-name-directory file-name))
	 (session-file-name
	  (file-name-concat
	   dir
	   (concat "." base-name ".session.el"))))
    session-file-name))

(defun eureka--save-session ()
  "Save current eureka session."
  (when eureka--current-session
    (let* ((session eureka--current-session)
	   (file-name (eureka-session-file session))
	   (session-file-name (eureka--get-session-file-name file-name)))
      (with-temp-file session-file-name
	(insert (prin1-to-string session))))))

(defun eureka-model-chat (prompt &rest args)
  "Query eureka for PROMPT.
ARGS contains keys for fine control.

:streaming t or nil -- Streaming or not.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert eureka reply
in.

:point POINT -- POINT is the point in buffer to insert eureka reply at.

:filter FILTER -- FILTER is a function that's applied to (partial) response
strings before they're inserted into the BUFFER.

:session SESSION -- SESSION is a eureka conversation session.

:on-error ON-ERROR -- ON-ERROR a function that's called with an error message on
failure (with BUFFER current) and session-id.

:on-done ON-DONE -- ON-DONE a function that's called with
 the full response text when the request completes (with BUFFER current) and session id."
  (let* ((session (plist-get args :session))
         (session-id (when session
                       (eureka-session-id session)))
         (streaming (plist-get args :streaming))
         (provider (or (plist-get args :provider)
                       (if session
                           (eureka-session-provider session)
                         eureka-provider)))
	 (buffer (or (plist-get args :buffer)
		     (when session
		       (eureka-session-buffer session))))
	 (point (or (plist-get args :point)
		    (when buffer (with-current-buffer buffer (point-max)))))
	 (filter (or (plist-get args :filter) #'identity))
	 (errcb (or (plist-get args :on-error)
		    (lambda (msg)
		      (error "Error calling the LLM: %s" msg))))
	 (donecb (or (plist-get args :on-done) #'ignore))
         (llm-prompt (llm-make-simple-chat-prompt prompt))
         (invoke-buffer (current-buffer)))
    (with-current-buffer invoke-buffer
      (spinner-start eureka-spinner-type))
    (when session
      (setf (eureka-session-prompt session) prompt)
      (setf (eureka-session-response session) nil))
    (if buffer
        (with-current-buffer buffer
          (eureka-request-mode +1)
          (let* ((start (make-marker))
	         (end (make-marker))
	         (insert-text
	          (lambda (text)
		    ;; Erase and insert the new text between the marker cons.
		    (with-current-buffer buffer
                      ;; disable markdown-mode
                      (when (eq major-mode 'markdown-mode)
                        (fundamental-mode))
		      ;; Manually save/restore point as save-excursion doesn't
		      ;; restore the point into the middle of replaced text.
		      (let ((pt (point)))
		        (goto-char start)
		        (delete-region start end)
		        (insert (funcall filter text))
                        (when (pcase eureka-fill-paragraphs
                                ((cl-type function) (funcall eureka-fill-paragraphs))
                                ((cl-type boolean) eureka-fill-paragraphs)
                                ((cl-type list) (and (apply #'derived-mode-p
							    eureka-fill-paragraphs)
						     (not (equal major-mode 'org-mode)))))
                          (fill-region start (point)))
		        (goto-char pt))
                      ;; enable markdown-mode
                      (when (fboundp 'markdown-mode)
                        (markdown-mode))
                      (when-let ((eureka-auto-scroll)
			         (window (get-buffer-window buffer)))
                        (with-selected-window window
		          (goto-char (point-max))
		          (recenter -1)))
		      (undo-amalgamate-change-group eureka--change-group)))))
	    (setq eureka--change-group (prepare-change-group))
	    (activate-change-group eureka--change-group)
	    (set-marker start point)
	    (set-marker end point)
	    (set-marker-insertion-type start nil)
	    (set-marker-insertion-type end t)
	    (setq eureka--current-request
	          (llm-chat-streaming provider
				      llm-prompt
				      (when streaming insert-text)
				      (lambda (text)
				        (funcall insert-text (string-trim text))
                                        (with-current-buffer invoke-buffer
                                          (spinner-stop))
                                        (when session
                                          (setf (eureka-session-response session) text))
                                        (funcall donecb text session-id invoke-buffer)
				        (with-current-buffer buffer
				          (accept-change-group eureka--change-group)
                                          (when (and ellama-session-auto-save
                                                     (buffer-file-name buffer))
                                            (save-buffer))
				          (setq eureka--current-request nil)
				          (eureka-request-mode -1)))
	                              (lambda (_ msg)
		                        (with-current-buffer buffer
		                          (cancel-change-group eureka--change-group)
                                          (with-current-buffer invoke-buffer
		                            (spinner-stop))
		                          (funcall errcb msg)
		                          (setq eureka--current-request nil)
		                          (eureka-request-mode -1)))))))
      ;; no buffer to write llm response to
      (setq eureka--current-request
	    (llm-chat-streaming provider
				llm-prompt
                                #'ignore
				(lambda (text)
                                  (with-current-buffer invoke-buffer
                                    (spinner-stop))
                                  (when session
                                    (setf (eureka-session-response session) text))
                                  (funcall donecb text session-id invoke-buffer))
	                        (lambda (_ msg)
                                  (with-current-buffer invoke-buffer
		                    (spinner-stop))
		                  (funcall errcb msg)
		                  (setq eureka--current-request nil)
                                  (eureka-request-mode -1)))))))

(defun eureka-instant (prompt &rest args)
  "Prompt eureka for PROMPT to reply instantly.

ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (let* ((session (plist-get args :session))
         (provider (or (plist-get args :provider)
                       (when session
                         (eureka-session-provider session))
		       eureka-provider))
	 (buffer (or (when session
                       (eureka-session-buffer session))
                     (get-buffer-create (eureka-generate-temp-name (llm-name provider)))))
         (point (with-current-buffer buffer
                  (goto-char (point-max))
                  (point))))
    (display-buffer buffer (when eureka-instant-display-action-function
			     `((ignore . (,eureka-instant-display-action-function)))))
    (eureka-model-chat prompt
                       :session session
		       :buffer buffer
		       :provider provider
                       :point point)))

(provide 'eureka-core)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eureka-core.el ends here

