;;; apple-container.el --- Manage Apple's container runtime from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anton Hibl

;; Author: Anton Hibl <antonhibl11@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.4"))
;; Keywords: tools, processes, unix
;; URL: https://github.com/antonhibl/apple-container.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; A purpose-built Emacs front end for Apple's `container' CLI
;; (https://github.com/apple/container), the native Linux-container
;; runtime for Apple silicon Macs.
;;
;; It talks to the `container' binary directly, using its
;; `--format json' output, so no Docker compatibility layer is needed.
;;
;; Entry points:
;;
;;   M-x apple-container             transient menu for everything below
;;   M-x apple-container-containers  list containers
;;   M-x apple-container-images      list images
;;   M-x apple-container-volumes     list volumes
;;   M-x apple-container-networks    list networks
;;
;; In any list buffer, press `?' for the actions available there.
;; Mark several rows with `m' to act on all of them at once.
;;
;; TRAMP: files inside a running container are reachable as
;;
;;   /container:CONTAINER-ID:/path/to/file
;;
;; with completion of running container IDs.  The method name is
;; controlled by `apple-container-tramp-method'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'compile)
(require 'tabulated-list)
(require 'transient)
(require 'ansi-color)
(require 'iso8601)

(declare-function tramp-set-completion-function "tramp")
(declare-function json-pretty-print-buffer "json")
(declare-function eat "ext:eat")
(declare-function portal-terminal-start "ext:portal-terminal")
(declare-function vterm "ext:vterm")
(declare-function term-mode "term")
(declare-function term-char-mode "term")
(declare-function json-ts-mode "json-ts-mode")
(declare-function js-json-mode "js")
(declare-function treesit-ready-p "treesit.c")
(defvar tramp-methods)
(defvar tramp-default-remote-shell)
(defvar vterm-shell)
(defvar vterm-kill-buffer-on-exit)
(defvar eat-buffer-name)

;;;; Customization

(defgroup apple-container nil
  "Manage Apple's container runtime."
  :group 'tools
  :prefix "apple-container-")

(defcustom apple-container-program nil
  "Path to the `container' executable.
When nil, look it up in variable `exec-path', then fall back to
/usr/local/bin/container, which is where Apple's installer puts it.
The fallback matters for GUI Emacs on macOS, whose PATH often lacks
/usr/local/bin."
  :type '(choice (const :tag "Find automatically" nil) file))

(defcustom apple-container-list-all t
  "When non-nil, container lists include stopped containers."
  :type 'boolean)

(defcustom apple-container-terminal-backend 'auto
  "Terminal used for interactive shells and attached runs.
`auto' picks Portal's native Ghostty terminal in graphical macOS
Emacs when Portal is installed, then eat, then vterm, then the
built-in term."
  :type '(choice (const :tag "Pick automatically" auto)
                 (const :tag "Portal (Ghostty)" portal)
                 (const :tag "eat" eat)
                 (const :tag "vterm" vterm)
                 (const :tag "Built-in term" term)))

(defcustom apple-container-shell-command
  '("/bin/sh" "-c"
    "if command -v bash >/dev/null 2>&1; then exec bash -l; else exec sh -l; fi")
  "Command run by `apple-container-shell' inside the container.
The default prefers bash and falls back to sh."
  :type '(repeat string))

(defcustom apple-container-logs-tail 500
  "Number of trailing log lines to fetch, or nil for the whole log."
  :type '(choice (const :tag "All" nil) natnum))

(defcustom apple-container-tramp-method "container"
  "TRAMP method name for files inside containers.
Files are then reachable as /METHOD:CONTAINER-ID:/path.  Set this
before TRAMP loads."
  :type 'string)

(defcustom apple-container-run-default-args "-d"
  "Initial arguments offered by `apple-container-run'."
  :type 'string)

(defface apple-container-running '((t :inherit success))
  "Face for running containers.")

(defface apple-container-stopped '((t :inherit shadow))
  "Face for stopped containers.")

(defface apple-container-transitioning '((t :inherit warning))
  "Face for containers that are starting or stopping.")

;;;; Running the CLI

(defun apple-container--program ()
  "Return the path of the `container' executable, or signal an error."
  (or apple-container-program
      (executable-find "container")
      (and (file-executable-p "/usr/local/bin/container")
           "/usr/local/bin/container")
      (user-error "Can't find the `container' executable; \
set `apple-container-program'")))

(defun apple-container--local-dir ()
  "Return a local directory to run processes from.
Processes started from a TRAMP buffer would otherwise try to use a
remote `default-directory'."
  (if (file-remote-p default-directory)
      (expand-file-name "~/")
    default-directory))

(defun apple-container--parse-json (string)
  "Parse JSON STRING into alists and lists."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun apple-container--explain-error (stderr)
  "Turn STDERR from the CLI into a short, helpful message."
  (let ((msg (string-trim (or stderr ""))))
    (when (string-empty-p msg) (setq msg "command failed"))
    (setq msg (car (last (split-string msg "\n" t))))
    (if (string-match-p
         "XPC\\|apiserver\\|not running\\|Connection invalid\\|interrupted"
         msg)
        (format "%s (is the service running? try M-x apple-container-system-start)"
                msg)
      msg)))

(defun apple-container--call (args)
  "Run the CLI synchronously with ARGS.
Return stdout as a string, or signal a `user-error' with stderr."
  (let ((default-directory (apple-container--local-dir))
        (err-file (make-temp-file "apple-container-err")))
    (unwind-protect
        (with-temp-buffer
          (let ((code (apply #'call-process (apple-container--program) nil
                             (list t err-file) nil args)))
            (if (eq code 0)
                (buffer-string)
              (user-error "`container %s' failed: %s" (car args)
                          (apple-container--explain-error
                           (with-temp-buffer
                             (insert-file-contents err-file)
                             (buffer-string)))))))
      (delete-file err-file))))

(defun apple-container--call-json (args)
  "Run the CLI synchronously with ARGS and parse its JSON output."
  (let ((out (string-trim (apple-container--call args))))
    (if (string-empty-p out) nil (apple-container--parse-json out))))

(defun apple-container--call-async (args on-success &optional on-error)
  "Run the CLI asynchronously with ARGS.
Call ON-SUCCESS with stdout when it exits with status 0.  Otherwise
call ON-ERROR with stderr, or report the error in the echo area."
  (let* ((default-directory (apple-container--local-dir))
         (out (generate-new-buffer " *apple-container-out*"))
         (err (generate-new-buffer " *apple-container-err*"))
         (err-proc (make-pipe-process :name "apple-container-stderr"
                                      :buffer err :noquery t
                                      :sentinel #'ignore)))
    (make-process
     :name "apple-container"
     :buffer out
     :stderr err-proc
     :noquery t
     :connection-type 'pipe
     :command (cons (apple-container--program) args)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         ;; Let any trailing stderr arrive before reading it.
         (accept-process-output err-proc 0.05)
         (let ((code (process-exit-status proc))
               (stdout (with-current-buffer out (buffer-string)))
               (stderr (with-current-buffer err (buffer-string))))
           (delete-process err-proc)
           (kill-buffer out)
           (kill-buffer err)
           (if (eq code 0)
               (funcall on-success stdout)
             (if on-error
                 (funcall on-error stderr)
               (message "container %s: %s" (car args)
                        (apple-container--explain-error stderr))))))))))

(defun apple-container--action (args done-message)
  "Run ARGS asynchronously, then show DONE-MESSAGE and refresh lists."
  (message "Running container %s..." (string-join args " "))
  (apple-container--call-async
   args
   (lambda (_)
     (message "%s" done-message)
     (apple-container-refresh-all))))

(defun apple-container--command-string (args)
  "Return a shell command string running the CLI with ARGS."
  (mapconcat #'shell-quote-argument
             (cons (apple-container--program) args) " "))

(defun apple-container--run-in-buffer (args &optional directory)
  "Run the CLI with ARGS in an interactive compilation buffer.
The buffer accepts input, so prompts from the CLI can be answered.
Run from DIRECTORY when given.  Lists refresh when it finishes."
  (let* ((default-directory (or directory (apple-container--local-dir)))
         (name (format "*container %s*" (string-join (seq-take args 2) " ")))
         (buf (compilation-start (apple-container--command-string args) t
                                 (lambda (_mode) name))))
    (with-current-buffer buf
      (add-hook 'compilation-finish-functions
                (lambda (_buf _status) (apple-container-refresh-all))
                nil t))
    buf))

(defun apple-container--terminal (name args)
  "Run the CLI with ARGS in a terminal buffer called NAME."
  (let* ((program (apple-container--program))
         (default-directory (apple-container--local-dir))
         (backend (if (eq apple-container-terminal-backend 'auto)
                      (cond ((and (eq window-system 'ns)
                                  (locate-library "portal-terminal"))
                             'portal)
                            ((locate-library "eat") 'eat)
                            ((locate-library "vterm") 'vterm)
                            (t 'term))
                    apple-container-terminal-backend)))
    (pcase backend
      ('portal
       (require 'portal-terminal)
       ;; Portal needs an existing local directory and plain strings; it
       ;; runs PROGRAM through the login shell, so a full path is safest.
       (portal-terminal-start (expand-file-name "~") program args name))
      ('eat
       (require 'eat)
       (let ((eat-buffer-name name))
         (eat (apple-container--command-string args) t)))
      ('vterm
       (require 'vterm)
       (let ((vterm-shell (apple-container--command-string args))
             (vterm-kill-buffer-on-exit nil))
         (vterm (generate-new-buffer-name name))))
      (_
       (require 'term)
       (let ((buf (apply #'make-term
                         (string-trim name "\\*" "\\*") program nil args)))
         (with-current-buffer buf
           (term-mode)
           (term-char-mode))
         (pop-to-buffer buf))))))

;;;; Reading JSON fields

(defun apple-container--get (obj &rest keys)
  "Follow KEYS (symbols) through nested alists starting at OBJ."
  (while (and keys obj (listp obj))
    (setq obj (alist-get (pop keys) obj)))
  (if keys nil obj))

(defun apple-container--config (obj)
  "Return the configuration part of OBJ.
Newer CLI versions wrap fields in `configuration'; older ones don't."
  (or (alist-get 'configuration obj) obj))

(defconst apple-container--swift-epoch 978307200
  "Seconds between the Unix epoch and 2001-01-01, Swift's reference date.")

(defun apple-container--time (value)
  "Convert VALUE, an ISO 8601 string or Swift date number, to a Lisp time."
  (condition-case nil
      (cond ((stringp value) (encode-time (iso8601-parse value)))
            ((numberp value) (+ value apple-container--swift-epoch)))
    (error nil)))

(defun apple-container--format-time (value)
  "Format VALUE (see `apple-container--time') for display."
  (if-let* ((time (apple-container--time value)))
      (format-time-string "%Y-%m-%d %H:%M" time)
    ""))

(defun apple-container--bytes (n)
  "Format N bytes for display, keeping N for numeric sorting."
  (if (numberp n)
      (propertize (file-size-human-readable n 'iec " ")
                  'apple-container-bytes n)
    ""))

(defun apple-container--sort-bytes (column)
  "Return a predicate sorting tabulated entries by byte COLUMN."
  (lambda (a b)
    (< (or (get-text-property 0 'apple-container-bytes (aref (cadr a) column)) -1)
       (or (get-text-property 0 'apple-container-bytes (aref (cadr b) column)) -1))))

(defun apple-container--short-ref (ref)
  "Shorten image REF the way the CLI's table output does."
  (let ((ref (or ref "")))
    (dolist (prefix '("docker.io/library/" "docker.io/") ref)
      (when (string-prefix-p prefix ref)
        (setq ref (substring ref (length prefix)))))))

(defun apple-container--container-id (c)
  "Return the ID of container C."
  (or (alist-get 'id c) (apple-container--get c 'configuration 'id)))

(defun apple-container--container-state (c)
  "Return the state of container C as a string."
  (let ((status (alist-get 'status c)))
    (or (if (stringp status) status (alist-get 'state status)) "unknown")))

(defun apple-container--container-networks (c)
  "Return the network attachments of container C."
  (let ((status (alist-get 'status c)))
    (or (and (consp status) (alist-get 'networks status))
        (alist-get 'networks c))))

(defun apple-container--container-started (c)
  "Return the start date of container C."
  (let ((status (alist-get 'status c)))
    (or (and (consp status) (alist-get 'startedDate status))
        (alist-get 'startedDate c))))

(defun apple-container--format-ports (ports)
  "Format published PORTS as HOST->CONTAINER/PROTO."
  (mapconcat
   (lambda (p)
     (let* ((host (alist-get 'hostPort p))
            (ctr (alist-get 'containerPort p))
            (count (or (alist-get 'count p) 1))
            (proto (or (alist-get 'proto p) "tcp")))
       (if (and (numberp count) (> count 1))
           (format "%s-%s->%s-%s/%s" host (+ host count -1)
                   ctr (+ ctr count -1) proto)
         (format "%s->%s/%s" host ctr proto))))
   ports ","))

(defun apple-container--state-face (state)
  "Return the face for container STATE."
  (pcase state
    ("running" 'apple-container-running)
    ("stopped" 'apple-container-stopped)
    (_ 'apple-container-transitioning)))

;;;; Generic list mode

(defvar-local apple-container--list-args nil
  "CLI arguments producing this buffer's JSON listing.")

(defvar-local apple-container--row-function nil
  "Function turning one JSON object into a tabulated-list entry.")

(defvar-local apple-container--loading nil
  "Non-nil while this buffer is waiting for the CLI.")

(defvar apple-container-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'apple-container-refresh)
    (define-key map "m" #'apple-container-mark)
    (define-key map "u" #'apple-container-unmark)
    (define-key map "U" #'apple-container-unmark-all)
    (define-key map "t" #'apple-container-toggle-marks)
    map)
  "Keymap shared by all container list buffers.")

(define-derived-mode apple-container-list-mode tabulated-list-mode
  "Container List"
  "Base mode for Apple container list buffers."
  (setq tabulated-list-padding 2)
  (setq-local revert-buffer-function
              (lambda (&rest _) (apple-container-refresh)))
  (setq mode-line-process '(apple-container--loading ":loading")))

(defun apple-container--marked-ids ()
  "Return the IDs of marked rows, top to bottom."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?*)
          (push (tabulated-list-get-id) ids))
        (forward-line 1)))
    (nreverse ids)))

(defun apple-container--targets ()
  "Return marked IDs, or the ID at point."
  (or (apple-container--marked-ids)
      (if-let* ((id (tabulated-list-get-id)))
          (list id)
        (user-error "No item on this line"))))

(defun apple-container--describe-targets (ids)
  "Describe IDS for a confirmation prompt."
  (if (cdr ids) (format "%d items" (length ids)) (car ids)))

(defun apple-container-mark ()
  "Mark the item on this line."
  (interactive)
  (when (tabulated-list-get-id)
    (tabulated-list-put-tag "*" t)))

(defun apple-container-unmark ()
  "Unmark the item on this line."
  (interactive)
  (when (tabulated-list-get-id)
    (tabulated-list-put-tag " " t)))

(defun apple-container-unmark-all ()
  "Unmark all items."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id) (tabulated-list-put-tag " "))
      (forward-line 1))))

(defun apple-container-toggle-marks ()
  "Toggle the mark on every item."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id)
        (tabulated-list-put-tag (if (eq (char-after) ?*) " " "*")))
      (forward-line 1))))

(defun apple-container--populate (buffer items)
  "Fill list BUFFER with ITEMS, keeping point and marks."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((marked (apple-container--marked-ids)))
        (setq tabulated-list-entries
              (delq nil (mapcar apple-container--row-function items)))
        (tabulated-list-print t)
        (when marked
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (member (tabulated-list-get-id) marked)
                (tabulated-list-put-tag "*"))
              (forward-line 1))))))))

(defun apple-container-refresh ()
  "Reload the current list buffer from the CLI."
  (interactive)
  (let ((buffer (current-buffer)))
    (setq apple-container--loading t)
    (force-mode-line-update)
    (apple-container--call-async
     apple-container--list-args
     (lambda (stdout)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq apple-container--loading nil)
           (force-mode-line-update))
         (let ((out (string-trim stdout)))
           (apple-container--populate
            buffer (if (string-empty-p out) nil
                     (apple-container--parse-json out))))))
     (lambda (stderr)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq apple-container--loading nil)
           (force-mode-line-update)))
       (message "container: %s" (apple-container--explain-error stderr))))))

(defun apple-container-refresh-all ()
  "Reload every visible container list buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'apple-container-list-mode)
                 (get-buffer-window buf t))
        (apple-container-refresh)))))

(defun apple-container--show-list (buffer-name mode)
  "Show list BUFFER-NAME in MODE and refresh it."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p mode) (funcall mode))
      (apple-container-refresh))
    (pop-to-buffer-same-window buf)))

;;;; Readers used outside list buffers

(defun apple-container--container-ids (&optional all)
  "Return container IDs; running ones only unless ALL."
  (mapcar #'apple-container--container-id
          (apple-container--call-json
           (append '("list" "--format" "json") (and all '("--all"))))))

(defun apple-container--image-names ()
  "Return the references of local images."
  (mapcar (lambda (i)
            (apple-container--short-ref
             (or (apple-container--get i 'configuration 'name)
                 (alist-get 'reference i) (alist-get 'name i))))
          (apple-container--call-json '("image" "list" "--format" "json"))))

(defun apple-container--read-container (prompt &optional all)
  "Read a container ID with PROMPT; offer stopped ones too when ALL.
Inside the containers list, use the container at point."
  (or (and (derived-mode-p 'apple-container-containers-mode)
           (tabulated-list-get-id))
      (completing-read prompt (apple-container--container-ids all) nil t)))

(defun apple-container--read-image (prompt)
  "Read an image reference with PROMPT."
  (or (and (derived-mode-p 'apple-container-images-mode)
           (tabulated-list-get-id))
      (completing-read prompt (ignore-errors (apple-container--image-names)))))

;;;; Shared viewers: inspect and logs

(defun apple-container--show-json (name args)
  "Run ARGS and show the pretty-printed JSON in buffer NAME."
  (apple-container--call-async
   args
   (lambda (stdout)
     (let ((buf (get-buffer-create name)))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert stdout)
           (require 'json)
           (ignore-errors (json-pretty-print-buffer))
           (goto-char (point-min)))
         (cond ((and (fboundp 'treesit-ready-p) (treesit-ready-p 'json t))
                (json-ts-mode))
               ((fboundp 'js-json-mode) (js-json-mode))
               (t (js-mode)))
         (view-mode 1))
       (pop-to-buffer buf)))))

(defvar-local apple-container--logs-args nil
  "CLI arguments that produced this logs buffer.")

(defvar apple-container-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'apple-container-logs-reload)
    (define-key map "f" #'apple-container-logs-toggle-follow)
    map)
  "Keymap for `apple-container-logs-mode'.")

(define-derived-mode apple-container-logs-mode special-mode "Container Logs"
  "Mode for container log output.
\\{apple-container-logs-mode-map}"
  (setq-local buffer-read-only t))

(defun apple-container--logs-filter (proc string)
  "Insert STRING from PROC, following the end when point is there."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((inhibit-read-only t)
             (at-end (= (point) (process-mark proc)))
             (windows (seq-filter (lambda (w) (= (window-point w)
                                                 (process-mark proc)))
                                  (get-buffer-window-list nil nil t))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (ansi-color-apply string))
          (set-marker (process-mark proc) (point)))
        (when at-end (goto-char (process-mark proc)))
        (dolist (w windows) (set-window-point w (process-mark proc)))))))

(defun apple-container--logs (name args)
  "Stream the output of ARGS into logs buffer NAME."
  (let ((buf (get-buffer-create name))
        (default-directory (apple-container--local-dir)))
    (with-current-buffer buf
      (when-let* ((old (get-buffer-process buf))) (delete-process old))
      (apple-container-logs-mode)
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq apple-container--logs-args args)
      (setq mode-line-process
            (if (or (member "--follow" args) (member "-f" args))
                ":following" nil))
      (make-process
       :name "apple-container-logs"
       :buffer buf
       :command (cons (apple-container--program) args)
       :connection-type 'pipe
       :noquery t
       :filter #'apple-container--logs-filter
       :sentinel (lambda (proc _event)
                   (when (and (memq (process-status proc) '(exit signal))
                              (buffer-live-p (process-buffer proc)))
                     (with-current-buffer (process-buffer proc)
                       (setq mode-line-process nil)
                       (force-mode-line-update))))))
    (pop-to-buffer buf)))

(defun apple-container-logs-reload ()
  "Fetch the logs in this buffer again."
  (interactive)
  (apple-container--logs (buffer-name) apple-container--logs-args))

(defun apple-container-logs-toggle-follow ()
  "Toggle following new log output in this buffer."
  (interactive)
  (let ((args (if (member "--follow" apple-container--logs-args)
                  (remove "--follow" apple-container--logs-args)
                (append (butlast apple-container--logs-args)
                        (list "--follow")
                        (last apple-container--logs-args)))))
    (apple-container--logs (buffer-name) args)))

;;;; Containers

(defun apple-container--container-row (c)
  "Turn container C into a tabulated-list entry."
  (let* ((cfg (alist-get 'configuration c))
         (id (apple-container--container-id c))
         (state (apple-container--container-state c))
         (res (alist-get 'resources cfg))
         (ips (mapconcat (lambda (n)
                           (car (split-string
                                 (format "%s" (or (alist-get 'ipv4Address n) ""))
                                 "/")))
                         (apple-container--container-networks c) ",")))
    (when id
      (list id
            (vector
             id
             (apple-container--short-ref
              (apple-container--get cfg 'image 'reference))
             (propertize state 'face (apple-container--state-face state))
             ips
             (apple-container--format-ports (alist-get 'publishedPorts cfg))
             (format "%s" (or (alist-get 'cpus res) ""))
             (apple-container--bytes (alist-get 'memoryInBytes res))
             (apple-container--format-time
              (apple-container--container-started c)))))))

(defvar apple-container-containers-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map apple-container-list-mode-map)
    (define-key map "?" #'apple-container-containers-help)
    (define-key map (kbd "RET") #'apple-container-inspect)
    (define-key map "a" #'apple-container-toggle-all)
    (define-key map "s" #'apple-container-start)
    (define-key map "S" #'apple-container-stop)
    (define-key map "R" #'apple-container-restart)
    (define-key map "K" #'apple-container-kill)
    (define-key map "D" #'apple-container-delete)
    (define-key map "l" #'apple-container-logs)
    (define-key map "L" #'apple-container-logs-follow)
    (define-key map "b" #'apple-container-shell)
    (define-key map "e" #'apple-container-exec)
    (define-key map "f" #'apple-container-dired)
    (define-key map "c" #'apple-container-copy-to)
    (define-key map "C" #'apple-container-copy-from)
    (define-key map "x" #'apple-container-prune)
    map)
  "Keymap for `apple-container-containers-mode'.")

(define-derived-mode apple-container-containers-mode apple-container-list-mode
  "Containers"
  "Major mode listing Apple containers.
\\{apple-container-containers-mode-map}"
  (setq tabulated-list-format
        `[("ID" 24 t)
          ("Image" 30 t)
          ("State" 9 t)
          ("IP" 15 t)
          ("Ports" 18 t)
          ("CPUs" 5 t :right-align t)
          ("Memory" 10 ,(apple-container--sort-bytes 6) :right-align t)
          ("Started" 16 t)])
  (setq tabulated-list-sort-key '("ID" . nil))
  (setq apple-container--row-function #'apple-container--container-row)
  (setq apple-container--list-args
        (append '("list" "--format" "json")
                (and apple-container-list-all '("--all"))))
  (tabulated-list-init-header))

;;;###autoload
(defun apple-container-containers ()
  "List Apple containers."
  (interactive)
  (apple-container--show-list "*containers*" #'apple-container-containers-mode))

(defun apple-container-toggle-all ()
  "Toggle showing stopped containers in this list."
  (interactive)
  (setq apple-container--list-args
        (if (member "--all" apple-container--list-args)
            (remove "--all" apple-container--list-args)
          (append apple-container--list-args '("--all"))))
  (message "Showing %s containers"
           (if (member "--all" apple-container--list-args) "all" "running"))
  (apple-container-refresh))

(defun apple-container--container-targets (&optional all)
  "Return target containers: marked rows, row at point, or prompt.
Offer stopped containers in the prompt when ALL."
  (if (derived-mode-p 'apple-container-containers-mode)
      (apple-container--targets)
    (list (apple-container--read-container "Container: " all))))

(defun apple-container-start (ids)
  "Start containers IDS."
  (interactive (list (apple-container--container-targets t)))
  (apple-container--action (cons "start" ids)
                           (format "Started %s" (string-join ids ", "))))

(defun apple-container-stop (ids)
  "Stop containers IDS gracefully."
  (interactive (list (apple-container--container-targets)))
  (apple-container--action (cons "stop" ids)
                           (format "Stopped %s" (string-join ids ", "))))

(defun apple-container-restart (ids)
  "Stop and then start containers IDS."
  (interactive (list (apple-container--container-targets)))
  (message "Restarting %s..." (string-join ids ", "))
  (apple-container--call-async
   (cons "stop" ids)
   (lambda (_)
     (apple-container--action (cons "start" ids)
                              (format "Restarted %s" (string-join ids ", "))))))

(defun apple-container-kill (ids &optional signal)
  "Send SIGNAL (default KILL) to containers IDS.
With a prefix argument, prompt for the signal."
  (interactive (list (apple-container--container-targets)
                     (and current-prefix-arg
                          (read-string "Signal: " "TERM"))))
  (when (yes-or-no-p (format "Kill %s? " (apple-container--describe-targets ids)))
    (apple-container--action
     (append '("kill") (and signal (list "--signal" signal)) ids)
     (format "Signalled %s" (string-join ids ", ")))))

(defun apple-container-delete (ids &optional force)
  "Delete containers IDS.  With prefix argument FORCE, delete running ones."
  (interactive (list (apple-container--container-targets t) current-prefix-arg))
  (when (yes-or-no-p (format "Delete %s%s? "
                             (apple-container--describe-targets ids)
                             (if force " (forced)" "")))
    (apple-container--action
     (append '("delete") (and force '("--force")) ids)
     (format "Deleted %s" (string-join ids ", ")))))

(defun apple-container-prune ()
  "Remove all stopped containers."
  (interactive)
  (when (yes-or-no-p "Remove all stopped containers? ")
    (apple-container--action '("prune") "Pruned stopped containers")))

;;;###autoload
(defun apple-container-inspect (id)
  "Show the JSON description of container ID."
  (interactive (list (apple-container--read-container "Inspect container: " t)))
  (apple-container--show-json (format "*container inspect: %s*" id)
                              (list "inspect" id)))

;;;###autoload
(defun apple-container-logs (id &optional boot)
  "Show the logs of container ID.
With prefix argument BOOT, show the VM boot log instead."
  (interactive (list (apple-container--read-container "Logs for: " t)
                     current-prefix-arg))
  (apple-container--logs
   (format "*container logs: %s*" id)
   (append '("logs")
           (and boot '("--boot"))
           (and apple-container-logs-tail
                (list "-n" (number-to-string apple-container-logs-tail)))
           (list id))))

(defun apple-container-logs-follow (id)
  "Follow the logs of container ID."
  (interactive (list (apple-container--read-container "Follow logs for: ")))
  (apple-container--logs
   (format "*container logs: %s*" id)
   (append '("logs")
           (and apple-container-logs-tail
                (list "-n" (number-to-string apple-container-logs-tail)))
           (list "--follow" id))))

;;;###autoload
(defun apple-container-shell (id &optional new)
  "Open an interactive shell in running container ID.
Switch to the existing shell buffer for ID if there is one; with
prefix argument NEW, always start another.  See
`apple-container-shell-command' and `apple-container-terminal-backend'."
  (interactive (list (apple-container--read-container "Shell in: ")
                     current-prefix-arg))
  (let ((name (format "*container shell: %s*" id)))
    (if (and (not new) (get-buffer name))
        (pop-to-buffer name)
      (apple-container--terminal
       name (append (list "exec" "-it" id) apple-container-shell-command)))))

;;;###autoload
(defun apple-container-exec (id command)
  "Run shell COMMAND inside container ID and show its output."
  (interactive
   (let ((id (apple-container--read-container "Exec in: ")))
     (list id (read-shell-command (format "Command in %s: " id)))))
  (apple-container--run-in-buffer (list "exec" id "/bin/sh" "-c" command)))

;;;###autoload
(defun apple-container-dired (id)
  "Open Dired at the root of container ID, over TRAMP."
  (interactive (list (apple-container--read-container "Browse files in: ")))
  (require 'tramp)
  (dired (format "/%s:%s:/" apple-container-tramp-method id)))

(defun apple-container-copy-to (id source destination)
  "Copy local SOURCE into container ID at DESTINATION."
  (interactive
   (let ((id (apple-container--read-container "Copy into: ")))
     (list id
           (read-file-name "Local file: " nil nil t)
           (read-string (format "Destination in %s: " id) "/tmp/"))))
  (apple-container--action
   (list "copy" (expand-file-name source) (format "%s:%s" id destination))
   (format "Copied %s to %s:%s" source id destination)))

(defun apple-container-copy-from (id source destination)
  "Copy SOURCE from container ID to local DESTINATION."
  (interactive
   (let ((id (apple-container--read-container "Copy from: ")))
     (list id
           (read-string (format "Path in %s: " id))
           (read-file-name "Local destination: "))))
  (apple-container--action
   (list "copy" (format "%s:%s" id source) (expand-file-name destination))
   (format "Copied %s:%s to %s" id source destination)))

;;;###autoload
(defun apple-container-run (image args &optional command)
  "Run IMAGE with extra CLI ARGS and optional COMMAND.
Detached runs (-d) report in a compilation buffer; anything else
opens in a terminal so you can interact with it."
  (interactive
   (let ((image (apple-container--read-image "Run image: ")))
     (list image
           (read-string (format "Options for %s: " image)
                        apple-container-run-default-args)
           (read-string "Command (empty for image default): "))))
  (let* ((opts (split-string-shell-command args))
         (cmd (and command (not (string-empty-p command))
                   (split-string-shell-command command)))
         (full (append '("run") opts (list image) cmd)))
    (if (or (member "-d" opts) (member "--detach" opts))
        (apple-container--run-in-buffer full)
      (apple-container--terminal (format "*container run: %s*" image) full))))

;;;; Images

(defun apple-container--image-size (i)
  "Return the size in bytes of image I for the host platform."
  (let* ((variants (alist-get 'variants i))
         (native (seq-find (lambda (v)
                             (equal (apple-container--get v 'platform 'architecture)
                                    "arm64"))
                           variants)))
    (or (alist-get 'size (or native (car variants)))
        (apple-container--get i 'configuration 'descriptor 'size)
        (apple-container--get i 'descriptor 'size))))

(defun apple-container--image-row (i)
  "Turn image I into a tabulated-list entry."
  (let* ((cfg (apple-container--config i))
         (full (or (alist-get 'name cfg) (alist-get 'reference i)))
         (ref (apple-container--short-ref full))
         (digest (or (apple-container--get cfg 'descriptor 'digest) ""))
         (tag-pos (and (not (string-match-p "@" ref))
                       (string-match ":\\([^:/]+\\)\\'" ref)))
         (name (if tag-pos (substring ref 0 tag-pos) ref))
         (tag (if tag-pos (match-string 1 ref) "<none>")))
    (when full
      (list ref
            (vector name tag
                    (let ((d (replace-regexp-in-string "\\`sha256:" "" digest)))
                      (substring d 0 (min 12 (length d))))
                    (apple-container--bytes (apple-container--image-size i))
                    (apple-container--format-time
                     (alist-get 'creationDate cfg)))))))

(defvar apple-container-images-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map apple-container-list-mode-map)
    (define-key map "?" #'apple-container-images-help)
    (define-key map (kbd "RET") #'apple-container-image-inspect)
    (define-key map "r" #'apple-container-run)
    (define-key map "p" #'apple-container-pull)
    (define-key map "P" #'apple-container-image-push)
    (define-key map "T" #'apple-container-image-tag)
    (define-key map "D" #'apple-container-image-delete)
    (define-key map "x" #'apple-container-image-prune)
    map)
  "Keymap for `apple-container-images-mode'.")

(define-derived-mode apple-container-images-mode apple-container-list-mode
  "Container Images"
  "Major mode listing images in Apple's container image store.
\\{apple-container-images-mode-map}"
  (setq tabulated-list-format
        `[("Name" 40 t)
          ("Tag" 16 t)
          ("Digest" 13 t)
          ("Size" 10 ,(apple-container--sort-bytes 3) :right-align t)
          ("Created" 16 t)])
  (setq tabulated-list-sort-key '("Name" . nil))
  (setq apple-container--row-function #'apple-container--image-row)
  (setq apple-container--list-args '("image" "list" "--format" "json"))
  (tabulated-list-init-header))

;;;###autoload
(defun apple-container-images ()
  "List images in Apple's container image store."
  (interactive)
  (apple-container--show-list "*container images*"
                              #'apple-container-images-mode))

(defun apple-container--image-targets ()
  "Return target images: marked rows, row at point, or prompt."
  (if (derived-mode-p 'apple-container-images-mode)
      (apple-container--targets)
    (list (apple-container--read-image "Image: "))))

(defun apple-container-image-inspect (ref)
  "Show the JSON description of image REF."
  (interactive (list (apple-container--read-image "Inspect image: ")))
  (apple-container--show-json (format "*container image: %s*" ref)
                              (list "image" "inspect" ref)))

;;;###autoload
(defun apple-container-pull (ref)
  "Pull image REF."
  (interactive (list (read-string "Pull image: ")))
  (apple-container--run-in-buffer (list "image" "pull" "--progress" "plain" ref)))

(defun apple-container-image-push (ref)
  "Push image REF to its registry."
  (interactive (list (apple-container--read-image "Push image: ")))
  (apple-container--run-in-buffer (list "image" "push" "--progress" "plain" ref)))

(defun apple-container-image-tag (source target)
  "Tag image SOURCE as TARGET."
  (interactive
   (let ((src (apple-container--read-image "Tag image: ")))
     (list src (read-string (format "New tag for %s: " src) src))))
  (apple-container--action (list "image" "tag" source target)
                           (format "Tagged %s as %s" source target)))

(defun apple-container-image-delete (refs)
  "Delete images REFS."
  (interactive (list (apple-container--image-targets)))
  (when (yes-or-no-p (format "Delete image %s? "
                             (apple-container--describe-targets refs)))
    (apple-container--action (append '("image" "delete") refs)
                             (format "Deleted %s" (string-join refs ", ")))))

(defun apple-container-image-prune (&optional all)
  "Remove dangling images.  With prefix argument ALL, remove all unused."
  (interactive "P")
  (when (yes-or-no-p (if all "Remove ALL unused images? "
                       "Remove dangling images? "))
    (apple-container--action (append '("image" "prune") (and all '("--all")))
                             "Pruned images")))

;;;###autoload
(defun apple-container-build (directory tag &optional file)
  "Build an image from DIRECTORY tagged TAG, using Dockerfile FILE."
  (interactive
   (let ((dir (read-directory-name "Build context: " nil nil t)))
     (list dir
           (read-string "Tag: " (concat (file-name-nondirectory
                                         (directory-file-name dir))
                                        ":latest"))
           (when current-prefix-arg
             (read-file-name "Dockerfile: " dir nil t)))))
  (when (file-remote-p directory)
    (user-error "The build context must be a local directory"))
  (apple-container--run-in-buffer
   (append '("build" "--progress" "plain" "-t") (list tag)
           (and file (list "-f" (expand-file-name file)))
           (list (expand-file-name directory)))
   directory))

;;;; Volumes

(defun apple-container--volume-row (v)
  "Turn volume V into a tabulated-list entry."
  (let* ((cfg (apple-container--config v))
         (name (or (alist-get 'name cfg) (alist-get 'id v)))
         (anonymous (assoc "com.apple.container.resource.anonymous"
                           (mapcar (lambda (p) (cons (symbol-name (car p)) (cdr p)))
                                   (alist-get 'labels cfg)))))
    (when name
      (list name
            (vector name
                    (if anonymous "anonymous" "named")
                    (or (alist-get 'driver cfg) "")
                    (apple-container--bytes (alist-get 'sizeInBytes cfg))
                    (apple-container--format-time (alist-get 'creationDate cfg)))))))

(defvar apple-container-volumes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map apple-container-list-mode-map)
    (define-key map "?" #'apple-container-volumes-help)
    (define-key map (kbd "RET") #'apple-container-volume-inspect)
    (define-key map "+" #'apple-container-volume-create)
    (define-key map "D" #'apple-container-volume-delete)
    (define-key map "x" #'apple-container-volume-prune)
    map)
  "Keymap for `apple-container-volumes-mode'.")

(define-derived-mode apple-container-volumes-mode apple-container-list-mode
  "Container Volumes"
  "Major mode listing Apple container volumes.
\\{apple-container-volumes-mode-map}"
  (setq tabulated-list-format
        `[("Name" 40 t)
          ("Type" 10 t)
          ("Driver" 8 t)
          ("Size" 10 ,(apple-container--sort-bytes 3) :right-align t)
          ("Created" 16 t)])
  (setq tabulated-list-sort-key '("Name" . nil))
  (setq apple-container--row-function #'apple-container--volume-row)
  (setq apple-container--list-args '("volume" "list" "--format" "json"))
  (tabulated-list-init-header))

;;;###autoload
(defun apple-container-volumes ()
  "List Apple container volumes."
  (interactive)
  (apple-container--show-list "*container volumes*"
                              #'apple-container-volumes-mode))

(defun apple-container-volume-inspect (name)
  "Show the JSON description of volume NAME."
  (interactive (list (or (and (derived-mode-p 'apple-container-volumes-mode)
                                (tabulated-list-get-id))
                           (read-string "Volume: "))))
  (apple-container--show-json (format "*container volume: %s*" name)
                              (list "volume" "inspect" name)))

(defun apple-container-volume-create (name &optional size)
  "Create volume NAME, optionally with SIZE (e.g. 10G)."
  (interactive (list (read-string "New volume name: ")
                     (let ((s (read-string "Size (empty for default): ")))
                       (unless (string-empty-p s) s))))
  (apple-container--action
   (append '("volume" "create") (and size (list "-s" size)) (list name))
   (format "Created volume %s" name)))

(defun apple-container-volume-delete (names)
  "Delete volumes NAMES."
  (interactive (list (apple-container--targets)))
  (when (yes-or-no-p (format "Delete volume %s? "
                             (apple-container--describe-targets names)))
    (apple-container--action (append '("volume" "delete") names)
                             (format "Deleted %s" (string-join names ", ")))))

(defun apple-container-volume-prune ()
  "Remove volumes no container refers to."
  (interactive)
  (when (yes-or-no-p "Remove all unreferenced volumes? ")
    (apple-container--action '("volume" "prune") "Pruned volumes")))

;;;; Networks

(defun apple-container--network-row (n)
  "Turn network N into a tabulated-list entry."
  (let* ((cfg (apple-container--config n))
         (status (alist-get 'status n))
         (name (or (alist-get 'id n) (alist-get 'name cfg) (alist-get 'id cfg))))
    (when name
      (list name
            (vector name
                    (format "%s" (or (alist-get 'ipv4Subnet status)
                                     (alist-get 'subnet cfg) ""))
                    (format "%s" (or (alist-get 'ipv4Gateway status) ""))
                    (format "%s" (or (alist-get 'ipv6Subnet status) ""))
                    (apple-container--format-time (alist-get 'creationDate cfg)))))))

(defvar apple-container-networks-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map apple-container-list-mode-map)
    (define-key map "?" #'apple-container-networks-help)
    (define-key map (kbd "RET") #'apple-container-network-inspect)
    (define-key map "+" #'apple-container-network-create)
    (define-key map "D" #'apple-container-network-delete)
    (define-key map "x" #'apple-container-network-prune)
    map)
  "Keymap for `apple-container-networks-mode'.")

(define-derived-mode apple-container-networks-mode apple-container-list-mode
  "Container Networks"
  "Major mode listing Apple container networks.
\\{apple-container-networks-mode-map}"
  (setq tabulated-list-format
        [("Name" 24 t)
         ("IPv4 subnet" 18 t)
         ("Gateway" 15 t)
         ("IPv6 subnet" 24 t)
         ("Created" 16 t)])
  (setq tabulated-list-sort-key '("Name" . nil))
  (setq apple-container--row-function #'apple-container--network-row)
  (setq apple-container--list-args '("network" "list" "--format" "json"))
  (tabulated-list-init-header))

;;;###autoload
(defun apple-container-networks ()
  "List Apple container networks."
  (interactive)
  (apple-container--show-list "*container networks*"
                              #'apple-container-networks-mode))

(defun apple-container-network-inspect (name)
  "Show the JSON description of network NAME."
  (interactive (list (or (and (derived-mode-p 'apple-container-networks-mode)
                                (tabulated-list-get-id))
                           (read-string "Network: "))))
  (apple-container--show-json (format "*container network: %s*" name)
                              (list "network" "inspect" name)))

(defun apple-container-network-create (name &optional subnet)
  "Create network NAME, optionally with IPv4 SUBNET in CIDR form."
  (interactive (list (read-string "New network name: ")
                     (let ((s (read-string "Subnet (empty for automatic): ")))
                       (unless (string-empty-p s) s))))
  (apple-container--action
   (append '("network" "create") (and subnet (list "--subnet" subnet))
           (list name))
   (format "Created network %s" name)))

(defun apple-container-network-delete (names)
  "Delete networks NAMES."
  (interactive (list (apple-container--targets)))
  (when (yes-or-no-p (format "Delete network %s? "
                             (apple-container--describe-targets names)))
    (apple-container--action (append '("network" "delete") names)
                             (format "Deleted %s" (string-join names ", ")))))

(defun apple-container-network-prune ()
  "Remove networks with no containers attached."
  (interactive)
  (when (yes-or-no-p "Remove all unused networks? ")
    (apple-container--action '("network" "prune") "Pruned networks")))

;;;; System

;;;###autoload
(defun apple-container-system-start ()
  "Start the container services.
Runs in an interactive buffer, so you can answer the kernel
installation prompt the first time."
  (interactive)
  (apple-container--run-in-buffer '("system" "start")))

;;;###autoload
(defun apple-container-system-stop ()
  "Stop the container services."
  (interactive)
  (when (yes-or-no-p "Stop the container services (and all containers)? ")
    (apple-container--action '("system" "stop") "Container services stopped")))

;;;###autoload
(defun apple-container-system-status ()
  "Show whether the container services are running."
  (interactive)
  (apple-container--call-async
   '("system" "status")
   (lambda (out) (message "%s" (string-trim out)))
   (lambda (err) (message "container services: %s"
                          (string-trim (or err "not running"))))))

(defun apple-container-system-version ()
  "Show the CLI and API server versions."
  (interactive)
  (apple-container--call-async
   '("system" "version" "--format" "json")
   (lambda (out)
     (message "%s"
              (mapconcat (lambda (c) (format "%s %s"
                                             (alist-get 'appName c)
                                             (alist-get 'version c)))
                         (apple-container--parse-json out) "; ")))))

(defun apple-container-system-df ()
  "Show disk usage for images, containers and volumes."
  (interactive)
  (apple-container--run-in-buffer '("system" "df")))

(defun apple-container-system-logs (&optional follow)
  "Show the last 10 minutes of service logs; with prefix FOLLOW, follow."
  (interactive "P")
  (apple-container--logs "*container system logs*"
                         (append '("system" "logs" "--last" "10m")
                                 (and follow '("--follow")))))

(defun apple-container-builder-start ()
  "Start the BuildKit builder."
  (interactive)
  (apple-container--action '("builder" "start") "Builder started"))

(defun apple-container-builder-stop ()
  "Stop the BuildKit builder."
  (interactive)
  (apple-container--action '("builder" "stop") "Builder stopped"))

;;;; Transient menus

;;;###autoload (autoload 'apple-container "apple-container" nil t)
(transient-define-prefix apple-container ()
  "Manage Apple's container runtime."
  [["Browse"
    ("c" "Containers" apple-container-containers)
    ("i" "Images" apple-container-images)
    ("v" "Volumes" apple-container-volumes)
    ("n" "Networks" apple-container-networks)]
   ["Create"
    ("r" "Run an image" apple-container-run)
    ("b" "Build an image" apple-container-build)
    ("p" "Pull an image" apple-container-pull)]
   ["Containers"
    ("e" "Shell in container" apple-container-shell)
    ("f" "Browse files" apple-container-dired)
    ("l" "Logs" apple-container-logs)]
   ["System"
    ("s" "Start services" apple-container-system-start)
    ("S" "Stop services" apple-container-system-stop)
    ("t" "Status" apple-container-system-status)
    ("V" "Version" apple-container-system-version)
    ("d" "Disk usage" apple-container-system-df)
    ("L" "Service logs" apple-container-system-logs)]
   ["Builder"
    ("B" "Start builder" apple-container-builder-start)
    ("X" "Stop builder" apple-container-builder-stop)]])

(transient-define-prefix apple-container-containers-help ()
  "Actions for the container list."
  [["Lifecycle"
    ("s" "Start" apple-container-start)
    ("S" "Stop" apple-container-stop)
    ("R" "Restart" apple-container-restart)
    ("K" "Kill (C-u: pick signal)" apple-container-kill)
    ("D" "Delete (C-u: force)" apple-container-delete)
    ("x" "Prune stopped" apple-container-prune)]
   ["Look inside"
    ("RET" "Inspect" apple-container-inspect)
    ("l" "Logs (C-u: boot log)" apple-container-logs)
    ("L" "Follow logs" apple-container-logs-follow)
    ("b" "Shell" apple-container-shell)
    ("e" "Run a command" apple-container-exec)
    ("f" "Browse files (TRAMP)" apple-container-dired)]
   ["Files"
    ("c" "Copy into container" apple-container-copy-to)
    ("C" "Copy out of container" apple-container-copy-from)]
   ["List"
    ("g" "Refresh" apple-container-refresh)
    ("a" "Toggle stopped" apple-container-toggle-all)
    ("m" "Mark" apple-container-mark :transient t)
    ("u" "Unmark" apple-container-unmark :transient t)
    ("U" "Unmark all" apple-container-unmark-all)]])

(transient-define-prefix apple-container-images-help ()
  "Actions for the image list."
  [["Image"
    ("RET" "Inspect" apple-container-image-inspect)
    ("r" "Run" apple-container-run)
    ("T" "Tag" apple-container-image-tag)
    ("P" "Push" apple-container-image-push)
    ("D" "Delete" apple-container-image-delete)]
   ["Store"
    ("p" "Pull" apple-container-pull)
    ("x" "Prune (C-u: all unused)" apple-container-image-prune)]
   ["List"
    ("g" "Refresh" apple-container-refresh)
    ("m" "Mark" apple-container-mark :transient t)
    ("u" "Unmark" apple-container-unmark :transient t)
    ("U" "Unmark all" apple-container-unmark-all)]])

(transient-define-prefix apple-container-volumes-help ()
  "Actions for the volume list."
  [["Volume"
    ("RET" "Inspect" apple-container-volume-inspect)
    ("+" "Create" apple-container-volume-create)
    ("D" "Delete" apple-container-volume-delete)
    ("x" "Prune unused" apple-container-volume-prune)]
   ["List"
    ("g" "Refresh" apple-container-refresh)
    ("m" "Mark" apple-container-mark :transient t)
    ("u" "Unmark" apple-container-unmark :transient t)]])

(transient-define-prefix apple-container-networks-help ()
  "Actions for the network list."
  [["Network"
    ("RET" "Inspect" apple-container-network-inspect)
    ("+" "Create" apple-container-network-create)
    ("D" "Delete" apple-container-network-delete)
    ("x" "Prune unused" apple-container-network-prune)]
   ["List"
    ("g" "Refresh" apple-container-refresh)
    ("m" "Mark" apple-container-mark :transient t)
    ("u" "Unmark" apple-container-unmark :transient t)]])

;;;; Evil

(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")

(defun apple-container--setup-evil ()
  "Let this package's keys win over Evil's normal and motion states.
Unbound keys such as j and k keep their Evil meaning."
  (dolist (map (list apple-container-list-mode-map
                     apple-container-containers-mode-map
                     apple-container-images-mode-map
                     apple-container-volumes-mode-map
                     apple-container-networks-mode-map
                     apple-container-logs-mode-map))
    (evil-make-overriding-map map nil))
  (dolist (hook '(apple-container-list-mode-hook apple-container-logs-mode-hook))
    (add-hook hook #'evil-normalize-keymaps)))

(with-eval-after-load 'evil
  (apple-container--setup-evil))

;;;; TRAMP

(defun apple-container--tramp-completion (&optional _file)
  "Return running containers for TRAMP host completion."
  (mapcar (lambda (id) (list nil id))
          (ignore-errors (apple-container--container-ids))))

;;;###autoload
(defun apple-container-tramp-setup ()
  "Register the TRAMP method for files inside Apple containers."
  (require 'tramp)
  (add-to-list 'tramp-methods
               `(,apple-container-tramp-method
                 (tramp-login-program ,(apple-container--program))
                 (tramp-login-args (("exec") ("-it") ("-u" "%u") ("%h") ("%l")))
                 (tramp-direct-async (,tramp-default-remote-shell "-c"))
                 (tramp-remote-shell ,tramp-default-remote-shell)
                 (tramp-remote-shell-login ("-l"))
                 (tramp-remote-shell-args ("-i" "-c"))))
  (tramp-set-completion-function
   apple-container-tramp-method
   '((apple-container--tramp-completion ""))))

;;;###autoload
(with-eval-after-load 'tramp
  (condition-case err
      (apple-container-tramp-setup)
    (error (message "apple-container: TRAMP method not set up: %s"
                    (error-message-string err)))))

(provide 'apple-container)
;;; apple-container.el ends here
