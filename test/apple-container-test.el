;;; apple-container-test.el --- Tests for apple-container  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/apple-container-test.el -f ert-run-tests-batch-and-exit
;; Uses test/mock-container in place of the real CLI.

;;; Code:

(require 'ert)
(require 'apple-container)

(defvar apple-container-test-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar apple-container-test-log
  (make-temp-file "mock-container-log"))

(setq apple-container-program
      (expand-file-name "mock-container" apple-container-test-dir))
(setenv "MOCK_LOG" apple-container-test-log)

(defun apple-container-test--wait (pred &optional timeout)
  "Spin the event loop until PRED is non-nil or TIMEOUT seconds pass."
  (let ((end (+ (float-time) (or timeout 5))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defun apple-container-test--rows (buffer)
  "Return the printed rows of list BUFFER as (ID . COLUMNS)."
  (with-current-buffer buffer
    (mapcar (lambda (e) (cons (car e) (append (cadr e) nil)))
            tabulated-list-entries)))

(defun apple-container-test--logged ()
  "Return the CLI calls recorded by the mock."
  (with-temp-buffer
    (insert-file-contents apple-container-test-log)
    (split-string (buffer-string) "\n" t)))

(defmacro apple-container-test--with-list (fn buffer-name &rest body)
  "Open list with FN, wait for BUFFER-NAME to fill, then run BODY."
  (declare (indent 2))
  `(save-window-excursion
     (funcall ,fn)
     (let ((buf (get-buffer ,buffer-name)))
       (should (apple-container-test--wait
                (lambda () (buffer-local-value 'tabulated-list-entries buf))))
       (unwind-protect (with-current-buffer buf ,@body)
         (kill-buffer buf)))))

(ert-deftest apple-container-test-containers-list ()
  (apple-container-test--with-list #'apple-container-containers "*containers*"
    (let ((rows (apple-container-test--rows buf)))
      (should (equal (sort (mapcar #'car rows) #'string<) '("db" "web")))
      (let ((web (cdr (assoc "web" rows))))
        (should (equal (nth 1 web) "nginx:latest"))
        (should (equal (substring-no-properties (nth 2 web)) "running"))
        (should (equal (nth 3 web) "192.168.64.3"))
        (should (equal (nth 4 web) "8080->80/tcp"))
        (should (equal (substring-no-properties (nth 6 web)) "1 GiB")))
      ;; Old-style snapshot: status as a plain string, Swift-epoch date.
      (let ((db (cdr (assoc "db" rows))))
        (should (equal (substring-no-properties (nth 2 db)) "stopped"))
        (should (string-prefix-p "2025-" (nth 7 db)))))
    (should (string-match-p "web" (buffer-string)))))

(ert-deftest apple-container-test-toggle-all ()
  (apple-container-test--with-list #'apple-container-containers "*containers*"
    (apple-container-toggle-all)
    (should (apple-container-test--wait
             (lambda () (= 1 (length tabulated-list-entries)))))))

(ert-deftest apple-container-test-marks-survive-refresh ()
  (apple-container-test--with-list #'apple-container-containers "*containers*"
    (goto-char (point-min))
    (while (not (equal (tabulated-list-get-id) "db")) (forward-line 1))
    (apple-container-mark)
    (should (equal (apple-container--marked-ids) '("db")))
    (apple-container-refresh)
    (should (apple-container-test--wait (lambda () (not apple-container--loading))))
    (should (equal (apple-container--marked-ids) '("db")))
    (apple-container-unmark-all)
    (should (null (apple-container--marked-ids)))))

(ert-deftest apple-container-test-images-list ()
  (apple-container-test--with-list #'apple-container-images "*container images*"
    (let ((rows (apple-container-test--rows buf)))
      (let ((nginx (cdr (assoc "nginx:latest" rows))))
        (should (equal (nth 0 nginx) "nginx"))
        (should (equal (nth 1 nginx) "latest"))
        (should (equal (nth 2 nginx) "0123456789ab"))
        ;; Picks the arm64 variant's size.
        (should (equal (get-text-property 0 'apple-container-bytes (nth 3 nginx))
                       70000000)))
      (let ((app (cdr (assoc "ghcr.io/me/app@sha256:feedface" rows))))
        (should (equal (nth 1 app) "<none>"))))))

(ert-deftest apple-container-test-volumes-and-networks ()
  (apple-container-test--with-list #'apple-container-volumes "*container volumes*"
    (let ((rows (apple-container-test--rows buf)))
      (should (equal (nth 1 (cdr (assoc "data" rows))) "named"))
      (should (equal (nth 1 (cdr (assoc "anon1" rows))) "anonymous"))))
  (apple-container-test--with-list #'apple-container-networks "*container networks*"
    (let ((row (cdr (assoc "default" (apple-container-test--rows buf)))))
      (should (equal (nth 1 row) "192.168.64.0/24"))
      (should (equal (nth 2 row) "192.168.64.1")))))

(ert-deftest apple-container-test-actions-on-marked ()
  (write-region "" nil apple-container-test-log)
  (apple-container-test--with-list #'apple-container-containers "*containers*"
    (apple-container-toggle-marks)
    (cl-letf (((symbol-function 'yes-or-no-p) #'always))
      (apple-container-delete (apple-container--targets) t))
    (should (apple-container-test--wait
             (lambda () (member "delete --force db web"
                                (apple-container-test--logged)))))))

(ert-deftest apple-container-test-error-hint ()
  (let (msg)
    (apple-container--call-async '("stop" "broken") #'ignore
                                 (lambda (err) (setq msg err)))
    (should (apple-container-test--wait (lambda () msg)))
    (should (string-match-p "system-start"
                            (apple-container--explain-error msg)))))

(ert-deftest apple-container-test-inspect-and-logs ()
  (save-window-excursion
    (apple-container-inspect "web")
    (should (apple-container-test--wait
             (lambda () (get-buffer "*container inspect: web*"))))
    (with-current-buffer "*container inspect: web*"
      (should (string-match-p "\n +\"nested\"" (buffer-string)))
      (should buffer-read-only))
    (apple-container-logs "web")
    (with-current-buffer "*container logs: web*"
      (should (apple-container-test--wait
               (lambda () (string-match-p "last line" (buffer-string)))))
      ;; ANSI escapes are turned into faces, not left as text.
      (should-not (string-match-p "\e" (buffer-string))))))

(ert-deftest apple-container-test-sync-readers ()
  (should (equal (apple-container--container-ids) '("web")))
  (should (equal (apple-container--container-ids t) '("web" "db")))
  (should (member "nginx:latest" (apple-container--image-names))))

(ert-deftest apple-container-test-tramp ()
  (require 'tramp)
  (should (assoc "container" tramp-methods))
  (should (equal (apple-container--tramp-completion) '((nil "web"))))
  (let ((file (make-temp-file "apple-container-tramp")))
    (with-temp-file file (insert "hello from inside"))
    (let* ((tramp-verbose 0)
           (remote (format "/container:web:%s" file)))
      (should (file-exists-p remote))
      (should (equal (with-temp-buffer
                       (insert-file-contents remote)
                       (buffer-string))
                     "hello from inside"))
      (write-region "edited" nil remote)
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     "edited")))))

;; Portal's native terminal can't run here, so stand in for its entry point.
(ert-deftest apple-container-test-portal-shell ()
  (let ((apple-container-terminal-backend 'portal)
        (calls nil))
    (cl-letf (((symbol-function 'portal-terminal-start)
               (lambda (dir program args name)
                 (push (list dir program args name) calls)
                 (get-buffer-create name))))
      (provide 'portal-terminal)
      (save-window-excursion
        (apple-container-shell "web")
        ;; A second call reuses the buffer instead of starting a new shell.
        (apple-container-shell "web")
        (should (= 1 (length calls)))
        (pcase-let ((`(,dir ,program ,args ,name) (car calls)))
          (should (equal dir (expand-file-name "~")))
          (should (equal program apple-container-program))
          (should (equal (seq-take args 3) '("exec" "-it" "web")))
          (should (equal name "*container shell: web*")))
        (apple-container-shell "web" t)
        (should (= 2 (length calls)))
        (kill-buffer "*container shell: web*")))))

(provide 'apple-container-test)
;;; apple-container-test.el ends here
