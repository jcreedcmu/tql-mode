;;; -*- lexical-binding: t -*-
;; Tiros query major mode

(require 'smie)

(setq tiros-mode-highlights
      '(("\\_<[A-Z][-a-zA-Z0-9/_]*" . font-lock-variable-name-face)
        ("\\[\\\([-a-zA-Z0-9/_ ]+\\)\\]" . (1 font-lock-preprocessor-face))
        ("\\(let\\|\\def\\)\s+\\([-a-zA-Z0-9/_]+\\)(" . (2 font-lock-type-face))
        ("\\bdef\\b\\|\\ball\\b\\|\\bex\\b\\|<=>\\|=>\\|&&\\|||\\|\\blet\\b\\|\\bin\\b\\|=" . font-lock-keyword-face)
        ("\\btrue\\b\\|\\bfalse\\b\\|\\blist\\b\\|\\bcount\\b" . font-lock-keyword-face)
        ("\\([-a-zA-Z0-9/_]+\\)(" . (1 font-lock-function-name-face))))

(defvar tiros-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'tiros-run-current-query-file)
    (define-key map (kbd "C-c C-c") 'tiros-run-current-query)
    map)
  "Keymap for `tiros-mode'.")

(define-derived-mode tiros-mode fundamental-mode
  (setq font-lock-defaults '(tiros-mode-highlights))
  (setq mode-name "Tiros")
  (tiros-init))

(defconst tiros-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (decl (prop ".")
             ("def" prop "=" prop "."))
       (terms (id) (terms "," terms))
       (prop ("all" id ":" prop)
             ("let" prop "=" prop "in" prop)
             ("ex" id ":" prop)
             (prop "<=>" prop)
             (prop "=>" prop)
             (prop "&&" prop)
             (prop "||" prop)
             ("(" prop ")")
             ("(" terms ")")
             (id)))
     '((assoc ","))
     '((assoc ":") (assoc "in") (assoc "<=>") (assoc "=>") (assoc "||") (assoc "&&")))
    )))

(defun tiros-rules (kind token)
  (pcase (cons kind token)
    (`(:after . "in") 0)
    (`(:elem . basic) 2)
    (`(:elem . args) 0)))

(setq tiros-mode-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        table))

(defun tiros-init ()
  (set-syntax-table tiros-mode-syntax-table)
  (smie-setup tiros-grammar #'tiros-rules)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (use-local-map tiros-mode-map))

(defun current-query-region ()
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (lexical-let* ((begin
                    (save-excursion
                      (let ((search (search-backward "." (buffer-end -1) t)))
                        (if search (+ 1 (point)) (buffer-end -1)))))
                   (end
                    (save-excursion
                      (search-forward "." (buffer-end 1) "limit") (point))))
      (cons begin end))))

(defun tiros-defs-of-string (str)
  (apply 'concat
         (delq nil
               (mapcar
                (lambda (x) (if (tiros-defp x) (concat x ".") nil))
                (split-string str "[.]" )))))

(defun tiros-defp (str)
  (string-match "^[ \n\r\t]*def" str))

(defun tiros-defs-of-buffer ()
  (tiros-defs-of-string (buffer-substring-no-properties (point-min) (point-max))))

(defvar tiros-response-buffer-name "*Tiros Response*")

(defvar tiros-endpoint 'dev "'dev | 'prod | 'local")
(defvar tiros-flags "" "extra flags for tiros cli")

(defun tiros-run-current-query ()
  (interactive)
  (lexical-let*
      ((region-endpoints (current-query-region))
       (query (buffer-substring-no-properties
                 (car region-endpoints)
                 (cdr region-endpoints)))
       (full-query
        (concat (tiros-defs-of-buffer)
                "\n"
                query)))
    (if (not (string-match "[^ \n\r\t]" query)) (error "There is no query here."))
    (if (tiros-defp query) (error "Can't run a definition. Try running a query instead."))
    (tiros-run-query full-query)))

(defun tiros-run-current-query-file ()
  (interactive)
  (tiros-run-query (buffer-substring-no-properties
                    (point-min)
                    (point-max))))

(defun tiros-run-query (query)
  (interactive)
  (lexical-let*
      ((query-file-name "/tmp/query.tql")
       (buf (get-buffer-create tiros-response-buffer-name))
       (start-time (current-time))
       (fin (lambda (proc event)
              (when (equal event "finished")
                (lexical-let* ((end-time (current-time))
                               (time (time-subtract end-time start-time)))
                  (message "Tiros finished: %s s" (time-to-seconds time))
                  (set-window-point (get-buffer-window buf) (point-min))))))
       (endp (pcase tiros-endpoint
               (`dev "--dev")
               (`prod "")
               (`local "--host localhost:9000 --no-ssl")))
       (cmd (concat
             "tiros query " tiros-flags " " endp " -f " query-file-name)))
    (with-temp-file query-file-name (insert query))
    (display-buffer buf '(display-buffer-reuse-window))
    (with-current-buffer buf (erase-buffer))
    (lexical-let* ((p (start-process-shell-command
                       "tiros" tiros-response-buffer-name cmd))
                   (s (process-sentinel p)))
      (set-process-sentinel p fin))))

(provide 'tiros)
