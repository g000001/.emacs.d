(easy-mmode-define-minor-mode pc-ppr-minor-mode
                              ""
                              nil
                              " Pieces of Paper"
                              '((" " . exit-pc-ppr)))

(defun exit-pc-ppr ()
  (interactive)
  (kill-buffer "*Piece of Paper*")
  (pc-ppr-minor-mode -1))

(defun write-pc-ppr (string)
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((buf (get-buffer-create "*Piece of Paper*")))
      (switch-to-buffer buf)
      (lisp-mode)
      (pc-ppr-minor-mode 1)
      (insert string)
      (goto-char (point-min))
      (setq buffer-read-only t) )))

(defun superimpose (string)
  (let* ((pc-ppr (concat string
                          (string 10)
                          ;; (make-string (- (window-total-width) 3) ?-)
                          (make-string (- (/ (window-body-width) 2) 4) ?─)
                          (make-string (- (/ (window-body-width) 2) 4) ?-)
                          ;; ─────
                          ;; ----------
                          ))
         (pc-ppr-nl (count-newlines pc-ppr))
         (bg (buffer-substring-no-properties (window-start) (window-end)))
         (bglen (length bg))
         (bgstart (if (<= (length pc-ppr) bglen)
                      (nth-line-pos pc-ppr-nl bg)
                      bglen)))
    (concat pc-ppr (subseq bg bgstart))))

(defun count-newlines (string)
  (count 10 string))

(defun nth-line-pos (n string)
  (let ((cnt 0)
        (pos 0)
        (limit (length string)))
    (catch 'found
      (while (< pos limit)
        (when (= 10 (aref string pos))
          (when (= n cnt)
            (throw 'found pos))
          (incf cnt))
        (incf pos)))))

;; emacs lisp macroexpand
(define-key emacs-lisp-mode-map [(control ?c) (control ?m)]
  (lambda ()
    (interactive)
    (write-pc-ppr
     (superimpose
      (with-output-to-string (pp
                              (macroexpand
                               (read-from-whole-string
                                (thing-at-point 'list)))))))))

;; CL/slime macroexpand-1
(defun ppr-pc-macroexpand-1 (&optional repeatedly)
  (interactive "P")
  (ppr-pc-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))


(defun ppr-pc-eval-macroexpand (expander &optional string)
  (let ((string (or string (slime-sexp-at-point))))
    (setq slime-eval-macroexpand-expression `(,expander ,string))
    (slime-eval-async slime-eval-macroexpand-expression
      (lambda (s) (write-pc-ppr (superimpose s))))))

(define-key slime-mode-map [(control shift ?m)]
  #'ppr-pc-macroexpand-1)



(progn
  (defun slime-print-form ()
    "(Print) the form at point."
    (interactive)
    (slime-eval-and-print
     `(swank:eval-and-grab-output
       ,(format "(cl:print %s)" (slime-defun-at-point)))))

  (defun slime-eval-and-print (form)
    "Print FORM in Lisp and display the result in a new buffer."
    (slime-eval-async form
      (lambda (s) (write-pc-ppr (superimpose (first s))))))

  ;; control-shift-d
  (define-key slime-mode-map
    [(control shift ?e)] 'slime-print-form))

(define-key slime-doc-map [(control ?d)]
  (defun slime-describe-symbol-ppr (symbol-name)
    "Describe the symbol at point."
    (interactive (list (slime-read-symbol-name "Describe symbol: ")))
    (when (not symbol-name)
      (error "No symbol given"))
    (slime-eval-async `(swank:describe-symbol ,symbol-name)
                       (lambda (s) (write-pc-ppr (superimpose s))))))


;; eof
