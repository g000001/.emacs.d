;;; /usr/local/share/emacs/27.0.50/lisp/startup.el.gz
;;(setq initial-buffer-choice (buffer-live-p "*Messages*"))
;early-init-file
;"/mc/.emacs.d/early-init"
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "emacs-startup-hook")
	    (switch-to-buffer "*Messages*")
	    (split-window-right)
	    (find-file user-init-file)))
(add-hook 'after-init-hook (lambda ()
			     (message "after-init-hook")
                             (setup.myinfo)
			     (setup.font)
                             (setup.keybinds)
			     (setup.looking)
                             (setup.text-editing)
                             (setup.ui)
                             (setup.utils)
			     (setup.slime)))

(defun setup.skk ()
  (set-language-environment "Japanese")
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default buffer-file-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (add-hook 'term-setup-hook
            (lambda ()
              (set-terminal-coding-system 'utf-8)))
  (setq skk-server-host "localhost")
  (autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
  (autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
  (setq skk-use-azik t)
  (setq skk-azik-keyboard-type 'en)
  (add-hook 'isearch-mode-hook
            (lambda ()
              (and (boundp 'skk-mode)
                   skk-mode
                   (skk-isearch-mode-setup))))
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (when (boundp 'skk-mode)
                skk-mode
                (skk-isearch-mode-cleanup)))))

(defun setup.looking ()
  (progn
    ;;(set-face-background 'mode-line "#990000")
    ;;(set-face-foreground 'mode-line "#eeeeee")
    (set-face-foreground 'mode-line "#003142")
    (set-face-background 'mode-line "#9f9f6f"))
  '(setup.display-time)
  ;;(powerline-vim-theme)
  (powerline-default-theme)
  ;;(powerline-center-theme)
  ;; (powerline-nano-theme)
  (setq column-number-mode t)
  (set-face-background 'region nil)
  (set-face-attribute 'region nil
                      :weight 'normal
                      :underline t
                      :overline nil
                      :slant 'normal)
  (setq-default indent-tabs-mode nil)
  (show-paren-mode 1)
  (midnight-mode 1))

(defun setup.text-editing ()
  (setq-default indent-tabs-mode nil)
  (setq indent-line-function 'indent-relative-maybe)
  (setup.skk))

(defun setup.ui ()
  (add-to-list 'ignored-local-variables 'syntax)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq browse-url-browser-function 'browse-url-firefox))

(defun setup.display-time ()
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (display-time))

(defun setup.font ()
  ;; フォント
  ;; abcdefghijklmnopqrstuvwxyz 
  ;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
  ;; `1234567890-=\[];',./
  ;; ~!@#$%^&*()_+|{}:"<>?
  ;;
  ;; 壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五
  ;; 123456789012345678901234567890123456789012345678901234567890
  ;; ABCdeＡＢＣｄｅ
  ;;
  ;; ┌─────────────────────────────┐
  ;; │　　　　　　　　　　　　　罫線                            │
  ;; └─────────────────────────────┘
  ;;
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Cica" :size 22))
  (set-fontset-font nil 'ascii (font-spec :family "Cica" :size 22))
  )

(defun setup.slime ()
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-default-lisp 'lispworks)
  (setq slime-lisp-implementations
	`((sbcl
           ("/l/sbcl/sb-quicklisp")
           :coding-system utf-8-unix)
          (sbcl-pure
           ("/l/sbcl/newest/bin/sbcl")
           :coding-system utf-8-unix)
          (sbcl-debian
           ("/usr/bin/sbcl.wrapper")
           :coding-system utf-8-unix)
          (allegro-beta
           ("/usr/local/acl/acl10.1-smp.64/alisp")
           :coding-system utf-8-unix)
          (allegro9
           ("/usr/local/acl/acl90-smp.64/alisp")
           :coding-system utf-8-unix)
          (allegro
           ("/l/allegro/acl82.64/alisp")
           :coding-system utf-8-unix)
          (allegro81
           ("/usr/local/acl/acl81.64/alisp")
           :coding-system utf-8-unix)
          (mlisp
           ("/usr/local/acl/newest/mlisp")
           :coding-system utf-8-unix)
          (mlisp81
           ("/usr/local/acl/acl81.64/mlisp")
           :coding-system utf-8-unix)
          (allegrox
           ("/l/allegro/acl10.1express/alisp")
           :coding-system utf-8-unix)
          (allegroxm
           ("/usr/local/acl/acl90express/mlisp")
           :coding-system utf-8-unix)
          (lispworks6
           ("/usr/local/lispworks/6.1.1/lispworks-personal-6-1-1-x86-linux")
           :coding-system utf-8-unix)
          (lispworks
           ("~/bin/lw-console")
           :coding-system utf-8-unix)
          (lw611
           ("/usr/local/lispworks/6.1.1/lispworks-personal-6-1-1-x86-linux")
           :coding-system utf-8-unix)
          (lw601
           ("/usr/local/lispworks/6.0.1/lispworks-personal-6-0-1-x86-linux")
           :coding-system utf-8-unix)
          (lw511
           ("/usr/local/lispworks/5.1.1/lispworks-personal-5-1-1-x86-linux")
           :coding-system utf-8-unix)
          (lw501
           ("/usr/local/lispworks/5.0.1/lispworks-personal-5-0-1-x86-linux")
           :coding-system utf-8-unix)
          (lw446
           ("/usr/local/lispworks/4.4.6/lispworks-personal-4460")
           :coding-system utf-8-unix)
          (lw445
           ("/usr/local/lispworks/4.4.5/lispworks-personal-4450")
           :coding-system utf-8-unix)
          (lw427
           ("~/bin/lw427")
           :coding-system utf-8-unix)
          (clisp
           ("/l/clisp/newest/bin/clisp")
           ;; ("/usr/bin/clisp")
           :coding-system utf-8-unix)
          ;; (ecl
          ;;  ("/usr/local/ecl/newest/bin/ecl.wrapper")
          ;;  :coding-system iso-8859-1-unix)
          (ecl
           ("/l/ecl/newest/bin/ecl")
           :coding-system utf-8-unix)
          (cmucl
           ;; ("/usr/local/cmucl/newest/bin/lisp")
           ("/l/cmucl/newest/bin/lisp")
           :coding-system utf-8-unix)
          (ccl
           ("/l/ccl/newest/lx86cl64")
           :coding-system utf-8-unix)
          (scl
           ("/opt/scl/bin/lisp")
           :coding-system utf-8-unix)
          (abcl
           ("abcl")
           :coding-system utf-8-unix)
          (mkcl
           ("/usr/local/mkcl/newest/bin/mkcl")
           :coding-system utf-8-unix)
          (scl
           ("/opt/scl/bin/lisp")
           :coding-system utf-8-unix)
          (clasp
           ("/usr/local/clasp/newest/clasp/build/clasp")
           :coding-system utf-8-unix)))
  (define-key global-map [(control ?\;)] 'slime-selector)
  (define-key slime-mode-map [(meta shift ?e)] 'slime-eval-region)
  (define-key slime-mode-map [(control shift ?c)] 'slime-compile-defun)
  (define-key slime-mode-map [(control ?c) ?\;] 'slime-insert-balanced-comments)
  (define-key slime-mode-map [(control ?c) (meta ?\;)] 'slime-remove-balanced-comments)
  (progn
    (dolist (font '("CMU Serif Extra-30"
                    "new century schoolbook-30"
                    "01フロップデザイン-30"))
      (when (ignore-errors (set-face-font 'sldb-topline-face font))
        (return)))
    (dolist (font '("CMU Serif Extra"
                    "new century schoolbook"))
      (when (ignore-errors (set-face-font 'sldb-section-face "new century schoolbook"))
        (return)))
    (set-face-foreground 'sldb-local-name-face "light green")
    (set-face-foreground 'sldb-detailed-frame-line-face "yellow")
    (set-face-foreground 'sldb-frame-label-face "#888888")
    (set-face-foreground 'sldb-frame-line-face "yellow")
    (set-face-foreground 'sldb-restart-number-face "cyan")
    (set-face-foreground 'sldb-restartable-frame-line-face "yellow")
    (set-face-foreground 'slime-repl-inputed-output-face "green")
    (set-face-bold 'slime-repl-inputed-output-face nil)
    (set-face-italic 'slime-repl-inputed-output-face nil)
    (set-face-underline 'sldb-section-face t)
    (set-face-foreground 'sldb-topline-face "orange")
    (set-face-attribute 'sldb-topline-face nil :underline nil)
    ;; (set-face-attribute 'sldb-topline-face nil :inverse-video t)
    (set-face-attribute 'sldb-topline-face nil :inverse-video nil)
    ;;(set-face-attribute 'sldb-topline-face nil :height 145)
    (set-face-attribute 'sldb-topline-face nil :bold t)
    (set-face-foreground 'sldb-condition-face "#8888aa")
    (set-face-foreground 'sldb-section-face "green")))


(defun setup.utils ()
  (require 'buffer-utils)
  (define-key global-map [(super ?n)] 'open-next-entry)
  (define-key global-map [(super ?p)] 'open-previous-entry)
  (define-key global-map [(super ?x) ?c] 'kill-friend-buffer))


(defun setup.keybinds ()
  (let ((map global-map))
      (define-key map [(control tab)] 'other-window)
      (define-key map [(menu)] nil)
      (define-key map [(control ?h)] 'delete-backward-char)
      (define-key map [(control super ?b)] 'blog-buffer-bundle-next-buffer)
      (define-key map [(control meta ?>)] 'find-testcase)
      ;; recursive-edit
      '(define-key map [(control ?c) ?r] (lambda ()
                                           (interactive)
                                           (save-window-excursion
                                             (save-excursion
                                               (recursive-edit)))))
      (define-key map [(control meta shift ?h) ] 'kill-backward-up-list)
      (define-key map [(control shift ?h)] 'backward-kill-word)
      (define-key map [(super ?c)] 'byte-compile-file)
      ;; help
      (define-key map [(super ?h)] 'help-command)
      (define-key map [(control shift ?d)] 'zap-to-char)
      (define-key map [(super shift ?s)] 'env-scheme)
      (define-key map [(super ?l)] 'env-slime)
      (define-key map [(super shift ?l)] 'env-log)
      (define-key map [(super ?d)] 'env-hatena)
      (define-key map [(super ?b)] 'env-blog)
      (define-key map [(super ?M)] 'env-memopad)
      (define-key map [(super ?.)] 'browse-url-at-point)
      
      ;; other window
      (define-key map [(control tab)] 'other-window)
      (define-key map [(control shift iso-lefttab)]
        (lambda () (interactive) (other-window -1)))
      ;; 逆向きkill-sexp
      (define-key map [(shift control meta ?k)]
        (lambda (&optional arg)
          (interactive "p")
          (kill-sexp (- arg))))
      (global-set-key [(meta ?\l)] 'insert-parentheses)
;; M-: runs the command eval-expression (found in global-map), which is
      (global-set-key [(meta ?\:)] 'move-past-close-and-reindent)))

(progn
  (defun the-last-g-file ()
    (format "~/doc/memo/%s"
            (car
             (reverse
              (sort (cl-remove-if-not (lambda (s)
                                        (eq 0 (string-match "g[0-9]+.md[^~#]*$" s)))
                                      (directory-files "~/doc/memo/"))
                    #'string-lessp)))))

  
  (defun the-last-lisp-scratch-file ()
    (format "~/lisp.scratch/%s"
            (car
             (reverse
              (sort (cl-remove-if-not (lambda (s)
                                        (eq 0 (string-match "g.*[^~#]$" s)))
                                      (directory-files "~/lisp.scratch/"))
                    #'string-lessp)))))


  (define-key goto-map [?t]
    (defun open-g-file ()
      (interactive)
      (find-file (the-last-g-file))))


  (define-key goto-map [?l]
    (defun open-lisp-scratch-file ()
      (interactive)
      (find-file (the-last-lisp-scratch-file)))))

(defun save-buffer-pathname ()
  (interactive)
  (kill-new (buffer-file-name)))

(defun just-one-space-to-next-sexp (&optional n)
  (interactive "*p")
  (let ((orig-pos (point)))
    (skip-chars-backward " \t\n")
    (constrain-to-field nil orig-pos)
    (cl-dotimes (i (or n 1))
      i
      (if (or (= (following-char) ?\s))
          (forward-char 1)
        (insert ?\s)))
    (delete-region
     (point)
     (progn
       (skip-chars-forward " \t\n")
       (constrain-to-field nil orig-pos t)))))

(define-key global-map [(super ?\ )] #'just-one-space-to-next-sexp)

;; shift spaceで、-を出す => control-/ に変更してみる  8:03pm Monday,29 December 2014
(define-key global-map [(meta ?\ )]
  (lambda (arg)
    (interactive "p")
    (cl-dotimes (i arg)
      i
      (insert "-"))))

(defun get-universal-time ()
  (truncate (+ 2208988800 (float-time (current-time)))))


(defun universal-time-string (ut)
  (format-time-string "%Y/%m/%d %H:%M" (seconds-to-time (- ut 2208988800))))

(defun insert-ut-and-date ()
  (interactive)
  (let ((ut (get-universal-time)))
    (insert
     (format "%s\n%s"
             ut
             (universal-time-string ut)))))


(progn
  (defvar my-eof-mesg ";;; *EOF*\n")


  (defun write-eof-line ()
    (let ((bfsize (buffer-size)))
      (unless (string-match "\\*EOF\\*"
                            (buffer-string)
                            (- bfsize (length my-eof-mesg)))
        (save-excursion
          (goto-char (point-max))
          (insert my-eof-mesg))
        )))


  (defvar lines-between-forms 2)


  (defun space-evenly ()
    (interactive)
    (save-excursion 
      (goto-char (point-min))
      (while (not (= (point-max) (point)))
        (end-of-defun 1)
        (just-one-space-to-next-sexp)
        (delete-char -1)
        (let ((cnt (+ 2 lines-between-forms)))
          (while (not (zerop (decf cnt)))
            (insert "\n"))))))


  (define-key global-map [(hyper ?f)]
    (defun format-lisp-file ()
      (interactive)
      (space-evenly)
      (write-eof-line))))


(defun tweet-region (start end)
  (interactive "r")
  (slime-eval-async `(\?:twe ,(buffer-substring-no-properties start end)))
  nil)

(defun tt-region (start end)
  (interactive "r")
  (slime-eval-async `(\?::twe/catface ,(buffer-substring-no-properties start end)))
  nil)

(defun tweet-cont-region (start end)
  (interactive "r")
  (slime-eval-async `(\?:con ,(buffer-substring-no-properties start end)))
  nil)


(defun how-many-days-since (date)
  (let* ((time (time-since
                (date-to-time date)))
         (ss (+ (ash (car time) 16) (cadr time))))
    (ceiling (+ ss (* 9 60 60))
             (* 24 60 60))))

(cl-defun ed (&optional arg)
  (cond
    ;; tならば、gazonk.delを開く(scratchファイルのつもり)
    ((eq t arg)
                                        ;(find-file "~/tmp/gazonk.del")
     (find-file "~/tmp/gazonk.del")
     )
    ;; (ed nil)ならば、最近のバッファに移動
    ((null arg) (switch-to-buffer (cadr (buffer-list))))
    ;; ファイル名が指定されていれば、指定ファイルに移動
    (t (find-file arg))))


(defun gazonk ()
  (interactive)
  (ed (format "~/doc/memo/g%s.del" (how-many-days-since "May 4 0:00 1974 +9:00")))
  (lisp-interaction-mode)
  (goto-char (point-max)))
(put 'downcase-region 'disabled nil)
