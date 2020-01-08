(if (not (fboundp 'last))
    (fset 'last 'lisp-last))

(if (not (fboundp 'gensym))
    (progn
      (defvar gensym-counter 0)
      (defun gensym ()
	(let ((s (make-symbol (format "g%06d" gensym-counter))))
	  (setq gensym-counter (+ 1 gensym-counter))
	  s))))

(if (not (fboundp 'push))
    (defmacro push (item var)
      (list 'let (list (list 'item item))
	    (list 'setq var (list 'cons 'item var)))))

;; 循環リストを作成する
(defun circular-list (&rest elts)
  (cdr (rplacd (last elts) elts)))

(defvar window-configuration-list)

(defun init-window-configuration-list ()
  (setq window-configuration-list
        (circular-list (list 0
                             (current-window-configuration)))))


;;

;; wcc => Window Configuration Container

;; window-configuration-list ::= {wcc} *
;; wcc ::= (name window-configuration)
;; name ::= symbol | function

(defun current-wcc ()
  (car window-configuration-list))

(defun wcc-wc (wc)
  (second wc))

(defun wcc-name (wc)
  (first wc))

(defun make-wcc (name &optional wcc)
  (list name
        (or wcc (current-window-configuration))))

(defun update-wcl (&optional wcc)
  (setq wcc (or wcc (list (wcc-name (current-wcc))
                          (current-window-configuration))))
  (setcar window-configuration-list
          (apply #'make-wcc wcc)))

(defun push-new-wcl (wcc)
  (setcar window-configuration-list
          (list wcset
                (current-window-configuration))))

(defun make-vanilla-wcc (name)
  (interactive)
  (prog1 (make-wcc name)
         (gazonk)
         (delete-other-windows)))

'(comment
 (defun search-wc (item)
   (search item (subseq window-configuration-list
                        0
                        (window-configuration-list-size)))))
(defun search-wc (key)
  (position-if #'(lambda (x)
                 (equal key (car x)))
               (subseq window-configuration-list
                       0
                       (window-configuration-list-size
                        window-configuration-list))))

;; window-configurationをリストにプッシュ
(defun push-window-configuration (&optional wc-name)
  (interactive)
  (let ((g (gensym)))
    (push (make-vanilla-wcc (or wc-name g))
          (cdr window-configuration-list))
    (message (format "created %s (%d)"
                     (or wc-name g)
                     (window-configuration-list-size)))))

;; window-configurationをリストにプッシュ
(defun push-window-configuration* (&optional wc-name)
  (interactive)
  (let ((g (gensym)))
    (push (make-wcc (or wc-name g))
          (cdr window-configuration-list))
    (message (format "created %s (%d)"
                     (or wc-name g)
                     (window-configuration-list-size)))))


;; FIXME eqで比較するだけ
(defun window-configuration-list-size (&optional wcl)
  (let ((start (or wcl window-configuration-list))
        (cnt 1))
    (catch 'count
      (mapl #'(lambda (x)
              (when (eq start x)
                (throw 'count nil))
              (incf cnt))
            (cdr wcl)))
    cnt))

(defun cycle-window-configuration-list ()
  (interactive)
  ;; 現在の窓の状態で更新
  (update-wcl)
  ;; 順送り
  (setq window-configuration-list
        (cdr window-configuration-list))
  ;; 次の要素を現在の窓設定にする
  (set-window-configuration (wcc-wc (current-wcc)))
  (message (format "%s" (wcc-name (current-wcc)))))

(defun delete-window-configuration ()
  (interactive)
  (setcar window-configuration-list
          (cadr window-configuration-list))
  (setcdr window-configuration-list
          (cddr window-configuration-list))
  (set-window-configuration
   (cadr (car window-configuration-list)))
  (message (format "deleted (-> %d)"
                   (window-configuration-list-size
                    window-configuration-list))))
(defun next-wcc ()
  (setq window-configuration-list
        (cdr window-configuration-list)))

(init-window-configuration-list)
;; gazonk
(define-key global-map [(super ?g)]
  #'(lambda ()
    (interactive)
    (let ((pos (search-wc '#'gazonk)))
      (if pos
          (progn
            ;; 現在の設定を保存
            (update-wcl)
            ;; gazonkを先頭に引き出す
            (setq window-configuration-list
                  (nthcdr pos window-configuration-list))
            (message "gazonk again")
            ;; 先頭(gazonk)を現在のwcへセット
            (set-window-configuration (wcc-wc (current-wcc)))
            (funcall #'gazonk))
          (progn
            ;; 新規にwccを作成(cdrにpushされる)
            (push-window-configuration* #'gazonk)
            ;; 順送りでpushしたwccを先頭に
            (next-wcc)
            ;; 中身を詰める
            ;;  gazonkを実行して
            (funcall #'gazonk)
            ;;  その状態でアップデート
            (update-wcl))))))

;; env-log
(define-key global-map [(super shift ?l)]
  #'(lambda ()
    (interactive)
    (let* ((fn '#'env-log)
           (pos (search-wc fn)))
      (if pos
          (progn
            ;; 現在の設定を保存
            (update-wcl)
            ;; gazonkを先頭に引き出す
            (setq window-configuration-list
                  (nthcdr pos window-configuration-list))
            (message "env-log again")
            ;; 先頭(gazonk)を現在のwcへセット
            (set-window-configuration (wcc-wc (current-wcc)))
            (funcall (eval fn)))
          (progn
            ;; 新規にwccを作成(cdrにpushされる)
            (push-window-configuration fn)
            ;; 順送りでpushしたwccを先頭に
            (next-wcc)
            ;; 中身を詰める
            ;;  gazonkを実行して
            (funcall (eval fn))
            ;;  その状態でアップデート
            (update-wcl))))))


(define-key global-map [(super ?l)]
  #'(lambda ()
    (interactive)
    (let* ((fn #'env-slime)
           (pos (search-wc fn)))
      (if pos
          (progn
            ;; 現在の設定を保存
            (update-wcl)
            ;; gazonkを先頭に引き出す
            (setq window-configuration-list
                  (nthcdr pos window-configuration-list))
            (message "env-slime again")
            ;; 先頭を現在のwcへセット
            (set-window-configuration (wcc-wc (current-wcc)))
            ;(funcall #'env-slime 1)
            )
          (progn
            ;; 新規にwccを作成(cdrにpushされる)
            (push-window-configuration fn)
            ;; 順送りでpushしたwccを先頭に
            (next-wcc)
            ;; 中身を詰める
            ;;  gazonkを実行して
            (funcall fn 1)
            ;;  その状態でアップデート
            (update-wcl))))))


(define-key global-map [(super ?d)]
  #'(lambda ()
    (interactive)
    (let* ((fn '#'env-hatena)
           (pos (search-wc fn)))
      (if pos
          (progn
            ;; 現在の設定を保存
            (update-wcl)
            ;; gazonkを先頭に引き出す
            (setq window-configuration-list
                  (nthcdr pos window-configuration-list))
            (message "env-hatena-diary again")
            ;; 先頭を現在のwcへセット
            (set-window-configuration (wcc-wc (current-wcc)))
            (funcall (eval fn)))
          (progn
            ;; 新規にwccを作成(cdrにpushされる)
            (push-window-configuration fn)
            ;; 順送りでpushしたwccを先頭に
            (next-wcc)
            ;; 中身を詰める
            ;;  gazonkを実行して
            (funcall (eval fn))
            ;;  その状態でアップデート
            (update-wcl))))))


;; init
(init-window-configuration-list)

(progn
  ;; keybind
  (define-key global-map [(control meta ?l)] 'cycle-window-configuration-list)
  (define-key global-map [(control meta shift ?i)] 'push-window-configuration)
  (define-key global-map [(control meta ?!)] 'delete-window-configuration) )

(provide 'my-switch)
