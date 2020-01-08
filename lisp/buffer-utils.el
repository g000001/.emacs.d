;;; 同じディレクトリ内で先頭3文字が一致するファイルを友達とみなし、順番に開いたりするもの
;;;
(require 'cl-lib)


(defun get-friend-files ()
  "ファイル名の先頭3文字が合致しているファイルは友達"
  (let ((list (file-name-all-completions (substring (buffer-name) 0 3) 
                                         (file-name-directory (buffer-file-name))))
        (file-name-list '()))
    ;; 自分自身は必ずみつかるので、listの内容が2つ以上なら友達をソートして返す。
    (when (cdr list)
      (while (not (endp list))
        (let ((file (car list)))
          (or (backup-file-name-p file)
              (push file file-name-list)))
        (pop list))
      (sort file-name-list #'string<))))


(defun get-previous-entry-name ()
  (let ((bn (buffer-name))
        (files (get-friend-files))
        (file-name "")) ;番兵)
    (dolist (f files)
      (if (string-lessp f bn)
          (setq file-name f)))
    (if (string= "" file-name)
        nil
        file-name)))


(defun get-next-entry-name ()
  (let ((bn (buffer-name))
        (files (get-friend-files)))
    (dolist (f files)
      (if (string< bn f)
          (return f)))))


(defun cd-to-buffer-directory ()
  (interactive)
  (cd (file-name-directory (buffer-file-name))))


(defun open-previous-entry ()
  (interactive)
  (let ((friends (get-previous-entry-name))
        (default-directory (file-name-directory (buffer-file-name)))) ;find-directory
    (if friends
        (find-file (get-previous-entry-name))
        (message "No previous friends."))))


(defun open-next-entry ()
  (interactive)
  (let ((friends (get-next-entry-name))
        (default-directory (file-name-directory (buffer-file-name)))) ;find-directory
    (cond (friends 
           (find-file (get-next-entry-name)))
          ((y-or-n-p "Make next file?")
           (find-file (next-file-name (buffer-name))))
          ('T (message "No next friends.")))))


(defun buffer-name-prefix ()
  (let ((buffer-name (buffer-name (current-buffer))))
    (substring buffer-name 0 (cl-position-if #'cl-digit-char-p buffer-name)) ))


(defun prefix-Y-m-d.suffix-to-list (string)
  (destructuring-bind (prefix Y m &optional d)
                      (split-string string "-")
    (when (and (null d)
               (cl-every #'cl-digit-char-p prefix) )
      (shiftf d m Y prefix ""))
    (destructuring-bind (d &optional suffix)
                        (split-string d "\\.")
      (list prefix Y m d (or suffix "")) )))


(defun prefix-Y-m-d.suffix-p (string)
  (string-match ".*-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\..*"
                string))


(defun inc-file-num (s)
  (string-match "\\([^0-9]*\\)\\([0-9]+\\)\\(.*\\)" s)
  (let* ((prefix (match-string 1 s))
         (suffix (match-string 3 s))
         (m (match-string 2 s))
         (n (string-to-number m))
         (fig (length m)))
    (incf n)
    (format (format "%%s%%0%dd%%s" fig)
                  prefix
                  n
                  suffix)))


(defun next-file-name (name)
  (if (prefix-Y-m-d.suffix-p name)
      (destructuring-bind (prefix Y m d suffix)
                          (prefix-Y-m-d.suffix-to-list name)
        (let ((time (encode-time 0
                                 0
                                 0
                                 (cl-parse-integer d)
                                 (cl-parse-integer m)
                                 (cl-parse-integer Y)
                                 nil
                                 0) ))
          (concat (if (string= "" prefix) "" (concat prefix "-"))
                  (format-time-string "%Y-%m-%d" (progn (incf (first time)) time))
                  (if (string= "" suffix) "" (concat "." suffix)) )))
      (inc-file-num name)))


(defun make-next-file ()
  (find-file (next-file-name (buffer-name))) )


(defun kill-friend-buffer ()
  (interactive)
  (let* ((bn (buffer-name))
         (ext (file-name-extension bn))
         (head (substring bn 0 4))
         (fri (format "%s.*.%s" head ext)))
    (mapc (lambda (buf)
            (let ((bname (buffer-name buf)))
              (and (not (string= (buffer-name) bname))
                   (string-match fri bname)
                   (kill-buffer buf))))
          (buffer-list)))
  (message "友達を掃除しました"))



;; 
(setq *blog-buffer-bundle* "")


;;;; ansi cl より
;;(defun append1 (lst obj)
;;  (append lst (list obj)))
;;
;;;;; 
;;(defun bundle-buffer ()
;;  (let ((group "blog.*.txt")
;;        (stack))
;;    (mapcar #'(lambda (buffer)          
;;                (and (string-match group (format "%s" buffer))
;;                     (push buffer stack)))
;;            (buffer-list))
;;    stack))
;;
;;(defun bundle-buffer ()
;;  (let* ((bn (buffer-name))
;;         (ext (file-name-extension bn))
;;         (head (substring bn 0 4))
;;         (fri (format "%s.*.%s" head ext)))
;;    (mapcar (lambda (buf)          
;;              (and (string-match fri (buffer-name buf))
;;                   buf))
;;            (buffer-list))))
;;
;;(format "%s" buffer)
;;    
;;    
;;         
;;         
;;
;;
;;(group "blog.*.txt")
;;        (stack))
;;    (mapcar #'(lambda (buffer)          
;;                (and (string-match group (format "%s" buffer))
;;                     (push buffer stack)))
;;            (buffer-list))
;;    stack))
;;
;;
;;(file-name-extension (buffer-name))
;;
;;(defun blog-buffer-bundle-next-buffer ()
;;  (interactive)
;;  (setq *blog-buffer-bundle* (bundle-buffer))
;;  (append1 *blog-buffer-bundle* (car *blog-buffer-bundle*))
;;  (switch-to-buffer (pop *blog-buffer-bundle*)))
;; 

(provide 'buffer-utils)
;;; *EOF*


