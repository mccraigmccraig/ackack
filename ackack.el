;; mark the output buffer as an ecb compilation buffer
(defun ackack-ecb-compat ( bufname )
  (if (boundp 'ecb-compilation-buffer-names)
      (add-to-list 'ecb-compilation-buffer-names '( bufname ))))

;; return the parent dir of a path
(defun ackack-parent-path ( path )
  (if path
      (let* ((dir (file-name-directory path))
	     (parent (if dir (directory-file-name dir))))
	parent)))

;; split a path into it's components
(defun ackack-split-path ( path )
  (let ((parent (ackack-parent-path path)))
    (if (not (equal path parent))
	(append (ackack-split-path parent) 
		(list (file-name-nondirectory path)))
      (if path (list path)))))

;; # of components in a path
(defun ackack-path-size ( path )
  (length (ackack-split-path path)))

;; return the path if it's an ecb source path, otherwise nil
(defun ackack-ecb-source-path-p ( path )
  (if (boundp 'ecb-source-path)
      (labels ((sptest (item sp) (equal item (directory-file-name (file-truename (first sp))))))
	(let ((source-path (find path ecb-source-path :test #'sptest)))
	  (if source-path (first source-path))))))

;; return true if a path is a root
(defun ackack-root-p ( path )
  (let ((parent (ackack-parent-path path)))
    (equal parent path)))

;; find an ecb-source-path which is a prefix of the given path
(defun ackack-ecb-source-path-for ( path )
  (let ((source-path (ackack-ecb-source-path-p path)))
    (if source-path
	source-path
      (if (not (ackack-root-p path))
	  (ackack-ecb-source-path-for (ackack-parent-path path))))))

;; return first n elements of list l
(defun ackack-first-n ( l n )
  (labels ((first-n-r (l n)
		   (if (> n 0) (cons (first l) (first-n-r (rest l) (- n 1))))))
    (first-n-r l (min n (length l)))))
  
;; turn a list of path components into a path string
(defun ackack-make-path (components)
  (if components
      (directory-file-name (concat (file-name-as-directory (first components)) 
				   (ackack-make-path (rest components))))
    ""))

;; either
;; - nearest prefixing ecb source path plus level or fewer components towards path
;; or
;; - path
(defun ackack-ack-dir ( path level )
  (let* ((ecb-dir (ackack-ecb-source-path-for path))
	 (dir (if (not (ackack-root-p ecb-dir)) ecb-dir path))
	 (dir-components (ackack-split-path dir))
	 (dir-size (length dir-components))
	 (path-components (ackack-split-path path))
	 (path-size (length path-components))
	 (select-size (min (+ dir-size level) path-size))
	 (select-components (ackack-first-n path-components select-size)))
    (ackack-make-path select-components)))
    
(load (concat (file-name-directory load-file-name) "linkify"))
(setq ackack-ack (concat (file-name-directory load-file-name) "ack"))
(setq ackack-linkify-regexps '("^\\([^:]+\\):\\([0-9]+\\):"))

;; run ack n levels down from the ecb source path or in default dir
(defun ackn (pattern n)
  (let ((dir (ackack-ack-dir default-directory n)))
    (ackack-invoke "ackack" ackack-linkify-regexps ackack-ack "--nofilter" pattern dir)))

;; run ack in the ecb-source-path or in default dir
(defun ack (pattern)
  (interactive "sack: ")
  (ackn pattern 0))

;; run ack one level down from ecb source path or in default dir
(defun ack1 (pattern)
  (interactive "sack1: ")
  (ackn pattern 1))

;; run ack two levels down from ecb source path or in default dir
(defun ack2 (pattern)
  (interactive "sack2: ")
  (ackn pattern 2))

(setq mdfind-linkify-regexps '("^\\([^:]+\\):\\([0-9]+\\):"))
(defun mdfn (search n)
  (let ((dir (ackack-ack-dir default-directory n)))
    (ackack-invoke "mdfind" mdfind-linkify-regexps "mdfind" "-onlyin" dir (format "kMDItemFSName == '%s'" search))))

(defun mdf (search)
  (interactive "smdf: ")
  (mdfn search 0))

(defun mdf1 (search)
  (interactive "smdf1: ")
  (mdfn search 1))

(defun mdf2 (search)
  (interactive "smdf2: ")
  (mdfn search 2))


(defun ackack-invoke (cmd-name linkify-relist cmd &rest args)
  (let ((buffer-name (format "*%s*" cmd-name)))
    (ackack-ecb-compat buffer-name)
    
    (lexical-let ((cmdname cmd-name)
		  (ackack-results (get-buffer-create buffer-name))
		  (startwin (selected-window)))
      (labels ((ackack-scroll-to-end-of-results (proc state)
						(save-excursion
						  (let ((curwin (selected-window)))
						    (select-window (display-buffer ackack-results) t)
						    (let ((resultswin (selected-window)))
						      (goto-char (point-max))
						      (insert (format "\n%s finished" cmdname))
						      (if (eql curwin resultswin)
							  (select-window startwin)
							(select-window curwin)))))))
	(save-excursion
	  (set-buffer ackack-results)
	  (erase-buffer)
	  (buffer-disable-undo)
	  (insert (format "%s %S\n\n" cmd args))
	  (setq linkify-regexps linkify-relist))
	
	(setq proc (apply #'start-process cmd-name ackack-results cmd args))
	(set-process-filter proc 'linkify-filter)
	(set-process-sentinel proc #'ackack-scroll-to-end-of-results)
	
	(select-window (display-buffer ackack-results))
	(goto-char (point-max))))))
 
(provide 'ackack)
