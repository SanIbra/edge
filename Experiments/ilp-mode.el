;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ilpml\\'" . ilp9-mode))


(defvar ilp-mode-map
  (let ((ilp-mode-map (make-keymap)))
    (define-key ilp-mode-map "\C-j" 'newline-and-indent)
    ilp-mode-map)
  "Keymap for WPDL major mode")


;Coloration syntaxique
(defconst ilp-font-lock-keywords1
  (list
   ; These define the beginning and end of each ILP entity definition
   '("\\<\\(let\\|if\\|then\\|in\\)\\>" . font-lock-keyword-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   '("[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-variable-name-face)))

(setq ilp-font-lock-keywords ilp-font-lock-keywords1)

;Indentation automatique
(defun ElemInLine(elem)
"Return number of elem in the current line"
   (setq occ 0)
   (save-excursion
     (end-of-line)
     (setq end (point))
     (beginning-of-line)
     (search-forward elem)
     
     (while (<= (point) end)
       (setq occ (+ occ 1))
       (search-forward elem)
       )	        
     (+ occ 0)))	;juste pour le valeur de retour de la foonction   


(defun ElemIn (elem start end)
  "Compte le nombre d'element entre start et end"
    (setq occ 0)
   (save-excursion
     (goto start)
     (search-forward elem)
     
     (while (<= (point) end)
       (setq occ (+ occ 1))
       (search-forward elem)
       )	        
     (+ occ 0)))

(defun  indentnv()
  "Retourne la difference enntre le nombres de parenthese ouvrante et fermant"
  (- (ElemInLine "(") (ElemInLine ")"))
  )

(defun  indenttest()
  "fonction de test"
   (interactive)
   (insert (number-to-string (- (ElemInLine "(") (ElemInLine ")"))))
  )
(defun ilp-indent-line ()
  "Fonction responsable de l'indentation"
   (interactive)
   (beginning-of-line)
   (if (bobp)
       (indent-line-to 0)
     (progn
       "recheche du niveau d'indentation"
       (let ((not-indented t) (cur-indent 0))
	 (save-excursion
	   (while not-indented
	     (forward-line -1)
	     (setq nvid (indentnv))
	     (if (< nvid 0)
		 (progn
		   (setq cur-indent  (- (current-indentation) default-tab-width))
		   (setq not-indented nil))
	       
	       (if (> nvid 0)
		   (progn
		     (setq cur-indent (+ (current-indentation) default-tab-width))
		     (setq not-indented nil))) 
	       );fin if
	     (if (bobp)
		 (setq not-indented nil))
	     );fin while
	   );fin save-excursion

	 "cas ou il y a une parenthese fermante sur la ligne"
					; (if (< (indentnv) 0)
					;  (setq cur-indent (- cur-indent default-tab-width))
	 (save-excursion
	   (beginning-of-line)
	   (if (looking-at "^\s*)$")
	       (progn
		 (setq cur-indent (- cur-indent default-tab-width)))))
	 
	 (if (< cur-indent 0)
	     (indent-line-to 0)
	   (indent-line-to cur-indent))))))

;Commentaire
(defvar ilp-mode-syntax-table
  (let ((ilp-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" ilp-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?/ ". 124b" ilp-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" ilp-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" ilp-mode-syntax-table)
	ilp-mode-syntax-table)
  "Syntax table for wpdl-mode")

(defun ilp9-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map ilp-mode-map)
  (set-syntax-table ilp-mode-syntax-table)
  ;; Set up font-lock
  (set  (make-local-variable 'font-lock-defaults) '(ilp-font-lock-keywords))
   ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'ilp-indent-line)  
  (setq major-mode 'ilp-mode)
  (setq mode-name "ILP")
  (run-hooks 'ilp-mode-hook)
  )


;; Code folding

(defun ElemIn (elem start end)
  "retourne le nombre d'elem entre start et end"
    (setq occ 0)
   (save-excursion
     (goto-char start)
     (search-forward elem)
     
     (while (<= (point) end)
       (setq occ (+ occ 1))
       (search-forward elem)
       )	        
     (+ occ 0)))


(defun hide_region(depart arrive)
  "fold the region start to DEPART and end to ARRIVE"
  (setq ov (make-overlay depart arrive))
  (overlay-put ov 'invisible t)
  (overlay-put ov 'owner 'ilp_mode))

(defun shown_region(depart arrive)
  "show all folding region who's between depart and arrive"
  (remove-overlays depart arrive 'owner 'ilp_mode)
  )

(defun fold_region()
  "Cache la prochaine region"
  (interactive)
  (setq depart 0)
  (setq arrive 0)
  (save-excursion
    (beginning-of-line)
    (if (search-forward "(" nil t)
	(progn
	  (setq depart (point))
	  (if (search-forward ")" nil t)
	      (setq arrive (point))))))
	  
  (unless (eq depart arrive)
      (unless (eq arrive 0)
	(hide_region depart arrive)))
  (+ arrive 0))

(defun fermante (start end)
  "Retourn la difference en le nombre de parenthese ouvrante et fermante. Si cette fonction retourne 0 la parenthese fermante a bien ete trouve"
  (- (ElemIn "(" start end) (ElemIn ")" start end)))


(defun show_all()
  "decouvre toutes les regions cachees"
  (interactive)
  (shown_region (point-min) (point-max)))


(defun show_next()
  "decouvre la prochaine region cache"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq start (point)))
  (delete-overlay (car (overlays-in start (point-max)))))


(defun fold_region()
  "cache la prochaine zone"
  (interactive)
  (setq depart 0)
  (setq arrive 0)
  (save-excursion
    (beginning-of-line)
    (setq begin (point))
    (when (search-forward "(" nil t)
      (setq depart (point))
      (let ((bool 1) (fini t))
	(while fini
	  (search-forward ")" nil t)
	  (if (eq (fermante begin (point)) 0)
	      (progn
		(setq arrive (point))
		(setq fini nil))
	    (setq bool 0));traitement de la parenthese fermante
	  
	  (if (bobp)
	      (setq fini nil)))))) ;fin while et let et when et save excursion
  
  (unless (eq depart arrive)
    (unless (eq arrive 0)
      (hide_region depart arrive)))
  (+ arrive 0))


(provide 'ilp9-mode)
