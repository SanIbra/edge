;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ilpml\\'" . ilp9-mode))


;Permet de faire le keyBinding 
(defvar ilp-mode-map
  (let ((ilp-mode-map (make-keymap)))
    (define-key ilp-mode-map "\C-j" 'newline-and-indent); la commande CTRL+J va appliquer la fonction newligne-and-indent
    (define-key ilp-mode-map "\C-x\C-a\C-a" 'fold_region); la commande CTRL+X,CTRL+A,CTRL+A va appliquer la fonction fold_region
    ilp-mode-map)
  "Keymap for WPDL major mode")


;Coloration syntaxique
(defvar ilp-font-lock-keywords1
  (list
   ; These define the beginning and end of each ILP entity definition
   '("\\<\\(let\\|if\\|then\\|in\\)\\>" . font-lock-keyword-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   '("[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-variable-name-face)))

(setq ilp-font-lock-keywords ilp-font-lock-keywords1)

(defun  indenttest()
  "fonction de test"
   (interactive)
   (insert (number-to-string (- (ElemInLine "(") (ElemInLine ")")))))

(setq openIdent "\\(let\\|(\\)")
(setq closeIdent ")")


;Indentation automatique
(defun ElemInLine(elem)
"Return number of elem in the current line"
   (setq occ 0)
   (save-excursion
     (end-of-line)
     (setq end (point))
     (beginning-of-line)
     (setq prec (point))
     (re-search-forward elem nil t)
     
     (while (and (<= (point) end) (not (equal prec (point)))  )
       (setq occ (+ occ 1))
       (setq prec (point))
       (re-search-forward elem nil t)
       )	        
     (+ occ 0)))	;juste pour le valeur de retour de la foonction   


(defun  indentnv()
  "Retourne la difference entre le nombres de parenthese ouvrante et fermant"
  (- (ElemInLine openIdent) (ElemInLine closeIdent))
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

	 (if (< cur-indent 0)
	     (indent-line-to 0)
	   (indent-line-to cur-indent))))))

;Commentaire
(defvar ilp-mode-syntax-table
  (let ((ilp-mode-syntax-table (make-syntax-table)))
	
        ;permet l'ajout de _ comment faisant partie d'un mots
	(modify-syntax-entry ?_ "w" ilp-mode-syntax-table)
	
	;Definition des commentaire 
	(modify-syntax-entry ?/ ". 124b" ilp-mode-syntax-table)
	   ; indique que le / fait partie est soit en 1er position ou deuxieme position d'un symbole ouvrant le commentaire(12) soit en 2ème position d'un symbole fermant la parenthèse
	(modify-syntax-entry ?* ". 23" ilp-mode-syntax-table)
	   ; indique que * est soit en deuxime position d'un symbole ouvrant le commmentaire(2) soit en 1er position d'un symbole fermant le commentaire(3)
	(modify-syntax-entry ?\n "> b" ilp-mode-syntax-table)
	  ;indique q'un retour a la ligne signal la fin d'un commentaire ligne
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



(provide 'ilp9-mode)
