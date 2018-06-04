module Mode.EmacsIndent where
indentfunction= ";Indentation automatique\n\
\    (defun  indentnv()\n\
\      \"Retourne la difference entre le nombres de parenthese ouvrante et fermant\"\n\
\      (- (ElemInLine openIdent) (ElemInLine closeIdent))\n\
\      )\n\
\      \n\
\    (defun ElemInLine(elem)\n\
\    \"Return number of elem in the current line\"\n\
\      (setq occ 0)\n\
\      (save-excursion\n\
\        (end-of-line)\n\
\        (setq end (point))\n\
\        (beginning-of-line)\n\
\        (setq prec (point))\n\
\        (re-search-forward elem nil t)\n\
\        \n\
\        \n\
\        (while (and (<= (point) end) (not (equal prec (point)))  )\n\
\          (setq occ (+ occ 1))\n\
\          (setq prec (point))\n\
\          (re-search-forward elem nil t)\n\
\          )\n\        
\        (+ occ 0)))  ;juste pour le valeur de retour de la fonction\n\ 
\ \n\
\    (defun my-indent-line ()\n\
\      \"Fonction responsable de l'indentation\"\n\
\      (interactive)\n\
\      (beginning-of-line)\n\
\      (if (bobp)\n\
\          (indent-line-to 0)\n\
\        (progn\n\
\          \"recheche du niveau d'indentation\"\n\
\          (let ((not-indented t) (cur-indent 0))\n\
\      (save-excursion\n\
\        (while not-indented\n\
\          (forward-line -1)\n\
\          (setq nvid (indentnv))\n\
\          (if (< nvid 0)\n\
\        (progn\n\
\          (setq cur-indent  (- (current-indentation) default-tab-width))\n\
\          (setq not-indented nil))\n\
\          \n\
\            (if (> nvid 0)\n\
\          (progn\n\
\            (setq cur-indent (+ (current-indentation) default-tab-width))\n\
\            (setq not-indented nil))) \n\
\            );fin if\n\
\          (if (bobp)\n\
\        (setq not-indented nil))\n\
\          );fin while\n\
\        );fin save-excursion\n\
\        \n\
\      (if (< cur-indent 0)\n\
\          (indent-line-to 0)\n\
\        (indent-line-to cur-indent))))))\n"
