;; Fichier compile.lsp
(proclaim '(optimize (safety 0)))
;       ***************************************************************
;       *                    MODULE SYM                               *
;       *       MANIPULATIONS DE FONCTIONS SYMETRIQUES                *
;       *        (version01: Commonlisp pour Maxima)                 *
;       *                                                             *
;       *                ----------------------                       *
;       *                  Annick VALIBOUZE                           *
;       *                    GDR MEDICIS                              *
;       *  (Mathe'matiques Effectives, De'veloppements Informatiques, *
;       *           Calculs et Ingenierie, Syste`mes)                 *
;       *             LITP (Equipe Calcul Formel)                     *
;       *                 Universite' Paris 6,                        *
;       *        4 place Jussieu, 75252 Paris cedex 05.               *
;       *              e-mail : avb@sysal.ibp.fr                      *
;       ***************************************************************

(compile-file '|arite.lsp|)
(compile-file '|chbase.lsp|) 
(compile-file '|direct.lsp|)  
(compile-file '|elem.lsp|)
(compile-file '|kak.lsp|)
(compile-file '|ecrivain.lsp|)
(compile-file '|lecteur.lsp|)
(compile-file '|macros.lsp|)
(compile-file '|multmon.lsp|)
(compile-file '|operations.lsp|) 
(compile-file '|partpol.lsp|) 
(compile-file '|permut.lsp|)
(compile-file '|pui.lsp|)
(compile-file '|resolvante.lsp|)  
(compile-file '|schur.lsp|)
(compile-file '|treillis.lsp|) 
(compile-file '|tri.lsp|)  
(compile-file '|util.lsp|)  

