
;;;BEGIN maxima addition
(autoload 'dbl "dbl" "Make a debugger to run lisp, maxima and or gdb in" t)
(autoload 'maxima-mode "maxima-mode" "Major mode for editing maxima code and interacting with debugger" t)
(autoload 'gcl-mode "gcl" "Major mode for editing maxima code and interacting with debugger" t)
(setq auto-mode-alist (cons '("\\.ma?[cx]\\'" . maxima-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.li?sp\\'" . gcl-mode) auto-mode-alist))
;;;END maxima addition
