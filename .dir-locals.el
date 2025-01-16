;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
((nil . ((eval . (set (make-local-variable 'projectile-project-root)
                      (locate-dominating-file default-directory ".dir-locals.el")))
         ;; (eval . (set (make-local-variable 'python-shell-interpreter-args)
         ;;              (format "--project-directory %s/build exec -T --privileged fortiguest ipython --simple-prompt -i"
         ;;                      (locate-dominating-file default-directory ".dir-locals.el"))))
         ;; (python-shell-interpreter . "docker-compose")
         ;; (eval . (unless (member "docker-compose" python-shell-completion-native-disabled-interpreters)
         ;;           (push "docker-compose" python-shell-completion-native-disabled-interpreters)))
         ;; (counsel-gtags-gtags-extra-update-options-list . "--gtagslabel=pygments")
         ))
 (python-mode . ((comint-process-echoes . t))))
