;;; startup-hooks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (cl-first load-path))))

;;;### (autoloads nil "startup-hooks" "startup-hooks.el" (23980 19891
;;;;;;  6707 502000))
;;; Generated autoloads from startup-hooks.el

(autoload 'any-frame-opened-p "startup-hooks" "\


\(fn)" nil nil)

(autoload 'lotus-general-disable-startup-setting-begin "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-general-disable-startup-setting-finish "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-disable-startup-interrupting-feature "startup-hooks" "\
Run only when emacs start from this file only,
it basically run when this ful get loaded at emacs start time,
its purpose to disable all interrupting feature that may cause
problem while emacs startup in daemon mode, non-interactively.

\(fn)" t nil)

(autoload 'lotus-disable-startup-interrupting-feature-in-frame-once "startup-hooks" "\


\(fn &optional FRAME)" nil nil)

(autoload 'lotus-general-enable-startup-setting-begin "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-general-enable-startup-setting-finish "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-enable-startup-interrupting-feature "startup-hooks" "\
Run only once when when very frame got created after emacs startup.
its purpose to re/enable all feature that may have cuused problem in emacs
startup in daemon mode.

\(fn)" t nil)

(autoload 'lotus-enable-startup-interrupting-feature-in-frame-once "startup-hooks" "\


\(fn FRAME)" nil nil)

(autoload 'add-to-enable-startup-interrupting-feature-hook "startup-hooks" "\
Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here.

\(fn FN &optional APPEND LOCAL)" t nil)

(autoload 'remove-from-enable-startup-interrupting-feature-hook "startup-hooks" "\
Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here.

\(fn FN &optional LOCAL)" t nil)

(autoload 'add-to-disable-startup-interrupting-feature-hook "startup-hooks" "\
Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here.

\(fn FN &optional APPEND LOCAL)" t nil)

(autoload 'remove-from-disable-startup-interrupting-feature-hook "startup-hooks" "\


\(fn FN &optional LOCAL)" t nil)

(autoload 'lotus-general-disable-login-session-setting-begin "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-general-disable-login-session-setting-finish "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-disable-login-session-interrupting-feature "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-disable-login-session-interrupting-feature-in-frame-once "startup-hooks" "\


\(fn F)" nil nil)

(autoload 'lotus-general-enable-login-session-setting-begin "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-general-enable-login-session-setting-finish "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-enable-login-session-interrupting-feature "startup-hooks" "\


\(fn)" t nil)

(autoload 'lotus-enable-login-session-interrupting-feature-in-frame-once "startup-hooks" "\


\(fn FRAME)" nil nil)

(autoload 'add-to-enable-login-session-interrupting-feature-hook "startup-hooks" "\
called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive.

\(fn FN &optional APPEND LOCAL)" t nil)

(autoload 'remove-from-enable-login-session-interrupting-feature-hook "startup-hooks" "\
called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive.

\(fn FN &optional LOCAL)" t nil)

(autoload 'remove-from-disable-login-session-interrupting-feature-hook "startup-hooks" "\


\(fn FN &optional LOCAL)" t nil)

(autoload 'startup-hooks-insinuate "startup-hooks" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("startup-hooks-pkg.el") (23980 19891 2707
;;;;;;  446000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; startup-hooks-autoloads.el ends here
