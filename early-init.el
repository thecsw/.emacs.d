;;; early-init.el --- machine-specific startup settings -*- lexical-binding: t; -*-

;; Keep platform/toolchain workarounds out of the portable configuration.
;; Emacs loads this file before .emacs, package initialization, and
;; configuration.org, so these variables are available to libgccjit.
(when (eq system-type 'darwin)
  ;; Prefer the full Xcode SDK when Xcode is installed.  This avoids stale
  ;; or broken Command Line Tools SDK selection after macOS upgrades.
  (let ((xcode-developer-dir
         "/Applications/Xcode.app/Contents/Developer"))
    (when (file-directory-p xcode-developer-dir)
      (setenv "DEVELOPER_DIR" xcode-developer-dir)))

  ;; Discover the installed SDK rather than hard-coding a macOS version.
  ;; If xcrun cannot find a valid SDK, leave the environment untouched.
  ;; Invoke xcrun directly to avoid starting the user's shell twice.
  (let ((sdk-path
         (ignore-errors
           (car (process-lines "/usr/bin/xcrun" "--sdk" "macosx"
                               "--show-sdk-path"))))
        (sdk-version
         (ignore-errors
           (car (process-lines "/usr/bin/xcrun" "--sdk" "macosx"
                               "--show-sdk-version")))))
    (when (and (stringp sdk-path)
               (file-directory-p sdk-path)
               (stringp sdk-version)
               (string-match-p
                "\\`[0-9]+\\(?:\\.[0-9]+\\)*\\'" sdk-version))
      (setenv "SDKROOT" sdk-path)
      (setenv "MACOSX_DEPLOYMENT_TARGET" sdk-version))))

;; Third-party packages commonly emit harmless native-compiler warnings
;; about optional or dynamically loaded functions. Keep those warnings
;; available to Emacs' warning machinery without displaying them at startup.
(with-eval-after-load 'comp-run
  (setq native-comp-async-report-warnings-errors 'silent))

(provide 'early-init)
;;; early-init.el ends here
