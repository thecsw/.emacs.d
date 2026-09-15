;;; early-init.el --- machine-specific startup settings -*- lexical-binding: t; -*-

;; Keep platform/toolchain workarounds out of the portable configuration.
;; Emacs loads this file before .emacs, package initialization, and
;; configuration.org, so these variables are available to libgccjit.
(when (eq system-type 'darwin)
  (require 'subr-x)

  ;; Prefer the full Xcode SDK when Xcode is installed.  This avoids stale
  ;; or broken Command Line Tools SDK selection after macOS upgrades.
  (let ((xcode-developer-dir
         "/Applications/Xcode.app/Contents/Developer"))
    (when (file-directory-p xcode-developer-dir)
      (setenv "DEVELOPER_DIR" xcode-developer-dir)))

  ;; Discover the installed SDK rather than hard-coding a macOS version.
  ;; If xcrun cannot find a valid SDK, leave the environment untouched.
  (let ((sdk-path
         (string-trim
          (shell-command-to-string
           "/usr/bin/xcrun --sdk macosx --show-sdk-path 2>/dev/null")))
        (sdk-version
         (string-trim
          (shell-command-to-string
           "/usr/bin/xcrun --sdk macosx --show-sdk-version 2>/dev/null"))))
    (when (and (file-directory-p sdk-path)
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
