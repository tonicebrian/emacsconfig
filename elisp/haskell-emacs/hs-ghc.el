(defun hs-ghc-version ()
  "Get the version of the GHC we're using."
  (let* ((out (shell-command-to-string (format "%s --version" hs-config-ghci-bin)))
         (match (string-match "version \\([0-9\.]+\\)$" out)))
    (when match
      (match-string 1 out))))

(provide 'hs-ghc)