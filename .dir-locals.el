;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (dante-project-root . "~/Programming/haskell/chip-free")
  (dante-repl-command-line . (("nix-shell" "--run" (concat "cabal new-repl " (or dante-target "") " --builddir=dist/dante")))))))

