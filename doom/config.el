;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq display-line-numbers-type 'relative
      doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'semi-bold)
      doom-theme 'doom-dracula
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'semi-bold)
      mac-right-option-modifier nil
      ns-use-native-fullscreen t
      org-directory "~/Google Drive/"
      org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode %f")
      user-full-name "Jeremy Barisch-Rooney"
      user-mail-address "barischrooneyj@protonmail.com"
      ;; zoom-size '(0.618 . 0.618))
      zoom-size '(0.8 . 0.8))

(map! "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right)

(toggle-frame-fullscreen)
