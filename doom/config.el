;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq display-line-numbers-type 'relative
      doom-font (font-spec :family "Iosevka" :size 24 :weight 'semi-bold)
      doom-theme 'doom-dracula
      doom-themes-treemacs-theme "doom-colors"
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 24 :weight 'semi-bold)
      mac-right-option-modifier nil
      ns-use-native-fullscreen t
      org-directory "~/Google Drive/"
      org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode %f")
      user-full-name "Jeremy Barisch-Rooney"
      user-mail-address "jerbaroo.work@pm.me"
      zoom-size '(0.618 . 0.618)
      )

(map! "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right
      )
