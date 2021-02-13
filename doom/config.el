;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq
  display-line-numbers-type 'relative
  doom-font (font-spec :family "Fira Code" :size 16 :weight 'semi-bold)
  doom-theme 'doom-dracula
  doom-themes-treemacs-theme "doom-colors"
  doom-variable-pitch-font (font-spec :family "Fira Code" :size 16 :weight 'semi-bold)
  mac-right-option-modifier nil
  ns-use-native-fullscreen t
  org-agenda-files '("~/files/barischrooneyj@gmail.com/docs/notebook/")
  org-latex-pdf-process
    '("xelatex -shell-escape -interaction nonstopmode %f"
      "bibtex %b"
      "xelatex -shell-escape -interaction nonstopmode %f")
  user-full-name "Jeremy Barisch-Rooney"
  user-mail-address "jerbaroo.work@pm.me"
  zoom-size '(0.618 . 0.618))

(map!
  "C-h" #'evil-window-left
  "C-j" #'evil-window-down
  "C-k" #'evil-window-up
  "C-l" #'evil-window-right)

(after! ivy-posframe
  (setq
    ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
    ivy-posframe-width 200))

;;; config.el ends here
