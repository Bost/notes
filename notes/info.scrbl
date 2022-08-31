#lang notes

@block{@block-name{Info}
  info - read Info documents
  Read documentation in Info format.

  [[https://www.youtube.com/watch?v=KDlVGg_VgQc][YouTube: Andrew Tropin - Info and Texinfo]]

  Plain Text:
  #+BEGIN_SRC bash :results output
  emacs -q ~/.guix-home/profile/share/info/bash.info.gz & disown
  #+END_SRC

  #+BEGIN_SRC bash :results output
  emacs -q -e (info-apropos "bash") & disown
  #+END_SRC

  #+BEGIN_SRC bash :results output
  echo $INFOPATH
  #+END_SRC

  The Top-Directory content in Emacs is defined by:
  #+BEGIN_SRC emacs-lisp
  Info-directory-list
  #+END_SRC

  #+RESULTS:
  | /home/bost/.emacs.d/elpa/27.2/develop/writeroom-mode-20210927.1301 | /home/bost/.emacs.d/elpa/27.2/develop/racket-mode-20220109.1535 | /home/bost/.emacs.d/elpa/27.2/develop/php-extras-20160518.1716 | /home/bost/.emacs.d/elpa/27.2/develop/org-roam-20220111.2305 | /home/bost/.emacs.d/elpa/27.2/develop/mmm-mode-20200908.2236 | /home/bost/.emacs.d/elpa/27.2/develop/editorconfig-20210830.1025 | /home/bost/.emacs.d/elpa/27.2/develop/haskell-mode-20210908.1543 | /home/bost/.emacs.d/elpa/27.2/develop/use-package-20210207.1926 | /home/bost/.emacs.d/elpa/27.2/develop/org-20210920 | /home/bost/.emacs.d/elpa/27.2/develop/modus-themes-20220112.1106 | /home/bost/.emacs.d/elpa/27.2/develop/geiser-guile-20220113.2232 | /home/bost/.emacs.d/elpa/27.2/develop/geiser-20211229.1905 | /home/bost/.emacs.d/elpa/27.2/develop/forge-20220112.1745 | /home/bost/.emacs.d/elpa/27.2/develop/magit-20220111.1034 | /home/bost/.emacs.d/elpa/27.2/develop/ghub-20220101.1019 | /home/bost/.emacs.d/elpa/27.2/develop/magit-section-20220101.841 | /home/bost/.emacs.d/elpa/27.2/develop/with-editor-20220107.1056 | /home/bost/.emacs.d/elpa/27.2/develop/annalist-20190929.207 | /home/bost/.emacs.d/elpa/27.2/develop/auctex-13.0.16 | /home/bost/.emacs.d/elpa/27.2/develop/dash-20210826.1149 | /home/bost/.emacs.d/elpa/27.2/develop/transient-20220112.1305 | /home/bost/.emacs.d/elpa/27.2/develop/evil-20220113.2007 | /home/bost/.guix-profile/share/info | /home/bost/.guix-profile/share/info | /home/bost/.config/guix/current/share/info | /run/current-system/profile/share/info | /home/bost/.guix-profile/share/info | /run/current-system/profile/share/info |
}
