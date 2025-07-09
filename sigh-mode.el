;;; sigh-mode.el --- Sentence highlighting -*- lexical-binding: t -*-

;; Author: Gabor Nyeki <gabor.nyeki@alumni.duke.edu>
;; Maintainer: Gabor Nyeki <gabor.nyeki@alumni.duke.edu>
;; URL: https://github.com/gn0/sigh-mode/
;; Version: 1.2
;; Package-requires: ((emacs "24.1"))

;;; License:

;; SPDX-License-Identifier: CC0-1.0

;;; Code:

(defvar-keymap sigh-map
  :doc "Keymap for `sigh-mode'."
  "h" 'backward-sentence
  "l" 'forward-sentence
  "<left>" 'backward-sentence
  "<right>" 'forward-sentence
  "j" 'next-line
  "k" 'previous-line)

(defvar sigh--overlay nil
  "Overlay used to highlight the current sentence.")

(defvar sigh--buffer-was-read-only nil
  "Buffer was read-only before activating `sigh-mode'.")

(define-minor-mode sigh-mode
  "Toggle sentence highlighting and sentence navigation in the current buffer.
Keybindings:
- `h' and <left>: move backward by sentence.
- `l' and <right>: move forward by sentence.
- `j' and <down>: move down by line.
- `k' and <up>: move up by line."
  :lighter " Sigh"
  :keymap sigh-map
  (if (bound-and-true-p sigh-mode)
      (progn
        ;; Switch buffer to read-only mode.
        (setq sigh--buffer-was-read-only buffer-read-only)
        (when (not buffer-read-only)
          (read-only-mode 1))
        ;; Add hook for highlighting.
        (add-hook
         'post-command-hook 'sigh-highlight-sentence-at-point nil t)
        ;; Keybindings for Evil's normal state.
        (when (and (require 'evil nil t) (bound-and-true-p evil-mode))
          (progn
            (evil-define-key* 'normal sigh-map
              "h" 'evil-backward-sentence-begin
              "l" 'evil-forward-sentence-begin
              (kbd "<left>") 'evil-backward-sentence-begin
              (kbd "<right>") 'evil-forward-sentence-begin
              "j" 'next-line
              "k" 'previous-line)
            ;; Prompt Evil to register the new keybindings.  Otherwise
            ;; the user would need to initiate an Evil state transition
            ;; for the keybindings to take effect, e.g., switch to Emacs
            ;; state and back by pressing C-z twice.  Related issue:
            ;; https://github.com/emacs-evil/evil/issues/301.
            (evil-normalize-keymaps))))
    (progn
      (when (not sigh--buffer-was-read-only)
        (read-only-mode -1))
      (setq sigh--buffer-was-read-only nil)
      (remove-hook
       'post-command-hook 'sigh-highlight-sentence-at-point t)
      (when sigh--overlay
        (delete-overlay sigh--overlay)
        (setq sigh--overlay nil)))))

(defun sigh-highlight-sentence-at-point ()
  "Highlight the current sentence using an overlay."
  (when sigh--overlay
    (delete-overlay sigh--overlay))
  (let* ((start (save-excursion
                  (forward-sentence 1) (backward-sentence 1) (point)))
         (end (save-excursion (forward-sentence 1) (point))))
    (setq sigh--overlay (make-overlay start end))
    (overlay-put sigh--overlay 'face 'highlight)))

(provide 'sigh-mode)
;;; sigh-mode.el ends here
