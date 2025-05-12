;;; sigh-mode.el --- Sentence highlighting -*- lexical-binding: t -*-

;; Author: Gabor Nyeki <gabor.nyeki@alumni.duke.edu>
;; Maintainer: Gabor Nyeki <gabor.nyeki@alumni.duke.edu>
;; URL: https://github.com/gn0/sigh-mode/
;; Version: 1.0
;; Package-requires: ((emacs "24.1"))

;;; License:

;; SPDX-License-Identifier: CC0-1.0

;;; Code:

(defvar sigh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") 'backward-sentence)
    (define-key map (kbd "l") 'forward-sentence)
    (define-key map (kbd "<left>") 'backward-sentence)
    (define-key map (kbd "<right>") 'forward-sentence)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "<down>") 'next-line)
    (define-key map (kbd "<up>") 'previous-line)
    map)
  "Keymap for `sigh-mode'.")

(defvar sigh-overlay nil
  "Overlay used to highlight the current sentence.")

(define-minor-mode sigh-mode
  "Minor mode to navigate the buffer sentence-by-sentence and
dynamically highlight the current one.  Keybindings:
- `h' and <left>: move backward by sentence.
- `l' and <right>: move forward by sentence.
- `j' and <down>: move down by line.
- `k' and <up>: move up by line."
  :lighter " Sigh"
  :keymap sigh-mode-map
  (if (bound-and-true-p sigh-mode)
      (progn
        ;; Add hook for highlighting.
        (add-hook
         'post-command-hook 'sigh-highlight-sentence-at-point nil t)
        ;; Keybindings for Evil's normal state.
        (when (bound-and-true-p evil-mode)
          (evil-define-key 'normal sigh-mode-map
            "h" 'evil-backward-sentence-begin
            "l" 'evil-forward-sentence-begin
            (kbd "<left>") 'evil-backward-sentence-begin
            (kbd "<right>") 'evil-forward-sentence-begin
            "j" 'next-line
            "k" 'previous-line)))
    (progn
      (remove-hook
       'post-command-hook 'sigh-highlight-sentence-at-point t)
      (when sigh-overlay
        (delete-overlay sigh-overlay)
        (setq sigh-overlay nil)))))

(defun sigh-highlight-sentence-at-point ()
  "Highlight the current sentence using an overlay."
  (when sigh-overlay
    (delete-overlay sigh-overlay))
  (let* ((start (save-excursion
                  (forward-sentence 1) (backward-sentence 1) (point)))
         (end (save-excursion (forward-sentence 1) (point))))
    (setq sigh-overlay (make-overlay start end))
    (overlay-put sigh-overlay 'face 'highlight)))

(provide 'sigh-mode)
;;; sigh-mode.el ends here
