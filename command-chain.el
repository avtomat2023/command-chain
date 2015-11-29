;;; Word Chain game by emacs command names.

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar command-chain--player-count)
(defvar command-chain--playing-player)
(defvar command-chain--players)

(defun command-chain--player-set (n key value)
  (let ((player (aref command-chain--players n)))
    (puthash key value player)))

(defun command-chain--player-get (n key)
  (let ((player (aref command-chain--players n)))
    (gethash key player)))

(defun command-chain--config-initialize-variables ()
  (setq command-chain--players (make-vector command-chain--player-count nil))
  (dotimes (n command-chain--player-count)
    (let ((player (aset command-chain--players n (make-hash-table :test 'eq))))
      (puthash 'name (concat "Player " (number-to-string (1+ n))) player))))

;;; c.f. Info widget
(defun command-chain--config ()
  "Create config buffer for command chain game."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Command Chain Config*"))
  (kill-all-local-variables)
  (command-chain--config-initialize-variables)

  (widget-insert "Game Config\n\n")
  (widget-create 'editable-field
                 :size 15
                 :format "Player 1's Name: %v"
                 :notify (lambda (widget &rest ignore)
                           (command-chain--player-set 0 'name (widget-value widget)))
                 (command-chain--player-get 0 'name))
  (widget-insert "\n")
  (widget-create 'editable-field
                 :size 15
                 :format "Player 2's Name: %v"
                 :notify (lambda (widget &rest ignore)
                           (command-chain--player-set 1 'name (widget-value widget)))
                 (command-chain--player-get 1 'name))
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (kill-buffer)
                           (command-chain--start-game))
                 "Start Game")
  (use-local-map widget-keymap)
  (widget-setup))

(defvar command-chain--point-after-prompt 0)
(defvar command-chain--editing nil)

(defmacro command-chain--edit (&rest body)
  (declare (indent 0))
  `(let ((command-chain--editing t))
     ,@body))

(defun command-chain--add-change-hooks ()
  (add-hook 'before-change-functions 'command-chain--before-change nil t)
  (add-hook 'after-change-functions 'command-chain--after-change nil t))

(defun command-chain--before-change (from to)
  (when (and (not command-chain--editing)
             (< from command-chain--point-after-prompt))
    (add-hook 'post-command-hook 'command-chain--add-change-hooks nil t)
    (signal 'text-read-only nil)))

(defun command-chain--after-change (from to old-len)
  (unless command-chain--editing
    (put-text-property from to 'face 'default)))

(defface command-chain--p1-face
  '((t :foreground "deep sky blue"))
  "")

(defun command-chain--put-property (prop value s)
  (put-text-property 0 (length s) prop value s))

(defun command-chain--prompt ()
  (command-chain--edit
    (let* ((name (command-chain--player-get command-chain--playing-player 'name))
           (prompt (concat name "> ")))
      (command-chain--put-property 'face 'command-chain--p1-face prompt)
      (insert prompt)))
  (setq command-chain--point-after-prompt (point-max)))

(defun command-chain--eval-print (input)
  (let* ((content (s-trim input))
         (ok (commandp (intern content))))
    (command-chain--edit
      (insert content ": " (if ok "" "NOT ") "an interactive function\n"))))

(defun command-chain--read-eval-print ()
  (interactive)
  (end-of-buffer)
  (command-chain--edit
    (insert "\n"))
  (let ((input (buffer-substring
                command-chain--point-after-prompt (point-max))))
    (command-chain--eval-print input)
    (setq command-chain--playing-player
          (% (1+ command-chain--playing-player) 2))
    (command-chain--prompt)))

;; beginning-of-lineなどを機能させるには、Fieldを使う
(defun command-chain--start-game ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Command Chain*"))
  (local-set-key (kbd "RET") 'command-chain--read-eval-print)
  (command-chain--add-change-hooks)

  (setq command-chain--playing-player 0)

  (command-chain--prompt))

(defun command-chain (player-count)
  "Play command chain game."
  (interactive "nHow many players: ")
  (when (< player-count 1)
    (error "Number of players must be 1 or more."))
  ;; FIXME
  (setq command-chain--player-count 2)
  (command-chain--config))
