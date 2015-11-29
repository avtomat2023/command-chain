;;; Word Chain game by emacs command names.

(require 'widget)
(require 's)

(eval-when-compile
  (require 'cl))

;; Faces

;; FIXME: change font according to player
(defface command-chain-prompt-face
  '((t :foreground "deep sky blue"))
  "Prompt's face."
  :group 'command-chain)

(defface command-chain-commited-input-face
  '((t :weight bold))
  "Commited input's face."
  :group 'command-chain)

;; Configurable Variables

(defvar command-chain-player-count nil "Number of players.")
(defvar command-chain-players nil
  "Hash tables cotaining players' status.

Keys (must be symbols) and values are following:
  name : player's name (string)

This variables shold be accessed not directly but via functions
`command-chain-player-set' and `command-chain-player-get'
for future implementation change.")

;; Utilities

(defun command-chain-player-set (n key value)
  "Set KEY of N th player in `command-chain-players' to VALUE."
  (let ((player (aref command-chain-players n)))
    (puthash key value player)))

(defun command-chain-player-get (n key)
  "Get a value associated with KEY for N th player in `command-chain-players'."
  (let ((player (aref command-chain-players n)))
    (gethash key player)))

(defun command-chain--put-property (prop value s)
  "Put text propery to whole the S."
  (put-text-property 0 (length s) prop value s))

(defmacro command-chain--make-local-variables (&rest vars)
  "Shorthand of `make-local-variable' for multiple variables.
VARS must not be quoted."
  (declare (indent 0))
  (let ((sexps (mapcar (lambda (var)
                         `(make-local-variable (quote ,var)))
                       vars)))
    (cons 'progn sexps)))

;; Definitions for config buffer

(defun command-chain-config-initialize-variables ()
  "Set configurable variables' initial values."
  (setq command-chain-players (make-vector command-chain-player-count nil))
  (dotimes (i command-chain-player-count)
    (let ((player (aset command-chain-players i (make-hash-table :test 'eq))))
      (puthash 'name (concat "Player " (number-to-string (1+ i))) player))))

(defun command-chain-config-create-player-widgets (player-n)
  "Create widgets to configure PLAYER-N th player's information."
  (widget-insert (concat "Player " (number-to-string (1+ player-n)) ":\n"))
  (widget-create 'editable-field
                 :size 12
                 :format (concat "    Name: %v\n")
                 :notify (lexical-let ((n player-n))
                           (lambda (widget &rest ignore)
                             (command-chain-player-set
                              n 'name (widget-value widget))))
                 (command-chain-player-get player-n 'name))
  (widget-insert "\n"))

;;; c.f. Info widget
(defun command-chain-config ()
  "Create config buffer."
  (interactive)
  (switch-to-buffer "*Command Chain Config*")
  (command-chain-config-initialize-variables)

  (kill-all-local-variables)
  (widget-insert "*** Game Config ***\n\n")
  (dotimes (i command-chain-player-count)
    (command-chain-config-create-player-widgets i))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (kill-buffer)
                           (command-chain-start-game))
                 "Start Game")
  (use-local-map widget-keymap)
  (widget-setup))

;; Game Variables

(defvar command-chain-current-player 0
  "Number representing whose turn the game is.")
(defvar command-chain-point-after-prompt 0
  "Point after prompt. Buffer content before this point must not be changed.")
(defvar command-chain-editing nil
  "Boolean indicating if buffer will be changed by private functions.
If non-nil, hooks for player's buffer editting get disabled.")

;; Game Utilities

(defmacro command-chain-edit (&rest body)
  "Macro to change buffer.
Example:
    (command-chain-edit
       (insert result)
       (insert prompt))"
  (declare (indent 0))
  `(let ((command-chain-editing t))
     ,@body))

(defun command-chain-insert (&rest args)
  "Shorthand for `insert'."
  (command-chain-edit
    (apply 'insert args)))

(defun command-chain-pass-turn-to-next-player ()
  "Set `command-chain-current-player' to the next player."
  (setq command-chain-current-player
        (% (1+ command-chain-current-player) command-chain-player-count)))

(defun command-chain-current-player-get (key)
  "Shorthand of `command-chain-player-get' to `command-chain-current-player'."
  (command-chain-player-get command-chain-current-player key))

;; game Functions

(defun command-chain-add-change-hooks ()
  "Add hooks to `before-change-functions' and `after-change-functions'."
  (add-hook 'before-change-functions 'command-chain-before-change nil t)
  (add-hook 'after-change-functions 'command-chain-after-change nil t))

(defun command-chain-before-change (from to)
  ;; Discard change if output gets rewritten
  (when (and (not command-chain-editing)
             (< from command-chain-point-after-prompt))
    ;; Hook functions seem to be removed when `signal'ed.
    ;; So `add-hook' again.
    (add-hook 'post-command-hook 'command-chain-add-change-hooks nil t)
    (signal 'text-read-only nil)))

(defun command-chain-after-change (from to old-len)
  ;; Face of the string inserted by players may be set to prompt's one.
  ;; So reset the face.
  (unless command-chain-editing
    (put-text-property from to 'face 'default)))

(defun command-chain-prompt ()
  "Print a prompt."
  (let* ((name (command-chain-current-player-get 'name))
         (prompt (concat name "> ")))
    (command-chain--put-property 'face 'command-chain-prompt-face prompt)
    (command-chain-insert prompt))
  (setq command-chain-point-after-prompt (point-max)))

(defun command-chain-process-input (input)
  (let* ((content (s-trim input))
         (ok (commandp (intern content))))
    (command-chain-insert
     content ": " (if ok "" "NOT ") "an interactive function\n")))

(defun command-chain-commit-input ()
  "Commit player's input and prompt next input."
  (interactive)
  (end-of-buffer)
  (command-chain-insert "\n")
  (let ((input (buffer-substring
                command-chain-point-after-prompt (point-max))))
    (command-chain-edit
      (put-text-property command-chain-point-after-prompt (point-max)
                         'face 'command-chain-commited-input-face))
    (command-chain-process-input input)
    (command-chain-pass-turn-to-next-player)
    (command-chain-prompt)))

;; beginning-of-lineなどを機能させるには、Fieldを使う
(defun command-chain-start-game ()
  "Create game buffer and start game."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Command Chain*"))
  (command-chain--make-local-variables
    command-chain-player-count
    command-chain-players
    command-chain-current-player
    command-chain-point-after-prompt
    command-chain-editing)
  (local-set-key (kbd "RET") 'command-chain-commit-input)
  (command-chain-add-change-hooks)
  (setq command-chain-current-player 0
        command-chain-point-after-prompt 0)
  (command-chain-prompt))

(defun command-chain (player-count)
  "Play command chain game, that is, word chain by Emacs commands."
  ;; FIXME: Number of players must be specified in cofig buffer
  ;;        because there is no chance to open multiple config buffers.
  (interactive "nHow many players: ")
  (when (< player-count 1)
    (error "Number of players must be 1 or more."))
  ;; FIXME
  (setq command-chain-player-count 2)
  (command-chain-config))
