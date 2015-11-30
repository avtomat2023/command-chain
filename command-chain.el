;;; Word Chain game by emacs command names.

(require 'widget)

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

;; Contants and Variables for Config

(defconst command-chain-config-buffer-name "*Command Chain Config*")
(defvar command-chain-players nil
  "Hash tables cotaining players' status.

Keys (must be symbols) and values are following:
  name : player's name (string)
  life : life point (integer)

This variable shold be accessed not directly but via functions
`command-chain-initialize-players', `command-chain-player-set',
`command-chain-player-get' and `command-chain-delete-player'
for future implementation changes.")

;; Utilities

(defun command-chain-player-count ()
  "Number of players."
  (length command-chain-players))

(defun command-chain-player-set (player-n key value)
  "Set KEY of N th player in `command-chain-players' to VALUE."
  (let ((player (aref command-chain-players player-n)))
    (puthash key value player)))

(defun command-chain-player-get (player-n key)
  "Get a value associated with KEY for N th player in `command-chain-players'."
  (let ((player (aref command-chain-players player-n)))
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

(defun command-chain--vector-remove-nth (vector n)
  "Return a new vector made by removing N th element of VECTOR."
  (let* ((len (length vector))
         (new-vec (make-vector (1- len) nil)))
    (dotimes (i n)
      (aset new-vec i (aref vector i)))
    (cl-loop for i from (1+ n) below len do
             (aset new-vec (1- i) (aref vector i)))
    new-vec))

(defmacro command-chain--vector-delete-nth (vector n)
  "Delete N th element of VECTOR destructively."
  `(setq ,vector (command-chain--vector-remove-nth ,vector ,n)))

(defun command-chain-delete-player (player-n)
  "Delete PLAYER-N th player."
  (command-chain--vector-delete-nth command-chain-players player-n))

(defun command-chain--number-vector (from &optional to inc)
  (vconcat (number-sequence from to inc)))

(defun command-chain--s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (when (string-match "\\`[ \t\n\r]+" s)
    (setq s (replace-match "" t t s)))
  (when (string-match "[ \t\n\r]+\\'" s)
    (setq s (replace-match "" t t s)))
  s)

;; Definitions for config buffer

(defun command-chain-initialize-players (player-count)
  "Set `command-chain-players' to default PLAYER-COUNT players and
`command-chain-player-count' to PLAYER-COUNT."
  (setq command-chain-players (make-vector player-count nil))
  (dotimes (i player-count)
    (let ((player (aset command-chain-players i (make-hash-table :test 'eq))))
      (puthash 'name (concat "Player " (number-to-string (1+ i))) player)
      (puthash 'life 2 player))))

(defun command-chain-config-delete-player (player-n)
  (when (<= (command-chain-player-count) 2)
    (error "2 or more players needed."))
  (command-chain--vector-delete-nth command-chain-players player-n)
  (kill-buffer)
  (command-chain-config))

(defun command-chain-config-create-player-widgets (player-n)
  "Create widgets to configure PLAYER-N th player's information."
  (widget-create 'push-button
                 :notify (lexical-let ((n player-n))
                           (lambda (&rest ignore)
                             (command-chain-config-delete-player n)))
                 "Delete Player")
  (widget-insert "\n")
  (widget-insert (concat "Player " (number-to-string (1+ player-n)) ":\n"))
  (widget-create 'editable-field
                 :size 12
                 :format (concat "    Name %v\n")
                 :notify (lexical-let ((n player-n))
                           (lambda (widget &rest ignore)
                             (command-chain-player-set
                              n 'name (widget-value widget))))
                 (command-chain-player-get player-n 'name))
  (widget-insert "\n"))

;;; c.f. Info widget
(defun command-chain-config ()
  "Create config buffer."
  (switch-to-buffer command-chain-config-buffer-name)
  (kill-all-local-variables)
  (widget-insert "*** Game Config ***\n\n")
  (dotimes (i (command-chain-player-count))
    (command-chain-config-create-player-widgets i))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (kill-buffer)
                           (command-chain-start-game))
                 "Start Game")
  (use-local-map widget-keymap)
  (widget-setup))

;; Game Variables

(defvar command-chain-player-ns nil "Numbers of living players.")
(defvar command-chain-current-player-index 0
  "Number representing whose turn the game is. This value is associated to
`command-chain-player-ns', not `command-chain-players'.")
(defvar command-chain-point-after-prompt 0
  "Point after prompt. Buffer content before this point must not be changed.")
(defvar command-chain-editing nil
  "Boolean indicating if buffer will be changed by private functions.
If non-nil, hooks for player's buffer editting get disabled.")
(defvar command-chain-char ?a
  "Character the next command should begins with.
The value is lowercase, but uppercase character is also accepted.")

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
  "Set `command-chain-current-player-index' to the next player."
  (setq command-chain-current-player-index
        (% (1+ command-chain-current-player-index)
           (length command-chain-player-ns))))

(defun command-chain-current-player-get (key)
  "Shorthand of `command-chain-player-get' to the current player."
  (command-chain-player-get
   (aref command-chain-player-ns command-chain-current-player-index) key))

(defun command-chain-current-player-set (key value)
  "Shorthand of `command-chain-player-set' to the current player."
  (command-chain-player-set
   (aref command-chain-player-ns command-chain-current-player-index)
   key value))

(defun command-chain-first-char (s)
  "Return the last char of S downcased. S must not be null string."
  (downcase (elt s 0)))

(defun command-chain-last-char (s)
  "Return the last char of S downcased. S must not be null string."
  (downcase (elt s (1- (length s)))))

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
  (command-chain-insert "Next character: " command-chain-char ?\n)
  (let* ((name (command-chain-current-player-get 'name))
         (n (1+ (aref command-chain-player-ns
                      command-chain-current-player-index)))
         (life (command-chain-current-player-get 'life))
         (prompt (concat (number-to-string n) " - " name
                         " (life: " (number-to-string life) ")> ")))
    (command-chain--put-property 'face 'command-chain-prompt-face prompt)
    (command-chain-insert prompt))
  (setq command-chain-point-after-prompt (point-max)))

(defun command-chain-damage-current-player (damage)
  "Decrease current player's life by DAMAGE. Process player's death if life <= 0."
  (let ((life (command-chain-current-player-get 'life))
        (name (command-chain-current-player-get 'name))
        (s? (if (> damage 1) "s" "")))
    (command-chain-insert name " takes " (number-to-string damage)
                          " point" s? " of damage.\n")
    (cl-decf life damage)
    (command-chain-current-player-set 'life life)
    (when (<= life 0)
      (command-chain-insert name " is DEAD.\n")
      (command-chain--vector-delete-nth command-chain-player-ns
                                        command-chain-current-player-index)
      (cl-decf command-chain-current-player-index))))

(defun command-chain-process-input (input)
  (let ((command (command-chain--s-trim input)))
    (cond ((or (eq (length command) 0)
               (not (eq (command-chain-first-char command) command-chain-char)))
           (command-chain-insert "You are a rule breaker.\n")
           (command-chain-damage-current-player 2))
          ((not (commandp (intern command)))
           (command-chain-insert
            ?\` command ?\' " is not an interactive function.\n")
           (command-chain-damage-current-player 1))
          (t
           (setq command-chain-char (command-chain-last-char command))))))

(defun command-chain-deactivate-game ()
  "Deactivate all commands."
  (local-unset-key (kbd "RET"))
  (setq command-chain-point-after-prompt (1+ (point-max))))

(defun command-chain-win ()
  "If only 1 player is alive, win the player and return non-nil.
Ohterwise, do nothing and return nil."
  (when (eq (length command-chain-player-ns) 1)
    (let ((name (command-chain-current-player-get 'name)))
      (command-chain-insert name " won the game.")
      (command-chain-deactivate-game)
      t)))

(defun command-chain-commit-input ()
  "Commit player's input and prompt next input."
  (interactive)
  (goto-char (point-max))
  (command-chain-insert "\n")
  (let ((input (buffer-substring
                command-chain-point-after-prompt (point-max))))
    (command-chain-edit
      (put-text-property command-chain-point-after-prompt (point-max)
                         'face 'command-chain-commited-input-face))
    (command-chain-process-input input)
    (command-chain-pass-turn-to-next-player)
    (or (command-chain-win)
        (command-chain-prompt))))

;; FIXME: Use text's 'field' to facilitate move commands like `beginning-of-line'.
(defun command-chain-start-game ()
  "Create game buffer and start game."
  (switch-to-buffer (generate-new-buffer "*Command Chain*"))
  (command-chain--make-local-variables
    command-chain-players
    command-chain-player-ns
    command-chain-current-player-index
    command-chain-point-after-prompt
    command-chain-editing
    command-chain-char)
  (local-set-key (kbd "RET") 'command-chain-commit-input)
  (command-chain-add-change-hooks)

  (setq command-chain-player-ns
        (command-chain--number-vector 0 (1- (command-chain-player-count)))
        command-chain-current-player-index 0
        command-chain-point-after-prompt 0
        command-chain-char (+ ?a (random (- ?z ?a))))
  (command-chain-edit
    (animate-string "GAME START." 0 0)
    (sit-for 0.3)
    (insert ?\n))
  (command-chain-prompt))

(defun command-chain (player-count)
  "Play command chain game, that is, word chain by Emacs commands."
  (interactive "sHow many players? (default 2): ")
  (when (stringp player-count)
    (setq player-count
          (if (string= player-count "") 2 (string-to-number player-count))))
  (when (< player-count 2)
    (error "Number of players must be 2 or more."))
  (unless (get-buffer command-chain-config-buffer-name)
    (command-chain-initialize-players player-count))
  (command-chain-config))

(provide 'command-chain)
