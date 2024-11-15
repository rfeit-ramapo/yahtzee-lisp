; /* *********************************************
; Source Code to handle rounds of the game
;   -> Relies on:
;       game-data.lisp
;       dice.lisp
;       turn.lisp
; ********************************************* */

; /* *********************************************************************
; Function Name: randomize-player-order
; Purpose: Randomizes player order by rolling dice for each player
; Parameters: none
; Return Value: a list with player names in turn order
; Reference: none
; ********************************************************************* */
(defun randomize-player-order ()
    (princ "You will roll first, followed by the computer.")
    (terpri)
    (let*
        ((human-roll (roll-one))
        (computer-roll (roll-one)))

        (princ "You rolled a ")
        (princ human-roll)
        (terpri)
        (princ "The computer rolled a ")
        (princ computer-roll)
        (terpri)
        (cond
            ((> human-roll computer-roll) (list 'Human 'Computer))
            ((> computer-roll human-roll) (list 'Computer 'Human))
            (t (princ "Rerolling due to a tie...") (terpri) (randomize-player-order)))))

; /* *********************************************************************
; Function Name: get-player-order
; Purpose: Determines player order by score, or randomly if tied
; Parameters: none
; Return Value: a list with player names in turn order
; Reference: none
; ********************************************************************* */
(defun get-player-order (game-data)
    (princ "Determing who will go first...")
    (terpri)
    (let* 
        ((player-scores (get-player-scores game-data))
        (human-score (second (first player-scores)))
        (computer-score (second (second player-scores))))
        
        (cond
            ((> human-score computer-score) (list 'Computer 'Human))
            ((> computer-score human-score) (list 'Human 'Computer))
            (t (randomize-player-order)))))

; /* *********************************************************************
; Function Name: print-round-header
; Purpose: Prints the header for a given round
; Parameters:
;           round-num, the number of the current round
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-round-header (round-num)
    (princ "=================================")
    (terpri)
    (terpri)
    (princ "Round ")
    (princ round-num)
    (princ ":")
    (terpri)
    (terpri)
    (princ "=================================")
    (terpri)
    nil)

; /* *********************************************************************
; Function Name: run-round
; Purpose: The function to handle running one round of a game
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: the game data after running one round of the game
; Reference: none
; ********************************************************************* */
(defun run-round (game-data)
    (print-round-header (get-round-num game-data))
    
    (let* 
        ; Get the order and run the turns.
        ((player-order (get-player-order game-data))
        (after-turn1 (run-turn (first player-order) game-data))
        (after-turn2 (cond 
            ; Do not run the second turn if the game is over already.
            ((check-scorecard-full after-turn1) after-turn1)
            (t (run-turn (second player-order) after-turn1)))))

        ; Print the score for each player.
        (print-scores after-turn2)

        ; Return updated game data after asking to save.
        (serialize-save after-turn2)))

; /* *********************************************************************
; Function Name: run-rounds
; Purpose: The function to handle running rounds of the game
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: the game data after running all rounds of the game
; Reference: none
; ********************************************************************* */
(defun run-rounds (game-data)
    (cond
        ; Return the input game data if the scorecard is full.
        ((check-scorecard-full game-data) game-data)
        ; Otherwise, recursively call function on result of single round.
        (t (run-rounds (increment-round (run-round game-data))))))