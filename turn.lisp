; /* *********************************************
; Source Code to handle rounds of the game
;   -> Relies on:
;       validation.lisp
;       dice.lisp
;       strategy.lisp
; ********************************************* */

; /* *********************************************************************
; Function Name: list-available-categories
; Purpose: To list or validate player-input list of available categories
; Parameters:
;           game-data, an object holding info on the game state
;           player-name, the name of the player whose turn it is
; Return Value: the list of available categories obtained
; Reference: none
; ********************************************************************* */
(defun list-available-categories (game-data player-name)
    (let
        ((available-categories (get-available-categories game-data)))
        
        (cond
            ; List available categories for the Computer player.
            ((equal player-name 'Computer)
                (terpri)
                (princ "Listing all available categories, given the dice set so far...")
                (terpri)
                (print available-categories)
            )
            ; For the Human player, validate their input.
            (t (validate-available-categories available-categories))) 
        available-categories))

; /* *********************************************************************
; Function Name: pursue-categories
; Purpose: To list the category or categories the player wants to pursue
; Parameters:
;           game-data, an object holding info on the game state
;           player-name, the name of the player whose turn it is
;           available-categories, a list of currently available categories to choose from
; Return Value: an updated game data object with newly determined strategy
; Reference: none
; ********************************************************************* */
(defun pursue-categories (game-data player-name available-categories)
    (let
        ((best-strategy (pick-strategy game-data)))
        ; YOU ARE HERE
        ; NEXT STEP IS TO WRITE PRINT STRATEGY FUNCTIONS AND FINISH PURSUING CATEGORIES
        
        (cond
            ; List available categories for the Computer player.
            ((equal player-name 'Computer)
                (terpri)
                (princ "Listing all available categories, given the dice set so far...")
                (terpri)
                (print available-categories)
            )
            ; For the Human player, validate their input.
            (t (validate-available-categories available-categories))) 
        nil))

; /* *********************************************************************
; Function Name: print-roll-header
; Purpose: Prints the header for a specific roll
; Parameters:
;           roll-num, the current roll number
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-roll-header (roll-num)
    (princ "=================================")
    (terpri)
    (princ "Roll ")
    (print roll-num)
    nil)

(defun handle-rolls (game-data player-name &optional (roll-num 1) stand)
    (cond
        ; Return updated game-data once player stood or finished third roll.
        ((or stand (> roll-num 3)) game-data)
        (t
            (let
                ((updated-game-data (let
                    ((roll-result (roll-all (get-dice game-data))))

                    (princ "Roll Result: ")
                    (print-dice roll-result)
                    (update-dice game-data roll-result))))
                
                (list-available-categories updated-game-data player-name)
            
                
                )))
; /* *********************************************************************
; Function Name: print-turn-header
; Purpose: Prints the header for a given turn
; Parameters:
;           player-name, the name of the player whose turn it is
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-turn-header (player-name)
    (princ "=================================")
    (terpri)
    (cond
        ((equal player-name 'Human) (princ "-- Your Turn --"))
        (t (princ "-- Computer's Turn --")))
    (terpri)
    nil)


(defun begin-turn (player-name game-data)
    (print-turn-header player-name)
    (let
        ((final-dice (handle-rolls game-data player-name)))
        ; ask to list available categories
        ; choose category
        ; no need to unlock dice! just use original values
        ; return updated game-data
            ; update-scorecard function for this
        ))