; /* *********************************************
; Source Code to handle rounds of the game
;   -> Relies on:
;       validation.lisp
;       dice.lisp
;       strategy.lisp
;       validation2.lisp
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
            (t 
                (princ "Please list all available scorecard categories, given your current dice set.")
                (terpri)
                (validate-available-categories available-categories))) 
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
        
        (cond
            ; Print chosen strategy for the Computer player.
            ((equalp player-name 'Computer)
                (print-strategy best-strategy 'Computer))
            ; For the Human player, validate their input.
            (t 
                (princ "Please input a list of categories you would like to pursue.")
                (terpri)
                (validate-pursue-categories available-categories best-strategy))) 
        (update-strategy game-data best-strategy)))


(defun handle-rerolls (game-data player-name)
    (let*
        ((best-strategy (get-strategy game-data))
         (diceset (get-dice game-data))
         (to-reroll (get-strat-to-reroll best-strategy))
         (curr-score (get-strat-curr-score best-strategy))
         (max-score (get-strat-max-score best-strategy)))

        (cond
            ((equalp player-name 'Computer)
             (cond
                ; If this is an empty strategy or current score = max score, return early (stand).
                ((null best-strategy) diceset)
                ((= curr-score max-score) diceset)
                ; Otherwise, lock all the dice and return updated dice set.
                (t
                    (lock-other-dice diceset to-reroll))))
            (t
                (cond 
                    ; If the player wants to reroll, lock the ones they want to keep.
                    ((validate-stand-reroll best-strategy)
                    (lock-other-dice
                        diceset
                        (validate-reroll 
                            best-strategy (count-dice-faces (filter-free-dice diceset)))))
                    ; Otherwise, do not alter the diceset.
                    (t diceset))))))
; then put together all the created functions into turn
; after this, if the dice set has CHANGED, reroll
; otherwise, do not
; this ensures that if the player said to reroll but input (), it determines they should stand

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
            
                
                ))))
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