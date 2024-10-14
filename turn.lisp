; /* *********************************************
; Source Code to handle rounds of the game
;   -> Relies on:
;       dice.lisp
; ********************************************* */

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

(defun handle-rolls (game-data &optional (roll-num 1) stand)
    (cond
        ; Return updated game-data once player stood or finished third roll.
        ((or stand (> roll-num 3)) game-data)
        (t
            (let
                ((roll-result (roll-all (get-dice game-data))))

                (princ "Roll Result: ")
                (print-dice roll-result))
                
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
        ((rolled-dice (roll-all (get-dice game-data))))
        
        
    ))