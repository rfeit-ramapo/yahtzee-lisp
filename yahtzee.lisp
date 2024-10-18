; ************************************************************
; * Name:  Rebecca Feit                                      *
; * Project:  Yahtzee - Lisp Implementation                  *
; * Class:  OPL (CMPS 366 01)                                *
; * Date:                                                    *
; ************************************************************

; Load all files in correct order
(load "utility.lisp")
(load "validation.lisp")
(load "game-data.lisp")
(load "dice.lisp")
(load "strategy.lisp")
(load "validation2.lisp")
(load "turn.lisp")
(load "rounds.lisp")
(load "serialize.lisp")

; /* *********************************************************************
; Function Name: print-instructions
; Purpose: To print instructions and wait for player confirmation
; Parameters: None
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-instructions ()
    ; Basic instruction string
    (princ "Welcome to Yahtzee! Below you will find the scorecard categories. When asked to input dice, please use face values. When asked for multiple values (dice or categories), please separate each by a space, and enclose in parentheses (e.g. (1 2 3)). All categories should be specified by index. To help visualize the dice, all 'locked' dice (those that have been set aside and cannot be rerolled) are displayed in red. If you need help, enter 'h' to get a recommendation.")
    (terpri)
    (terpri)

    ; Print default scorecard
    (print-scorecard)

    ; Await user confirmation and return nil
    (princ "To begin the game, please press enter.")
    (terpri)
    (read-line)
    nil)

; /* *********************************************************************
; Function Name: print-final
; Purpose: Prints the final results of the game
; Parameters: None
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-final (game-data)
    (terpri)
    (princ "=================================")
    (terpri)
    (princ "Game Complete!")
    (terpri)
    (terpri)
    (print-scorecard  game-data)
    (print-scores game-data)
    (let* 
        ((player-scores (get-player-scores game-data))
        (human-score (second (first player-scores)))
        (computer-score (second (second player-scores))))
        
        (cond
            ((> human-score computer-score) (princ "You won!") (terpri))
            ((> computer-score human-score) (princ "The computer won!" (terpri)))
            (t (princ "It was a draw!") (terpri)))))

; /* *********************************************************************
; Function Name: run-tournament
; Purpose: The main function to kick off and run the tournament
; Parameters: None
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun run-tournament ()
    (print-instructions)
    (let ((game-data (initialize-game-data (serialize-load))))
        (print-scorecard game-data)
        
        (print-final (run-rounds game-data)))
    nil)

; /* *********************************************
; Function calls
; ********************************************* */

; Run the tournament
;(run-tournament)