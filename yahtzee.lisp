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
(load "serialize.lisp")
(load "rounds.lisp")

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
(run-tournament)