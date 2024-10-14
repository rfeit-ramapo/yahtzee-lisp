; ************************************************************
; * Name:  Rebecca Feit                                      *
; * Project:  Yahtzee - Lisp Implementation                  *
; * Class:  OPL (CMPS 366 01)                                *
; * Date:                                                    *
; ************************************************************

; Load all files in correct order
(load "utility.lisp")
(load "game-data.lisp")
(load "validation.lisp")
(load "serialize.lisp")
(load "dice.lisp")
(load "strategy.lisp")
(load "rounds.lisp")

; /* *********************************************************************
; Function Name: SAMPLE-FUNCTION-HEADER
; Purpose: To return a list containing a blank scorecard to use to start games
; Parameters:
;             grades[], an array passed by value. It holds individual grades
;             size, an integer. It refers to the number of students in the class
; Return Value: The average grade in the class, a real value
; Algorithm:
;             1) Add all the grades
;        2) Divide the sum by the number of students in class to calculate the average
; Reference: none
; ********************************************************************* */

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
    nil
)

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
    )
    nil
)

; /* *********************************************
; Function calls
; ********************************************* */

; Run the tournament
;(run-tournament)