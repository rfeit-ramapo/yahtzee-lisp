; /* *********************************************
; Source Code to handle validation of user input
;   Second separate file to rely on strategy.lisp
;   -> Relies on:
;       strategy.lisp
; ********************************************* */

(load "utility.lisp")
(load "validation.lisp")
(load "game-data.lisp")
(load "dice.lisp")
(load "strategy.lisp")

; /* *********************************************************************
; Function Name: validate-pursue-categories
; Purpose: Validates user input of valid categories to pursue
; Parameters:
;           available-categories, a list of available categories for current diceset
;           best-strategy, the strategy to recommend if the user asks for help
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun validate-pursue-categories (available-categories best-strategy)
    (let
        ((input (read)))
        (cond
            ; Help functionality
            ((equalp input 'h)
                (print-strategy best-strategy 'Human)
                (validate-pursue-categories available-categories best-strategy))
            ; If user input was valid, return.
            ((validate-pursued-categories input available-categories) nil)
            ; Otherwise, print error message and recursively call the function again.
            (t
                (princ "Error: Input must be a subset of available categories that have at least one contributing die (e.g. (11 12)). Please try again.")
                (terpri)
                (validate-pursue-categories available-categories best-strategy)))))