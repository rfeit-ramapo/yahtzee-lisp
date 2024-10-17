; /* *********************************************
; Source Code to handle validation of user input
;   Second separate file to rely on strategy.lisp
;   -> Relies on:
;       dice.lisp
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


; /* *********************************************************************
; Function Name: validate-stand-reroll
; Purpose: Validates input as 'stand' or 'reroll'
; Parameters:
;           best-strategy, the strategy to recommend if the user asks for help
; Return Value: t if 'reroll' and nil if 'stand'
; Reference: none
; ********************************************************************* */
(defun validate-stand-reroll (best-strategy)
    (let 
        ; Get user input.
        ((input (read)))
        (cond 
            ; Help functionality
            ((equalp input 'h)
                (print-strategy best-strategy 'Human)
                (validate-stand-reroll best-strategy))

            ; Valid input: yes or no (y or n)
            ((equalp input 'stand) nil)
            ((equalp input 'reroll) t)
            ; Invalid input: output error and retry.
            (t 
            (princ "Error: Input must be 'stand' or 'reroll'. Please try again.")
            (terpri)
            (validate-stand-reroll best-strategy)))))

; /* *********************************************************************
; Function Name: validate-reroll-counts
; Purpose: Validates that a user has selected dice faces that can be rerolled
; Parameters:
;           free-counts, dice face counts for unlocked dice (ones that can be rerolled)
;           reroll-counts, dice face counts for user's desired rerolls
; Return Value: t if valid rerolls, nil if not
; Reference: none
; ********************************************************************* */
(defun validate-reroll-counts (free-counts reroll-counts)
    (cond
        ; Validated entire list, so return true.
        ((null free-counts) t)
        (t (cond
            ; Can only reroll up to number of free dice for each face.
            ((> (first reroll-counts) (first free-counts)) nil)
            ; Recursively call for rest of the counts.
            (t (validate-reroll-counts (rest free-counts) (rest reroll-counts)))))))

; /* *********************************************************************
; Function Name: validate-reroll
; Purpose: Gets user input for which dice to reroll and validates it
; Parameters:
;           best-strategy, the strategy to recommend if the user asks for help
;           free-counts, dice face counts for unlocked dice (ones that can be rerolled)
; Return Value: dice counts of which dice to reroll
; Reference: none
; ********************************************************************* */
(defun validate-reroll (best-strategy free-counts)
    (let* 
        ; Get a valid list of dice faces from the user.
        ((input (validate-dice-faces nil t))
         (to-reroll (cond 
            ((equalp input 'h) nil)
            (t (count-dice-faces (faces-to-dice input))))))

        (cond
            ; Help functionality
            ((equalp input 'h)
                (print-strategy best-strategy 'Human)
                (validate-reroll best-strategy free-counts))
            ; Validate that the faces picked can be rerolled.
            ((validate-reroll-counts free-counts to-reroll) to-reroll)
            (t
                (princ "Error: Input must be a list of free dice by their face values (e.g. (3 3 2)). Please try again.")
                (terpri)
                (validate-reroll best-strategy free-counts)))))