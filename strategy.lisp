; /* *********************************************
; Source Code to handle strategizing functions
;   -> Relies on:
;       dice.lisp
; ********************************************* */

; /* *********************************************************************
; Function Name: score-multiples
; Purpose: To score a dice set for a Multiples category (aces, twos, etc.)
; Parameters:
;           dice, the dice set to score
;           multiple-num, the face value of multiples to score
; Return Value: an integer representing the score obtained from this dice set
; Reference: none
; ********************************************************************* */
(defun score-multiples (dice multiple-num)
    (* (count-dice-face dice multiple-num) multiple-num)
)

; /* *********************************************************************
; Function Name: strategize-multiples
; Purpose: To create a strategy for a multiples category
; Parameters:
;           dice, the dice set to strategize for
;           multiple-num, the face value of multiples to score
;           name, the name of the category
; Return Value: a strategy to score for this category (or nil if impossible)
; Algorithm:
;           1) Add all the grades
;           2) Divide the sum by the number of students in class to calculate the average
; Reference: none
; ********************************************************************* */
(defun strategize-multiples (dice multiple-num name)
    ; get current score
    ; determine rerolls (free-unscored dice)
    ; determine target roll
        ; dice set with current dice-count for multiples, plus free-unscored
    ; get max score from target roll
    ; return all this info wrapped neatly into a strategy object
        ; or return nil if the max score is 0
)



; STRATEGY ROUTING NOTE: USE THIS FUNCTION TO SKIP FULL CATEGORIES