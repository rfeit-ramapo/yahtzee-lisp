; /* *********************************************
; Source Code to handle strategizing functions
;   -> Relies on:
;       dice.lisp
;       utility.lisp
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
    (* (count-dice-face dice multiple-num) multiple-num))

; /* *********************************************************************
; Function Name: get-multiples-scoring-dice
; Purpose: Get a perfect scoring dice set for a multiples category
; Parameters:
;           multiple-num, the face value for this category
;           dice, an optional parameter indicating the current dice set
;               If unset, indicates an ideal scoring dice set.
; Return Value: a list of dice that would score maximum points for this category
; Reference: none
; ********************************************************************* */
(defun get-multiples-scoring-dice (multiple-num &optional (dice '()))
    (replace-free-dice dice (list (list multiple-num 5))))

; /* *********************************************************************
; Function Name: strategize-multiples
; Purpose: To create a strategy for a multiples category
; Parameters:
;           dice, the dice set to strategize for
;           multiple-num, the face value of multiples to score
;           name, the name of the category
; Return Value: a strategy to score for this category (or nil if impossible)
; Reference: none
; ********************************************************************* */
(defun strategize-multiples (dice multiple-num name)
    (let* 
        ; Get the score given the current dice set.
        ((current-score (score-multiples dice multiple-num))
        ; Determine what should be rerolled based on free, unscoring dice and best dice set.
        (to-reroll (count-free-unscored-dice dice (count-dice-faces(get-multiples-scoring-dice multiple-num))))
        ; Determine what dice set the player should aim for.
        (target-list (get-multiples-scoring-dice multiple-num dice))
        (target (count-dice-faces target-list))
        ; Determine the max score from target dice.
        (max-score (score-multiples target-list multiple-num)))

        ; Return a list representing the strategy.
        (list current-score max-score to-reroll target name)))

; /* *********************************************************************
; Function Name: score-kind
; Purpose: To score a dice set for a Kind category (3 or 4 of a Kind)
; Parameters:
;           dice, the dice set to score
;           multiple-num, the number "of a kind" needed (3 or 4)
; Return Value: an integer representing the score obtained from this dice set
; Reference: none
; ********************************************************************* */
(defun score-kind (dice multiple-num)
    (* (count-dice-face dice multiple-num) multiple-num))

    
; STRATEGY ROUTING NOTE: USE THIS FUNCTION TO SKIP FULL CATEGORIES, OR IF ALL DICE ARE LOCKED.