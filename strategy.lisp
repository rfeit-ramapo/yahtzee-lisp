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
;           kind-num, the number "of a kind" needed (3 or 4)
; Return Value: an integer representing the score obtained from this dice set
; Reference: none
; ********************************************************************* */
(defun score-kind (dice kind-num)
    (cond 
        ; If there is a face with at least 3 or 4, sum all dice for the score.
        ((>= (max-list (count-dice-faces dice)) kind-num) (sum-dice dice))
        ; Otherwise, this does not score.
        (t 0)))

; /* *********************************************************************
; Function Name: get-kind-scoring-dice
; Purpose: Get a perfect scoring dice set for a kind category
; Parameters:
;           kind-num, the number "of a kind" for this category
;           repeated, the face value that is repeated to make up the "kind"
;           dice, an optional parameter indicating the current dice set
;               If unset, indicates an ideal scoring dice set.
; Return Value: a list of dice that would score maximum points for this category
; Reference: none
; ********************************************************************* */
(defun get-kind-scoring-dice (kind-num repeated &optional (dice '()))
    (replace-free-dice dice (list (list repeated kind-num) (list 6 5))))

; /* *********************************************************************
; Function Name: get-best-kind-scoring-dice
; Purpose: Get the best possible dice set for scoring for a kind category
; Parameters:
;           kind-num, the number "of a kind" for this category
;           dice, an optional parameter indicating the current dice set
;           repeated, optional parameter indicating face value repeated to make up the "kind"
; Return Value: a list of dice that would score maximum points for this category
; Reference: none
; ********************************************************************* */
(defun get-best-kind-scoring-dice (kind-num &optional (dice '()) (repeated 6))
    ; If repeated reached 0, this category is impossible so return 'nil'
    (cond ((= repeated 0) nil)
          ; Get ideal target dice for current value of 'repeated'
          (t (let ((target (get-kind-scoring-dice kind-num repeated dice)))
               ; If this scored, return the target dice set.
               (cond ((> (score-kind target kind-num) 0) target)
                     ; Otherwise, keep checking lower values of the kind category.
                     (t (get-best-kind-scoring-dice kind-num dice (- repeated 1))))))))

; /* *********************************************************************
; Function Name: strategize-kind
; Purpose: To create a strategy for a kind category
; Parameters:
;           dice, the dice set to strategize for
;           kind-num, the number "of a kind" needed (3 or 4)
;           name, the name of the category
; Return Value: a strategy to score for this category (or nil if impossible)
; Reference: none
; ********************************************************************* */
(defun strategize-kind (dice kind-num name)
    (let* 
        ; Get the score given the current dice set.
        ((current-score (score-kind dice kind-num))
        ; Determine what dice set the player should aim for.
        (target-list (get-best-kind-scoring-dice kind-num dice))
        (target (count-dice-faces target-list))
        ; Determine what should be rerolled based on free, unscoring dice and target values.
        (to-reroll (count-free-unscored-dice dice target))
        ; Determine the max score from target dice.
        (max-score (score-kind target-list kind-num)))

        ; Return a list representing the strategy.
        (list current-score max-score to-reroll target name)))


; STRATEGY ROUTING NOTE: USE THIS FUNCTION TO SKIP FULL CATEGORIES, OR IF ALL DICE ARE LOCKED.