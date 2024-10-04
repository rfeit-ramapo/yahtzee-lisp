; /* *********************************************
; Source Code to manipulate dice into different formats
; ********************************************* */

; /* *********************************************************************
; Function Name: get-dice
; Purpose: Extract the dice set from game-data
; Parameters: 
;           game-data, an object holding info on the game state
; Return Value: the dice set (list of dice)
; Reference: none
; ********************************************************************* */
(defun get-dice (game-data)
    (third game-data))

; /* *********************************************************************
; Function Name: filter-free-dice
; Purpose: Filters out dice that are locked from a list of dice
; Parameters: 
;           dice, a list of dice
; Return Value: the list of free dice (unlocked)
; Reference: none
; ********************************************************************* */
(defun filter-free-dice (dice)
    (cond
        ; Base case: initial list of dice
        ((null (first dice)) '())

        (t (cond
            ; If the die is locked, skip it and return rest of free dice
            ((second (first dice)) (filter-free-dice (rest dice)))
            ; If die is free, add it to the list of free dice
            (t (cons (first dice) (filter-free-dice (rest dice))))))))

; /* *********************************************************************
; Function Name: count-dice-face
; Purpose: Count the number of dice of a given face value
; Parameters: 
;           dice, a list of dice
;           face, the value of the face to count
; Return Value: an integer representing how many of this dice face there are [0-5]
; Reference: none
; ********************************************************************* */
(defun count-dice-face (dice face)
    ; Base case: no more dice
    (cond ((null dice) 0)

    ; Recursive case: add this face if it counts, to the count of the rest
        (t ( + 
            (cond
                ; If this die has the desired face, count it.
                ((= (first (first dice)) face) 1)
                (t 0)
            )
            (count-dice-face (rest dice) face)))))

; /* *********************************************************************
; Function Name: count-dice-faces
; Purpose: Get a count of how many dice of each face are in a list of dice
; Parameters: 
;           dice, a list of dice
;           initial-face, an optional parameter determining where to start counting
; Return Value: a list indicating the counts of each possible dice face [1-6]
; Reference: none
; ********************************************************************* */
(defun count-dice-faces (dice &optional (initial-face 1))
    (cond 
        ; Base case: final face -- return a list of this count
        ((= initial-face 6) (list (count-dice-face dice 6)))

        ; Recursive case: cons current face count onto the rest of the counts
        (t (cons (count-dice-face dice initial-face) 
                 (count-dice-faces dice (+ initial-face 1))))))

; /* *********************************************************************
; Function Name: total-unscored-dice
; Purpose: Count how many dice are unscored for a single face
; Parameters: 
;           curr-die-count, an integer [0-5] representing how many dice of this face there are
;           scoring-die-count, an integer [0-5] representing how many dice of this face score
; Return Value: an integer representing how many dice of this face are unscored
; Reference: none
; ********************************************************************* */
(defun total-unscored-dice (curr-die-count scoring-die-count)
    (max 0 (- curr-die-count scoring-die-count)))

; /* *********************************************************************
; Function Name: count-unscored-dice
; Purpose: Get the dice that would score from counts of each face
; Parameters: 
;           dice-counts, a list of integers for each die face that represent how many there are
;           scoring-counts, a list of integers for each die face that represent what would score
; Return Value: a list of dice counts for each face that contribute to a score
; Reference: none
; ********************************************************************* */
(defun count-unscored-dice (dice-counts scoring-counts)
    (cond
        ; Base case: last dice count in lists
        ((null (rest dice-counts)) 
         ; Make a list with number of unscored dice
         (list (total-unscored-dice (first dice-counts) (first scoring-counts))))

        ; Recursive case: add to the list of unscored dice
        (t (cons 
            (total-unscored-dice (first dice-counts) (first scoring-counts))
            (count-unscored-dice (rest dice-counts) (rest scoring-counts))))))

; /* *********************************************************************
; Function Name: count-free-unscored-dice
; Purpose: Get the dice that would score from a list of dice
; Parameters: 
;           curr-dice, a list of dice
;           scoring-counts, a list of integers for each die face that represent what would score
; Return Value: a list of dice counts for each face that contribute to a score
; Reference: none
; ********************************************************************* */
(defun count-free-unscored-dice (curr-dice scoring-counts)
    ; Get the counts of free dice and use that to compare against scoring dice
    (count-unscored-dice (count-dice-faces (filter-free-dice curr-dice)) scoring-counts))