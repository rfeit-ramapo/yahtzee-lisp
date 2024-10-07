; /* *********************************************
; Source Code to manipulate dice into different formats
;   -> Relies on:
;       utility.lisp
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
; Function Name: filter-locked-dice
; Purpose: Filters out dice that are free from a list of dice
; Parameters: 
;           dice, a list of dice
; Return Value: the list of locked dice
; Reference: none
; ********************************************************************* */
(defun filter-locked-dice (dice)
    (cond
        ; Base case: initial list of dice
        ((null (first dice)) '())

        (t (cond
            ; If the die is locked, skip it and return rest of free dice
            ((second (first dice)) (cons (first dice) (filter-locked-dice (rest dice))))
            ; If die is free, add it to the list of free dice
            (t (filter-locked-dice (rest dice)))))))

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
; Function Name: total-scored-dice
; Purpose: Count how many dice are scored for a single face
; Parameters: 
;           curr-die-count, an integer [0-5] representing how many dice of this face there are
;           scoring-die-count, an integer [0-5] representing how many dice of this face score
; Return Value: an integer representing how many dice of this face score
; Reference: none
; ********************************************************************* */
(defun total-scored-dice (curr-die-count scoring-die-count)
    (min curr-die-count scoring-die-count))

; /* *********************************************************************
; Function Name: count-unscored-dice
; Purpose: Get the dice that would NOT score from counts of each face
; Parameters: 
;           dice-counts, a list of integers for each die face that represent how many there are
;           scoring-counts, a list of integers for each die face that represent what would score
; Return Value: a list of dice counts for each face that do not contribute to a score
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
; Function Name: count-scored-dice
; Purpose: Get the dice that would score from counts of each face
; Parameters: 
;           dice-counts, a list of integers for each die face that represent how many there are
;           scoring-counts, a list of integers for each die face that represent what would score
; Return Value: a list of dice counts for each face that contribute to a score
; Reference: none
; ********************************************************************* */
(defun count-scored-dice (dice-counts scoring-counts)
    (cond
        ; Base case: last dice count in lists
        ((null (rest dice-counts)) 
         ; Make a list with number of scoring dice
         (list (total-scored-dice (first dice-counts) (first scoring-counts))))

        ; Recursive case: add to the list of scoring dice
        (t (cons 
            (total-scored-dice (first dice-counts) (first scoring-counts))
            (count-scored-dice (rest dice-counts) (rest scoring-counts))))))

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


; /* *********************************************************************
; Function Name: add-dice
; Purpose: Add dice to a list until there are none left or the list is full
; Parameters: 
;           dice, a list of dice to add to
;           replacements, a list of holding lists that contain a face value and 
;               number of dice to add. For example, '(5 5) indicates add 5 Fives.
; Return Value: an updated list of dice with added values
; Reference: none
; ********************************************************************* */
(defun add-dice (dice replacements)
    (cond 
        ; Base case - no more replacement dice, or the dice list hit its max (5).
        ((or (null replacements) 
             (= (list-size dice) 5))
         dice)
        ; If the next replacement value has none left, use the next one.
        ((= (second (first replacements)) 0) (add-dice dice (rest replacements)))
        (t (add-dice 
                ; Add the replacement value onto the beginning of the dice list.
                (cons (list (first (first replacements)) nil) dice)
                ; Update replacement list to have one less value.
                (cons (list (first (first replacements))
                            (- (second (first replacements)) 1))
                      (rest replacements))))))

; /* *********************************************************************
; Function Name: replace-free-dice
; Purpose: Replaces free dice in a list with new dice
; Parameters: 
;           dice, a list of dice to replace the free dice in
;           replacements, a list of holding lists that contain a face value and 
;               number of dice to add. For example, '(5 5) indicates add 5 Fives.
; Return Value: an updated list of dice with added values
; Reference: none
; ********************************************************************* */
(defun replace-free-dice (dice replacements)
    (add-dice (filter-locked-dice dice) replacements))

; /* *********************************************************************
; Function Name: sum-dice
; Purpose: Sum a list of dice
; Parameters:
;           dice, the set of dice
; Return Value: the sum of all face values of the dice set
; Reference: none
; ********************************************************************* */
(defun sum-dice (dice)
    (cond ((null dice) 0)
          (t (+ (first (first dice)) (sum-dice (rest dice))))))