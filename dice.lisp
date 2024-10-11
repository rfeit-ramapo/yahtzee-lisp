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
; Function Name: filter-out-face
; Purpose: Filters out dice of a specified face value
; Parameters: 
;           dice, a list of dice
;           face, the face to filter out
; Return Value: the list of dice, excluding the given face value
; Reference: none
; ********************************************************************* */
(defun filter-out-face (dice face)
    (cond
        ; Base case: initial list of dice
        ((null (first dice)) '())
        ; If no face was given, just return the full dice set
        ((null face) dice)
        (t (cond
            ; If the die is of this face, skip it and return rest of the dice
            ((= (first (first dice)) face) (filter-out-face (rest dice) face))
            ; If die is not of this face, add it to the list of free dice
            (t (cons (first dice) (filter-out-face (rest dice) face)))))))

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
; Function Name: expand-dice-face
; Purpose: Expand a given face into a list of individual dice based on the count
; Parameters: 
;           count, an integer representing how many dice of this face exist
;           face, the value of the face to expand
; Return Value: a list of dice represented by (face nil) for each die
; Reference: Received help from ChatGPT to write function, then edited
; ********************************************************************* */
(defun expand-dice-face (count face)
  (cond
    ; Base case: no more dice to expand
    ((= count 0) nil)
    ; Recursive case: prepend the current face and recurse with one less count
    (t (cons (list face nil) (expand-dice-face (- count 1) face)))))

; /* *********************************************************************
; Function Name: counts-to-dice
; Purpose: Convert a list of dice counts into a list of individual dice
; Parameters: 
;           counts, a list representing the counts of each face [0-5]
; Return Value: a list of individual dice, where each die is (face nil)
; Reference: Received help from ChatGPT to write function, then edited
; ********************************************************************* */
(defun counts-to-dice (counts &optional (face 1))
  (cond
    ; Base case: no more faces to process
    ((null counts) nil)
    ; Recursive case: expand the current face count and concatenate with the rest
    (t (append (expand-dice-face (first counts) face) 
               (counts-to-dice (rest counts) (+ face 1))))))

; /* *********************************************************************
; Function Name: total-unscored-dice
; Purpose: Count how many dice are unscored for a single face
; Parameters: 
;           curr-die-count, an integer [0-5] representing how many dice of this face there are
;           scoring-die-count, an integer [0-5] representing how many dice of this face score
;           locked-die-count, an optional parameter representing how many dice of this face are 
;               locked [0-5]. If supplied, only returns free unscored dice.
; Return Value: an integer representing how many dice of this face are unscored
; Reference: none
; ********************************************************************* */
(defun total-unscored-dice (curr-die-count scoring-die-count &optional (locked-die-count 0))
    (max (- curr-die-count (max scoring-die-count locked-die-count)) 0))

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
;           locked-dice, an optional parameter listing how many dice of each face are 
;               locked. If supplied, only returns free unscored dice.
; Return Value: a list of dice counts for each face that do not contribute to a score
; Reference: none
; ********************************************************************* */
(defun count-unscored-dice (dice-counts scoring-counts &optional locked-counts)
    (cond
        ; Base case: last dice count in lists
        ((null (rest dice-counts)) 
         ; Make a list with number of unscored dice
         (list (total-unscored-dice (first dice-counts) (first scoring-counts))))

        ; Recursive case: add to the list of unscored dice
        (t (cons 
            (total-unscored-dice (first dice-counts) (first scoring-counts) (first locked-counts))
            (count-unscored-dice (rest dice-counts) (rest scoring-counts) (rest locked-counts))))))

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
    (count-unscored-dice 
        (count-dice-faces curr-dice) 
        scoring-counts 
        (count-dice-faces (filter-locked-dice curr-dice))))


; /* *********************************************************************
; Function Name: add-dice
; Purpose: Add dice to a list until there are none left or the list is full
; Parameters: 
;           dice, a list of dice to add to
;           replacements, a list of holding lists that contain a face value and 
;               number of dice to add. For example, '(5 4) indicates add 4 Fives.
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

; /* *********************************************************************
; Function Name: count-scored-dice
; Purpose: Get the dice that would score, or are locked from a set of dice counts
; Parameters: 
;           dice-counts, a list of integers for each die face that represent how many there are
;           locked-counts, a list of integers for each die face that represent how many are locked
;           scoring-counts, a list of integers for each die face that represent what would score
; Return Value: a list of dice counts for each face that contribute to a score, or are locked
; Reference: I got help from ChatGPT to provide test cases for this function
; ********************************************************************* */
(defun count-scored-locked-dice (dice-counts locked-counts scoring-counts)
    (cond
        ; Base case: last dice count in lists
        ((null (rest dice-counts)) 
         ; Make a list with number of scoring (or locked) dice
         (list 
           (max 
            (first locked-counts) 
            (total-scored-dice (first dice-counts) (first scoring-counts)))))

        ; Recursive case: add to the list of scoring (or locked) dice
        (t (cons 
            (max 
              (first locked-counts) 
              (total-scored-dice (first dice-counts) (first scoring-counts)))
            (count-scored-locked-dice (rest dice-counts) (rest locked-counts) (rest scoring-counts))))))

; /* *********************************************************************
; Function Name: match-counts
; Purpose: Match the current dice counts to the target, determining which dice are needed
; Parameters: 
;           dice-counts, a list of integers for each die face that represent how many there are
;           target-counts, a list of integers for each die face that represent what scores
;           reroll-list, an optional parameter indicating which dice need to be rerolled
;           curr-face, an optional parameter indicating what face value to start at
; Return Value: a list of dice needed, with sublists specifying the face and number required
; Reference: I got help from ChatGPT to provide test cases for this function
; ********************************************************************* */
(defun match-counts (dice-counts target-counts &optional (reroll-list '()) (curr-face 1))
    ; If all dice have been checked, return num-rerolls.
    (cond ((null dice-counts) reroll-list)
          ; Otherwise, alter num-rerolls based on how many are needed for this face.
          (t 
           (match-counts 
            (rest dice-counts) 
            (rest target-counts) 
            ; Update the reroll list based on the target and current dice for this face value.
            (let ((num-rerolls (- (first target-counts) (first dice-counts))))
                 (cond ((> num-rerolls 0) (cons (list curr-face num-rerolls) reroll-list))
                       (t reroll-list)))
            (+ curr-face 1)))))

; /* *********************************************************************
; Function Name: max-dice-face
; Purpose: Get the dice face with the maximum count
; Parameters: 
;           dice-counts, a list of integers for each die face that represent how many there are
;           curr-face, an optional parameter indicating what face value to start at
; Return Value: a list containing the face with the most dice, and its count
; Reference: none
; ********************************************************************* */
(defun max-dice-face (dice-counts &optional (curr-face 1))
    (cond 
        ; Base case - no dice left, so return nil.
        ((null dice-counts) nil)
        ; Get the max-dice-face from the rest of the list.
        (t (let ((max-rest (max-dice-face (rest dice-counts) (+ curr-face 1))))
                (cond 
                    ; If the rest does not have any dice
                    ((null max-rest) 
                        (cond 
                            ; Set the max to be the current face, if it holds any dice.
                            ((= (first dice-counts) 0) nil) 
                            (t (list curr-face (first dice-counts)))))
                    ; If this face has more dice than max of rest, return this face and count.
                    ((> (first dice-counts) (second max-rest)) (list curr-face (first dice-counts)))
                    ; Otherwise, use the max of the rest of the list.
                    (t max-rest))))))