; /* *********************************************
; Source Code to handle and print game data
; ********************************************* */

; /* *********************************************************************
; Function Name: get-default-scorecard
; Purpose: To return a list containing a blank scorecard to use to start games
; Parameters: None
; Return Value: A list containing the scorecard (12 empty categores)
; Reference: none
; ********************************************************************* */
(defun get-default-scorecard ()
    ; categories
    '((0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0))  
)

; /* *********************************************************************
; Function Name: initialize-game-data
; Purpose: To add game state information onto serialized information, or create it from scratch
; Parameters:
;            round-and-scorecard, an optional list representing containing a round and list of 
;            categories for the scorecard
; Return Value: The game data, represented by a list of the round, scorecard categories, dice, and
;               strategy state info
; Reference: none
; ********************************************************************* */
(defun initialize-game-data (&optional round-and-scorecard)
    (append 
        ; Use existing round and scorecard data, or create from default
        (cond (round-and-scorecard round-and-scorecard)
              (t (list 1 (get-default-scorecard)))
        )
        ; Append to default dice and strategy lists
        '(
            ; dice
            ( (1 nil) (1 nil) (1 nil) (1 nil) (1 nil) )
            ; strategy info
            ( 0 0 (0 0 0 0 0 0) (0 0 0 0 0 0) "None")
        )
    )
)

; /* *********************************************************************
; Function Name: print-category
; Purpose: To print a particular category
; Parameters:
;           score-data, a list with either 0 indicating empty, or points scored (integer), the 
;           winner ('Human or 'Computer), and the round scored in (an integer)
;           category-num, an integer representing which category this is (index from [1,12])
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-category (score-data category-num)
    (let 
        ; Set the detailed info depending on the category number.
        (   (info 
             (cond 
                ((= category-num 1) '("Aces" "Any combination" "Sum of dice with the number 1"))
                ((= category-num 2) '("Twos" "Any combination" "Sum of dice with the number 2"))
                ((= category-num 3) '("Threes" "Any combination" "Sum of dice with the number 3"))
                ((= category-num 4) '("Fours" "Any combination" "Sum of dice with the number 4"))
                ((= category-num 5) '("Fives" "Any combination" "Sum of dice with the number 5"))
                ((= category-num 6) '("Sixes" "Any combination" "Sum of dice with the number 6"))
                ((= category-num 7) '("Three of a Kind" "At least three dice the same" "Sum of all dice"))
                ((= category-num 8) '("Four of a Kind" "At least four dice the same" "Sum of all dice"))
                ((= category-num 9) '("Full House" "Three of one number and two of another" "25"))
                ((= category-num 10) '("Four Straight" "Four sequential dice" "30"))
                ((= category-num 11) '("Five Straight" "Five sequential dice" "40"))
                ((= category-num 12) '("Yahtzee" "All five dice the same" "50"))
             )
            )
        )

        ; MCHECK
        ; Print out the category's information with correct spacing for the scorecard.
        (apply #'format t "~7A~17A~40A~33A" category-num info)

        ; If the category was filled, output that data. Otherwise, make a new line.
        (cond
            ((= (first score-data) 0) (terpri))
            (t (format t "~10A~8A~5A~%" (second score-data) (first score-data) (third score-data)))
        )

        ; Return value
        nil
    )
)

; /* *********************************************************************
; Function Name: print-categories
; Purpose: To print a list of categories
; Parameters:
;           score-data, a list of categories' score data
;           start-at, the integer to start printing the list from. Set as 1 by default
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-categories (score-data &optional (start-at 1))
    (cond
        ; Base case - no more score data, so print a new line.
        ((null score-data) (terpri))

        ; Recursive case
        (t
            ; Print the first value of score data with current category index.
            (print-category (first score-data) start-at)

            ; Recursively call print-categories on the rest of the list, increasing index.
            (print-categories (rest score-data) (+ start-at 1))
        )
    )
)

; /* *********************************************************************
; Function Name: print-scorecard
; Purpose: To print the current scorecard, or print default one if not given
; Parameters:
;           game-data, optional parameter holding all game info to pull scorecard from
;           start-at, the integer to start printing the list from. Set as 1 by default
; Return Value: game-data
; Reference: none
; ********************************************************************* */
(defun print-scorecard (&optional game-data)
    (let 
        ; Pull the scorecard from game-data, or use default values.
        (   (scorecard 
            (cond 
                ((null game-data) (get-default-scorecard))
                (t (second game-data))))
        )

        ; Print the header info.
        (princ "Current Scorecard:")
        (terpri)
        (format t "~7A~17A~40A~33A~10A~8A~5A~%" 
            "Index" "Category" "Description" "Score" "Winner" "Points" "Round")
        (princ "========================================================================================================================")
        (terpri)

        ; Print all category information.
        (print-categories scorecard)

        ; Return whatever was passed in
        game-data
    )
)