; /* *********************************************
; Source Code to handle and print game data
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
; Function Name: get-scorecard
; Purpose:  To return the scorecard from a list of game data
;           The scorecard is represented by a list of categories.
;           Each category contains either 0 (if unfilled), or 
;           the points earned, name symbol of the winner, and round filled.
; Parameters: game-data, a list containing all saved data for the game
; Return Value: The scorecard extracted from the game-data
; Reference: none
; ********************************************************************* */
(defun get-scorecard (game-data)
    (second game-data))

; /* *********************************************************************
; Function Name: get-round-num
; Purpose:  To return the current round number from a list of game data
; Parameters: game-data, a list containing all saved data for the game
; Return Value: The round number extracted from the game-data
; Reference: none
; ********************************************************************* */
(defun get-round-num (game-data)
    (first game-data))

; /* *********************************************************************
; Function Name: get-strategy
; Purpose:  To return the currently saved strategy from a list of game data
; Parameters: game-data, a list containing all saved data for the game
; Return Value: The currently saved strategy
; Reference: none
; ********************************************************************* */
(defun get-strategy (game-data)
    (fourth game-data))

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
            nil
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

; /* *********************************************************************
; Function Name: check-category-full
; Purpose: To check if a category has been filled
; Parameters:
;           category, a list representing score data for a category
;               Will only have 0 if unfilled, or will have the following:
;               - points scored
;               - winner
;               - the round the category was filled in
; Return Value: t if filled, or nil if unfilled
; Reference: none
; ********************************************************************* */
(defun check-category-full (category)
    (cond
        ; The category is full if there is more than one value in score data
        ((= (length category) 1) nil)
        (t t)))

; /* *********************************************************************
; Function Name: count-full-categories
; Purpose: Counts the number of categories that have been filled
; Parameters:
;           scorecard, a list containing score data for each category
; Return Value: the number of filled categories
; Reference: none
; ********************************************************************* */
(defun count-full-categories (scorecard)
    (cond 
        ; Base case - start at 0 filled for an empty scorecard
        ((null scorecard) 0)
        ; Add one if this category is full
        ((check-category-full (first scorecard)) (+ 1 (count-full-categories (rest scorecard))))
        ; Otherwise, check the rest of the categories
        (t (count-full-categories (rest scorecard)))))

; /* *********************************************************************
; Function Name: check-scorecard-full
; Purpose: Checks if the scorecard is completely full
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: t if the scorecard is full, or nil if there are unfilled categories
; Reference: none
; ********************************************************************* */
(defun check-scorecard-full (game-data)
    (cond 
        ; If 12 categories are filled, the scorecard is full.
        ((= (count-full-categories (get-scorecard game-data)) 12) t)
        (t nil)))

; /* *********************************************************************
; Function Name: score-player
; Purpose: Adds points scored to a particular player's info
; Parameters:
;           player-name, a symbol of the player's name
;           scorecard, a list of categories to add points from
;           player-points, an optional number representing the total points this player has scored
; Return Value: the total score of the player based on the scorecard and initial points
; Reference: none
; ********************************************************************* */
(defun score-player (player-name scorecard &optional (player-points 0))
    (cond
        ; If the scorecard is empty, return current value of player-points
        ((null scorecard) player-points)
        ; If the player name matches the first scorecard category
        ((equal player-name (second (first scorecard)))
            ; Recursively call function with updated points
            (score-player 
                player-name 
                (rest scorecard)
                (+ player-points (first (first scorecard)))))
        ; Recursively call function without altering points
        (t (score-player player-name (rest scorecard) player-points))))

; /* *********************************************************************
; Function Name: get-player-scores
; Purpose: Gets the scores for both players (Human and Computer)
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: a list containing two lists; both contain:
;               - The symbolic name of a player
;               - A number representing their current score
; Reference: none
; ********************************************************************* */
(defun get-player-scores (game-data)
    (list 
        (list 'Human (score-player 'Human (get-scorecard game-data)))
        (list 'Computer (score-player 'Computer (get-scorecard game-data)))))

; /* *********************************************************************
; Function Name: get-category-index
; Purpose: To get the index of a category from its name
; Parameters:
;           category-name, the name of the category to get the index of
; Return Value: the index value of the category [1-12]
; Reference: none
; ********************************************************************* */
(defun get-category-index (category-name)
    (cond 
        ((null category-name) nil)
        ((equalp category-name "Aces") 1)
        ((equalp category-name "Twos") 2)
        ((equalp category-name "Threes") 3)
        ((equalp category-name "Fours") 4)
        ((equalp category-name "Fives") 5)
        ((equalp category-name "Sixes") 6)
        ((equalp category-name "Three of a Kind") 7)
        ((equalp category-name "Four of a Kind") 8)
        ((equalp category-name "Full House") 9)
        ((equalp category-name "Four Straight") 10)
        ((equalp category-name "Five Straight") 11)
        ((equalp category-name "Yahtzee") 12)))

; /* *********************************************************************
; Function Name: update-dice
; Purpose: To update the diceset for a list of game data
; Parameters:
;           game-data, a list containing all saved data for the game
;           dice, the new dice set to update the game data with
; Return Value: the updated game data
; Reference: none
; ********************************************************************* */
(defun update-dice (game-data dice)
    (list
        (get-round-num game-data)
        (get-scorecard game-data)
        dice
        (get-strategy game-data)))

; /* *********************************************************************
; Function Name: update-strategy
; Purpose: To update the strategy for a list of game data
; Parameters:
;           game-data, a list containing all saved data for the game
;           strategy, the new strategy to update the game data with
; Return Value: the updated game data
; Reference: none
; ********************************************************************* */
(defun update-strategy (game-data strategy)
    (list
        (get-round-num game-data)
        (get-scorecard game-data)
        (get-dice game-data)
        strategy))

; /* *********************************************************************
; Function Name: fill-category
; Purpose: To fill a scorecard category with relevant information
; Parameters:
;           scorecard, a list of categories representing the scorecard data
;           category-num, the index [1-12] of the category to fill
;           points, the points earned by filling this category
;           winner, a symbol representing the player who claimed this category
;           round-num, the round this category was filled in
; Return Value: the updated scorecard
; Reference: none
; ********************************************************************* */
(defun fill-category (scorecard category-num points winner round-num)
    (cond
        ((= category-num 1) (cons (list points winner round-num) (rest scorecard)))
        (t (cons 
            (first scorecard) 
            (fill-category (rest scorecard) (- category-num 1) points winner round-num)))))