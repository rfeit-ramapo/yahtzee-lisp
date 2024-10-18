; /* *********************************************
; Source Code to handle strategizing functions
;   -> Relies on:
;       dice.lisp
;       utility.lisp
;       game-data.lisp
; ********************************************* */

; /* *********************************************************************
; Function Name: get-strat-curr-score
; Purpose: Extract the current score from the strategy object
; Parameters: 
;           strategy, an object holding information about the strategy
; Return Value: the current score associated with the strategy's category
; Reference: none
; ********************************************************************* */
(defun get-strat-curr-score (strategy)
    (first strategy))

; /* *********************************************************************
; Function Name: get-strat-max-score
; Purpose: Extract the maximum score from the strategy object
; Parameters: 
;           strategy, an object holding information about the strategy
; Return Value: the maximum score possible using this strategy
; Reference: none
; ********************************************************************* */
(defun get-strat-max-score (strategy)
    (second strategy))

; /* *********************************************************************
; Function Name: get-strat-to-reroll
; Purpose: Extract the dice to reroll from the strategy object
; Parameters: 
;           strategy, an object holding information about the strategy
; Return Value: a list of dice counts to reroll
; Reference: none
; ********************************************************************* */
(defun get-strat-to-reroll (strategy)
    (third strategy))

; /* *********************************************************************
; Function Name: get-strat-target
; Purpose: Extract the target value from the strategy object
; Parameters: 
;           strategy, an object holding information about the strategy
; Return Value: a list of dice counts to target with this strategy
; Reference: none
; ********************************************************************* */
(defun get-strat-target (strategy)
    (fourth strategy))

; /* *********************************************************************
; Function Name: get-strat-category-name
; Purpose: Extract the category name from the strategy object
; Parameters: 
;           strategy, an object holding information about the strategy
; Return Value: the name of the category this strategy pursues
; Reference: none
; ********************************************************************* */
(defun get-strat-category-name (strategy)
    (fifth strategy))

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
        (cond ((= 0 max-score) nil) (t (list current-score max-score to-reroll target name)))))

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

; /* *********************************************************************
; Function Name: find-streak
; Purpose: To determine whether a set of dice has a requisite "streak" (face values in a row)
; Parameters:
;           dice-counts, the counts of each face value for this dice set
;           streak-needed, the number of values in a row required
;           streak, an optional paramter defining the current streak value
; Return Value: t if the streak-needed was met, and nil if not
; Reference: none
; ********************************************************************* */
(defun find-streak (dice-counts streak-needed &optional (streak 0))
    (cond 
        ; If the streak reached the required amount, return true.
        ((= streak streak-needed) t)
        ; If there are no more dice to check, and streak-needed was not reached, return nil.
        ((null dice-counts) nil)
        ; If there are dice for this face value, continue the streak.
        ((>= (first dice-counts) 1) (find-streak (rest dice-counts) streak-needed (+ streak 1)))
        ; If there are no dice for this face value, restart the streak.
        (t (find-streak (rest dice-counts) streak-needed))))

; /* *********************************************************************
; Function Name: score-straight
; Purpose: To score a dice set for a Straight category (4 or 5 Straight)
; Parameters:
;           dice, the dice set to score
;           streak-num, the number needed in a row to make this Straight
;           value, the points value of this category if filled
; Return Value: an integer representing the score obtained from this dice set
; Reference: none
; ********************************************************************* */
(defun score-straight (dice straight-num value)
    (cond 
        ; If the requisite streak was met, this scores.
        ((find-streak (count-dice-faces dice) straight-num) value)
        ; Otherwise, this earns no points.
        (t 0)))

; /* *********************************************************************
; Function Name: check-straight-config
; Purpose: To evaluate if the current dice configuration can complete 
;          a 4- or 5-straight
; Parameters:
;             dice, a list of dice. Each die is represented as a pair 
;               of values (face value, lock status).
;             straight-num, an integer. It indicates the length of the straight 
;               to check (either 4 or 5).
;             point-value, an integer. It is the score value for completing 
;               the straight (30 for 4-straight, 40 for 5-straight).
;             config, a list of integers. It represents the required counts 
;               of each die face for a valid straight.
; Return Value: A list containing:
;             - The number of rerolls required
;             - The dice to reroll, if necessary
;             - The updated dice configuration
;             If the straight cannot be completed, the function returns nil.
; Algorithm:
;             1) Count the number of dice for each face in the current dice set.
;             2) Identify which dice are free to reroll based on the input config.
;             3) Find the counts of dice that are scoring or locked.
;             4) Create a set of unchangeable dice.
;             5) Determine the number of rerolls needed.
;             6) Try to add the necessary dice to complete the straight.
;             7) If the straight is completed, return the reroll information 
;                and updated dice; otherwise, return nil.
; Reference: Received help from ChatGPT for getting test cases & header documentation
; ********************************************************************* */
(defun check-straight-config (dice straight-num point-value config)
    (let* 
        ; Get the counts for each face of the dice set.
        ((counts (count-dice-faces dice))
        ; Get which to reroll based on the current dice and input config.
        (to-reroll (cond 
            ((> (score-straight dice straight-num point-value) 0) '(0 0 0 0 0 0))
            (t (count-free-unscored-dice dice config))))
        ; Find counts of which dice are either already scoring, or are locked.
        (scoring-or-locked 
            (count-scored-locked-dice 
                counts
                (count-dice-faces (filter-locked-dice dice)) 
                config))
        ; Turn scoring-or-locked into a list of dice that cannot be altered.
        (set-list (counts-to-dice scoring-or-locked))
        ; Rerolls required is 5 (total dice) - length of the set list.
        (num-rerolls (- 5 (length set-list)))
        ; Find which dice need to be added to complete the configuration.
        (replacements (match-counts counts config))
        ; Add the required dice to the list if possible.
        (target (add-dice set-list replacements)))

        ; Return dice counts to reroll, target counts, and rerolls needed, or NIL if impossible.
        (cond 
            ((> (score-straight target straight-num point-value) 0)
                (list num-rerolls to-reroll (count-dice-faces target)))
            (t nil))))

; /* *********************************************************************
; Function Name: check-five-straight-configs
; Purpose: To evaluate the current dice configuration for completing 
;          a 5-straight
; Parameters:
;             dice, a list of dice. Each die is represented as a pair 
;               of values (face value, lock status).
; Return Value: A list containing:
;             - The number of rerolls required
;             - The dice to reroll, if necessary
;             - The updated dice configuration
;             If the 5-straight cannot be completed, the function returns nil.
; Algorithm:
;             1) Check each possible configuration for a 5-straight.
;             2) Determine the number of rerolls needed for each configuration.
;             3) Compare the reroll counts to find the most efficient configuration.
;             4) Return the configuration with the least rerolls; otherwise, return nil.
; Reference: Received help from ChatGPT for getting test cases & header documentation
; ********************************************************************* */
(defun check-five-straight-configs (dice)
    (let*
        ((config1 (check-straight-config dice 5 40 '(1 1 1 1 1 0)))
         (config2 (check-straight-config dice 5 40 '(0 1 1 1 1 1)))
         (config1-rerolls (first config1))
         (config2-rerolls (first config2)))

        (cond ((null config1) config2)
              ((null config2) config1)
              ((< config1-rerolls config2-rerolls) config1)
              (t config2))))

; /* *********************************************************************
; Function Name: check-four-straight-configs
; Purpose: To evaluate the current dice configuration for completing 
;          a 4-straight
; Parameters:
;             dice, a list of dice. Each die is represented as a pair 
;               of values (face value, lock status).
; Return Value: A list containing:
;             - The number of rerolls required
;             - The dice to reroll, if necessary
;             - The updated dice configuration
;             If the 4-straight cannot be completed, the function returns nil.
; Algorithm:
;             1) Check each possible configuration for a 4-straight.
;             2) Determine the number of rerolls needed for each configuration.
;             3) Compare the reroll counts to find the most efficient configuration.
;             4) Return the configuration with the least rerolls; otherwise, return nil.
; Reference: Received help from ChatGPT for getting test cases & header documentation
; ********************************************************************* */
(defun check-four-straight-configs (dice)
    (let*
        ; Check each possible configuration for a 4-straight.
        ((config1 (check-straight-config dice 4 30 '(1 1 1 1 0 0)))
         (config2 (check-straight-config dice 4 30 '(0 1 1 1 1 0)))
         (config3 (check-straight-config dice 4 30 '(0 0 1 1 1 1)))
         
         ; Extract the reroll counts for comparison.
         (config1-rerolls (first config1))
         (config2-rerolls (first config2))
         (config3-rerolls (first config3)))

        ; Conditionally check for which configuration has the least rerolls.
        (cond 
            ; If only config1 is null, compare config2 and config3.
            ((null config1)
             (cond 
                ((null config2) config3)
                ((null config3) config2)
                ((< config2-rerolls config3-rerolls) config2)
                (t config3)))

            ; If only config2 is null, compare config1 and config3.
            ((null config2)
             (cond 
                ((null config3) config1)
                ((< config1-rerolls config3-rerolls) config1)
                (t config3)))

            ; If only config3 is null, compare config1 and config2.
            ((null config3)
             (cond 
                ((< config1-rerolls config2-rerolls) config1)
                (t config2)))

            ; If all configurations are valid, compare the reroll counts.
            (t
             (cond 
                ((< config1-rerolls config2-rerolls) 
                 (cond ((< config1-rerolls config3-rerolls) config1) (t config3)))
                ((< config2-rerolls config3-rerolls) config2)
                (t config3))))))

; /* *********************************************************************
; Function Name: strategize-straight
; Purpose: To create a strategy for a straight category
; Parameters:
;           dice, the dice set to strategize for
;           straight-num, the number in a row needed (4 or 5)
;           value, the point value of this category
;           name, the name of the category
; Return Value: a strategy to score for this category (or nil if impossible)
; Reference: none
; ********************************************************************* */
(defun strategize-straight (dice straight-num value name)
    (let* 
        ; Get the score given the current dice set.
        ((current-score (score-straight dice straight-num value))
        ; Determine what should be rerolled based on free, unscoring dice and target values.
        (best-config 
            (cond 
                ((= 5 straight-num) (check-five-straight-configs dice)) 
                (t (check-four-straight-configs dice))))
        ; Extract data from the best config found.
        (to-reroll (second best-config))
        (target (third best-config)))
        
        (cond ((null target) nil) (t (list current-score value to-reroll target name)))))

; /* *********************************************************************
; Function Name: score-full-house
; Purpose: To score a dice set for the Full House category
; Parameters:
;           dice, the dice set to score
; Return Value: an integer representing the score obtained from this dice set
; Reference: none
; ********************************************************************* */
(defun score-full-house (dice)
    (let*
        ((max-face1 (max-dice-face (count-dice-faces dice)))
        (max-face2 (max-dice-face (count-dice-faces (filter-out-face dice (first max-face1))))))
        
        ; If there are 3 of one dice face and 2 of another, this scores.
        (cond ((and (= 3 (second max-face1)) (= 2 (second max-face2)))
               25)
              ; Otherwise, 0 points earned.
              (t 0))))

; /* *********************************************************************
; Function Name: get-full-house-target-list
; Purpose: To generate a list of target dice for achieving 
;          a full house (three of one face and two of another).
; Parameters:
;             dice, a list of dice. Each die is represented as a pair 
;               of values (face value, lock status).
; Return Value: A list containing a target set of dice.
;             If a full house is not feasible with the current configuration, or if
;             all dice need to be rerolled, the function returns nil.
; Algorithm:
;             1) Identify the two most frequent dice faces from the current set.
;             2) If no dice are locked, use the most frequent faces.
;             3) If one or two dice faces are locked, prioritize locked faces.
;             4) Return the appropriate target dice for a full house.
;             5) Return nil if a full house is not possible with the current dice 
;                configuration.
; Reference: Received assistance from ChatGPT in documenting function
; ********************************************************************* */
(defun get-full-house-target-list (dice)
    (let*
        ; Get the first and second max dice faces (e.g. if 3 threes and 2 aces, '(3 3) '(1 2)).
        ((max-face1 (max-dice-face (count-dice-faces dice)))
        (max-face2 (max-dice-face (count-dice-faces (filter-out-face dice (first max-face1)))))
        ; Get locked dice and face counts for them.
        (locked-dice (filter-locked-dice dice))
        (locked-counts (count-dice-faces locked-dice))
        ; Get the max dice face from locked dice.
        (locked-max1 (max-dice-face locked-counts))
        ; Get the locked dice and counts, excluding the maximum face.
        (locked-no-max (filter-out-face locked-dice (first locked-max1)))
        (locked-counts-no-max (count-dice-faces locked-no-max))
        ; Get the second max dice face from locked dice.
        (locked-max2 (max-dice-face locked-counts-no-max))
        ; Get the remaining locked dice, without the first or second max.
        (locked-no-max2 (filter-out-face locked-no-max (first locked-max2))))

        (cond 
            ; If there are more than 2 locked faces, or more than 3 locked of the same face
            ; then Full House is impossible. Return nil strategy.
            ((> (max-list (count-dice-faces locked-no-max2)) 0) nil)
            ((and locked-max1 (> (second locked-max1) 3)) nil)

            (t (let*
                ((faces-to-use 
                    (cond
                        ; If no dice are locked, use the mode of entire dice set.
                        ((null locked-max1) (list max-face1 max-face2))
                        ; If only one dice face was locked
                        ((null locked-max2) 
                            (cond 
                                ; If the locked and overall max are the same, use overall values.
                                ((= (first max-face1) (first locked-max1)) 
                                    (list max-face1 max-face2))
                                ; Use locked-max1 first if counts are the same, but different faces.
                                ((= (second max-face1) (second locked-max1))
                                    (list locked-max1 max-face1))
                                ; Otherwise, use it as the second face.
                                (t (list max-face1 locked-max1))))
                        ; If two faces are locked, use those.
                        (t (list locked-max1 locked-max2))))
                ; Extract the face to aim for 3 with.
                (three-face (cond
                             ; Only fill if there is a locked face, or one with > 1 die.
                             ((and (null locked-max1) (= (second (first faces-to-use)) 1)) nil)
                             (t (first (first faces-to-use)))))
                ; Extract the face to aim for 2 with.
                (two-face (cond
                            ; Only fill if this die is locked, or has at least 2 dice.
                            ((null (second faces-to-use)) nil)
                            ((and 
                                (= (count-dice-face locked-dice (first (second faces-to-use))) 0) (< (second (second faces-to-use)) 2)) 
                             nil)
                            (t (first (second faces-to-use))))))

                (cond 
                    ; Edge case where dice are in a five-straight: return nil (nothing to prioritize).
                    ((null three-face) nil)
                    ; If only one die has more than one value (or is locked), target 3 of them.
                    ((null two-face) (add-dice '() (list (list three-face 3))))
                    ; Otherwise, use these dice as the targets.
                    (t (add-dice '() (list (list three-face 3) (list two-face 2))))))))))

; /* *********************************************************************
; Function Name: strategize-full-house
; Purpose: To create a strategy for the Full House category
; Parameters:
;           dice, the dice set to strategize for
; Return Value: a strategy to score for this category (or nil if impossible)
; Reference: none
; ********************************************************************* */
(defun strategize-full-house (dice)
    (let* 
        ; Get the score given the current dice set.
        ((current-score (score-full-house dice))
        ; Determine the target list and counts.
        (target-list (get-full-house-target-list dice))
        (target (count-dice-faces target-list))
        ; Extract data from the best config found.
        (to-reroll (count-free-unscored-dice dice target)))
        
        (cond 
            ((and (null target-list) (filter-locked-dice dice)) nil)
            (t (list current-score 25 to-reroll target "Full House")))))

; /* *********************************************************************
; Function Name: score-yahtzee
; Purpose: To score a dice set for the Yahtzee category
; Parameters:
;           dice, the dice set to score
; Return Value: an integer representing the score obtained from this dice set
; Reference: none
; ********************************************************************* */
(defun score-yahtzee (dice)
    (cond 
        ; If there is a face with 5 values, this scores 50 points.
        ((>= (max-list (count-dice-faces dice)) 5) 50)
        ; Otherwise, this does not score.
        (t 0)))

; /* *********************************************************************
; Function Name: get-yahtzee-target-list
; Purpose: To generate a target list of dice faces for achieving a Yahtzee
;          (five of the same dice face).
; Parameters:
;             dice, a list of dice. Each die is represented as a pair 
;               of values (face value, lock status).
; Return Value: A list containing:
;             - The dice face to aim for five of a kind (or nil if unachievable).
;             If a Yahtzee is not possible, the function returns nil.
; Algorithm:
;             1) Identify the most frequent dice face from the current set.
;             2) Check for locked dice faces and prioritize them for the target.
;             3) If there are multiple locked faces or no suitable target, return nil.
;             4) Return the appropriate target list for achieving a Yahtzee (five of the same face).
;             5) Return nil if a Yahtzee is not feasible with the current configuration.
; Reference: Received assistance from ChatGPT in documenting function
; ********************************************************************* */
(defun get-yahtzee-target-list (dice)
    (let*
        ; Get the max dice face, and its count.
        ((max-face (max-dice-face (count-dice-faces dice)))
        ; Get locked dice and face counts for them.
        (locked-dice (filter-locked-dice dice))
        (locked-counts (count-dice-faces locked-dice))
        ; Get the max dice face from locked dice, and locked dice after its removal.
        (locked-max (first (max-dice-face locked-counts)))
        (locked-no-max (filter-out-face locked-dice locked-max))
        ; Use the locked face to score, if it exists. Otherwise, use the overall mode.
        (face-to-use 
            (cond
                (locked-max locked-max)
                ((= (second max-face) 1) nil)
                (t (first max-face)))))
        (cond 
            ; If there is more than 1 locked face, Yahtzee is impossible. Return nil strategy.
            ((> (max-list (count-dice-faces locked-no-max)) 0) nil)
            ; If face-to-use is null (only if all counts are 1), return nil strategy.
            ((null face-to-use) nil)
            (t (add-dice '() (list (list face-to-use 5)))))))

; /* *********************************************************************
; Function Name: strategize-yahtzee
; Purpose: To create a strategy for the Yahtzee category
; Parameters:
;           dice, the dice set to strategize for
; Return Value: a strategy to score for this category (or nil if impossible)
; Reference: none
; ********************************************************************* */
(defun strategize-yahtzee (dice)
    (let* 
        ; Get the score given the current dice set.
        ((current-score (score-yahtzee dice))
        ; Determine the target list and counts.
        (target-list (get-yahtzee-target-list dice))
        (target (count-dice-faces target-list))
        ; Extract data from the best config found.
        (to-reroll (count-free-unscored-dice dice target)))
        
        (cond 
            ((and (null target-list) (filter-locked-dice dice)) nil)
            (t (list current-score 50 to-reroll target "Yahtzee")))))

; /* *********************************************************************
; Function Name: check-category-strategy
; Purpose: To get a strategy for a particular dice set and category
; Parameters:
;           score-data, the scorecard data for this category
;           dice, the dice set to strategize for
;           category-num, the index of the category to check [1-12]
; Return Value: a strategy to score for this category (or nil if impossible)
; Reference: none
; ********************************************************************* */
(defun check-category-strategy (score-data dice category-num)
    (cond 
        ; Return nil if this category is already full.
        ((> (length score-data) 1) nil)
        ; Otherwise, get the strategy from relevant function.
        (t (cond
                ((= category-num 1) (strategize-multiples dice 1 "Aces"))
                ((= category-num 2) (strategize-multiples dice 2 "Twos"))
                ((= category-num 3) (strategize-multiples dice 3 "Threes"))
                ((= category-num 4) (strategize-multiples dice 4 "Fours"))
                ((= category-num 5) (strategize-multiples dice 5 "Fives"))
                ((= category-num 6) (strategize-multiples dice 6 "Sixes"))
                ((= category-num 7) (strategize-kind dice 3 "Three of a Kind"))
                ((= category-num 8) (strategize-kind dice 4 "Four of a Kind"))
                ((= category-num 9) (strategize-full-house dice))
                ((= category-num 10) (strategize-straight dice 4 30 "Four Straight"))
                ((= category-num 11) (strategize-straight dice 5 40 "Five Straight"))
                ((= category-num 12) (strategize-yahtzee dice))))))

; /* *********************************************************************
; Function Name: check-category-strategies
; Purpose: To get a list of strategies based on input scorecard data
; Parameters:
;           score-data, a list of scorecard data for every remaining category
;           dice, the dice set to strategize for
;           category-num, optional parameter indicating the index of the first category [1-12]
; Return Value: a strategy to score for all categories
; Reference: none
; ********************************************************************* */
(defun check-category-strategies (scorecard dice &optional (category-num 1))
    (cond 
        ; Base case: no more scorecard categories, so empty list.
        ((null scorecard) '())
        ; Add first category strategy onto the rest of them.
        (t (cons 
            (check-category-strategy (first scorecard) dice category-num) (check-category-strategies (rest scorecard) dice (+ category-num 1))))))

; /* *********************************************************************
; Function Name: find-best-strategy
; Purpose: To find the best strategy from a list of strategies
; Parameters:
;           strategies, the list of strategies to pick from
; Return Value: the best strategy from the list
; Reference: none
; ********************************************************************* */
(defun find-best-strategy (strategies)
    (cond 
        ; If no strategies passed in, there is no best strategy.
        ((null strategies) nil)
        (t (let* 
            ; Get the first strategy in the list, and the best from the rest.
            ((curr-strat (first strategies))
            (best-remaining (find-best-strategy (rest strategies)))
            ; Extract max score from each (second value in strategy).
            (curr-strat-score (second curr-strat))
            (best-remaining-score (second best-remaining)))
            
            (cond
                ; If there is no strategy for one, use the other.
                ((null best-remaining) curr-strat)
                ((null curr-strat) best-remaining)
                ; If both strategies exist, use the one with higher max score.
                ; If there is a tie, go for the higher index strategy.
                ((> curr-strat-score best-remaining-score) curr-strat)
                (t best-remaining))))))

; /* *********************************************************************
; Function Name: pick-strategy
; Purpose: To find the best strategy based on current game state
; Parameters:
;           game-data, a list representing the current state of the game
; Return Value: the best strategy determined from the scorecard and dice
; Reference: none
; ********************************************************************* */
(defun pick-strategy (game-data) 
    (find-best-strategy (check-category-strategies (get-scorecard game-data) (get-dice game-data))))

; /* *********************************************************************
; Function Name: filter-available-strategies
; Purpose: To filter out strategies that are unavailable or have 0 contributing dice.
;          Creates a list of category indices based on the list.
; Parameters:
;           strategies, the list of strategies to filter
; Return Value: a filtered list of available categories
; Reference: none
; ********************************************************************* */
(defun filter-available-strategies (strategies)
    (cond  
        ; Start with an empty list of strategies.
        ((null strategies) '())
        ; If this strategy is null, skip it and filter the rest.
        ((null (first strategies)) (filter-available-strategies (rest strategies)))
        (t (let* 
            ((curr-strat (first strategies))
            (curr-score (get-strat-curr-score curr-strat))
            (category-name (get-strat-category-name curr-strat))
            (category-index (get-category-index category-name)))
            ; Only count the first 6 categories if they have at least one contributing die.
            (cond ((and 
                    (<= category-index 6)
                    (= curr-score 0))
                (filter-available-strategies (rest strategies)))

                (t (cons category-index (filter-available-strategies (rest strategies)))))))))

; /* *********************************************************************
; Function Name: get-available-categories
; Purpose: To get a list of available categories given the game data
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: a list of available categories
; Reference: none
; ********************************************************************* */
(defun get-available-categories (game-data)
    (filter-available-strategies 
        (check-category-strategies 
            (get-scorecard game-data)
            (get-dice game-data))))

; /* *********************************************************************
; Function Name: print-target-die
; Purpose: To print a string for a single target face
; Parameters:
;           face, the face value of the dice
;           count, the count of this face value
;           last-print, an optional value indicating that this is the last die to print
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-target-die (face count &optional last-print)
    (cond
        ; Do not print if there are no dice of this face.
        ((= count 0) nil)

        (t 
        ; Print the count and face value (e.g. "3 Two")
        (princ count) 
        (princ " ")
        (princ (cond
            ((= face 1) "Ace")
            ((= face 2) "Two")
            ((= face 3) "Three")
            ((= face 4) "Four")
            ((= face 5) "Five")
            ((= face 6) "Six")))
        ; If plural, add either "es" for "Six" or "s" for anything else.
        (cond ((> count 1)
            (cond 
                ((= face 6) (princ "es"))
                (t (princ "s")))))
        ; If this is not the last value to print, add a comma
        (cond ((not last-print) (princ ", ") nil) (t (princ " ") nil)))))

; /* *********************************************************************
; Function Name: print-target-dice
; Purpose: To print a string for target dice counts
; Parameters:
;           dice, the list of target dice face counts
;           total-dice, the total number of dice to print
;           curr-face, an optional value indicating the current face to print
;           curr-printed, an optional value indicating how many faces have been printed
;           multiple-faces, an optional value indicating if multiple faces have been printed
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-target-dice (dice total-dice &optional (curr-face 1) (curr-printed 0) multiple-faces)
    (cond
        ; If this is the last face to print
        ((= total-dice (+ (first dice) curr-printed))
        (cond (multiple-faces (princ "and ")))
        (print-target-die curr-face (first dice) t))

        ; If there are more faces to print
        (t
        (print-target-die curr-face (first dice))
        (print-target-dice 
            (rest dice) total-dice (+ curr-face 1) (+ curr-printed (first dice)) t))))

; /* *********************************************************************
; Function Name: print-strategy
; Purpose: To print the strategy recommendation for a player or computer 
;          based on the provided strategy and dice configuration.
; Parameters:
;           strategy, a list representing the selected strategy
;           player-name, a symbol representing the player’s name
;           selection, an optional parameter representing whether this is for selecting
;               a category instead of choosing what/whether to reroll
; Return Value: None
; Algorithm:
;           1) Extract the strategy components: current score, max score, 
;              dice to reroll, target dice, and the category name.
;           2) If the player is 'Human:
;              - If the strategy is nil, recommend standing due to no available categories.
;              - If current score equals max score, suggest standing and pursuing 
;                the category with the highest score.
;              - Otherwise, suggest rerolling dice to aim for the target category.
;           3) If the player is not 'Human (assume 'Computer):
;              - Apply similar logic as above but use computer-specific messages.
;           4) Print the appropriate message using `princ` and `terpri` for formatting.
; Reference: Received help from ChatGPT for header documentation
; ********************************************************************* */
(defun print-strategy (strategy player-name &optional selection)
    (let
        ((curr-score (get-strat-curr-score strategy))
        (max-score (get-strat-max-score strategy))
        (to-reroll (get-strat-to-reroll strategy))
        (target (get-strat-target strategy))
        (category-name (get-strat-category-name strategy)))
        
        (cond
            ; Print statements for human recommendations.
            ((equalp player-name 'Human)
                (cond 
                    ; No good strategy was found.
                    ((null strategy)
                    (princ "I recommend that you stand because there are no fillable categories given your current dice set.")
                    (terpri))
                    
                    ; Best strategy is to stand.
                    ((= curr-score max-score)
                    (cond 
                        (selection (princ "I recommend that you select the "))
                        (t (princ "I recommend that you stand and try for the ")))
                    (princ category-name)
                    (princ " category because it gives the maximum possible points (")
                    (princ curr-score)
                    (princ ") among all the options.")
                    (terpri))
                    
                    ; Best strategy is to reroll.
                    (t
                    (princ "I recommend that you reroll and try for the ")
                    (princ category-name)
                    (princ " category with ")
                    (print-target-dice target (sum-list target))
                    (princ "because it gives the maximum possible points (")
                    (princ max-score)
                    (princ ") among all the options. Therefore, ")
                    (print-target-dice to-reroll (sum-list to-reroll))
                    (princ "should be rerolled.")
                    (terpri))))

            ; Computer statements.
            (t
                (cond 
                    ; No good strategy was found.
                    ((null strategy)
                    (princ "The computer plans to stand because there are no fillable categories given its current dice set.")
                    (terpri))
                    
                    ; Best strategy is to stand.
                    ((= curr-score max-score)
                    (cond 
                        (selection (princ "The computer will select the "))
                        (t (princ "The computer plans to stand and try for the ")))
                    (princ category-name)
                    (princ " category because it gives the maximum possible points (")
                    (princ curr-score)
                    (princ ") among all the options.")
                    (terpri))

                    ; Best strategy is to reroll.
                    (t
                    (princ "The computer plans to reroll and try for the ")
                    (princ category-name)
                    (princ " category with ")
                    (print-target-dice target (sum-list target))
                    (princ "because it gives the maximum possible points (")
                    (princ max-score)
                    (princ ") among all the options. Therefore, ")
                    (print-target-dice to-reroll (sum-list to-reroll))
                    (princ "will be rerolled.")
                    (terpri)))))))