; /* *********************************************
; Source Code to handle rounds of the game
;   -> Relies on:
;       validation.lisp
;       dice.lisp
;       strategy.lisp
;       validation2.lisp
; ********************************************* */

; /* *********************************************************************
; Function Name: list-available-categories
; Purpose: To list or validate player-input list of available categories
; Parameters:
;           game-data, an object holding info on the game state
;           player-name, the name of the player whose turn it is
; Return Value: the list of available categories obtained
; Reference: none
; ********************************************************************* */
(defun list-available-categories (game-data player-name)
    (let
        ((available-categories (get-available-categories game-data)))
        
        (cond
            ; List available categories for the Computer player.
            ((equal player-name 'Computer)
                (terpri)
                (princ "Listing all available categories, given the dice set so far...")
                (terpri)
                (print available-categories)
            )
            ; For the Human player, validate their input.
            (t 
                (princ "Please list all available scorecard categories, given your current dice set.")
                (terpri)
                (validate-available-categories available-categories))) 
        available-categories))

; /* *********************************************************************
; Function Name: pursue-categories
; Purpose: To list the category or categories the player wants to pursue
; Parameters:
;           game-data, an object holding info on the game state
;           player-name, the name of the player whose turn it is
;           available-categories, a list of currently available categories to choose from
; Return Value: an updated game data object with newly determined strategy
; Reference: none
; ********************************************************************* */
(defun pursue-categories (game-data player-name available-categories)
    (let
        ((best-strategy (pick-strategy game-data)))
        
        (cond
            ; Print chosen strategy for the Computer player.
            ((equalp player-name 'Computer)
                (print-strategy best-strategy 'Computer))
            ; For the Human player, validate their input.
            (t 
                (princ "Please input a list of categories you would like to pursue.")
                (terpri)
                (validate-pursue-categories available-categories best-strategy))) 
        (update-strategy game-data best-strategy)))

; /* *********************************************************************
; Function Name: handle-rerolls
; Purpose: To handle locking dice based on what the player chooses to reroll
; Parameters:
;           game-data, an object holding info on the game state
;           player-name, the name of the player whose turn it is
; Return Value: the updated diceset
; Reference: none
; ********************************************************************* */
(defun handle-rerolls (game-data player-name)
    (let*
        ((best-strategy (get-strategy game-data))
         (diceset (get-dice game-data))
         (to-reroll (get-strat-to-reroll best-strategy))
         (curr-score (get-strat-curr-score best-strategy))
         (max-score (get-strat-max-score best-strategy)))

        (cond
            ((equalp player-name 'Computer)
             (cond
                ; If this is an empty strategy or current score = max score, return early (stand).
                ((null best-strategy) diceset)
                ((= curr-score max-score) diceset)
                ; Otherwise, lock all the dice and return updated dice set.
                (t
                    (lock-other-dice diceset to-reroll))))
            (t
                (princ "Would you like to stand or reroll?")
                (terpri)
                (cond 
                    ; If the player wants to reroll, lock the ones they want to keep.
                    ((validate-stand-reroll best-strategy)
                    (princ "Please input a list of dice faces to reroll.")
                    (terpri)
                    (lock-other-dice
                        diceset
                        (validate-reroll 
                            best-strategy (count-dice-faces (filter-free-dice diceset)))))
                    ; Otherwise, do not alter the diceset.
                    (t diceset))))))

; /* *********************************************************************
; Function Name: print-roll-header
; Purpose: Prints the header for a specific roll
; Parameters:
;           roll-num, the current roll number
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-roll-header (roll-num)
    (princ "=================================")
    (terpri)
    (princ "Roll ")
    (print roll-num)
    nil)

; /* *********************************************************************
; Function Name: handle-rolls
; Purpose: To manage player rolls and update the game state accordingly
; Parameters:
;             game-data, an object holding info on the game state
;             player-name, the name of the player whose turn it is
;             roll-num, an optional parameter indicating the current roll number
;             stand, an optional paramterindicating whether the player has chosen to stand
; Return Value: the updated game-data object
; Algorithm:
;             1) If the player stands or finishes the third roll, lock all dice and 
;               update the game data
;             2) Roll all dice and print the result
;             3) List available categories
;             3) Pursue categories to determine a strategy and update game data accordingly
;             4) Handle rerolls based on strategy and check if the player stands
;             5) Recursively call handle-rolls until the player stands or completes the third roll
; Reference: none
; ********************************************************************* */
(defun handle-rolls (game-data player-name &optional (roll-num 1) stand)
    (cond
        ; Return updated game-data once player stood or finished third roll.
        ((or stand (> roll-num 3)) 
            (update-dice game-data (toggle-dice-lock (get-dice game-data) t)))
        (t
            (let*
                ; Update game data with new roll.
                ((updated-game-data1 (let
                    ((roll-result (roll-all (get-dice game-data))))

                    (princ "Roll Result: ")
                    (print-dice roll-result)
                    (update-dice game-data roll-result)))
                ; Update game data again with strategy from pursue-categories.
                 (updated-game-data2 
                    (pursue-categories updated-game-data1 player-name
                        (list-available-categories updated-game-data1 player-name)))
                ; Update the dice by determining what to reroll.
                (updated-dice (handle-rerolls updated-game-data2 player-name))
                ; Stand if the dice did not change.
                (stand (cond ((equalp (get-dice updated-game-data2) updated-dice) t) (t nil))))
                
                ; Recursively call the function with updated game data, roll num, and stand value.
                (handle-rolls 
                    (update-dice updated-game-data2 updated-dice) 
                    player-name 
                    (+ roll-num 1) 
                    stand)))))

; /* *********************************************************************
; Function Name: choose-category
; Purpose: Chooses a category to fill based on the player's input or the best strategy.
; Parameters:
;             game-data, an object holding info on the game state
;             player-name, the name of the player whose turn it is
; Return Value: the updated game data after filling a category
; Algorithm:
;           1. Retrieve the current scorecard, dice set, available strategies, 
;              and available categories from the game data.
;           2. Determine the best strategy for scoring.
;           3. If no categories are available, print a message indicating that 
;              the turn will be skipped and return early.
;           4. For Human players, prompt them to choose a category by index, validate 
;              the input, and retrieve the associated strategy. For Computer, use the 
;              best strategy's category index directly.
;           5. Retrieve the chosen category's strategy and calculate the points it scores.
;           6. If the turn is skipped, return the current game state without changes.
;           7. For 'Human players, validate the points input and current round number.
;              Print the computer’s actions if the player is 'Computer.
;           8. Build and return a new game state list with the updated scorecard, 
;              unlocked dice, and current round number.
; Reference: Got help from ChatGPT to write function header
; ********************************************************************* */
(defun choose-category (game-data player-name)
    (let*
        ; Get possible categories & strategies, and pick the best.
        (
         (scorecard (get-scorecard game-data))
         (diceset (get-dice game-data))
         (available-strategies (check-category-strategies scorecard diceset))
         (available-categories (get-available-categories game-data))
         (best-strategy (get-strategy game-data))
         ; Save the current round.
         (curr-round (get-round-num game-data))
         ; Choose which category to fill.
         (chosen-category (cond 
            ((null available-categories) 
                (princ "No categories can be filled with this diceset. Skipping turn.")
                (terpri)
                nil)
            ((equalp player-name 'Human) 
                (princ "Please choose a category to fill by its index.")
                (terpri)
                (validate-choose-category available-categories best-strategy))
            (t (get-category-index (get-strat-category-name best-strategy)))))
        ; Get the strategy for the chosen category, and how many points it scores.
        (chosen-category-strategy (get-value-by-index chosen-category available-strategies))
        (points-scored (get-strat-curr-score chosen-category-strategy)))

        ; Return early if skipping turn.
        (cond ((null chosen-category) 
            (list 
                curr-round 
                (get-scorecard game-data) 
                (toggle-dice-lock (get-dice game-data) nil) 
                nil)))

        ; Validate points scored and round input for human; print actions for computer.
        (cond 
            ((equalp player-name 'Human)
                (princ "Please input the points scored for this category.")
                (terpri)
                (validate-points (get-strat-curr-score chosen-category-strategy))
                (princ "Please input the current round.")
                (terpri)
                (validate-round curr-round))
            (t (print-strategy best-strategy 'Computer)))
        
        ; Return a newly built game data with new info.
        (list
            curr-round
            (fill-category 
                (get-scorecard game-data) chosen-category points-scored player-name curr-round)
            (toggle-dice-lock (get-dice game-data) nil) 
            nil)))

; /* *********************************************************************
; Function Name: print-turn-header
; Purpose: Prints the header for a given turn
; Parameters:
;           player-name, the name of the player whose turn it is
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun print-turn-header (player-name)
    (princ "=================================")
    (terpri)
    (cond
        ((equal player-name 'Human) (princ "-- Your Turn --"))
        (t (princ "-- Computer's Turn --")))
    (terpri)
    nil)

; /* *********************************************************************
; Function Name: run-turn
; Purpose: The function to handle running a turn in the game
; Parameters:
;           player-name, the name of the player whose turn it is
;           game-data, the initial game data to start the turn with
; Return Value: updated game data following the completion of this turn
; Reference: none
; ********************************************************************* */
(defun run-turn (player-name game-data)
    (print-turn-header player-name)
    (let*
        ((updated-game-data (handle-rolls game-data player-name))
         (updated-game-data2 (choose-category updated-game-data player-name)))
        
        (print-scorecard updated-game-data2)
        updated-game-data2))