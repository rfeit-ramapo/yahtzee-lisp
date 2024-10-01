

; print category
(defun printCategory (scoreData categoryNum)
    (let 
        (   (info (cond 
                    ((= categoryNum 1) '("Aces" "Any combination" "Sum of dice with the number 1"))
                    ((= categoryNum 2) '("Twos" "Any combination" "Sum of dice with the number 2"))
                    ((= categoryNum 3) '("Threes" "Any combination" "Sum of dice with the number 3"))
                    ((= categoryNum 4) '("Fours" "Any combination" "Sum of dice with the number 4"))
                    ((= categoryNum 5) '("Fives" "Any combination" "Sum of dice with the number 5"))
                    ((= categoryNum 6) '("Sixes" "Any combination" "Sum of dice with the number 6"))
                    ((= categoryNum 7) '("Three of a Kind" "At least three dice the same" "Sum of all dice"))
                    ((= categoryNum 8) '("Four of a Kind" "At least four dice the same" "Sum of all dice"))
                    ((= categoryNum 9) '("Full House" "Three of one number and two of another" "25"))
                    ((= categoryNum 10) '("Four Straight" "Four sequential dice" "30"))
                    ((= categoryNum 11) '("Five Straight" "Five sequential dice" "40"))
                    ((= categoryNum 12) '("Yahtzee" "All five dice the same" "50"))
                  )
            )
        )

        (apply #'format t "~7A~17A~40A~33A" categoryNum info)
        (cond
            ((= (first scoreData) 0) (terpri))
            (t (format t "~10A~8A~5A~%" (second scoreData) (first scoreData) (third scoreData)))
        )
    )
)

; get default scorecard
(defun getDefaultScorecard () '(1 ((0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0))))

; print categories
(defun printCategories (scoreData startAt)
    (cond
        ((null scoreData) (terpri))
        (t
            (printCategory (first scoreData) startAt)
            (printCategories (rest scoreData) (+ startAt 1))
        )
    )
)

; print instructions
(defun printScorecard (input)
    (let 
        (   (scorecard 
            (cond 
                ((null input) (getDefaultScorecard))
                (t input)))
        )

        ; Print Category List
        (princ "Current Scorecard:")
        (terpri)
        (format t "~7A~17A~40A~33A~10A~8A~5A~%" 
        "Index" "Category" "Description" "Score" "Winner" "Points" "Round")
        (princ "========================================================================================================================")
        (terpri)
        (printCategories (first (rest scorecard)) 1)

    )
)

; validate category
; category is a list of '(0) or (int 'Human int) or (int 'Computer int)
(defun validateCategory (category)
    ; ensure the category is a list
    (cond 
        ((not (listp category)) nil)
        ; validate category values
        (t (cond
            ; the first value must be an integer
            ((not (integerp (first category))) nil)
            ; if the first value is 0
            ((= (first category) 0) 
                ; there can not be any remaining values
                (cond 
                    ((null (rest category)) t)
                    (t nil)
                )
            )
            ; the second value must be 'Human or 'Computer
            ((not (or (eq (second category) 'Human) (eq (second category) 'Computer)))
                nil
            )
            ; the third value must be an integer
            ((not (integerp (third category))) nil)

            ; passed all checks
            (t t)
        ))
    )
)

; validate categories
(defun validateCategories (categories numValidated)
    (cond 
        ; base case - ran out of categories to check
        ((null (first categories))
            ; if 12 were validated, then this section is valid
            (cond
                ((= numValidated 12) t)
                (t nil)
            )
        )
        (t
            ; validate the first category in the list and recursively validate the rest
            (and (validateCategory (first categories)) 
                (validateCategories (rest categories) (+ numValidated 1)))
        )
    )
)

; open file
(defun openSerializationFile ()
    (let 
        ((filestream (open (read-line) :if-does-not-exist nil)))
        (cond (filestream filestream)
              (t ((princ "Error: Could not open file. Please try again.") (openSerializationFile)))
        )
    )
)

; get file contents
(defun getFileContents ()
    (let
        ((fileContents (read (openSerializationFile))))
        (cond
            ((cond 
                ; the file is not a list
                ((not (listp fileContents)) 
                    (princ "Error: Invalid file format Please try again.") (getFileContents))
                ; the first value is not an integer
                ((not (integerp (first fileContents)))
                    (princ "Error: Invalid file format Please try again.") (getFileContents))
                ; the second value of the list is not a list
                ((not (listp (second fileContents)))
                    (princ "Error: Invalid file format Please try again.") (getFileContents))
                ; basic checks complete
                ; validate the list of categories
                (t (validateCategories (second fileContents) 0))
            ) (princ "Successfully loaded file!") (terpri) fileContents)
            (t nil)
        )
    )
)

; load a file
(defun loadFile ()
    (princ "Please input the name of the file to load from.")
    (terpri)
    (getFileContents)
)

; validate yes no
(defun validateYesNo ()
    (let 
        ((input (read-line)))
        (cond ((equalp input "y") (terpri) t)
              ((equalp input "n") (terpri) NIL)
              (t (princ "Error: Input must be 'y' or 'n'. Please try again.") (validateYesNo))
        )
    )
)

; serialize
(defun serialize ()
    (princ "Would you like to load the game from a file? (y/n)")
    (terpri)
    (cond ((validateYesNo) (loadFile))
          (t (getDefaultScorecard))
    )
)



(defun printInstructions ()
    (princ "Welcome to Yahtzee! Below you will find the scorecard categories. When asked to input dice, please use face values. When asked for multiple values (dice or categories), please separate each by a space, and enclose in parentheses (e.g. (1 2 3)). All categories should be specified by index. To help visualize the dice, all 'locked' dice (those that have been set aside and cannot be rerolled) are displayed in red. If you need help, enter 'h' to get a recommendation.")
    (terpri)
    (terpri)
    (printScorecard ())
    (princ "To begin the game, please press enter.")
    (terpri)
    (read-line)
)

; run tournament
(defun runTournament ()
    (printInstructions)
    ; save the scorecard here -- EVERYTHING REVOLVES AROUND THIS
    ; possibly edit so that scorecard is parsed separately from printing
        ; that way, more info can be added (like scoring functions and strategies)
    ; refactor to avoid useless variables as well
    (printScorecard (serialize))
    ; play round
        ; function checks at the beginning if the scorecard is full
        ; if it is, it returns
        ; otherwise, it prints round info and determines first player
        ; goes into turn, which then uses the scorecard and dice
)




; /* *********************************************
; Function calls
; ********************************************* */
;(runTournament)