; /* *********************************************
; Source Code to handle validation of user input
; ********************************************* */

; /* *********************************************************************
; Function Name: validate-category
; Purpose: To ensure validity of the category parameter passed in
; Parameters:
;             category, a list containing 0 or (int Winner int)
;               Winner must be 'Human or 'Computer
; Return Value: t if valid category, and nil if not
; Algorithm:
;           1) Ensure the category is a list
;           2) Ensure the first value is an integer
;               3) If the first value is 0, ensure no values remain in the list and return
;           4) Ensure the second value is either 'Human or 'Computer
;           5) Ensure the third value is an integer, and no values remain afterwards
;           6) Validate the category if nil was not returned already
; Reference: none
; ********************************************************************* */
(defun validate-category (category)
    (cond 
        ; Ensure the category is a list.
        ((not (listp category)) nil)

        (t (cond
            ; The first value must be an integer.
            ((not (integerp (first category))) nil)
            
            ; When the first value is 0...
            ((= (first category) 0) 
                ; There must be no remaining values in the list.
                (cond 
                    ((null (rest category)) t)
                    (t nil)
                )
            )

            ; If the category was filled, the second value must be 'Human or 'Computer.
            ((not (or 
                    (eq (second category) 'Human) 
                    (eq (second category) 'Computer)
             ))
                nil
            )

            ; The third value must be an integer.
            ((not (integerp (third category))) nil)

            ; No values allowed after third value.
            ((fourth category) nil)

            ; Passed all checks.
            (t t)
        ))
    )
)

; /* *********************************************************************
; Function Name: validate-categories
; Purpose: To ensure validity of a list of categories
; Parameters:
;             categories, a list containing categories
;             num-valid, an optional parameter indicating how many categories have been validated
; Return Value: t if the list is valid, and nil if not
; Reference: none
; ********************************************************************* */
(defun validate-categories (categories &optional (num-valid 0))
    (cond 
        ; Base case - ran out of categories to check.
        ((null (first categories))
            ; If all 12 were validated, then this list is valid.
            (cond
                ((= num-valid 12) t)
                (t nil)
            )
        )

        ; Recursive case
        (t
            ; Validate the first category in the list and recursively validate the rest.
            (and (validate-category (first categories)) 
                (validate-categories (rest categories) (+ num-valid 1)))
        )
    )
)

; /* *********************************************************************
; Function Name: validate-yes-no
; Purpose: Validates input as 'y' or 'n'
; Parameters: None
; Return Value: t if 'y' and nil if 'n'
; Reference: none
; ********************************************************************* */
(defun validate-yes-no ()
    (let 
        ; Get user input.
        ((input (read-line)))
        (cond 
            ; Valid input: yes or no (y or n)
            ((equalp input "y") t)
            ((equalp input "n") nil)

            ; Invalid input: output error and retry.
            (t 
            (princ "Error: Input must be 'y' or 'n'. Please try again.")
            (terpri)
            (validate-yes-no)))))

; /* *********************************************************************
; Function Name: open-serialization-file
; Purpose: Opens a file for serialization based on user input
; Parameters: None
; Return Value: The filestream opened
; Reference: none
; ********************************************************************* */
(defun open-serialization-file ()
    (let 
        ; Open the file, and set filestream to contents (or nil if could not open).
        ((filestream (open (read-line) :if-does-not-exist nil)))

        ; Return the filestream if opened successfully.
        (cond (filestream filestream)
              ; If could not open, report error and recursively call until it works.
              (t 
              (princ "Error: Could not open file. Please try again.")
              (terpri)
              (open-serialization-file))
        )
    )
)

; /* *********************************************************************
; Function Name: open-save-file
; Purpose: Opens a file for saving a game based on user input
; Parameters: None
; Return Value: The filestream opened for writing
; Reference: none
; ********************************************************************* */
(defun open-save-file ()
    ; Open the file, overwriting anything already there.
    (open (read-line) :direction :output :if-exists :supersede))


; /* *********************************************************************
; Function Name: get-file-contents
; Purpose: Gets a user-provided file until the contents are valid
; Parameters: None
; Return Value: A list containing the round number and scorecard parsed from the file
; Reference: none
; ********************************************************************* */
(defun get-file-contents ()
    (let
        ; Read the file contents, and set to nil if it was empty.
        ((file-contents (read (open-serialization-file) nil)))

        (cond 
            ((and 
                ; Ensure the contents are a list.
                (listp file-contents)
                ; First of the list should be an integer (round number).
                (integerp (first file-contents))
                ; Second of the list should be another list (scorecard data).
                (listp (second file-contents))
                ; Validate all categories of the scorecard.
                (validate-categories (second file-contents))) 
             ; If validation was passed, return the file contents.
             file-contents
            )

            ; Validation was not passed, so print error and prompt user again.
            (t 
            (princ "Error: Invalid file format. Please try again.")
            (terpri)
            (get-file-contents))
        )
    )
)

; /* *********************************************************************
; Function Name: validate-die-face
; Purpose: Validates input of a die face [1-6]
; Parameters: None
; Return Value: the die face that was input
; Reference: none
; ********************************************************************* */
(defun validate-die-face ()
    (let 
        ; Get user input.
        ((input (read)))
        (cond 
            ; Valid input: must be an integer
            ((not (integerp input))
                (princ "Error: Input must be an integer. Please try again.")
                (terpri)
                (validate-die-face))
            ((and (>= input 1) (<= input 6)) input)
            (t
                (princ "Error: Input must be a valid die face [1-6]. Please try again.")
                (terpri)
                (validate-die-face)))))

; /* *********************************************************************
; Function Name: validate-dice-list
; Purpose: Validates a list of dice
; Parameters:
;           dice-list, a list of dice faces to validate
;           num-to-roll, the number of dice required
; Return Value: t if the list is valid, or nil if not
; Reference: none
; ********************************************************************* */
(defun validate-dice-list (dice-list num-to-roll)
    (cond
        ; If reached end of dice list, validate if input was correct length.
        ((null dice-list) (cond
            (num-to-roll (cond
                ((= num-to-roll 0) t)
                (t nil)))
            (t t)))
        ; Invalid if the dice list has non-integer values, or values outside range [1,6].
        ((not (integerp (first dice-list))) nil)
        ((or (< (first dice-list) 1) (> (first dice-list) 6)) nil)
        ; Recursively check the rest of the dice list.
        (t (cond
            (num-to-roll (validate-dice-list (rest dice-list) (- num-to-roll 1)))
            (t (validate-dice-list (rest dice-list) num-to-roll))))))

; /* *********************************************************************
; Function Name: validate-dice-faces
; Purpose: Validates input of multiple dice faces [1-6]
; Parameters:
;           num-to-roll, optional parameter representing how many dice should be rolled
;           help-allowed, optional parameter indicating if this should return if help requested
; Return Value: a list of validated dice faces
; Reference: none
; ********************************************************************* */
(defun validate-dice-faces (&optional num-to-roll help-allowed)
    (let 
        ; Get user input.
        ((input (read)))
        (cond 
            ; If help requested and help-allowed:
            ((and help-allowed (equalp input 'h)) 'h)
            ; Retry if invalid dice list was input.
            ((not (and (listp input) (validate-dice-list input num-to-roll)))
                (princ "Error: Input must be a list of ")
                (cond 
                    (num-to-roll (princ num-to-roll) (cond
                        ((= num-to-roll 1) (princ " dice face (e.g. (3)). Please try again."))
                        (t (princ " dice faces (e.g. (1 2 3)). Please try again."))))
                    (t (princ "dice faces (e.g. (1 2 3)). Please try again.")))
                (terpri)
                (validate-dice-faces num-to-roll help-allowed))
            (t input))))

; /* *********************************************************************
; Function Name: validate-available-categories
; Purpose: Validates that the user input all available categories correctly
; Parameters:
;           available-categories, a list of available categories for current diceset
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun validate-available-categories (available-categories)
    (let
        ((input (read)))
        (cond
            ; Help functionality
            ((equalp input 'h)
                (princ "The available categories are: ")
                (princ available-categories)
                (terpri)
                (validate-available-categories available-categories))
            ; If the user input a non-list or the wrong values
            ((not (and (listp input) (equalp input available-categories)))
                (princ "Error: Input must be a list of available categories that have at least one contributing die (e.g. (1 3 8)). Please try again.")
                (terpri)
                (validate-available-categories available-categories))
            ; Validated
            (t nil))))

; /* *********************************************************************
; Function Name: validate-pursued-categories
; Purpose: Validates that the user input a valid subset of available categories
; Parameters:
;           input, a user-inputted list of categories to pursue
;           available-categories, a list of available categories for current diceset
; Return Value: t if the user input was valid, or nil if not
; Reference: none
; ********************************************************************* */
(defun validate-pursued-categories (input available-categories)
    (cond 
        ; No more items in the list, so the categories were all valid.
        ((null input) t)
        ; Ensure the input was a list.
        ((not (listp input)) nil)
        ; Validate that the first input is an available category, as well as the rest.
        (t (and 
            (member (first input) available-categories) 
            (validate-pursued-categories (rest input) available-categories)))))
