; /* *********************************************
; Source Code to serialize (load and save) game data
;   -> Relies on:
;       validation.lisp
;       game-data.lisp
; ********************************************* */

; /* *********************************************************************
; Function Name: load-file
; Purpose: Asks the user for a file name and loads it
; Parameters: None
; Return Value: The contents of the file (list containing round number and scorecard data)
; Reference: none
; ********************************************************************* */
(defun load-file ()
    (princ "Please input the name of the file to load from.")
    (terpri)
    (let ((file-contents (get-file-contents)))
        (princ "Successfully loaded file!")
        (terpri)
        (terpri)
        file-contents
    )
)

; /* *********************************************************************
; Function Name: save-file
; Purpose: Asks the user for a file name and saves to it
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: nil
; Reference: none
; ********************************************************************* */
(defun save-file (game-data)
    (princ "Please input the name of the file to save to")
    (terpri)
    (let ((file-contents (open-save-file)))
        (write (append 
            (list (get-round-num game-data)) 
            (list (get-scorecard game-data))) 
            :stream file-contents)
        (close file-contents)
        (princ "Successfully saved to file! Exiting game.")
        (terpri)
        (terpri)
        nil))

; /* *********************************************************************
; Function Name: serialize-load
; Purpose: Asks the user if they want to serialize, and loads the file if they do
; Parameters: None
; Return Value: The contents of the file (list containing round number and scorecard data), or nil 
;               if not loaded
; Reference: none
; ********************************************************************* */
(defun serialize-load ()
    (princ "Would you like to load the game from a file? (y/n)")
    (terpri)
    (cond ((validate-yes-no) (load-file))
          (t nil)
    )
)

; /* *********************************************************************
; Function Name: serialize-save
; Purpose: Asks the user if they want to save, and saves to a file and exits if they do
; Parameters:
;           game-data, a list containing all saved data for the game
; Return Value: The game data that was passed in
;           Exits the program if the user chooses to save.
; Reference: none
; ********************************************************************* */
(defun serialize-save (game-data)
    (princ "Would you like to save the game to a file? (y/n)")
    (terpri)
    (cond ((validate-yes-no) (save-file game-data) (exit))
          (t game-data)))