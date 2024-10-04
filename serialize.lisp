; /* *********************************************
; Source Code to serialize (load and save) game data
;   -> Relies on:
;       validation.lisp
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