; /* *********************************************
; Source Code for basic utility functions used in other locations
; ********************************************* */

; /* *********************************************************************
; Function Name: sum-list
; Purpose: Add a list of numbers together to get a sum
; Parameters:
;           l, the list to sum
; Return Value: the sum of all values of the list
; Reference: none
; ********************************************************************* */
(defun sum-list (l)
    (cond ((null l) 0)
          (t (+ (first l) (sum-list (rest l))))))

; /* *********************************************************************
; Function Name: list-size
; Purpose: Get the size (length) of a list
; Parameters:
;           l, the list to get the size of
; Return Value: the size (length) of the passed in list
; Reference: none
; ********************************************************************* */
(defun list-size (l)
    (cond ((null l) 0)
          (t (+ 1 (list-size (rest l))))))

; /* *********************************************************************
; Function Name: max-list
; Purpose: Get the maximum value in a list of positive numbers
; Parameters:
;           l, the list to get the max value from
; Return Value: the maximum value in the list, or 0
; Reference: none
; ********************************************************************* */
(defun max-list (l)
    (cond ((null l) 0)
          (t (max (first l) (max-list (rest l))))))