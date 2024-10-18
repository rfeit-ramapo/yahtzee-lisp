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
; Function Name: get-value-by-index
; Purpose: Gets a value from a list by its index (starting from 1)
; Parameters:
;           index, the index to get the value at
;           l, the list to traverse
; Return Value: the value at the given index, or nil if out of bounds
; Reference: none
; ********************************************************************* */
(defun get-value-by-index (index l)
    (cond 
        ((null index) nil)
        ((= 1 index) (first l))
        (t (get-value-by-index (- index 1) (rest l)))))

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

