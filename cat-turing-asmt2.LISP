;;; ===========================================================================
;;;  CMPU-365, Fall 2014
;;;  Turing Machinne
;;;  Cat Morgan
;;; ===========================================================================
 
;;; ===========================================================================
;;; Global Definitions
;;; ===========================================================================

;; Turing Machine Struct
;; Input: a filled hash table
(defstruct TM table)

;; A hash table that uses the #'equal comparator 
(setf hashTable411 (make-hash-table :test #'equal))
(setf hashTableBinary (make-hash-table :test #'equal))

;; Transition Table 4.1.1
(defparameter transitionTable411
  '((0 a 1 _)
    (0 _ -1 _)
    (0 $ 0 >)
    (1 a 0 a)
    (1 _ 0 >)
    (1 $ 1 >)))

;; Binary Transition Table: flip 1s and 0s
(defparameter transitionTableBinary
  '((0 $ 1 >)
    (1 0 2 1)
    (1 1 2 0)
    (2 0 1 >)
    (2 1 1 >)))

;; Turing Machine Tape 1
(defparameter TAPE1 (make-array 8 :initial-contents '($ a a a _ _ _ _)))

;; Turing Machine Tape 2
(defparameter TAPE2 (make-array 8 :initial-contents '($ a _ a _ _ _ _)))

;; Turing Machine Tape 3
(defparameter TAPE3 (make-array 6 :initial-contents '($ _ _ _ _ _)))

;; Turing Machine Tape 4
(defparameter TAPE4 (make-array 4 :initial-contents '($ a a a)))

;; Turing Machine Binary Tape 1
(defparameter TAPEB1 (make-array 6 :initial-contents '($ 1 1 0 0 1)))

;; Turing Machine Binary Tape 2
(defparameter TAPEB2 (make-array 4 :initial-contents '($ 0 0 0)))

;;; ===========================================================================
;;; Functions
;;; ===========================================================================

;;; ===========================================================================
;;; FILLHASH
;;;
;;; Input: hashy, an empty hash table
;;;        table, a list of lists presenting a transition table
;;; Output: ---
;;; Contract: Fill in the given hash table with the given table, 
;;;           using the first two elements of a list as the key
;;;           and the second two elements as the value
;;; ===========================================================================

(defun fillHash (hashy table)
  (maplist
   #'(lambda (listy)
       (setf (gethash 
              ;use the first two elements of a list as a key in the hash table
              (list (first(first listy)) (second(first listy))) hashy) 
             ;use the last two elements of the list as the value
             (list (third(first listy)) 
                   (fourth(first listy))))) 
         ;the table to use for the hash table
   table))

;; printing out the hash table to check correctness
;(maphash 
; #'(lambda (key val)
;     (format t "key: ~A, val: ~A~%" key val)) hashTable)

;;; ===========================================================================
;;; RUN-TM-HELPER: a helper function
;;;
;;; Input: tm, a turing machine
;;;        tape, an array representing a tape
;;;        state, a num for current state in transition table
;;;        pos, a num for current position along tape
;;; Output: SIDE-EFFECT. Prints/modifies the tape based on the 
;;;         transition rules in the given turing machine
;;; Contract: Fill in the given hash table with the given table, 
;;;           using the first two elements of a list as the key
;;;           and the second two elements as the value
;;; ===========================================================================
(defun run-tm-helper
       (tm tape state pos)
  (cond 
   ; if the current state is -1 (an end state) or reached end of tape
   ((or (eq state -1) (=  pos (array-total-size tape)))
    ; print out the final tape
    (format t  "State: ~a, Pos: ~a, Char:~a, Tape:~a~% Finished!~% " 
            state pos (aref tape (- pos 1)) tape)
    NIL)  
   ;else 
   (t
    ; save the data from the current key as a local variable
    (let 
        ((datum (gethash (list state (aref tape pos)) (TM-table tm))))
      ;;print out the current tape
      (format t
              "State: ~a, Pos: ~a, Char:~a, Tape:~a~%"
              state pos (aref tape pos) tape)
      ;;if you do not move right
      (if (not (equal (second datum) '>))
          ;; replace the current character with the new character
          (progn
            (setf (aref tape pos) (second datum))
            ;;print out the new character
            (format t "Printing: ~a~%" (second datum)))
        ;;otherwise move forward and print out the action
        (progn
          (setf pos (+ pos 1))
          (format t "Moving right!~%"))
        )
      ;;move to next element in array with the state changed to be the new state
      (run-tm-helper tm tape (first datum) pos)))))

;;; ===========================================================================
;;; RUN-TM
;;;
;;; Input: tm, a turing machine
;;;        tape, an array representing a tape
;;; Output: SIDE-EFFECT. Prints/modifies the tape based on the 
;;;         transition rules in the given turing machine
;;; Contract: Fill in the given hash table with the given table, 
;;;           using the first two elements of a list as the key
;;;           and the second two elements as the value
;;; ===========================================================================
(defun run-tm
       (tm tape)
  (run-tm-helper tm tape 0 0))

;;; ===========================================================================
;;; Testing
;;; ===========================================================================

;; fill the global hashTable to be the transition table 4.1.1
(fillHash hashTable411 transitionTable411)

;; fill the global hash table to be the binary table 
(fillHash hashTableBinary transitionTableBinary)

;; make a turing machine with the 4.1.1 hash table
(defparameter tm-from-example-4-1-1 (make-TM :table hashTable411))

;;make a turing machine with the binary table
(defparameter tm-binary (make-TM :table hashTableBinary))

(run-tm tm-from-example-4-1-1 TAPE1)
(run-tm tm-from-example-4-1-1 TAPE2)
(run-tm tm-from-example-4-1-1 TAPE3)
(run-tm tm-from-example-4-1-1 TAPE4)
(run-tm tm-binary TAPEB1)
(run-tm tm-binary TAPEB2)
