; ========================================
; DETERMINE power of i can be divided by n
; ========================================
(defun level-count(n i result)
	(if (= (mod n i) 0)
		(level-count (/ n i) i (+ result 1))
		result
	)
)

; ========================================
; 		APPEND list of factorization
; ========================================
(defun add-element(i m result)
	(nconc result (cons (cons i m) nil))
)

; ========================================
; 		CASE odd-factorization
; ========================================
(defun odd-fact(n jn result)
	(cond 
		((> n 1)
			(let
				(
					(jm (level-count n jn 0))
				)
				(cond
					((> jm 0)
						(odd-fact 
							(/ n (expt jn jm))
							(+ jn 2)
							(add-element jn jm result)
						)
					)
					(T
						(odd-fact n (+ jn 2) result)
					)
				)
			)
		)
		(T
			result
		)
	)
)

; ===================================================
; Using recursion to get the final factorization of N
; ===================================================
(defun fact-get(n result)
	(let
		(
			(m (level-count n 2 0))
		)
		(cond
			((> m 0)
				(odd-fact 
					(/ n (expt 2 m))
					3
					(add-element 2 m result)
				)
			)
			(T
				(odd-fact n 3 result)
			)
		)
	)
)

; ===========================
"	 	FACTORIZATION 		"
; Author: Binh D. Nguyen
; ===========================
(defun f(n)
	(fact-get n '())
)