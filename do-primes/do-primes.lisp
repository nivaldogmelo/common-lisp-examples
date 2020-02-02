;; Auxiliar functions to check and find primes
(defun primep (number)
  "Function to test whether a number is prime or not"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Given a number, returns the next prime (If the number is a prime it is returned)"
  (loop for n from number when (primep n) return n))

;; Macro's definitions
(defmacro with-gensyms ((&rest names) &body body)
  "Macro to assing a gensym to variables"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  "Do macro structure to iterate over primes"
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

