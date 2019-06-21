# **Unit Test Framework**
A simple unit test framework built with Common Lisp, from the book Practical Common Lisp

## Usage

To use this framework is pretty simple, just create your function

Example: 

	(defun exponential (base operator)
	  (let ((x 1))
	    (dotimes (i operator)
	      (setf x (* x base)))
	    x))

Now you create a test expression using the `deftest` macro:

	(deftest test-exponential ()
	  (check
	    (= (exponential 3 3) 27)
	    (= (exponential 4 5) 1024)
	    (= (exponential 2 3) 9)))

Now you call the (test-exponential) on your interpreter and you'll get:

	CL-USER> (text-exponential)
	pass ...(TEXT-EXPONENTIAL): (= (EXPONENTIAL 3 3) 27)
	pass ...(TEXT-EXPONENTIAL): (= (EXPONENTIAL 4 5) 1024)
	FAIL ...(TEXT-EXPONENTIAL): (= (EXPONENTIAL 2 3) 9)
	
