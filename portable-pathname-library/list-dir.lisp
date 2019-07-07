(defun component-present-p (value)
  "Test whether a given component of a pathname is present"
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  "Test whether a pathname is already in directory form"
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  "Converts any pathname to a directory form pathname"
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliaby convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name      nil
	 :type      nil
	 :defaults  pathname)
	pathname)))

(defun directory-wildcard (dirname)
  "Takes a pathname in directory or file form and returns a proper wildcard for
 the given implementation"
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))
