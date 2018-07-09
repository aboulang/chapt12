;;-*- Package: USER; Syntax: Common-lisp; Mode: LISP -*-
;; $Header: clchap12.scm,v 1.4 86/11/25 11:17:13 las Exp $

;;Chapter 12 -- Numbers

;; $Log:	clchap12.scm,v $
;; Revision 1.4  86/11/25  11:17:13  las
;; Repaired most-positive-fixnum and most-negative fixnum -- was based on 25 bits; corrected
;; to 24.
;; Made expt be eqv to scheme's for now, rather than being unimplemented, to make reader work.
;; 
;; Revision 1.3  86/11/10  15:50:08  allen
;; Fix error in truncate, etc. related to elimination of commonlisp-global-env
;; 
;; Revision 1.2  86/10/19  09:31:42  las
;; Major revision: new environment arrangement (support env is gone),
;; syntaxer mod to mutate symbols used to name functions, reader mods to
;; assist in bootstrapping CL reader and to make life easier for current users
;; 
;; Revision 1.1  86/10/04  10:46:11  allen
;; Initial revision
;;

(define-syntax define-macro
  (macro (pattern . body)
    (let ((the-macro `(macro ,(cdr pattern) ,@body)))
      `(sequence
	(define-syntax ,(car pattern) ,the-macro)	; Compile-time
	(add-syntax! ',(car pattern) ,the-macro)))))


;;;+++Also check setf database knows about incf decf
;;;+++and ldb

;;;+++ Major thing to do still:
;;;+++ Go through the file /usr/blisp/release/scm/utabmb.scm
;;;+++ and use the primitives listed in the MICROCODE-PRIMITIVES-VECTOR
;;;+++ (such as PLUS-FIXNUM, EQUAL-BIGNUM?, etc) instead of scheme's more
;;;+++ generic operators such a + or =, etc. in order to avoid unnecessory
;;;+++ dispatching on the arg types
;;;+++ BETTER YET: Use compiler declarations when they work.
;;;+++ [AGB]


;;;
;;; In the current implementation, there is only one type of float: the scheme
;;; 64 bit floating point number. To quote:
;;; "If one internal format is provided, then it is considered to be
;;; single, but serves also as short, double, and long."
;;; - Page 18 of the silver book
;;; [AGB]

;;; Where we use stuff from spice for trig we use the double stuff for floating
;;; [AGB]

;;;For generality we probably want to split-out the float checks so that we don't
;;;have to revise this stuff if we include more than one float format
;;; [AGB]

;;problem -- there are commonlisp functions defined in these files that make 
;;use both of scheme and commonlisp functions. it is assumed that these files
;;will be syntaxed by the scheme syntaxer, thus commonlisp functions with mutated
;;names will not be found. what to do? (mod is an example -- wants to use rem)
;; idea -- have defun also emit a macro that transforms ordinary refs to refs
;;to mutated symbol. does this eliminate syntaxer mod? what about refs to scheme
;;symbols that are the same, e.g. cl truncate wants to reference scheme truncate


;;;
;;; Implementation-dependent constants
;;;

;;;(proclaim '(inline %signum %multbyi %negate-complex))


;;;Spice was 53bit mantissa with 11 bit exponents
;;;We are    55bit mantissa with  7 bit exponents for vax and
;;;          52                  12               for Sun

;;;
;;;Looks like they were conservative on the long constants so that
;;;we can use them. Cross your fingers....

;;; 
;;; Constants for trig etc.
;;;

;;;+++Need this stuff?
(defun scale-float (f e) (ash f e))

(eval-when (compile)
(defmacro **short-float** (f e)
  `(scale-float (decode-float (coerce ,f 'short-float)) ,e))

(defmacro **single-float** (f e)
  `(scale-float (decode-float (coerce ,f 'single-float)) ,e))

(defmacro **double-float** (f e)
  `(scale-float (decode-float (coerce ,f 'double)) ,e))

(defmacro **long-float** (f e)
  `(scale-float (decode-float (coerce ,f 'long-float)) ,e))
)


;;;+++These need to be conditioned on machine type
(defconstant %short-float-exponent-length 7)
(defconstant %short-float-mantissa-length 55)
(defconstant %single-float-exponent-length 7)
(defconstant %single-float-mantissa-length 55)
(defconstant %double-float-exponent-length 7)
(defconstant %double-float-mantissa-length 55)
(defconstant %long-float-exponent-length 7)
(defconstant %long-float-mantissa-length 55)

(defconstand pi  3.1415926535897932384626433832795028841972L0)
(defconstant %short-pi 3.1415926535897932384)
(defconstant %short-pi/2 1.5707963267948966192)
(defconstant %short-pi/4 .7853981633974)

(defconstant %long-pi/2 1.57079632679489661923L0)
(defconstant %long-pi/4 .78539816339744830962L0)

;;;Used in atan
(defconstant sixth-pi 0.5236)
(defconstant sqrt3 1.7305248)
(defconstant 2-sqrt3 0.26794816)
(defconstant inv-2-sqrt3 3.7320704)

(defconstant %fixnum-length 24)


;;;================================================================
;;;12.10 Implementation Parameters
;;;================================================================

;;;These are here because they are used in functions below

;;;53 -> 55 in our implementation
;;;53 -> 52 for suns

;;;+++These are machine dependant

(defconstant most-positive-fixnum (1- (expt 2 (1- %fixnum-length))))
(defconstant most-negative-fixnum (- (expt 2 (1- %fixnum-length))))

(defconstant most-positive-short-float    (**short-float** 1 1024))
(defconstant most-negative-short-float    (**short-float** -1 1024))
(defconstant least-positive-short-float   (**short-float** 1 -1021))
(defconstant least-negative-short-float   (**short-float** -1 -1021))
(defconstant short-float-epsilon          (**short-float** 1 -52))
(defconstant short-float-negative-epsilon (**short-float** -1 -53))

(defconstant most-positive-single-float    (**single-float** 1 1024))
(defconstant most-negative-single-float    (**single-float** -1 1024))
(defconstant least-positive-single-float   (**single-float** 1 -1021))
(defconstant least-negative-single-float   (**single-float** -1 -1021))
(defconstant single-float-epsilon          (**single-float** 1 -52))
(defconstant single-float-negative-epsilon (**single-float** -1 -53))

(defconstant most-positive-double-float    (**double-float** 1 1024))
(defconstant most-negative-double-float    (**double-float** -1 1024))
(defconstant least-positive-double-float   (**double-float** 1 -1021))
(defconstant least-negative-double-float   (**double-float** -1 -1021))
(defconstant double-float-epsilon          (**double-float** 1 -52))
(defconstant double-float-negative-epsilon (**double-float** -1 -53))

(defconstant most-positive-long-float    (**long-float** 1 1024))
(defconstant most-negative-long-float    (**long-float** -1 1024))
(defconstant least-positive-long-float   (**long-float** 1 -1021))
(defconstant least-negative-long-float   (**long-float** -1 -1021))
(defconstant long-float-epsilon          (**long-float** 1 -52))
(defconstant long-float-negative-epsilon (**long-float** -1 -53))


;;;================================================================
;;; DEFMATHOP - macro for generic arithmetic dispatching
;;;================================================================
;;;Say after me:
;;;"Fixnum Bignum Short-float Single-float Double-float Long-float Ratio Complex"
;;;
;;;(DEFMATHOP OP ARG-STYLE SCHEME-OP INIT ARG-NAMES &BODY TABLE)
;;;
;;; OP - is the operation to be defined
;;;
;;; ARG-STYLE - is one of:
;;;  one-or-more-early one-or-more-exhaustive one-or-more
;;;  zero-or-more two one one-and-optional
;;;
;;; SCHEME-OP - what primitive scheme operator to call for the "s" entries
;;;
;;; INIT - used with zero-or-more for * and + as the return value when called with zero args
;;;        (Also used as the inverse function for / and -)
;;;
;;; ARG-NAMES - override the default arg names by providing your own lambda list here.
;;;
;;;To understand the syntax of the body, here is an example:
;;;  
;;;(defmathop = one-or-more-early = nil nil
;;;  (integer	float		ratio		complex)		;<- arg2 ->
;;;  (s		s     		'nil		=integer-complex)	;integer
;;;  (x		s		(float 2 s)	=float-complex)		;float
;;;  (x		x		=ratio		=ratio-complex)		;ratio
;;;  (x		x		x		=complex)		;complex
;;;  )
;;;
;;;	The first row lists all the allowed types for this operation
;;;     The rest of the row(s) provides what to to for: 
;;;	  type-of(arg1) -> row position
;;;	  type-of(arg2) -> col position
;;;	[If the arg type is one this will only be one row, otherwise it is a square table.]
;;;
;;;	S as an entry means call scheme operator
;;;	X as an entry means this case is symmetric, only with the args switched
;;;	An ATOM as an entry means to call this function
;;;	A quoted ATOM means to return this value directly
;;;	A LIST means to do the coercion function of the first element of the list on 
;;;	  args which is the second position of the list (this itself can be a list) and
;;;	  then call the function represented by the last position. For examples
;;;	   - (float 2 s) means to float the 2nd arg and call scheme =
;;;        - (float (1 2) foobar) means to float the 1st and 2nd and call foobar.
;;;
;;;     [Note that even with the multiarg cases, there is an assumption of some primitive
;;;	 two arg function in which to use]
;;;
;;;To give you a feel of what this is all about, here is how the above defmathop expands:
;;;(PROGN (DEFUN =-ERROR-REPORT
;;;              (NUM1 NUM2)
;;;              (LET ((ARG (IF (TYPEP NUM1 '(OR INTEGER FLOAT RATIO COMPLEX)) NUM2 NUM1)))
;;;                (ERROR "The argument ~A was not of type: Integer, Float, Ratio, or Comple."
;;;                       ARG)))
;;;       (LET ((ARRAY (MAKE-VECTOR 256)))
;;;         (DISPATCH-ASET (FUNCTION %SCHEME-=) ARRAY 1 1)
;;;	 		.
;;;			.
;;;         (DISPATCH-ASET (FUNCTION (LAMBDA (NUMBER &REST MORE-NUMBERS)
;;;                                      NIL)) ARRAY 1 7)
;;;         (DISPATCH-ASET (FUNCTION =INTEGER-COMPLEX) ARRAY 1 8)
;;;         (DISPATCH-ASET (FUNCTION =-ERROR-REPORT) ARRAY 1 0)
;;;			.
;;;			.
;;;         (DISPATCH-ASET (FUNCTION (LAMBDA (ARG1 ARG2)
;;;                                      (FUNCALL (FUNCTION =) ARG1 (FLOAT ARG2))))
;;;                        ARRAY
;;;                        3
;;;                        7)
;;;			.
;;;			.
;;;         (DISPATCH-ASET (FUNCTION =INTEGER-COMPLEX-ARG2) ARRAY 8 1)
;;;  			.
;;;			.
;;;         (DISPATCH-ASET (FUNCTION =-ERROR-REPORT) ARRAY 0 0)
;;;         (SETF (GET '= 'DISPATCHER) ARRAY))
;;;       (CL-DEFINE =
;;;                  (LET NIL
;;;                    (CL-DEFINE %SCHEME-= (ACCESS = 'NIL))
;;;                    (CL-LAMBDA (NUMBER &REST MORE-NUMBERS)
;;;                               (DO ((NLIST MORE-NUMBERS (CDR NLIST)))
;;;                                   ((ATOM NLIST) T)
;;;                                 (DECLARE (LIST NLIST))
;;;                                 (IF (NOT (DISPATCH-ARGS = (CAR NLIST) NUMBER))
;;;                                     (RETURN NIL)))))))


(defun genarith-tag-from-type-name (type-name)
  (let ((tag (cdr (assq 
		   type-name
		   '((null . 9)
		     (fixnum . 1)
		     (bignum . 2)
		     (short-float . 3)
		     (single-float . 4)
		     (double-float . 5)
		     (long-float . 6)
		     (ratio . 7)
		     (complex . 8))))))
    (if tag tag 0)))

(defun genarith-tag-from-object (object)
  (typecase object
    (fixnum 1)
    (bignum 2)
    (short-float 3)
    (single-float 4)
    (double-float 5)
    (long-float 6)
    (ratio 7)
    (complex 8)
    (null 9)					;for optional args
    (otherwise 0)))

(defmacro make-vector (size)
  `(make-array '(,size)))

(defmacro vector-ref (arr ind)
  `(aref ,arr ,ind))

(defmacro vector-set! (arr ind val)
  `(setf (aref ,arr ,ind) ,val))

;;;My own fast 2d matrix stuff
;;;Assumes tables of 16x16 for the 2d case
(defmacro dispatch-aref (arr row &optional col)
  (if col
      `(vector-ref ,arr (+ (ash ,row 4) ,col))
      `(vector-ref ,arr ,row)))

(defmacro dispatch-aset (val arr row &optional col)
  (if col
      `(vector-set! ,arr (+ (ash ,row 4) ,col) ,val)
      `(vector-set! ,arr ,row ,val)))


(defmacro dispatch-args (op arg1 &optional arg2 op-check)
  (if (not op-check)
      (if arg2
	  `(funcall (dispatch-aref (get ',op 'dispatcher)
				   (genarith-tag-from-object ,arg1)
				   (genarith-tag-from-object ,arg2))
		    ,arg1 ,arg2)
	  `(funcall (dispatch-aref (get ',op 'dispatcher)
				   (genarith-tag-from-object ,arg1))
		    ,arg1))
      `(if ,arg2
	   (funcall (dispatch-aref (get ',op 'dispatcher)
				   (genarith-tag-from-object ,arg1)
				   (genarith-tag-from-object ,arg2))
		    ,arg1 ,arg2)
	   (funcall (dispatch-aref (get ',op 'dispatcher)
				   (genarith-tag-from-object ,arg1)
				   ,(genarith-tag-from-object nil))
		    ,arg1))))


(defun master-subtype-of (master types)
  (if (eq master 'unknown) nil
      (dolist (type types)
	(if (subtypep master type) (return (position type types))))))

(defun gen-lambda-args (arg-style)
  (if (eq arg-style 'one) '(arg) '(arg1 arg2)))

;;;Parse the tokens in the table. This has logic to handle the symmetric
;;;case.
;;;Note that the array get stuffed with a list of the function to
;;;dispatch to AND the original token. This is to process the symmetric
;;;cases.
(defun parse-token (token scheme-op arg-style arg1-type arg2-type flip)
  arg2-type
  (let ((name (if flip (intern  (format nil "~A-ARG2" token)) token))
	(new-args (if flip '(arg2 arg1) '(arg1 arg2))))
    (cond
      ;;Drop to scheme
      ((eq token 's)  `(,(intern (format nil "%SCHEME-~A" scheme-op)) ,token))
      ;;Call this macro
      ((atom token)
       ;;++Get rid of this stuff?
       (multiple-value-bind (form success)
	   (macroexpand `(,name ,arg1-type ,arg2-type))
	 (if success
	     `(,form ,token)
	     `(,name ,token))))
      ;;List: either coerce the args or return the value
      (t (case (car token)
	   ;;Return this value
	   (quote
	     `((lambda ,(gen-lambda-args arg-style)
		 ,token)
	       ,token))
	   ;;Insert
	   ;;++Make work with flip case?
	   ;;++Not used yet
	   (insert
	     `((lambda ,(gen-lambda-args arg-style) 
		 ,(macroexpand (second token))
		 (funcall (function ,(if (eq (third token) 's)
					 (intern (format nil "%SCHEME-~A" scheme-op))
					 (third token)))))))
	   ;;coerce the args then call function
	   (otherwise
	     (let ((coercer (first token))
		   (args (second token))
		   (fcn (third token))
		   res)
	       (unless (listp args) (setq args (list args)))
	       `((lambda ,(gen-lambda-args arg-style)
		   (funcall (function ,(if (eq fcn 's)
					   (intern (format nil "%SCHEME-~A" scheme-op)) fcn))
			    ,@(if (eq arg-style 'one)
				  `((,coercer arg))
				  (dotimes (i 2 (if flip res (reverse res)))
				    (if (memq (1+ i) args)
					(push `(,coercer
						,(nth i new-args)) res)
					(push (nth i new-args) res))))
			    ))
		 ,token)))
	   )))))
    

(eval-when (compile)

;;;Helper macro for defmathop that builds the initial array
;;;This array is square with dimensions of number of types
;;;listed in the first line of the table (for the multiarg case). 
;;;This array gets expanded to a larger array to cover all the basic 
;;;commonlisp numeric types later. In this bigger array, those cases that
;;;are not covered by the supplied types are given the error-report
;;;function.
(defmacro stuff-array ()
  `(dotimes (i ntypes t)
     (setq arg1-type (nth i types))
     (setq line (nth i entries))
     (unless line (return))			;Case for one arg
     (dotimes (j ntypes t)
       (setq arg2-type (nth j types))
       (setq token (nth j line))
       (setf (aref dispatch-array-elements i j)
	     (if (eq token 'x)
		 ;;Symmetric case
		 ;;++This may be one lambda too much
		 ;;++Do these lambdas need compiling?
		 ;;
		 ;;Get the old token that is stored in the array
		 ;;and call parse token with the flip flag set.
		 (parse-token (second (aref dispatch-array-elements j i))
			      scheme-op (if (eq arg2-type 'null) 'one arg-style)
			      arg1-type arg2-type t)
		 (parse-token token scheme-op (if (eq arg2-type 'null) 'one arg-style)
			      arg1-type arg2-type nil))))))

;;;Another helper macro
;;;Generate the defun given the different arg styles:
;;;  one-or-more-early one-or-more-exhaustive one-or-more
;;;  zero-or-more two one one-and-optional
(defmacro generate-body ()
  `(case arg-style
     ;;For logic that quits early
     (one-or-more-early
       `(defun ,op ,lambda-list
	  (do ((nlist more-numbers (cdr nlist)))
	      ((atom nlist) T)
	    (declare (list nlist))
	    (if (not (dispatch-args ,op (car nlist) number))
		(return nil)))))
     ;;One case /=
     (one-or-more-exhaustive
       `(defun ,op ,lambda-list
	  (do* ((head number (car nlist))
		(nlist more-numbers (cdr nlist)))
	       ((atom nlist) t)
	    ;;(declare (list nlist))
	    (unless (do* ((nl nlist (cdr nl)))
			 ((atom nl) T)
		      ;;(declare (list nl))
		      (if (dispatch-args ,op head (car nl))
			  (return nil)))
	      (return nil)))))
     
     (max-min
       `(defun ,op ,lambda-list
	  (do ((nlist more-numbers (cdr nlist))
	       (result number))
	      ((null nlist) (return result))
	    (declare (list nlist))
	    (if (dispatch-args ,op (car nlist) result)
		(setq result (car nlist))))))
     ;;This case uses init for the inverse operator
     (one-or-more
       `(defun ,op ,lambda-list
	  (if more-numbers
	      (do ((nlist more-numbers (cdr nlist))
		   (result number))
		  ((atom nlist) result)
		(declare (list nlist))
		(setq result (dispatch-args ,op result (car nlist))))
	      (funcall (function ,init) number))))
     ;;For + and *
     (zero-or-more
       `(defun ,op ,lambda-list
	  (if (null args) ,init
	      (do ((args (cdr args) (cdr args))
		   (res (car args) (dispatch-args ,op res (car args))))
		  ((null args) res)))))
     (one-and-optional
       `(defun ,op ,lambda-list
	  ;;This may be nit-picking but (atan .5 nil) should be an error
	  (if (and (not ,(first (third lambda-list))) ,(third (third lambda-list)))
	      (funcall ',error-report ,(first lambda-list) ,(first (third lambda-list)))
	      (dispatch-args ,op ,(first lambda-list) ,(first (third lambda-list)) t))))
     ((two one)
      `(defun ,op ,lambda-list
	 (dispatch-args ,op ,@lambda-list)))))


;;;Another helper macro to build the lambda list for the dispatcher function
(defmacro build-lambda-list ()
  `(case arg-style
     ((test max-min one-or-more one-or-more-early one-or-more-exhaustive)
      (setq lambda-list '(number &rest more-numbers)))
     (zero-or-more
       (setq lambda-list '(&rest numbers)))
     (two
       (setq lambda-list (or arg-names '(num1 num2))))
     (one
       (setq lambda-list (or arg-names '(number))))
     (one-and-optional
       (setq lambda-list (or arg-names '(number &optional (other nil otherp)))))))

;;;This helper macro is the code to generate all the one arg array settings
(defmacro generate-one-arg-array-stuffs ()
  `(dotimes (i 9 (reverse res))
     (setq rowind (genarith-tag-from-type-name (nth i master-types)))
     (setq row (master-subtype-of (nth i master-types) types))
     (if row
	 (push
	   `(dispatch-aset (function ,(car (aref dispatch-array-elements 0 row)))
			   array ,rowind)
	   res)
	 (push
	   `(dispatch-aset (function ,error-report)
			   array ,rowind)
	   res))))

;;;This helper macro is the code to generate all the multiarg array settings
(defmacro generate-multiarg-array-stuffs ()
  `(dotimes (i 9 (reverse res))
     (setq rowind (genarith-tag-from-type-name (nth i master-types)))
     (setq row (master-subtype-of (nth i master-types) types))
     (dotimes (j 10 t)
       (setq colind (genarith-tag-from-type-name (nth j master-types)))
       (setq col (master-subtype-of (nth j master-types) types))
       (if (and row col)
	   (push
	     `(dispatch-aset
		(function ,(car (aref dispatch-array-elements row col)))
		array ,rowind ,colind)
	     res)
	   (push
	     `(dispatch-aset (function ,error-report) array ,rowind ,colind)
	     res)))))
)

;;;+++To do:
;;; - optimization of 2,1,0 args for multiarg cases
;;; - one and optional arg-style case is not right
(defmacro defmathop (op arg-style scheme-op init arg-names &body table)
  (let* ((types (car table))
	 (etypes (remove 'null types))
	 (master-types '(fixnum bignum short-float
			 single-float double-float
			 long-float ratio complex unknown null))
	 (entries (cdr table))
	 (ntypes (length types))
	 (dispatch-array-elements (if (eq arg-style 'one)
				      (make-array (list 1 ntypes))
				      (make-array (list ntypes ntypes))))
	 (error-report (intern (format nil "~A-ERROR-REPORT" op)))
	 res
	 col row rowind colind
	 arg1-type arg2-type line token
	 lambda-list)
    (build-lambda-list)				;build the lambda list
    (stuff-array)				;stuff the initial array
    `(progn
       ,@(if scheme-op
	     ;;++This stuff probably needs to be declared special
	     `((cl-define ,(intern (format nil "%SCHEME-~A" scheme-op))
			  (access ,scheme-op '()))))
       ;;Define the error reporter
       (defun ,error-report ,@(if (eq arg-style 'one) '((num)) '((num1 num2)))
	  (let ((arg ,@(if (eq arg-style 'one)
			   '(num)
			   `((if (typep num1 '(or ,@etypes)) num2 num1))) ))
	    (error
	      ,(apply #'format
		      nil
		      "The argument ~~A was not of type:~
                       ~#[ none~; ~A~; ~A or ~A~:;~
                       ~@{~#[~1; or~] ~A~^,~}~]."
		      (mapcar (lambda (thing) (string-capitalize thing)) etypes))
	      arg)))
       
       ;;make the array and bind it
       (let ((array (make-vector ,(if (eq arg-style 'one) 9 #.(* 16 16)))))
	 ,@(if (eq arg-style 'one)
	       ;;One arg case
	       (generate-one-arg-array-stuffs)
	       ;;Multiple arg case
	       (generate-multiarg-array-stuffs))
	 ;;Set up the dispatcher property on the symbol of the op.
	 (setf (get ',op 'dispatcher) array))
       ;;Generate the dispatcher function itself
       ,(generate-body))))

;;;================
;;; DEFOP-SEMANTICS
;;;================
;;; (defop-semantics op arg2-type &body semantics)
;;;This macro helps generate all the various itty bitty functions that
;;;are needed for generic arith. It handles the symmetric cases for two
;;;arg functions when the types of the args are flipped by generating a
;;;function postfixed with -ARG2 and munging the body to flip the arg
;;;names. When the two arg types are the same it generates one function.
;;;[Note that this is for 2 arg functions. The 1st arg name should be
;;;ARG1. The second should be ARG2. Also where the operator is not communitive
;;;one can provide the flip semantics by preceeding the list of types
;;;with the keyword :flip.]
;;;
;;; An example is worth 1000 words:
;;;(defop-semantics = complex
;;;  ((integer float ratio) (and (= (imagpart arg2) 0)
;;;			      (= (realpart arg2) arg1)))
;;;  (complex  (and (= (realpart arg1) (realpart arg2))
;;;		 (= (imagpart arg1) (imagpart arg2)))))
;;;
;;; Expands to:
;;;
;;;(PROGN (DEFUN =-INTEGER-COMPLEX
;;;              (ARG1 ARG2)
;;;              (AND (= (IMAGPART ARG2) 0)
;;;                   (= (REALPART ARG2) ARG1)))
;;;       (DEFUN =-INTEGER-COMPLEX-ARG2
;;;              (ARG1 ARG2)
;;;              (AND (= (IMAGPART ARG1) 0)
;;;                   (= (REALPART ARG1) ARG2)))
;;;       (DEFUN =-FLOAT-COMPLEX
;;;              (ARG1 ARG2)
;;;              (AND (= (IMAGPART ARG2) 0)
;;;                   (= (REALPART ARG2) ARG1)))
;;;       (DEFUN =-FLOAT-COMPLEX-ARG2
;;;              (ARG1 ARG2)
;;;              (AND (= (IMAGPART ARG1) 0)
;;;                   (= (REALPART ARG1) ARG2)))
;;;       (DEFUN =-RATIO-COMPLEX
;;;              (ARG1 ARG2)
;;;              (AND (= (IMAGPART ARG2) 0)
;;;                   (= (REALPART ARG2) ARG1)))
;;;       (DEFUN =-RATIO-COMPLEX-ARG2
;;;              (ARG1 ARG2)
;;;              (AND (= (IMAGPART ARG1) 0)
;;;                   (= (REALPART ARG1) ARG2)))
;;;       (DEFUN =-COMPLEX
;;;              (ARG1 ARG2)
;;;              (AND (= (REALPART ARG1) (REALPART ARG2))
;;;                   (= (IMAGPART ARG1) (IMAGPART ARG2)))))
(defmacro defop-semantics (op arg2-type &body semantics &aux types res body func)
  (dolist (item semantics t)
    (if (listp (car item)) (setq types (car item)) (setq types (list (car item))))
    (if (eq (car types) :flip)
	(dolist (type (cdr types))
	  (setq func (car (member (intern (format nil "~A-~A-~A-ARG2" op type arg2-type))
				  res
				  :test (lambda (item elt) (eq item (second elt))))))
	  ;;bash the body
	  (setf (nthcdr 3 func) (cdr item)))
	(dolist (type types)
	  (push `(defun ,(intern (if (eq type arg2-type)
				     (format nil "~A-~A" op type)
				     (format nil "~A-~A-~A" op type arg2-type)))
			(arg1 arg2)
		   ,@(cdr item)) res)
	  (unless (eq type arg2-type)
	    (setq body (copy-tree (cdr item)))
	    (nsubst 'arg1-temp 'arg1 body)
	    (nsubst 'arg1 'arg2 body)
	    (nsubst 'arg2 'arg1-temp body)
	    (push `(defun ,(intern (format nil "~A-~A-~A-ARG2" op type arg2-type))
			  (arg1 arg2)
		     ,@body) res)))))
  `(progn #+ignore (cl-define ,(intern (format nil "%SCHEME-~A" op)) (access ,op '()))
	  ,@(reverse res)))



;;;================================================================
;;;Some useful helper functions


(defmathop type-of-numeric-result two nil nil nil
  (rational    short-float  single-float  double-float  long-float  complex)  ;<- arg2 ->
  ('rational   'short-float 'single-float 'double-float 'long-float 'complex) ;rational
  (x           'short-float 'single-float 'double-float 'long-float 'complex) ;short-float
  (x           x            'single-float 'double-float 'long-float 'complex) ;single-float
  (x           x            x             'double-float 'long-float 'complex) ;double-float
  (x           x            x             x             'long-float 'complex) ;long-float
  (x           x            x             x             x           'complex) ;complex
  ) 


;;;+++ Could use coerce for this
(defmathop typed-zero one nil nil nil
  (rational    short-float  single-float  double-float  long-float  complex)
  ('0          '0.0s0       '0.0f0        '0.0d0        '0.0l0      complex-zero)
  )

(defun complex-zero (arg1)
  (typed-zero (complex-realpart arg1)))


(defmathop typed-zero one nil nil nil
  (rational    short-float  single-float  double-float  long-float  complex)
  ('1          '1.0s0       '1.0f0        '1.0d0        '1.0l0      complex-one)
  )

(defun complex-one (arg1)
  (typed-one (complex-realpart arg1)))


;;;================================================================
;;;Ratio Numbers
(defstruct (ratio
	     ;;(:type (vector integer)) I wish i could do this..
	     ;;	     :named
	     (:constructor %make-ratio
			   (numerator denominator))
	     (:predicate ratiop)
	     (:print-function print-ratio)
	     )
  (numerator 0 :type integer)
  (denominator 1 :type integer))

;;;+++Want to check if this turns into an integer?
(defun make-ratio (numerator denominator)
  ;;++type check needed since this is not at user level?
  (check-type numerator 'integer)
  (check-type denominator 'integer)
  (cond ((zerop denominator) (error "Zero denominator in ratio number"))
	((zerop numerator) 0)
	;;++I don't know if this minus check on the denom is needed
	(t (when (minusp denominator)
	     (setq numerator (- numerator))
	     (setq denominator (- denominator)))
	   ;;++Could check for g being 1.
	   (let ((g (gcd numerator denominator)))
	     (%make-ratio (%scheme-/ numerator g) (%scheme-/ denominator g))))))

;;;Spice lisp from spnum.slisp
#+ignore
(defun make-ratio (x y)
  (multiple-value-bind (q r) (truncate x y)
    (if (zerop r) q
	(let ((gcd (gcd2 x y)))
	  (unless (= gcd 1) (setq x (/ x gcd) y (/ y gcd)))
	  (if (minusp y)
	      (%primitive make-ratio (- x) (- y))
	      (%primitive make-ratio x y))))))


(defun print-ratio (num stream depth)
  depth
  (prin1 (ratio-numerator num) stream)
  (princ "/" stream)
  (prin1 (ratio-denominator num) stream))


;;;================================================================
;;;Complex numbers
(defstruct (complex
	     ;;(:type (vector float)) :named
	     (:constructor %make-complex
			   (realpart imagpart))
	     (:predicate complexp)
	     (:print-function print-complex))
  (realpart 0.0)
  (imagpart 0.0))


(defun print-complex (num stream depth)
  (princ "#C(" stream)
  (prin1 (complex-realpart num) stream)
  (prin1 (complex-imagpart num) stream)
  (princ ")" stream))

;;; Assume that real and imag are same type
(defun make-complex (real imag)
  (if (and (rationalp real)
	   (zerop imag))
      real
      (%make-complex real imag)))
  



;;;For integers
(defun %complex-mag (z)
  (let* ((x (realpart z))
	 (y (imagpart z)))
    (sqrt (+ (* x x) (* y y)))))


;multiplying i (the square root of -1) times a number
(defun %multbyi (z)
  (make-complex (- (imagpart z)) (realpart z)))


(defun magnitude (number)
  (let ((r (complex-realpart number))
	(i (complex-imagpart number)))
    (if (typep r 'float)
	;;This is for scaling to avoid underflow and overflow
	(multiple-value-bind (fract exponent sign)
	    (decode-float (max (abs r) (abs i)))
	  fract sign
	  (setq r (ash r (- exponent)))
	  (setq i (ash i (- exponent)))
	  (ash (sqrt (+ (* r r) (* i i)))
	       exponent))
	;;integer case
	(sqrt (+ (* r r) (* i i))))))



;;;================================================================
;;;12.2 -- Predicates on Numbers
;;;================================================================


;;;================
;;; zerop
;;;================
(defun complex-zerop (number)
  (complex (and (zerop (complex-realpart number))
		(zerop (complex-imagpart number)))))

(defmathop zerop one zero? nil (number)
  (integer	float		ratio		complex)
  (s		s     		'nil		complex-zerop))


;;;================
;;; plusp
;;;================
(defun ratio-plusp (ratio)
  (%scheme-positive? (numerator ratio)))

(defmathop plusp one positive? nil (number)
  (integer	float		ratio)
  (s		s     		ratio-plusp))

;;;================
;;; minusp
;;;================
(defun ratio-minusp (ratio)
  (%scheme-negative? (numerator ratio)))

(defmathop minusp one negative? nil (number)
  (integer	float		ratio)
  (s		s     		ratio-minusp))

;;;================
;;; oddp
;;;================
(defun oddp (number)
  (check-type number integer)
  (odd? number))

;;;================
;;; evenp
;;;================
(defun evenp (number)
  (check-type number integer)
  (even? number))




;;;================================================================
;;;12.3 Comparisons on Numbers
;;;================================================================

;;;(setf (symbol-function '<ratio-arg2) (symbol-function '<ratio))
;;;+++ Choose the operators used more carefully: commonlisp,scheme,or type specific
;;;================
;;;     =
;;;================
(defop-semantics = complex
  ((integer float ratio)
   (and (= (imagpart arg2) 0)
	(= (realpart arg2) arg1)))
  (complex  (and (%scheme-= (realpart arg1) (realpart arg2))
		 (%scheme-= (imagpart arg1) (imagpart arg2)))))
  
(defun =-ratio (arg1 arg2)
  (%scheme-= (* (numerator arg1) (denominator arg2))
	    (* (numerator arg2) (denominator arg1))))


;;;(op arg-style scheme-op init arg-names &body table)
(defmathop = one-or-more-early = nil nil
  (integer	float		ratio		complex)		;<- arg2 ->
  (s		s     		'nil		=-integer-complex)	;integer
  (x		s		(float 2 s)	=-float-complex)	;float
  (x		x		=-ratio		=-ratio-complex)	;ratio
  (x		x		x		=-complex)		;complex
  )


;;;================
;;;   /=
;;;================

;;;Doing it this way is a bit more efficient than defining it based on commonlisp =
(defmathop /= one-or-more-exhaustive = nil nil
  (integer	float		ratio		complex)		;<- arg2 ->
  (s		s     		'nil		=-integer-complex)	;integer
  (x		s		(float 2 s)	=-float-complex)	;float
  (x		x		=-ratio		=-ratio-complex)	;ratio
  (x		x		x		=-complex)		;complex
  )

;;;================
;;;   <
;;;================

(defop-semantics < ratio
  (integer (%scheme-< (* arg1 (denominator arg2))
		     (denominator arg1)))
  (ratio (%scheme-< (* (numerator arg1) (denominator arg2))
		   (* (numerator arg2) (denominator arg1)))))

(defmathop < one-or-more-early < nil nil
  (integer	float		ratio)			;<- arg2 ->
  (s		s     		<-integer-ratio)	;integer
  (x		s		(float 2 s))		;float
  (x		x		<-ratio)		;ratio
  )

;;;================
;;;    >
;;;================
(defop-semantics > ratio
  (integer (%scheme-> (* arg1 (denominator arg2))
		     (denominator arg1)))
  (ratio (%scheme-> (* (numerator arg1) (denominator arg2))
		   (* (numerator arg2) (denominator arg1)))))

(defmathop > one-or-more-early > nil nil
  (integer	float		ratio)			;<- arg2 ->
  (s		s     		>-integer-ratio)	;integer
  (x		s		(float 2 s))		;float
  (x		x		>-ratio)		;ratio
  )

;;;================
;;;    <=
;;;================
(defop-semantics <= ratio
  (integer (%scheme-<= (* arg1 (denominator arg2))
		      (denominator arg1)))
  (ratio (%scheme-<= (* (numerator arg1) (denominator arg2))
		    (* (numerator arg2) (denominator arg1)))))

(defmathop <= one-or-more-early <= nil nil
  (integer	float		ratio)			;<- arg2 ->
  (s		s     		<=-integer-ratio)	;integer
  (x		s		(float 2 s))		;float
  (x		x		<=-ratio)		;ratio
  )

;;;================
;;;    >=
;;;================
(defop-semantics >= ratio
  (integer (>= (* arg1 (denominator arg2))
	      (denominator arg1)))
  (ratio (>= (* (numerator arg1) (denominator arg2))
	   (* (numerator arg2) (denominator arg1)))))

(defmathop >= one-or-more-early >= nil nil
  (integer	float		ratio)			;<- arg2 ->
  (s		s     		>=-integer-ratio)	;integer
  (x		s		(float 2 s))		;float
  (x		x		>=-ratio)		;ratio
  )


;;;================
;;;    max
;;;================

Note that the operator called is the > operator
(defmathop max max-min > nil nil
  (integer	float		ratio)			;<- arg2 ->
  (s		s     		>=-integer-ratio)	;integer
  (x		s		(float 2 s))		;float
  (x		x		>=-ratio)		;ratio
  )

;;;================
;;;    min
;;;================

Note that the operator called is the < operator
(defmathop min min-min < nil nil
  (integer	float		ratio)			;<- arg2 -<
  (s		s     		<=-integer-ratio)	;integer
  (x		s		(float 2 s))		;float
  (x		x		<=-ratio)		;ratio
  )


;;;================================================================
;;; 12.4 Arithmetic Operations
;;;================================================================

;;;================
;;;  +
;;;================
(defop-semantics + ratio
  (integer
    (let* ((darg (denominator arg2))
	   (n (+ (* arg1 darg) (numerator arg2))))
      (%make-ratio n darg)))
  (ratio
    (let* ((narg1 (numerator arg1))
	   (darg1 (denominator arg1))
	   (narg2 (numerator arg2))
	   (darg2 (denominator arg2))
	   (g1 (%scheme-gcd darg1 darg2)))
      (if (= g1 1)
	  (%make-ratio (+ (* narg1 darg2) (* darg1 narg2)) (* darg1 darg2))
	  (let* ((t1 (+ (* narg1 (truncate darg2 g1)) (* (truncate darg1 g1) narg2)))
		 (g2 (%scheme-gcd t1 g1))
		 (t2 (truncate darg1 g1)))
	    (if (= g2 1)
		(%make-ratio t1 (* t2 darg2))
		(if (= g2 0)
		    0
		    (let* ((nn (truncate t1 g2))
			   (t3 (truncate darg2 g2))
			   (nd (if (= t2 1) t3 (* t2 t3))))
		      (if (= nd 1) nn (%make-ratio nn nd))))))))))


(defop-semantics + complex
  ((ratio float integer)
   (make-complex
     (+ (realpart arg2) arg1)
     (imagpart arg2)))
  (complex
    (make-complex
      (+ (realpart arg1) (realpart arg2))
      (+ (imagpart arg1) (imagpart arg2)))))

 
;;;(op arg-style scheme-op init arg-names &body table)
(defmathop + zero-or-more + 0 nil
  (integer	float		ratio		complex)		;<- arg2 ->
  (s		s     		+-integer-ratio	+-integer-complex)	;integer
  (x		s		(float 2 s)     +-float-complex)	;float
  (x		x		+-ratio		+-ratio-complex)	;ratio
  (x		x		x		+-complex)		;complex
  )


;;;================
;;;  *
;;;================
(defop-semantics * ratio
  (integer
    (if (zerop arg1) 0
	(let* ((narg (numerator arg2))
	       (darg (denominator arg2))
	       (gcd (%scheme-gcd arg1 darg)))
	  (if (= gcd 1)
	      (%make-ratio (* arg1 narg) darg)
	      (let ((nn (* (truncate arg1 gcd) narg))
		    (nd (truncate darg gcd)))
		(if (= nd 1) nn
		    (%make-ratio nn nd)))))))
  (ratio
    (let* ((narg1 (numerator arg1))
	   (darg1 (denominator arg1))
	   (narg2 (numerator arg2))
	   (darg2 (denominator arg2))
	   (g1 (%scheme-gcd narg1 darg2))
	   (g2 (%scheme-gcd darg1 narg2))
	   (nn (* (if (= g1 1) narg1 (truncate narg1 g1))
		  (if (= g2 1) narg2 (truncate narg2 g2))))
	   (nd (* (if (= g2 1) darg1 (truncate darg1 g2))
		  (if (= g1 1) darg2 (truncate darg2 g1)))))
      (if (= nd 1) nn (%make-ratio nn nd)))))

(defop-semantics * complex
  ((integer float ratio)
   (%make-complex
	      (* arg1 (realpart arg2))
	      (* arg1 (imagpart arg2))))
  (complex
    (let* ((rarg1 (realpart arg1))
	   (iarg1 (imagpart arg1))
	   (rarg2 (realpart arg2))
	   (iarg2 (imagpart arg2)))
      (make-complex
	(- (* rarg1 rarg2) (* iarg1 iarg2))
	(+ (* rarg1 iarg2) (* iarg1 rarg2))))))

;;;(op arg-style scheme-op init arg-names &body table)
(defmathop * zero-or-more * 1 nil
  (integer	float		ratio		complex)		;<- arg2 ->
  (s		s     		*-integer-ratio	*-integer-complex)	;integer
  (x		s		(float 2 s)     *-float-complex)	;float
  (x		x		*-ratio		*-ratio-complex)	;ratio
  (x		x		x		*-complex)		;complex
  )



;;;================
;;;  -
;;;================

(defop-semantics - ratio
  (integer
    (let* ((darg2 (denominator arg2))
	   (n (- (* arg1 darg2) (numerator arg2))))
      (%make-ratio n darg2)))
  ((:flip integer)
   (let* ((darg1 (denominator arg1))
	  (n (- (numerator arg1) (* arg2 darg1))))
     (%make-ratio n darg1)))
  (ratio
    (let* ((narg1 (numerator arg1))
	   (darg1 (denominator arg1))
	   (narg2 (numerator arg2))
	   (darg2 (denominator arg2))
	   (g1 (%scheme-gcd darg1 darg2)))
      (if (= g1 1)
	  (%make-ratio (- (* narg1 darg2) (* darg1 narg2)) (* darg1 darg2))
	  (let* ((t1 (- (* narg1 (truncate darg2 g1)) (* (truncate darg1 g1) narg2)))
		 (g2 (%scheme-gcd t1 g1))
		 (t2 (truncate darg1 g1)))
	    (if (= g2 1)
		(%make-ratio t1 (* t2 darg2))
		(let* ((nn (truncate t1 g2))
		       (t3 (truncate darg2 g2))
		       (nd (if (= t2 1) t3 (* t2 t3))))
		  (if (= nd 1) nn (%make-ratio nn nd)))))))))


(defop-semantics - complex
  ((ratio float integer)
   (%make-complex
     (- arg1 (realpart arg2))
     (imagpart arg2)))
  ((:flip ratio float integer)
   (%make-complex
     (- (realpart arg1) arg2)
     (imagpart arg1)))
  (complex
    (make-complex
      (- (realpart arg1) (realpart arg2))
      (- (imagpart arg1) (imagpart arg2)))))


;;;(op arg-style scheme-op init arg-names &body table)
(defmathop - one-or-more - negate nil
  (integer	float		ratio		complex)		;<- arg2 ->
  (s		s     		--integer-ratio	--integer-complex)	;integer
  (x		s		(float 2 s)     --float-complex)	;float
  (x		x		--ratio		--ratio-complex)	;ratio
  (x		x		x		--complex)		;complex
  )

;;;================
;;;  negate
;;; (one arg case 
;;;  of -)
;;;================

;;;+++Do better sometime: the redispatching on type is not necessary
;;;+++since we know the type and could pass it down somehow.... Sigh

(defun negate-ratio (arg)
  (%make-ratio (- (ratio-numerator arg)) (ratio-denominator arg)))

(defun negate-complex (arg)
  (%make-complex (- (realpart arg)) (- (imagpart arg))))

;;;(op arg-style scheme-op init arg-names &body table)
(defmathop negate one - negate nil
  (integer	float		ratio		complex)
  (s		s     		negate-ratio	negate-complex))



;;;================
;;;  /
;;;================


;;;What r was, I don't know!
#+ignore
(defun %bignum/fixnum (arg1 arg2 r)
  (let ((g (%scheme-gcd arg2 r)))
    (if (= g 1)
	(%make-ratio arg1 arg2)
	(%make-ratio (truncate arg1 g) (truncate arg2 g)))))

;;;+++This is very redundant
(defop-semantics / bignum
  (fixnum
    (if (zerop arg1) 0
	(let ((g (%scheme-gcd arg1 arg2)))
	  (if (= g 1)
	      (%make-ratio arg1 arg2)
	      (%make-ratio (truncate arg1 g) (truncate arg2 g))))))
  ((:flip fixnum)
    (if (zerop arg1) 0
	(let ((g (%scheme-gcd arg1 arg2)))
	  (if (= g 1)
	      (%make-ratio arg1 arg2)
	      (%make-ratio (truncate arg1 g) (truncate arg2 g))))))
  (bignum
    (if (zerop arg1) 0
	(let ((g (%scheme-gcd arg1 arg2)))
	  (if (= g 1)
	      (%make-ratio arg1 arg2)
	      (%make-ratio (truncate arg1 g) (truncate arg2 g)))))))



(defop-semantics / ratio
  (integer
    (if (zerop arg1) 0
	(let* ((narg2 (numerator arg2))
	       (darg2 (denominator arg2))
	       (gcd (%scheme-gcd arg1 narg2)))
	  (if (= gcd 1)
	      (let ((nn (* arg1 darg2)))
		(if (= narg2 1) nn (%make-ratio nn narg2)))
	      (let ((nn (* (truncate arg1 gcd) darg2))
		    (nd (truncate narg2 gcd)))
		(if (= nd 1) nn
		    (%make-ratio nn nd)))))))
  ((:flip integer)
   (let* ((narg1 (numerator arg1))
	  (darg1 (denominator arg1))
	  (gcd (%scheme-gcd narg1 arg2)))
     (if (= gcd 1)
	 (%make-ratio narg1 (* arg2 darg1))
	 (%make-ratio (truncate narg1 gcd) (* (truncate arg2 gcd) darg1)))))
  (ratio
    (let* ((narg1 (numerator arg1))
	   (darg1 (denominator arg1))
	   (narg2 (numerator arg2))
	   (darg2 (denominator arg2))
	   (g1 (%scheme-gcd narg1 narg2))
	   (g2 (%scheme-gcd darg1 darg2))
	   (nn (* (if (= g1 1) narg1 (truncate narg1 g1))
		  (if (= g2 1) darg2 (truncate darg2 g2))))
	   (nd (* (if (= g2 1) darg1 (truncate darg1 g2))
		  (if (= g1 1) narg2 (truncate darg2 g1)))))
      (if (= nd 1) nn (%make-ratio nn nd)))))


(defop-semantics / complex
  ((integer float ratio)
   (let* ((rarg2 (realpart arg2))
	  (iarg2 (imagpart arg2))
	  (dn (+ (* rarg2 rarg2) (* iarg2 iarg2))))
     (%make-complex
       (/ (* arg1 rarg2) dn)
       (/ (- (* arg1 iarg2)) dn))))
  ((:flip integer float ratio)
   (%make-complex
     (/ (realpart arg1) arg2)
     (/ (imagpart arg1) arg2)))
  (complex
    (let* ((rarg1 (realpart arg1))
	   (iarg1 (imagpart arg1))
	   (rarg2 (realpart arg2))
	   (iarg2 (imagpart arg2))
	   (dn (+ (* rarg2 rarg2) (* iarg2 iarg2))))
      (make-complex
	(/ (+ (* rarg1 rarg2) (* iarg1 iarg2)) dn)
	(/ (- (* iarg1 rarg2) (* rarg1 iarg2)) dn)))))

;;;(op arg-style scheme-op init arg-names &body table)
(defmathop / zero-or-more / recip nil
  (fixnum	bignum		float	ratio		complex)		;<- arg2 ->
  (make-ratio	/-fixnum-bignum	s	/-integer-ratio	/-integer-complex)	;fixnum
  (x		/-bignum	s	/-integer-ratio	/-integer-complex)	;bignum
  (x		x		s	(float 2 s)     /-float-complex)	;float
  (x		x		x	/-ratio		/-ratio-complex)	;ratio
  (x		x		x	x		/-complex)		;complex
  )


;;;================
;;;  recip
;;; (one arg case 
;;;  of \)
;;;================

(defun recip-ratio (arg)
  (make-ratio (ratio-denominator arg) (ratio-numerator arg)))

;;;+++These could be more efficient
(defun recip-complex (arg)
  (/ 1 arg))

(defun recip-fixnum (arg)
  (/ 1 arg))

(defun recip-bignum (arg)
  (/ 1 arg))

;;;(op arg-style scheme-op init arg-names &body table)
(defmathop recip one - recip nil
  (fixnum	bignum		float	ratio		complex)
  (recip-fixnum	recip-bignum	s	recip-ratio	recip-complex))


;;;+++Also worry about incf and decf [pg 201]
;;;================
;;;  1+
;;;================
(defun %1+ratio (arg)
  (let ((darg (denominator arg)))
    (%make-ratio (+ (numerator arg) darg) darg)))

(defun %1+complex (arg)
  (make-complex
	      (+ (complex-realpart arg) 1)
	      (complex-imagpart arg)))

(defmathop 1+ one 1+ nil nil
  (integer	float	ratio		complex)
  (s		s	%1+ratio	%1+complex))



;;;================
;;;  1-
;;;================

(defun %1-ratio (arg)
  (let ((darg (denominator arg)))
    (%make-ratio (- (numerator arg) darg) darg)))


(defun %1-complex (arg)
  (make-complex
	      (- (realpart arg) 1)
	      (imagpart arg)))


(defmathop 1- one -1+ nil nil
  (integer	float	ratio		complex)
  (s		s	%1-ratio	%1-complex))


;;;================
;;; Conjugate
;;;================

#+ignore
(defmathop conjugate one nil nil nil
  (integer	float	ratio	complex)
  (values	values	values	%conjugate-complex))

(defun conjugate (number)
  (etypecase number
    (complex (%make-complex (complex-realpart number) (- (complex-imagpart number))))
    (number number)))


;;;================
;;; GCD
;;;================

;;; GCD -- gcd of an arbitrary number of integers.
;;; Since the probability is >.6 that the GCD of two numbers is 1, 
;;; it is worth to time to check for GCD=1 and quit if so.
;;; However, in this case some arguments may never be type-checked.

;;  "Returns the greatest common divisor of zero or more integers"

;;;This is used other places as well...
(cl-define %scheme-gcd (access gcd '()))

(cl-define gcd
  (let ()
    (cl-lambda (&rest args) 
	       (if (null args) 0
		   (do* ((gcd (car args))
			 (args (cdr args) (cdr args)))
			((null args) gcd)
		     (cond ((= gcd 1)
			    ;;Finish early but still check the types
			    (dolist (x args)
			      (check-type x 'integer))
			    (return 1)))
		     (check-type (car args) 'integer)
		     (setq gcd (%scheme-gcd gcd (car args))))))))


;;;================
;;; LCM
;;;================

;;; LCM -- least common multiple.  At least one argument is required.
;;; We must quit when LCM=0 is computed to avoid division by zero.
;;; In this case, some arguments may never be type-checked.
;;; Timings show time is saved by avoiding division when GCD=1.

;;; Lcm2 is defined this way so that operations won't unnecessarily bignumify.
(defmacro lcm2 (n m)
  `(let ((the-gcd (%scheme-gcd ,n ,m)))
     (* (if (= the-gcd 1) (%scheme-max ,n ,m) (/ (%scheme-max ,n ,m) the-gcd))
	(%scheme-min ,n ,m))))


(defun lcm (integer &rest more-integers)
  (do ((more-integers (cons integer more-integers) (cdr more-integers))
       (lcm 1 (lcm2 lcm (car more-integers))))
      ((null more-integers) lcm)
    (cond ((check-type (car more-integers) 'integer))
	  ((zerop (car more-integers))				;Result is zero.
	   (dolist (arg (cdr more-integers))
	     (check-type arg 'integer))
	   (return 0)))))


;;;================================================================
;;;12.5.1 Exponential and Logarithmic Functions
;;;================================================================

;;;+++Some of the trig functions below have not been converted to the 
;;;+++table format because the case analysis of the code is too hard for
;;;+++me to justify the time to do it. [AGB]

#||
(defun collect-vars (sexp &aux list)
  (if (listp (second sexp))
      (loop for exp in (cdr sexp)
	    do (collect-vars exp))
      (push (second sexp) list))
  list)

(defun do-universe (vars cond-body)
  (if (listp vars)
    (loop for num in (0 1 -1 -1/2 1/2 -.5 .5 #c(0.0 0.0) #c(1 1) #c(-1.0 1.0))
	  do (set (car vars) num)
	  do (do-universe (cdr vars) cond-body))
    ()))

(defun gts (cond-clause &aux vars)
  (loop for clause in (cdr cond-clause)
	when (listp (car clause))
	do (setq vars (union vars (collect-vars (car clause)))))
  vars)
	
||#

;;;================
;;; EXP
;;;================ 

;compute exp(z) where z is complex
;is called by exp
(defun complex-exp (arg)
  (let* ((x (complex-realpart arg))
	 (y (complex-imagpart arg)))
	 (* (exp x) (cis y)))) 


;;;(op arg-style scheme-op init arg-names &body table)
(defmathop exp one exp nil nil
  (integer		float		ratio		complex)
  (s			s		(float 1 s)	complex-exp))




;;;================
;;;  EXPT
;;;================
;;;+++To use scheme for some of this or not to - that is the question
    
;;; Function calculates the value of x raised to the nth power.
;;; This function calculates the successive squares of base,
;;; storing them in newbase, halving n at the same time.  If
;;; n is odd total is multiplied by base, at any given time (fix later)
;;;Base can be ratio or integer also complex
(defun intexp (base power)
  (cond ((minusp power)
	 (/ (intexp base (- power))))
	((and (rationalp base)
	      (not (integerp base)))
	 ;;++Could be a make-rational.
	 (/ (intexp (numerator base) power) (intexp (denominator base) power)))
	((and (integerp base) (= base 2))
	 (ash 1 power))
	(t (do ((nextn (ash power -1) (ash power -1))
		(total (if (oddp power) base 1)
		       (if (oddp power) (* base total) total)))
	       ((zerop nextn) total)
	     (setq base (* base base))
	     (setq power nextn)))))


;compute (complex)^n where n is an integer
;some round-off error if n is not a fixnum
(defun complex-expt (z n)
  (* (expt (%complex-mag z) n) (cis (* n (phase z)))))


;this function computes z ^ w where w is a complex number
;it can also be used for any positive real number.
(defun complex-expt-compow (z w)
  (exp (* w (log z))))

(cl-define %scheme-expt (access expt '()))
;;; This function calculates x raised to the nth power.  It separates
;;; the  cases by the type of n, for efficiency reasons, as powers can
;;; be calculated more efficiently if n is a positive integer,  Therefore,
;;; All integer values of n are calculated as positive integers, and
;;; inverted if negative.

(defun expt (x n)
  (cond
    ;;Ratio or integer to integer
    ((and (rationalp x) (integerp n)) (intexp x n))
    ((zerop x) (if (plusp n) x
		   (error "~A to a non-positive power ~A." x n)))
    ;;Complex to integer
    ((and (complexp x) (integerp n)) (intexp x n))
    ;;complex or pos number to complex
    ((complexp n) (if (or (complexp x) (plusp x)) (complex-expt-compow x n)
		      (error "~A negative number to a complex power ~A." x n)))
    ;;complex to integer
    ((complexp x) (complex-expt x n))
    ;;negative number to non-integer
    ((and (not (integerp n)) (minusp x))
     (exp (* n (log x))))
    ;;Pop down to scheme
    (t (%scheme-expt x n))))


;;;================
;;; LOG
;;;================

;ATTENTION:
;log(exp(z)) <> z most of the time  when z is complex 
;exp(log(z)) = z all of the time.   when z is complex

(cl-define %scheme-log (access log '()))

;natural log of a complex number
(defun %complex-log (z)
  (complex (log (%complex-mag z)) (phase z)))


(eval-when (compile)
  #+ignore
  (defmacro log-base (type)
  `(/ (log arg1) (log arg2)))
  (defun %log-base (x base)
    (/ (log x) (log base))))

(defun %log (number)
  (if (minusp number)
      (complex (log (abs number)) (float pi number))
      (%scheme-log number)))

#+ignore
(defun log (number &optional (base nil base-supplied))
  (cond (base-supplied (log-base number base))
	((and (not (complexp number)) (minusp number))
	  (complex (log (abs number)) (float pi number)))
	(t (etypecase number
	     (float   (%scheme-log number))
	     (complex (%complex-log number))
	     (integer (%scheme-log number))
	     (ratio   (%scheme-log (float number)))))))


(defmathop log one-and-optional log nil (number &optional (base nil base-supplied))
  (integer	float		ratio		complex		null)		;<- arg2 ->
  (%log-base	%log-base	%log-base	%log-base	%log)		;integer
  (%log-base	%log-base	%log-base	%log-base	%log)		;float
  (%log-base	%log-base	%log-base	%log-base	(float 1 %log))	;ratio
  (%log-base	%log-base	%log-base	%log-base	%complex-log))	;complex

;;;================
;;; SQRT
;;;================
(cl-define %scheme-sqrt (access sqrt '()))

;the square root of a complex number returns two roots
;this function returns the primary root
(defun complex-root (z)
  (* (exp (/ (%multbyi (phase z)) 2)) (sqrt (%complex-mag z))))


(defun sqrt (number)
  (if (and (not (complexp number)) (minusp number))
      (complex 0 (sqrt (abs number)))
      (etypecase number
	(float   (%scheme-sqrt number))
	(complex (complex-root number))
	(integer (%scheme-sqrt number))
	(ratio   (%scheme-sqrt (float number)))
	)))


;;;================
;;; ISQRT
;;;================
;;; ISQRT:  Integer square root - isqrt(n)**2 <= n
;;; Upper and lower bounds on the result are estimated using integer-length.
;;; On each iteration, one of the bounds is replaced by their mean.
;;; The lower bound is returned when the bounds meet or differ by only 1.
;;; Initial bounds guarantee that lg(sqrt(n)) = lg(n)/2 iterations suffice.

;;(deftype non-negative-integer () `(integer 0))

(defun isqrt (n)
  ;;++ Could use: (check-type n (integer 0))
  (if (and (integerp n) (not (minusp n)))
      (do* ((lg (integer-length n))
	    (lo (ash 1 (ash (1- lg) -1)))
	    (hi (+ lo (ash lo (if (oddp lg) -1 0)))))	;tighten by 3/4 if possible.
	   ((<= (1- hi) lo) lo)
	(let ((mid (ash (+ lo hi) -1)))
	  (if (<= (* mid mid) n) (setq lo mid) (setq hi mid))))
      (error "Isqrt: ~S argument must be a nonnegative integer" n)))



;;;================================================================
;;;12.5.2 Trigonometric and Related functions
;;;================================================================

;;;================
;;;  ABS
;;;================

;;; Absolute value of various random numbers.
;;; %abs-ratio accepts a single ratio and returns its absolute value.
(defun abs-ratio (x)
  (let ((n (numerator x))
	(d (denominator x)))
    (if (%scheme-negative? n)
	(%make-ratio (%scheme-- n) d)
	x))) 

(defmathop abs one abs nil (number)
  (integer		float		ratio		complex)
  (s			s		abs-ratio	magnitude))


;;;================
;;; PHASE
;;;================

;;;+++ This call was originally atan (realpart number) (imagpart number)
;;;+++ but it gave imprecise results. -- original spice comments
;;; I put it back to atan. Lets see how it works out. [AGB]
(defun phase-complex (number)
  (atan (/ (imagpart number) (realpart number))))

(defun phase-number (number)
  (if (minusp number) (float pi number) (typed-zero number)))

(defmathop phase one nil nil (number)
  (rational		float 		complex)
  (phase-number		phase-number	phase-complex))


;;;================
;;; SIGNUM
;;;================

;;;+++This repetition is booring, but you see that this will run FAST!
(defun integer-sig (number)
  (if (%scheme-zero? number) number
      (if (%scheme-plus? number) 1 -1)))

(defun short-sig (number)
  (if (%scheme-zero? number) number
      (if (%scheme-plus? number) 1s0 -1s0)))

(defun single-sig (number)
  (if (%scheme-zero? number) number
      (if (%scheme-plus? number) 1f0 -1f0)))

(defun double-sig (number)
  (if (%scheme-zero? number) number
      (if (%scheme-plus? number) 1d0 -1d0))) 

(defun long-sig (number)
  (if (%scheme-zero? number) number
      (if (%scheme-plus? number) 1l0 -1l0)))

(defun ratio-sig (number)
  (if (ratio-plusp number) 1 -1))

(defun complex-sig (number)
  (if  (compex-zerop number) number
       (let ((real (complex-realpart number))
	     (imag (complex-imagpart number)))
	 (cond ((zerop imag)
		(make-complex (typed-one real) imag)))
	 ((zerop real)
	  (%make-complex real (typed-one imag)))
	 (t (%scheme-/ number (abs number))))))

(defmathop signum one nil nil (number)
  (integer      short-float    single-float   double-float  long-float   ratio   complex)
  (integer-sig  short-sig      single-sig     double-sig    long-sig  ratio-sig  complex-sig))


;;;================
;;;  SIN
;;;================

;sin of a complex number
(defun %complex-sin (z)
  (let* ((x (realpart z))
	 (y (imagpart z)))
    (complex (* (sin x) (cosh y)) (* (cos x) (sinh y)))))



(defmathop sin one sin nil (radians)
  (integer	float	ratio		complex)
  (s		s	(float 1 s)	%complex-sin))


;;;================
;;; COS
;;;================
;cosine of a complex number
(defun %complex-cos (z)
  (let* ((x (realpart z))
	 (y (imagpart z)))
    (complex (* (cos x) (cosh y)) (- (* (sin x) (sinh y))))))

(defmathop cos one cos nil (radians)
  (integer	float	ratio		complex)
  (s		s	(float 1 s)	%complex-cos))


;;;================
;;;  TAN
;;;================
;tan of a complex number
;there was a nicer algorithm but it turned out not to work so well.
(defun %complex-tan (z)
  (let* ((num (sin z))
	 (denom (cos z)))
    (if (zerop denom) (error "~S undefined tangent." z)
	(/ num denom))))

(defmathop tan one tan nil (radians)
  (integer	float	ratio		complex)
  (s		s	(float 1 s)	%complex-tan))



;;;================
;;;  CIS
;;;================

(defun cis-int (theta)
  ;;(check-type theta (and number (not complex)))
  (%make-complex (cos theta) (sin theta)))

(defmathop cis one nil nil (radians)
  (rational	float)
  (cis-int	cis-int))


;;;================
;;;  ASIN
;;;================

;;;+++Should we be using the long version of these constants?
(defun in-asin-domain (z)
  (cond ((< (- %short-pi/2) (realpart z) %short-pi/2) t)
	((= (realpart z) (- %short-pi/2))
	 (if (>= (imagpart z) 0) t
	     nil))
	((= (realpart z) %short-pi/2)
	 (if (<= (imagpart z) 0) t
	     nil))
	(t nil)))


(defun complex-asin (z)
  (if (in-asin-domain z)
      (let* ((i (%multbyi z)))
	(%negate-complex (%multbyi (log (+ i (sqrt (- 1 (* z z))))))))
      (error "Argument not in domain for asin. ~S" z)))


(defmathop asin one asin nil (number)
  (integer	float	ratio		complex)
  (s		s	(float 1 s)	%complex-asin))


;;;================
;;;  ACOS
;;;================

(defun in-acos-domain (z)
  (cond ((< 0 (realpart z) %short-pi) t)
	((= 0 (realpart z))
	 (if (>= (imagpart z) 0)
	     t
	     nil))
	  ((= (realpart z) %short-pi)
	   (if (<= (imagpart z) 0) 
	       t
	       nil))
	(t nil)))

(defun complex-acos (z)
  (if (in-acos-domain z)
      (%negate-complex (%multbyi (log (+ z (%multbyi (sqrt (- 1 (* z z))))))))
      (error "Argument not in domain for acos. ~S" z)))

(defmathop acos one acos nil (number)
  (integer	float	ratio		complex)
  (s		s	(float 1 s)	%complex-acos))


;;;================
;;;  ATAN
;;;================-

(cl-define %scheme-atan (access atan '()))

;define the domain for atan for complex numbers
(defun in-atan-domain (z)
  (cond ((< (- %short-pi/2) (realpart z) %short-pi/2) t)
	((= (realpart z) (- %short-pi/2))
	 (if (plusp (imagpart z)) t
	     nil))
	((= (realpart z) %short-pi/2)
	 (if (minusp (imagpart z)) t
	     nil))
	(t nil)))


(defun %complex-atan (z)
  (if (in-atan-domain z)
      (let ((i (%multbyi z)))
	(%negate-complex (%multbyi (* .5 (log (/ (+ 1 i) (- 1 i)))))))
      (error "Argument not in domain for atan. ~S" z)))


(defun %ce (arg1 arg2)
  (error "Either ~S or ~S are complex." arg1 arg2))

#+ignore
(defun atan (y &optional x)
  (cond (x (if (or (complexp y) (complexp x))
	       (error "Either ~S or ~S are complex." y x))
	   (when (typep x 'ratio) (setq x (float x)))
	   (when (typep y 'ratio) (setq y (float x)))
	   (%scheme-atan y x))
	((complexp y) (%complex-atan y))
	(t (when (typep y 'ratio) (setq y (float x)))
	   (%scheme-atan y))))


(defmathop atan one-and-optional atan nil (y &optional (x nil x-supplied))
  (integer	float		ratio		complex	null)		;<- arg2 ->
  (s		s		(float 2 s)	s	s)		;integer
  (s		s		(float 2 s)	s	s)		;float
  (x		x		(float (1 2) s)	s	(float 1 s))	;ratio
  (%ce		%ce		%ce		%ce	%complex-atan))	;complex

;;;================================================================
;;; Hyperbolic trig functions.
;;; Each of the hyperbolic trig functions is calculated directly from 
;;; their definition.  Exp(x) is calculated only once for efficiency.
;;; They all work with complex arguments without modification.
;;;================================================================

;;;================
;;;  SINH
;;;================
(defun sinh (x)
  (let ((z (exp x)))
    (/ (- z (/ z)) 2)))

;;;================
;;;  COSH
;;;================
(defun cosh (x)
  (let ((z (exp x)))
    (/ (+ z (/ z)) 2)))

;;;================
;;;  TANH
;;;================
;;; Different form than in the manual.  Does much better.
(defun tanh (x)
  (let* ((z (exp (* 2 x)))
	 (y (/ z)))
    (- (/ (1+ y)) (/ (1+ z)))))



;;;================
;;;  PI
;;;================

;;;See the constants section



;;;================
;;; ASINH
;;;================
(defun in-asinh-domain (z)
  (cond ((< (- %short-pi/2) (imagpart z) %short-pi/2) t)
	((= (imagpart z) (- %short-pi/2))
	 (if (<= (realpart z) 0) t
	     nil))
	((= (imagpart z) %short-pi/2)
	 (if (>= (realpart z) 0) t
	     nil))
	(t nil)))

(defun %complex-asinh (x)
  (if (in-asinh-domain x)
      (log (+ x (sqrt (+ (* x x) 1))))
      (error "~S argument out of range." x)))

(defun %real-asinh (x)
  (log (+ x (sqrt (+ (* x x) 1)))))

(defmathop asinh one nil nil (number)
  (rational	float		complex)
  (%real-asinh	%real-asinh	%complex-asinh))

;;;================
;;;  ACOSH
;;;================
(defun in-acosh-domain (x)
  (cond ((plusp (realpart x))
		(if (and (<= (- %short-pi) x) (< x %short-pi))
		    t
		    nil))
	       ((= (realpart x) 0)
		(if (<= 0 (imagpart x) %short-pi)
		    t
		    nil))
	       (t  nil)))

(defun %complex-acosh (x)
  (if (in-acosh-domain x)
      (log (+ x (sqrt (- (* x x) 1))))
      (error "~S argument out of range." x)))

(defun %real-acosh (x)
  (if (plusp x)
      (log (+ x (sqrt (- (* x x) 1))))
      (error "~S argument out of range." x)))

(defmathop acosh one nil nil (number)
  (rational	float		complex)
  (%real-acosh	%real-acosh	%complex-acosh))


;;;================
;;;  ATANH
;;;================
(defun in-atanh-domain (z)
  (cond ((< (- %short-pi/2) (imagpart z) %short-pi/2) t)
	((= (imagpart z) (- %short-pi/2))
	 (if (minusp (realpart z)) t
	     nil))
	((= (imagpart z) %short-pi/2)
	 (if (plusp (realpart z)) t
	     nil))
	(t nil)))


(defun %real-atanh (x)
  (if (< -1 x 1)
      (* 0.5 (log (/ (1+ x) (- 1 x))))
      (error "~S argument out of range." x)))

(defun %complex-atanh (x)
  (if (in-atanh-domain x)
      (* 0.5 (log (/ (1+ x) (- 1 x))))
      (error "~S argument out of range." x)))

(defmathop atanh one nil nil (number)
  (rational	float		complex)
  (%real-atanh	%real-atanh	%complex-atanh))


;;;================================================================
;;12.6 Type Conversions and Component Extractions on Numbers
;;;================================================================

;;;================================================================
;;; Float stuff:
;
;	Data formats are generally as described in the article in
;	the January 1980 issue of "Computer" on the proposed IEEE
;	floating point standard : sign bit, 8-bit base 2 exponent
;	biased by 127, a hidden leading 1, and 23 fractional bits
;	so that
;	s e ff = (-1)**s * 2**(e-127) * 1.ff


;;;================
;;;  FLOAT
;;;================

;;;Returns the floating point number (flonum) corresponding to
;;;either a bignum or a fixnum.  If the bignum is too large or small
;;;to be converted to floating point, or if the argument isn't of
;;;the correct type, FIXNUM-OR-BIGNUM is returned unchanged.
(define %scheme-FLOAT (make-primitive-procedure 'COERCE-INTEGER-TO-FLONUM))

(defmacro trap-overflow (num)
  `(let ((result (%scheme-float ,num)))
     (if (eq ,num result)
	 (error "Float was given ~A, a bignum too big to be represented ~
                 as a floating point number" ,num)
	 result)))

(defun %short-float (num)
  (trap-overflow num))

(defun %single-float (num)
  (trap-overflow num))

(defun %double-float (num)
  (trap-overflow num))

(defun %long-float (num)
  (trap-overflow num))

(defun %fe (arg1 arg2)
  arg1
  (error "The second argument to FLOAT, ~A, was not a floating point number." arg2))
  
(defun %float-ratio (arg1 &optional arg2)
  arg2
  (/ (%scheme-float (numerator arg1)) (%scheme-float (denominator arg1))))

(defmathop float one-and-optional nil nil (number &optional (other nil otherp))
  (integer  short-float	     single-float    double-float   long-float  ratio  null)
  (%fe      %short-float     %single-float   %double-float  %long-float  %fe   %single-float) 
  (%fe      prog1	     %single-float   %double-float  %long-float  %fe   prog1)
  (%fe      %short-float     prog1	     %double-float  %long-float  %fe   prog1)
  (%fe      %short-float     %single-float   prog1	    %long-float  %fe   prog1)
  (%fe	    %short-float     %single-float   %double-float  prog1        %fe   prog1)
  (%fe      %float-ratio     %float-ratio    %float-ratio   %float-ratio %fe   %float-ratio))

;;;================
;;;  Rational
;;;================
;;; Rational produces a rational number for any numeric argument.
;;; Rational assumed that the floating point is completely accurate.

;;This assumes exactness
(defun %float-rational (num)
  (multiple-value-bind (f e) (decode-float num)
    (let* ((precision (float-precision f))
	   (f (truncate (scale-float f precision))))
      (if (minusp e)
	  (make-ratio f (ash 1 (+ precision (abs e))))
	  (make-ratio (ash f e) (ash 1 precision))))))

(defmathop rational one nil nil (number)
  (rational	float)
  (prog1	%float-rational))

;;;================
;;;  Rationalize
;;;================
;;;***Interesting spice-lisp comment [AGB]***
;;; Thanks to Kim Fateman, who stole this function rationalize-float
;;; from macsyma's rational. Macsyma'a rationalize was written
;;; by the legendary Gosper (rwg). Gosper is now working for Symbolics.
;;; Guy Steele said about Gosper, "He has been called the
;;; only living 17th century mathematician and is also the best
;;; pdp-10 hacker I know." So, if you can understand or debug this
;;; code you win big.

(defun rationalize-float (x &optional (eps long-float-epsilon))
  (cond ((minusp x) (- (rationalize (- x))))
	((zerop x) 0)
	(t (let ((y ())
		 (a ()))
	     (do ((xx x (setq y (/ 1.0 (- xx (float a x)))))
		  (num (setq a (truncate x))
		       (+ (* (setq a (truncate y)) num) onum))
		  (den 1 (+ (* a den) oden))
		  (onum 1 num)
		  (oden 0 den))
		 ((and (not (zerop den))
		       (not (> (abs (/ (- x (/ (float num x)
					       (float den x)))
				       x))
			       eps)))
		  (/ num den)))))))

(defun rf-short (arg)
  (rationalize-float arg short-float-epsilon)) 
(defun rf-single (arg)
  (rationalize-float arg single-float-epsilon)) 
(defun rf-double (arg)
  (rationalize-float arg double-float-epsilon)) 
(defun rf-long (arg)
  (rationalize-float arg long-float-epsilon))

;;; Rationalize does a rational, but it assumes that floats
;;; are only accurate to their precision, and generates a good
;;; rational aproximation of them.
(defmathop rationalize one nil nil (number)
  (rational	short-float	single-float	double-float	long-float)
  (prog1	rf-short	rf-single	rf-double	rf-long))


;;;================
;;; Numerator
;;;================
(defmathop numerator one nil nil (rational)
  (integer	ratio)
  (prog1        ratio-numerator))


;;;================
;;; Denominator
;;;================
(defmathop numerator one nil nil (rational)
  (integer	ratio)
  ('1	        ratio-denominator))



;;;================
;;; Floor
;;;================

(define %bignum-divide (make-primitive-procedure 'divide-bignum))
(define %fixnum-divide (make-primitive-procedure 'divide-fixnum))
(define %integer-divide (make-primitive-procedure 'integer-divide))
(define %truncate-flonum (make-primitive-procedure 'truncate-flonum))

(defmacro floor-check (arg1 arg2 op &optional values-style)
  (if (eq values-style 'multi)
      `(multiple-value-bind (num rem) (,op ,arg1 ,arg2)
	 (when (and (not (zerop rem))
		    (minusp num))
	   (decf num 1) (setq rem (- ,arg1 (* num ,arg2))))
	 (values num rem))
      `(let* ((results (,op ,arg1 ,arg2))
	      (num (car results))
	      (rem (cdr results)))
	 (when (and (not (zerop rem))
		    (minusp num))
	   (decf num 1) (setq rem (- ,arg1 (* num ,arg2))))
	 (values num rem))))


(defop-semantics floor fixnum
  (fixnum (floor-check arg1 arg2 %fixnum-divide))
  (bignum (floor-check arg1 arg2 %integer-divide))
  ((:flip bignum) (floor-check arg1 arg2 %integer-divide))
  (float  (floor (/ arg1 arg2)))
  ((:flip float) (floor (/ arg1 arg2)))
  (ratio  (floor-check %r-t-i arg1 arg2 multi))
  ((:flip ratio) (floor-check %i-t-r arg1 arg2 multi)))

(defop-semantics floor bignum
  (bignum (floor-check arg1 arg2 %bignum-divide))
  (float  (floor (/ arg1 arg2)))
  ((:flip float) (floor (/ arg1 arg2)))
  (ratio  (floor-check %r-t-i arg1 arg2 multi))
  ((:flip ratio) (floor-check %i-t-r arg1 arg2 multi)))

(defop-semantics floor float
  (float  (floor (/ arg1 arg2)))
  (ratio  (floor (/ arg1 arg2)))
  ((:flip ratio) (floor (/ arg1 arg2))))


(defun floor-ratio2 (arg1 arg2)
  (floor-check %r-t-r arg1 arg2 multi))

(defun floor-ratio1 (arg)
  (multiple-value-bind (num rem) (%ratio-trunc arg)
    (when (and (not (zerop rem))
	       (minusp num))
      (decf num 1) (setq rem (- ,arg  ,num)))
    (values num rem)))

(defun values-int (num)
  (values num 0))

(defun floor-float (arg)
  (let* ((num (%truncate-flonum arg))
	 (rem (- arg num)))
     (when (and (not (zerop rem))
		(minusp num))
       (decf num 1) (setq rem (- ,arg  ,num)))
     (values num rem)))

(defmathop floor one-and-optional nil nil (number &optional (divisor nil divisorp))
  (fixnum	    bignum		float		    ratio		null)
  (floor-fixnum	    floor-bignum-fixnum	floor-float-fixnum  floor-ratio-fixnum  values-int)
  (x		    floor-bignum	floor-float-bignum  floor-ratio-bignum  values-int)
  (x		    x			floor-float  	    floor-ratio-float   floor-float)
  (x		    x			x  	    	    floor-ratio2	floor-ratio1)
  )

;;;================
;;; Ceiling
;;;================

(defmacro ceiling-check (arg1 arg2 op &optional values-style)
  (if (eq values-style 'multi)
      `(multiple-value-bind (num rem) (,op ,arg1 ,arg2)
	 (when (and (not (zerop rem))
		    (plusp num))
	   (incf num 1) (setq rem (- ,arg1 (* num ,arg2))))
	 (values num rem))
      `(let* ((results (,op ,arg1 ,arg2))
	      (num (car results))
	      (rem (cdr results)))
	 (when (and (not (zerop rem))
		    (plusp num))
	   (incf num 1) (setq rem (- ,arg1 (* num ,arg2))))
	 (values num rem))))


(defop-semantics ceiling fixnum
  (fixnum (ceiling-check arg1 arg2 %fixnum-divide))
  (bignum (ceiling-check arg1 arg2 %integer-divide))
  ((:flip bignum) (ceiling-check arg1 arg2 %integer-divide))
  (float  (ceiling (/ arg1 arg2)))
  ((:flip float) (ceiling (/ arg1 arg2)))
  (ratio  (ceiling-check %r-t-i arg1 arg2 multi))
  ((:flip ratio) (ceiling-check %i-t-r arg1 arg2 multi)))

(defop-semantics ceiling bignum
  (bignum (ceiling-check arg1 arg2 %bignum-divide))
  (float  (ceiling (/ arg1 arg2)))
  ((:flip float) (ceiling (/ arg1 arg2)))
  (ratio  (ceiling-check %r-t-i arg1 arg2 multi))
  ((:flip ratio) (ceiling-check %i-t-r arg1 arg2 multi)))

(defop-semantics ceiling float
  (float  (ceiling (/ arg1 arg2)))
  (ratio  (ceiling (/ arg1 arg2)))
  ((:flip ratio) (ceiling (/ arg1 arg2))))


(defun ceil-ratio2 (arg1 arg2)
  (ceiling-check (%r-t-r arg1 arg2)))

(defun ceil-ratio1 (arg)
  (multiple-value-bind (num rem) (%ratio-trunc)
    (when (and (not (zerop rem))
	       (plusp num))
      (incf num 1) (setq rem (- ,arg  ,num)))
    (values num rem)))

(defun ceil-float (arg)
  (let* ((num (%truncate-flonum arg))
	 (rem (- arg num)))
     (when (and (not (zerop rem))
		(plusp num))
       (incf num 1) (setq rem (- ,arg  ,num)))
     (values num rem)))

(defmathop ceiling one-and-optional nil nil (number &optional (divisor nil divisorp))
  (fixnum	  bignum		float		     ratio		  null)
  (ceiling-fixnum ceiling-bignum-fixnum ceiling-float-fixnum ceiling-ratio-fixnum values-int)
  (x		  ceiling-bignum	ceiling-float-bignum ceiling-ratio-bignum values-int)
  (x		  x			ceiling-float  	     ceiling-ratio-float  ceil-float)
  (x		  x			x  	    	     ceil-ratio2	  ceil-ratio1)
  )


;;;================
;;; TRUNCATE
;;;================

;;; Ratio-trunc returns the integer quotient and the ratio remainder.
(defun %ratio-trunc (x)
  (multiple-value-bind (q r)
      (truncate (ratio-numerator x)
		(ratio-denominator x))
    (values q
	    (if (zerop r) 0
		(make-ratio r (ratio-denominator x))))))

(defun %i-t-r (x y)
  (let* ((dy (denominator y)))
    (multiple-value-bind (q r) (truncate (* x dy) (numerator y))
      (if (zerop r)
	  (truncate-values q 0)
	  (let* ((g (%scheme-gcd r dy)))
	    (values q (if (= g 1)
			  (%make-ratio r dy)
			  (%make-ratio (truncate r g)
				       (truncate dy g)))))))))

(defun %r-t-i (x y)
  (let* ((dx (denominator x)))
    (multiple-value-bind (q r) (truncate (numerator x) (* dx y))
      (let* ((g (%scheme-gcd r dx)))
	(values q (if (= g 1)
		      (%make-ratio r dx)
		      (%make-ratio (truncate r g)
				   (truncate dx g))))))))

(defun %r-t-r (x y)
  (let* ((nx (numerator x))
	 (dx (denominator x))
	 (ny (numerator y))
	 (dy (denominator y)))
    (multiple-value-bind (q r) (truncate (* nx dy) (* dx ny))
      (if (zerop r)
	  (truncate-values q 0)
	  (let* ((nd (* dx dy))
		 (g (%scheme-gcd r (* dx dy))))
	    (values q (if (= g 1)
			  (%make-ratio r nd)
			  (%make-ratio (truncate r g)
				       (truncate nd g)))))))))

(defun trunc-float (arg)
  (values-list (%truncate-flonum arg)))

(defun trunc-2-args (arg1 arg2)
  (truncate (/ arg1 arg2)))


(defmathop trunc one-and-optional nil nil (number &optional (divisor nil divisorp))
  (fixnum	   bignum		float		ratio		null)
  (%fixnum-divide  %integer-divide	trunc-2-args	%r-t-i		values-int)
  (%integer-divide %bignum-divide	trunc-2-args	%r-t-i		values-int)
  (trunc-2-args	   trunc-2-args		trunc-2-args	trunc-2-args	trunc-float)
  (%i-t-r	   %i-t-r		trunc-2-args	%r-t-r		%ratio-trunc))


;;;================
;;;  REM
;;;================
(defun rem (number divisor)
  (multiple-value-bind (tru rem) (truncate number divisor)
    (declare (ignore tru))
    rem))

;;;================
;;;  MOD
;;;================
(defun mod (number divisor)
  "Returns second result of FLOOR."
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
	rem)))

;;;================
;;;  FFLOOR
;;;================
;;;"Same as FLOOR, but returns first value as a float."


(defun ffloor (number &optional (divisor 1))
  (multiple-value-bind (flr rem) (floor number divisor)
    (values (float flr) rem)))

;;;================
;;; FCEILING
;;;================
;;;"Same as CEILING, but returns first value as a float."
(defun fceiling (number &optional (divisor 1))
  (multiple-value-bind (cei rem) (ceiling number divisor)
    (values (float cei) rem)))

;;;================
;;;  FTRUNCATE
;;;================
;;;"Same as TRUNCATE, but returns first value as a float."
(defun ftruncate (number &optional (divisor 1))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (values (float tru) rem)))


;;;================
;;;  FROUND
;;;================
;;;"Same as ROUND, but returns first value as a float."
(defun fround (number &optional (divisor 1))
  (multiple-value-bind (rou rem) (round number divisor)
    (values (float rou) rem)))




;;;================
;;;  DECODE-FLOAT
;;;================

;;;Retruns float integer float

(define DECODE-FLOAT-SIGN
	(make-primitive-procedure 'DECODE-FLOAT-SIGN))

(define DECODE-FLOAT-MANTISSA
	(make-primitive-procedure 'DECODE-FLOAT-MANTISSA))

(define DECODE-FLOAT-EXPONENT
	(make-primitive-procedure 'DECODE-FLOAT-EXPONENT))


(defun decode-float  (float)
  (check-type float real)
  (values
    (decode-float-mantissa float)
    (decode-float-exponent float)
    (decode-float-sign float)))

;;;================
;;;  SCALE-FLOAT
;;;================

;;;+++This not efficient
;;;+++LAS should write this one
(defun scale-float (float integer)
  (check-type float float)
  (check-type integer integer)
  (* float (expt (float (float-radix float) float) integer)))

;;;================
;;;  FLOAT-RADIX
;;;================
(defun float-radix (f)
  (check-type f 'float)
  2)

;;;================
;;;  FLOAT-SIGN
;;;================
(defun float-sign (float1 &optional (float2 (float 1 float1)))
  (check-type float1 'float)
  (check-type float2 'float)
  (if (eq (minusp float1) (minusp float2))
      float2
      (- float2)))


;;;================
;;;  FLOAT-DIGITS
;;;================

;;;NB this won't work UNLESS the funny #.`', is done because of the way
;;;defmathop reads the table entries. [AGB]
(defmathop float-digits one nil nil (float)
  (short-float	single-float	double-float	long-float)
  (#.`',%short-float-mantissa-length
   		#.`',%single-float-mantissa-length
				#.`',%double-float-mantissa-length
						#.`',%long-float-mantissa-length))


;;;================
;;; FLOAT-PRECISION
;;;================
(defun float-precision (f)
  (check-type f 'float)
  (if (zerop f)
      0
      (float-digits f)))


;;;================
;;;INTEGER-DECODE-FLOAT
;;;================

;;;Returns integer integer integer
(define INTEGER-DECODE-FLOAT-MANTISSA
	(make-primitive-procedure 'INTEGER-DECODE-FLOAT-MANTISSA))

(define INTEGER-DECODE-FLOAT-EXPONENT
	(make-primitive-procedure 'INTEGER-DECODE-FLOAT-EXPONENT))


(defun integer-decode-float  (float)
  (check-type float real)
  (values
    (INTEGER-DECODE-FLOAT-MANTISSA float)
    (INTEGER-DECODE-FLOAT-EXPONENT float)
    (truncate (decode-float-sign float))))

#+ignore
(defun integer-decode-float (x)
  (check-type x 'float)
  (let ((precision (float-precision x)))
    (multiple-value-bind (f e s) (decode-float x)
      (values (truncate (scale-float f precision))
	      (- e precision)
	      (truncate s)))))


;;;================
;;;  COMPLEX
;;;================
(setf (symbol-function '%mc) (symbol-function '%make-complex))
(setf (symbol-function 'mc) (symbol-function 'make-complex))
(setf (symbol-function '%sf) (symbol-function '%short-float))
(setf (symbol-function '%f) (symbol-function '%single-float))
(setf (symbol-function '%df) (symbol-function '%double-float))
(setf (symbol-function '%lf) (symbol-function '%long-float))

(defun mczshort (realpart)
  (%mc realpart 0s0))
(defun mczsingle (realpart)
  (%mc realpart 0f0))
(defun mczdouble (realpart)
  (%mc realpart 0d0))
(defun mczlong (realpart)
  (%mc realpart 0l0))

(defmathop complex one-and-optional nil nil (realpart &optional (imagpart nil imagpartp))
  (rational	short-float	single-float	double-float	long-float	null)
  (mc		(%sf 1 %mc)	(%f 1 %mc)	(%df 1 %mc)	(%lf 1 %mc)	prog1)
  ((%sf 2 %mc)	%mc		(%f 1 %mc)	(%df 1 %mc)	(%lf 1 %mc)	mczshort)
  ((%f 2 %mc)	(%f 2 %mc)	%mc		(%df 1 %mc)	(%lf 1 %mc)	mczsingle)
  ((%df 2 %mc)	(%df 2 %mc)	(%df 2 %mc)	%mc		(%lf 1 %mc)	mczdouble)
  ((%lf 2 %mc)	(%lf 2 %mc)	(%lf 2 %mc)	(lf 2 %mc)	%mc		mczlong))


;;;================
;;; REALPART
;;;================
(defmathop realpart one nil nil (number)
  (float	rational	complex)
  (prog1	prog1		complex-realpart))


;;;================
;;; IMAGPART
;;;================
(defmathop imagpart one nil nil (number)
  (short-float	single-float	double-float	long-float	rational    complex)
  ('0s0		'0f0		'0d0		'0l0		'0	    compelx-imagpart))



;;;================================================================
;;; 12.7 Logical Operations of Numbers
;;;================================================================

;;;+++The code commented out is being done by Chan Russel
;;;+++He also needs to do ASH, LOGCOUNT, and INTEGER-LENGTH
;;;+++Also make sure type checking is done

;;;================
;;;  LOGIOR
;;;================
#+ignore
(defun logior (&rest integers)
  (do* ((result 0 (logical-ior result integer))
	(integers integers (cdr integers))
	(integer (car integers) (car integers)))
       ((null integers) result)))

;;;================
;;;  LOGAND
;;;================
#+ignore
(defun logand (&rest integers)
  (cond ((null integers)
	 0)
	(else
	 (do* ((result (car integers) (logical-and result integer))
	       (integers (cdr integers) (cdr integers))
	       (integer (car integers) (car integers)))

	      ((null integers) result)))))


;;;================
;;;  LOGXOR
;;;================
#+ignore
(defun logxor (&rest integers)
  (cond ((null integers) 0)
	(else
	 (do* ((result (car integers) (logical-xor result integer))
	       (integers (cdr integers) (cdr integers))
	       (integer (car integers) (car integers)))

	      ((null integers) result)))))


;;;================
;;;  LOGEQV
;;;================

#+ignore
(defun logeqv (&rest integers)
  (cond ((null integers) -1)
	(else
	 (do* ((result (car integers)
		       (logical-not (logical-xor result integer)))
	       (integers (cdr integers) (cdr integers))
	       (integer (car integers) (car integers)))

	      ((null integers) result)))))

;;;================
;;;  LOGNOT
;;;================
#+ignore
(cl-define lognot (fundefsym 'logical-not))


;;;================
;;;  LOGNAND
;;;================
#+ignore
(defun lognand (a b)
  (lognot (logand a b)))

;;;================
;;;  LOGNOR
;;;================
#+ignore
(defun lognor (a b)
  (lognot (logior a b)))


;;;================
;;;  LOGANDC1
;;;================
#+ignore
(defun logandc1 (a b)
  (logand (lognot a) b))

;;;================
;;;  LOGANDC2
;;;================
#+ignore
(defun logandc2 (a b)
  (logand a (lognot b)))

;;;================
;;;  LOGIORC1
;;;================
#+ignore
(defun logiorc1 (a b)
  (logior (lognot a) b))

;;;================
;;;  LOGIORC2
;;;================
#+ignore
(defun logiorc2 (a b)
  (logior a (lognot b)))

;;;================
;;; BOOLE & Constants
;;;================

#|
(defconstant boole-clr 0)
(defconstant boole-set 1)
(defconstant boole-1 2)
(defconstant boole-2 3)
(defconstant boole-c1 4)
(defconstant boole-c2 5)
(defconstant boole-and 6)
(defconstant boole-ior 7)
(defconstant boole-xor 8)
(defconstant boole-eqv 9)
(defconstant boole-nand 10)
(defconstant boole-nor 11)
(defconstant boole-andc1 12)
(defconstant boole-andc2 13)
(defconstant boole-orc1 14)
(defconstant boole-orc2 15)

(define n-ops 16)
(define boole-dispatch-table
  (vector (lambda (n1 n2) 0)		;boole-clr
	  (lambda (n1 n2) -1)		;boole-set
	  (lambda (n1 n2) n1)		;boole-1
	  (lambda (n1 n2) n2)		;boole-2
	  (lambda (n1 n2)		;boole-c1
	    (logical-not n1))
	  (lambda (n1 n2)		;boole-c2
	    (logical-not n2))
	  (function logical-and)	;boole-and
	  (function logical-ior)	;boole-ior
	  (function logical-xor)	;boole-xor
	  (function logeqv)		;boole-eqv
	  (function lognand)		;boole-nand
	  (function lognor)		;boole-nor
	  (function logandc1)		;boole-andc1
	  (function logandc2)		;boole-andc2
	  (function logiorc1)		;boole-orc1
	  (function logiorc2)))		;boole-orc2

(defun boole (op integer1 integer2)
  (if (or (< op 0) (>= op n-ops))
      (error "Op is out of range for BOOLE:" op)
      ((vector-ref boole-dispatch-table op) integer1 integer2)))
|#


;;;================
;;;  LOGTEST
;;;================
(defun logtest (integer1 integer2)
  (check-type integer1 integer)
  (check-type integer2 integer)
  (not (zerop (logand integer1 integer2))))

;;;================
;;;  LOGBITP
;;;================
(defun logbitp (index integer)
  (check-type index integer)
  (check-type integer integer)
  (logtest integer (ash 1 index)))


;;;================
;;;  ASH
;;;================

;;;Currently bound to a-shift

;;;================
;;;  LOGCOUNT
;;;================
;;; Logcount returns the number of bits that are the complement of 
;;; the sign in the integer argument x.

#+ignore
(defun logcount (x)				;FROM SPNUM
  "If X is negative, then the number of 0 bits is returned, otherwise
  number of 1 bits is returned."
  (macrolet ((count-bits (n)
	       `(do ((n ,n (logand n (1- n)))
		     (cnt 0 (1+ cnt)))
		    ((zerop n) cnt))))
    (typecase x
      (fixnum
       (count-bits (if (minusp x) (lognot x) x)))
      (bignum
       (let* ((res 0)
	      (l (bignum-length x))
	      (words (ash l -1)))
	 (cond
	  ((zerop (bignum-sign x))
	   (dotimes (i words)
	     (incf res (count-bits (bref16 x i))))
	   (when (oddp l)
	     (incf res (count-bits (bref x (1- l))))))
	  (t
	   (dotimes (i words)
	     (incf res (count-bits (logxor (bref16 x i) #xFFFF))))
	   (when (oddp l)
	     (incf res (count-bits (logxor (bref x (1- l)) #xFF))))))
	 res))
      (otherwise
       (error "Argument not integer, ~A." x)))))


;;;================
;;; INTEGER-LENGTH
;;;================

;;;+++This does not work for really BIG nums!
#+ignore
(defun integer-length (i)
  (check-type i integer)
  (ceiling (log (if (< i 0) (- i) (1+ i)) 2)))

;;;================================================================
;;; 12.8 Byte Manipulation Functions
;;;================================================================

;;;================
;;; BYTE, BYTE-SIZE, BYTE-POSITION
;;;================
(defstruct (byte-specifier
	     (:conc-name byte-)
	     (:constructor byte (size position))
	     ;;(:type list)
	     :named)
  (size 0 :type integer)
  (position 0 :type integer))

;;;================
;;;  BYTE
;;;================
#+ignore
(defun byte (size position)
  (cond ((> (+ size position) 24)
	 (error "Um, this bytespec is out of range for now ..."))
	(t
	 (cons size position))))

;;;================
;;;  BYTE-SIZE
;;;================
#+ignore
(defun byte-size (bytespec)
  (car bytespec))

;;;================
;;; BYTE-POSITION
;;;================
#+ignore
(defun byte-position (bytespec)
  (cdr bytespec))

;;;================
;;;  LDB
;;;================

;;;+++Another way to make masks is with (lognot (lsh -1 (byte-size bytespec))) but this
;;;+++only works for fixnums since lsh only works for fixnums

(defun ldb (bytespec integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (logand (ash integer (- (byte-position bytespec)))
	  (1- (ash 1 (byte-size bytespec)))))

;;;================
;;; LDB-TEST
;;;================
;;;"Returns T if any of the specified bits in integer are 1's."

(defun ldb-test (bytespec integer)
  (check-type bytespec byte-specifier)
  (check-type integer inetger)
  (declare (function ldb (t integer) integer))
  (not (zerop (ldb bytespec integer))))

;;;================
;;;  MASK-FIELD
;;;================
;;;"Extract the specified byte from integer,  but do not right justify result."

(defun mask-field (bytespec integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (logand (dpb -1 bytespec 0) integer))

;;;================
;;; DPB
;;;================
(defun dpb (newbyte bytespec integer)
  (check-type newbyte integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (let* ((size (byte-size bytespec))
	 (position (byte-position bytespec))
	 (mask (1- (ash 1 size))))
    (logior (logandc2 integer (ash mask position))	;need to clear out the bits
	    (ash (logand newbyte mask) position))))


;;;================
;;; DEPOSIT-FIELD
;;;================
;;;"Returns new integer with newbyte in specified position, newbyte is not right justified."

(defun deposit-field (newbyte bytespec integer)
  (check-type newbyte integer)
  (check-type bytespec byte-specifier)
  (check-type integer integer)
  (let* ((size (byte-size bytespec))
	 (position (byte-position bytespec))
	 (mask (ash (1- (ash 1 size)) position)))
    (logior (logandc2 integer mask)		;clear out bits for the new ones
	    (logand newbyte mask))))



;;;================================================================
;;; 12.9 Random Numbers
;;;================================================================


;;;================
;;; RANDOM STATE OBJ
;;;================
(defvar *random-state*)

(defconstant random-const-a 8373)
(defconstant random-const-c 101010101)
(defconstant random-upper-bound 134217726)
(defconstant random-max 54)
(defvar rand-seed 0)



(defstruct (random-state (:constructor make-random-object))
  (j 24 :type integer)
  (k 0 :type integer)
  (seed (make-array (1+ random-max) :initial-contents
		    (do ((list-rands () (cons (rand1) list-rands))
			 (i 0 (1+ i)))
			((> i random-max) list-rands)))
	:type '(simple-vector * fixnum)))


;;; Generates a random number from rand-seed.
(defun rand1 ()
   (setq rand-seed (mod (+ (* rand-seed random-const-a) random-const-c)
			(1+ random-upper-bound))))

;;; rand3  --  Internal
;;;
;;; This function generates fixnums between 0 and random-upper-bound, 
;;; inclusive For the algorithm to work random-upper-bound must be an 
;;; even positive fixnum.  State is the random state to use.
;;;
(defun rand3 (state)
  (let ((seed (random-state-seed state))
	(j (random-state-j state))
	(k (random-state-k state)))
    (declare (fixnum j k) (type (simple-vector * fixnum) seed))
    (setf (svref seed k)
	  (let ((a (- random-upper-bound
		      (svref seed
			      (setf (random-state-j state)
				    (if (= j 0) random-max (1- j))))
		      (svref seed
			      (setf (random-state-k state)
				    (if (= k 0) random-max (1- k)))))))
	    (if (minusp a) (- a) (- random-upper-bound a))))))


(defun random-init ()
  (setq *random-state*
	(make-random-object :seed
	 (make-array (1+ random-max) :initial-contents
		     '(45117816 133464727 86324180 99419799 68851957 87250180
		      52971860 84081967 30854110 121122797 70449044 18801152
		      45149898 15881380 27398356 117706009 49915564 80620628
		      120974070 98193932 43883764 53717012 100954825 82579490
		      17280729 118523949 42282975 127220348 6288263 56575578
		      2474156 47934425 561006 21989698 74046730 105055318
		      113363907 48749716 78183593 109613585 37323232 65101428
		      46453209 76906562 5371267 86544820 33922642 60765033
		      41889257 77176406 38775255 78514879 72553872 66916641
		      100613180)))))

(defun copy-state (cur-state)
  (let ((state (make-random-object
		:seed (make-array 55)
		:j (random-state-j cur-state)
		:k (random-state-k cur-state))))
    (do ((i 0 (1+ i)))
	((= i 55) state)
      (declare (fixnum i))
      (setf (aref (random-state-seed  state) i)
	    (aref (random-state-seed cur-state) i)))))


;;;================
;;; RANDOM
;;;================ 
(proclaim '(ftype (function (t) fixnum) rand3))
(defun random (arg &optional (state *random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (typecase arg
    (fixnum
     (unless (plusp (the fixnum arg))
       (error "Non-positive argument, ~A, to RANDOM." arg))     
     (rem (the fixnum (rand3 state)) (the fixnum arg)))
    (float
     (unless (plusp arg)
       (error "Non-positive argument, ~A, to RANDOM." arg))
     (let ((arg-length (typecase arg
			 (short-float %short-float-mantissa-length)
			 (single-float %single-float-mantissa-length)
			 (double-float %double-float-mantissa-length)
			 (long-float %long-float-mantissa-length))))
       (* arg (/ (random (ash 2 arg-length) state)
		 (ash 2 arg-length)))))
    (integer
     (unless (plusp arg)
       (error "Non-positive argument, ~A, to RANDOM." arg))
     (do ((tot (rand3 state) (+ (ash tot %fixnum-length) (rand3 state)))
	  (end (ash arg (- %fixnum-length))
	       (ash end (- %fixnum-length))))
	 ((zerop end) (mod tot arg))))
    (t (error "Wrong type argument, ~A, to RANDOM." arg))))


;;;================
;;; MAKE-RANDOM-STATE
;;;================
;;;  "Make a random state object.  If State is not supplied, return a copy
;;;  of the default random state.  If State is a random state, then return a
;;;  copy of it.  If state is T then return a random state generated from
;;;  the universal time."
(defun make-random-state (&optional state)
  (cond ((not state) (copy-state *random-state*))
	((random-state-p state) (copy-state state))
	((eq state t) (setq rand-seed (get-universal-time))
		      (make-random-object))
	(t (error "Bad argument, ~A, for RANDOM-STATE." state))))


;;;================
;;; Random-state-p
;;;================

;;;Defined in the defstruct


;;;12.10 is up at the beginnig of this file