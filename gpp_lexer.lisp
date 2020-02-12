
(defun gppinterpreter() 
	(write "USAGE: [$g++] [filename] for the reading codes from file")
	(terpri)
	(write "USAGE: [$g++] for the writing codes from console")
	(terpri)
	;this ~ symbol is the exit symbol.
	(setf final #\~)
	;these 3 for the readed codes from the user
	(setf chrs nil) 
	(setf newchr nil)
	(setf code nil)

	(setf termcommand (read-line)) ;read from user

	(if (string= "$g++" termcommand)
		(progn 
			(write "please start to writing your code ")
			(terpri)
			(write "USAGE: pres '~' when you done and want to see the tokens")
			(terpri)
			(princ ">")
			(loop while (string/= code final) do
				(setf code (read-char))
				(push code chrs)
			)
			(when (string= final code)
				(setf newchr (nreverse chrs))
				(loop for i from 0 to (length chrs) do
					(if (  not(eq (nth i newchr) nil ))
					(addspace (nth i newchr))
					)
				)
			)
		)
		(readfile (subseq termcommand 5)) ;if user gives a filename take that name and send the readfile function
	)
)


(defun readfile (filename)
	(setf document '())
	(setf word '())
	(setf paragraph '())

	;open and read file 
	(with-open-file (stream filename)
		(do ((chr (read-char stream nil) (read-char stream nil))) ;reads char by char from file
			( (null chr) )

			;if the readed char is ';' check if the statement is comment or not 
			(when (string= chr ";")
				(setf chr (read-char stream nil))
				(when (string= chr ";") ;if the next readed char is also ';' ignore the line
					(princ "COMMENT")   ;and print "COMMENT"
					(terpri)
					(loop while (not(eq chr #\Newline)) do
						(setf chr (read-char stream nil))
					)
				)
			)
			;if the readed char is '*' check if the statement is double star or not
			(when (string= chr "*")
		
				(setf chr (read-char stream nil))
				(if (string= chr "*") ;if the next readed char is also '*' print "OP_DBMULT"
					(progn
					(princ "OP_DBLMULT")
					(terpri)
					)
					(progn
					(princ "OP_MULT") ; if not print "OP_MULT"
					(terpri)
					)
				)
			)
			(addspace chr) ;send the all other readed chars to the addspace function
							
		)	
	)
)

;this function add space after all the operators because lexer function lookit up that space. its requires of my algorithm
(defun addspace (chr)
	(cond
		( (string= chr OP_OC)		(lexer #\Space) (lexer chr) )
		( (string= chr OP_CC)		(lexer #\Space) (lexer chr) )	
		( (string= chr OP_OP)		(lexer #\Space) (lexer chr) )
		( (string= chr OP_CP)		(lexer #\Space) (lexer chr) )
		( (string= chr OP_DIV)		(lexer #\Space) (lexer chr) )
		( (string= chr OP_MULT)		(lexer #\Space) (lexer chr) )
		( (string= chr OP_PLUS)		(lexer #\Space) (lexer chr) )
		( (string= chr OP_MINUS)	(lexer #\Space) (lexer chr) )
		( (string= chr OP_COMMA) 	(lexer #\Space) (lexer chr) )
		( (string= chr OP_DBLMULT) 	(lexer #\Space) (lexer chr) )
		( (string= chr #\LEFT_DOUBLE_QUOTATION_MARK)  (lexer #\Space) (lexer chr) )
		( (string= chr #\RIGHT_DOUBLE_QUOTATION_MARK)  (lexer #\Space) (lexer chr) )
		( t (lexer chr))
	)
)


(defun lexer (chr)

	;this two quotation marks taken from a word document because there are " " these two are same. these are indistinguishable
	;so I add a sample input file in my homework zip file. there are two quatation marks that are distinc.
	(when (string= chr #\LEFT_DOUBLE_QUOTATION_MARK ) 
		(princ "OP_OC")
		(terpri)
	)
	(when (string= chr  #\RIGHT_DOUBLE_QUOTATION_MARK )
		(princ "OP_CC")
		(terpri)
	)
	
	;if the readed char is same as any operator, print teh corresponding token name
	(cond
		( (string= chr OP_OC) 		(princ "OP_OC")		(terpri) )
		( (string= chr OP_CC) 		(princ "OP_CC")		(terpri) )	
		( (string= chr OP_OP) 		(princ "OP_OP")		(terpri) )
		( (string= chr OP_CP) 		(princ "OP_CP")		(terpri) )
		( (string= chr OP_DIV) 		(princ "OP_DIV")	(terpri) )
		( (string= chr OP_PLUS) 	(princ "OP_PLUS")	(terpri) )
		( (string= chr OP_MINUS) 	(princ "OP_MINUS")	(terpri) )
		( (string= chr OP_COMMA) 	(princ "OP_COMMA")	(terpri) )
	)

	;if the readed char is a digit add the digits into a num list. so if the number has more than one digit i can take all of them
	(when (digit-char-p chr)
		(push chr num)
	)
	;if the readed char is a alphabetic char add the char intp a word list. so if the word has more than one char i can take all of them
	(when (alpha-char-p chr)
		(push chr word)
	)

	;when the readed char is space or newline
	(when (or(eq chr #\Space)(eq chr #\Newline))
		(setf w (coerce (nreverse word) 'string))
		(setf n (nreverse num))

			;first look if the num list is empty or not 
			(if (> (length n) 1 )
				(progn
					(if (string= (nth 0 n) #\0) 
						(princ "INVALID VALUE") ;if its not just one digit and the first digit is 0 it is an invalid value
						(princ "VALUE") ;if the first digit is not 0 then it is a proper value and print "VALUE"
					)
				(terpri)
				)
			)
			(if (= (length n) 1 ) ;if the num list have just one digit its simply a value
				(progn  
					(princ "VALUE") ;print "VALUE"
					(terpri) 
				)
			)

			;second look if the any word can match of these keywords
			;if they are matched, print corresponding token name 
			(cond
				( (string= w KW_OR)		(princ "KW_OR")		(terpri) )
				( (string= w KW_IF) 	(princ "KW_IF")		(terpri) )
				( (string= w KW_AND) 	(princ "KW_AND")	(terpri) )
				( (string= w KW_NOT) 	(princ "KW_NOT")	(terpri) )
				( (string= w KW_NIL) 	(princ "KW_NIL")	(terpri) )
				( (string= w KW_SET)	(princ "KW_SET")	(terpri) )
				( (string= w KW_FOR) 	(princ "KW_FOR")	(terpri) )
				( (string= w KW_LESS) 	(princ "KW_LESS")	(terpri) )
				( (string= w KW_EXIT) 	(princ "KW_EXIT")	(terpri) )
				( (string= w KW_LOAD) 	(princ "KW_LOAD")	(terpri) )	
				( (string= w KW_DISP) 	(princ "KW_DISP")	(terpri) )
				( (string= w KW_TRUE) 	(princ "KW_TRUE")	(terpri) )
				( (string= w KW_LIST) 	(princ "KW_LIST")	(terpri) )
				( (string= w KW_EQUAL) 	(princ "KW_EQUAL")	(terpri) )
				( (string= w KW_FALSE) 	(princ "KW_FALSE")	(terpri) )
				( (string= w KW_APPEND) (princ "KW_APPEND")	(terpri) )	
				( (string= w KW_CONCAT) (princ "KW_CONCAT")	(terpri) )
				( (string= w KW_DEFFUN) (princ "KW_DEFFUN")	(terpri) )	
				( (if(not(string= w "")) ;if no keyword is matching and the list is not empty
					;look the word is an improper keyword name or not (i.g. 'liste' is a syntax error because there is a keyword as 'list')
					;if there is such a situation send the word to a exitprog function
					(cond
						( (string= (subseq w 0 (- (length w) 1 )) KW_OR)	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_IF) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_AND) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_NOT) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_NIL)    (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_SET)	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_FOR) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_LESS) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_EXIT) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_LOAD) 	 (exitprog w) )	
						( (string= (subseq w 0 (- (length w) 1 )) KW_DISP) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_TRUE) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_LIST) 	 (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_EQUAL)  (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_FALSE)  (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_APPEND) (exitprog w) )	
						( (string= (subseq w 0 (- (length w) 1 )) KW_CONCAT) (exitprog w) )
						( (string= (subseq w 0 (- (length w) 1 )) KW_DEFFUN) (exitprog w) )
						( t (princ "IDENTIFIER")(terpri) )  ; if there is not such asituation like that. the word must be an indentifier
															;so, print "IDENTIFIER"
					)	
				  )
				)
			)		

		(setf word nil)
		(setf num nil)
	)
	
)

; this function is terminate the program with a message when a syntax error occurs
(defun exitprog (token)
	 (princ "SYNTAX_ERROR ")(format t "'~A' cannot be tokenized" token)(terpri)
	 (exit)	 
)

(defun main()

	;these are the token names for operators and keywords.
	(defvar OP_COMMA ",")(defvar OP_OC " “ ")(defvar OP_CC " ” ")(defvar OP_OP "(")(defvar OP_CP ")")
	(defvar OP_PLUS "+")(defvar OP_MINUS "-")(defvar OP_DIV "/")(defvar OP_MULT "*")(defvar OP_DBLMULT "**")
	(defvar KW_AND "and")(defvar KW_OR "or")(defvar KW_NOT "not")(defvar KW_EQUAL "equal")(defvar KW_LESS "less")
	(defvar KW_DEFFUN "deffun")(defvar KW_FOR "for")(defvar KW_IF "if")(defvar KW_EXIT "exit")(defvar KW_LOAD "load")
	(defvar KW_NIL "nil")(defvar KW_LIST "list")(defvar KW_APPEND "append")(defvar KW_CONCAT "concat")(defvar KW_SET "set")
	(defvar KW_DISP "disp")(defvar KW_TRUE "true")(defvar KW_FALSE "false")

	(setf word nil)
	(setf num nil)

	(gppinterpreter) 
)

(main)	

