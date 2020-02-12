# G-lexer-using-Lisp
Given the description of the G++ language (G++Syntax.pdf) implement a lexer that does the tokenization of any set of valid G++ expressions or statements. 

#G++ Language Lexer in Lisp: 
Lexer implemented in Common Lisp. 
*gppinterpreter* function starts the interpreter. *gppinterpreter* have zero or one input. 
The input can be a file name which will be loaded by the interpreter and interpreted right away.

When the program starts, the menu that we see asks us to determine which code we will tokenize.
Whether code lines will be taken from the user as input or whether it is from a g++ file.
- User writes $ g++, the user is expected to write lines of code and is asked to enter the ~ sign to see the tokens when there is no more code to write.
- User writes $ g++ 'filename' *($ g++ file.g++)*, the code in the specified file is shown as reserved for tokens.


##CFGs

 START -> INPUT  
  INPUT -> EXPI | EXPLISTI  
  
  EXPI -> ( set ID EXPI )  
  EXPI -> ( , EXPI EXPI ) 
  EXPI -> ( “ EXPI EXPI )  
  EXPI -> ( ” EXPI EXPI )  
  EXPI -> ( ( EXPI EXPI ) 
  EXPI -> ( ) EXPI EXPI ) 
  EXPI -> ( + EXPI EXPI )  
  EXPI -> ( - EXPI EXPI )  
  EXPI -> ( / EXPI EXPI ) 
  EXPI -> ( * EXPI EXPI )  
  EXPI -> (** EXPI EXPI ) 
  
  EXPI -> ID | (ID EXPLISTI) | VALUES  
  EXPI -> (deffun ID IDLIST EXPLISTI)  
  EXPI -> (ID EXPLISTI)  
  EXPI -> (if EXPB EXPLISTI)  
  EXPI -> (if EXPB EXPLISTI EXPLISTI)  
  EXPI -> (for (ID EXPI EXPI) EXPLISTI) 
  
  EXPB -> (and EXPB EXPB)  
  EXPB -> (or EXPB EXPB)  
  EXPB -> (not EXPB)  
  EXPB -> (equal EXPB EXPB)  
  EXPB -> (equal EXPI EXPI) 
  EXPB -> (less EXPB EXPB)  
  
  EXPLISTI -> EXPI | (concat EXPLISTI EXPLISTI) | (append EXPI EXPLISTI) | null | ‘( VALUES ) | ‘()  
  VALUES -> VALUES IntegerValue | IntegerValue  
  IDLIST -> ID | (IDLIST) | ID IDLIST 
 
