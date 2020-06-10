(defun lexer (filename)
	
  	(setq liste'())
    (readFileAsList filename)
    (setq temp "")
    (setq cnt 0)
    (setq cnb 0)
    (parserMain liste)

)
(defun isInt(x)

  (setf x1 (position #\Space x))

  (setf x2 (position #\) x))

  (setf x3 (position #\Newline x))

  (if (and (<= x1 x2) (<= x1 x3))
    (setq tut (subseq x 0 x1))
    (if (and (<= x2 x1) (<= x2 x3))
      (setq tut (subseq x 0 x2))
      (if (and (<= x3 x2) (<= x3 x1))
        (setq tut (subseq x 0 x3))
      )

    )
  )
  
  (if (equal (length tut) 1)
    (if (or (equal (car tut) #\0) (equal (car tut) #\1) (equal (car tut) #\2) (equal (car tut) #\3) (equal (car tut) #\4) (equal (car tut) #\5) (equal (car tut) #\6) (equal (car tut) #\7) (equal (car tut) #\8) (equal (car tut) #\9) )
    
      T

    )
    (if (or (equal (car tut) #\0) (equal (car tut) #\1) (equal (car tut) #\2) (equal (car tut) #\3) (equal (car tut) #\4) (equal (car tut) #\5) (equal (car tut) #\6) (equal (car tut) #\7) (equal (car tut) #\8) (equal (car tut) #\9) )
      (and (setf cnt (+ cnt 1)) (isInt (cdr x)))

    )

  )
  

)

(defun isId(x)

  (setf x1 (position #\Space x))

  (setf x2 (position #\) x))

  (setf x3 (position #\Newline x))

  (if (and (<= x1 x2) (<= x1 x3))
    (setq tut (subseq x 0 x1))
    (if (and (<= x2 x1) (<= x2 x3))
      (setq tut (subseq x 0 x2))
      (if (and (<= x3 x2) (<= x3 x1))
        (setq tut (subseq x 0 x3))
      )

    )
  )
  
  (if (equal (length tut) 1)
    (if (and (not (equal (car tut) #\+))  (not (equal (car tut) #\-))  (not (equal (car tut) #\*))  (not (equal (car tut) #\/))  (not (equal (car tut) #\( ))  (not (equal (car tut) #\) ))  (not (equal (car tut) #\, ))  (not (equal (car tut) #\;))  (not (equal (car tut) #\"))  (not (equal (car tut) #\Space)) (not (equal (car tut) #\Newline)) )
    
      T

    )
    (if (and (not (equal (car tut) #\+))  (not (equal (car tut) #\-))  (not (equal (car tut) #\*))  (not (equal (car tut) #\/))  (not (equal (car tut) #\( ))  (not (equal (car tut) #\) ))  (not (equal (car tut) #\, ))  (not (equal (car tut) #\;))  (not (equal (car tut) #\"))  (not (equal (car tut) #\Space)) (not (equal (car tut) #\Newline)) )
      (and (setf cnb (+ cnb 1)) (isId (cdr x)))

    )

  )
  

)

(defun parserMain(liste)
   
    (if (> (length liste) 0)

    (if (and (equal (car liste) #\;) (equal (car (cdr liste)) #\;) )
      (and (print (setf temp "COMMENT")) (parserMain (nthcdr (+ 1 (position #\Newline liste)) liste) ) )
      (if (equal (car liste) #\( )
        (and (print (setf temp "OP_OP")) (parserMain (cdr liste)))
        (if (and (equal (car liste) #\i )  (equal (car (cdr liste)) #\f ) )
          (and (print (setf temp "KW_IF")) (parserMain (cdr (cdr liste))) )           
          (if (and (equal (car liste) #\a )  (equal (car (cdr liste)) #\n ) (equal (car (cdr (cdr liste))) #\d ))
            (and (print (setf temp "KW_AND")) (parserMain (cdr (cdr (cdr liste)))))
            (if (and (equal (car liste) #\o )  (equal (car (cdr liste)) #\r ) )
              (and (print (setf temp "KW_OR")) (parserMain (cdr (cdr liste))))
              (if (and (equal (car liste) #\n )  (equal (car (cdr liste)) #\o ) (equal (car (cdr (cdr liste))) #\t ))
                (and (print (setf temp "KW_NOT")) (parserMain (cdr (cdr (cdr liste)))))
                (if (and (equal (car liste) #\e )  (equal (car (cdr liste)) #\q ) (equal (car (cdr (cdr liste))) #\u ) (equal (car (cdr (cdr (cdr liste)))) #\a ) (equal (car (cdr (cdr (cdr (cdr liste))))) #\l ) )
                  (and (print (setf temp "KW_EQUAL")) (parserMain (cdr (cdr (cdr (cdr (cdr liste)))))))
                  (if (and (equal (car liste) #\l )  (equal (car (cdr liste)) #\e ) (equal (car (cdr (cdr liste))) #\s ) (equal (car (cdr (cdr (cdr liste)))) #\s ) )
                    (and (print (setf temp "KW_LESS")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                    (if (and (equal (car liste) #\l )  (equal (car (cdr liste)) #\i ) (equal (car (cdr (cdr liste))) #\s ) (equal (car (cdr (cdr (cdr liste)))) #\t ) )
                      (and (print (setf temp "KW_LIST")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                      (if (and (equal (car liste) #\n )  (equal (car (cdr liste)) #\i ) (equal (car (cdr (cdr liste))) #\l ))
                        (and (print (setf temp "KW_NIL")) (parserMain (cdr (cdr (cdr liste)))))
                        (if (and (equal (car liste) #\a )  (equal (car (cdr liste)) #\p ) (equal (car (cdr (cdr liste))) #\p ) (equal (car (cdr (cdr (cdr liste)))) #\e ) (equal (car (cdr (cdr (cdr (cdr liste))))) #\n ) (equal (car (cdr (cdr (cdr (cdr (cdr liste)))))) #\d )  )
                          (and (print (setf temp "KW_APPEND")) (parserMain (cdr (cdr (cdr (cdr (cdr (cdr liste))))))))
                          (if (and (equal (car liste) #\c )  (equal (car (cdr liste)) #\o ) (equal (car (cdr (cdr liste))) #\n ) (equal (car (cdr (cdr (cdr liste)))) #\c ) (equal (car (cdr (cdr (cdr (cdr liste))))) #\a ) (equal (car (cdr (cdr (cdr (cdr (cdr liste)))))) #\t )  )
                            (and (print (setf temp "KW_CONCAT")) (parserMain (cdr (cdr (cdr (cdr (cdr (cdr liste))))))))
                            (if (and (equal (car liste) #\s )  (equal (car (cdr liste)) #\e ) (equal (car (cdr (cdr liste))) #\t ))
                              (and (print (setf temp "KW_SET")) (parserMain (cdr (cdr (cdr liste)))))
                              (if (and (equal (car liste) #\d )  (equal (car (cdr liste)) #\e ) (equal (car (cdr (cdr liste))) #\f ) (equal (car (cdr (cdr (cdr liste)))) #\f ) (equal (car (cdr (cdr (cdr (cdr liste))))) #\u ) (equal (car (cdr (cdr (cdr (cdr (cdr liste)))))) #\n )  )
                                (and (print (setf temp "KW_DEFFUN")) (parserMain (cdr (cdr (cdr (cdr (cdr (cdr liste))))))))
                                (if (and (equal (car liste) #\f )  (equal (car (cdr liste)) #\o ) (equal (car (cdr (cdr liste))) #\r ))
                                  (and (print (setf temp "KW_FOR")) (parserMain (cdr (cdr (cdr liste)))))
                                  (if (and (equal (car liste) #\e )  (equal (car (cdr liste)) #\x ) (equal (car (cdr (cdr liste))) #\i ) (equal (car (cdr (cdr (cdr liste)))) #\t ) )
                                    (and (print (setf temp "KW_EXIT")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                                    (if (and (equal (car liste) #\l )  (equal (car (cdr liste)) #\o ) (equal (car (cdr (cdr liste))) #\a ) (equal (car (cdr (cdr (cdr liste)))) #\d ) )
                                      (and (print (setf temp "KW_LOAD")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                                      (if (and (equal (car liste) #\d )  (equal (car (cdr liste)) #\i ) (equal (car (cdr (cdr liste))) #\s ) (equal (car (cdr (cdr (cdr liste)))) #\p ) )
                                        (and (print (setf temp "KW_DISP")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                                        (if (and (equal (car liste) #\t )  (equal (car (cdr liste)) #\h ) (equal (car (cdr (cdr liste))) #\e ) (equal (car (cdr (cdr (cdr liste)))) #\n ) )
                                          (and (print (setf temp "KW_THEN")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                                          (if (and (equal (car liste) #\e )  (equal (car (cdr liste)) #\l ) (equal (car (cdr (cdr liste))) #\s ) (equal (car (cdr (cdr (cdr liste)))) #\e ) )
                                            (and (print (setf temp "KW_ELSE")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                                            (if (and (equal (car liste) #\t )  (equal (car (cdr liste)) #\r ) (equal (car (cdr (cdr liste))) #\u ) (equal (car (cdr (cdr (cdr liste)))) #\e ) )
                                              (and (print (setf temp "KW_TRUE")) (parserMain (cdr (cdr (cdr (cdr liste))))))
                                              (if (and (equal (car liste) #\f )  (equal (car (cdr liste)) #\a ) (equal (car (cdr (cdr liste))) #\l ) (equal (car (cdr (cdr (cdr liste)))) #\s ) (equal (car (cdr (cdr (cdr (cdr liste))))) #\e ) )
                                                (and (print (setf temp "KW_FALSE")) (parserMain (cdr (cdr (cdr (cdr (cdr liste)))))))
                                                (if (or (equal (car liste) #\Space ) (equal (car liste) #\Newline ))
                                                  (parserMain (cdr liste))
                                                  (if (equal (car liste) #\) )
                                                    (and (print (setf temp "OP_CP")) (parserMain (cdr liste)))
                                                    (if (equal (car liste) #\+ )
                                                      (and (print (setf temp "OP_PLUS")) (parserMain (cdr liste)))
                                                      (if (equal (car liste) #\- )
                                                        (and (print (setf temp "OP_MINUS")) (parserMain (cdr liste)))
                                                        (if (equal (car liste) #\/ )
                                                          (and (print (setf temp "OP_DIV")) (parserMain (cdr liste)))
                                                          (if (equal (car liste) #\* )
                                                            (and (print (setf temp "OP_MULT")) (parserMain (cdr liste)))
                                                            (if (and (equal (car liste) #\* ) (equal (car (cdr liste)) #\* ) )
                                                              (and (print (setf temp "OP_DBLMULT")) (parserMain (cdr liste)))
                                                              (if (equal (car liste) #\" )
                                                                (and (print (setf temp "OP_KESME")) (parserMain (cdr liste)))
                                                                (if (equal (car liste) #\, )
                                                                  (and (print (setf temp "OP_COMMA")) (parserMain (cdr liste)))
                                                                  (and (setf cnt 0) 
                                                                    (if (equal (isInt liste) t)
                                                                      (and (print "VALUE") (parserMain (nthcdr (+ cnt 1) liste) ))
                                                                      (and (setf cnb 0) 
                                                                        (if (equal (isId liste) t)
                                                                          (and (print "IDENTIFIER") (parserMain (nthcdr (+ cnb 1) liste) ))
                                                                          (print "Cannot Tokinezed")

                                                                        )
                                                                      )

                                                                      
                                                                    )
                                                                  )
                                                                  )

                                                              )

                                                            )

                                                            )


                                                          )


                                                        )



                                                      )



                                                  )


                                                  )

                                                )

                                              )


                                            )


                                          )


                                        )

                                      )


                                    )


                                  )

                                )


                              )

                            )

                          )

                      )

                    )

                  )

                )

              )

            )


          )
          
        ) 
      )

    )


  )

)

(defun readFileAsList(filename)


  (with-open-file (stream filename)
    (loop for line = (read-char stream nil)
    while line do (push line liste)
    collect line))
    (setq liste (reverse liste))


  )

(defun gppinterpreter ()

  (setq user_input (read-line))

  (if  (search "g++" user_input)
      (lexer  user_input)
      (print "Wrong Input!!!")
    )

  )

(gppinterpreter)


