(setq Keywordlist '("and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "then" "else"))
(setq Binarylist '("true" "false"))
(setq Operatorlist '("+" "-" "/" "*"))
(setq Mainlist '())
(setq Tokenlist '())


(defun split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
    (split chars (subseq str 1) (cons accm lst) "")
    (split chars (subseq str 1) 
                        lst 
                        (concatenate 'string
           accm
         (string c))))
            ))))

(defun helper (line)

  (setq temp1 '())
  (setq temp2 '())
  (setq control1 0)
  (setq ls (split '(#\space) line))

  (if (and (> (length (nth 0 ls)) 1)  (search "(" (nth 0 ls))   )
    (and
      (setq temp1(append temp1 (list (subseq (nth 0 ls) 0 1))))
      (setq temp1(append temp1 (list (subseq (nth 0 ls) 1 (length (nth 0 ls))))))
      (setq control1 1)
      ;(print temp1)
    )

  )

  (if (and (> (length (nth (- (length ls) 1) ls)) 1)  (search ")" (nth (- (length ls) 1) ls)   )  )
    (and
      (setq temp2(append temp2 (list (subseq (nth (- (length ls) 1) ls) 0 1))))
      (setq temp2(append temp2 (list (subseq (nth (- (length ls) 1) ls) 1 (length (nth (- (length ls) 1) ls) )))))
      (setq control1 1)
      ;(print temp2)
    )

  )

  (setq ls (cons (car temp1)(append (cdr temp1) (cdr ls))))
  (setq ls (append (subseq ls 0 (- (length ls) 1)) (cons (car temp2) (cdr temp2) )))
  (print ls)

  (if (= control1 1)

  (and

    (setq tempsize1 0)
    (loop while (< tempsize1 (length ls))

        do(if (string= "(" (nth tempsize1 ls))
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "LeftParanthesis" ))   
              )

        )
        do(if (string=  ")" (nth tempsize1 ls))
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "RightParanthesis" ))   
              
              )

        )
        do(if (string= "+" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Operator" ))   
              
              )

        ) 
        do(if (string= "-" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Operator" ))   
              
              )

        )
        do(if (string= "/" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Operator" ))   
              
              )

        )
        do(if (string= "*" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Operator" ))   
              
              )

        )
        do(if (string= "true" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Binary" ))   
              
              )

        )
        do(if (string= "false" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Binary" ))   
              
              )

        ) 
        do(if (string= "and" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "or" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "not" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "equal" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "append" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "concat" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "set" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "deffun" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "for" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "while" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "if" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "then" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        do(if (string= "else" (nth tempsize1 ls) )
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Keyword" ))   
              
              )

        ) 
        
        do(if (ignore-errors(numberp (read-from-string (nth tempsize1 ls))))
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Integer" ))   
              
              )

        )  

        do(if (not(or (find (nth tempsize1 ls) Binarylist) (find (nth tempsize1 ls) Operatorlist) (find (nth tempsize1 ls) Keywordlist)   ))
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Identifier" ))   
              
              )

        ) 


        (setq tempsize1 (+ tempsize1 1))

    )


  )
  

  (print Mainlist)
  )

)

(defun lexer (filename)
	
  	(with-open-file (stream filename)
    (loop for line = (read-line stream nil)

          while line

           do(helper line)
          
          )

    )

	)

(defun main ()

	(setq user_input (read-line))

	(if  (search "coffee" user_input)
      (lexer  "coffee.g++")
      (print "Wrong Input!!!")
    )

    )

(main)


