(setq Keywordlist '("and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "then" "else"))
(setq Binarylist '("true" "false"))
(setq Operatorlist '("+" "-" "/" "*"))
(setq Mainlist '())
(setq Tokenlist '())


; Her operatorden sonra identifier da yaziyor belirtmek isterim
; Her operatorden sonra identifier da yaziyor belirtmek isterim
; Her operatorden sonra identifier da yaziyor belirtmek isterim
; Her operatorden sonra identifier da yaziyor belirtmek isterim


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
  (setq control2 0)
  (setq ls (split '(#\space) line))


  (if (= control1 0)

  (and

    (setq tempsize1 0)
    (loop while (< tempsize1 (- (length ls) 0)) 
        do(if  (and (search "(" (nth tempsize1 ls) ) (> (length(nth tempsize1 ls)) 1) )
            (and
              (setq temp1(append temp1 (list (subseq (nth 0 ls) 0 1))))
              (setq temp1(append temp1 (list (subseq (nth 0 ls) 1 (length (nth 0 ls))))))
              (setq Tokenlist (cons Tokenlist (nth 0 temp1)))
              (setq Tokenlist (cons Tokenlist (nth 1 temp1)))
              (setq Mainlist (cons Mainlist "LeftParanthesis" ))
              (setq Mainlist (cons Mainlist "Identifier or Operator or Keyword" ))
              (setq newLs ls)  
              )

        )
        do(if  (and (search ")" (nth tempsize1 ls) ) (> (length(nth tempsize1 ls)) 1) )
            (and
              (setq temp1(append temp1 (list (subseq (nth 0 ls) 0 1))))
              (setq temp1(append temp1 (list (subseq (nth 0 ls) 1 (length (nth 0 ls))))))
              (setq Tokenlist (cons (nth 0 temp1) Tokenlist ))
              (setq Tokenlist (cons (nth 1 temp1) Tokenlist ))
              (setq Mainlist (cons Mainlist "Identifier or Operator or Keyword" ))
              (setq Mainlist (cons Mainlist "RightParanthesis" ))
              (setq newLs ls)  
              )

        )
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
        do(if (not(or (find (nth tempsize1 ls) Binarylist) (find (nth tempsize1 ls) Operatorlist) (find (nth tempsize1 ls) Keywordlist)   ))
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Identifier" ))   
              
              )

        )
         
        do(if (ignore-errors(numberp (read-from-string (nth tempsize1 ls))))
            (and
              (setq Tokenlist (cons Tokenlist (nth tempsize1 ls)))
              (setq Mainlist (cons Mainlist "Integer" ))   
              
              )

        ) 
        

        (setq tempsize1 (+ tempsize1 1))


    )
   

    
  )

  )
  (print Mainlist)
  (print Tokenlist)

  (setq Mainlist '())
  (setq Tokenlist '())

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

	(if  (search "g++" user_input)
      (lexer  "G++.g++")
      (print "Wrong Input!!!")
    )

    )

(main)


