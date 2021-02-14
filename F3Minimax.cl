(defun minimax (stanje potez alpha beta dubina moj-potez)
    (cond
        ((zerop dubina) (list potez (if moj-potez (heuristika potez moj-potez stanje) (- (heuristika potez moj-potez stanje)))))
        (t(let*
            (
             (listaMogucihPoteza (novastanja 0 stanje)) ;;pravi listu sa koordinatama mogucih poteza oblika (x y)
                    (result
                        (if moj-potez
                            (maxdeo listaMogucihPoteza '() dubina alpha beta moj-potez stanje);;za nas potez
                            (min listaMogucihPoteza '() dubina alpha beta moj-potez stanje);;za potez protivnika
                          ))
             )
                (cond
                 ((null listaMogucihPoteza) (list potez (if moj-potez (heuristika potez moj-potez stanje) (- (heuristika potez moj-potez stanje)))))
                 (t (list potez (cadr result)))
                )
         ))       
    )
  )

(setq nova (napravilistu niz))

(defun menjaj (i j z stara nova)
  (if (< z (length stara))
  (if(< i (length stara))
      (progn 
        (setf (nth i (nth j (nth z nova))) (nth i (nth j (nth z stara)))) 
        (menjaj (+ 1 i) j z stara nova)
        )
    (progn
      (if(< j (- (length stara) 1))
          (menjaj 0 (+ 1 j) z stara nova)
          (menjaj 0 0 (incf z) stara nova))
      )
    ))
  )

(defun maxdeo (listapoteza najbolji dubina alpha beta moj-potez trenutnostanje)
    (cond 
        ((null listapoteza) (list (trenutnostanje) alpha))
        (t 
            (let*
                (   (prvi (menjaj 0 0 0 trenutnostanje nova)) ;;preslikavanje liste            
                    (drugi (upisi (caar listapoteza) (cadar listapoteza) 0 nova moj-potez)) ;;caar je x cadar je y ((x y) () ()......)
                    (minimalni (minimax nova (car listapoteza)  alpha beta (1- dubina) (not moj-potez)))
                    (novi (if (>= alpha (cadr minimalni)) (list najbolji alpha) minimalni))
                )
                (if (or (> (cadr novi) beta) (null (cdr listapoteza)))
                        (list najbolji (cadr novi))
                        (maxdeo (cdr listapoteza) (car novi) dubina (cadr novi) beta moj-potez trenutnostanje)
                )
            )
        )
    )
)

(defun mindeo (listapoteza najbolji dubina alpha beta moj-potez trenutnostanje)
    (cond 
        ((null listapoteza) (list trenutnostanje alpha))
        (t 
            (let*
                (    
                     (prvi (menjaj 0 0 0 trenutnostanje nova)) ;;preslikavanje liste            
                     (drugi (upisi (caar listapoteza) (cadar listapoteza) 0 nova moj-potez)) ;;caar je x cadar je y ((x y) () ()......) 
                     (maksimalni (minimax prethodna (car listapoteza) alpha beta (1- dubina) (not moj-potez)))
                     (novi (if (<= beta (cadr maksimalni)) (list najbolji beta) maksimalni))
                )
                (if (or (< (cadr novi) alpha)(null (cdr listapoteza)))
                        (list najbolji (cadr novi))
                        (mindeo (cdr listapoteza) (car novi) dubina alpha (cadr novi) moj-potez trenutnostanje)
                )
            )
        )
    )
)