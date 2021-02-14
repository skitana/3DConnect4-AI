(load "../inference_engine.cl")

;prvo pravilo:
(defun !poen (i j mojpotez stanje)
  (if (mojpotez)
      (progn
         (if (= (length (member 'X (reverse (nth i (nth j stanje))))) 3) (setq isPts t)(setq isPts '()))
        )
      (progn
         (if (= (length (member 'O (reverse (nth i (nth j stanje))))) 3) (setq isPts t)(setq isPts '()))
        )
    )
  isPts
  ;;provere za dijagonalu, horizontalno i vertikalno
  )

;;drugo pravilo:
;;(defun preventPt (i j stanje) //potez koji sprecava poentiranje protivnika)

;;najveci skor bi bio za poentiranje i sprecavanje poentiranja protivnika.

;trece pravilo: losa heuristika za polje oblika (X O) ako smo X, i obrnuto, tj ako nemamo dva ista.

;cetvrto pravilo: ako imamo prvi element u prvoj, drugi u drugoj, obavezno staviti cetvrti u cetvrtoj koloni
;;iako ne donosi poen, a pritom treca u trecoj imamo jedan, jer nam je to skoro zagarantovan poen na kraju partije.
;preracunati unapred da li je poslednji potez nas ili protivnikov naravno.

(defparameter *T1-RULES* '(
	(if (and (X ?a ?b) (!poen ?a ?b t (heurStanje))) then (scoreX))
	(if (and (O ?a ?b) (!poen ?a ?b '() (heurStanje))) then (scoreO))
 ))

(defun heuristika (stanje mojpotez)
    (let* 
        (
            (*T1-FACTS* (novastanja 0 stanje))
        )
        (progn
            (setq heurStanje stanje)
            (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
            (let*
                (
                    (eval1 (if (/= (count-results '(scoreX)) 0) (if (mojpotez) '10 '-10) '0))
                    (eval2 (if (/= (count-results '(scoreO)) 0) (if (not mojpotez) '-10 '10) '0))
           
                    (eval (+ eval1 eval2))
                )
                eval
            )
        )
    )
)