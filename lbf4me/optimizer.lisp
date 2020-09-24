(import lua/basic (get-idx set-idx!))
(import lua/os os)

(import lbf4me/ir ir)
(import lbf4me/pattern pattern)

(define clear-loop-removal :hidden { :matcher (pattern/matcher (open a) (dec a 1) (close))
                                      :replacer (lambda (match) (list { :op ir/op_clear :offset 0 })) })

(defun move-loop-removal-replacer (match) :hidden
  (list { :op ir/op_move :offset-src (.> (.> match :vars) :src) :offset-dst (.> (.> match :vars) :dst) }))

(define move-loop-removal-a :hidden { :matcher (pattern/matcher (open src) (dec src 1) (inc dst 1) (close))
                                      :replacer move-loop-removal-replacer })

(define move-loop-removal-b :hidden { :matcher (pattern/matcher (open src) (inc dst 1) (dec src 1) (close))
                                      :replacer move-loop-removal-replacer })

(defun optimize-ir (ir)
  (let* [(opts (list move-loop-removal-a move-loop-removal-b clear-loop-removal))
          (delta 1)
          (start (n ir))]
    (while (> delta 0)
      (with (l (n ir))
        (for-each opt opts
          (set! ir (match-and-replace ir (.> opt :matcher) (.> opt :replacer))))
        (set! delta (- l (n ir)))))
    (print! (.. "opt: -" (- start (n ir))))
    ir))

(defun match-and-replace (ir matcher replacer) :hidden
  (let* [(matches (matcher ir))
          (nir '())
          (i 1)
          (indices {})]
    (for-each match matches
      (set-idx! indices (.> match :start) match))
    (while (<= i (n ir))
      (let* [(match (get-idx indices i))
              (np nil)]
        (when match
          (set! np (replacer match)))
        (cond [(and match np)
                (for-each e np
                  (push-cdr! nir e))
                (set! i (+ (.> match :end) 1))]
              [else
                (push-cdr! nir (get-idx ir i))
                (set! i (+ i 1))])))
    nir))