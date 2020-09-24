(import lua/basic (get-idx set-idx!))

(import lbf4me/ir ir)

(defmacro matcher (&body)
  (let* [(ir (gensym))
          (matches (gensym))
          (i (gensym))
          (m (gensym))
          (pattern (gensym))]
    (for-each e body
      (with (op (symbol->string (get-idx e 1)))
        (cond [(= op "inc")
                (set-idx! e 1 ir/op_inc)]
              [(= op "dec")
                (set-idx! e 1 ir/op_dec)]
              [(= op "right")
                (set-idx! e 1 ir/op_right)]
              [(= op "left")
                (set-idx! e 1 ir/op_left)]
              [(= op "write")
                (set-idx! e 1 ir/op_write)]
              [(= op "read")
                (set-idx! e 1 ir/op_read)]
              [(= op "open")
                (set-idx! e 1 ir/op_open)]
              [(= op "close")
                (set-idx! e 1 ir/op_close)]
              [(= op "clear")
                (set-idx! e 1 ir/op_clear)]
              [(= op "move")
                (set-idx! e 1 ir/op_move)]
              [(= op "copy")
                (set-idx! e 1 ir/op_copy)]
              [else (fail! $"invalid instruction")])))
    `(lambda (,ir)
                (let* [(,pattern ',body)
                        (,matches '())]
                  (for ,i 1 (n ,ir) 1
                    (when-with (,m (match ,ir ,pattern ,i))
                      (push-cdr! ,matches ,m)
                      (set! ,i (+ (.> ,m :end) 1))))
                  ,matches))))

(defun match (ir pattern index) :hidden
  (let* [(match { :start index
                  :end (+ index (- (n pattern) 1))
                  :data '() })
          (i 1)
          (vars {})
          (lookup (lambda (var val) (with (value (get-idx vars var))
                                  (if value
                                      value
                                      (progn (set-idx! vars var val)
                                              val)))))
          (check! (lambda (a b) (when (= (type a) "symbol")
                                  (set! a (lookup (symbol->string a) b)))
                                (when (= (type b) "symbol")
                                  (set! b (lookup (symbol->string b) a)))
                                (when (or (= a nil) (= b nil) (/= a b))
                                    (set! match nil))))]
    (while (and (<= i (n pattern)) match)
      (when match
        (let* [(insn (nth ir (- (+ index i) 1)))
                (op (.> insn :op))
                (cnt (.> insn :count))
                (offset (.> insn :offset))
                (target (nth pattern i))]
          (cond [(/= (.> insn :op) (nth target 1))
                  (set! match nil)]
                [else (push-cdr! (.> match :data) insn)
                      (cond [(or (= op ir/op_inc) (= op ir/op_dec))
                              (check! offset (nth target 2))
                              (check! cnt (nth target 3))]
                            [(or (= op ir/op_left) (= op ir/op_right))
                              (check! cnt (nth target 2))]
                            [(or (= op ir/op_read) (= op ir/op_write) (= op ir/op_clear) (= op ir/op_open))
                              (check! offset (nth target 2))]
                            [(or (= op ir/op_move) (= op ir/op_copy))
                              (check! (.> insn :offset-src) (nth target 2))
                              (check! (.> insn :offset-dst) (nth target 3))]
                            [else nil])])))
      (inc! i))
    (when match
      (.<! match :vars vars))
    match))