(import lua/basic (get-idx set-idx! load))
(import lua/math ())
(import string (char-at char byte))

(define op_inc 0)
(define op_dec 1)
(define op_left 2)
(define op_right 3)
(define op_read 4)
(define op_write 5)
(define op_open 6)
(define op_close 7)
(define op_clear 8)
(define op_move 9)
(define op_copy 10)

(defun parse (code)
  (let* [(result '())
         (pointer 0)
         (buffer {})
         (pointer-min 0)
         (pointer-max 0)
         (emit! (lambda (insn) (push-cdr! result insn)))
         (reset-pointer! (lambda (move)
            (when (and (/= pointer 0) move)
              (with (insn {})
                (if (< pointer 0)
                    (.<! insn :op op_left)
                    (.<! insn :op op_right))
                (.<! insn :count (abs pointer))
                (emit! insn)))
            (set! pointer 0)
            (set! pointer-min 0)
            (set! pointer-max 0)
            (set! buffer {})))
         (emit-buffer! (lambda ()
            (for i pointer-min pointer-max 1
              (with (be (get-idx buffer i))
                (when (and (/= be nil) (/= be 0))
                  (with (insn {})
                    (if (< be 0)
                        (.<! insn :op op_dec)
                        (.<! insn :op op_inc))
                    (.<! insn :offset i)
                    (.<! insn :count (abs be))
                    (emit! insn)))))
            (set! buffer {})))
         (inc! (lambda () (if (get-idx buffer pointer)
                             (set-idx! buffer pointer (+ (get-idx buffer pointer) 1))
                             (set-idx! buffer pointer 1))))
         (dec! (lambda () (if (get-idx buffer pointer)
                             (set-idx! buffer pointer (- (get-idx buffer pointer) 1))
                             (set-idx! buffer pointer -1))))]
    (for i 1 (n code) 1
      (with (c (char-at code i))
        (cond [(= c "+") (inc!)]
              [(= c "-") (dec!)]
              [(= c ">") (set! pointer (+ pointer 1))
                          (when (> pointer pointer-max) (set! pointer-max pointer))]
              [(= c "<") (set! pointer (- pointer 1))
                          (when (< pointer pointer-min) (set! pointer-min pointer))]
              [(= c ".") (emit-buffer!)
                          (emit! { :op op_write
                                    :offset pointer })]
              [(= c ",") (emit-buffer!)
                          (emit! { :op op_read
                                    :offset pointer })]
              [(= c "[") (emit-buffer!)
                          (reset-pointer! true)
                          (emit! { :op op_open
                                    :offset pointer })]
              [(= c "]") (emit-buffer!)
                          (reset-pointer! true)
                          (emit! { :op op_close })]
              [else nil])))
    ;(use-loop-offsets result)
    result))

(defun ir->string (ir)
  (let* [(result "")
          (level 0)
          (emit! (lambda (s) (set! result (.. result s))))]
    (for-each insn ir
      (let* [(op (.> insn :op))
              (cnt (.> insn :count))
              (offset (.> insn :offset))
              (offset-src (.> insn :offset-src))
              (offset-dst (.> insn :offset-dst))
              (name nil)]
        (when (= op op_close)
          (set! level (- level 1)))
        (for i 1 level 1
          (emit! "    "))
        (cond [(= op op_inc)
                (emit! $"inc [${offset}] ${cnt}\n")]
              [(= op op_dec)
                (emit! $"dec [${offset}] ${cnt}\n")]
              [(= op op_left)
                (emit! $"left ${cnt}\n")]
              [(= op op_right)
                (emit! $"right ${cnt}\n")]
              [(= op op_write)
                (emit! $"write [${offset}]\n")]
              [(= op op_read)
                (emit! $"read [${offset}]\n")]
              [(= op op_open)
                (emit! $"open [${offset}]\n")
                (set! level (+ level 1))]
              [(= op op_close)
                (emit! "close\n")]
              [(= op op_clear)
                (emit! $"clear [${offset}]\n")]
              [(= op op_move)
                (emit! $"move [${offset-src}] [${offset-dst}]\n")]
              [(= op op_copy)
                (emit! $"copy [${offset-src}] [${offset-dst}]\n")])))
    result))

(defun build-jump-table (ir)
  (let* [(jump-table {})
          (loop-stack '())
          (push! (lambda (i) (push-cdr! loop-stack i)))
          (pop! (lambda () (with (i (nth loop-stack (n loop-stack)))
                            (remove-nth! loop-stack (n loop-stack))
                            i)))]
    (for i 1 (n ir) 1
      (let* [(insn (nth ir i))
              (op (.> insn :op))]
        (cond [(= op op_open)
                (push! i)]
              [(= op op_close)
                (with (start (pop!))
                  (set-idx! jump-table i (+ start 1))
                  (set-idx! jump-table start (+ i 1)))]
              [else nil])))
    jump-table))

(defun build-balance-table (ir)
  (let* [(balance-table {})
          (lstate nil)
          (loop-stack '())
          (push! (lambda (i) (push-cdr! loop-stack i)))
          (pop! (lambda () (with (i (nth loop-stack (n loop-stack)))
                            (remove-nth! loop-stack (n loop-stack))
                            i)))]
    (for i 1 (n ir) 1
      (let* [(insn (nth ir i))
              (op (.> insn :op))]
        (cond [(= op op_left)
                (when lstate
                  (.<! lstate :lefts (+ (.> lstate :lefts) 1)))]
              [(= op op_right)
                (when lstate
                  (.<! lstate :rights (+ (.> lstate :rights) 1)))]
              [(= op op_open)
                (push! lstate)
                (set! lstate { :start i :lefts 0 :rights 0 })]
              [(= op op_close)
                (let* [(balanced (= (.> lstate :lefts) (.> lstate :rights)))]
                  (set-idx! balance-table i balanced)
                  (set-idx! balance-table (.> lstate :start) balanced))
                (set! lstate (pop!))]
              [else nil])))
    balance-table))

(defun use-loop-offsets (ir) :hidden
  (let* [(nir '())
          (balance-table (build-balance-table ir))]
    ir))