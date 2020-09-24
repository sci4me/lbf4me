(import lua/basic (get-idx set-idx! load))
(import lua/io (read write flush stdout))
(import string (char byte))

(import lbf4me/ir ir)

(defun interpret-ir! (ir)
  (let* [(jump-table (ir/build-jump-table ir))
          (tape '())
          (dp 1)
          (ip 1)]
    (for i 1 30000 1
      (set-idx! tape i 0))
    (while (and (>= ip 1) (<= ip (n ir)))
      (let* [(nip (+ ip 1))
              (insn (nth ir ip))
              (op (.> insn :op))
              (offset (.> insn :offset))
              (offset-src (.> insn :offset-src))
              (offset-dst (.> insn :offset-dst))
              (cnt (.> insn :count))]
        (cond [(= op ir/op_inc)
                (with (idx (+ dp offset))
                  (set-idx! tape idx (+ (get-idx tape idx) cnt)))]
              [(= op ir/op_dec)
                (with (idx (+ dp offset))
                  (set-idx! tape idx (- (get-idx tape idx) cnt)))]
              [(= op ir/op_left)
                (set! dp (- dp cnt))]
              [(= op ir/op_right)
                (set! dp (+ dp cnt))]
              [(= op ir/op_write)
                (write (char (get-idx tape (+ dp offset))))
                (flush)]
              [(= op ir/op_read)
                (let* [(c (read 1))
                        (idx (+ dp offset))]
                  (if c
                    (set-idx! tape idx (byte c))
                    (set-idx! tape idx 0)))]
              [(= op ir/op_open)
                (when (= (get-idx tape (+ dp offset)) 0)
                  (set! nip (get-idx jump-table ip)))]
              [(= op ir/op_close)
                (when (/= (get-idx tape dp) 0)
                  (set! nip (get-idx jump-table ip)))]
              [(= op ir/op_clear)
                (set-idx! tape (+ dp offset) 0)]
              [(= op ir/op_move)
                (when (/= (get-idx tape (+ dp offset-src)) 0)
                  (set-idx! tape (+ dp offset-dst) (+ (get-idx tape (+ dp offset-dst)) (get-idx tape (+ dp offset-src))))
                  (set-idx! tape (+ dp offset-src) 0))]
              [(= op ir/op_copy)
                (when (/= (get-idx tape (+ dp offset-src)) 0)
                  (set-idx! tape (+ dp (.> insn :offset-dst)) (get-idx tape (+ dp (.> insn :offset-src)))))])
        (set! ip nip)))))