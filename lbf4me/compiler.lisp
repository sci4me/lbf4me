(import lua/basic (get-idx set-idx!))

(import lbf4me/ir ir)

(defun ir-to-lua (ir)
  (let* [(result "")
          (level 0)
          (emit! (lambda (s) (set! result (.. result s))))]
    (emit! "local tape = {}\n")
    (emit! "local dp = 1\n")
    (emit! "for i = 1, 30000 do tape[i] = 0 end\n")
    (for i 1 (n ir) 1
      (let* [(insn (get-idx ir i))
              (op (.> insn :op))
              (offset (.> insn :offset))
              (offset-src (.> insn :offset-src))
              (offset-dst (.> insn :offset-dst))
              (cnt (.> insn :count))]
        (when (= op ir/op_close)
          (dec! level))
        (for i 1 level 1
          (emit! "    "))
        (cond [(= op ir/op_inc)
                (emit! $"tape[dp + ${offset}] = tape[dp + ${offset}] + ${cnt}\n")]
              [(= op ir/op_dec)
                (emit! $"tape[dp + ${offset}] = tape[dp + ${offset}] - ${cnt}\n")]
              [(= op ir/op_right)
                (emit! $"dp = dp + ${cnt}\n")]
              [(= op ir/op_left)
                (emit! $"dp = dp - ${cnt}\n")]
              [(= op ir/op_write)
                (emit! $"io.write(string.char(tape[dp + ${offset}])) io.flush()\n")]
              [(= op ir/op_read)
                (emit! $"do local c = io.read(1) if c then tape[dp + ${offset}] = string.byte(c) else tape[dp + ${offset}] = 0 end end\n")]
              [(= op ir/op_open)
                (emit! $"while tape[dp + ${offset}] ~= 0 do\n")
                (inc! level)]
              [(= op ir/op_close)
                (emit! "end\n")]
              [(= op ir/op_clear)
                (emit! $"tape[dp + ${offset}] = 0\n")]
              [(= op ir/op_move)
                (emit! $"if tape[dp + ${offset-src}] ~= 0 then tape[dp + ${offset-dst}] = tape[dp + ${offset-dst}] + tape[dp + ${offset-src}] tape[dp + ${offset-src}] = 0 end\n")]
              [(= op ir/op_copy)
                (emit! $"if tape[dp + ${offset-src}] ~= 0 then tape[dp + ${offset-dst}] = tape[dp + ${offset-dst}] + tape[dp + ${offset-src}] end\n")])))
    result))

(defun ir-to-c (ir)
  (let* [(result "")
          (level 1)
          (emit! (lambda (s) (set! result (.. result s))))]
    (emit! "#include <stdio.h>\n")
    (emit! "int main(int argc, char **argv) {\n")
    (emit! "    unsigned char tape[30000];\n")
    (emit! "    unsigned int dp = 0;\n")
    (for i 1 (n ir) 1
      (let* [(insn (nth ir i))
              (op (.> insn :op))
              (offset (.> insn :offset))
              (offset-src (.> insn :offset-src))
              (offset-dst (.> insn :offset-dst))
              (cnt (.> insn :count))]
        (when (= op ir/op_close)
          (dec! level))
        (for i 1 level 1
          (emit! "    "))
        (cond [(= op ir/op_inc)
                (emit! $"tape[dp + ${offset}] += ${cnt};\n")]
              [(= op ir/op_dec)
                (emit! $"tape[dp + ${offset}] -= ${cnt};\n")]
              [(= op ir/op_right)
                (emit! $"dp += ${cnt};\n")]
              [(= op ir/op_left)
                (emit! $"dp -= ${cnt};\n")]
              [(= op ir/op_write)
                (emit! $"putchar(tape[dp + ${offset}]); fflush(stdout);\n")]
              [(= op ir/op_read)
                (emit! $"tape[dp + ${offset}] = getchar();\n")]
              [(= op ir/op_open)
                (emit! $"while(tape[dp + ${offset}]) {\n")
                (inc! level)]
              [(= op ir/op_close)
                (emit! "}\n")]
              [(= op ir/op_clear)
                (emit! $"tape[dp + ${offset}] = 0;\n")]
              [(= op ir/op_move)
                (emit! $"if(tape[dp + ${offset-src}]) { tape[dp + ${offset-dst}] += tape[dp + ${offset-src}]; tape[dp + ${offset-src}] = 0; }\n")]
              [(= op ir/op_move)
                (emit! $"if(tape[dp + ${offset-src}]) tape[dp + ${offset-dst}] += tape[dp + ${offset-src}];\n")])
        ))
    (emit! "}")
    result))