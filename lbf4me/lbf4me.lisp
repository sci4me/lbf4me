(import extra/argparse argparse)
(import extra/io (read-all!))
(import lua/basic (load))
(import lua/os os)

(import lbf4me/ir ir)
(import lbf4me/compiler compiler)
(import lbf4me/interpreter interpreter)
(import lbf4me/optimizer optimizer)

(let* [(spec (argparse/create))
        (args nil)]
  (argparse/add-help! spec)

  (argparse/add-argument! spec '("files")
    :help "The input files")

  (argparse/add-argument! spec '("--optimize" "-o"))

  (argparse/add-argument! spec '("--backend" "-b")
    :help "The backend to use (interp, jit)"
    :narg 1
    :default "jit")

  (set! args (argparse/parse! spec))

  (let* [(files (.> args :files))
         (sources '())
         (append! (lambda (x) (push-cdr! sources x)))]
    (when (<= (n files) 0)
      (argparse/usage-error! spec "lbf4me" "no sources specified"))
    (for-each file files
      (with (file-text (read-all! file))
        (unless file-text (fail! $"could not read file '${file}'"))
        (append! file-text)))
    (let* [(ir (ir/parse (reduce .. "" sources)))
            (backend (.> args :backend))]
      (when (.> args :optimize)
        (set! ir (optimizer/optimize-ir ir)))
      (cond [(= backend "interp")
              (with (start (os/clock))
                (interpreter/interpret-ir! ir)
                (print! (.. (- (os/clock) start) "s")))]
            [(= backend "jit")
              (with (start (os/clock))
                ((load (compiler/ir-to-lua ir)))
                (print! (.. (- (os/clock) start) "s")))]
            [(= backend "ir")
              (print! (ir/ir->string ir))]
            [(= backend "lua")
              (print! (compiler/ir-to-lua ir))]
            [(= backend "c")
              (print! (compiler/ir-to-c ir))]
            [else (print! $"Invalid backend '${backend}'")]))))