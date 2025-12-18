#!chezscheme
;./build.ss && petite -b ./main.boot
(import (chezscheme)
        (scrutiny scrutiny)
        (tiny-struct tiny-struct))

(suppress-greeting #t)
;(test-compile)

(define *std* (load-shared-object "libSystem.dylib"))

(define-ftype winsize
    (struct
        [ws-row unsigned-short]
        [ws-col unsigned-short]
        [ws-xpixel unsigned-short]
        [ws-ypixel unsigned-short]))

(define-ftype termios
    (struct
      [c-iflag long]
      [c-oflag long]
      [c-clfag long]
      [c-lflag long]
      [cc-t (array 20 char )]
      [c-ispeed long]
      [c-ospeed long]))

(define TIOCGWINSZ 1074295912) ;;check spelling
(define ioctl (foreign-procedure (__varargs_after 2)"ioctl" (int unsigned-long (* winsize)) int))
(define tcsetattr (foreign-procedure #f "tcsetattr" (int int (* termios)) int))
(define tcgetattr (foreign-procedure #f "tcgetattr" (int (* termios)) int))
(define setlocale (foreign-procedure #f "setlocale" (int string) string))

(define-tiny-struct cell
    u 4 type
    u 4 dir)

(define fps-nano (make-time 'time-duration 33333333 0))
(define max-rows #f)
(define max-cols #f)
(define max-width #f)
(define game-state #f)
(define head #f)
(define tail #f)

(define empty 0)
(define snake 1)
(define food 2)
(define north 0)
(define south 1)
(define east 2)
(define west 3)

(define-values (p c) (open-string-output-port))

(define new-food-index
    (lambda ()
        (let roll ([attempt (random (bytevector-length game-state))])
                (if (fx= (cell-type (bytevector-u8-ref game-state attempt)) snake)
                    (roll (random (bytevector-length game-state)))
                    attempt))))

(define init
    (lambda ()
        (test-if
            (begin
                (set! max-cols (fx1+ (random 1000)))
                (set! max-rows (fx1+ (random 1000))))
            (begin
;                (setlocale 0 "en_US.UTF-8") ;;6 on linux for LC_ALL
                (raw 'on)
                (screen-cmd #\? #\1 #\0 #\4 #\9 #\h)
                (hide-cursor)
                (display (c))
                (set-screen-limits)))
        (if (odd? max-cols)
            (set! max-width (fx1+ (fx/ max-cols 2)))
            (set! max-width (fx/ max-cols 2)))
        (when (even? max-rows)
            (set! max-rows (fx1- max-rows))) ;;WHY IS THIS NEEDED??
        (set! game-state (make-bytevector (fx* max-width max-rows) 0))
        (let* ([start-cell (cell-dir-set (cell-type-set 0 snake) (random 4))]
               [index (fx/ (bytevector-length game-state) 2)])
            (bytevector-u8-set! game-state index start-cell)
            (bytevector-u8-set! game-state (new-food-index) (cell-type-set 0 food))
            (set! head index)
            (set! tail index))))

(define-syntax define-with-state
    (lambda (stx)
        (syntax-case stx ()
        [(_ name ((sname state) ...) body)
         #`(define name ((lambda ()
                            (let* ((sname state) ...)
                                body))))])))

(define-with-state raw ([tios (make-ftype-pointer termios (foreign-alloc (ftype-sizeof termios)))])
    (lambda opts
        (define on? (memq 'on opts))
        (define ECHO #x00000008)
        (define ISIG #x00000080)
        (define ICANON #x00000100)
        (define IXON #x00000200)
        (define IEXTEN #x00000400)
        ;(define ICRNL #x00000100)
        (define TCSAFLUSH 2)
        (tcgetattr 0 tios)
        (let ([lflag (ftype-ref termios (c-lflag) tios)]
              [iflag (ftype-ref termios (c-iflag) tios)])
            (if on?
                (begin
                    (ftype-set! termios (c-lflag) tios (fxand lflag (fxnot (fxior ECHO ICANON 
;ISIG
 IEXTEN))))
                    (ftype-set! termios (c-iflag) tios (fxand iflag (fxnot (fxior IXON)))))
                (begin
                    (ftype-set! termios (c-lflag) tios (fxior lflag (fxior ECHO ICANON ISIG IEXTEN)))
                    (ftype-set! termios (c-iflag) tios (fxior iflag (fxior IXON)))))
            (tcsetattr 0 TCSAFLUSH  tios))))

(define-with-state set-screen-limits ([w (make-ftype-pointer winsize (foreign-alloc (ftype-sizeof winsize)))])
    (lambda ()
        (ioctl 0 TIOCGWINSZ w)
        (set! max-rows (ftype-ref winsize (ws-row) w))
        (set! max-cols (ftype-ref winsize (ws-col) w))))

(define screen-cmd
    (lambda sequence
        (display #\esc p)
        (display #\[ p)
        (for-each (lambda (x) (display x p)) sequence)))

(define erase
    (lambda ()
        (screen-cmd #\2 #\J)))

(define-unit move
    ((y fixnum?) 
     (x fixnum?)
     any?)
    (screen-cmd y #\; x #\H ))

(define hide-cursor
    (lambda ()
        (screen-cmd #\? #\2 #\5 #\l)))

(define show-cursor
    (lambda ()
        (screen-cmd #\? #\2 #\5 #\h)))

(define endwin
    (lambda ()
        (erase)
        (move 0 0)
        (screen-cmd #\? #\1 #\0 #\4 #\9 #\l)
        (show-cursor)
        (raw 'off)
        (display (c))))

(define-unit set-color
    ((x fixnum?)
     any?)
    (screen-cmd x #\m))

(define-unit draw 1000
    (any?)
    (test-execute
        (define e (init))
        (define virtual-display (make-vector (fx* max-cols max-rows) 0))
        (define index 0)
        (define set-color (lambda (n) (void)))
        (define display (lambda (ch x) (vector-set! virtual-display index ch)
                                       (set! index (fx1+ index)))))
    (let loop ([i 0])
        (when (fx< i (bytevector-length game-state))
            (case (cell-type (bytevector-u8-ref game-state i))
                (0 (set-color 49) (display #\_ p))
                (1 (set-color 40) (display #\_ p))
                (else (set-color 41) (display #\_ p)))
            (when (not (and (odd? max-cols) (fx= (fxmod i max-width) (fx1- max-width))))
                  (set-color 49)
                  (display #\| p))
            (loop (fx1+ i))))
    (test-execute
        (let loop ([i 0])
            (when (fx< i (vector-length virtual-display))
                (let ([c (vector-ref virtual-display i)])
                    (assert (or (char=? c #\_) (char=? c #\|)))
                    (loop (fx1+ i)))))))

(define-unit new-pos
    ((cur (is-all? fixnum? (lambda (n) (fx< n (bytevector-length game-state)))))
     (dir (is-all? whole-fxnumber? (lambda (n) (fx< n 4))))
     (lambda (n) (or (boolean? n)
                    (and (whole-fxnumber? n)
                         (fx< n (bytevector-length game-state))))))
    (cond
        ((fx= dir north)
         (let ([n (fx- cur max-width)]) (if (fx< n 0) #f n)))
        ((fx= dir south)
         (let ([n (fx+ cur max-width)]) (if (fx>= n (bytevector-length game-state)) #f n)))
        ((fx= dir east)
         (let ([n (fx1+ cur)]) (if (or (fx>= n (bytevector-length game-state))
                                       (not (fx= (fx/ n max-width) (fx/ cur max-width)))) #f n)))
        ((fx= dir west)
         (let ([n (fx1- cur)]) (if (or (fx< n 0)
                                       (not (fx= (fx/ n max-width) (fx/ cur max-width)))) #f n)))))

(define-unit opposite?
    ((dir1 (is-all? whole-fxnumber? (lambda (n) (fx< n 4))))
     (dir2 (is-all? whole-fxnumber? (lambda (n) (fx< n 4))))
     boolean?)
    (let ([s (fx+ dir1 dir2)])
        (or (fx= s 1) (fx= s 5))))

(define-unit update 1000
    ((dir
          (lambda (n) (or (boolean? n)
                          (and (fx< n 4)
                               (whole-fxnumber? n)))) 
          (lambda () (case (random 5)
                        (0 #f)
                        (else (random 4)))))
    (lambda (n) (memq n '(nothing ate oob))))

    (test-execute (init))
    (let* ([hcell (bytevector-u8-ref game-state head)]
           [hdir (cell-dir hcell)]
           [new-dir (if (and dir (not (opposite? dir hdir))) dir hdir)]
           [new-head (new-pos head new-dir)]
           [new-head-type (if new-head (cell-type (bytevector-u8-ref game-state new-head)) #f)])
        (if (and new-head (not (fx= new-head-type snake)))
            (begin 
                (test-execute (assert new-head-type))
                (bytevector-u8-set! game-state head (cell-dir-set hcell new-dir))
                (move (fx1+ (fx/ new-head max-width)) (fx1+ (fx* 2 (fxmod new-head max-width))))
                (set-color 40)
                (display #\_ p)
                (bytevector-u8-set! game-state new-head (cell-dir-set (cell-type-set 0 snake) new-dir))
                (set! head new-head)
                (if (not (fx= new-head-type food))
                    (let ([tcell (bytevector-u8-ref game-state tail)])
                         (bytevector-u8-set! game-state tail 0)
                         (move (fx1+ (fx/ tail max-width)) (fx1+ (fx* 2 (fxmod tail max-width))))
                         (set-color 49)
                         (display #\_ p)
                         (set! tail (new-pos tail (cell-dir tcell)))
                         'nothing)
                    'ate))
            'oob)))

(define input
    (lambda ()
        (if (char-ready?)
            (case (read-char)
                ;;dvorak bindings
                (#\, north)
                (#\o south)
                (#\e east)
                (#\a west)
                (else #f)) 
            #f)))

(define game-loop    
    (lambda ()
        (let ([state (update (input))])
            (case state
                ('ate
                 (let ([nf (new-food-index)])
                     (bytevector-u8-set! game-state nf (cell-type-set 0 food))
                     (move (fx1+ (fx/ nf max-width)) (fx1+ (fx* 2 (fxmod nf max-width))))
                     (set-color 41)
                     (display #\_ p)))
                ('oob (endwin) (exit))))
;        (move 0 0)
;        (draw)
        (display (c))
        (sleep fps-nano)
        (game-loop)))

(scheme-start
    (lambda ()
        (test-if
            (display "done")
            (begin
                (init)
                (move 1 1)
                (draw)
                (display (c))
                (game-loop)))))