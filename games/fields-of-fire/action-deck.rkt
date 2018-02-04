#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/list
         syntax/parse/define)

(struct card () #:transparent)
(struct card:reshuffle () #:transparent)
(struct card:action
  (activated-commands
   initiative-commands
   top-evt
   mid-evt
   hq-evt?
   anti-tank-number
   hit-effect-vet
   hit-effect-line
   hit-effect-green
   combat-hit-low
   combat-pin-low
   combat-miss-low
   randoms) #:transparent)

(define-syntax-rule (cards [i . c] ...)
  (list (make-card . c) ...))
(define-syntax (make-card stx)
  (syntax-parse stx
    [(_ (~datum reshuffle)) (syntax/loc stx (card:reshuffle))]
    [(_ ac ic te me hq at hev hel heg chl cpl cml . randoms)
     (syntax/loc stx
       (card:action ac ic te me hq at hev hel heg chl cpl cml
                    (list->vector (list . randoms))))]))

(define C 'Contact)
(define V 'Cover)
(define R 'Rally)

(define X 'Concentrate-Fire)
(define I 'Infiltration)
(define G 'Grenade)
(define F 'Call-for-Fire)
(define F3 'Battalion-Fire) ;; And F!
(define J 'Jam)
(define FS 'Short)

;; L = Litter, F = Fire, A = Assault
(define CC '(Casualty Casualty))
(define CP '(Casualty Paralyzed))
(define CL '(Casualty Litter))
(define LC '(Litter Casualty))
(define CF '(Casualty Fire-Team))
(define FC '(Fire-Team Casualty))
(define PC '(Paralyzed Casualty))
(define PP '(Paralyzed Paralyzed))
(define PL '(Paralyzed Litter))
(define PF '(Paralyzed Fire-Team))
(define P '(Paralyzed))
(define L '(Litter))
(define A '(Assault))

(define deck
  (cards
   ;;                  V  L  G  H  P  M 2 3 4 5 6 7 8 9 10 11 12
   [ 1 6 4  V  X #t 9 CC CC CC #f #f -4 1 1 1 1 1 1 1 1  1  1  1]
   [ 2 6 4  C  I #f 9 CP CP CC #f -4 -3 1 1 1 1 1 1 1 1  1  1  1]
   [ 3 5 4  R  G #f 9 CP CP CP #f -4 -2 1 1 1 1 1 1 1 1  1  1  1]
   [ 4 5 4 #f  F #f 9 CL CL CP #f -4 -1 1 1 1 1 1 1 1 1  1  1  1]
   [ 5 5 3 #f  X #t 9 CL CL CP #f -4 -1 1 1 1 1 1 1 1 1  1  1  2]
   [ 6 5 3  R  I #f 8 CF CL CL #f -4  0 1 1 1 1 1 1 1 1  2  2  2]
   [ 7 5 3  C  G #f 8 CF CF CL #f -4  0 1 1 1 1 1 1 1 2  2  2  2]
   [ 8 5 3 #f  F #f 8 CF CF CF #f -4  0 1 1 1 1 1 2 2 2  2  2  2]
   [ 9 5 3  R  X #f 8 PC PC CF #f -4  0 1 1 1 1 1 2 2 2  2  2  3]
   [10 5 3  V  I #t 8 PC PC PC #f -4  0 1 1 1 1 2 2 2 2  2  3  3]
   [11 4 3  V  G #f 7 PC PC PC -4 -3  1 1 1 1 2 2 2 2 2  3  3  3]
   [12 4 3  R  F #f 7 LC LC PC -4 -3  1 1 1 1 2 2 2 2 3  3  3  3]
   [13 4 3  V  X #f 7 LC LC PC -4 -3  1 1 1 1 2 2 2 2 3  3  3  4]
   [14 4 3 #f  I #f 7 LC LC PC -4 -3  1 1 1 2 2 2 2 3 3  3  4  4]
   [15 4 3  R  G #t 7 FC LC LC -4 -3  1 1 1 2 2 2 3 3 3  3  4  4]
   [16 4 3 #f  F #f 6 FC FC LC -4 -2  2 1 1 2 2 2 3 3 3  4  4  4]
   [17 4 2  C  X #f 6 FC FC LC -4 -2  2 1 2 2 2 2 3 3 4  4  4  5]
   [18 4 2  R  I #f 6 FC FC FC -4 -2  2 1 2 2 2 3 3 3 4  4  4  5]
   [19 4 2  V  G #f 6 PP PP FC -4 -2  2 1 2 2 2 3 3 3 4  4  5  5]
   [20 4 2 #f  F #t 6 PP PP FC -4 -2  2 1 2 2 2 3 3 4 4  4  5  5]
   [21 4 2  R  X #f 5 PP PP PP -4 -1  2 1 2 2 3 3 3 4 4  5  5  6]
   [22 4 2 #f  I #t 5 PL PL PP -4 -1  2 1 2 2 3 3 4 4 5  5  5  6]
   [23 4 2  V  G #f 5 PL PL PP -4 -1  3 1 2 2 3 3 4 4 5  5  6  6]
   [24 4 2  R  F #f 5 PL PL PP -4 -1  3 1 2 2 3 3 4 4 5  5  6  6]
   [25 3 2 #f  X #t 5 PF PF PP -4 -1  3 1 2 2 3 3 4 4 5  5  6  7]
   [26 3 2  C  I #f 4 PF PF PL -4  0  4 2 2 3 3 4 4 5 5  6  6  7]
   [27 3 2  R  G #f 4 PF PF PL -4  0  4 2 2 3 3 4 4 5 6  6  7  7]
   [28 3 2  V  F #f 4 PF PF PL -4  0  4 2 2 3 3 4 4 5 6  6  7  7]
   [29 3 2 #f  X #f 4  C  C PL -4  0  4 2 2 3 3 4 5 5 6  6  7  8]
   [30 3 2  R  I #f 4  C  C PF -4  0  4 2 2 3 3 4 5 5 6  6  7  8]
   [31 3 2  V  G #f 3  C  C PF -4  1  5 2 2 3 4 4 5 5 6  7  8  8]
   [32 3 2 #f  F #t 3  C  C PF -4  1  5 2 2 3 4 4 5 6 7  7  8  8]
   [33 3 2  R  X #f 3  C  C  C -4  1  5 2 2 3 4 5 5 6 7  7  8  9]
   [34 3 2  V  I #f 3  P  C  C -4  1  5 2 3 3 4 5 5 6 7  7  8  9]
   [35 3 1  V  G #t 3  P  P  C -4  1  5 2 3 3 4 5 5 6 7  7  9  9]
   [36 3 1  R  F #f 2  P  P  C -4  2  6 2 3 3 4 5 6 6 7  8  9  9]
   [37 2 1  V  X #f 2  P  P  C -4  2  6 2 3 3 4 5 6 6 7  8  9 10]
   [38 2 1 #f  I #t 2  P  P  C -4  2  6 2 3 4 4 5 6 7 8  8  9 10]
   [39 2 1  R  G #f 2  L  P  P -4  2  6 2 3 4 4 5 6 7 8  8  9 10]
   [40 2 1  C  F #f 2  L  P  P -4  2 #f 2 3 4 4 5 6 7 8  8  9 10]
   [41 2 1  C  X #f 1  L  L  P -4  3 #f 2 3 4 5 5 6 7 8  9 10 11]
   [42 2 1  R F3 #f 1  L  L  P -4  3 #f 2 3 4 5 6 6 7 8  9 10 11]
   [43 2 1  V  X #t 1  F  L  P -4  3 #f 2 3 4 5 6 7 7 8  9 10 11]
   [44 2 1 #f F3 #f 1  F  L  P -4  3 #f 2 3 4 5 6 7 7 9  9 10 11]
   [45 2 1  R  X #f 1  F  F  L -4  3 #f 2 3 4 5 6 7 8 9  9 10 11]
   [46 2 1 #f F3 #f 0  F  F  L -4  4 #f 2 3 4 5 6 7 8 9 10 11 12]
   [47 1 0  C  X #t 0  F  F  L -4  4 #f 2 3 4 5 6 7 8 9 10 11 12]
   [48 1 0 #f F3 #f 0  A  F  F -4  5 #f 2 3 4 5 6 7 8 9 10 11 12]
   [49 1 0 #f  J #f 0  A  A  F -4  6 #f 2 3 4 5 6 7 8 9 10 11 12]
   [50 1 0 #f FS #f 0  A  A  A -4 #f #f 2 3 4 5 6 7 8 9 10 11 12]
   [51 reshuffle]))

(module+ repl
  (define draw-pile (box #f))
  (define discard-pile (box #f))

  (define (reshuffle! why)
    (printf ";; Reshuffling (~a)\n" why)
    (set-box! draw-pile (shuffle deck))
    (set-box! discard-pile empty))
  (reshuffle! "Game Start")

  (define (raw-draw)
    (match (unbox draw-pile)
      ['()
       (reshuffle! "Empty Draw Pile")
       (raw-draw)]
      [(cons c new-draw)
       (set-box! draw-pile new-draw)
       (set-box! discard-pile (cons c (unbox discard-pile)))
       c]))

  (define (1draw)
    (match (raw-draw)
      [(card:reshuffle)
       (reshuffle! "Reshuffle Card")
       (1draw)]
      [c c]))

  (define (draw [n 1])
    (for/list ([i (in-range n)])
      (1draw)))

  (define (quit) (exit 0))

  (provide raw-draw
           reshuffle!
           1draw
           draw
           quit
           (rename-out [quit q])))


(module+ main
  (require racket/dict
           readline
           readline/readline)

  (define the-mod '(submod rtts/games/fields-of-fire/action-deck repl))
  (define (no-parens-read-syn src in)
    (define l (read-line in))
    (cond
      [(eof-object? l) l]
      [else
       (define new-in (open-input-string (format "(~a)" l)))
       (read-syntax src new-in)]))

  (define the-ns (make-base-namespace))
  (parameterize ([current-namespace the-ns]
                 [current-read-interaction no-parens-read-syn])
    (namespace-require the-mod)
    (define-values (vals stxs) (module->exports the-mod))
    (define available-commands
      (for/list ([v (in-list (dict-ref vals 0))])
        (symbol->string (first v))))

    (define (the-mod-complete s)
      (define sr (regexp (string-append "^" (regexp-quote s))))
      (filter (λ (f) (regexp-match sr f)) available-commands))

    (set-completion-function! the-mod-complete)

    (read-eval-print-loop)))
