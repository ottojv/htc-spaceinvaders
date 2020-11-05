(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ====================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; ====================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:

;; Game -> Game
;; start the world with (make-game '() '() (make-tank (/ WIDTH 2) 0))
;;
(define (main s)
  (big-bang s                    ; Game
    (on-tick   next-game)        ; Game -> Game
    (to-draw   render-game)      ; Game -> Image
    (stop-when touch-ground?)    ; Game -> Boolean
    (on-key    tank-direction))) ; Game KeyEvent -> Game

;; --------------------
;; Game -> Game
;; produce the next game state
;;  - move tank
;;  - move missile
;;  - move invaders
;;  - periodically generate more invaders

; (define (next-game s) s) ; stub

(define (next-game s)
  (make-game (destroy-invaders (game-missiles s) (generate-invader (move-invaders (game-invaders s))))
             (explode-missiles (game-invaders s) (move-missiles (game-missiles s)))
             (move-tank (game-tank s))))

;; --------------------
;; Game -> Image
;; render the images of all invaders, missiles and the tank
(check-expect (render-game (make-game '() '() T0))
              (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-game (make-game (list I1) '() T1))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image TANK
                                        (tank-x T1)
                                        (- HEIGHT TANK-HEIGHT/2)
                                        BACKGROUND)))
(check-expect (render-game (make-game '() (list M1) T1))
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           (place-image TANK
                                        (tank-x T1)
                                        (- HEIGHT TANK-HEIGHT/2)
                                        BACKGROUND)))
(check-expect (render-game (make-game (list I1) (list M2) T1))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image MISSILE
                                        (missile-x M2)
                                        (missile-y M2)
                                        (place-image TANK
                                                     (tank-x T1)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
(check-expect (render-game (make-game (list I1 I2) (list M1 M2) T2))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image INVADER
                                        (invader-x I2)
                                        (invader-y I2)
                                        (place-image MISSILE
                                                     (missile-x M1)
                                                     (missile-y M1)
                                                     (place-image MISSILE
                                                                  (missile-x M2)
                                                                  (missile-y M2)
                                                                  (place-image TANK
                                                                               (tank-x T1)
                                                                               (- HEIGHT TANK-HEIGHT/2)
                                                                               BACKGROUND))))))

; (define (render-game s) BACKGROUND) ; stub

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))

;; --------------------
;; Game -> Boolean
;; returns true if there is an invader touching the ground
(check-expect (touch-ground? (make-game '() '() T0)) #false)
(check-expect (touch-ground? (make-game (list (make-invader 190 93 3)
                                              (make-invader 83 329 -5))
                                        '()
                                        T0))
              #false)
(check-expect (touch-ground? (make-game (list (make-invader 190 93 3)
                                              (make-invader 190 (- HEIGHT (image-height INVADER)) 3))
                                        '()
                                        T0))
              #true)

; (define (touch-ground? s) #false) ; stub

(define (touch-ground? s)
  (cond [(empty? (game-invaders s))
         #false]
        [else (if (>= (invader-y (first (game-invaders s)))
                      (- HEIGHT (image-height INVADER)))
                  #true
                  (touch-ground? (make-game (rest (game-invaders s))
                                            (game-missiles s)
                                            (game-tank s))))]))

;; --------------------
;; Game KeyEvent -> Game
;; make the tank move to the right if right-arrow is pressed or
;; to the left if left-arrow is pressed or
;; fire a missile if spacebar is pressed
(check-expect (tank-direction (make-game (list I1) (list M2) T1) "s")
              (make-game (list I1) (list M2) T1))
(check-expect (tank-direction (make-game (list I2) (list M1) T0) "n")
              (make-game (list I2) (list M1) T0))
(check-expect (tank-direction (make-game (list I1 I2) (list M2 M1) T1) "left")
              (make-game (list I1 I2)
                         (list M2 M1)
                         (make-tank (tank-x T1)
                                    -1)))
(check-expect (tank-direction (make-game (list I2 I1) (list M2 M1) T0) "right")
              (make-game (list I2 I1)
                         (list M2 M1)
                         (make-tank (tank-x T0)
                                    1)))
(check-expect (tank-direction (make-game (list I2 I1) (list M2 M1) T0) " ")
              (make-game (list I2 I1)
                         (cons (make-missile (tank-x T0)
                                             (- HEIGHT TANK-HEIGHT/2))
                               (list M2 M1))
                         T0))

; (define (tank-direction s ke) s) ; stub

(define (tank-direction s ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s))
                               -1))]
        [(key=? ke "right")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s))
                               1))]
        [else (if (key=? ke " ")
                  (make-game (game-invaders s)
                             (cons (make-missile (tank-x (game-tank s))
                                                 (- HEIGHT TANK-HEIGHT/2))
                                   (game-missiles s))
                             (game-tank s))
                  s)]))


;; --------------------
;; ListOfMissile ListOfInvader -> ListOfInvader
;; destroy all invaders that are within HIT-RANGE of a missile
(check-expect (destroy-invaders '() '()) '())
(check-expect (destroy-invaders (list M1) '()) '())
(check-expect (destroy-invaders '() (list I1)) (list I1))
(check-expect (destroy-invaders (list (make-missile 93 187)) (list (make-invader 82 198 10)))
              (list (make-invader 82 198 10)))
(check-expect (destroy-invaders (list (make-missile 92 187)) (list (make-invader 82 197 10)))
              '())
(check-expect (destroy-invaders (list (make-missile 93 187) (make-missile 30 30))
                                (list (make-invader 92 199 10) (make-invader 60 80 -10)))
              (list (make-invader 92 199 10) (make-invader 60 80 -10)))
(check-expect (destroy-invaders (list (make-missile 92 187) (make-missile 55 76))
                                (list (make-invader 82 197 10) (make-invader 60 80 -10)))
              '())


; (define (destroy-invaders lom loi) loi) ; stub

(define (destroy-invaders lom loi)
  (cond [(or (empty? loi)
             (empty? lom))
         loi]
        [else (if (invader-hit? lom (first loi))
                  (destroy-invaders lom (rest loi))
                  (cons (first loi)
                        (destroy-invaders lom (rest loi))))]))

;; --------------------
;; ListOfInvader -> ListOfInvader
;; randomly generate a new invader at a rate of INVADE-RATE

;(define (generate-invader loi) loi) ; stub

(define (generate-invader loi)
  (if (<= (random (* INVADE-RATE 75)) INVADE-RATE)
      (cons (make-invader
             (random WIDTH)
             (image-height INVADER)
             (random 10))
            loi)
      loi))

;; --------------------
;; ListOfInvader -> ListOfInvader
;; advances the invaders positions by:
;;  - (* (invader-dx i) INVADER-X-SPEED)) pixels horizontally (bounce if hit the edge of the screen)and
;;  - INVADER-Y-SPEED pixels vertically
(check-expect (move-invaders '()) '())
(check-expect (move-invaders (list I1))
              (list (make-invader
                     (+ (invader-x (first (list I1))) (* (invader-dx (first (list I1))) INVADER-X-SPEED))
                     (+ (invader-y (first (list I1))) INVADER-Y-SPEED)
                     (invader-dx (first (list I1))))))
(check-expect (move-invaders (list (make-invader 290 300 10)))
              (list (make-invader (- WIDTH (image-width INVADER)) (+ 300 INVADER-Y-SPEED) -10)))

; (define (move-invaders loi) loi) ; stub

(define (move-invaders loi)
  (cond [(empty? loi) loi]
        [else (cons
               (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; --------------------
;; ListOfMissile -> ListOfMissile
;; explodes all missiles that come within HIT-RANGE of an invader
(check-expect (explode-missiles '() '()) '())
(check-expect (explode-missiles '() (list M1)) (list M1))
(check-expect (explode-missiles (list I1) '()) '())
(check-expect (explode-missiles (list (make-invader 82 198 10)) (list (make-missile 93 187)))
              (list (make-missile 93 187)))
(check-expect (explode-missiles (list (make-invader 82 197 10)) (list (make-missile 92 187)))
              '())
(check-expect (explode-missiles (list (make-invader 92 199 10) (make-invader 60 80 -10))
                                (list (make-missile 93 187) (make-missile 30 30)))
              (list (make-missile 93 187) (make-missile 30 30)))
(check-expect (explode-missiles (list (make-invader 82 197 10) (make-invader 60 80 -10))
                                (list (make-missile 92 187) (make-missile 55 76)))
              '())

; (define (explode-missiles loi lom) lom) ; stub

(define (explode-missiles loi lom)
  (cond [(or (empty? lom)
             (empty? loi))
         lom]
        [else (if (missile-hit? loi (first lom))
                  (explode-missiles loi (rest lom))
                  (cons (first lom)
                        (explode-missiles loi (rest lom))))]))

;; --------------------
;; ListOfMissile -> ListOfMissile
;; moves all missiles by MISSILE-SPEED pixels upwards
;; removes the missiles outside the edge of the screen
(check-expect (move-missiles (list M1))
              (list (make-missile (missile-x (first (list M1)))
                                  (- (missile-y (first (list M1))) MISSILE-SPEED))))
(check-expect (move-missiles (list (make-missile 50 (- (image-height MISSILE)))))
              '())

; (define (move-missiles lom) lom) ; stub

(define (move-missiles lom)
  (cond [(empty? lom) lom]
        [else (if (< (missile-y (move-missile (first lom)))
                     (- (image-height MISSILE)))
                  (move-missiles (rest lom))
                  (cons (move-missile (first lom))
                        (move-missiles (rest lom))))]))

;; --------------------
;; Tank -> Tank
;; moves the tank by (* (tank-dir t) TANK-SPEED) pixels horizontally
;; stop the tank if it hits the edge of the screens
(check-expect (move-tank T0)
              (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))
(check-expect (move-tank (make-tank 39 -1))
              (make-tank (- 39 TANK-SPEED) -1))
(check-expect (move-tank (make-tank (image-width TANK) -1))
              (make-tank (image-width TANK) 0))
(check-expect (move-tank (make-tank (- WIDTH (image-width TANK)) 1))
              (make-tank (- WIDTH (image-width TANK)) 0))


; (define (move-tank t) t) ; stub

(define (move-tank t)
  (cond [(= (tank-dir t) -1)
         (if (< (- (tank-x t) TANK-SPEED) (image-width TANK))
             (make-tank (image-width TANK) 0)
             (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))]
        [(= (tank-dir t) 1)
         (if (> (+ (tank-x t) TANK-SPEED) (- WIDTH (image-width TANK)))
             (make-tank (- WIDTH (image-width TANK)) 0)
             (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))]
        [else t]))

;; --------------------
;; ListOfMissile Invader -> Boolean
;; returns true if an invader has been hit, else return false
(check-expect (invader-hit? '() I1) #false)
(check-expect (invader-hit? (list (make-missile 30 50))
                            (make-invader 69 183 -3))
              #false)
(check-expect (invader-hit? (list (make-missile 30 60))
                            (make-invader 40 89 4))
              #false)
(check-expect (invader-hit? (list (make-missile 30 60))
                            (make-invader 94 55 -2))
              #false)
(check-expect (invader-hit? (list (make-missile 30 60))
                            (make-invader 36 53 9))
              #true)

; (define (invader-hit? lom i) #false) ; stub

(define (invader-hit? lom i)
  (cond [(empty? lom) #false]
        [else (if (hit-range? (first lom) i)
                  #true
                  (invader-hit? (rest lom) i))]))

;; --------------------
;; Missile Invader -> Boolean
;; returns true if Missile is within HIT-RANGE distance of invader
(check-expect (hit-range? (make-missile 93 187) (make-invader 82 198 10))
              #false)
(check-expect (hit-range? (make-missile 92 187) (make-invader 82 197 10))
              #true)

; (define (hit-range? m i) #false) ; stub

(define (hit-range? m i)
  (and (<= (abs (- (missile-x m) (invader-x i))) HIT-RANGE)
       (<= (abs (- (missile-y m) (invader-y i))) HIT-RANGE)))

;; --------------------
;; Invader -> Invader
;; move the invader by:
;;  - (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) pixels horizontally and
;;  - (+ (invader-y i) INVADER-Y-SPEED) pixels vertically
(check-expect (move-invader (make-invader 100 300 10))
              (make-invader (+ 100 (* 10 INVADER-X-SPEED)) (+ 300 INVADER-Y-SPEED) 10))
(check-expect (move-invader (make-invader 295 300 10))
              (make-invader (- WIDTH (image-width INVADER)) (+ 300 INVADER-Y-SPEED) (- 10)))
(check-expect (move-invader (make-invader 10 300 -10))
              (make-invader (image-width INVADER)
                            (+ 300 INVADER-Y-SPEED)
                            10))
(check-expect (move-invader (make-invader 150 160 -5))
              (make-invader (+ 150 (* -5 INVADER-X-SPEED)) (+ 160 INVADER-Y-SPEED) -5))

; (define (move-invader i) i) ; stub

(define (move-invader i)
  (cond [(< (invader-dx i) 0)
         (if (<= (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                 (image-width INVADER))
             (make-invader (image-width INVADER)
                           (+ (invader-y i) INVADER-Y-SPEED)
                           (- (invader-dx i)))
             (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                           (+ (invader-y i) INVADER-Y-SPEED)
                           (invader-dx i)))]
        [(>= (invader-dx i) 0)
         (if (>= (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                 (- WIDTH (image-width INVADER)))
             (make-invader (- WIDTH (image-width INVADER))
                           (+ (invader-y i) INVADER-Y-SPEED)
                           (- (invader-dx i)))
             (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                           (+ (invader-y i) INVADER-Y-SPEED)
                           (invader-dx i)))]))

;; --------------------
;; Missile -> Missile
;; moves the missile upwards by MISSILE-SPEED pixels
(check-expect (move-missile M1)
              (make-missile (missile-x (first (list M1)))
                            (- (missile-y (first (list M1))) MISSILE-SPEED)))
(check-expect (move-missile (make-missile 50 (- (image-height MISSILE))))
              (make-missile 50 (- (- (image-height MISSILE)) MISSILE-SPEED)))

; (define (move-missile m) m) ; stub

(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))

;; --------------------
;; ListOfInvader Missile -> Boolean
;; returns true if an invader has been hit, else return false
(check-expect (missile-hit? '() M1) #false)
(check-expect (missile-hit? (list (make-invader 69 183 -3))
                            (make-missile 30 50))
              #false)
(check-expect (missile-hit? (list (make-invader 40 89 2))
                            (make-missile 30 60))
              #false)
(check-expect (missile-hit? (list (make-invader 94 55 6))
                            (make-missile 30 60))
              #false)
(check-expect (missile-hit? (list (make-invader 36 53 -2))
                            (make-missile 30 60))
              #true)

; (define (missile-hit? loi m) #false) ; stub

(define (missile-hit? loi m)
  (cond [(empty? loi) #false]
        [else (if (hit-range? m (first loi))
                  #true
                  (missile-hit? (rest loi) m))]))

;; --------------------
;; ListOfInvader Image -> Image
;; create the image of all invaders in the list
(check-expect (render-invaders '()
                               (place-image TANK
                                            5
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
              (place-image TANK
                           5
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (render-invaders (list I1)
                               (place-image TANK
                                            5
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image TANK
                                        5
                                        (- HEIGHT TANK-HEIGHT/2)
                                        BACKGROUND)))
(check-expect (render-invaders (list I1 I2)
                               (place-image TANK
                                            5
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
              (place-image INVADER
                           (invader-x I2)
                           (invader-y I2)
                           (place-image INVADER
                                        (invader-x I1)
                                        (invader-y I1)
                                        (place-image TANK
                                                     5
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))

; (define (render-invaders loi) BACKGROUND) ; stub

(define (render-invaders loi i)
  (cond [(empty? loi) i]
        [else (place-image INVADER
                           (invader-x (first loi))
                           (invader-y (first loi))
                           (render-invaders (rest loi) i))]))

;; --------------------
;; Missile Image -> Image
;; create the image of all missiles in the Game
(check-expect (render-missiles '()
                               (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
              (place-image TANK
                           (tank-x T0)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (render-missiles (list M1)
                               (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           (place-image TANK
                                        (tank-x T0)
                                        (- HEIGHT TANK-HEIGHT/2)
                                        BACKGROUND)))
(check-expect (render-missiles (list M1 M2)
                               (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           (place-image MISSILE
                                        (missile-x M2)
                                        (missile-y M2)
                                        (place-image TANK
                                                     (tank-x T0)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))

; (define (render-missiles lom i) BACKGROUND) ; stub

(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else (place-image MISSILE
                           (missile-x (first lom))
                           (missile-y (first lom))
                           (render-missiles (rest lom) i))]))

;; --------------------
;; Tank -> Image
;; create the image of a tank
(check-expect (render-tank T0)
              (place-image TANK
                           (tank-x T0)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

; (define (render-tank t) BACKGROUND) ; stub

(define (render-tank t)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))
