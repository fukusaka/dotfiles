(setq tetris-buffer-width 70)
(setq tetris-buffer-height 30)
(setq tetris-width 20)
(setq tetris-height 25)
(setq tetris-top-left-x 25)
(setq tetris-top-left-y 2)


(setq tetris-next-x (+ tetris-top-left-x 3 tetris-width))
(setq tetris-next-y tetris-top-left-y)
(setq tetris-score-x tetris-next-x)
(setq tetris-score-y (+ tetris-next-y 6))

(defun tetris-my-update-speed-function (shapes rows) 0.5)

(setq tetris-update-speed-function
      'tetris-my-update-speed-function)

