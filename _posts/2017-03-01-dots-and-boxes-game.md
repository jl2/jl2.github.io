---
layout: post
published: false
---
This is a short walk through showing how I implemented a [Dots and Boxes](https://en.wikipedia.org/wiki/Dots_and_Boxes) game in Common Lisp.

Dots and Boxes is a two player game using a square grid of dots.  The players take turns connecting the dots using horizontal and vertical lines.  When a player connects the 4th edge of a 1x1 square they earn a point and take another turn.  The game ends when there are no more horizontal or vertical edges to connect.  The winner is the player with the most points.

First, lets flush out a few more details.  In this implementation, a single player will play against the computer.  The human player will go first by clicking the mouse between two dots.  As the player moves the mouse around the screen to potential positions a highlighed blue line will appear, and when they click in a valid position the line will turn green and stay there.  The score will be updated, and if the player has scored a point then it will remain their turn, and if not it will become the computer's turn.  The computer will determine the best line to place and do so.  Again, the score will be updated, and if the computer has scored any points it will take another turn.  When the player completes a box it will turn green, and when the computer fills a box it will turn red.


Now it's time to create a new project using Quickproject:

``` common-lisp

(ql:quickload :quickproject)
(quickproject:make-project "~/src/lisp/dots-and-boxes/" :depends-on '(:qtools :qtgui :qtcore))
(ql:quickload :dots-and-boxes)
```

Next I cheated a bit and copy/pasted a minimal Qt application into my dots-and-boxes.lisp file:

``` common-lisp
;;;; dots-and-boxes.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:dots-and-boxes)

(named-readtables:in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ())

(define-override (main-window close-event) (ev)
  (q+:accept ev))


(define-menu (main-window Game)
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Dots and Boxes.")))


(define-widget dab-drawer (QWidget)
  ()
  (:documentation "Dots and boxes ."))

(define-override (dab-drawer paint-event paint) (ev)
  "Handle paint events."

  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter dab-drawer))
       (pen (q+:make-qpen )))

    ;; Clear the background
    (q+:fill-rect painter (q+:rect dab-drawer) (q+:qt.black))
    (q+:set-color pen (q+:make-qcolor 0 205 0))
    (q+:set-pen painter pen)
    (let* ((height (q+:height dab-drawer))
           (width (q+:width dab-drawer)))
      (q+:draw-arc painter
                   (round (- (/ width 2) 10)) (round (- (/ height 2) 10))
                   20 20
                   0 (* 16 360))
      (q+:draw-line painter 0 0 width height))))


(define-subwidget (main-window dab-widget) (make-instance 'dab-drawer)
  "The dab-drawer itself.")

(define-initializer (main-window setup)
  "Set the window title and set the dab-widget to be the central widget."
  (setf (q+:window-title main-window) "Dots And Boxes")
  (setf (q+:central-widget main-window) dab-widget))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))
```

And then I exported the main function:

``` common-lisp
(defpackage #:dots-and-boxes
  (:use #:cl+qt)
  (:export #:main))
```



The first thing to do is come up with a data structure for representing the game state.  Coming up with a good data structure can be tricky, but one good way to do it is to to think about it helps to think about how the structure will be used as the game is played and what operations will be required.  For Dots and Boxes 

Development will be split up into roughly three parts.  First, I'll create the underlying data structures that the game will use and implement some low level functions for working with the data and manipulating the game state.  Next, I'll create a GUI for the user to interact with the game.  Finally, I'll implement the computer's game play.

The first thing to do is come up with a data structure that represents the state of the game as it's being played.  For Dots and Boxes the game consists of a square pattern of dots
