#lang gdlisp

(extends Control)

(require threading)

(define (export Font) type-face) ;; haha

(signal pressed)

(signal chosen #;index)

(define (time-for c)
  ;; punctuation breathing times with Eutro
  (match c
    ["\n" 0.4]
    [(or "." "!") 0.2]
    [";" 0.15]
    ["," 0.1]
    [_ 0.02]))

(define onready textbox $Text)
(define onready next $Text/Panel/Next)
(define onready choices (get-node "%Choices"))

(define (start-show-text)
  (.show textbox)
  (.clear textbox)
  textbox)

(define (continue-show-text confirm?)
  (.get-content-height textbox) ;; fun fact! this "const" function has the side effect of recomputing character counts
  (set! textbox.visible-characters 1)
  (for ([c (.-text textbox)])
    (+set! textbox.visible-characters 1)
    (yield-for (time-for c)))
  (set! textbox.percent-visible 1)
  (when confirm?
    (.show next)
    (yield self "pressed")
    (.hide next)))

(define (show-text txt confirm?)
  (.append-bbcode (start-show-text) txt)
  (continue-show-text confirm?))

(define (_on_Choice_pressed i)
  (emit-signal "chosen" i)
  (for ([child (.get-children choices)])
    (.queue-free child))
  (.hide choices)
  null)

(define (present-choice choice-a)
  (for ([i (range (len choice-a))])
    (define ch (.new Button))
    (.set-text ch (ref choice-a i))
    (.add-child choices ch)
    (.connect ch "pressed" self "_on_Choice_pressed" [i]))
  (.show choices)
  (yield self "chosen"))

(define (_gui-input evt)
  (when (Input.is-action-just-pressed "ui_accept")
    (emit-signal "pressed"))
  (accept-event)
  null)

(define (grab-mouse-rudely) ;; so very rude
  (set "mouse_filter" MOUSE_FILTER_STOP))

(define (release-mouse)
  (set "mouse_filter" MOUSE_FILTER_IGNORE))

(begin-escape
  (require (only-in racket/base ...))
  (define-syntax-rule (with-mouse-grabbed body ...)
    (begin
      (grab-mouse-rudely)
      body ...
      (release-mouse))))

(define (start)
  (.hide textbox)
  (.hide next)

  (with-mouse-grabbed
    (call-deferred "grab_focus")
    (yield-for 3)
    (await (show-text "You seem to be struggling to leave your room...\nIs everything alri-" true))
    (await (show-text "Ah, of course you haven't [b]defined[/b] how to move yet." true))
    (await (show-text "Open up the [b]Composer[/b] with the button in the bottom right corner." false)))

  (yield (Game.ui.get-composer-button) "pressed")

  (with-mouse-grabbed
    (await (show-text "You'll see two tabs, [b]Combinator[/b] and [b]Keys[/b]. Let's start with the [b]Combinator[/b]." true))
    (let loop ()
      (await (show-text "The [b]Combinator[/b] lets you [i]combine[/i] values (mostly functions) to create new ones." true))
      (await (show-text (+ "The squares in the middle (called the [b]Applicator[/b]) let you apply functions. "
                           "The function comes first, followed by any arguments.")
                        true))
      (await (show-text (+ "Below that is the [b]Constant Generator[/b]. "
                           "This lets you generate some constant integers to use in calculations.")
                        true))
      (await (show-text (+ "On the left you'll find the [b]Value List[/b], a list of values and functions. "
                           "You can even add your own and name them.")
                        true))
      (await (show-text "You can move values between slots by dragging them. Holding [b]shift[/b] will duplicate values you move."
                        true))
      (await (show-text (+ "You can also use the keyboard (space, enter, delete) or "
                           "controller (figure it out for yours) buttons to select, drop, or delete values in slots.")
                        true))
      (await (show-text (+ "If you got all of that, it's time to apply some functions. "
                           "Try adding two numbers together, or something similar, to warm up.")
                        false))
      (release-mouse)
      (match (await (present-choice ["Ok, done!" "Uhm... Again?"]))
        [0 null]
        [1
         (grab-mouse-rudely)
         (await (show-text "Okay, listen carefully." true))
         (recur loop)])))

  (let loop ()
    (with-mouse-grabbed
      (await (show-text "Now for the [b]Keys[/b]." true))
      (await (show-text (+ "Select the tab, press the button, and issue an input, "
                           "preferrably a mouse click, touch, or joystick motion...")
                        false)))
    (match (await (present-choice ["Ok, done!" "Uhm... Again?"]))
      [0 null]
      [1 (recur loop)]))

  (with-mouse-grabbed
    (let loop ()
      (await (show-text (+ "You'll have seen a slot appear with some text below it. You can drag an [b]action[/b] "
                           "into this slot to [b]bind[/b] it, so when you enact the input it'll run the action.")
                        true))
      (await (show-text (+ "The text below it, the one with a squiggly (technical term) arrow, "
                           "is the [b]type[/b] required by the slot, it won't "
                           "let you put anything else into the slot.")
                        true))
      (await (show-text (+ "The [b]type[/b] of a value tells you what it is: a number, a vector, "
                           "a function from one to the other, or something else.")
                        true))
      (await (show-text (+ "If you hover over a value it will show you its type on the tooltip, "
                           "so if a slot isn't letting you put a value in: [i]check the types[/i]!")
                        true))

      (start-show-text)
      (append-bb "Some common types are: ")
      (append-type "Num")
      (append-bb " (a number), ")
      (append-type "Vec2")
      (append-bb " (a 2D vector), ")
      (append-type "Vec3")
      (append-bb " (a 3D vector), ")
      (append-type "a ƀ b")
      (append-bb " (a [i]function[/i] from ")
      (append-type "a") (append-bb " to ") (append-type "b") (append-bb ", for any ")
      (append-type "a") (append-bb " and ") (append-type "b") (append-bb "), and ")
      (append-type "a Ƃ b")
      (append-bb " (an [i]action[/i] from ")
      (append-type "a") (append-bb " to ") (append-type "b") (append-bb ", for any ")
      (append-type "a") (append-bb " and ") (append-type "b") (append-bb ").")
      (await (continue-show-text true))

      (await (show-text "If you haven't quite got it yet, you can read the manual later." true))
      (await (show-text "Have you got that?" false))
      (release-mouse)
      (match (await (present-choice ["Got it!" "Come again?" "I'll read the funny manual"]))
        [(or 0 2) null]
        [1
         (grab-mouse-rudely)
         (await (show-text "Right, types..." true))
         (recur loop)])))

  (with-mouse-grabbed
    (await (show-text "[b]Right,[/b] you wanted to [i]move[/i], didn't you?" true))
    (await (show-text (+ "To do that nicely, you'll need to project a ray from the camera through the mouse, "
                         "intersect it with the XZ plane (or fall back to some far away point), "
                         "subtract the player's position, project to two dimensions, then compose "
                         "the action into the [i]move[/i] action.")
                      true))

    (await (show-text "If you're feeling up to it, great! If not, I've helpfully done the first part for you."
                      true))
    (start-show-text)
    (append-bb "All [i]you[/i] need to do is apply [b]Compose Actions[/b] ")
    (append-text "[") (append-bb "[code]>>>[/code]") (append-text "]")
    (append-bb " to [b]PSCtXZPRtP[/b] ")
    (append-text "[") (append-bb "[code]:)[/code]") (append-text "]")
    (append-bb " and [b]Move[/b] ")
    (append-text "[") (append-bb "[code]mv[/code]") (append-text "]")
    (append-bb ", then [b]bind[/b] it to a mouse, touch, or joystick input.")
    (await (continue-show-text false))
    (release-mouse)
    (await (present-choice ["Done!"]))
    (grab-mouse-rudely)

    (await (show-text (+ "You'll also want to bind the [b]Interact[/b] action to something, "
                         "and interact with that closet over there. I believe it has a [b]puzzle[/b] for you.")
                      true)))

  (.unmount (get-parent)) ;; bye bye
  null)

(define (append-bb bb)
  (.append-bbcode textbox bb))

(define (append-text txt)
  (.add-text textbox txt))

(define (append-type ty)
  (.push-font textbox type-face) (.append-bbcode textbox ty) (.pop textbox))
