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

(define (_ready)
  (.hide textbox)
  (.hide next))

(define (start-show-text)
  (.show textbox)
  (.clear textbox)
  textbox)

(define onready timer $Timer)
(define (_on_Timer_timeout)
  (set! skipping? false))

(define skipping? false)
(define (continue-show-text confirm?)
  (.get-content-height textbox) ;; fun fact! this "const" function has the side effect of recomputing character counts
  (set! textbox.visible-characters 1)
  (.start timer)
  (set! skipping? false)
  (for ([c (.-text textbox)])
    (when (and (.is-stopped timer)
               skipping?)
      (#%gdscript "break"))
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

(define (set-tab i)
  (~> Game.ui .mount-gui .get-tabs (.set-current-tab i)))

(define (_on_Choice_pressed i)
  (emit-signal "chosen" i)
  (for ([child (.get-children choices)])
    (.queue-free child))
  (.hide choices)
  null)

(define (present-choice choice-a)
  (for ([i (range (len choice-a))])
    (define ch (.new Button))
    (.set ch "mouse_default_cursor_shape" CURSOR_POINTING_HAND)
    (.set-text ch (ref choice-a i))
    (.add-child choices ch)
    (.connect ch "pressed" self "_on_Choice_pressed" [i]))
  (.show choices)
  (yield self "chosen"))

(define (_gui-input evt)
  (when (Input.is-action-just-pressed "ui_accept")
    (emit-signal "pressed")
    (set! skipping? true))
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

(define (start include-dysfunction)
  (release-mouse)
  (call-deferred "grab_focus")

  (when include-dysfunction
    (with-mouse-grabbed
      (yield-for 3)
      (await (show-text "You seem to be struggling to leave your room...\nIs everything alri-" true))
      (await (show-text "Ah, of course you haven't [b]defined[/b] how to move yet." true))))

  (when (not (Game.ui.is-gui-showing))
    (with-mouse-grabbed
      (await (show-text "Open up the [b]Composer[/b] with the button in the bottom right corner." false)))
    (yield (Game.ui.get-composer-button) "pressed")
    (call-deferred "grab_focus")) ;; grab it again!

  (with-mouse-grabbed
    (await (show-text "You'll see a few tabs, we'll go through the [b]Combinator[/b] and \
[b]Keys[/b] tabs in more depth in a moment." true))
    (set-tab 2)
    (await (show-text "The [b]Manual[/b] tab contains, as its name suggests, the \
manual. Read it if you get stuck or don't understand something." true))
    (set-tab 3)
    (await (show-text "The [b]Admin[/b] tab contains things that would require fourth-wall \
breaking to explain, such as settings. I don't really understand what \
that means, but I'm sure you do." true))

    (await (show-text "Okay, now onto the [b]Combinator[/b]." false))
    (release-mouse)
    (await (present-choice ["Let's go!"]))
    (grab-mouse-rudely)

    (let loop ()
      (set-tab 0)
      (await (show-text "The [b]Combinator[/b] lets you [i]combine[/i] values (mostly functions) to create new ones." true))
      (await (show-text (+ "The squares in the middle (called the [b]Applicator[/b]) let you apply functions. "
                           "[b]The function comes first, followed by any arguments.[/b]")
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
      (set-tab 1)
      (await (show-text "Now for the [b]Keys[/b]." true))
      (await (show-text "Press the button, and issue an input, preferrably a mouse click, touch, or joystick motion."
                        false)))
    (yield (~> Game.ui .get-gui .get-tabs (.get-node "Keys")) "key_setup"))

  (with-mouse-grabbed
    (let loop ()
      (await (show-text "You'll see a slot appear with some text below it. You can drag an [b]action[/b] \
into this slot to [b]bind[/b] it, so when you enact the input it'll run the action."
                        true))
      (await (show-text "The slots on the right persist across tabs, if you need to move a \
value from the [b]Combinator[/b] to [b]Keys[/b]."
                        true))
      (await (show-text "The text below the slot in the middle, the one with a squiggly (technical term) arrow, \
is the [b]type[/b] required by the slot, it won't let you put anything else into that slot."
                        true))
      (await (show-text "The [b]type[/b] of a value tells you what it is: a number, a vector, \
a function from one to the other, or something else."
                        true))
      (await (show-text "If you hover over a value it will show you its type on the tooltip, \
so if a slot isn't letting you put a value in: [i]check the types[/i]!"
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
      (append-bb " Read the manual for more information on these.")
      (await (continue-show-text true))

      (await (show-text "If you haven't completely understood (and want to), you can always read the manual." true))
      (await (show-text "Have you got that?" false))
      (release-mouse)
      (match (await (present-choice ["Got it!" "Come again?" "I'll read the funny manual"]))
        [(or 0 2) null]
        [1
         (grab-mouse-rudely)
         (await (show-text "Right, types..." true))
         (recur loop)])))

  (with-mouse-grabbed
    (await (show-text "Right, you wanted to [i]move[/i], didn't you?" true))
    (await (show-text "To do that nicely, you'll need to project a ray from the camera \
through the mouse, intersect it with the XZ plane (or fall back to \
some far away point), subtract the player's position, project to two \
dimensions, then compose the action into the [i]move[/i] action."
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
    (append-bb ", then [i]bind[/i] it to a mouse, touch, or joystick input.")
    (await (continue-show-text false))
    (release-mouse)
    (await (present-choice ["Done!"]))
    (grab-mouse-rudely)

    (await (show-text "You'll also want to bind the [b]Interact[/b] action to something, \
so you can interact with that closet over there. I believe it has a [b]puzzle[/b] for you."
                      false))
    (release-mouse)
    (await (present-choice ["Done!"]))
    (grab-mouse-rudely)

    (await (show-text "If you ever want to replay the tutorial, find it in the [b]Admin[/b] tab."
                      true)))

  (.unmount (get-parent)) ;; bye bye
  (set! Game.tutorial-done true)
  (Game.dirty)
  null)

(define (start-reloaded)
  (release-mouse)
  (call-deferred "grab_focus")

  (with-mouse-grabbed
    (await (show-text "Hey, you're back!" true))
    (await (show-text "You got a bit lost, I thought, so I dragged you back to your room." true))
    (await (show-text "Most of the puzzles got reset, and the world may look a bit \
different, I hope you don't mind!" true))
    (await (show-text "All your functions and values should be intact, though!" true)))

  (.unmount (get-parent)) ;; bye bye
  null)

(define (append-bb bb)
  (.append-bbcode textbox bb))

(define (append-text txt)
  (.add-text textbox txt))

(define (append-type ty)
  (.push-font textbox type-face) (.append-bbcode textbox ty) (.pop textbox))
