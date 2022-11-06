#lang gdlisp

(extends Control)
(require threading
         "../macros.rkt")

(define (export Font) type-face)

(define onready lbl $Label)

(define (_ready)
  (doto
   lbl

   (.push-align RichTextLabel.ALIGN_CENTER)
   .push-bold
   (.add-text "Introduction")
   .pop
   .pop

   .newline
   .push-italics
   (.add-text "Compose your thoughts and functions. You'll be alright.\n")
   .pop

   .newline
   (.append-bbcode "Composure is a game about solving puzzles with functions built in \
point-free style: there are no [i]variables[/i], only combinators.\n")

   .newline
   (.add-text "This manual is designed as a reference for hints and tricks, or for terms you don't know.")

   .newline
   (.push-align RichTextLabel.ALIGN_CENTER)
   .push-bold
   (.add-text "\nTips")
   .pop
   .pop

   .newline
   (.append-bbcode "-\tYou might find pen and paper useful, or looking up things on the internet.\n")
   (.append-bbcode "-\tThe type system here is very similar to Haskell's, if you need to find tutorials on type systems.\n")
   (.append-bbcode "-\tFunctions are [i]curried[/i]: a binary function is just a function \
which returns another function. This means you can [i]partially apply[/i] it by only applying one argument.\n")
   (.append-bbcode "-\tIf you need to reuse an argument, you'll want to use the [i]S Combinator[/i], that's what it's for.\n")
   .newline
   (.append-bbcode "-\tIf you feel overwhelmed, take a break. Drink some water, take a breath. Try to \
think about other things. You don't need to be working yourself \
constantly.\n")
   (.append-bbcode "-\tYou don't need to be so hard on yourself. It's okay to be critical, \
but you are only human and you are allowed to make mistakes.\n")
   .newline
   (.append-bbcode "-\tThank you for playing, I genuinely wholeheartedly appreciate you for it.\n")

   .newline
   (.push-align RichTextLabel.ALIGN_CENTER)
   .push-bold
   (.add-text "Types")
   .pop
   .pop

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "a") .pop (.add-text ", ") (.push-font type-face) (.add-text "b") .pop
   (.add-text " or any other lowercase letters: type variables")
   (.append-bbcode "\n  -\tThese can represent [i]any[/i] type, much like variables do in algebra for numbers.")
   (.add-text "\n  -\tA type variable may have constraints, such as ")
   (.push-font type-face) (.add-text "Add") .pop
   (.add-text ", in which case the the type it represents must implement that constraint.")
   .newline

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "Num") .pop (.add-text ": a (real) number\n")
   (.add-text "\t-\tImplements: ")
   (.push-font type-face) (.add-text "Add") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Sub") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Mul") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Div") .pop
   .newline

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "Bool") .pop (.add-text ": either true or false")
   .newline

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "Vec2") .pop (.add-text " or ") (.push-font type-face) (.add-text "Vec3") .pop
   (.add-text ": a 2D or 3D vector, respectively\n")
   (.add-text "\t-\tImplement: ")
   (.push-font type-face) (.add-text "Add") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Sub") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Mul") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Div") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Vec") .pop
   .newline

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "a ƀ b") .pop (.add-text ": a function")
   (.add-text "\n  -\tThis is a function from any ")
   (.push-font type-face) (.add-text "a") .pop (.add-text " to any ")
   (.push-font type-face) (.add-text "b") .pop (.add-text ".")
   (.add-text "\n  -\tThis type means that, given an ") (.push-font type-face) (.add-text "a") .pop
   (.append-bbcode ", you can [i]apply[/i] the function to that ") (.push-font type-face) (.add-text "a") .pop
   (.add-text ", and it will return a ") (.push-font type-face) (.add-text "b") .pop
   (.add-text ".")
   (.append-bbcode "\n  -\tYou can combine functions with [i]combinators[/i], functions that take and return other functions.")
   (.append-bbcode "\n  -\tIndeed, this is the [i]only[/i] way to create new functions, so you'll have to get used to it.")
   (.add-text "\n  -\tYou'll usually see functions with concrete types rather than type variables.")
   .newline

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "a : b") .pop (.add-text ": a pair")
   (.add-text "\n  -\tThis is a pair of any ")
   (.push-font type-face) (.add-text "a") .pop (.add-text " and any ")
   (.push-font type-face) (.add-text "b") .pop (.add-text ".")
   (.add-text "\n  -\tNote that this is right-associative: ")
   (.push-font type-face) (.add-text "a : b : c") .pop (.add-text " is ")
   (.push-font type-face) (.add-text "a : (b : c)") .pop (.add-text ".")
   .newline

   .newline
   (.add-text "-\t") (.push-font type-face) (.add-text "a Ƃ b") .pop (.add-text ": an action\n")
   (.add-text "\t-\tThis is an action from any ")
   (.push-font type-face) (.add-text "a") .pop (.add-text " to any ")
   (.push-font type-face) (.add-text "b") .pop (.add-text ".\n")
   (.add-text "\t-\tThe difference between an action and a function is that an action can \
have side effects, such as moving.\n\t\tUnfortunately, they're not much \
use, since the only things you can do are move and interact with \
furniture, and I didn't expose most of the action combinators.")
   (.append-bbcode "\n  -\tFormally, you might think of these as arrows in the Kleisli category \
of the [code]IO[/code] monad.\n\t\tYou would, unfortunately, be slightly incorrect, since these arrows \
cannot be [code]join[/code]ed, but the spirit is there. \"Function with side effects\" is more catchy \
and easier to grasp than \"[code]IO[/code] monad\", though.")
   .newline

   ;;
   )
  null)
