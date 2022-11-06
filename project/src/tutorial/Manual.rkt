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
   (.add-text "\nTricks")
   .pop
   .pop

   .newline
   (.append-bbcode "- You might find pen and paper useful, or looking up things on the internet.\n")
   (.append-bbcode "- Functions are [i]curried[/i]: a binary function is just a function \
which returns another function. This means you can [i]partially apply[/i] it by only applying one argument.\n")
   (.append-bbcode "- If you need to reuse an argument, you'll want to use the [b]S Combinator[/b], that's what it's for.\n")

   .newline
   (.push-align RichTextLabel.ALIGN_CENTER)
   .push-bold
   (.add-text "\nTypes")
   .pop
   .pop

   .newline
   (.add-text "- ") (.push-font type-face) (.add-text "Num") .pop (.add-text ": a (real) number\n")
   (.add-text "  - Implements: ")
   (.push-font type-face) (.add-text "Add") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Sub") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Mul") .pop (.add-text ", ")
   (.push-font type-face) (.add-text "Div") .pop
   (.add-text "\n")

   ;;
   )
  null)
