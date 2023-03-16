

alias Html msg =
    VirtualDom.VirtualNode msg


alias Attr msg =
    VirtualDom.Attr msg


map as fn (fn a: b), Html a: Html b =
    VirtualDom.map


#
# Elements
#

none as Html msg =
    VirtualDom.TextNode ""

a as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "a" __ __

button as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "button" __ __

canvas as fn [Attr msg]: Html msg =
    VirtualDom.ElementNode "canvas" __ []

code as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "code" __ __

div as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "div" __ __

h1 as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "h1" __ __

img as fn [Attr msg]: Html msg =
    VirtualDom.ElementNode "img" __ []

input as fn [Attr msg]: Html msg =
    VirtualDom.ElementNode "input" __ []

pre as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "pre" __ __

span as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "span" __ __

text as fn Text: Html msg =
    VirtualDom.TextNode __

textarea as fn [Attr msg], Text: Html msg =
    fn attrs, content:
    VirtualDom.ElementNode "textarea" attrs [ VirtualDom.TextNode content ]

#
# Attributes
#

alt as fn Text: Attr msg =
    VirtualDom.DomAttribute "alt" __

ariaHidden as fn Bool: Attr msg =
    fn s:
    VirtualDom.DomAttribute "aria-hidden" (if s then "true" else "false")

id as fn Text: Attr msg =
    VirtualDom.DomAttribute "id" __

class as fn Text: Attr msg =
    VirtualDom.CssClass

style as fn Text, Text: Attr msg =
    VirtualDom.CssStyle

href as fn Text: Attr msg =
    VirtualDom.DomAttribute "href" __

width as fn Text: Attr msg =
    VirtualDom.DomAttribute "width" __

height as fn Text: Attr msg =
    VirtualDom.DomAttribute "height" __

src as fn Text: Attr msg =
    VirtualDom.DomAttribute "src" __

value as fn Text: Attr msg =
    VirtualDom.DomProperty "value" __

on as fn Text, VirtualDom.EventHandler msg: Attr msg =
    VirtualDom.Listener __ __

onClick as fn msg: Attr msg =
    fn msg:
    on "click" (fn event: Ok msg)

onInput as fn (fn Text: msg): Attr msg =
    fn textToMsg:
    on "input" (fn e: VirtualDom.eventToText ["target", "value"] e >> Result.map textToMsg __)

spellcheck as fn Bool: Attr msg =
    fn s:
    VirtualDom.DomAttribute "spellcheck" (if s then "true" else "false")

