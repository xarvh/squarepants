

alias Html msg =
    VirtualDom.VirtualNode msg


alias Attr msg =
    VirtualDom.Attr msg


#
# Elements
#

none as Html msg =
    VirtualDom.TextNode ""

text as fn Text: Html msg =
    VirtualDom.TextNode __

div as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "div" __ __

span as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "span" __ __

a as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "a" __ __

input as fn [Attr msg]: Html msg =
    VirtualDom.ElementNode "input" __ []

textarea as fn [Attr msg], Text: Html msg =
    fn attrs, content:
    VirtualDom.ElementNode "textarea" attrs [ VirtualDom.TextNode content ]

pre as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "pre" __ __

code as fn [Attr msg], [Html msg]: Html msg =
    VirtualDom.ElementNode "code" __ __

canvas as fn [Attr msg]: Html msg =
    VirtualDom.ElementNode "canvas" __ []

#
# Attributes
#

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

