

alias Html msg =
    VirtualDom.VirtualNode msg


alias Attr msg =
    VirtualDom.Attr msg


#
# Elements
#

none as Html msg =
    VirtualDom.TextNode ""

text as Text: Html msg =
    VirtualDom.TextNode

div as [Attr msg]: [Html msg]: Html msg =
    VirtualDom.ElementNode "div"

span as [Attr msg]: [Html msg]: Html msg =
    VirtualDom.ElementNode "span"

a as [Attr msg]: [Html msg]: Html msg =
    VirtualDom.ElementNode "a"

input as [Attr msg]: Html msg =
    attrs:
    VirtualDom.ElementNode "input" attrs []

textarea as [Attr msg]: Text: Html msg =
    attrs: content:
    VirtualDom.ElementNode "textarea" attrs [ VirtualDom.TextNode content ]

pre as [Attr msg]: [Html msg]: Html msg =
    VirtualDom.ElementNode "pre"

code as [Attr msg]: [Html msg]: Html msg =
    VirtualDom.ElementNode "code"

canvas as [Attr msg]: Html msg =
    attrs:
    VirtualDom.ElementNode "canvas" attrs []

#
# Attributes
#

id as Text: Attr msg =
    VirtualDom.DomAttribute "id"

class as Text: Attr msg =
    VirtualDom.CssClass

href as Text: Attr msg =
    VirtualDom.DomAttribute "href"

width as Text: Attr msg =
    VirtualDom.DomAttribute "width"

height as Text: Attr msg =
    VirtualDom.DomAttribute "height"

on as Text: VirtualDom.EventHandler msg: Attr msg =
    VirtualDom.Listener

onClick as msg: Attr msg =
    msg:
    on "click" (event: Ok msg)

onInput as (Text: msg): Attr msg =
    textToMsg:
    on "input" (e: e >> VirtualDom.eventToText ["target", "value"] >> Result.map textToMsg)

