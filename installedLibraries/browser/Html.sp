Html msg =
    VirtualDom.VirtualNode msg


Attr msg =
    VirtualDom.Attr msg


map as fn fn a: b, Html a: Html b =
    VirtualDom.map


#
# Elements
#

none as Html msg =
    VirtualDom.'textNode ""


a as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "a" __ __


button as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "button" __ __


canvas as fn [ Attr msg ]: Html msg =
    VirtualDom.'elementNode "canvas" __ []


code as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "code" __ __


div as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "div" __ __


h1 as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "h1" __ __


img as fn [ Attr msg ]: Html msg =
    VirtualDom.'elementNode "img" __ []


input as fn [ Attr msg ]: Html msg =
    VirtualDom.'elementNode "input" __ []


pre as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "pre" __ __


span as fn [ Attr msg ], [ Html msg ]: Html msg =
    VirtualDom.'elementNode "span" __ __


text as fn Text: Html msg =
    VirtualDom.'textNode __


textarea as fn [ Attr msg ], Text: Html msg =
    fn attrs, content:
    VirtualDom.'elementNode "textarea" attrs [ VirtualDom.'textNode content ]


#
# Attributes
#

alt as fn Text: Attr msg =
    VirtualDom.'domAttribute "alt" __


ariaHidden as fn Bool: Attr msg =
    fn s:
    VirtualDom.'domAttribute "aria-hidden" (if s then "true" else "false")


id as fn Text: Attr msg =
    VirtualDom.'domAttribute "id" __


class as fn Text: Attr msg =
    VirtualDom.'cssClass __


style as fn Text, Text: Attr msg =
    VirtualDom.'cssStyle __ __


href as fn Text: Attr msg =
    VirtualDom.'domAttribute "href" __


width as fn Text: Attr msg =
    VirtualDom.'domAttribute "width" __


height as fn Text: Attr msg =
    VirtualDom.'domAttribute "height" __


src as fn Text: Attr msg =
    VirtualDom.'domAttribute "src" __


value as fn Text: Attr msg =
    VirtualDom.'domProperty "value" __


on as fn Text, VirtualDom.EventHandler msg: Attr msg =
    VirtualDom.'listener __ __


onClick as fn msg: Attr msg =
    fn msg:
    on "click" (fn event: 'ok msg)


onInput as fn fn Text: msg: Attr msg =
    fn textToMsg:
    on "input" (fn e: VirtualDom.eventToText [ "target", "value" ] e >> Result.map __ textToMsg)


spellcheck as fn Bool: Attr msg =
    fn s:
    VirtualDom.'domAttribute "spellcheck" (if s then "true" else "false")
