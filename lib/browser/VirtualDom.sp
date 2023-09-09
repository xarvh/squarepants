#
# IMPORTANT
#
# The following code desperately needs to expose only safe interfaces,
# but this will be possible only once the libraries system is in place.
#
# Until then, this code is **SUUUUUUPER UNSAFE**.
#
# Please use it only if you are willing to cope with a world of hurt.
#

var VirtualNode msg =
    , 'elementNode Text [ Attr msg ] [ VirtualNode msg ]
    , 'textNode Text


var DomNode =
    , # This is a stupid way to say that DomNode has no constructors..
      'domNode DomNode


var Effect =
    , 'effect Effect


var Event =
    , 'event Event


EventHandler msg =
    fn Event: Result Text msg


var Attr msg =
    , 'cssClass Text
    , 'cssStyle Text Text
    , 'listener Text (EventHandler msg)
    , 'domAttribute Text Text
    , 'domProperty Text Text


#
#
#
map as fn fn a: b, VirtualNode a: VirtualNode b =
    fn f, n:
    try n as
        'textNode t: 'textNode t
        'elementNode name attrs children: 'elementNode name (List.map (mapAttr f __) attrs) (List.map (map f __) children)


mapAttr as fn fn a: b, Attr a: Attr b =
    fn f, a:
    try a as
        'listener n handler: 'listener n (fn ev: Result.map f (handler ev))
        _: a


###################

jsCreateTextNode as fn Text: DomNode =
    fn content:
    todo "jsCreateTextNode"


jsCreateElement as fn Text: DomNode =
    fn tagName:
    todo "jsCreateElement"


jsReplaceWith as fn DomNode, DomNode: DomNode =
    fn new, old:
    todo "jsReplaceWith"


jsAppendChild as fn { child as DomNode, parent as DomNode }: None =
    fn pars:
    todo "jsAppendChild"


jsSetProperty as fn Text, Text, DomNode: None =
    fn name, value, domNode:
    todo "jsSetProperty"


jsSetAttribute as fn Text, Text, DomNode: None =
    fn name, value, domNode:
    todo "jsSetAttribute"


jsRemoveAttribute as fn Text, DomNode: None =
    fn name, domNode:
    todo "jsRemoveAttribute"


jsAddEventListener as fn Text, EventHandler msg, DomNode: None =
    fn eventName, eventHandler, domNode:
    todo "jsAddEventListener"


jsRemoveEventListener as fn Text, EventHandler msg, DomNode: None =
    fn eventName, eventHandler, domNode:
    todo "jsRemoveEventListener"


setChild as fn fn DomNode: DomNode, Int, DomNode: None =
    fn update, index, parent:
    todo "setChild"


removeAllChildrenStartingFromIndex as fn Int, DomNode: None =
    fn index, parent:
    todo "removeAllChildrenStartingFromIndex"


##################

# TODO eventually we'll support decoders

eventToText as fn [ Text ], Event: Result Text Text =
    fn path, event:
    todo "eventToText"


eventToFloat as fn [ Text ], Event: Result Text Number =
    fn path, event:
    todo "eventToFloat"


###################

drawCanvas as fn Text, fn Number, Number: { b as Number, g as Number, r as Number }: Effect =
    fn id, shaderFn:
    todo "drawCanvas"


setViewportOf as fn Text, Number, Number: Effect =
    fn id, top, left:
    todo "setViewportOf"


###################

updateDomAttrs as fn [ Attr msg ], [ Attr msg ], DomNode: None =
    fn new, old, domNode:
    # TODO maybe I should store these separately so I don't have to re-process old to compare them?

    # TODO also, I'm removing and re-adding listeners every single time, it's probably not a good idea

    !oldClass =
        ""

    !oldStyle =
        ""

    !oldDomAttrs =
        Hash.fromList []

    List.each old fn a:
        try a as
            'cssClass value: @oldClass := cloneUni @oldClass .. " " .. value
            'cssStyle key value: @oldStyle := cloneUni @oldStyle .. key .. ": " .. value .. "; "
            'listener eventName eventHandler: jsRemoveEventListener eventName eventHandler domNode
            'domAttribute name value: Hash.insert @oldDomAttrs name value
            'domProperty name value: 'none

    if cloneUni @oldClass /= "" then
        Hash.insert @oldDomAttrs "class" (cloneUni @oldClass)
    else
        'none

    if cloneUni @oldStyle /= "" then
        Hash.insert @oldDomAttrs "style" (cloneUni @oldStyle)
    else
        'none

    !newClass =
        ""

    !newStyle =
        ""

    !newDomAttrs =
        Hash.fromList []

    List.each new fn a:
        try a as
            'cssClass value: @newClass := cloneUni @newClass .. " " .. value
            'cssStyle key value: @newStyle := cloneUni @newStyle .. key .. ": " .. value .. "; "
            'listener eventName eventHandler: jsAddEventListener eventName eventHandler domNode
            'domAttribute name value: Hash.insert @newDomAttrs name value
            'domProperty name value: jsSetProperty name value domNode

    if cloneUni @newClass /= "" then
        Hash.insert @newDomAttrs "class" (cloneUni @newClass)
    else
        'none

    if cloneUni @newStyle /= "" then
        Hash.insert @newDomAttrs "style" (cloneUni @newStyle)
    else
        'none

    Hash.each @newDomAttrs fn newName, newValue:
        try Hash.get @oldDomAttrs newName as

            'nothing:
                jsSetAttribute newName newValue domNode

            'just oldValue:
                Hash.remove @oldDomAttrs newName

                if oldValue /= newValue then
                    jsSetAttribute newName newValue domNode
                else
                    'none

    Hash.each @oldDomAttrs fn oldName, oldValue:
        try Hash.get @newDomAttrs oldName as
            'nothing: jsRemoveAttribute oldName domNode
            'just newValue: 'none


render as fn VirtualNode msg: DomNode =
    fn vnode:
    try vnode as
        'elementNode tagName attrs children: renderElementNode tagName attrs children
        'textNode content: jsCreateTextNode content


renderElementNode as fn Text, [ Attr msg ], [ VirtualNode msg ]: DomNode =
    fn tagName, attrs, children:
    domNode =
        jsCreateElement tagName

    updateDomAttrs attrs [] domNode

    List.each children fn child:
        jsAppendChild { child = render child, parent = domNode }

    domNode


updateDomNode as fn VirtualNode msg, VirtualNode msg, DomNode: DomNode =
    fn new, old, domNode:
    try new & old as

        'elementNode nTag nAttr nChildren & 'elementNode oTag oAttr oChildren:
            if nTag /= oTag then
                jsReplaceWith (renderElementNode nTag nAttr nChildren) domNode
            else
                updateDomAttrs nAttr oAttr domNode

                updateDomChildren nChildren oChildren 0 domNode

                domNode

        'textNode n & 'textNode o:
            if n == o then
                domNode
            else
                jsReplaceWith (jsCreateTextNode n) domNode

        _:
            jsReplaceWith (render new) domNode


updateDomChildren as fn [ VirtualNode msg ], [ VirtualNode msg ], Int, DomNode: None =
    fn new, old, index, parentNode:
    try new & old as

        (n :: ns) & (o :: os):
            setChild (updateDomNode n o __) index parentNode

            updateDomChildren ns os (index + 1) parentNode

        (n :: ns) & []:
            List.each new fn child:
                jsAppendChild { child = render child, parent = parentNode }

        [] & (o :: os):
            removeAllChildrenStartingFromIndex index parentNode

        [] & []:
            'none


## APP

App msg model =
    {
    , init as fn @Array Effect: model
    , update as fn @Array Effect, msg, model: model
    , view as fn model: VirtualNode msg
    }
