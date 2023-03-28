
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


union VirtualNode msg =
    , ElementNode Text [Attr msg] [VirtualNode msg]
    , TextNode Text


union DomNode =
    # This is a stupid way to say that DomNode has no constructors..
    DomNode DomNode


union Effect =
    Effect Effect


union Event =
    Event Event


alias EventHandler msg =
    fn Event: Result Text msg


union Attr msg =
    , CssClass Text
    , CssStyle Text Text
    , Listener Text (EventHandler msg)
    , DomAttribute Text Text


#
#
#
map as fn (fn a: b), VirtualNode a: VirtualNode b =
    fn f, n:
    try n as
        , TextNode t:
            TextNode t
        , ElementNode name attrs children:
            ElementNode name (List.map (mapAttr f __) attrs) (List.map (map f __) children)

mapAttr as fn (fn a: b), Attr a: Attr b =
    fn f, a:
    try a as
        , Listener n handler:
            Listener n (fn ev: Result.map f (handler ev))

        , _:
          a


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

jsAppendChild as fn { parent as DomNode, child as DomNode }: None =
    fn pars:
    todo "jsAppendChild"

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

setChild as fn (fn DomNode: DomNode), Int, DomNode: None =
    fn update, index, parent:
    todo "setChild"


removeAllChildrenStartingFromIndex as fn Int, DomNode: None =
    fn index, parent:
    todo "removeAllChildrenStartingFromIndex"


##################

# TODO eventually we'll support decoders

eventToText as fn [Text], Event: Result Text Text =
    fn path, event:
    todo "eventToText"


eventToFloat as fn [Text], Event: Result Text Number =
    fn path, event:
    todo "eventToFloat"


###################


drawCanvas as fn Text, (fn Number, Number: { r as Number, g as Number, b as Number }): Effect =
    fn id, shaderFn:
    todo "drawCanvas"


setViewportOf as fn Text, Number, Number: Effect =
    fn id, top, left:
    todo "setViewportOf"


###################


updateDomAttrs as fn [Attr msg], [Attr msg], DomNode: None =
    fn new, old, domNode:

    # TODO maybe I should store these separately so I don't have to re-process old to compare them?

    # TODO also, I'm removing and re-adding listeners every single time, it's probably not a good idea

    !oldClass = ""
    !oldStyle = ""
    !oldDomAttrs = Hash.fromList []

    List.each old fn a:
        try a as
            , CssClass value:
                @oldClass := cloneUni @oldClass .. " " .. value

            , CssStyle key value:
                @oldStyle := cloneUni @oldStyle .. key .. ": " .. value .. "; "

            , Listener eventName eventHandler:
                jsRemoveEventListener eventName eventHandler domNode

            , DomAttribute name value:
                Hash.insert @oldDomAttrs name value

    if cloneUni @oldClass /= "" then
        Hash.insert @oldDomAttrs "class" (cloneUni @oldClass)
    else
        None

    if cloneUni @oldStyle /= "" then
        Hash.insert @oldDomAttrs "style" (cloneUni @oldStyle)
    else
        None


    !newClass = ""
    !newStyle = ""
    !newDomAttrs = Hash.fromList []

    List.each new fn a:
        try a as
            , CssClass value:
                @newClass := cloneUni @newClass .. " " .. value

            , CssStyle key value:
                @newStyle := cloneUni @newStyle .. key .. ": " .. value .. "; "

            , Listener eventName eventHandler:
                jsAddEventListener eventName eventHandler domNode

            , DomAttribute name value:
                Hash.insert @newDomAttrs name value


    if cloneUni @newClass /= "" then
        Hash.insert @newDomAttrs "class" (cloneUni @newClass)
    else
        None

    if cloneUni @newStyle /= "" then
        Hash.insert @newDomAttrs "style" (cloneUni @newStyle)
    else
        None


    Hash.each @newDomAttrs fn newName, newValue:
        try Hash.get @oldDomAttrs newName as
            , Nothing:
                jsSetAttribute newName newValue domNode

            , Just oldValue:

                Hash.remove @oldDomAttrs newName

                if oldValue /= newValue then
                    jsSetAttribute newName newValue domNode
                else
                    None

    Hash.each @oldDomAttrs fn oldName, oldValue:
        try Hash.get @newDomAttrs oldName as
            , Nothing:
                jsRemoveAttribute oldName domNode
            , Just newValue:
                None



render as fn VirtualNode msg: DomNode =
    fn vnode:

    try vnode as
        , ElementNode tagName attrs children:
            renderElementNode tagName attrs children

        , TextNode content:
            jsCreateTextNode content


renderElementNode as fn Text, [Attr msg], [VirtualNode msg]: DomNode =
    fn tagName, attrs, children:

    domNode =
        jsCreateElement tagName

    updateDomAttrs attrs [] domNode

    List.each children fn child:
        jsAppendChild { parent = domNode, child = render child }

    domNode


updateDomNode as fn VirtualNode msg, VirtualNode msg, DomNode: DomNode =
    fn new, old, domNode:

    try new & old as

        , ElementNode nTag nAttr nChildren & ElementNode oTag oAttr oChildren:

            if nTag /= oTag then
                jsReplaceWith (renderElementNode nTag nAttr nChildren) domNode

            else
                updateDomAttrs nAttr oAttr domNode
                updateDomChildren nChildren oChildren 0 domNode
                domNode

        , TextNode n & TextNode o:
            if n == o then
                domNode
            else
                jsReplaceWith (jsCreateTextNode n) domNode

        , _:
            jsReplaceWith (render new) domNode


updateDomChildren as fn [VirtualNode msg], [VirtualNode msg], Int, DomNode: None =
    fn new, old, index, parentNode:

    try new & old as
        , (n :: ns) & (o :: os):
            setChild (updateDomNode n o __) index parentNode
            updateDomChildren ns os (index + 1) parentNode

        , (n :: ns) & []:
            List.each new fn child:
                jsAppendChild { parent = parentNode, child = render child }

        , [] & (o :: os):
            removeAllChildrenStartingFromIndex index parentNode

        , [] & []:
            None


## APP


alias App msg model =
    {
    , init as fn @Array Effect: model
    , update as fn @Array Effect, msg, model: model
    , view as fn model: VirtualNode msg
    }
