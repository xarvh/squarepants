
#
# IMPORTANT
#
# The following code desperately needs two language features that are not yet available:
#   1) Uniqueness types, to safely manage the update-in-place of DOM nodes
#   2) Libraries, to expose only safe interfaces
#
# Until those are in place, this code is ***SUUUUUUPER UNSAFE**.
#
# Please use it only if you are willing to cope with a world of hurt.
#


union VirtualNode msg =
    , ElementNode Text [Attr msg] [VirtualNode msg]
    , TextNode Text


union DomNode =
    # This is a stupid way to say that DomNode has no constructors..
    DomNode DomNode


union Event =
    Event Event


alias EventHandler msg =
    Event: Result Text msg


union Attr msg =
    , CssClass Text
    , CssStyle Text Text
    , Listener Text (EventHandler msg)
    , DomAttribute Text Text


###################


jsCreateTextNode as Text: DomNode =
    content:
    todo "jsCreateTextNode"

jsCreateElement as Text: DomNode =
    tagName:
    todo "jsCreateElement"

jsReplaceWith as DomNode: DomNode: DomNode =
    new: old:
    todo "jsReplaceWith"

jsAppendChild as { parent as DomNode, child as DomNode }: None =
    pars:
    todo "jsAppendChild"

jsSetAttribute as Text: Text: DomNode: None =
    name: value: domNode:
    todo "jsSetAttribute"

jsRemoveAttribute as Text: DomNode: None =
    name: domNode:
    todo "jsRemoveAttribute"

jsAddEventListener as Text: EventHandler msg: DomNode: None =
    eventName: eventHandler: domNode:
    todo "jsAddEventListener"

jsRemoveEventListener as Text: EventHandler msg: DomNode: None =
    eventName: eventHandler: domNode:
    todo "jsRemoveEventListener"

setChild as (DomNode: DomNode): Int: DomNode: None =
    update: index: parent:
    todo "setChild"


removeAllChildrenStartingFromIndex as Int: DomNode: None =
    index: parent:
    todo "removeAllChildrenStartingFromIndex"


##################

# TODO eventually we'll support decoders

eventToText as [Text]: Event: Result Text Text =
    path: event:
    todo "eventToText"


eventToFloat as [Text]: Event: Result Text Number =
    path: event:
    todo "eventToFloat"


# TODO eventually we'll have some FFI

unsafeExecuteJavaScript as Text: a: Result Text None =
    functionName: argument:
    todo "unsafeExecuteJavaScript"


###################


updateDomAttrs as [Attr msg]: [Attr msg]: DomNode: None =
    new: old: domNode:

    # TODO maybe I should store these separately so I don't have to re-process old to compare them?

    # TODO also, I'm removing and re-adding listeners every single time, it's probably not a good idea

    oldClass @= ""
    oldStyle @= ""
    oldDomAttrs @= Hash.empty

    List.each old a:
        try a as
            CssClass value:
                @oldClass := oldClass .. " " .. value

            CssStyle key value:
                @oldStyle := oldStyle .. key .. ": " .. value .. "; "

            Listener eventName eventHandler:
                jsRemoveEventListener eventName eventHandler domNode

            DomAttribute name value:
                Hash.insert @oldDomAttrs name value

    if oldClass /= "" then
        Hash.insert @oldDomAttrs "class" oldClass
    else
        None

    if oldStyle /= "" then
        Hash.insert @oldDomAttrs "style" oldStyle
    else
        None


    newClass @= ""
    newStyle @= ""
    newDomAttrs @= Hash.empty

    List.each new a:
        try a as
            CssClass value:
                @newClass := newClass .. " " .. value

            CssStyle key value:
                @newStyle := newStyle .. key .. ": " .. value .. "; "

            Listener eventName eventHandler:
                jsAddEventListener eventName eventHandler domNode

            DomAttribute name value:
                Hash.insert @newDomAttrs name value


    if newClass /= "" then
        Hash.insert @newDomAttrs "class" newClass
    else
        None

    if newStyle == "" then
        Hash.insert @newDomAttrs "style"newStyle
    else
        None


    Hash.each newDomAttrs newName: newValue:
        try Hash.get oldDomAttrs newName as
            Nothing:
                jsSetAttribute newName newValue domNode

            Just oldValue:

                Hash.remove @oldDomAttrs newName

                if oldValue /= newValue then
                    jsSetAttribute newName newValue domNode
                else
                    None

    Hash.each oldDomAttrs oldName: oldValue:
        try Hash.get newDomAttrs oldName as
            Nothing:
                jsRemoveAttribute oldName domNode
            Just newValue:
                None



render as VirtualNode msg: DomNode =
    vnode:

    try vnode as
        ElementNode tagName attrs children:
            renderElementNode tagName attrs children

        TextNode content:
            jsCreateTextNode content


renderElementNode as Text: [Attr msg]: [VirtualNode msg]: DomNode =
    tagName: attrs: children:

    domNode =
        jsCreateElement tagName

    updateDomAttrs attrs [] domNode

    List.each children child:
        jsAppendChild { parent = domNode, child = render child }

    domNode


updateDomNode as VirtualNode msg: VirtualNode msg: DomNode: DomNode =
    new: old: domNode:

    try new & old as

        ElementNode nTag nAttr nChildren & ElementNode oTag oAttr oChildren:

            if nTag /= oTag then
                jsReplaceWith (renderElementNode nTag nAttr nChildren) domNode

            else
                updateDomAttrs nAttr oAttr domNode
                updateDomChildren nChildren oChildren 0 domNode
                domNode

        TextNode n & TextNode o:
            if n == o then
                domNode
            else
                jsReplaceWith (jsCreateTextNode n) domNode

        _:
            jsReplaceWith (render new) domNode


updateDomChildren as [VirtualNode msg]: [VirtualNode msg]: Int: DomNode: None =
    new: old: index: parentNode:

    try new & old as
        (n :: ns) & (o :: os):
            setChild (updateDomNode n o) index parentNode
            updateDomChildren ns os (index + 1) parentNode

        (n :: ns) & []:
            List.each new child:
                jsAppendChild { parent = parentNode, child = render child }

        [] & (o :: os):
            removeAllChildrenStartingFromIndex index parentNode

        [] & []:
            None


## APP


alias App msg model =
    { init as None: model
    , update as msg: model: model
    , view as model: VirtualNode msg
    }
