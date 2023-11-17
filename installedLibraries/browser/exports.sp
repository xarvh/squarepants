
module =
   path = Html
   exposes =
        Html
        Attr
        map
        none
        a
        button
        canvas
        code
        div
        h1
        img
        input
        pre
        span
        text
        textarea

        alt
        ariaHidden
        id
        class
        style
        href
        width
        height
        src
        value
        on
        onClick
        onInput
        spellcheck



module =
   path = VirtualDom
   exposes =
        VirtualNode
        DomNode
        Effect
        Event
        EventHandler

        Attr
        'cssClass
        'cssStyle
        'listener
        'domAttribute
        'domProperty

        map
        mapAttr
        jsCreateTextNode
        jsCreateElement
        jsReplaceWith
        jsAppendChild
        jsSetProperty
        jsSetAttribute
        jsRemoveAttribute
        jsAddEventListener
        jsRemoveEventListener
        setChild
        removeAllChildrenStartingFromIndex

        eventToText
        eventToFloat
        drawCanvas
        setViewportOf
        updateDomAttrs
        render
        renderElementNode
        updateDomNode
        updateDomChildren
        App

