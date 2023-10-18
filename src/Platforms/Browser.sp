platform as Platform =
    {
    , defaultModules = DefaultModules.asText .. modules
    , defaultOutputPath = "index.js"
    , makeExecutable
    , name = "browser"
    , quickstart = "TODO"
    }


modules as Text =
    """

    library =
        source = "core:browser"

        module =
            path = Browser

        module =
            path = Html

        module =
            path = VirtualDom
    """


virtualDomModule as fn Text: USR =
    'USR ('UMR Meta.'platform "VirtualDom") __


compile as fn Self.LoadPars: Text =
    fn out:
    log "Creating JS AST..." ""

    jaStatements =
        Targets/Javascript/EmittableToJs.translateAll
            {
            , constructors = out.constructors
            , eaDefs = out.defs
            , platformOverrides = overrides
            }

    log "Emitting JS..." ""

    jaStatements
    >> List.map (Targets/Javascript/JsToText.emitStatement 0 __) __
    >> Text.join "\n\n" __


makeExecutable as fn Self.LoadPars: Text =
    fn out:
    compiledStatements =
        compile out

    # TODO check that type is ....?

    header .. Targets/Javascript/Runtime.nativeDefinitions .. runtime .. compiledStatements .. footer out.entryUsr


overrides as [ USR & Text ] =
    [
    , virtualDomModule "jsCreateTextNode" & "virtualDom_jsCreateTextNode"
    , virtualDomModule "jsCreateElement" & "virtualDom_jsCreateElement"
    , virtualDomModule "jsReplaceWith" & "virtualDom_jsReplaceWith"
    , virtualDomModule "jsAppendChild" & "virtualDom_jsAppendChild"
    , virtualDomModule "jsSetProperty" & "virtualDom_jsSetProperty"
    , virtualDomModule "jsSetAttribute" & "virtualDom_jsSetAttribute"
    , virtualDomModule "jsRemoveAttribute" & "virtualDom_jsRemoveAttribute"
    , virtualDomModule "jsAddEventListener" & "virtualDom_jsAddEventListener"
    , virtualDomModule "jsRemoveEventListener" & "virtualDom_jsRemoveEventListener"
    , virtualDomModule "eventToText" & "virtualDom_eventToText"
    , virtualDomModule "eventToFloat" & "virtualDom_eventToFloat"
    , virtualDomModule "setChild" & "virtualDom_setChild"
    , virtualDomModule "removeAllChildrenStartingFromIndex" & "virtualDom_removeAllChildrenStartingFromIndex"
    , virtualDomModule "drawCanvas" & "virtualDom_drawCanvas"
    , virtualDomModule "setViewportOf" & "virtualDom_setViewportOf"
    ]


header as Text =
    "(function (win) {\n"


footer as fn USR: Text =
    fn mainUsr:
    mainName =
        Targets/Javascript/EmittableToJs.translateUsr mainUsr

    updateDomNode =
        Targets/Javascript/EmittableToJs.translateUsr (virtualDomModule "updateDomNode")

    """

    // TODO these globals will be a hell of trouble if we want to run more than one app
    let effects = [];
    let oldVirtualDom = {}; // TODO this should be properly initialized
    let model = null;
    let elementId = null;

    function dispatch(msgResult) {
        if (msgResult[0] === "Ok") {

            const msg = msgResult[1];

            model =
    """
    .. mainName
    .. """
    .update(effects, msg, model)[0];

                // TODO set a flag and use requestAnimationFrame
                updateDom();
            } else {
                console.log('rejecting msg: ', msgResult[1]);
            }
        }


        function updateDom() {
            const e = win.document.getElementById(elementId);

            const newVirtualDom =
    """
    .. mainName
    .. """
    .view(model);


    """
    .. updateDomNode
    .. """
    (newVirtualDom, oldVirtualDom, e.childNodes[0]);

            oldVirtualDom = newVirtualDom;

            effects.forEach((e) => e());
            effects = [];
        }



        function main(eid) {
            elementId = eid;
            model =
    """
    .. mainName
    .. """
    .init(effects)[0];
            updateDom();
        }





        win.Squarepants = {
            main: main,
        };

    })(this);

    """


runtime as Text =
    """
    const crawlObject = (path, type, object) => {

        while(path[0] === 'Cons') {

            const head = path[1];
            const tail = path[2];

            const o = object[head];

            if (o === undefined) {
                return [ 'Err', 'no field named: ' + head ];
            }

            object = o;
            path = path[2];
        }

        return typeof object === type
            ? [ 'Ok', object ]
            : [ 'Err', 'wrong type: ' + typeof object ]
            ;
    }


    const virtualDom_eventToText = (path, event) => crawlObject(path, 'string', event);
    const virtualDom_eventToFloat = (path, event) => crawlObject(path, 'number', event);

    // TODO ensure that those who must return None actually return None (ie, null)
    const virtualDom_jsCreateTextNode = (content) => document.createTextNode(content);
    const virtualDom_jsCreateElement = (tag) => document.createElement(tag);
    const virtualDom_jsReplaceWith = (new_, old) => { old.replaceWith(new_); return new_; }
    const virtualDom_jsAppendChild = (pars) => pars.parent.appendChild(pars.child);
    const virtualDom_jsSetAttribute = (name, value, node) => node.setAttribute(name, value);
    const virtualDom_jsRemoveAttribute = (name, node) => node.removeAttribute(name);
    const virtualDom_jsSetProperty = (name, value, node) => node[name] = value;


    const virtualDom_setChild = (upd, index, parentNode) => {
        const child = parentNode.childNodes[index];
        child && upd(child);
    };


    const virtualDom_removeAllChildrenStartingFromIndex = (index, parentNode) => {
        while(parentNode.childNodes[index]) {
          parentNode.removeChild(parentNode.childNodes[index]);
        }
    }


    // an EventHandler is a function that takes an Event and produces a msg
    const virtualDom_jsAddEventListener = (eventName, handler, node) => {

        node.squarepantsEventHandlers = node.squarepantsEventHandlers || {};

        if (node.squarepantsEventHandlers[eventName]) {
          node.removeEventListener(eventName, node.squarepantsEventHandlers[eventName]);
        }

        const onEvent = (event) => dispatch(handler(event));
        node.squarepantsEventHandlers[eventName] = onEvent;
        node.addEventListener(eventName, onEvent);
    };

    const virtualDom_jsRemoveEventListener = (eventName, handler, node) => {
        node.removeEventListener(eventName, node.squarepantsEventHandlers[eventName]);
        node.squarepantsEventHandlers[eventName] = undefined;
    }


    const virtualDom_setViewportOf = (id, top, left) => () => {
        const e = document.getElementById(id);
        if (!e) {
            console.error('could not find element #' + id);
            return
        }

        e.scrollTop = top;
        e.scrollLeft = left;
    }


    const virtualDom_drawCanvas = (canvasId, shaderFn) => () => {

        const canvas = document.getElementById(canvasId);
        if (!canvas) {
            console.error('could not find canvas', canvasId);
            return
        }

        const w = canvas.width;
        const h = canvas.height;

        const ctx = canvas.getContext('2d');
        const imageData = ctx.createImageData(w, h);

        for (let x = 0; x < w; x++) for (let y = 0; y < h; y++) {

            const frag = shaderFn(x / (w - 1), 1 - y / (h - 1));

            let j = (x + y * w) * 4;
            imageData.data[j + 0] = frag.r * 255;
            imageData.data[j + 1] = frag.g * 255;
            imageData.data[j + 2] = frag.b * 255;
            imageData.data[j + 3] = 255;
        }

        ctx.putImageData(imageData, 0, 0);
    };
    """
