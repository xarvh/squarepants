
platform as Types/Platform.Platform =
    {
    , name = "browser"
    , defaultModules = DefaultModules.asText .. modules
    , quickstart = "TODO"
    , defaultOutputPath = "index.js"
#    , compileStatements
    , makeExecutable
    }


modules as Text =
    """

library =
    source = "core:browser"

    module =
        path = Browser
    """


virtualDomModule as fn Text: USR =
    USR (UMR Meta.Browser "VirtualDom") __


#compileStatements as fn Types/Platform.GetRidOfMe, @Compiler/MakeEmittable.State, [EA.GlobalDefinition]: Text =
#    fn getRidOfMe, @emState, emittableStatements:
#
#    { constructors } =
#        getRidOfMe



makeExecutable as fn Compiler/Compiler.CompileModulesOut: Text =
    fn out:

    !emState =
        cloneImm out.state

    compiledStatements =
        log "Creating JS AST..." ""
        jaStatements =
            Targets/Javascript/EmittableToJs.translateAll @emState
                {
                , constructors = out.constructors
                , eaDefs = out.defs
                , platformOverrides = overrides
                }

        log "Emitting JS..." ""
        jaStatements
        >> List.map (Targets/Javascript/JsToText.emitStatement 0 __) __
        >> Text.join "\n\n" __

    # TODO check that type is ....?

    header .. Targets/Javascript/Runtime.nativeDefinitions .. runtime .. compiledStatements .. footer @emState mainName


overrides as [USR & Text] =
    [
    , virtualDomModule "jsCreateTextNode" & "virtualDom_jsCreateTextNode"
    , virtualDomModule "jsCreateElement" & "virtualDom_jsCreateElement"
    , virtualDomModule "jsReplaceWith" & "virtualDom_jsReplaceWith"
    , virtualDomModule "jsAppendChild" & "virtualDom_jsAppendChild"
    , virtualDomModule "jsSetAttribute" & "virtualDom_jsSetAttribute"
    , virtualDomModule "jsRemoveAttribute" & "virtualDom_jsRemoveAttribute"
    , virtualDomModule "jsAddEventListener" & "virtualDom_jsAddEventListener"
    , virtualDomModule "jsRemoveEventListener" & "virtualDom_jsRemoveEventListener"
    , virtualDomModule "eventToText" & "virtualDom_eventToText"
    , virtualDomModule "eventToFloat" & "virtualDom_eventToFloat"
    , virtualDomModule "setChild" & "virtualDom_setChild"
    , virtualDomModule "removeAllChildrenStartingFromIndex" & "virtualDom_removeAllChildrenStartingFromIndex"
    , virtualDomModule "unsafeExecuteJavaScript" & "virtualDom_unsafeExecuteJavaScript"
    ]


header as Text =
    "(function (win) {\n"


footer as fn @Compiler/MakeEmittable.State, Text: Text =
    fn @state, mainName:

    updateDomNode =
        Compiler/MakeEmittable.translateUsr @state (virtualDomModule "updateDomNode")

    """

    // TODO these globals will be a hell of trouble if we want to run more than one app
    let oldVirtualDom = {}; // TODO this should be properly initialized
    let model = null;
    let elementId = null;

    function dispatch(msgResult) {
        if (msgResult[0] === "Ok") {

            const msg = msgResult[1];

            model = """ .. mainName .. """.update(msg, model);

            // TODO set a flag and use requestAnimationFrame
            updateDom();
        } else {
            console.log('rejecting msg: ', msgResult[1]);
        }
    }


    function updateDom() {
        const e = win.document.getElementById(elementId);

        const newVirtualDom = """ .. mainName .. """.view(model);

        """ .. updateDomNode .. """(newVirtualDom, oldVirtualDom, e.childNodes[0]);

        oldVirtualDom = newVirtualDom;
    }



    function main(eid) {
        elementId = eid;
        model = """ .. mainName .. """.init(null);
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

const virtualDom_unsafeExecuteJavaScript = (functionName, argument) => {
    let error = null;

    try {
        const fn = functionName.split('.').reduce((obj, name) => obj[name], window);
        fn(argument);
    } catch (e) {
      error = e;
    }

    return error
        ? [ 'Err', error.message ]
        : [ 'Ok', null ]
}

    """
