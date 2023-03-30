(function (win) {
let __re__;


const sp_clone = (src) => {
 if (Array.isArray(src))
   return src.map(sp_clone);

 if (typeof(src) === 'object') {
   const dest = {};
   for (let k in src) { dest[k] = sp_clone(src[k]); }
   return dest;
 }

 return src;
}


/*  HACK

    TODO this is super brittle
    once we have a proper Platform system in place, the platform can probably
    use its internal Meta to figure out the proper constructor

*/
const maybe_nothing = [ "Nothing" ];
const maybe_just = (a) => [ "Just", a ];


//
// Basic ops
//


const sp_equal = (a, b) => {
  if (a === b)
    return true

  if (Array.isArray(a)) {
    if (!Array.isArray(b)) return false;

    const l = a.length;
    if (l !== b.length) return false;

    let i = 0;
    while (i < l) {
      if (!sp_equal(a[i], b[i])) return false;
      ++i;
    }

    return true;
  }

  if (typeof(a) === 'object') {
    if (typeof(b) !== 'object') return false;

    const keys = Object.keys(a);
    const l = keys.length;
    if (l !== Object.keys(b).length) return false;

    let i = 0;
    while (i < l) {
      let k = keys[i];
      if (!sp_equal(a[k], b[k])) return false;
      ++i;
    }

    return true;
  }

  return false;
}


const sp_not_equal = (a, b) => {
  return !sp_equal(a, b);
}


const basics_compare = (a, b) => {

  // union type
  if (Array.isArray(a)) {
    // compare constructor names
    if (a[0] > b[0]) return 1;
    if (b[0] > a[0]) return -1;
    for (let i = 1; i < a.length; i++) {
        const cmp = basics_compare(a[i], b[i]);
        if (cmp) return cmp;
    }
    return 0;
  }

  // None is represented as null
  if (a === null)
      return 0;

  if (typeof a === 'object') {
    const keys = Object.keys(a).sort();
    for (let k of keys) {
        const cmp = basics_compare(a[k], b[k]);
        if (cmp) return cmp;
    }
    return 0;
  }

  if (a > b) return 1;
  if (a < b) return -1;
  return 0;
}

const sp_divide = (left, right) => {
  if (right === 0) return 0;
  return left / right;
}


// TODO remove this and handle it like any other op?
const basics_modBy = (a, b) => b % a;


const basics_cloneImm = sp_clone;


const basics_cloneUni = (uni) =>
    [ sp_clone(uni), uni ];


//
// Debug
//


const sp_todo = (message) => {
  throw new Error("TODO: " + message);
}


const sp_log = (message, thing) => {
  console.log(message, sp_toHuman(thing));
  return thing;
}


const sp_throw = function (errorName) {
    console.error(...arguments);
    throw new Error(errorName);
}


//
// Benchmarking
//


var debug_benchStartTime = null;
var debug_benchStartStack = null;
var debug_benchEntries = {};


const pad = (l, s) => ' '.repeat(Math.max(0, l - s.length)) + s;


const fmt = (n) => {
    const s = Math.floor(n) + '';
    return s.slice(0, -3) + '.' + pad(3, s.slice(-3));
}


// TODO how should benchmark work in a browser?
typeof process !== 'undefined' && process.on('beforeExit', (code) => {
    if (debug_benchStartStack !== null)
        console.error(`ERROR: a benchmark has been started but not stopped!
Start was at:${debug_benchStartStack}`);

    const ks = Object.keys(debug_benchEntries);
    if (ks.length) {
        console.info("");
        console.info("Benchmark results:");
        ks.sort().forEach(k => {
            const entry = debug_benchEntries[k];
            console.info(
                    'TotalTime:', pad(10, fmt(entry.dt )) + 's',
                    '   ',
                    'Runs:', pad(6, '' + entry.n),
                    '   ',
                    'Key:', k,
            );
        });
    }
});


const sp_benchStart = (none) => {
    if (debug_benchStartStack !== null)
        throw new Error(`
benchStart called when a benchmark is already ongoing!
Previous benchStart call was ${debug_benchStartStack}
`);

    debug_benchStartStack = new Error().stack;
    debug_benchStartTime = performance.now();
}


const sp_benchStop = (name) => {
    const now = performance.now();

    if (debug_benchStartStack === null)
        throw new Error("benchStop called while no benchmark is ongoing!");

    debug_benchStartStack = null;

    const dt = now - debug_benchStartTime;

    const entry = debug_benchEntries[name] || { dt: 0, n: 0 };
    entry.dt += dt;
    entry.n += 1;
    debug_benchEntries[name] = entry;
}




//
// To Human
//


const id = (n) => '    '.repeat(n);


const sp_toHuman = (a, l = 0) => {

  if (Array.isArray(a))
    return sp_toHumanAsList([], a, l) || sp_toHumanAsUnion(a, l);

  if (typeof a === 'function') {
    return '<fn ' + a.length + '>';
  }

  if (typeof a === 'object') {
    let acc = '{\n';
    for (let key in a)
        acc += id(l + 1) + key + ' = ' + sp_toHuman(a[key], l + 1) + '\n';

    return acc + id(l) + '}';
  }

  return JSON.stringify(a, null, 0);
}


const sp_toHumanAsUnion = (a, l) => {

  if (a.length === 1) {
      return a[0];
  }

  let acc = a[0] + '\n';

  a.slice(1).forEach(arg => {

      const sub = sp_toHuman(arg, l + 1);
      if (!sub.startsWith('{') && sub.indexOf('\n') > -1)
          acc += id(l + 1) + '(' + sub + id(l + 1) + ')\n';
      else
          acc += id(l + 1) + sub + '\n';

  })

  return acc;
}


const sp_toHumanAsList = (arrayAccum, list, l) => {
  if (list[0] === 'Cons' && list.length === 3) {
    arrayAccum.push(sp_toHuman(list[1], l));
    return sp_toHumanAsList(arrayAccum, list[2], l);
  }

  if (list[0] === 'Nil')
    return '[' + arrayAccum.join(', ') + ']';

  return false;
}


//
// Text
//


const text_fromNumber = (n) => '' + n;

const text_toNumber = (t) => {
    const n = +t;

    return isNaN(n) ? maybe_nothing : maybe_just(n);
}

const text_split = (separator, target) => arrayToListLow(target.split(separator));

const text_length = (s) => s.length;

const text_slice = (start, end, s) => s.slice(start, end);

const text_startsWith = (sub, s) => s.startsWith(sub);

const text_startsWithRegex = (regex) => {
  let re;
  try {
    re = new RegExp('^' + regex);
  } catch (e) {
    return () => ""
  }

  return (s) => {
    let m = s.match(re);
    return m ? m[0] : "";
  }
}

const text_replaceRegex = (regex) => {
  let re;
  try {
    re = new RegExp(regex, 'g');
  } catch (e) {
    return () => ""
  }

  return (replacer, s) => s.replace(re, replacer);
}

const text_trimLeft = (s) => s.trimLeft();

const text_dropLeft = (n, s) => s.slice(n);

const text_forEach = (s, f) => {
  for (let i of s) f(i);
  return null;
}


//
// Hashes
//

const hash_fromList = (list) => {
  const hash = {};

  // TODO iteration instead of recursion
  const rec = (ls) => {
    if (ls[0] === 'Nil')
      return hash;

    const { first, second } = ls[1];

    hash[JSON.stringify(first)] = [first, second];

    return rec(ls[2]);
  };

  return rec(list);
}


const hash_insert = (hash, key, value) => {
    hash[JSON.stringify(key)] = [key, value];
    return [null, hash];
}


const hash_remove = (hash, key) => {
    delete hash[JSON.stringify(key)];
    return [null, hash];
}


const hash_get = (hash, key) => {
    const r = hash[JSON.stringify(key)];
    return [r === undefined ? maybe_nothing : maybe_just(r[1]), hash];
}


const hash_for = (hash, f, acc) => {
    for (let k in hash) {
        const kv = hash[k];
        acc = f(kv[0], kv[1], acc);
    }
    return [acc, hash];
}


const hash_each = (hash, f) => {
    for (let k in hash) {
        const kv = hash[k];
        f(kv[0], kv[1]);
    }
    return [null, hash];
}


//
// Arrays
//

const array_each = (array, f) => {
    array.forEach(f);
    return [null, array];
}

const array_push = (array, item) => {
    array.push(item);
    return [null, array];
}

const array_pop = (a) => {
    return [a.length ? maybe_just(a.pop()) : maybe_nothing, a];
}

const array_get = (array, index) => {
    const r = array[index];
    return [r === undefined ? maybe_nothing : maybe_just(r), array];
}

const array_set = (a, index, item) => {
    if (index < 0) return false;
    if (index >= a.length) return [false, a];
    a[index] = item;
    return [true, a];
}

const array_sortBy = (arr, f) => {
    arr.sort((a, b) => basics_compare(f(a), f(b)));
    return [null, arr];
}

const arrayToListLow = (arr) => {
  const length = arr.length;
  let list = [ 'Nil' ];
  for (let i = length - 1; i >= 0; i--) {
      list = [ 'Cons', arr[i], list ];
  }
  return list;
}

const array_toList = (arr) => [arrayToListLow(arr), arr];


const arrayFromListLow = (list) => {
  const array = [];
  const rec = (ls) => {
    if (ls[0] === 'Nil')
      return array;

    array.push(ls[1]);
    return rec(ls[2]);
  };

  return rec(list);
}

const array_fromList = arrayFromListLow;


//
// Lists
//


const sp_cons = (item, list) => {
  return [ 'Cons', item, list];
}

const list_sortBy = (f, list) => arrayToListLow(arrayFromListLow(list).sort((a, b) => basics_compare(f(a), f(b))));


//
// Dynamic loading
//
const self_load = (requestedTypeHumanized, pars, variantConstructor) => {

    const actualTypeHumanized = sp_toHuman(pars.type);
    if (actualTypeHumanized !== requestedTypeHumanized) {
        return [ 'Err', pars.type ];
    }

    // TODO using directly the source name sd1 is super fragile: must revisit this as soon as I have `Load.expose`
    // TODO hoping that the state won't be mutated, once we have `Load.expose` maybe we don't need to lug the state around any more?
    const translateUsr = $sd1$Targets$Javascript$EmittableToJs$translateUsr;
    const js = $sd1$Platforms$Browser$compile(pars);

    //   { name1, name2, name3, ... } = externals;
    const unpackExterns = 'const { ' + pars.externalValues.map((e) => translateUsr(e.usr)).join(', ') + ' } = externs;';

    const body = `{ ${unpackExterns}
${js}; return ${translateUsr(pars.entryUsr)}; }`;

    const arg = {};
    pars.externalValues.forEach((e) => arg[translateUsr(e.usr)] = e.self.value);

    return [ 'Ok', variantConstructor(Function('externs', body)(arg)) ];
};
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
};const $browser$VirtualDom$CssClass = (($1) => ([
  "CssClass",
  $1,
]));

const $browser$VirtualDom$CssStyle = (($1, $2) => ([
  "CssStyle",
  $1,
  $2,
]));

const $browser$VirtualDom$DomAttribute = (($1, $2) => ([
  "DomAttribute",
  $1,
  $2,
]));

const $browser$VirtualDom$DomNode = (($1) => ([
  "DomNode",
  $1,
]));

const $browser$VirtualDom$DomProperty = (($1, $2) => ([
  "DomProperty",
  $1,
  $2,
]));

const $browser$VirtualDom$Effect = (($1) => ([
  "Effect",
  $1,
]));

const $browser$VirtualDom$ElementNode = (($1, $2, $3) => ([
  "ElementNode",
  $1,
  $2,
  $3,
]));

const $browser$VirtualDom$Event = (($1) => ([
  "Event",
  $1,
]));

const $browser$VirtualDom$Listener = (($1, $2) => ([
  "Listener",
  $1,
  $2,
]));

const $browser$VirtualDom$TextNode = (($1) => ([
  "TextNode",
  $1,
]));

const $core$Array$Array__ = (($1) => ([
  "Array__",
  $1,
]));

const $core$Core$Cons = (($1, $2) => ([
  "Cons",
  $1,
  $2,
]));

const $core$Core$False = ([
  "False",
]);

const $core$Core$Nil = ([
  "Nil",
]);

const $core$Core$None = ([
  "None",
]);

const $core$Core$True = ([
  "True",
]);

const $core$Dict$Black = ([
  "Black",
]);

const $core$Dict$RBEmpty_elm_builtin = ([
  "RBEmpty_elm_builtin",
]);

const $core$Dict$RBNode_elm_builtin = (($1, $2, $3, $4, $5) => ([
  "RBNode_elm_builtin",
  $1,
  $2,
  $3,
  $4,
  $5,
]));

const $core$Dict$Red = ([
  "Red",
]);

const $core$Hash$Hash__ = (($1) => ([
  "Hash__",
  $1,
]));

const $core$Maybe$Just = (($1) => ([
  "Just",
  $1,
]));

const $core$Maybe$Nothing = ([
  "Nothing",
]);

const $core$Result$Err = (($1) => ([
  "Err",
  $1,
]));

const $core$Result$Ok = (($1) => ([
  "Ok",
  $1,
]));

const $core$Self$Value = (($1) => ([
  "Value",
  $1,
]));

const $posix$IO$IO = (($1) => ([
  "IO",
  $1,
]));

const $posix$IO$Never = (($1) => ([
  "Never",
  $1,
]));

const $sd1$Compiler$Error$FormattedText_Decoration = (($1) => ([
  "FormattedText_Decoration",
  $1,
]));

const $sd1$Compiler$Error$FormattedText_Default = (($1) => ([
  "FormattedText_Default",
  $1,
]));

const $sd1$Compiler$Error$FormattedText_Emphasys = (($1) => ([
  "FormattedText_Emphasys",
  $1,
]));

const $sd1$Compiler$Error$FormattedText_Warning = (($1) => ([
  "FormattedText_Warning",
  $1,
]));

const $sd1$Compiler$Error$HighlightBlock = (($1) => ([
  "HighlightBlock",
  $1,
]));

const $sd1$Compiler$Error$HighlightWord = (($1) => ([
  "HighlightWord",
  $1,
]));

const $sd1$Compiler$Error$Nested = (($1) => ([
  "Nested",
  $1,
]));

const $sd1$Compiler$Error$Raw = (($1) => ([
  "Raw",
  $1,
]));

const $sd1$Compiler$Error$Simple = (($1, $2, $3) => ([
  "Simple",
  $1,
  $2,
  $3,
]));

const $sd1$Compiler$Lexer$BlockComment = (($1) => ([
  "BlockComment",
  $1,
]));

const $sd1$Compiler$Lexer$ContentOpeningBlockComment = ([
  "ContentOpeningBlockComment",
]);

const $sd1$Compiler$Lexer$ContentOpeningQuotes_One = ([
  "ContentOpeningQuotes_One",
]);

const $sd1$Compiler$Lexer$ContentOpeningQuotes_Two = ([
  "ContentOpeningQuotes_Two",
]);

const $sd1$Compiler$Lexer$Default = ([
  "Default",
]);

const $sd1$Compiler$Lexer$Dot_One = ([
  "Dot_One",
]);

const $sd1$Compiler$Lexer$Dot_Two = ([
  "Dot_Two",
]);

const $sd1$Compiler$Lexer$Indent = ([
  "Indent",
]);

const $sd1$Compiler$Lexer$LineComment = ([
  "LineComment",
]);

const $sd1$Compiler$Lexer$Mutable = ([
  "Mutable",
]);

const $sd1$Compiler$Lexer$NoTabsOrSpacesYet = ([
  "NoTabsOrSpacesYet",
]);

const $sd1$Compiler$Lexer$NumberLiteral = ([
  "NumberLiteral",
]);

const $sd1$Compiler$Lexer$SingleQuote = (($1) => ([
  "SingleQuote",
  $1,
]));

const $sd1$Compiler$Lexer$Spaces = ([
  "Spaces",
]);

const $sd1$Compiler$Lexer$Squiggles = ([
  "Squiggles",
]);

const $sd1$Compiler$Lexer$Tabs = ([
  "Tabs",
]);

const $sd1$Compiler$Lexer$TripleQuote = (($1) => ([
  "TripleQuote",
  $1,
]));

const $sd1$Compiler$Lexer$Word = (($1) => ([
  "Word",
  $1,
]));

const $sd1$Compiler$MakeEmittable$GenerateName = ([
  "GenerateName",
]);

const $sd1$Compiler$MakeEmittable$NoNamedVariables = ([
  "NoNamedVariables",
]);

const $sd1$Compiler$MakeEmittable$TrivialPattern = (($1, $2) => ([
  "TrivialPattern",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$CanBeCastNo = (($1) => ([
  "CanBeCastNo",
  $1,
]));

const $sd1$Compiler$TypeCheck$CanBeCastYes = ([
  "CanBeCastYes",
]);

const $sd1$Compiler$TypeCheck$Context_Argument = (($1, $2) => ([
  "Context_Argument",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$Context_AttributeName = (($1, $2) => ([
  "Context_AttributeName",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$Context_FnBody = (($1, $2) => ([
  "Context_FnBody",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$Context_FnPar = (($1, $2) => ([
  "Context_FnPar",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$Context_Global = ([
  "Context_Global",
]);

const $sd1$Compiler$TypeCheck$Context_IfCondition = ([
  "Context_IfCondition",
]);

const $sd1$Compiler$TypeCheck$Context_IfFalse = ([
  "Context_IfFalse",
]);

const $sd1$Compiler$TypeCheck$Context_IfTrue = ([
  "Context_IfTrue",
]);

const $sd1$Compiler$TypeCheck$Context_LetInBody = (($1) => ([
  "Context_LetInBody",
  $1,
]));

const $sd1$Compiler$TypeCheck$Context_Module = (($1) => ([
  "Context_Module",
  $1,
]));

const $sd1$Compiler$TypeCheck$Context_TryBranch = ([
  "Context_TryBranch",
]);

const $sd1$Compiler$TypeCheck$Equality = (($1, $2, $3, $4, $5) => ([
  "Equality",
  $1,
  $2,
  $3,
  $4,
  $5,
]));

const $sd1$Compiler$TypeCheck$ErrorCallingANonFunction = (($1) => ([
  "ErrorCallingANonFunction",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorCircularAlias = (($1) => ([
  "ErrorCircularAlias",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorCircularValue = ([
  "ErrorCircularValue",
]);

const $sd1$Compiler$TypeCheck$ErrorConstructorNotFound = (($1) => ([
  "ErrorConstructorNotFound",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorIncompatiblePattern = (($1, $2) => ([
  "ErrorIncompatiblePattern",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$ErrorIncompatibleRecycling = ([
  "ErrorIncompatibleRecycling",
]);

const $sd1$Compiler$TypeCheck$ErrorIncompatibleTypes = (($1, $2) => ([
  "ErrorIncompatibleTypes",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$ErrorNamedTypeNotFound = (($1) => ([
  "ErrorNamedTypeNotFound",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorNotCompatibleWithRecord = ([
  "ErrorNotCompatibleWithRecord",
]);

const $sd1$Compiler$TypeCheck$ErrorNotEnoughArguments = ([
  "ErrorNotEnoughArguments",
]);

const $sd1$Compiler$TypeCheck$ErrorRecordDoesNotHaveAttribute = (($1) => ([
  "ErrorRecordDoesNotHaveAttribute",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorRecordHasAttributesNotInAnnotation = ([
  "ErrorRecordHasAttributesNotInAnnotation",
]);

const $sd1$Compiler$TypeCheck$ErrorRecordIsMissingAttibutesInAnnotation = ([
  "ErrorRecordIsMissingAttibutesInAnnotation",
]);

const $sd1$Compiler$TypeCheck$ErrorRecyclingDoesNotMatch = ([
  "ErrorRecyclingDoesNotMatch",
]);

const $sd1$Compiler$TypeCheck$ErrorShouldBeUnique = ([
  "ErrorShouldBeUnique",
]);

const $sd1$Compiler$TypeCheck$ErrorTryingToAccessAttributeOfNonRecord = (($1, $2) => ([
  "ErrorTryingToAccessAttributeOfNonRecord",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$ErrorTypeAllowsFunctions = (($1, $2, $3) => ([
  "ErrorTypeAllowsFunctions",
  $1,
  $2,
  $3,
]));

const $sd1$Compiler$TypeCheck$ErrorUndefinedTypeVariable = (($1) => ([
  "ErrorUndefinedTypeVariable",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorUniInRecordAttribute = (($1) => ([
  "ErrorUniInRecordAttribute",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorUniInTypeArg = ([
  "ErrorUniInTypeArg",
]);

const $sd1$Compiler$TypeCheck$ErrorUniqueGlobal = ([
  "ErrorUniqueGlobal",
]);

const $sd1$Compiler$TypeCheck$ErrorUniquenessDoesNotMatch = (($1) => ([
  "ErrorUniquenessDoesNotMatch",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorUniquenessDoesNotMatchArgument = ([
  "ErrorUniquenessDoesNotMatchArgument",
]);

const $sd1$Compiler$TypeCheck$ErrorUniquenessDoesNotMatchParameter = (($1, $2) => ([
  "ErrorUniquenessDoesNotMatchParameter",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$ErrorUnresolvableUniqueness = (($1, $2) => ([
  "ErrorUnresolvableUniqueness",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$ErrorVariableNotFound = (($1) => ([
  "ErrorVariableNotFound",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorWrongNumberOfArguments = (($1) => ([
  "ErrorWrongNumberOfArguments",
  $1,
]));

const $sd1$Compiler$TypeCheck$ErrorWrongNumberOfConstructorArguments = ([
  "ErrorWrongNumberOfConstructorArguments",
]);

const $sd1$Compiler$TypeCheck$ErrorWrongNumberOfParameters = ([
  "ErrorWrongNumberOfParameters",
]);

const $sd1$Compiler$TypeCheck$ErrorWrongNumberOfTypeArguments = (($1, $2, $3) => ([
  "ErrorWrongNumberOfTypeArguments",
  $1,
  $2,
  $3,
]));

const $sd1$Compiler$TypeCheck$Why_Annotation = ([
  "Why_Annotation",
]);

const $sd1$Compiler$TypeCheck$Why_Argument = (($1) => ([
  "Why_Argument",
  $1,
]));

const $sd1$Compiler$TypeCheck$Why_Attribute = (($1) => ([
  "Why_Attribute",
  $1,
]));

const $sd1$Compiler$TypeCheck$Why_CalledAsFunction = ([
  "Why_CalledAsFunction",
]);

const $sd1$Compiler$TypeCheck$Why_FunctionInput = (($1, $2) => ([
  "Why_FunctionInput",
  $1,
  $2,
]));

const $sd1$Compiler$TypeCheck$Why_FunctionOutput = (($1) => ([
  "Why_FunctionOutput",
  $1,
]));

const $sd1$Compiler$TypeCheck$Why_IfBranches = ([
  "Why_IfBranches",
]);

const $sd1$Compiler$TypeCheck$Why_IfCondition = ([
  "Why_IfCondition",
]);

const $sd1$Compiler$TypeCheck$Why_LetIn = ([
  "Why_LetIn",
]);

const $sd1$Compiler$TypeCheck$Why_Record = ([
  "Why_Record",
]);

const $sd1$Compiler$TypeCheck$Why_RecordAccess = ([
  "Why_RecordAccess",
]);

const $sd1$Compiler$TypeCheck$Why_RecordExt = ([
  "Why_RecordExt",
]);

const $sd1$Compiler$TypeCheck$Why_ReturnType = ([
  "Why_ReturnType",
]);

const $sd1$Compiler$TypeCheck$Why_Todo = ([
  "Why_Todo",
]);

const $sd1$Compiler$TypeCheck$Why_TryExpression = ([
  "Why_TryExpression",
]);

const $sd1$Compiler$TypeCheck$Why_TryPattern = ([
  "Why_TryPattern",
]);

const $sd1$Compiler$TypeCheck$Why_TypeArgument = (($1, $2, $3) => ([
  "Why_TypeArgument",
  $1,
  $2,
  $3,
]));

const $sd1$Compiler$UniquenessCheck$Available = ([
  "Available",
]);

const $sd1$Compiler$UniquenessCheck$ConsumedAt = (($1) => ([
  "ConsumedAt",
  $1,
]));

const $sd1$Compiler$UniquenessCheck$Immutable = ([
  "Immutable",
]);

const $sd1$Compiler$UniquenessCheck$Unique = (($1) => ([
  "Unique",
  $1,
]));

const $sd1$Human$Type$Block = (($1, $2) => ([
  "Block",
  $1,
  $2,
]));

const $sd1$Human$Type$IndentedWithHeader = (($1) => ([
  "IndentedWithHeader",
  $1,
]));

const $sd1$Human$Type$NotIndented = ([
  "NotIndented",
]);

const $sd1$Human$Type$Span = (($1, $2) => ([
  "Span",
  $1,
  $2,
]));

const $sd1$Main$Compile = (($1) => ([
  "Compile",
  $1,
]));

const $sd1$Main$Help = ([
  "Help",
]);

const $sd1$Main$Selftest = ([
  "Selftest",
]);

const $sd1$ModulesFile$Dir = (($1) => ([
  "Dir",
  $1,
]));

const $sd1$ModulesFile$Lib = (($1) => ([
  "Lib",
  $1,
]));

const $sd1$SPLib$Parser$Aborted = (($1, $2) => ([
  "Aborted",
  $1,
  $2,
]));

const $sd1$SPLib$Parser$Accepted = (($1, $2) => ([
  "Accepted",
  $1,
  $2,
]));

const $sd1$SPLib$Parser$Rejected = ([
  "Rejected",
]);

const $sd1$SPON$Accepted = (($1, $2) => ([
  "Accepted",
  $1,
  $2,
]));

const $sd1$SPON$Failed = (($1) => ([
  "Failed",
  $1,
]));

const $sd1$SPON$Rejected = (($1) => ([
  "Rejected",
  $1,
]));

const $sd1$Targets$Javascript$EmittableToJs$Block = (($1) => ([
  "Block",
  $1,
]));

const $sd1$Targets$Javascript$EmittableToJs$Inline = (($1) => ([
  "Inline",
  $1,
]));

const $sd1$Targets$Javascript$EmittableToJs$Override = (($1) => ([
  "Override",
  $1,
]));

const $sd1$Test$CodeExpectation = (($1) => ([
  "CodeExpectation",
  $1,
]));

const $sd1$Test$Error = (($1) => ([
  "Error",
  $1,
]));

const $sd1$Test$Group = (($1, $2) => ([
  "Group",
  $1,
  $2,
]));

const $sd1$Test$NotNow = (($1) => ([
  "NotNow",
  $1,
]));

const $sd1$Test$Single = (($1, $2, $3) => ([
  "Single",
  $1,
  $2,
  $3,
]));

const $sd1$Test$Skipped = ([
  "Skipped",
]);

const $sd1$Test$Success = ([
  "Success",
]);

const $sd1$Types$Ast$Depends = (($1) => ([
  "Depends",
  $1,
]));

const $sd1$Types$Ast$Imm = ([
  "Imm",
]);

const $sd1$Types$Ast$RefGlobal = (($1) => ([
  "RefGlobal",
  $1,
]));

const $sd1$Types$Ast$RefLocal = (($1) => ([
  "RefLocal",
  $1,
]));

const $sd1$Types$Ast$Uni = ([
  "Uni",
]);

const $sd1$Types$CanonicalAst$ArgumentExpression = (($1) => ([
  "ArgumentExpression",
  $1,
]));

const $sd1$Types$CanonicalAst$ArgumentRecycle = (($1, $2, $3) => ([
  "ArgumentRecycle",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$Call = (($1, $2, $3) => ([
  "Call",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$Complete = ([
  "Complete",
]);

const $sd1$Types$CanonicalAst$Constructor = (($1, $2) => ([
  "Constructor",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$DestroyIn = (($1, $2) => ([
  "DestroyIn",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$Fn = (($1, $2, $3) => ([
  "Fn",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$If = (($1, $2) => ([
  "If",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$LetIn = (($1, $2) => ([
  "LetIn",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$LiteralNumber = (($1, $2) => ([
  "LiteralNumber",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$LiteralText = (($1, $2) => ([
  "LiteralText",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$ParRe = (($1) => ([
  "ParRe",
  $1,
]));

const $sd1$Types$CanonicalAst$ParSp = (($1) => ([
  "ParSp",
  $1,
]));

const $sd1$Types$CanonicalAst$ParameterPattern = (($1, $2) => ([
  "ParameterPattern",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$ParameterPlaceholder = (($1, $2) => ([
  "ParameterPlaceholder",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$ParameterRecycle = (($1, $2) => ([
  "ParameterRecycle",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$Partial = ([
  "Partial",
]);

const $sd1$Types$CanonicalAst$PatternAny = (($1, $2) => ([
  "PatternAny",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$PatternConstructor = (($1, $2, $3) => ([
  "PatternConstructor",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$PatternLiteralNumber = (($1, $2) => ([
  "PatternLiteralNumber",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$PatternLiteralText = (($1, $2) => ([
  "PatternLiteralText",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$PatternRecord = (($1, $2, $3) => ([
  "PatternRecord",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$Record = (($1, $2, $3) => ([
  "Record",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$RecordAccess = (($1, $2, $3) => ([
  "RecordAccess",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$Try = (($1, $2) => ([
  "Try",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$TypeAnnotationVariable = (($1, $2) => ([
  "TypeAnnotationVariable",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$TypeError = (($1) => ([
  "TypeError",
  $1,
]));

const $sd1$Types$CanonicalAst$TypeFn = (($1, $2, $3) => ([
  "TypeFn",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$TypeNamed = (($1, $2, $3) => ([
  "TypeNamed",
  $1,
  $2,
  $3,
]));

const $sd1$Types$CanonicalAst$TypeRecord = (($1, $2) => ([
  "TypeRecord",
  $1,
  $2,
]));

const $sd1$Types$CanonicalAst$Variable = (($1, $2) => ([
  "Variable",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$And = (($1) => ([
  "And",
  $1,
]));

const $sd1$Types$EmittableAst$ArgumentRecycle = (($1, $2, $3) => ([
  "ArgumentRecycle",
  $1,
  $2,
  $3,
]));

const $sd1$Types$EmittableAst$ArgumentSpend = (($1, $2) => ([
  "ArgumentSpend",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$ArrayAccess = (($1, $2) => ([
  "ArrayAccess",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$Call = (($1, $2) => ([
  "Call",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$Conditional = (($1, $2, $3) => ([
  "Conditional",
  $1,
  $2,
  $3,
]));

const $sd1$Types$EmittableAst$Constructor = (($1) => ([
  "Constructor",
  $1,
]));

const $sd1$Types$EmittableAst$ConstructorAccess = (($1, $2) => ([
  "ConstructorAccess",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$Fn = (($1, $2) => ([
  "Fn",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$IsConstructor = (($1, $2) => ([
  "IsConstructor",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$LetIn = (($1) => ([
  "LetIn",
  $1,
]));

const $sd1$Types$EmittableAst$LiteralArray = (($1) => ([
  "LiteralArray",
  $1,
]));

const $sd1$Types$EmittableAst$LiteralNumber = (($1) => ([
  "LiteralNumber",
  $1,
]));

const $sd1$Types$EmittableAst$LiteralRecord = (($1, $2) => ([
  "LiteralRecord",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$LiteralText = (($1) => ([
  "LiteralText",
  $1,
]));

const $sd1$Types$EmittableAst$MissingPattern = (($1, $2) => ([
  "MissingPattern",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$RecordAccess = (($1, $2) => ([
  "RecordAccess",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$ShallowEqual = (($1, $2) => ([
  "ShallowEqual",
  $1,
  $2,
]));

const $sd1$Types$EmittableAst$Variable = (($1) => ([
  "Variable",
  $1,
]));

const $sd1$Types$FormattableAst$AliasDef = (($1) => ([
  "AliasDef",
  $1,
]));

const $sd1$Types$FormattableAst$ArgumentPlaceholder = ([
  "ArgumentPlaceholder",
]);

const $sd1$Types$FormattableAst$Binop = (($1, $2) => ([
  "Binop",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$Call = (($1, $2) => ([
  "Call",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$Evaluation = (($1) => ([
  "Evaluation",
  $1,
]));

const $sd1$Types$FormattableAst$Expression = (($1, $2) => ([
  "Expression",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$Fn = (($1, $2) => ([
  "Fn",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$If = (($1) => ([
  "If",
  $1,
]));

const $sd1$Types$FormattableAst$List = (($1) => ([
  "List",
  $1,
]));

const $sd1$Types$FormattableAst$LiteralNumber = (($1, $2) => ([
  "LiteralNumber",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$LiteralText = (($1) => ([
  "LiteralText",
  $1,
]));

const $sd1$Types$FormattableAst$Poly = (($1, $2) => ([
  "Poly",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$Record = (($1) => ([
  "Record",
  $1,
]));

const $sd1$Types$FormattableAst$Statements = (($1) => ([
  "Statements",
  $1,
]));

const $sd1$Types$FormattableAst$Try = (($1) => ([
  "Try",
  $1,
]));

const $sd1$Types$FormattableAst$UnionDef = (($1) => ([
  "UnionDef",
  $1,
]));

const $sd1$Types$FormattableAst$Unop = (($1, $2) => ([
  "Unop",
  $1,
  $2,
]));

const $sd1$Types$FormattableAst$ValueDef = (($1) => ([
  "ValueDef",
  $1,
]));

const $sd1$Types$FormattableAst$Variable = (($1) => ([
  "Variable",
  $1,
]));

const $sd1$Types$JavascriptAst$AccessWithBrackets = (($1, $2) => ([
  "AccessWithBrackets",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$AccessWithDot = (($1, $2) => ([
  "AccessWithDot",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$Array = (($1) => ([
  "Array",
  $1,
]));

const $sd1$Types$JavascriptAst$Binop = (($1, $2, $3) => ([
  "Binop",
  $1,
  $2,
  $3,
]));

const $sd1$Types$JavascriptAst$BlockLambda = (($1, $2) => ([
  "BlockLambda",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$Call = (($1, $2) => ([
  "Call",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$Comma = (($1) => ([
  "Comma",
  $1,
]));

const $sd1$Types$JavascriptAst$Conditional = (($1, $2, $3) => ([
  "Conditional",
  $1,
  $2,
  $3,
]));

const $sd1$Types$JavascriptAst$Define = (($1, $2, $3) => ([
  "Define",
  $1,
  $2,
  $3,
]));

const $sd1$Types$JavascriptAst$Eval = (($1) => ([
  "Eval",
  $1,
]));

const $sd1$Types$JavascriptAst$If = (($1, $2) => ([
  "If",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$Literal = (($1) => ([
  "Literal",
  $1,
]));

const $sd1$Types$JavascriptAst$Mutop = (($1, $2, $3, $4) => ([
  "Mutop",
  $1,
  $2,
  $3,
  $4,
]));

const $sd1$Types$JavascriptAst$Record = (($1) => ([
  "Record",
  $1,
]));

const $sd1$Types$JavascriptAst$Return = (($1) => ([
  "Return",
  $1,
]));

const $sd1$Types$JavascriptAst$SimpleLambda = (($1, $2) => ([
  "SimpleLambda",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$Unop = (($1, $2) => ([
  "Unop",
  $1,
  $2,
]));

const $sd1$Types$JavascriptAst$Var = (($1) => ([
  "Var",
  $1,
]));

const $sd1$Types$Meta$Browser = ([
  "Browser",
]);

const $sd1$Types$Meta$Core = ([
  "Core",
]);

const $sd1$Types$Meta$Posix = ([
  "Posix",
]);

const $sd1$Types$Meta$SourceDirId = (($1) => ([
  "SourceDirId",
  $1,
]));

const $sd1$Types$Meta$UMR = (($1, $2) => ([
  "UMR",
  $1,
  $2,
]));

const $sd1$Types$Meta$USR = (($1, $2) => ([
  "USR",
  $1,
  $2,
]));

const $sd1$Types$Op$Addittive = ([
  "Addittive",
]);

const $sd1$Types$Op$Comparison = ([
  "Comparison",
]);

const $sd1$Types$Op$Cons = ([
  "Cons",
]);

const $sd1$Types$Op$Exponential = ([
  "Exponential",
]);

const $sd1$Types$Op$Left = ([
  "Left",
]);

const $sd1$Types$Op$Logical = ([
  "Logical",
]);

const $sd1$Types$Op$Multiplicative = ([
  "Multiplicative",
]);

const $sd1$Types$Op$Mutop = ([
  "Mutop",
]);

const $sd1$Types$Op$NonAssociative = ([
  "NonAssociative",
]);

const $sd1$Types$Op$Pipe = ([
  "Pipe",
]);

const $sd1$Types$Op$Right = ([
  "Right",
]);

const $sd1$Types$Op$Tuple = ([
  "Tuple",
]);

const $sd1$Types$Op$UnopMinus = ([
  "UnopMinus",
]);

const $sd1$Types$Op$UnopPlus = ([
  "UnopPlus",
]);

const $sd1$Types$Op$UnopRecycle = ([
  "UnopRecycle",
]);

const $sd1$Types$Op$UnopUnique = ([
  "UnopUnique",
]);

const $sd1$Types$Pos$At = (($1, $2) => ([
  "At",
  $1,
  $2,
]));

const $sd1$Types$Pos$End = ([
  "End",
]);

const $sd1$Types$Pos$G = ([
  "G",
]);

const $sd1$Types$Pos$I = (($1) => ([
  "I",
  $1,
]));

const $sd1$Types$Pos$M = ([
  "M",
]);

const $sd1$Types$Pos$N = ([
  "N",
]);

const $sd1$Types$Pos$P = (($1, $2) => ([
  "P",
  $1,
  $2,
]));

const $sd1$Types$Pos$S = ([
  "S",
]);

const $sd1$Types$Pos$T = ([
  "T",
]);

const $sd1$Types$Token$Af = (($1, $2) => ([
  "Af",
  $1,
  $2,
]));

const $sd1$Types$Token$ArgumentPlaceholder = ([
  "ArgumentPlaceholder",
]);

const $sd1$Types$Token$As = ([
  "As",
]);

const $sd1$Types$Token$BadIndent = ([
  "BadIndent",
]);

const $sd1$Types$Token$Be = (($1, $2) => ([
  "Be",
  $1,
  $2,
]));

const $sd1$Types$Token$Binop = (($1) => ([
  "Binop",
  $1,
]));

const $sd1$Types$Token$BlockEnd = ([
  "BlockEnd",
]);

const $sd1$Types$Token$BlockStart = ([
  "BlockStart",
]);

const $sd1$Types$Token$Closed = ([
  "Closed",
]);

const $sd1$Types$Token$Colon = ([
  "Colon",
]);

const $sd1$Types$Token$Comma = ([
  "Comma",
]);

const $sd1$Types$Token$CurlyBrace = (($1) => ([
  "CurlyBrace",
  $1,
]));

const $sd1$Types$Token$Defop = ([
  "Defop",
]);

const $sd1$Types$Token$Else = ([
  "Else",
]);

const $sd1$Types$Token$Fn = ([
  "Fn",
]);

const $sd1$Types$Token$If = ([
  "If",
]);

const $sd1$Types$Token$N = ([
  "N",
]);

const $sd1$Types$Token$NameNoModifier = ([
  "NameNoModifier",
]);

const $sd1$Types$Token$NameStartsWithDot = ([
  "NameStartsWithDot",
]);

const $sd1$Types$Token$NewSiblingLine = ([
  "NewSiblingLine",
]);

const $sd1$Types$Token$NumberLiteral = (($1, $2) => ([
  "NumberLiteral",
  $1,
  $2,
]));

const $sd1$Types$Token$Open = ([
  "Open",
]);

const $sd1$Types$Token$RoundParen = (($1) => ([
  "RoundParen",
  $1,
]));

const $sd1$Types$Token$SquareBracket = (($1) => ([
  "SquareBracket",
  $1,
]));

const $sd1$Types$Token$TextLiteral = (($1) => ([
  "TextLiteral",
  $1,
]));

const $sd1$Types$Token$Then = ([
  "Then",
]);

const $sd1$Types$Token$ThreeDots = ([
  "ThreeDots",
]);

const $sd1$Types$Token$Token = (($1, $2, $3, $4) => ([
  "Token",
  $1,
  $2,
  $3,
  $4,
]));

const $sd1$Types$Token$Try = ([
  "Try",
]);

const $sd1$Types$Token$UniquenessPolymorphismBinop = ([
  "UniquenessPolymorphismBinop",
]);

const $sd1$Types$Token$Unop = (($1) => ([
  "Unop",
  $1,
]));

const $sd1$Types$Token$With = ([
  "With",
]);

const $sd1$Types$Token$Word = (($1) => ([
  "Word",
  $1,
]));

const $sd1$Types$TypedAst$ArgumentExpression = (($1, $2) => ([
  "ArgumentExpression",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$ArgumentRecycle = (($1, $2, $3, $4) => ([
  "ArgumentRecycle",
  $1,
  $2,
  $3,
  $4,
]));

const $sd1$Types$TypedAst$Call = (($1, $2, $3) => ([
  "Call",
  $1,
  $2,
  $3,
]));

const $sd1$Types$TypedAst$Constructor = (($1, $2) => ([
  "Constructor",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$DestroyIn = (($1, $2) => ([
  "DestroyIn",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$Error = (($1) => ([
  "Error",
  $1,
]));

const $sd1$Types$TypedAst$Fn = (($1, $2, $3, $4) => ([
  "Fn",
  $1,
  $2,
  $3,
  $4,
]));

const $sd1$Types$TypedAst$If = (($1, $2) => ([
  "If",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$LetIn = (($1, $2, $3) => ([
  "LetIn",
  $1,
  $2,
  $3,
]));

const $sd1$Types$TypedAst$LiteralNumber = (($1, $2) => ([
  "LiteralNumber",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$LiteralText = (($1, $2) => ([
  "LiteralText",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$ParRe = (($1) => ([
  "ParRe",
  $1,
]));

const $sd1$Types$TypedAst$ParSp = (($1) => ([
  "ParSp",
  $1,
]));

const $sd1$Types$TypedAst$ParameterPattern = (($1, $2) => ([
  "ParameterPattern",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$ParameterRecycle = (($1, $2, $3) => ([
  "ParameterRecycle",
  $1,
  $2,
  $3,
]));

const $sd1$Types$TypedAst$PatternAny = (($1, $2) => ([
  "PatternAny",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$PatternConstructor = (($1, $2, $3) => ([
  "PatternConstructor",
  $1,
  $2,
  $3,
]));

const $sd1$Types$TypedAst$PatternLiteralNumber = (($1, $2) => ([
  "PatternLiteralNumber",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$PatternLiteralText = (($1, $2) => ([
  "PatternLiteralText",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$PatternRecord = (($1, $2) => ([
  "PatternRecord",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$Record = (($1, $2, $3) => ([
  "Record",
  $1,
  $2,
  $3,
]));

const $sd1$Types$TypedAst$RecordAccess = (($1, $2, $3) => ([
  "RecordAccess",
  $1,
  $2,
  $3,
]));

const $sd1$Types$TypedAst$Try = (($1, $2) => ([
  "Try",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$TypeError = ([
  "TypeError",
]);

const $sd1$Types$TypedAst$TypeExact = (($1, $2) => ([
  "TypeExact",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$TypeFn = (($1, $2) => ([
  "TypeFn",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$TypeRecord = (($1, $2) => ([
  "TypeRecord",
  $1,
  $2,
]));

const $sd1$Types$TypedAst$TypeVar = (($1) => ([
  "TypeVar",
  $1,
]));

const $sd1$Types$TypedAst$Variable = (($1, $2) => ([
  "Variable",
  $1,
  $2,
]));

const $sd3$App$OnEmbeddedInput = (($1) => ([
  "OnEmbeddedInput",
  $1,
]));

const $sd3$App$OnInput = (($1) => ([
  "OnInput",
  $1,
]));

const $sd3$App$OnMouseLeave = ([
  "OnMouseLeave",
]);

const $sd3$App$OnMouseMove = (($1, $2) => ([
  "OnMouseMove",
  $1,
  $2,
]));

const $sd3$App$OnScroll = (($1, $2) => ([
  "OnScroll",
  $1,
  $2,
]));

const $sd3$CompileText$CompiledHtml = (($1) => ([
  "CompiledHtml",
  $1,
]));

const $sd3$CompileText$CompiledNumber = (($1) => ([
  "CompiledNumber",
  $1,
]));

const $sd3$CompileText$CompiledShader = (($1) => ([
  "CompiledShader",
  $1,
]));

const $sd3$CompileText$CompiledText = (($1) => ([
  "CompiledText",
  $1,
]));

const $browser$Html$a = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("a", $1, $2);
});

const $browser$Html$alt = (($1) => {
  return ($browser$VirtualDom$DomAttribute)("alt", $1);
});

const $browser$Html$ariaHidden = (($s) => {
  return ($browser$VirtualDom$DomAttribute)("aria-hidden", ($s
    ? "true"
    : "false"));
});

const $browser$Html$button = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("button", $1, $2);
});

const $browser$Html$canvas = (($1) => {
  return ($browser$VirtualDom$ElementNode)("canvas", $1, $core$Core$Nil);
});

const $browser$Html$class = $browser$VirtualDom$CssClass;

const $browser$Html$code = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("code", $1, $2);
});

const $browser$Html$div = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("div", $1, $2);
});

const $browser$Html$h1 = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("h1", $1, $2);
});

const $browser$Html$height = (($1) => {
  return ($browser$VirtualDom$DomAttribute)("height", $1);
});

const $browser$Html$href = (($1) => {
  return ($browser$VirtualDom$DomAttribute)("href", $1);
});

const $browser$Html$id = (($1) => {
  return ($browser$VirtualDom$DomAttribute)("id", $1);
});

const $browser$Html$img = (($1) => {
  return ($browser$VirtualDom$ElementNode)("img", $1, $core$Core$Nil);
});

const $browser$Html$input = (($1) => {
  return ($browser$VirtualDom$ElementNode)("input", $1, $core$Core$Nil);
});

const $core$Result$map = (($f, $result) => {
  return ((($result)[0] === "Err")
    ? ((() => {
      const $e = ($result)[1];
      return ($core$Result$Err)($e);
    }))()
    : ((($result)[0] === "Ok")
      ? ((() => {
        const $a = ($result)[1];
        return ($core$Result$Ok)(($f)($a));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 10:4', (sp_toHuman)($result))));
});

const $browser$VirtualDom$mapAttr = (($f, $a) => {
  return ((($a)[0] === "Listener")
    ? ((() => {
      const $n = ($a)[1];
      const $handler = ($a)[2];
      return ($browser$VirtualDom$Listener)($n, (($ev) => {
        return ($core$Result$map)($f, ($handler)($ev));
      }));
    }))()
    : (true
      ? $a
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 57:4', (sp_toHuman)($a))));
});

const $core$List$for = (($init, $aList, $function) => {
  return ((($aList)[0] === "Nil")
    ? $init
    : ((($aList)[0] === "Cons")
      ? ((() => {
        const $h = ($aList)[1];
        const $tail = ($aList)[2];
        return ($core$List$for)(($function)($h, $init), $tail, $function);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 62:4', (sp_toHuman)($aList))));
});

const $core$List$reverse = (($aList) => {
  return ($core$List$for)($core$Core$Nil, $aList, $core$Core$Cons);
});

const $core$List$forReversed = (($init, $list, $f) => {
  const $foldrHelper = (($acc, $ctr, $ls) => {
    return ((($ls)[0] === "Nil")
      ? $acc
      : ((($ls)[0] === "Cons")
        ? ((() => {
          const $a = ($ls)[1];
          const $r1 = ($ls)[2];
          return ((($r1)[0] === "Nil")
            ? ($f)($a, $acc)
            : ((($r1)[0] === "Cons")
              ? ((() => {
                const $b = ($r1)[1];
                const $r2 = ($r1)[2];
                return ((($r2)[0] === "Nil")
                  ? ($f)($a, ($f)($b, $acc))
                  : ((($r2)[0] === "Cons")
                    ? ((() => {
                      const $c = ($r2)[1];
                      const $r3 = ($r2)[2];
                      return ((($r3)[0] === "Nil")
                        ? ($f)($a, ($f)($b, ($f)($c, $acc)))
                        : ((($r3)[0] === "Cons")
                          ? ((() => {
                            const $d = ($r3)[1];
                            const $r4 = ($r3)[2];
                            const $res = (($ctr > 500)
                              ? ($core$List$for)($acc, ($core$List$reverse)($r4), $f)
                              : ($foldrHelper)($acc, ($ctr + 1), $r4));
                            return ($f)($a, ($f)($b, ($f)($c, ($f)($d, $res))));
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 96:32', (sp_toHuman)($r3))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 91:24', (sp_toHuman)($r2))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 86:16', (sp_toHuman)($r1))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 81:8', (sp_toHuman)($ls))));
  });
  return ($foldrHelper)($init, 0, $list);
});

const $core$List$map = (($f, $list) => {
  return ($core$List$forReversed)($core$Core$Nil, $list, (($x, $acc) => {
    return (sp_cons)(($f)($x), $acc);
  }));
});

const $browser$VirtualDom$map = (($f, $n) => {
  return ((($n)[0] === "TextNode")
    ? ((() => {
      const $t = ($n)[1];
      return ($browser$VirtualDom$TextNode)($t);
    }))()
    : ((($n)[0] === "ElementNode")
      ? ((() => {
        const $name = ($n)[1];
        const $attrs = ($n)[2];
        const $children = ($n)[3];
        return ($browser$VirtualDom$ElementNode)($name, ($core$List$map)((($1) => {
          return ($browser$VirtualDom$mapAttr)($f, $1);
        }), $attrs), ($core$List$map)((($1) => {
          return ($browser$VirtualDom$map)($f, $1);
        }), $children));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 49:4', (sp_toHuman)($n))));
});

const $browser$Html$map = $browser$VirtualDom$map;

const $browser$Html$none = ($browser$VirtualDom$TextNode)("");

const $browser$Html$on = (($0, $1) => {
  return ($browser$VirtualDom$Listener)($0, $1);
});

const $browser$Html$onClick = (($msg) => {
  return ($browser$Html$on)("click", (($event) => {
    return ($core$Result$Ok)($msg);
  }));
});

const $browser$Html$onInput = (($textToMsg) => {
  return ($browser$Html$on)("input", (($e) => {
    return ((($1) => {
      return ($core$Result$map)($textToMsg, $1);
    }))((virtualDom_eventToText)(($core$Core$Cons)("target", ($core$Core$Cons)("value", $core$Core$Nil)), $e));
  }));
});

const $browser$Html$pre = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("pre", $1, $2);
});

const $browser$Html$span = (($1, $2) => {
  return ($browser$VirtualDom$ElementNode)("span", $1, $2);
});

const $browser$Html$spellcheck = (($s) => {
  return ($browser$VirtualDom$DomAttribute)("spellcheck", ($s
    ? "true"
    : "false"));
});

const $browser$Html$src = (($1) => {
  return ($browser$VirtualDom$DomAttribute)("src", $1);
});

const $browser$Html$style = $browser$VirtualDom$CssStyle;

const $browser$Html$text = (($0) => {
  return ($browser$VirtualDom$TextNode)($0);
});

const $browser$Html$textarea = (($attrs, $content) => {
  return ($browser$VirtualDom$ElementNode)("textarea", $attrs, ($core$Core$Cons)(($browser$VirtualDom$TextNode)($content), $core$Core$Nil));
});

const $browser$Html$value = (($1) => {
  return ($browser$VirtualDom$DomProperty)("value", $1);
});

const $browser$Html$width = (($1) => {
  return ($browser$VirtualDom$DomAttribute)("width", $1);
});

const $core$List$each = (($ls, $f) => {
  return ((($ls)[0] === "Nil")
    ? null
    : ((($ls)[0] === "Cons")
      ? ((() => {
        const $h = ($ls)[1];
        const $tail = ($ls)[2];
        ($f)($h);
        return ($core$List$each)($tail, $f);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 341:4', (sp_toHuman)($ls))));
});

const $browser$VirtualDom$updateDomAttrs = (($new, $old, $domNode) => {
  let $oldClass = "";
  let $oldStyle = "";
  let $oldDomAttrs = (hash_fromList)($core$Core$Nil);
  ($core$List$each)($old, (($a) => {
    return ((($a)[0] === "CssClass")
      ? ((() => {
        const $value = ($a)[1];
        return ($oldClass = (((__re__ = (basics_cloneUni)($oldClass)), ($oldClass = (__re__)[1]), (__re__)[0]) + (" " + $value)));
      }))()
      : ((($a)[0] === "CssStyle")
        ? ((() => {
          const $key = ($a)[1];
          const $value = ($a)[2];
          return ($oldStyle = (((__re__ = (basics_cloneUni)($oldStyle)), ($oldStyle = (__re__)[1]), (__re__)[0]) + ($key + (": " + ($value + "; ")))));
        }))()
        : ((($a)[0] === "Listener")
          ? ((() => {
            const $eventName = ($a)[1];
            const $eventHandler = ($a)[2];
            return (virtualDom_jsRemoveEventListener)($eventName, $eventHandler, $domNode);
          }))()
          : ((($a)[0] === "DomAttribute")
            ? ((() => {
              const $name = ($a)[1];
              const $value = ($a)[2];
              return ((__re__ = (hash_insert)($oldDomAttrs, $name, $value)), ($oldDomAttrs = (__re__)[1]), (__re__)[0]);
            }))()
            : ((($a)[0] === "DomProperty")
              ? ((() => {
                const $name = ($a)[1];
                const $value = ($a)[2];
                return null;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 156:8', (sp_toHuman)($a)))))));
  }));
  ((sp_not_equal)(((__re__ = (basics_cloneUni)($oldClass)), ($oldClass = (__re__)[1]), (__re__)[0]), "")
    ? ((__re__ = (hash_insert)($oldDomAttrs, "class", ((__re__ = (basics_cloneUni)($oldClass)), ($oldClass = (__re__)[1]), (__re__)[0]))), ($oldDomAttrs = (__re__)[1]), (__re__)[0])
    : null);
  ((sp_not_equal)(((__re__ = (basics_cloneUni)($oldStyle)), ($oldStyle = (__re__)[1]), (__re__)[0]), "")
    ? ((__re__ = (hash_insert)($oldDomAttrs, "style", ((__re__ = (basics_cloneUni)($oldStyle)), ($oldStyle = (__re__)[1]), (__re__)[0]))), ($oldDomAttrs = (__re__)[1]), (__re__)[0])
    : null);
  let $newClass = "";
  let $newStyle = "";
  let $newDomAttrs = (hash_fromList)($core$Core$Nil);
  ($core$List$each)($new, (($a) => {
    return ((($a)[0] === "CssClass")
      ? ((() => {
        const $value = ($a)[1];
        return ($newClass = (((__re__ = (basics_cloneUni)($newClass)), ($newClass = (__re__)[1]), (__re__)[0]) + (" " + $value)));
      }))()
      : ((($a)[0] === "CssStyle")
        ? ((() => {
          const $key = ($a)[1];
          const $value = ($a)[2];
          return ($newStyle = (((__re__ = (basics_cloneUni)($newStyle)), ($newStyle = (__re__)[1]), (__re__)[0]) + ($key + (": " + ($value + "; ")))));
        }))()
        : ((($a)[0] === "Listener")
          ? ((() => {
            const $eventName = ($a)[1];
            const $eventHandler = ($a)[2];
            return (virtualDom_jsAddEventListener)($eventName, $eventHandler, $domNode);
          }))()
          : ((($a)[0] === "DomAttribute")
            ? ((() => {
              const $name = ($a)[1];
              const $value = ($a)[2];
              return ((__re__ = (hash_insert)($newDomAttrs, $name, $value)), ($newDomAttrs = (__re__)[1]), (__re__)[0]);
            }))()
            : ((($a)[0] === "DomProperty")
              ? ((() => {
                const $name = ($a)[1];
                const $value = ($a)[2];
                return (virtualDom_jsSetProperty)($name, $value, $domNode);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 188:8', (sp_toHuman)($a)))))));
  }));
  ((sp_not_equal)(((__re__ = (basics_cloneUni)($newClass)), ($newClass = (__re__)[1]), (__re__)[0]), "")
    ? ((__re__ = (hash_insert)($newDomAttrs, "class", ((__re__ = (basics_cloneUni)($newClass)), ($newClass = (__re__)[1]), (__re__)[0]))), ($newDomAttrs = (__re__)[1]), (__re__)[0])
    : null);
  ((sp_not_equal)(((__re__ = (basics_cloneUni)($newStyle)), ($newStyle = (__re__)[1]), (__re__)[0]), "")
    ? ((__re__ = (hash_insert)($newDomAttrs, "style", ((__re__ = (basics_cloneUni)($newStyle)), ($newStyle = (__re__)[1]), (__re__)[0]))), ($newDomAttrs = (__re__)[1]), (__re__)[0])
    : null);
  ((__re__ = (hash_each)($newDomAttrs, (($newName, $newValue) => {
    const $6 = ((__re__ = (hash_get)($oldDomAttrs, $newName)), ($oldDomAttrs = (__re__)[1]), (__re__)[0]);
    return ((($6)[0] === "Nothing")
      ? (virtualDom_jsSetAttribute)($newName, $newValue, $domNode)
      : ((($6)[0] === "Just")
        ? ((() => {
          const $oldValue = ($6)[1];
          ((__re__ = (hash_remove)($oldDomAttrs, $newName)), ($oldDomAttrs = (__re__)[1]), (__re__)[0]);
          return ((sp_not_equal)($oldValue, $newValue)
            ? (virtualDom_jsSetAttribute)($newName, $newValue, $domNode)
            : null);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 217:8', (sp_toHuman)($6))));
  }))), ($newDomAttrs = (__re__)[1]), (__re__)[0]);
  return ((__re__ = (hash_each)($oldDomAttrs, (($oldName, $oldValue) => {
    const $6 = ((__re__ = (hash_get)($newDomAttrs, $oldName)), ($newDomAttrs = (__re__)[1]), (__re__)[0]);
    return ((($6)[0] === "Nothing")
      ? (virtualDom_jsRemoveAttribute)($oldName, $domNode)
      : ((($6)[0] === "Just")
        ? ((() => {
          const $newValue = ($6)[1];
          return null;
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 231:8', (sp_toHuman)($6))));
  }))), ($oldDomAttrs = (__re__)[1]), (__re__)[0]);
});

const $browser$VirtualDom$renderElementNode = (($tagName, $attrs, $children) => {
  const $domNode = (virtualDom_jsCreateElement)($tagName);
  ($browser$VirtualDom$updateDomAttrs)($attrs, $core$Core$Nil, $domNode);
  ($core$List$each)($children, (($child) => {
    return (virtualDom_jsAppendChild)(({
      child: ($browser$VirtualDom$render)($child),
      parent: $domNode,
    }));
  }));
  return $domNode;
});

const $browser$VirtualDom$render = (($vnode) => {
  return ((($vnode)[0] === "ElementNode")
    ? ((() => {
      const $tagName = ($vnode)[1];
      const $attrs = ($vnode)[2];
      const $children = ($vnode)[3];
      return ($browser$VirtualDom$renderElementNode)($tagName, $attrs, $children);
    }))()
    : ((($vnode)[0] === "TextNode")
      ? ((() => {
        const $content = ($vnode)[1];
        return (virtualDom_jsCreateTextNode)($content);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 242:4', (sp_toHuman)($vnode))));
});

const $browser$VirtualDom$updateDomNode = (($new, $old, $domNode) => {
  const $4 = ({
    first: $new,
    second: $old,
  });
  return (((($4.first)[0] === "ElementNode") && (($4.second)[0] === "ElementNode"))
    ? ((() => {
      const $nTag = ($4.first)[1];
      const $nAttr = ($4.first)[2];
      const $nChildren = ($4.first)[3];
      const $oTag = ($4.second)[1];
      const $oAttr = ($4.second)[2];
      const $oChildren = ($4.second)[3];
      return ((sp_not_equal)($nTag, $oTag)
        ? (virtualDom_jsReplaceWith)(($browser$VirtualDom$renderElementNode)($nTag, $nAttr, $nChildren), $domNode)
        : ((() => {
          ($browser$VirtualDom$updateDomAttrs)($nAttr, $oAttr, $domNode);
          ($browser$VirtualDom$updateDomChildren)($nChildren, $oChildren, 0, $domNode);
          return $domNode;
        }))());
    }))()
    : (((($4.first)[0] === "TextNode") && (($4.second)[0] === "TextNode"))
      ? ((() => {
        const $n = ($4.first)[1];
        const $o = ($4.second)[1];
        return ((sp_equal)($n, $o)
          ? $domNode
          : (virtualDom_jsReplaceWith)((virtualDom_jsCreateTextNode)($n), $domNode));
      }))()
      : (true
        ? (virtualDom_jsReplaceWith)(($browser$VirtualDom$render)($new), $domNode)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 267:4', (sp_toHuman)($4)))));
});

const $browser$VirtualDom$updateDomChildren = (($new, $old, $index, $parentNode) => {
  const $5 = ({
    first: $new,
    second: $old,
  });
  return (((($5.first)[0] === "Cons") && (($5.second)[0] === "Cons"))
    ? ((() => {
      const $n = ($5.first)[1];
      const $ns = ($5.first)[2];
      const $o = ($5.second)[1];
      const $os = ($5.second)[2];
      (virtualDom_setChild)((($2) => {
        return ($browser$VirtualDom$updateDomNode)($n, $o, $2);
      }), $index, $parentNode);
      return ($browser$VirtualDom$updateDomChildren)($ns, $os, ($index + 1), $parentNode);
    }))()
    : (((($5.first)[0] === "Cons") && (($5.second)[0] === "Nil"))
      ? ((() => {
        const $n = ($5.first)[1];
        const $ns = ($5.first)[2];
        return ($core$List$each)($new, (($child) => {
          return (virtualDom_jsAppendChild)(({
            child: ($browser$VirtualDom$render)($child),
            parent: $parentNode,
          }));
        }));
      }))()
      : (((($5.first)[0] === "Nil") && (($5.second)[0] === "Cons"))
        ? ((() => {
          const $o = ($5.second)[1];
          const $os = ($5.second)[2];
          return (virtualDom_removeAllChildrenStartingFromIndex)($index, $parentNode);
        }))()
        : (((($5.first)[0] === "Nil") && (($5.second)[0] === "Nil"))
          ? null
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/browser/VirtualDom.sp 292:4', (sp_toHuman)($5))))));
});

const $sd1$Test$maybeToOutcome = (($m) => {
  return ((($m)[0] === "Just")
    ? ((() => {
      const $e = ($m)[1];
      return ($sd1$Test$Error)($e);
    }))()
    : ((($m)[0] === "Nothing")
      ? $sd1$Test$Success
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 15:4', (sp_toHuman)($m))));
});

const $sd1$Test$valueTest = (($toText, $title, $generateValue, $ce) => {
  const $5 = $ce;
  const $toMaybeError = ($5)[1];
  return ($sd1$Test$Single)($title, "", ((_0) => {
    return ($sd1$Test$maybeToOutcome)(((($1) => {
      return ($toMaybeError)($toText, $1);
    }))(($core$Result$Ok)(($generateValue)(null))));
  }));
});

const $core$Array_Test$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $core$Text$join = (($sep, $listOfText) => {
  return ((($listOfText)[0] === "Nil")
    ? ""
    : ((($listOfText)[0] === "Cons")
      ? ((() => {
        const $head = ($listOfText)[1];
        const $tail = ($listOfText)[2];
        const $rec = (($ls, $acc) => {
          return ((($ls)[0] === "Nil")
            ? $acc
            : ((($ls)[0] === "Cons")
              ? ((() => {
                const $h = ($ls)[1];
                const $t = ($ls)[2];
                return ($rec)($t, ($acc + ($sep + $h)));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Text.sp 136:12', (sp_toHuman)($ls))));
        });
        return ($rec)($tail, $head);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Text.sp 128:4', (sp_toHuman)($listOfText))));
});

const $sd1$Test$isOkAndEqualTo = (($expectedOk) => {
  return ($sd1$Test$CodeExpectation)((($toText, $result) => {
    return ((($result)[0] === "Err")
      ? ((() => {
        const $e = ($result)[1];
        return ($core$Maybe$Just)($e);
      }))()
      : ((($result)[0] === "Ok")
        ? ((() => {
          const $actualOk = ($result)[1];
          return ((sp_equal)($actualOk, $expectedOk)
            ? $core$Maybe$Nothing
            : ($core$Maybe$Just)(((($1) => {
              return ($core$Text$join)("\n", $1);
            }))(($core$Core$Cons)("expected = ", ($core$Core$Cons)(($toText)($expectedOk), ($core$Core$Cons)("", ($core$Core$Cons)("actual = ", ($core$Core$Cons)(($toText)($actualOk), $core$Core$Nil))))))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 82:6', (sp_toHuman)($result))));
  }));
});

const $core$Array_Test$tests = ($sd1$Test$Group)("Array", ($core$Core$Cons)(($core$Array_Test$valueTest)("push", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)("a", $core$Core$Nil));
  ((__re__ = (array_push)($a, "b")), ($a = (__re__)[1]), (__re__)[0]);
  ((__re__ = (array_push)($a, "c")), ($a = (__re__)[1]), (__re__)[0]);
  return ((__re__ = (array_toList)($a)), ($a = (__re__)[1]), (__re__)[0]);
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)("a", ($core$Core$Cons)("b", ($core$Core$Cons)("c", $core$Core$Nil))))), ($core$Core$Cons)(($core$Array_Test$valueTest)("pop 1", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)("x", ($core$Core$Cons)("y", ($core$Core$Cons)("z", $core$Core$Nil))));
  const $b = ((__re__ = (array_pop)($a)), ($a = (__re__)[1]), (__re__)[0]);
  const $c = ((__re__ = (array_pop)($a)), ($a = (__re__)[1]), (__re__)[0]);
  const $l = ((__re__ = (array_toList)($a)), ($a = (__re__)[1]), (__re__)[0]);
  return ({
    b: $b,
    c: $c,
    l: $l,
  });
}), ($sd1$Test$isOkAndEqualTo)(({
  b: ($core$Maybe$Just)("z"),
  c: ($core$Maybe$Just)("y"),
  l: ($core$Core$Cons)("x", $core$Core$Nil),
}))), ($core$Core$Cons)(($core$Array_Test$valueTest)("pop empty", ((_0) => {
  let $a = (array_fromList)($core$Core$Nil);
  const $b = ((__re__ = (array_pop)($a)), ($a = (__re__)[1]), (__re__)[0]);
  const $l = ((__re__ = (array_toList)($a)), ($a = (__re__)[1]), (__re__)[0]);
  return ({
    b: $b,
    l: $l,
  });
}), ($sd1$Test$isOkAndEqualTo)(({
  b: $core$Maybe$Nothing,
  l: $core$Core$Nil,
}))), ($core$Core$Cons)(($core$Array_Test$valueTest)("get Just", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)("p", ($core$Core$Cons)("q", $core$Core$Nil)));
  return ((__re__ = (array_get)($a, 1)), ($a = (__re__)[1]), (__re__)[0]);
}), ($sd1$Test$isOkAndEqualTo)(($core$Maybe$Just)("q"))), ($core$Core$Cons)(($core$Array_Test$valueTest)("get Nothing", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)("p", ($core$Core$Cons)("q", $core$Core$Nil)));
  return ((__re__ = (array_get)($a, 3)), ($a = (__re__)[1]), (__re__)[0]);
}), ($sd1$Test$isOkAndEqualTo)($core$Maybe$Nothing)), ($core$Core$Cons)(($core$Array_Test$valueTest)("set success", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)(8, ($core$Core$Cons)(9, $core$Core$Nil)));
  const $r = ((__re__ = (array_set)($a, 0, 10)), ($a = (__re__)[1]), (__re__)[0]);
  const $l = ((__re__ = (array_toList)($a)), ($a = (__re__)[1]), (__re__)[0]);
  return ({
    l: $l,
    r: $r,
  });
}), ($sd1$Test$isOkAndEqualTo)(({
  l: ($core$Core$Cons)(10, ($core$Core$Cons)(9, $core$Core$Nil)),
  r: true,
}))), ($core$Core$Cons)(($core$Array_Test$valueTest)("set fail", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)(8, ($core$Core$Cons)(9, $core$Core$Nil)));
  const $r = ((__re__ = (array_set)($a, 3, 10)), ($a = (__re__)[1]), (__re__)[0]);
  const $l = ((__re__ = (array_toList)($a)), ($a = (__re__)[1]), (__re__)[0]);
  return ({
    l: $l,
    r: $r,
  });
}), ($sd1$Test$isOkAndEqualTo)(({
  l: ($core$Core$Cons)(8, ($core$Core$Cons)(9, $core$Core$Nil)),
  r: false,
}))), ($core$Core$Cons)(($core$Array_Test$valueTest)("sortBy", ((_0) => {
  let $a = (array_fromList)(($core$Core$Cons)(55, ($core$Core$Cons)(99, ($core$Core$Cons)(22, $core$Core$Nil))));
  ((__re__ = (array_sortBy)($a, (($x) => {
    return -($x);
  }))), ($a = (__re__)[1]), (__re__)[0]);
  return ((__re__ = (array_toList)($a)), ($a = (__re__)[1]), (__re__)[0]);
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(99, ($core$Core$Cons)(55, ($core$Core$Cons)(22, $core$Core$Nil))))), $core$Core$Nil)))))))));

const $core$Basics$btw = (($f, $a, $c) => {
  ($f)($a);
  return $c;
});

const $core$Basics$clamp = (($low, $high, $n) => {
  return (($n < $low)
    ? $low
    : (($n > $high)
      ? $high
      : $n));
});

const $core$Basics$identity = (($a) => {
  return $a;
});

const $core$Basics$max = (($a, $b) => {
  return (($a > $b)
    ? $a
    : $b);
});

const $core$Basics$min = (($a, $b) => {
  return (($a < $b)
    ? $a
    : $b);
});

const $core$Basics$not = (($b) => {
  return ($b
    ? false
    : true);
});

const $core$Dict$any = (($f, $dict) => {
  return ((($dict)[0] === "RBNode_elm_builtin")
    ? ((() => {
      const $color = ($dict)[1];
      const $key = ($dict)[2];
      const $v = ($dict)[3];
      const $left = ($dict)[4];
      const $right = ($dict)[5];
      return (($f)($key, $v)
        ? true
        : (($core$Dict$any)($f, $left) || ($core$Dict$any)($f, $right)));
    }))()
    : ((($dict)[0] === "RBEmpty_elm_builtin")
      ? false
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 522:4', (sp_toHuman)($dict))));
});

const $core$Dict$balance = (($color, $key, $value, $left, $right) => {
  return (((($right)[0] === "RBNode_elm_builtin") && ((($right)[1])[0] === "Red"))
    ? ((() => {
      const $rK = ($right)[2];
      const $rV = ($right)[3];
      const $rLeft = ($right)[4];
      const $rRight = ($right)[5];
      return (((($left)[0] === "RBNode_elm_builtin") && ((($left)[1])[0] === "Red"))
        ? ((() => {
          const $lK = ($left)[2];
          const $lV = ($left)[3];
          const $lLeft = ($left)[4];
          const $lRight = ($left)[5];
          return ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $key, $value, ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $lK, $lV, $lLeft, $lRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $rK, $rV, $rLeft, $rRight));
        }))()
        : (true
          ? ($core$Dict$RBNode_elm_builtin)($color, $rK, $rV, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $key, $value, $left, $rLeft), $rRight)
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 113:6', (sp_toHuman)($left))));
    }))()
    : (true
      ? (((($left)[0] === "RBNode_elm_builtin") && (((($left)[1])[0] === "Red") && (((($left)[4])[0] === "RBNode_elm_builtin") && (((($left)[4])[1])[0] === "Red"))))
        ? ((() => {
          const $lK = ($left)[2];
          const $lV = ($left)[3];
          const $llK = (($left)[4])[2];
          const $llV = (($left)[4])[3];
          const $llLeft = (($left)[4])[4];
          const $llRight = (($left)[4])[5];
          const $lRight = ($left)[5];
          return ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $llK, $llV, $llLeft, $llRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $key, $value, $lRight, $right));
        }))()
        : (true
          ? ($core$Dict$RBNode_elm_builtin)($color, $key, $value, $left, $right)
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 126:6', (sp_toHuman)($left))))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 111:2', (sp_toHuman)($right))));
});

const $core$Dict$for = (($acc, $dict, $func) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? $acc
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        return ($core$Dict$for)(($func)($key, $value, ($core$Dict$for)($acc, $left, $func)), $right, $func);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 465:2', (sp_toHuman)($dict))));
});

const $core$Dict$moveRedLeft = (($dict) => {
  return (((($dict)[0] === "RBNode_elm_builtin") && (((($dict)[4])[0] === "RBNode_elm_builtin") && (((($dict)[5])[0] === "RBNode_elm_builtin") && ((((($dict)[5])[4])[0] === "RBNode_elm_builtin") && ((((($dict)[5])[4])[1])[0] === "Red")))))
    ? ((() => {
      const $clr = ($dict)[1];
      const $k = ($dict)[2];
      const $v = ($dict)[3];
      const $lClr = (($dict)[4])[1];
      const $lK = (($dict)[4])[2];
      const $lV = (($dict)[4])[3];
      const $lLeft = (($dict)[4])[4];
      const $lRight = (($dict)[4])[5];
      const $rClr = (($dict)[5])[1];
      const $rK = (($dict)[5])[2];
      const $rV = (($dict)[5])[3];
      const $rlK = ((($dict)[5])[4])[2];
      const $rlV = ((($dict)[5])[4])[3];
      const $rlL = ((($dict)[5])[4])[4];
      const $rlR = ((($dict)[5])[4])[5];
      const $rRight = (($dict)[5])[5];
      return ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $rlK, $rlV, ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, $lLeft, $lRight), $rlL), ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $rK, $rV, $rlR, $rRight));
    }))()
    : (((($dict)[0] === "RBNode_elm_builtin") && (((($dict)[4])[0] === "RBNode_elm_builtin") && ((($dict)[5])[0] === "RBNode_elm_builtin")))
      ? ((() => {
        const $clr = ($dict)[1];
        const $k = ($dict)[2];
        const $v = ($dict)[3];
        const $lClr = (($dict)[4])[1];
        const $lK = (($dict)[4])[2];
        const $lV = (($dict)[4])[3];
        const $lLeft = (($dict)[4])[4];
        const $lRight = (($dict)[4])[5];
        const $rClr = (($dict)[5])[1];
        const $rK = (($dict)[5])[2];
        const $rV = (($dict)[5])[3];
        const $rLeft = (($dict)[5])[4];
        const $rRight = (($dict)[5])[5];
        return ((($clr)[0] === "Black")
          ? ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, $lLeft, $lRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $rK, $rV, $rLeft, $rRight))
          : ((($clr)[0] === "Red")
            ? ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, $lLeft, $lRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $rK, $rV, $rLeft, $rRight))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 281:6', (sp_toHuman)($clr))));
      }))()
      : (true
        ? $dict
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 271:2', (sp_toHuman)($dict)))));
});

const $core$Dict$getMin = (($dict) => {
  return ((($dict)[0] === "RBNode_elm_builtin")
    ? ((() => {
      const $left = ($dict)[4];
      return ((($left)[0] === "RBNode_elm_builtin")
        ? ($core$Dict$getMin)($left)
        : (true
          ? $dict
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 230:6', (sp_toHuman)($left))));
    }))()
    : (true
      ? $dict
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 228:2', (sp_toHuman)($dict))));
});

const $core$Dict$removeMin = (($dict) => {
  return ((($dict)[0] === "RBNode_elm_builtin")
    ? ((() => {
      const $color = ($dict)[1];
      const $key = ($dict)[2];
      const $value = ($dict)[3];
      const $left = ($dict)[4];
      const $right = ($dict)[5];
      return ((($left)[0] === "RBNode_elm_builtin")
        ? ((() => {
          const $lColor = ($left)[1];
          const $lLeft = ($left)[4];
          return ((($lColor)[0] === "Black")
            ? (((($lLeft)[0] === "RBNode_elm_builtin") && ((($lLeft)[1])[0] === "Red"))
              ? ($core$Dict$RBNode_elm_builtin)($color, $key, $value, ($core$Dict$removeMin)($left), $right)
              : (true
                ? ((() => {
                  const $2 = ($core$Dict$moveRedLeft)($dict);
                  return ((($2)[0] === "RBNode_elm_builtin")
                    ? ((() => {
                      const $nColor = ($2)[1];
                      const $nKey = ($2)[2];
                      const $nValue = ($2)[3];
                      const $nLeft = ($2)[4];
                      const $nRight = ($2)[5];
                      return ($core$Dict$balance)($nColor, $nKey, $nValue, ($core$Dict$removeMin)($nLeft), $nRight);
                    }))()
                    : ((($2)[0] === "RBEmpty_elm_builtin")
                      ? $core$Dict$RBEmpty_elm_builtin
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 253:18', (sp_toHuman)($2))));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 248:14', (sp_toHuman)($lLeft))))
            : (true
              ? ($core$Dict$RBNode_elm_builtin)($color, $key, $value, ($core$Dict$removeMin)($left), $right)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 246:10', (sp_toHuman)($lColor))));
        }))()
        : (true
          ? $core$Dict$RBEmpty_elm_builtin
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 244:6', (sp_toHuman)($left))));
    }))()
    : (true
      ? $core$Dict$RBEmpty_elm_builtin
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 242:2', (sp_toHuman)($dict))));
});

const $core$Dict$removeHelpEQGT = (($targetKey, $dict) => {
  return ((($dict)[0] === "RBNode_elm_builtin")
    ? ((() => {
      const $color = ($dict)[1];
      const $key = ($dict)[2];
      const $value = ($dict)[3];
      const $left = ($dict)[4];
      const $right = ($dict)[5];
      return ((sp_equal)($targetKey, $key)
        ? ((() => {
          const $3 = ($core$Dict$getMin)($right);
          return ((($3)[0] === "RBNode_elm_builtin")
            ? ((() => {
              const $minKey = ($3)[2];
              const $minValue = ($3)[3];
              return ($core$Dict$balance)($color, $minKey, $minValue, $left, ($core$Dict$removeMin)($right));
            }))()
            : ((($3)[0] === "RBEmpty_elm_builtin")
              ? $core$Dict$RBEmpty_elm_builtin
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 213:8', (sp_toHuman)($3))));
        }))()
        : ($core$Dict$balance)($color, $key, $value, $left, ($core$Dict$removeHelp)($targetKey, $right)));
    }))()
    : ((($dict)[0] === "RBEmpty_elm_builtin")
      ? $core$Dict$RBEmpty_elm_builtin
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 210:2', (sp_toHuman)($dict))));
});

const $core$Dict$moveRedRight = (($dict) => {
  return (((($dict)[0] === "RBNode_elm_builtin") && (((($dict)[4])[0] === "RBNode_elm_builtin") && ((((($dict)[4])[4])[0] === "RBNode_elm_builtin") && (((((($dict)[4])[4])[1])[0] === "Red") && ((($dict)[5])[0] === "RBNode_elm_builtin")))))
    ? ((() => {
      const $clr = ($dict)[1];
      const $k = ($dict)[2];
      const $v = ($dict)[3];
      const $lClr = (($dict)[4])[1];
      const $lK = (($dict)[4])[2];
      const $lV = (($dict)[4])[3];
      const $llK = ((($dict)[4])[4])[2];
      const $llV = ((($dict)[4])[4])[3];
      const $llLeft = ((($dict)[4])[4])[4];
      const $llRight = ((($dict)[4])[4])[5];
      const $lRight = (($dict)[4])[5];
      const $rClr = (($dict)[5])[1];
      const $rK = (($dict)[5])[2];
      const $rV = (($dict)[5])[3];
      const $rLeft = (($dict)[5])[4];
      const $rRight = (($dict)[5])[5];
      return ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $llK, $llV, $llLeft, $llRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, $lRight, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $rK, $rV, $rLeft, $rRight)));
    }))()
    : (((($dict)[0] === "RBNode_elm_builtin") && (((($dict)[4])[0] === "RBNode_elm_builtin") && ((($dict)[5])[0] === "RBNode_elm_builtin")))
      ? ((() => {
        const $clr = ($dict)[1];
        const $k = ($dict)[2];
        const $v = ($dict)[3];
        const $lClr = (($dict)[4])[1];
        const $lK = (($dict)[4])[2];
        const $lV = (($dict)[4])[3];
        const $lLeft = (($dict)[4])[4];
        const $lRight = (($dict)[4])[5];
        const $rClr = (($dict)[5])[1];
        const $rK = (($dict)[5])[2];
        const $rV = (($dict)[5])[3];
        const $rLeft = (($dict)[5])[4];
        const $rRight = (($dict)[5])[5];
        return ((($clr)[0] === "Black")
          ? ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, $lLeft, $lRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $rK, $rV, $rLeft, $rRight))
          : ((($clr)[0] === "Red")
            ? ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $lK, $lV, $lLeft, $lRight), ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $rK, $rV, $rLeft, $rRight))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 314:6', (sp_toHuman)($clr))));
      }))()
      : (true
        ? $dict
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 304:2', (sp_toHuman)($dict)))));
});

const $core$Dict$removeHelpPrepEQGT = (($targetKey, $dict, $color, $key, $value, $left, $right) => {
  return (((($left)[0] === "RBNode_elm_builtin") && ((($left)[1])[0] === "Red"))
    ? ((() => {
      const $lK = ($left)[2];
      const $lV = ($left)[3];
      const $lLeft = ($left)[4];
      const $lRight = ($left)[5];
      return ($core$Dict$RBNode_elm_builtin)($color, $lK, $lV, $lLeft, ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $key, $value, $lRight, $right));
    }))()
    : (true
      ? (((($right)[0] === "RBNode_elm_builtin") && (((($right)[1])[0] === "Black") && (((($right)[4])[0] === "RBNode_elm_builtin") && (((($right)[4])[1])[0] === "Black"))))
        ? ($core$Dict$moveRedRight)($dict)
        : (((($right)[0] === "RBNode_elm_builtin") && (((($right)[1])[0] === "Black") && ((($right)[4])[0] === "RBEmpty_elm_builtin")))
          ? ($core$Dict$moveRedRight)($dict)
          : (true
            ? $dict
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 195:6', (sp_toHuman)($right)))))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 185:2', (sp_toHuman)($left))));
});

const $core$Dict$removeHelp = (($targetKey, $dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? $core$Dict$RBEmpty_elm_builtin
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $color = ($dict)[1];
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        return ((sp_equal)((basics_compare)($targetKey, $key), (0 - 1))
          ? (((($left)[0] === "RBNode_elm_builtin") && ((($left)[1])[0] === "Black"))
            ? ((() => {
              const $lLeft = ($left)[4];
              return (((($lLeft)[0] === "RBNode_elm_builtin") && ((($lLeft)[1])[0] === "Red"))
                ? ($core$Dict$RBNode_elm_builtin)($color, $key, $value, ($core$Dict$removeHelp)($targetKey, $left), $right)
                : (true
                  ? ((() => {
                    const $3 = ($core$Dict$moveRedLeft)($dict);
                    return ((($3)[0] === "RBNode_elm_builtin")
                      ? ((() => {
                        const $nColor = ($3)[1];
                        const $nKey = ($3)[2];
                        const $nValue = ($3)[3];
                        const $nLeft = ($3)[4];
                        const $nRight = ($3)[5];
                        return ($core$Dict$balance)($nColor, $nKey, $nValue, ($core$Dict$removeHelp)($targetKey, $nLeft), $nRight);
                      }))()
                      : ((($3)[0] === "RBEmpty_elm_builtin")
                        ? $core$Dict$RBEmpty_elm_builtin
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 169:16', (sp_toHuman)($3))));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 164:12', (sp_toHuman)($lLeft))));
            }))()
            : (true
              ? ($core$Dict$RBNode_elm_builtin)($color, $key, $value, ($core$Dict$removeHelp)($targetKey, $left), $right)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 162:8', (sp_toHuman)($left))))
          : ($core$Dict$removeHelpEQGT)($targetKey, ($core$Dict$removeHelpPrepEQGT)($targetKey, $dict, $color, $key, $value, $left, $right)));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 156:2', (sp_toHuman)($dict))));
});

const $core$Dict$remove = (($key, $dict) => {
  const $3 = ($core$Dict$removeHelp)($key, $dict);
  return (((($3)[0] === "RBNode_elm_builtin") && ((($3)[1])[0] === "Red"))
    ? ((() => {
      const $k = ($3)[2];
      const $v = ($3)[3];
      const $l = ($3)[4];
      const $r = ($3)[5];
      return ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, $l, $r);
    }))()
    : (true
      ? ((() => {
        const $x = $3;
        return $x;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 144:2', (sp_toHuman)($3))));
});

const $core$Dict$diff = (($t1, $t2) => {
  return ($core$Dict$for)($t1, $t2, (($k, $v, $t) => {
    return ($core$Dict$remove)($k, $t);
  }));
});

const $core$Dict$each = (($dict, $func) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? null
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        ($func)($key, $value);
        ($core$Dict$each)($left, $func);
        return ($core$Dict$each)($right, $func);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 453:2', (sp_toHuman)($dict))));
});

const $core$Dict$empty = $core$Dict$RBEmpty_elm_builtin;

const $core$Dict$insertHelp = (($key, $value, $dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? ($core$Dict$RBNode_elm_builtin)($core$Dict$Red, $key, $value, $core$Dict$RBEmpty_elm_builtin, $core$Dict$RBEmpty_elm_builtin)
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $nColor = ($dict)[1];
        const $nKey = ($dict)[2];
        const $nValue = ($dict)[3];
        const $nLeft = ($dict)[4];
        const $nRight = ($dict)[5];
        const $4 = (basics_compare)($key, $nKey);
        return ((1 === $4)
          ? ($core$Dict$balance)($nColor, $nKey, $nValue, $nLeft, ($core$Dict$insertHelp)($key, $value, $nRight))
          : ((0 === $4)
            ? ($core$Dict$RBNode_elm_builtin)($nColor, $nKey, $value, $nLeft, $nRight)
            : (true
              ? ($core$Dict$balance)($nColor, $nKey, $nValue, ($core$Dict$insertHelp)($key, $value, $nLeft), $nRight)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 96:6', (sp_toHuman)($4)))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 89:2', (sp_toHuman)($dict))));
});

const $core$Dict$insert = (($key, $value, $dict) => {
  const $4 = ($core$Dict$insertHelp)($key, $value, $dict);
  return (((($4)[0] === "RBNode_elm_builtin") && ((($4)[1])[0] === "Red"))
    ? ((() => {
      const $k = ($4)[2];
      const $v = ($4)[3];
      const $l = ($4)[4];
      const $r = ($4)[5];
      return ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $k, $v, $l, $r);
    }))()
    : (true
      ? ((() => {
        const $x = $4;
        return $x;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 79:2', (sp_toHuman)($4))));
});

const $core$Dict$filter = (($isGood, $dict) => {
  return ($core$Dict$for)($core$Dict$empty, $dict, (($k, $v, $d) => {
    return (($isGood)($k, $v)
      ? ($core$Dict$insert)($k, $v, $d)
      : $d);
  }));
});

const $core$Result$onOk = (($f) => {
  return (($result) => {
    return ((($result)[0] === "Err")
      ? ((() => {
        const $e = ($result)[1];
        return ($core$Result$Err)($e);
      }))()
      : ((($result)[0] === "Ok")
        ? ((() => {
          const $a = ($result)[1];
          return ($f)($a);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 18:4', (sp_toHuman)($result))));
  });
});

const $core$Dict$forRes = (($acc, $dict, $func) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? ($core$Result$Ok)($acc)
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        return (($core$Result$onOk)((($l) => {
          return (($core$Result$onOk)((($f) => {
            return ($core$Dict$forRes)($f, $right, $func);
          })))(($func)($key, $value, $l));
        })))(($core$Dict$forRes)($acc, $left, $func));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 475:4', (sp_toHuman)($dict))));
});

const $core$Dict$forReversed = (($acc, $t, $func) => {
  return ((($t)[0] === "RBEmpty_elm_builtin")
    ? $acc
    : ((($t)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $key = ($t)[2];
        const $value = ($t)[3];
        const $left = ($t)[4];
        const $right = ($t)[5];
        return ($core$Dict$forReversed)(($func)($key, $value, ($core$Dict$forReversed)($acc, $right, $func)), $left, $func);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 487:2', (sp_toHuman)($t))));
});

const $core$Dict$fromList = (($1) => {
  return ($core$List$for)($core$Dict$empty, $1, (($keyAndValue, $dict) => {
    return ($core$Dict$insert)($keyAndValue.first, $keyAndValue.second, $dict);
  }));
});

const $core$Dict$get = (($targetKey, $dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? $core$Maybe$Nothing
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        const $3 = (basics_compare)($targetKey, $key);
        return ((1 === $3)
          ? ($core$Dict$get)($targetKey, $right)
          : ((0 === $3)
            ? ($core$Maybe$Just)($value)
            : (true
              ? ($core$Dict$get)($targetKey, $left)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 23:6', (sp_toHuman)($3)))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 18:2', (sp_toHuman)($dict))));
});

const $core$Dict$member = (($key, $dict) => {
  const $3 = ($core$Dict$get)($key, $dict);
  return ((($3)[0] === "Just")
    ? true
    : ((($3)[0] === "Nothing")
      ? false
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 39:2', (sp_toHuman)($3))));
});

const $core$Dict$intersect = (($t1, $t2) => {
  return ($core$Dict$filter)((($k, _1) => {
    return ($core$Dict$member)($k, $t2);
  }), $t1);
});

const $core$Dict$isEmpty = (($dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? true
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? false
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 66:2', (sp_toHuman)($dict))));
});

const $core$Dict$join = (($0, $1) => {
  return ($core$Dict$for)($0, $1, $core$Dict$insert);
});

const $core$Dict$keys = (($1) => {
  return ($core$Dict$forReversed)($core$Core$Nil, $1, (($key, $value, $keyList) => {
    return ($core$Core$Cons)($key, $keyList);
  }));
});

const $core$Dict$map = (($func, $dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? $core$Dict$RBEmpty_elm_builtin
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $color = ($dict)[1];
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        return ($core$Dict$RBNode_elm_builtin)($color, $key, ($func)($key, $value), ($core$Dict$map)($func, $left), ($core$Dict$map)($func, $right));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 424:2', (sp_toHuman)($dict))));
});

const $core$Dict$mapKeys = (($func, $dict) => {
  return ($core$Dict$for)($core$Dict$empty, $dict, (($k, $v, $d) => {
    return ($core$Dict$insert)(($func)($k), $v, $d);
  }));
});

const $core$Dict$mapRes = (($func, $dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? ($core$Result$Ok)($core$Dict$RBEmpty_elm_builtin)
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $color = ($dict)[1];
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        return (($core$Result$onOk)((($one) => {
          return (($core$Result$onOk)((($two) => {
            return (($core$Result$onOk)((($three) => {
              return ($core$Result$Ok)(($core$Dict$RBNode_elm_builtin)($color, $key, $one, $two, $three));
            })))(($core$Dict$mapRes)($func, $right));
          })))(($core$Dict$mapRes)($func, $left));
        })))(($func)($key, $value));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 434:2', (sp_toHuman)($dict))));
});

const $core$Dict$toList = ((() => {
  const $f = (($key, $value, $list) => {
    return ($core$Core$Cons)(({
      first: $key,
      second: $value,
    }), $list);
  });
  return (($1) => {
    return ($core$Dict$forReversed)($core$Core$Nil, $1, $f);
  });
}))();

const $core$Dict$merge = (($leftStep, $bothStep, $rightStep, $leftDict, $rightDict, $initialResult) => {
  const $stepState = (($rKey, $rValue, $q) => {
    const $10 = $q;
    const $res = $10.second;
    const $list = $10.first;
    return ((($list)[0] === "Nil")
      ? ({
        first: $list,
        second: ($rightStep)($rKey, $rValue, $res),
      })
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $lKey = ($list)[1].first;
          const $lValue = ($list)[1].second;
          const $rest = ($list)[2];
          const $11 = (basics_compare)($lKey, $rKey);
          return ((1 === $11)
            ? ({
              first: $list,
              second: ($rightStep)($rKey, $rValue, $res),
            })
            : ((0 === $11)
              ? ({
                first: $rest,
                second: ($bothStep)($lKey, $lValue, $rValue, $res),
              })
              : (true
                ? ($stepState)($rKey, $rValue, ({
                  first: $rest,
                  second: ($leftStep)($lKey, $lValue, $res),
                }))
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 383:10', (sp_toHuman)($11)))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 378:4', (sp_toHuman)($list))));
  });
  const $7 = ($core$Dict$for)(({
    first: ($core$Dict$toList)($leftDict),
    second: $initialResult,
  }), $rightDict, $stepState);
  const $intermediateResult = $7.second;
  const $leftovers = $7.first;
  const $liftLeftStep = (($t, $res) => {
    const $10 = $t;
    const $v = $10.second;
    const $k = $10.first;
    return ($leftStep)($k, $v, $res);
  });
  return ($core$List$for)($intermediateResult, $leftovers, $liftLeftStep);
});

const $core$Dict$ofOne = (($key, $value) => {
  return ($core$Dict$RBNode_elm_builtin)($core$Dict$Black, $key, $value, $core$Dict$RBEmpty_elm_builtin, $core$Dict$RBEmpty_elm_builtin);
});

const $core$Dict$onlyBothOnly = (($da, $db) => {
  const $onAOnly = (($key, $a, $3) => {
    const $aOnly = $3.first;
    const $both = $3.second;
    const $bOnly = $3.third;
    return ({
      first: ($core$Dict$insert)($key, $a, $aOnly),
      second: $both,
      third: $bOnly,
    });
  });
  const $onBOnly = (($key, $b, $3) => {
    const $aOnly = $3.first;
    const $both = $3.second;
    const $bOnly = $3.third;
    return ({
      first: $aOnly,
      second: $both,
      third: ($core$Dict$insert)($key, $b, $bOnly),
    });
  });
  const $onBoth = (($key, $a, $b, $3) => {
    const $aOnly = $3.first;
    const $both = $3.second;
    const $bOnly = $3.third;
    return ({
      first: $aOnly,
      second: ($core$Dict$insert)($key, ({
        first: $a,
        second: $b,
      }), $both),
      third: $bOnly,
    });
  });
  return ($core$Dict$merge)($onAOnly, $onBoth, $onBOnly, $da, $db, ({
    first: $core$Dict$empty,
    second: $core$Dict$empty,
    third: $core$Dict$empty,
  }));
});

const $core$Dict$partition = (($isGood, $dict) => {
  const $add = (($key, $value, $t) => {
    const $6 = $t;
    const $t2 = $6.second;
    const $t1 = $6.first;
    return (($isGood)($key, $value)
      ? ({
        first: ($core$Dict$insert)($key, $value, $t1),
        second: $t2,
      })
      : ({
        first: $t1,
        second: ($core$Dict$insert)($key, $value, $t2),
      }));
  });
  return ($core$Dict$for)(({
    first: $core$Dict$empty,
    second: $core$Dict$empty,
  }), $dict, $add);
});

const $core$Dict$size = ((() => {
  const $sizeHelp = (($n, $dict) => {
    return ((($dict)[0] === "RBEmpty_elm_builtin")
      ? $n
      : ((($dict)[0] === "RBNode_elm_builtin")
        ? ((() => {
          const $left = ($dict)[4];
          const $right = ($dict)[5];
          return ($sizeHelp)(($sizeHelp)(($n + 1), $right), $left);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 53:4', (sp_toHuman)($dict))));
  });
  return (($1) => {
    return ($sizeHelp)(0, $1);
  });
}))();

const $core$Dict$update = (($targetKey, $alter, $dictionary) => {
  const $4 = ($alter)(($core$Dict$get)($targetKey, $dictionary));
  return ((($4)[0] === "Just")
    ? ((() => {
      const $value = ($4)[1];
      return ($core$Dict$insert)($targetKey, $value, $dictionary);
    }))()
    : ((($4)[0] === "Nothing")
      ? ($core$Dict$remove)($targetKey, $dictionary)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 337:2', (sp_toHuman)($4))));
});

const $core$Dict$values = (($1) => {
  return ($core$Dict$forReversed)($core$Core$Nil, $1, (($key, $value, $valueList) => {
    return ($core$Core$Cons)($value, $valueList);
  }));
});

const $core$Dict_Test$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $core$Dict_Test$insertAndGet = ($sd1$Test$Group)("insertAndGet", ($core$Core$Cons)(($core$Dict_Test$valueTest)("get, success", ((_0) => {
  return ((($1) => {
    return ($core$Dict$get)(($core$Maybe$Just)("a"), $1);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("b"), 2, $2);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("a"), 1, $2);
  }))($core$Dict$empty)));
}), ($sd1$Test$isOkAndEqualTo)(($core$Maybe$Just)(1))), ($core$Core$Cons)(($core$Dict_Test$valueTest)("get, fail", ((_0) => {
  return ((($1) => {
    return ($core$Dict$get)(($core$Maybe$Just)("c"), $1);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("b"), 2, $2);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("a"), 1, $2);
  }))($core$Dict$empty)));
}), ($sd1$Test$isOkAndEqualTo)($core$Maybe$Nothing)), $core$Core$Nil)));

const $core$Dict_Test$lists = ($sd1$Test$Group)("lists", ($core$Core$Cons)(($core$Dict_Test$valueTest)("keys", ((_0) => {
  return ((($1) => {
    return (list_sortBy)($core$Basics$identity, $1);
  }))(($core$Dict$keys)(((($2) => {
    return ($core$Dict$insert)($core$Maybe$Nothing, 2, $2);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("b"), 2, $2);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("a"), 1, $2);
  }))($core$Dict$empty)))));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Maybe$Just)("a"), ($core$Core$Cons)(($core$Maybe$Just)("b"), ($core$Core$Cons)($core$Maybe$Nothing, $core$Core$Nil))))), ($core$Core$Cons)(($core$Dict_Test$valueTest)("values", ((_0) => {
  return ((($1) => {
    return (list_sortBy)($core$Basics$identity, $1);
  }))(($core$Dict$values)(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("b"), ({
      a: 3,
    }), $2);
  }))(((($2) => {
    return ($core$Dict$insert)(($core$Maybe$Just)("a"), ({
      a: 1,
    }), $2);
  }))($core$Dict$empty))));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(({
  a: 1,
}), ($core$Core$Cons)(({
  a: 3,
}), $core$Core$Nil)))), $core$Core$Nil)));

const $core$Dict_Test$tests = ($sd1$Test$Group)("Dict", ($core$Core$Cons)($core$Dict_Test$insertAndGet, ($core$Core$Cons)($core$Dict_Test$lists, $core$Core$Nil)));

const $core$Hash$toList = (($h) => {
  return ([
    ((__re__ = (hash_for)($h, (($k, $v, $l) => {
      return ($core$Core$Cons)(({
        first: $k,
        second: $v,
      }), $l);
    }), $core$Core$Nil)), ($h = (__re__)[1]), (__re__)[0]),
    $h,
  ]);
});

const $core$Hash_Test$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $core$List$range = (($low, $high) => {
  const $rec = (($accum, $up) => {
    return (($up > $low)
      ? ($rec)((sp_cons)($up, $accum), ($up - 1))
      : ((sp_equal)($up, $low)
        ? (sp_cons)($up, $accum)
        : $accum));
  });
  return ($rec)($core$Core$Nil, $high);
});

const $core$Tuple$first = (($t) => {
  return $t.first;
});

const $core$Hash_Test$tests = ($sd1$Test$Group)("Hash", ($core$Core$Cons)(($core$Hash_Test$valueTest)("insert", ((_0) => {
  let $h = (hash_fromList)(($core$Core$Cons)(({
    first: 1,
    second: 2,
  }), $core$Core$Nil));
  ((__re__ = (hash_insert)($h, 2, 3)), ($h = (__re__)[1]), (__re__)[0]);
  return $h;
}), ($sd1$Test$isOkAndEqualTo)((hash_fromList)(($core$Core$Cons)(({
  first: 1,
  second: 2,
}), ($core$Core$Cons)(({
  first: 2,
  second: 3,
}), $core$Core$Nil))))), ($core$Core$Cons)(($core$Hash_Test$valueTest)("remove", ((_0) => {
  let $h = (hash_fromList)(($core$Core$Cons)(({
    first: 1,
    second: 2,
  }), ($core$Core$Cons)(({
    first: 3,
    second: 4,
  }), $core$Core$Nil)));
  ((__re__ = (hash_remove)($h, 1)), ($h = (__re__)[1]), (__re__)[0]);
  return $h;
}), ($sd1$Test$isOkAndEqualTo)((hash_fromList)(($core$Core$Cons)(({
  first: 3,
  second: 4,
}), $core$Core$Nil)))), ($core$Core$Cons)(($core$Hash_Test$valueTest)("get Just", ((_0) => {
  let $h = (hash_fromList)(($core$Core$Cons)(({
    first: 1,
    second: 2,
  }), ($core$Core$Cons)(({
    first: 3,
    second: 4,
  }), $core$Core$Nil)));
  return ((__re__ = (hash_get)($h, 1)), ($h = (__re__)[1]), (__re__)[0]);
}), ($sd1$Test$isOkAndEqualTo)(($core$Maybe$Just)(2))), ($core$Core$Cons)(($core$Hash_Test$valueTest)("get Nothing", ((_0) => {
  let $h = (hash_fromList)(($core$Core$Cons)(({
    first: 1,
    second: 2,
  }), ($core$Core$Cons)(({
    first: 3,
    second: 4,
  }), $core$Core$Nil)));
  return ((__re__ = (hash_get)($h, 66)), ($h = (__re__)[1]), (__re__)[0]);
}), ($sd1$Test$isOkAndEqualTo)($core$Maybe$Nothing)), ($core$Core$Cons)(($core$Hash_Test$valueTest)("for", ((_0) => {
  let $hash = (hash_fromList)(($core$Core$Cons)(({
    first: ($core$Maybe$Just)(true),
    second: 2,
  }), ($core$Core$Cons)(({
    first: $core$Maybe$Nothing,
    second: 4,
  }), $core$Core$Nil)));
  return ((($1) => {
    return (list_sortBy)($core$Tuple$first, $1);
  }))(((($2) => {
    return ((__re__ = (hash_for)($hash, (($k, $v, $a) => {
      return ($core$Core$Cons)(({
        first: $v,
        second: $k,
      }), $a);
    }), $2)), ($hash = (__re__)[1]), (__re__)[0]);
  }))($core$Core$Nil));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(({
  first: 2,
  second: ($core$Maybe$Just)(true),
}), ($core$Core$Cons)(({
  first: 4,
  second: $core$Maybe$Nothing,
}), $core$Core$Nil)))), ($core$Core$Cons)(($core$Hash_Test$valueTest)("each", ((_0) => {
  let $a = (array_fromList)($core$Core$Nil);
  let $hash = (hash_fromList)(($core$Core$Cons)(({
    first: ($core$Maybe$Just)(true),
    second: 2,
  }), ($core$Core$Cons)(({
    first: $core$Maybe$Nothing,
    second: 1,
  }), $core$Core$Nil)));
  ((__re__ = (hash_each)($hash, (($k, $v) => {
    return ($core$List$each)(($core$List$range)(1, $v), ((_0) => {
      return ((__re__ = (array_push)($a, $k)), ($a = (__re__)[1]), (__re__)[0]);
    }));
  }))), ($hash = (__re__)[1]), (__re__)[0]);
  ((__re__ = (array_sortBy)($a, $core$Basics$identity)), ($a = (__re__)[1]), (__re__)[0]);
  return $a;
}), ($sd1$Test$isOkAndEqualTo)((array_fromList)(($core$Core$Cons)(($core$Maybe$Just)(true), ($core$Core$Cons)(($core$Maybe$Just)(true), ($core$Core$Cons)($core$Maybe$Nothing, $core$Core$Nil)))))), $core$Core$Nil)))))));

const $core$List$all = (($fun, $list) => {
  return ((($list)[0] === "Nil")
    ? true
    : ((($list)[0] === "Cons")
      ? ((() => {
        const $h = ($list)[1];
        const $t = ($list)[2];
        return (($fun)($h)
          ? ($core$List$all)($fun, $t)
          : false);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 13:4', (sp_toHuman)($list))));
});

const $core$List$any = (($fun, $list) => {
  return ((($list)[0] === "Nil")
    ? false
    : ((($list)[0] === "Cons")
      ? ((() => {
        const $h = ($list)[1];
        const $t = ($list)[2];
        return (($fun)($h)
          ? true
          : ($core$List$any)($fun, $t));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 6:4', (sp_toHuman)($list))));
});

const $core$List$append = (($xs, $ys) => {
  return ((($ys)[0] === "Nil")
    ? $xs
    : (true
      ? ($core$List$forReversed)($ys, $xs, $core$Core$Cons)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 199:2', (sp_toHuman)($ys))));
});

const $core$List$concat = (($lists) => {
  return ($core$List$forReversed)($core$Core$Nil, $lists, $core$List$append);
});

const $core$List$concatMap = (($f, $list) => {
  return ($core$List$concat)(($core$List$map)($f, $list));
});

const $core$List$drop = (($n, $ls) => {
  return ((sp_equal)($n, 0)
    ? $ls
    : ((($ls)[0] === "Nil")
      ? $core$Core$Nil
      : ((($ls)[0] === "Cons")
        ? ((() => {
          const $h = ($ls)[1];
          const $tail = ($ls)[2];
          return ($core$List$drop)(($n - 1), $tail);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 371:6', (sp_toHuman)($ls)))));
});

const $core$List$filter = (($f, $ls) => {
  return ($core$List$forReversed)($core$Core$Nil, $ls, (($item, $acc) => {
    return (($f)($item)
      ? (sp_cons)($item, $acc)
      : $acc);
  }));
});

const $core$List$filterMap = (($f, $la) => {
  const $update = (($a, $acc) => {
    const $5 = ($f)($a);
    return ((($5)[0] === "Just")
      ? ((() => {
        const $b = ($5)[1];
        return (sp_cons)($b, $acc);
      }))()
      : ((($5)[0] === "Nothing")
        ? $acc
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 319:6', (sp_toHuman)($5))));
  });
  return ($core$List$forReversed)($core$Core$Nil, $la, $update);
});

const $core$List$find = (($test, $list) => {
  return ((($list)[0] === "Nil")
    ? $core$Maybe$Nothing
    : ((($list)[0] === "Cons")
      ? ((() => {
        const $h = ($list)[1];
        const $t = ($list)[2];
        return (($test)($h)
          ? ($core$Maybe$Just)($h)
          : ($core$List$find)($test, $t));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 24:4', (sp_toHuman)($list))));
});

const $core$List$forRes = (($accum, $ls, $f) => {
  return ((($ls)[0] === "Nil")
    ? ($core$Result$Ok)($accum)
    : ((($ls)[0] === "Cons")
      ? ((() => {
        const $h = ($ls)[1];
        const $t = ($ls)[2];
        const $4 = ($f)($h, $accum);
        return ((($4)[0] === "Err")
          ? ((() => {
            const $x = ($4)[1];
            return ($core$Result$Err)($x);
          }))()
          : ((($4)[0] === "Ok")
            ? ((() => {
              const $newAccum = ($4)[1];
              return ($core$List$forRes)($newAccum, $t, $f);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 153:12', (sp_toHuman)($4))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 148:4', (sp_toHuman)($ls))));
});

const $core$List$head = (($list) => {
  return ((($list)[0] === "Nil")
    ? $core$Maybe$Nothing
    : ((($list)[0] === "Cons")
      ? ((() => {
        const $h = ($list)[1];
        const $t = ($list)[2];
        return ($core$Maybe$Just)($h);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 226:4', (sp_toHuman)($list))));
});

const $core$Tuple$second = (($t) => {
  return $t.second;
});

const $core$List$indexedFor = (($init, $aList, $function) => {
  return ($core$Tuple$second)(($core$List$for)(({
    first: 0,
    second: $init,
  }), $aList, (($item, $4) => {
    const $index = $4.first;
    const $accum = $4.second;
    return ({
      first: ($index + 1),
      second: ($function)($index, $item, $accum),
    });
  })));
});

const $core$List$indexedMap = (($f, $aa) => {
  const $rec = (($accum, $n, $list) => {
    return ((($list)[0] === "Nil")
      ? ($core$List$reverse)($accum)
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $h = ($list)[1];
          const $t = ($list)[2];
          return ($rec)((sp_cons)(($f)($n, $h), $accum), ($n + 1), $t);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 178:8', (sp_toHuman)($list))));
  });
  return ($rec)($core$Core$Nil, 0, $aa);
});

const $core$List$indexedMap2 = (($f, $aaa, $bbb) => {
  const $rec = (($accum, $n, $aa, $bb) => {
    const $8 = ({
      first: $aa,
      second: $bb,
    });
    return (((($8.first)[0] === "Cons") && (($8.second)[0] === "Cons"))
      ? ((() => {
        const $a = ($8.first)[1];
        const $at = ($8.first)[2];
        const $b = ($8.second)[1];
        const $bt = ($8.second)[2];
        return ($rec)((sp_cons)(($f)($n, $a, $b), $accum), ($n + 1), $at, $bt);
      }))()
      : (true
        ? ($core$List$reverse)($accum)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 190:8', (sp_toHuman)($8))));
  });
  return ($rec)($core$Core$Nil, 0, $aaa, $bbb);
});

const $core$List$last = (($list) => {
  return ((($list)[0] === "Nil")
    ? $core$Maybe$Nothing
    : (((($list)[0] === "Cons") && ((($list)[2])[0] === "Nil"))
      ? ((() => {
        const $h = ($list)[1];
        return ($core$Maybe$Just)($h);
      }))()
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $h = ($list)[1];
          const $t = ($list)[2];
          return ($core$List$last)($t);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 233:4', (sp_toHuman)($list)))));
});

const $core$List$length = (($list) => {
  return ($core$List$for)(0, $list, ((_0, $a) => {
    return ($a + 1);
  }));
});

const $core$List$map2 = (($f, $aa, $bb) => {
  const $rec = (($accum, $ax, $bx) => {
    const $7 = ({
      first: $ax,
      second: $bx,
    });
    return (((($7.first)[0] === "Cons") && (($7.second)[0] === "Cons"))
      ? ((() => {
        const $ahead = ($7.first)[1];
        const $atail = ($7.first)[2];
        const $bhead = ($7.second)[1];
        const $btail = ($7.second)[2];
        return ($rec)(($core$Core$Cons)(($f)($ahead, $bhead), $accum), $atail, $btail);
      }))()
      : (true
        ? ($core$List$reverse)($accum)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 127:6', (sp_toHuman)($7))));
  });
  return ($rec)($core$Core$Nil, $aa, $bb);
});

const $core$List$mapFirst = (($f, $ls) => {
  return ((($ls)[0] === "Nil")
    ? $core$Maybe$Nothing
    : ((($ls)[0] === "Cons")
      ? ((() => {
        const $h = ($ls)[1];
        const $tail = ($ls)[2];
        const $r = ($f)($h);
        return ((($r)[0] === "Nothing")
          ? ($core$List$mapFirst)($f, $tail)
          : (true
            ? $r
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 334:12', (sp_toHuman)($r))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 328:4', (sp_toHuman)($ls))));
});

const $core$List$mapRes = (($f, $list) => {
  const $fun = (($a, $acc) => {
    return ($core$Result$map)((($b) => {
      return ($core$Core$Cons)($b, $acc);
    }), ($f)($a));
  });
  return ((($1) => {
    return ($core$Result$map)($core$List$reverse, $1);
  }))(($core$List$forRes)($core$Core$Nil, $list, $fun));
});

const $core$List$maximum = (($list) => {
  return ((($list)[0] === "Cons")
    ? ((() => {
      const $x = ($list)[1];
      const $xs = ($list)[2];
      return ($core$Maybe$Just)(($core$List$for)($x, $xs, $core$Basics$max));
    }))()
    : (true
      ? $core$Maybe$Nothing
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 390:4', (sp_toHuman)($list))));
});

const $core$List$member = (($a, $list) => {
  return ((($list)[0] === "Nil")
    ? false
    : ((($list)[0] === "Cons")
      ? ((() => {
        const $h = ($list)[1];
        const $t = ($list)[2];
        return ((sp_equal)($a, $h)
          ? true
          : ($core$List$member)($a, $t));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 35:4', (sp_toHuman)($list))));
});

const $core$List$minimum = (($list) => {
  return ((($list)[0] === "Cons")
    ? ((() => {
      const $x = ($list)[1];
      const $xs = ($list)[2];
      return ($core$Maybe$Just)(($core$List$for)($x, $xs, $core$Basics$min));
    }))()
    : (true
      ? $core$Maybe$Nothing
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 379:4', (sp_toHuman)($list))));
});

const $core$List$partition = (($f, $ls) => {
  return ((($0) => {
    return ($core$List$forReversed)($0, $ls, (($item, $4) => {
      const $true = $4.first;
      const $false = $4.second;
      return (($f)($item)
        ? ({
          first: (sp_cons)($item, $true),
          second: $false,
        })
        : ({
          first: $true,
          second: (sp_cons)($item, $false),
        }));
    }));
  }))(({
    first: $core$Core$Nil,
    second: $core$Core$Nil,
  }));
});

const $core$List$repeat = (($n, $a) => {
  const $rec = (($c, $acc) => {
    return (($c > 0)
      ? ($rec)(($c - 1), (sp_cons)($a, $acc))
      : $acc);
  });
  return ($rec)($n, $core$Core$Nil);
});

const $core$List$sort = (($1) => {
  return (list_sortBy)($core$Basics$identity, $1);
});

const $core$List$takeReverse = (($n, $list, $kept) => {
  return (($n < 1)
    ? $kept
    : ((($list)[0] === "Nil")
      ? $kept
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $x = ($list)[1];
          const $xs = ($list)[2];
          return ($core$List$takeReverse)(($n - 1), $xs, ($core$Core$Cons)($x, $kept));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 283:4', (sp_toHuman)($list)))));
});

const $core$List$takeTailRec = (($n, $list) => {
  return ($core$List$reverse)(($core$List$takeReverse)($n, $list, $core$Core$Nil));
});

const $core$List$takeFast = (($ctr, $n, $list) => {
  return (($n < 1)
    ? $core$Core$Nil
    : ((() => {
      const $4 = ({
        first: $n,
        second: $list,
      });
      return ((($4.second)[0] === "Nil")
        ? $list
        : (((1 === $4.first) && (($4.second)[0] === "Cons"))
          ? ((() => {
            const $x = ($4.second)[1];
            return ($core$Core$Cons)($x, $core$Core$Nil);
          }))()
          : (((2 === $4.first) && ((($4.second)[0] === "Cons") && ((($4.second)[2])[0] === "Cons")))
            ? ((() => {
              const $x = ($4.second)[1];
              const $y = (($4.second)[2])[1];
              return ($core$Core$Cons)($x, ($core$Core$Cons)($y, $core$Core$Nil));
            }))()
            : (((3 === $4.first) && ((($4.second)[0] === "Cons") && (((($4.second)[2])[0] === "Cons") && (((($4.second)[2])[2])[0] === "Cons"))))
              ? ((() => {
                const $x = ($4.second)[1];
                const $y = (($4.second)[2])[1];
                const $z = ((($4.second)[2])[2])[1];
                return ($core$Core$Cons)($x, ($core$Core$Cons)($y, ($core$Core$Cons)($z, $core$Core$Nil)));
              }))()
              : (((($4.second)[0] === "Cons") && (((($4.second)[2])[0] === "Cons") && ((((($4.second)[2])[2])[0] === "Cons") && ((((($4.second)[2])[2])[2])[0] === "Cons"))))
                ? ((() => {
                  const $x = ($4.second)[1];
                  const $y = (($4.second)[2])[1];
                  const $z = ((($4.second)[2])[2])[1];
                  const $w = (((($4.second)[2])[2])[2])[1];
                  const $tl = (((($4.second)[2])[2])[2])[2];
                  const $cons = $core$Core$Cons;
                  return (($ctr > 1000)
                    ? ($cons)($x, ($cons)($y, ($cons)($z, ($cons)($w, ($core$List$takeTailRec)(($n - 4), $tl)))))
                    : ($cons)($x, ($cons)($y, ($cons)($z, ($cons)($w, ($core$List$takeFast)(($ctr + 1), ($n - 4), $tl))))));
                }))()
                : (true
                  ? $list
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 249:4', (sp_toHuman)($4))))))));
    }))());
});

const $core$List$take = (($1, $2) => {
  return ($core$List$takeFast)(0, $1, $2);
});

const $core$List$takeWhile = (($test, $its) => {
  const $rec = (($accum, $list) => {
    return ((($list)[0] === "Nil")
      ? ($core$List$reverse)($accum)
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $head = ($list)[1];
          const $tail = ($list)[2];
          return (($test)($head)
            ? ($rec)((sp_cons)($head, $accum), $tail)
            : ($core$List$reverse)($accum));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 296:6', (sp_toHuman)($list))));
  });
  return ($rec)($core$Core$Nil, $its);
});

const $core$List_Test$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $core$List_Test$concat = ($sd1$Test$Group)("concat", ($core$Core$Cons)(($core$List_Test$valueTest)("concats two lists", ((_0) => {
  return ($core$List$concat)(($core$Core$Cons)(($core$Core$Cons)(1, ($core$Core$Cons)(2, $core$Core$Nil)), ($core$Core$Cons)(($core$Core$Cons)(3, ($core$Core$Cons)(4, $core$Core$Nil)), $core$Core$Nil)));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(1, ($core$Core$Cons)(2, ($core$Core$Cons)(3, ($core$Core$Cons)(4, $core$Core$Nil)))))), $core$Core$Nil));

const $core$List_Test$sortBy = ($sd1$Test$Group)("sortBy", ($core$Core$Cons)(($core$List_Test$valueTest)("Can actually sort stuff", ((_0) => {
  return ((($1) => {
    return (list_sortBy)($core$Basics$identity, $1);
  }))(($core$Core$Cons)(($core$Maybe$Just)(23), ($core$Core$Cons)($core$Maybe$Nothing, ($core$Core$Cons)(($core$Maybe$Just)(11), $core$Core$Nil))));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Maybe$Just)(11), ($core$Core$Cons)(($core$Maybe$Just)(23), ($core$Core$Cons)($core$Maybe$Nothing, $core$Core$Nil))))), ($core$Core$Cons)(($core$List_Test$valueTest)("Correctly orders tuple-2", ((_0) => {
  return ((($1) => {
    return (list_sortBy)($core$Basics$identity, $1);
  }))(($core$Core$Cons)(({
    first: 23,
    second: 1,
  }), ($core$Core$Cons)(({
    first: 1,
    second: 2,
  }), ($core$Core$Cons)(({
    first: 11,
    second: 3,
  }), $core$Core$Nil))));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(({
  first: 1,
  second: 2,
}), ($core$Core$Cons)(({
  first: 11,
  second: 3,
}), ($core$Core$Cons)(({
  first: 23,
  second: 1,
}), $core$Core$Nil))))), ($core$Core$Cons)(($core$List_Test$valueTest)("Correctly orders tuple-3", ((_0) => {
  return ((($1) => {
    return (list_sortBy)($core$Basics$identity, $1);
  }))(($core$Core$Cons)(({
    first: "z",
    second: "a",
    third: "2",
  }), ($core$Core$Cons)(({
    first: "a",
    second: "b",
    third: "33",
  }), ($core$Core$Cons)(({
    first: "z",
    second: "a",
    third: "1",
  }), ($core$Core$Cons)(({
    first: "z",
    second: "b",
    third: "3",
  }), $core$Core$Nil)))));
}), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(({
  first: "a",
  second: "b",
  third: "33",
}), ($core$Core$Cons)(({
  first: "z",
  second: "a",
  third: "1",
}), ($core$Core$Cons)(({
  first: "z",
  second: "a",
  third: "2",
}), ($core$Core$Cons)(({
  first: "z",
  second: "b",
  third: "3",
}), $core$Core$Nil)))))), $core$Core$Nil))));

const $core$List_Test$tests = ($sd1$Test$Group)("List", ($core$Core$Cons)($core$List_Test$sortBy, ($core$Core$Cons)($core$List_Test$concat, $core$Core$Nil)));

const $core$Maybe$andThen = (($f) => {
  return (($ma) => {
    return ((($ma)[0] === "Nothing")
      ? $core$Maybe$Nothing
      : ((($ma)[0] === "Just")
        ? ((() => {
          const $a = ($ma)[1];
          return ($f)($a);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 9:4', (sp_toHuman)($ma))));
  });
});

const $core$Maybe$map = (($f, $m) => {
  return ((($m)[0] === "Nothing")
    ? $core$Maybe$Nothing
    : ((($m)[0] === "Just")
      ? ((() => {
        const $v = ($m)[1];
        return ($core$Maybe$Just)(($f)($v));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 16:4', (sp_toHuman)($m))));
});

const $core$Maybe$mapRes = (($f, $m) => {
  return ((($m)[0] === "Nothing")
    ? ($core$Result$Ok)($core$Maybe$Nothing)
    : ((($m)[0] === "Just")
      ? ((() => {
        const $a = ($m)[1];
        return ($core$Result$map)($core$Maybe$Just, ($f)($a));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 38:4', (sp_toHuman)($m))));
});

const $core$Maybe$withDefault = (($default, $maybe) => {
  return ((($maybe)[0] === "Just")
    ? ((() => {
      const $v = ($maybe)[1];
      return $v;
    }))()
    : ((($maybe)[0] === "Nothing")
      ? $default
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 48:4', (sp_toHuman)($maybe))));
});

const $core$Result$fromMaybe = (($err, $maybe) => {
  return ((($maybe)[0] === "Nothing")
    ? ($core$Result$Err)($err)
    : ((($maybe)[0] === "Just")
      ? ((() => {
        const $a = ($maybe)[1];
        return ($core$Result$Ok)($a);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 42:4', (sp_toHuman)($maybe))));
});

const $core$Result$mapError = (($f, $result) => {
  return ((($result)[0] === "Ok")
    ? ((() => {
      const $a = ($result)[1];
      return ($core$Result$Ok)($a);
    }))()
    : ((($result)[0] === "Err")
      ? ((() => {
        const $e1 = ($result)[1];
        return ($core$Result$Err)(($f)($e1));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 34:4', (sp_toHuman)($result))));
});

const $core$Result$onErr = (($f) => {
  return (($result) => {
    return ((($result)[0] === "Err")
      ? ((() => {
        const $e = ($result)[1];
        return ($f)($e);
      }))()
      : ((($result)[0] === "Ok")
        ? ((() => {
          const $a = ($result)[1];
          return ($core$Result$Ok)($a);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 26:4', (sp_toHuman)($result))));
  });
});

const $core$Result$withDefault = (($default, $result) => {
  return ((($result)[0] === "Ok")
    ? ((() => {
      const $a = ($result)[1];
      return $a;
    }))()
    : ((($result)[0] === "Err")
      ? $default
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 50:4', (sp_toHuman)($result))));
});

const $core$Set$diff = $core$Dict$diff;

const $core$Set$empty = $core$Dict$empty;

const $core$Set$insert = (($0, $2) => {
  return ($core$Dict$insert)($0, null, $2);
});

const $core$Set$fromList = (($1) => {
  return ($core$List$for)($core$Set$empty, $1, $core$Set$insert);
});

const $core$Set$intersect = $core$Dict$intersect;

const $core$Set$isEmpty = $core$Dict$isEmpty;

const $core$Set$join = $core$Dict$join;

const $core$Set$map = (($f, $set) => {
  return ($core$Dict$for)($core$Set$empty, $set, (($k, _1, $d) => {
    return ($core$Dict$insert)(($f)($k), null, $d);
  }));
});

const $core$Set$member = $core$Dict$member;

const $core$Set$ofOne = (($0) => {
  return ($core$Dict$ofOne)($0, null);
});

const $core$Set$remove = $core$Dict$remove;

const $core$Set$size = $core$Dict$size;

const $core$Set$toList = $core$Dict$keys;

const $core$Text$contains = (($sub, $str) => {
  const $3 = (text_split)($sub, $str);
  return (((($3)[0] === "Cons") && ((($3)[2])[0] === "Nil"))
    ? false
    : (true
      ? true
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Text.sp 116:4', (sp_toHuman)($3))));
});

const $core$Text$dropRight = (($n, $s) => {
  return (($n > 0)
    ? (text_slice)(0, -($n), $s)
    : $s);
});

const $core$Text$repeat = (($n, $s) => {
  return ($core$Text$join)("", ($core$List$repeat)($n, $s));
});

const $core$Text$padLeft = (($minLength, $pad, $s) => {
  const $textLength = (text_length)($s);
  return (($textLength < $minLength)
    ? ((() => {
      const $times = (sp_divide)(($textLength - $minLength), (text_length)($pad));
      return (($core$Text$repeat)($times, $pad) + $s);
    }))()
    : $s);
});

const $core$Text$padRight = (($minLength, $pad, $s) => {
  const $textLength = (text_length)($s);
  return (($textLength < $minLength)
    ? ((() => {
      const $times = (sp_divide)(($textLength - $minLength), (text_length)($pad));
      return ($s + ($core$Text$repeat)($times, $pad));
    }))()
    : $s);
});

const $core$Text$replace = (($toRemove, $toPut, $s) => {
  return ((($1) => {
    return ($core$Text$join)($toPut, $1);
  }))(((($1) => {
    return (text_split)($toRemove, $1);
  }))($s));
});

const $core$Tuple$mapBoth = (($fa, $fb, $t) => {
  return ({
    first: ($fa)($t.first),
    second: ($fb)($t.second),
  });
});

const $core$Tuple$mapFirst = (($f, $t) => {
  return ({
    first: ($f)($t.first),
    second: $t.second,
  });
});

const $core$Tuple$mapSecond = (($f, $t) => {
  return ({
    first: $t.first,
    second: ($f)($t.second),
  });
});

const $core$Tuple$pair = (($a, $b) => {
  return ({
    first: $a,
    second: $b,
  });
});

const $posix$IO$_run = (($never, $r) => {
  const $3 = $r;
  const $neverToResult = ($3)[1];
  return ($neverToResult)($never);
});

const $posix$IO$fail = (($message) => {
  return ($posix$IO$IO)((($never) => {
    return ($core$Result$Err)($message);
  }));
});

const $posix$IO$onResult = (($f) => {
  return (($m) => {
    return ($posix$IO$IO)((($never) => {
      return ((($1) => {
        return ($posix$IO$_run)($never, $1);
      }))(($f)(((($1) => {
        return ($posix$IO$_run)($never, $1);
      }))($m)));
    }));
  });
});

const $posix$IO$onSuccess = (($f) => {
  return (($m) => {
    return ($posix$IO$IO)((($never) => {
      const $4 = ($posix$IO$_run)($never, $m);
      return ((($4)[0] === "Ok")
        ? ((() => {
          const $a = ($4)[1];
          return ($posix$IO$_run)($never, ($f)($a));
        }))()
        : ((($4)[0] === "Err")
          ? ((() => {
            const $e = ($4)[1];
            return ($core$Result$Err)($e);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/posix/IO.sp 40:8', (sp_toHuman)($4))));
    }));
  });
});

const $posix$IO$parallel = ((_0) => {
  return (sp_todo)("io.parallel");
});

const $posix$IO$readDir = ((_0) => {
  return (sp_todo)("io.readDir");
});

const $posix$IO$readFile = ((_0) => {
  return (sp_todo)("io.readFile");
});

const $posix$IO$succeed = (($a) => {
  return ($posix$IO$IO)((($never) => {
    return ($core$Result$Ok)($a);
  }));
});

const $posix$IO$writeFile = ((_0, _1) => {
  return (sp_todo)("io.writeFile");
});

const $posix$IO$writeStderr = ((_0) => {
  return (sp_todo)("io.writeStderr");
});

const $posix$IO$writeStdout = ((_0) => {
  return (sp_todo)("io.writeStdout");
});

const $posix$Path$dirname = ((_0) => {
  return (sp_todo)("Path.dirname");
});

const $posix$Path$resolve = ((_0) => {
  return (sp_todo)("Path.resolve");
});

const $sd1$Compile$asModule = (($tuple) => {
  const $2 = $tuple;
  const $name = $2.second;
  const $isDirectory = $2.first;
  return (($isDirectory || (sp_not_equal)(((text_startsWithRegex)("[A-Z][a-zA-Z0-9_]*[.]sp$"))($name), $name))
    ? $core$Maybe$Nothing
    : ($core$Maybe$Just)(((($2) => {
      return ($core$Text$replace)(".sp", "", $2);
    }))($name)));
});

const $sd1$Compile$asModuleDirectory = (($tuple) => {
  const $2 = $tuple;
  const $name = $2.second;
  const $isDirectory = $2.first;
  return (($isDirectory && (sp_equal)(((text_startsWithRegex)("^[A-Z][a-zA-Z0-9_]*$"))($name), $name))
    ? ($core$Maybe$Just)($name)
    : $core$Maybe$Nothing);
});

const $sd1$Compile$libDirectoryName = "lib";

const $sd1$Compile$listSourceDir = (($sourceDirRoot, $modulePathWithTrailingSlash) => {
  const $path = ($sourceDirRoot + ("/" + $modulePathWithTrailingSlash));
  return (($posix$IO$onSuccess)((($dirContents) => {
    const $directChildren = ((($1) => {
      return ($core$List$map)((($fileName) => {
        return ($modulePathWithTrailingSlash + $fileName);
      }), $1);
    }))(((($1) => {
      return ($core$List$filterMap)($sd1$Compile$asModule, $1);
    }))($dirContents));
    const $getDescendants = ($posix$IO$parallel)(((($1) => {
      return ($core$List$map)((($subDir) => {
        return ($sd1$Compile$listSourceDir)($sourceDirRoot, ($modulePathWithTrailingSlash + ($subDir + "/")));
      }), $1);
    }))(((($1) => {
      return ($core$List$filterMap)($sd1$Compile$asModuleDirectory, $1);
    }))($dirContents)));
    return (($posix$IO$onSuccess)((($descendants) => {
      return ($posix$IO$succeed)(($core$List$concat)(($core$Core$Cons)($directChildren, ($core$Core$Cons)(($core$List$concat)($descendants), $core$Core$Nil))));
    })))($getDescendants);
  })))(($posix$IO$readDir)($path));
});

const $sd1$Compile$modulesFileName = "modules.sp";

const $sd1$Term$color = (($code) => {
  return (($text) => {
    return ($code + ($text + "\x1b[0m"));
  });
});

const $sd1$Term$blue = ($sd1$Term$color)("\x1b[34m");

const $sd1$Term$red = ($sd1$Term$color)("\x1b[31m");

const $sd1$Term$yellow = ($sd1$Term$color)("\x1b[33m");

const $sd1$Compile$formattedToConsoleColoredText = (($formattedText) => {
  return ((($formattedText)[0] === "FormattedText_Default")
    ? ((() => {
      const $t = ($formattedText)[1];
      return $t;
    }))()
    : ((($formattedText)[0] === "FormattedText_Emphasys")
      ? ((() => {
        const $t = ($formattedText)[1];
        return ($sd1$Term$yellow)($t);
      }))()
      : ((($formattedText)[0] === "FormattedText_Warning")
        ? ((() => {
          const $t = ($formattedText)[1];
          return ($sd1$Term$red)($t);
        }))()
        : ((($formattedText)[0] === "FormattedText_Decoration")
          ? ((() => {
            const $t = ($formattedText)[1];
            return ($sd1$Term$blue)($t);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 20:4', (sp_toHuman)($formattedText))))));
});

const $sd1$Compiler$Error$formatSeparator = "$|$|$";

const $sd1$Compiler$Error$formatSuffix = "$`$`$";

const $sd1$Compiler$Error$breakDownText = (($text) => {
  const $formatSnippet = (($index, $snippet) => {
    return ((sp_equal)((basics_modBy)(2, $index), 0)
      ? ($sd1$Compiler$Error$FormattedText_Default)($snippet)
      : ((() => {
        const $4 = (text_split)($sd1$Compiler$Error$formatSuffix, $snippet);
        return (((($4)[0] === "Cons") && (("emphasys" === ($4)[1]) && (((($4)[2])[0] === "Cons") && (((($4)[2])[2])[0] === "Nil"))))
          ? ((() => {
            const $s = (($4)[2])[1];
            return ($sd1$Compiler$Error$FormattedText_Emphasys)($s);
          }))()
          : (((($4)[0] === "Cons") && (("warning" === ($4)[1]) && (((($4)[2])[0] === "Cons") && (((($4)[2])[2])[0] === "Nil"))))
            ? ((() => {
              const $s = (($4)[2])[1];
              return ($sd1$Compiler$Error$FormattedText_Warning)($s);
            }))()
            : (((($4)[0] === "Cons") && (("decoration" === ($4)[1]) && (((($4)[2])[0] === "Cons") && (((($4)[2])[2])[0] === "Nil"))))
              ? ((() => {
                const $s = (($4)[2])[1];
                return ($sd1$Compiler$Error$FormattedText_Decoration)($s);
              }))()
              : (true
                ? ($sd1$Compiler$Error$FormattedText_Default)($snippet)
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 70:10', (sp_toHuman)($4))))));
      }))());
  });
  return ((($1) => {
    return ($core$List$indexedMap)($formatSnippet, $1);
  }))(((($1) => {
    return (text_split)($sd1$Compiler$Error$formatSeparator, $1);
  }))($text));
});

const $sd1$Compiler$Error$rawToText = (($desc) => {
  const $description = ((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($s) => {
      return ("  " + $s);
    }), $1);
  }))(((($1) => {
    return ($core$List$concatMap)((($1) => {
      return (text_split)("\n", $1);
    }), $1);
  }))($desc)));
  return ($sd1$Compiler$Error$breakDownText)(((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(($core$Core$Cons)("", ($core$Core$Cons)("", ($core$Core$Cons)($description, ($core$Core$Cons)("", $core$Core$Nil))))));
});

const $sd1$Compiler$Error$formatWrap = (($fmtName, $text) => {
  return ($sd1$Compiler$Error$formatSeparator + ($fmtName + ($sd1$Compiler$Error$formatSuffix + ($text + $sd1$Compiler$Error$formatSeparator))));
});

const $sd1$Compiler$Error$deco = (($1) => {
  return ($sd1$Compiler$Error$formatWrap)("decoration", $1);
});

const $sd1$Compiler$Error$positionToLineAndColumn = (($s, $index) => {
  const $before = (text_slice)(0, $index, $s);
  const $lines = (text_split)("\n", $before);
  const $lineNumber = ($core$List$length)($lines);
  const $colNumber = ((($1) => {
    return ($core$Maybe$withDefault)(0, $1);
  }))(((($1) => {
    return ($core$Maybe$map)(text_length, $1);
  }))(($core$List$last)($lines)));
  return ({
    col: $colNumber,
    line: $lineNumber,
  });
});

const $sd1$Compiler$Error$highlightSplit = (($h, $x) => {
  const $3 = $x;
  const $lines = $3.second;
  const $words = $3.first;
  return ((($h)[0] === "HighlightWord")
    ? ((() => {
      const $colEnd = ($h)[1].colEnd;
      const $colStart = ($h)[1].colStart;
      const $line = ($h)[1].line;
      return ({
        first: ($core$Dict$insert)($line, ({
          first: $colStart,
          second: $colEnd,
        }), $words),
        second: $lines,
      });
    }))()
    : ((($h)[0] === "HighlightBlock")
      ? ((() => {
        const $lineEnd = ($h)[1].lineEnd;
        const $lineStart = ($h)[1].lineStart;
        return ({
          first: $words,
          second: ($core$List$for)($lines, ($core$List$range)($lineStart, $lineEnd), $core$Set$insert),
        });
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 129:4', (sp_toHuman)($h))));
});

const $sd1$Compiler$Error$warn = (($1) => {
  return ($sd1$Compiler$Error$formatWrap)("warning", $1);
});

const $sd1$Compiler$Error$fmtBlock = (($start, $highlights, $ls) => {
  const $4 = ($core$List$for)(({
    first: $core$Dict$empty,
    second: $core$Set$empty,
  }), $highlights, $sd1$Compiler$Error$highlightSplit);
  const $highlightedLines = $4.second;
  const $highlightedWords = $4.first;
  const $pad = (text_length)((text_fromNumber)(($start + ($core$List$length)($ls))));
  const $wordHighlight = (($lineNumber) => {
    const $6 = ($core$Dict$get)($lineNumber, $highlightedWords);
    return ((($6)[0] === "Nothing")
      ? ""
      : ((($6)[0] === "Just")
        ? ((() => {
          const $s = ($6)[1].first;
          const $e = ($6)[1].second;
          return ("\n" + (($core$Text$repeat)($pad, " ") + ("   " + (($core$Text$repeat)(($s - 1), " ") + ($sd1$Compiler$Error$warn)(($core$Text$repeat)(($core$Basics$max)(1, ($e - $s)), "^"))))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 150:8', (sp_toHuman)($6))));
  });
  const $lineDem = (($lineIndex) => {
    return (($core$Set$member)($lineIndex, $highlightedLines)
      ? ($sd1$Compiler$Error$warn)(" > ")
      : " | ");
  });
  const $fmtLine = (($i, $line) => {
    const $index = ($i + $start);
    const $s = ((($2) => {
      return ($core$Text$padLeft)($pad, " ", $2);
    }))((text_fromNumber)($index));
    return ($s + (($lineDem)($index) + ($line + ($wordHighlight)($index))));
  });
  return ((($s) => {
    return ($s + "\n");
  }))(((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$indexedMap)($fmtLine, $1);
  }))($ls)));
});

const $sd1$Compiler$Error$showCodeBlock = (($code, $start, $end) => {
  return (($end.line < 0)
    ? ""
    : ((() => {
      const $highlight = ((sp_not_equal)($start.line, $end.line)
        ? ($sd1$Compiler$Error$HighlightBlock)(({
          lineEnd: $end.line,
          lineStart: $start.line,
        }))
        : ($sd1$Compiler$Error$HighlightWord)(({
          colEnd: $end.col,
          colStart: $start.col,
          line: $start.line,
        })));
      const $extraLines = 2;
      const $lines = (text_split)("\n", $code);
      const $maxLines = ($core$List$length)($lines);
      const $startLine = ($core$Basics$clamp)(0, ($maxLines - 1), ($start.line - ($extraLines - 1)));
      const $endLine = ($core$Basics$clamp)(0, ($maxLines - 1), ($end.line + $extraLines));
      const $size = ($core$Basics$max)(1, ($endLine - $startLine));
      return ((($2) => {
        return ($sd1$Compiler$Error$fmtBlock)(($startLine + 1), ($core$Core$Cons)($highlight, $core$Core$Nil), $2);
      }))(((($1) => {
        return ($core$List$take)($size, $1);
      }))(((($1) => {
        return ($core$List$drop)($startLine, $1);
      }))($lines)));
    }))());
});

const $sd1$Compiler$Error$posToHuman = (($mod, $pos) => {
  const $noBlock = (($loc) => {
    return ({
      block: "",
      location: $loc,
    });
  });
  return ((($pos)[0] === "P")
    ? ((() => {
      const $startAsInt = ($pos)[1];
      const $endAsInt = ($pos)[2];
      const $start = ($sd1$Compiler$Error$positionToLineAndColumn)($mod.content, $startAsInt);
      const $end = ($sd1$Compiler$Error$positionToLineAndColumn)($mod.content, $endAsInt);
      return ({
        block: ($sd1$Compiler$Error$showCodeBlock)($mod.content, $start, $end),
        location: ($mod.fsPath + (" " + ((text_fromNumber)($start.line) + (":" + (text_fromNumber)($start.col))))),
      });
    }))()
    : ((($pos)[0] === "End")
      ? ((() => {
        const $end = ($sd1$Compiler$Error$positionToLineAndColumn)($mod.content, ((text_length)($mod.content) - 1));
        const $start = ({
          col: 0,
          line: ($end.line - 8),
        });
        return ({
          block: ($sd1$Compiler$Error$showCodeBlock)($mod.content, $start, $end),
          location: ($mod.fsPath + (" " + ((text_fromNumber)($end.line) + ":0 (end of file)"))),
        });
      }))()
      : ((($pos)[0] === "N")
        ? ($noBlock)("<native code>")
        : ((($pos)[0] === "S")
          ? ($noBlock)("<the location information has been stripped>")
          : ((($pos)[0] === "T")
            ? ($noBlock)("<defined in test modules>")
            : ((($pos)[0] === "I")
              ? ((() => {
                const $n = ($pos)[1];
                return ($noBlock)(("<inferred " + ((text_fromNumber)($n) + ">")));
              }))()
              : ((($pos)[0] === "G")
                ? ($noBlock)("<generated>")
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 238:4', (sp_toHuman)($pos)))))))));
});

const $sd1$Compiler$Error$simpleToText = (($mod, $pos, $desc) => {
  const $4 = ($sd1$Compiler$Error$posToHuman)($mod, $pos);
  const $location = $4.location;
  const $block = $4.block;
  const $description = ((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($s) => {
      return ("  " + $s);
    }), $1);
  }))(((($1) => {
    return ($core$List$concatMap)((($1) => {
      return (text_split)("\n", $1);
    }), $1);
  }))(($core$Core$Cons)($block, $desc))));
  return ($sd1$Compiler$Error$breakDownText)(((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(($core$Core$Cons)("", ($core$Core$Cons)("", ($core$Core$Cons)(($sd1$Compiler$Error$deco)(($core$Text$padRight)(50, "-", ($location + " "))), ($core$Core$Cons)("", ($core$Core$Cons)($description, ($core$Core$Cons)("", $core$Core$Nil))))))));
});

const $sd1$Compiler$Error$flatten = (($e, $accum) => {
  return ((($e)[0] === "Simple")
    ? ((() => {
      const $mod = ($e)[1];
      const $pos = ($e)[2];
      const $desc = ($e)[3];
      return ($core$List$concat)(($core$Core$Cons)($accum, ($core$Core$Cons)(($sd1$Compiler$Error$simpleToText)($mod, $pos, $desc), $core$Core$Nil)));
    }))()
    : ((($e)[0] === "Raw")
      ? ((() => {
        const $desc = ($e)[1];
        return ($core$List$concat)(($core$Core$Cons)($accum, ($core$Core$Cons)(($sd1$Compiler$Error$rawToText)($desc), $core$Core$Nil)));
      }))()
      : ((($e)[0] === "Nested")
        ? ((() => {
          const $ls = ($e)[1];
          return ($core$List$for)($accum, $ls, $sd1$Compiler$Error$flatten);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 42:4', (sp_toHuman)($e)))));
});

const $sd1$Compiler$Error$toFormattedText = (($0) => {
  return ($sd1$Compiler$Error$flatten)($0, $core$Core$Nil);
});

const $sd1$Compile$resToIo = (($res) => {
  return ((($res)[0] === "Ok")
    ? ((() => {
      const $a = ($res)[1];
      return ($posix$IO$succeed)($a);
    }))()
    : ((($res)[0] === "Err")
      ? ((() => {
        const $e = ($res)[1];
        return ($posix$IO$fail)(((($1) => {
          return ($core$Text$join)("", $1);
        }))(((($1) => {
          return ($core$List$map)($sd1$Compile$formattedToConsoleColoredText, $1);
        }))(($sd1$Compiler$Error$toFormattedText)($e))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 29:4', (sp_toHuman)($res))));
});

const $sd1$ModulesFile$initModulesFile = ({
  libraries: $core$Core$Nil,
  sourceDirs: $core$Core$Nil,
});

const $sd1$SPON$expressionToStatements = (($e) => {
  return (((($e)[0] === "Expression") && (((($e)[2])[0] === "Statements") && ((((($e)[2])[1])[0] === "Cons") && (((((($e)[2])[1])[1])[0] === "Evaluation") && ((((($e)[2])[1])[2])[0] === "Nil")))))
    ? ((() => {
      const $nested = (((($e)[2])[1])[1])[1];
      return ($sd1$SPON$expressionToStatements)($nested);
    }))()
    : (((($e)[0] === "Expression") && ((($e)[2])[0] === "Statements"))
      ? ((() => {
        const $stats = (($e)[2])[1];
        return $stats;
      }))()
      : (true
        ? ($core$Core$Cons)(($sd1$Types$FormattableAst$Evaluation)($e), $core$Core$Nil)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 215:4', (sp_toHuman)($e)))));
});

const $sd1$Types$FormattableAst$statementPos = (($statement) => {
  return (((($statement)[0] === "Evaluation") && ((($statement)[1])[0] === "Expression"))
    ? ((() => {
      const $pos = (($statement)[1])[1];
      const $expr_ = (($statement)[1])[2];
      return $pos;
    }))()
    : (((($statement)[0] === "ValueDef") && ((($statement)[1].pattern)[0] === "Expression"))
      ? ((() => {
        const $body = ($statement)[1].body;
        const $nonFn = ($statement)[1].nonFn;
        const $pos = (($statement)[1].pattern)[1];
        const $expr_ = (($statement)[1].pattern)[2];
        return $pos;
      }))()
      : (((($statement)[0] === "AliasDef") && ((($statement)[1].name)[0] === "At"))
        ? ((() => {
          const $args = ($statement)[1].args;
          const $pos = (($statement)[1].name)[1];
          const $type = ($statement)[1].type;
          return $pos;
        }))()
        : (((($statement)[0] === "UnionDef") && ((($statement)[1].name)[0] === "At"))
          ? ((() => {
            const $args = ($statement)[1].args;
            const $constructors = ($statement)[1].constructors;
            const $pos = (($statement)[1].name)[1];
            return $pos;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 127:4', (sp_toHuman)($statement))))));
});

const $sd1$SPON$field = (($fieldName, $fieldReader) => {
  return (($statements) => {
    return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "ValueDef") && ((((($statements)[1])[1].pattern)[0] === "Expression") && (((((($statements)[1])[1].pattern)[2])[0] === "Variable") && ((((((($statements)[1])[1].pattern)[2])[1].maybeType)[0] === "Nothing") && ((((((($statements)[1])[1].pattern)[2])[1].word.attrPath)[0] === "Nil") && ((((((($statements)[1])[1].pattern)[2])[1].word.maybeModule)[0] === "Nothing") && (((((($statements)[1])[1].pattern)[2])[1].word.modifier)[0] === "NameNoModifier"))))))))
      ? ((() => {
        const $body = (($statements)[1])[1].body;
        const $nonFn = (($statements)[1])[1].nonFn;
        const $pos = ((($statements)[1])[1].pattern)[1];
        const $name = (((($statements)[1])[1].pattern)[2])[1].word.name;
        const $tail = ($statements)[2];
        return ((sp_equal)($name, $fieldName)
          ? ((() => {
            const $4 = ($fieldReader)(($sd1$SPON$expressionToStatements)($body));
            return ((($4)[0] === "Accepted")
              ? ((() => {
                const $unreadStatements = ($4)[1];
                const $a = ($4)[2];
                return ((($unreadStatements)[0] === "Nil")
                  ? ($sd1$SPON$Accepted)($tail, $a)
                  : ((($unreadStatements)[0] === "Cons")
                    ? ((() => {
                      const $head = ($unreadStatements)[1];
                      return ($sd1$SPON$Failed)(((($1) => {
                        return ($sd1$Types$Pos$At)(($sd1$Types$FormattableAst$statementPos)($head), $1);
                      }))(("Could not make sense of all the statements in field `" + ($fieldName + "`."))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 249:28', (sp_toHuman)($unreadStatements))));
              }))()
              : (true
                ? ((() => {
                  const $otherwise = $4;
                  return $otherwise;
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 247:20', (sp_toHuman)($4))));
          }))()
          : ($sd1$SPON$Rejected)(((($1) => {
            return ($sd1$Types$Pos$At)($pos, $1);
          }))(("expecting `" + ($fieldName + " =`")))));
      }))()
      : ((($statements)[0] === "Cons")
        ? ((() => {
          const $head = ($statements)[1];
          const $tail = ($statements)[2];
          return ($sd1$SPON$Rejected)(($sd1$Types$Pos$At)(($sd1$Types$FormattableAst$statementPos)($head), "missing a simple assignment (ie `something = `)"));
        }))()
        : ((($statements)[0] === "Nil")
          ? ($sd1$SPON$Rejected)(($sd1$Types$Pos$At)($sd1$Types$Pos$End, "unexpected end of file"))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 225:4', (sp_toHuman)($statements)))));
  });
});

const $sd1$SPON$onAcc = (($chainedReaderB) => {
  return (($readerA) => {
    return (($statements) => {
      const $4 = ($readerA)($statements);
      return ((($4)[0] === "Accepted")
        ? ((() => {
          const $newStatements = ($4)[1];
          const $a = ($4)[2];
          return (($chainedReaderB)($a))($newStatements);
        }))()
        : ((($4)[0] === "Rejected")
          ? ((() => {
            const $reason = ($4)[1];
            return ($sd1$SPON$Rejected)($reason);
          }))()
          : ((($4)[0] === "Failed")
            ? ((() => {
              const $reason = ($4)[1];
              return ($sd1$SPON$Failed)($reason);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 18:4', (sp_toHuman)($4)))));
    });
  });
});

const $sd1$SPON$getPos = (($statements) => {
  return ((($statements)[0] === "Cons")
    ? ((() => {
      const $head = ($statements)[1];
      const $tail = ($statements)[2];
      return ($sd1$Types$FormattableAst$statementPos)($head);
    }))()
    : ((($statements)[0] === "Nil")
      ? $sd1$Types$Pos$End
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 36:4', (sp_toHuman)($statements))));
});

const $sd1$SPON$reject = (($message) => {
  return (($statements) => {
    return ($sd1$SPON$Rejected)(($sd1$Types$Pos$At)(($sd1$SPON$getPos)($statements), $message));
  });
});

const $sd1$SPON$return = (($a) => {
  return (($statements) => {
    return ($sd1$SPON$Accepted)($statements, $a);
  });
});

const $sd1$SPON$word = (($statements) => {
  return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Evaluation") && ((((($statements)[1])[1])[0] === "Expression") && ((((($statements)[1])[1])[2])[0] === "Variable"))))
    ? ((() => {
      const $maybeType = (((($statements)[1])[1])[2])[1].maybeType;
      const $word = (((($statements)[1])[1])[2])[1].word;
      const $tail = ($statements)[2];
      return ($sd1$SPON$Accepted)($tail, $word);
    }))()
    : (((($statements)[0] === "Cons") && ((($statements)[2])[0] === "Nil"))
      ? ((() => {
        const $s = ($statements)[1];
        return ($sd1$SPON$Rejected)(($sd1$Types$Pos$At)(($sd1$Types$FormattableAst$statementPos)($s), "expecting an Uppercase name"));
      }))()
      : (true
        ? ($sd1$SPON$Failed)(($sd1$Types$Pos$At)($sd1$Types$Pos$End, "expecting a statement"))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 109:4', (sp_toHuman)($statements)))));
});

const $sd1$SPON$lowerOrUpperName = (($sd1$SPON$onAcc)((($w) => {
  return (((($w.attrPath)[0] === "Nil") && ((($w.maybeModule)[0] === "Nothing") && (($w.modifier)[0] === "NameNoModifier")))
    ? ((() => {
      const $name = $w.name;
      return ($sd1$SPON$return)($name);
    }))()
    : (true
      ? ($sd1$SPON$reject)("expecting an upper or lower case name")
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 133:4', (sp_toHuman)($w))));
})))($sd1$SPON$word);

const $sd1$SPON$many = (($readerA) => {
  const $rec = (($accum) => {
    return (($statements) => {
      return ((sp_equal)($statements, $core$Core$Nil)
        ? ($sd1$SPON$Accepted)($core$Core$Nil, ($core$List$reverse)($accum))
        : ((() => {
          const $4 = ($readerA)($statements);
          return ((($4)[0] === "Accepted")
            ? ((() => {
              const $tail = ($4)[1];
              const $a = ($4)[2];
              return (($rec)((sp_cons)($a, $accum)))($tail);
            }))()
            : ((($4)[0] === "Rejected")
              ? ((() => {
                const $e = ($4)[1];
                return ($sd1$SPON$Rejected)($e);
              }))()
              : ((($4)[0] === "Failed")
                ? ((() => {
                  const $e = ($4)[1];
                  return ($sd1$SPON$Failed)($e);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 185:12', (sp_toHuman)($4)))));
        }))());
    });
  });
  return ($rec)($core$Core$Nil);
});

const $sd1$SPON$maybe = (($readerA) => {
  return (($statements) => {
    const $3 = ($readerA)($statements);
    return ((($3)[0] === "Accepted")
      ? ((() => {
        const $tail = ($3)[1];
        const $a = ($3)[2];
        return ($sd1$SPON$Accepted)($tail, ($core$Maybe$Just)($a));
      }))()
      : ((($3)[0] === "Rejected")
        ? ($sd1$SPON$Accepted)($statements, $core$Maybe$Nothing)
        : ((($3)[0] === "Failed")
          ? ((() => {
            const $r = ($3)[1];
            return ($sd1$SPON$Failed)($r);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 201:4', (sp_toHuman)($3)))));
  });
});

const $sd1$SPON$upperName = (($sd1$SPON$onAcc)((($w) => {
  return (((($w.attrPath)[0] === "Nil") && ($w.isUpper && ((($w.maybeModule)[0] === "Nothing") && (($w.modifier)[0] === "NameNoModifier"))))
    ? ((() => {
      const $name = $w.name;
      return ($sd1$SPON$return)($name);
    }))()
    : (true
      ? ($sd1$SPON$reject)("expecting an upper case name")
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 123:4', (sp_toHuman)($w))));
})))($sd1$SPON$word);

const $sd1$ModulesFile$moduleReader = (($sd1$SPON$onAcc)((($path) => {
  return (($sd1$SPON$onAcc)((($visibleAs) => {
    return (($sd1$SPON$onAcc)((($globalTypes) => {
      return (($sd1$SPON$onAcc)((($globalValues) => {
        return ($sd1$SPON$return)(({
          globalTypes: ($core$Maybe$withDefault)($core$Core$Nil, $globalTypes),
          globalValues: ($core$Maybe$withDefault)($core$Core$Nil, $globalValues),
          path: $path,
          visibleAs: ($core$Maybe$withDefault)($path, $visibleAs),
        }));
      })))(($sd1$SPON$maybe)(($sd1$SPON$field)("globalValues", ($sd1$SPON$many)($sd1$SPON$lowerOrUpperName))));
    })))(($sd1$SPON$maybe)(($sd1$SPON$field)("globalTypes", ($sd1$SPON$many)($sd1$SPON$upperName))));
  })))(($sd1$SPON$maybe)(($sd1$SPON$field)("importAs", $sd1$SPON$upperName)));
})))(($sd1$SPON$field)("path", $sd1$SPON$upperName));

const $sd1$SPON$text = (($statements) => {
  return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Evaluation") && ((((($statements)[1])[1])[0] === "Expression") && (((((($statements)[1])[1])[2])[0] === "LiteralText") && ((($statements)[2])[0] === "Nil")))))
    ? ((() => {
      const $t = (((($statements)[1])[1])[2])[1];
      return ($sd1$SPON$Accepted)($core$Core$Nil, $t);
    }))()
    : (((($statements)[0] === "Cons") && ((($statements)[2])[0] === "Nil"))
      ? ((() => {
        const $s = ($statements)[1];
        return ($sd1$SPON$Rejected)(($sd1$Types$Pos$At)(($sd1$Types$FormattableAst$statementPos)($s), "expecting a text literal"));
      }))()
      : (true
        ? ($sd1$SPON$Failed)(($sd1$Types$Pos$At)($sd1$Types$Pos$End, "expecting a single statement"))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 96:4', (sp_toHuman)($statements)))));
});

const $sd1$ModulesFile$libraryReader = (($sd1$SPON$onAcc)((($source) => {
  return (($sd1$SPON$onAcc)((($modules) => {
    return ($sd1$SPON$return)(({
      modules: $modules,
      source: $source,
    }));
  })))(($sd1$SPON$many)(($sd1$SPON$field)("module", $sd1$ModulesFile$moduleReader)));
})))(($sd1$SPON$field)("source", $sd1$SPON$text));

const $sd1$ModulesFile$sourceDirectoryReader = (($sd1$SPON$onAcc)((($path) => {
  return (($sd1$SPON$onAcc)((($modules) => {
    return ($sd1$SPON$return)(({
      modules: $modules,
      path: $path,
    }));
  })))(($sd1$SPON$many)(($sd1$SPON$field)("module", $sd1$ModulesFile$moduleReader)));
})))(($sd1$SPON$field)("path", $sd1$SPON$text));

const $sd1$SPON$oneOf = (($readers) => {
  return (($statements) => {
    return ((($readers)[0] === "Nil")
      ? ((() => {
        const $pos = ((($statements)[0] === "Cons")
          ? ((() => {
            const $head = ($statements)[1];
            return ($sd1$Types$FormattableAst$statementPos)($head);
          }))()
          : (true
            ? $sd1$Types$Pos$End
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 152:16', (sp_toHuman)($statements))));
        return ($sd1$SPON$Rejected)(($sd1$Types$Pos$At)($pos, "options exhausted"));
      }))()
      : ((($readers)[0] === "Cons")
        ? ((() => {
          const $headReader = ($readers)[1];
          const $tail = ($readers)[2];
          const $3 = ($headReader)($statements);
          return ((($3)[0] === "Rejected")
            ? (($sd1$SPON$oneOf)($tail))($statements)
            : (true
              ? ((() => {
                const $otherwise = $3;
                return $otherwise;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 159:12', (sp_toHuman)($3))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 149:4', (sp_toHuman)($readers))));
  });
});

const $sd1$ModulesFile$modulesFileReader = ($sd1$SPON$many)(($sd1$SPON$oneOf)(($core$Core$Cons)((($sd1$SPON$onAcc)((($lib) => {
  return ($sd1$SPON$return)(($sd1$ModulesFile$Lib)($lib));
})))(($sd1$SPON$field)("library", $sd1$ModulesFile$libraryReader)), ($core$Core$Cons)((($sd1$SPON$onAcc)((($dir) => {
  return ($sd1$SPON$return)(($sd1$ModulesFile$Dir)($dir));
})))(($sd1$SPON$field)("sourceDir", $sd1$ModulesFile$sourceDirectoryReader)), $core$Core$Nil))));

const $sd1$Compiler$Lexer$getPos = (($state) => {
  return ([
    ((__re__ = (basics_cloneUni)($state.nextPos)), ($state.nextPos = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$closeOpenBlocks = (($state) => {
  const $pos = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  const $s = ((__re__ = (array_toList)($state.indentStack)), ($state.indentStack = (__re__)[1]), (__re__)[0]);
  ($core$List$each)($s, ((_0) => {
    return ((__re__ = (array_push)($state.tokens, ($sd1$Types$Token$Token)($sd1$Types$Token$N, $pos, $pos, $sd1$Types$Token$BlockEnd))), ($state.tokens = (__re__)[1]), (__re__)[0]);
  }));
  return ([
    ((__re__ = (array_push)($state.sections, ((__re__ = (array_toList)($state.tokens)), ($state.tokens = (__re__)[1]), (__re__)[0]))), ($state.sections = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$addIndentToken = (($pos, $kind, $state) => {
  return ([
    ((__re__ = (array_push)($state.tokens, ($sd1$Types$Token$Token)($sd1$Types$Token$N, $pos, $pos, $kind))), ($state.tokens = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$updateIndent = (($start, $end, $kind, $state) => {
  const $manageIndent = (($head) => {
    const $lineIndent = ((__re__ = (basics_cloneUni)($state.lineIndent)), ($state.lineIndent = (__re__)[1]), (__re__)[0]);
    return (($lineIndent > $head.indent)
      ? ((() => {
        const $newIndent = ({
          indent: $lineIndent,
          isBlock: ((__re__ = (basics_cloneUni)($state.indentStartsABlock)), ($state.indentStartsABlock = (__re__)[1]), (__re__)[0]),
        });
        ((__re__ = (array_push)($state.indentStack, $newIndent)), ($state.indentStack = (__re__)[1]), (__re__)[0]);
        return (((__re__ = (basics_cloneUni)($state.indentStartsABlock)), ($state.indentStartsABlock = (__re__)[1]), (__re__)[0])
          ? ((__re__ = ($sd1$Compiler$Lexer$addIndentToken)($start, $sd1$Types$Token$BlockStart, $state)), ($state = (__re__)[1]), (__re__)[0])
          : null);
      }))()
      : ($head.isBlock
        ? ((() => {
          const $list = ((__re__ = (array_toList)($state.tokens)), ($state.tokens = (__re__)[1]), (__re__)[0]);
          (((sp_not_equal)(((__re__ = (basics_cloneUni)($state.lineIndent)), ($state.lineIndent = (__re__)[1]), (__re__)[0]), 0) || (sp_equal)($list, $core$Core$Nil))
            ? null
            : ((() => {
              ((__re__ = (array_push)($state.sections, $list)), ($state.sections = (__re__)[1]), (__re__)[0]);
              return ($state.tokens = (array_fromList)($core$Core$Nil));
            }))());
          return ((__re__ = ($sd1$Compiler$Lexer$addIndentToken)($start, $sd1$Types$Token$NewSiblingLine, $state)), ($state = (__re__)[1]), (__re__)[0]);
        }))()
        : null));
  });
  const $5 = ((__re__ = (array_pop)($state.indentStack)), ($state.indentStack = (__re__)[1]), (__re__)[0]);
  return ([
    ((($5)[0] === "Just")
      ? ((() => {
        const $head = ($5)[1];
        return ((((__re__ = (basics_cloneUni)($state.lineIndent)), ($state.lineIndent = (__re__)[1]), (__re__)[0]) < $head.indent)
          ? ((() => {
            ($head.isBlock
              ? ((__re__ = ($sd1$Compiler$Lexer$addIndentToken)($start, $sd1$Types$Token$BlockEnd, $state)), ($state = (__re__)[1]), (__re__)[0])
              : null);
            return ((__re__ = ($sd1$Compiler$Lexer$updateIndent)($start, $end, $kind, $state)), ($state = (__re__)[1]), (__re__)[0]);
          }))()
          : ((() => {
            ((__re__ = (array_push)($state.indentStack, $head)), ($state.indentStack = (__re__)[1]), (__re__)[0]);
            return ($manageIndent)($head);
          }))());
      }))()
      : ((($5)[0] === "Nothing")
        ? ($manageIndent)(({
          indent: 0,
          isBlock: true,
        }))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 155:4', (sp_toHuman)($5)))),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$absAddToken = (($start, $end, $kind, $state) => {
  (((__re__ = (basics_cloneUni)($state.soFarThereAreNoTokensInThisLine)), ($state.soFarThereAreNoTokensInThisLine = (__re__)[1]), (__re__)[0])
    ? ((() => {
      ($state.soFarThereAreNoTokensInThisLine = false);
      return ((__re__ = ($sd1$Compiler$Lexer$updateIndent)($start, $end, $kind, $state)), ($state = (__re__)[1]), (__re__)[0]);
    }))()
    : null);
  let $indentStartsABlock = ((($kind)[0] === "Then")
    ? true
    : ((($kind)[0] === "Else")
      ? true
      : ((($kind)[0] === "As")
        ? true
        : ((($kind)[0] === "Colon")
          ? true
          : ((($kind)[0] === "Defop")
            ? true
            : (true
              ? false
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 184:8', (sp_toHuman)($kind))))))));
  ($state.indentStartsABlock = $indentStartsABlock);
  ((__re__ = (array_push)($state.tokens, ($sd1$Types$Token$Token)($sd1$Types$Token$N, $start, $end, $kind))), ($state.tokens = (__re__)[1]), (__re__)[0]);
  return ([
    ($state.tokenStart = (basics_cloneImm)($end)),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$addError = (($message, $state) => {
  const $end = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  const $start = ((__re__ = (basics_cloneUni)($state.tokenStart)), ($state.tokenStart = (__re__)[1]), (__re__)[0]);
  const $error = (($0) => {
    return ($sd1$Compiler$Error$Simple)($0, ($sd1$Types$Pos$P)($start, $end), ($core$Core$Cons)($message, $core$Core$Nil));
  });
  ((__re__ = (array_push)($state.errors, $error)), ($state.errors = (__re__)[1]), (__re__)[0]);
  return ([
    ($state.tokenStart = (basics_cloneImm)($end)),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$getChunk = (($buffer, $state) => {
  const $start = ((__re__ = (basics_cloneUni)($state.tokenStart)), ($state.tokenStart = (__re__)[1]), (__re__)[0]);
  const $end = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  return ([
    ({
      first: $start,
      second: $end,
      third: (text_slice)($start, $end, $buffer),
    }),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$addNumberToken = (($isPercent, $buffer, $state) => {
  const $4 = ((__re__ = ($sd1$Compiler$Lexer$getChunk)($buffer, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $chunk = $4.third;
  const $end = $4.second;
  const $start = $4.first;
  return ([
    ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, ($sd1$Types$Token$NumberLiteral)($isPercent, $chunk), $state)), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$relAddToken = (($ds, $de, $kind, $state) => {
  const $pos = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  return ([
    ((__re__ = ($sd1$Compiler$Lexer$absAddToken)(($pos + $ds), ($pos + $de), $kind, $state)), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$addParenOrCommaToken = (($char, $state) => {
  const $add = (($2) => {
    return ((__re__ = ($sd1$Compiler$Lexer$relAddToken)(0, 1, $2, $state)), ($state = (__re__)[1]), (__re__)[0]);
  });
  return ([
    (("(" === $char)
      ? ($add)(($sd1$Types$Token$RoundParen)($sd1$Types$Token$Open))
      : ((")" === $char)
        ? ($add)(($sd1$Types$Token$RoundParen)($sd1$Types$Token$Closed))
        : (("[" === $char)
          ? ($add)(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Open))
          : (("]" === $char)
            ? ($add)(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Closed))
            : (("{" === $char)
              ? ($add)(($sd1$Types$Token$CurlyBrace)($sd1$Types$Token$Open))
              : (("}" === $char)
                ? ($add)(($sd1$Types$Token$CurlyBrace)($sd1$Types$Token$Closed))
                : (("," === $char)
                  ? ($add)($sd1$Types$Token$Comma)
                  : (true
                    ? ((__re__ = ($sd1$Compiler$Lexer$addError)(("I can't make sense of this piece of text: `" + ($char + "`")), $state)), ($state = (__re__)[1]), (__re__)[0])
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 497:4', (sp_toHuman)($char)))))))))),
    $state,
  ]);
});

const $sd1$Compiler$CoreTypes$p = $sd1$Types$Pos$N;

const $sd1$Compiler$CoreTypes$defToType = (($def, $pars) => {
  return ($sd1$Types$CanonicalAst$TypeNamed)($sd1$Compiler$CoreTypes$p, $def.usr, $pars);
});

const $sd1$Compiler$CoreTypes$umr = ($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Core");

const $sd1$Compiler$CoreTypes$makeUsr = (($1) => {
  return ($sd1$Types$Meta$USR)($sd1$Compiler$CoreTypes$umr, $1);
});

const $sd1$Compiler$CoreTypes$numberDef = ({
  constructors: $core$Dict$empty,
  directTypeDeps: $core$Set$empty,
  pars: $core$Core$Nil,
  usr: ($sd1$Compiler$CoreTypes$makeUsr)("Number"),
});

const $sd1$Compiler$CoreTypes$number = ($sd1$Compiler$CoreTypes$defToType)($sd1$Compiler$CoreTypes$numberDef, $core$Core$Nil);

const $sd1$Prelude$numberUsr = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Number"), $1);
});

const $sd1$Types$Ast$toImm = (($raw) => {
  return ({
    raw: $raw,
    uni: $sd1$Types$Ast$Imm,
  });
});

const $sd1$Types$Ast$toUni = (($raw) => {
  return ({
    raw: $raw,
    uni: $sd1$Types$Ast$Uni,
  });
});

const $sd1$Prelude$typeBinopUnique = (($ty) => {
  return ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$N, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParSp)(($sd1$Types$Ast$toImm)($ty)), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParSp)(($sd1$Types$Ast$toImm)($ty)), $core$Core$Nil)), ($sd1$Types$Ast$toUni)($ty));
});

const $sd1$Prelude$add = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Addittive,
  symbol: "+",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$numberUsr)("add"),
});

const $sd1$Compiler$CoreTypes$nameToType = (($name, $args) => {
  return ($sd1$Types$CanonicalAst$TypeNamed)($sd1$Compiler$CoreTypes$p, ($sd1$Compiler$CoreTypes$makeUsr)($name), $args);
});

const $sd1$Compiler$CoreTypes$bool = ($sd1$Compiler$CoreTypes$nameToType)("Bool", $core$Core$Nil);

const $sd1$Prelude$coreUsr = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Core"), $1);
});

const $sd1$Prelude$and_ = ({
  associativity: $sd1$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Logical,
  symbol: "and",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("and_"),
});

const $sd1$Prelude$divide = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Multiplicative,
  symbol: "/",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$numberUsr)("divide"),
});

const $sd1$Prelude$tyVar = (($name) => {
  return ($sd1$Types$CanonicalAst$TypeAnnotationVariable)($sd1$Types$Pos$N, $name);
});

const $sd1$Prelude$tyFn = (($pars, $to) => {
  return ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$N, ($core$List$map)((($p) => {
    return ($sd1$Types$CanonicalAst$ParSp)(($sd1$Types$Ast$toImm)($p));
  }), $pars), ($sd1$Types$Ast$toImm)($to));
});

const $sd1$Prelude$typeBinop = (($left, $right, $return) => {
  return ($sd1$Prelude$tyFn)(($core$Core$Cons)($left, ($core$Core$Cons)($right, $core$Core$Nil)), $return);
});

const $sd1$Prelude$equal = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  precedence: $sd1$Types$Op$Comparison,
  symbol: "==",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("a"), $sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("equal"),
});

const $sd1$Prelude$greaterOrEqualThan = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  precedence: $sd1$Types$Op$Comparison,
  symbol: ">=",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("a"), $sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("greaterOrEqualThan"),
});

const $sd1$Prelude$greaterThan = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  precedence: $sd1$Types$Op$Comparison,
  symbol: ">",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("a"), $sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("greaterThan"),
});

const $sd1$Prelude$lesserOrEqualThan = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  precedence: $sd1$Types$Op$Comparison,
  symbol: "<=",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("a"), $sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("lesserOrEqualThan"),
});

const $sd1$Prelude$lesserThan = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  precedence: $sd1$Types$Op$Comparison,
  symbol: "<",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("a"), $sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("lesserThan"),
});

const $sd1$Compiler$CoreTypes$list = (($item) => {
  return ($sd1$Compiler$CoreTypes$nameToType)("List", ($core$Core$Cons)($item, $core$Core$Nil));
});

const $sd1$Prelude$listUsr = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "List"), $1);
});

const $sd1$Prelude$listCons = ((() => {
  const $item = ($sd1$Prelude$tyVar)("item");
  return ({
    associativity: $sd1$Types$Op$Right,
    nonFn: $core$Core$Nil,
    precedence: $sd1$Types$Op$Cons,
    symbol: "::",
    type: ($sd1$Prelude$typeBinop)($item, ($sd1$Compiler$CoreTypes$list)($item), ($sd1$Compiler$CoreTypes$list)($item)),
    usr: ($sd1$Prelude$listUsr)("stack"),
  });
}))();

const $sd1$Prelude$multiply = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Multiplicative,
  symbol: "*",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$numberUsr)("multiply"),
});

const $sd1$Compiler$CoreTypes$noneName = "None";

const $sd1$Compiler$CoreTypes$none = ($sd1$Compiler$CoreTypes$nameToType)($sd1$Compiler$CoreTypes$noneName, $core$Core$Nil);

const $sd1$Prelude$mutableAdd = ({
  associativity: $sd1$Types$Op$NonAssociative,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Mutop,
  symbol: "+=",
  type: ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$N, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParRe)($sd1$Compiler$CoreTypes$number), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParSp)(({
    raw: $sd1$Compiler$CoreTypes$number,
    uni: $sd1$Types$Ast$Imm,
  })), $core$Core$Nil)), ({
    raw: $sd1$Compiler$CoreTypes$none,
    uni: $sd1$Types$Ast$Imm,
  })),
  usr: ($sd1$Prelude$numberUsr)("mutableAdd"),
});

const $sd1$Prelude$mutableAssign = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Mutop,
  symbol: ":=",
  type: ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$N, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParRe)(($sd1$Prelude$tyVar)("a")), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParSp)(({
    raw: ($sd1$Prelude$tyVar)("a"),
    uni: $sd1$Types$Ast$Uni,
  })), $core$Core$Nil)), ({
    raw: $sd1$Compiler$CoreTypes$none,
    uni: $sd1$Types$Ast$Imm,
  })),
  usr: ($sd1$Prelude$coreUsr)("mutableAssign"),
});

const $sd1$Prelude$mutableSubtract = ({
  associativity: $sd1$Types$Op$NonAssociative,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Mutop,
  symbol: "-=",
  type: ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$N, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParRe)($sd1$Compiler$CoreTypes$number), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParSp)(({
    raw: $sd1$Compiler$CoreTypes$number,
    uni: $sd1$Types$Ast$Imm,
  })), $core$Core$Nil)), ({
    raw: $sd1$Compiler$CoreTypes$none,
    uni: $sd1$Types$Ast$Imm,
  })),
  usr: ($sd1$Prelude$numberUsr)("mutableSubtract"),
});

const $sd1$Prelude$notEqual = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  precedence: $sd1$Types$Op$Comparison,
  symbol: "/=",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("a"), $sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("notEqual"),
});

const $sd1$Prelude$or_ = ({
  associativity: $sd1$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Logical,
  symbol: "or",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$bool),
  usr: ($sd1$Prelude$coreUsr)("or_"),
});

const $sd1$Prelude$sendLeft = ({
  associativity: $sd1$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Pipe,
  symbol: "<<",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyFn)(($core$Core$Cons)(($sd1$Prelude$tyVar)("a"), $core$Core$Nil), ($sd1$Prelude$tyVar)("b")), ($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("b")),
  usr: ($sd1$Prelude$coreUsr)("sendLeft"),
});

const $sd1$Prelude$sendRight = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Pipe,
  symbol: ">>",
  type: ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyFn)(($core$Core$Cons)(($sd1$Prelude$tyVar)("a"), $core$Core$Nil), ($sd1$Prelude$tyVar)("b")), ($sd1$Prelude$tyVar)("b")),
  usr: ($sd1$Prelude$coreUsr)("sendRight"),
});

const $sd1$Prelude$subtract = ({
  associativity: $sd1$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Addittive,
  symbol: "-",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$numberUsr)("subtract"),
});

const $sd1$Compiler$CoreTypes$textDef = ({
  constructors: $core$Dict$empty,
  directTypeDeps: $core$Set$empty,
  pars: $core$Core$Nil,
  usr: ($sd1$Compiler$CoreTypes$makeUsr)("Text"),
});

const $sd1$Compiler$CoreTypes$text = ($sd1$Compiler$CoreTypes$defToType)($sd1$Compiler$CoreTypes$textDef, $core$Core$Nil);

const $sd1$Prelude$textUsr = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Text"), $1);
});

const $sd1$Prelude$textConcat = ({
  associativity: $sd1$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Addittive,
  symbol: "..",
  type: ($sd1$Prelude$typeBinopUnique)($sd1$Compiler$CoreTypes$text),
  usr: ($sd1$Prelude$textUsr)("concat"),
});

const $sd1$Prelude$tupleUsr = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Tuple"), $1);
});

const $sd1$Prelude$tuple = ({
  associativity: $sd1$Types$Op$NonAssociative,
  nonFn: $core$Core$Nil,
  precedence: $sd1$Types$Op$Tuple,
  symbol: "&",
  type: ((($2) => {
    return ($sd1$Prelude$typeBinop)(($sd1$Prelude$tyVar)("a"), ($sd1$Prelude$tyVar)("b"), $2);
  }))(((($1) => {
    return ($sd1$Types$CanonicalAst$TypeRecord)($sd1$Types$Pos$N, $1);
  }))(((($2) => {
    return ($core$Dict$insert)("second", ($sd1$Prelude$tyVar)("b"), $2);
  }))(((($2) => {
    return ($core$Dict$insert)("first", ($sd1$Prelude$tyVar)("a"), $2);
  }))($core$Dict$empty)))),
  usr: ($sd1$Prelude$tupleUsr)(""),
});

const $sd1$Prelude$binops = ($core$Core$Cons)($sd1$Prelude$and_, ($core$Core$Cons)($sd1$Prelude$or_, ($core$Core$Cons)($sd1$Prelude$textConcat, ($core$Core$Cons)($sd1$Prelude$listCons, ($core$Core$Cons)($sd1$Prelude$tuple, ($core$Core$Cons)($sd1$Prelude$add, ($core$Core$Cons)($sd1$Prelude$subtract, ($core$Core$Cons)($sd1$Prelude$multiply, ($core$Core$Cons)($sd1$Prelude$divide, ($core$Core$Cons)($sd1$Prelude$mutableAssign, ($core$Core$Cons)($sd1$Prelude$mutableAdd, ($core$Core$Cons)($sd1$Prelude$mutableSubtract, ($core$Core$Cons)($sd1$Prelude$equal, ($core$Core$Cons)($sd1$Prelude$notEqual, ($core$Core$Cons)($sd1$Prelude$lesserThan, ($core$Core$Cons)($sd1$Prelude$greaterThan, ($core$Core$Cons)($sd1$Prelude$lesserOrEqualThan, ($core$Core$Cons)($sd1$Prelude$greaterOrEqualThan, ($core$Core$Cons)($sd1$Prelude$sendRight, ($core$Core$Cons)($sd1$Prelude$sendLeft, $core$Core$Nil))))))))))))))))))));

const $sd1$Prelude$binopsBySymbol = ($core$List$for)($core$Dict$empty, $sd1$Prelude$binops, (($bop, $d) => {
  return ($core$Dict$insert)($bop.symbol, $bop, $d);
}));

const $sd1$Compiler$Lexer$addSquiggleToken = (($buffer, $nextIsSpace, $state) => {
  const $4 = ((__re__ = ($sd1$Compiler$Lexer$getChunk)($buffer, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $chunk = $4.third;
  const $end = $4.second;
  const $start = $4.first;
  const $add = (($2) => {
    return ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, $2, $state)), ($state = (__re__)[1]), (__re__)[0]);
  });
  return ([
    ((":" === $chunk)
      ? ($add)($sd1$Types$Token$Colon)
      : (("=" === $chunk)
        ? ($add)($sd1$Types$Token$Defop)
        : (("?" === $chunk)
          ? ($add)($sd1$Types$Token$UniquenessPolymorphismBinop)
          : (("!" === $chunk)
            ? ($add)(($sd1$Types$Token$Unop)($sd1$Types$Op$UnopUnique))
            : (("@" === $chunk)
              ? ($add)(($sd1$Types$Token$Unop)($sd1$Types$Op$UnopRecycle))
              : (("-" === $chunk)
                ? ($add)(($nextIsSpace
                  ? ($sd1$Types$Token$Binop)($sd1$Prelude$subtract)
                  : ($sd1$Types$Token$Unop)($sd1$Types$Op$UnopMinus)))
                : (("+" === $chunk)
                  ? ($add)(($nextIsSpace
                    ? ($sd1$Types$Token$Binop)($sd1$Prelude$add)
                    : ($sd1$Types$Token$Unop)($sd1$Types$Op$UnopPlus)))
                  : (true
                    ? ((() => {
                      const $op = $chunk;
                      const $5 = ($core$Dict$get)($chunk, $sd1$Prelude$binopsBySymbol);
                      return ((($5)[0] === "Just")
                        ? ((() => {
                          const $binop = ($5)[1];
                          return ($add)(($sd1$Types$Token$Binop)($binop));
                        }))()
                        : ((($5)[0] === "Nothing")
                          ? ((__re__ = ($sd1$Compiler$Lexer$addError)(("Invalid operator: `" + ($chunk + "`")), $state)), ($state = (__re__)[1]), (__re__)[0])
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 481:12', (sp_toHuman)($5))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 472:4', (sp_toHuman)($chunk)))))))))),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$startsWithUpperChar = ((() => {
  const $re = (text_startsWithRegex)("[A-Z]");
  return (($s) => {
    const $2 = ($re)($s);
    return (("" === $2)
      ? false
      : (true
        ? true
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 281:4', (sp_toHuman)($2))));
  });
}))();

const $sd1$Compiler$Lexer$addLowerOrUpperWord = (($start, $end, $modifier, $chunk, $state) => {
  const $upperName = (($maybeModule, $name) => {
    return ((($modifier)[0] === "NameNoModifier")
      ? ((() => {
        const $word = ({
          attrPath: $core$Core$Nil,
          isUpper: true,
          maybeModule: $maybeModule,
          modifier: $modifier,
          name: $name,
        });
        return ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, ($sd1$Types$Token$Word)($word), $state)), ($state = (__re__)[1]), (__re__)[0]);
      }))()
      : ((($modifier)[0] === "NameStartsWithDot")
        ? ((__re__ = ($sd1$Compiler$Lexer$addError)("Types or constructors can't start with `.` and attribute names can't start with an uppercase letter. =|", $state)), ($state = (__re__)[1]), (__re__)[0])
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 291:8', (sp_toHuman)($modifier))));
  });
  const $lowerName = (($maybeModule, $name, $attrs) => {
    return (($core$List$any)($sd1$Compiler$Lexer$startsWithUpperChar, $attrs)
      ? ((__re__ = ($sd1$Compiler$Lexer$addError)("attribute names must start with a lowercase letter", $state)), ($state = (__re__)[1]), (__re__)[0])
      : (((sp_not_equal)($maybeModule, $core$Maybe$Nothing) && (sp_not_equal)($modifier, $sd1$Types$Token$NameNoModifier))
        ? ((__re__ = ($sd1$Compiler$Lexer$addError)("can't use . or @ modifier on an imported value", $state)), ($state = (__re__)[1]), (__re__)[0])
        : ((() => {
          const $word = ({
            attrPath: $attrs,
            isUpper: false,
            maybeModule: $maybeModule,
            modifier: $modifier,
            name: $name,
          });
          return ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, ($sd1$Types$Token$Word)($word), $state)), ($state = (__re__)[1]), (__re__)[0]);
        }))()));
  });
  const $snips = (text_split)(".", $chunk);
  return ([
    (($core$List$any)((($s) => {
      return (sp_equal)($s, "");
    }), $snips)
      ? ((__re__ = ($sd1$Compiler$Lexer$addError)("use spaces around `..` to concatenate Text", $state)), ($state = (__re__)[1]), (__re__)[0])
      : ((($snips)[0] === "Nil")
        ? (sp_todo)("should not happen")
        : (((($snips)[0] === "Cons") && ((($snips)[2])[0] === "Nil"))
          ? ((() => {
            const $one = ($snips)[1];
            return (($sd1$Compiler$Lexer$startsWithUpperChar)($one)
              ? ($upperName)($core$Maybe$Nothing, $one)
              : ($lowerName)($core$Maybe$Nothing, $one, $core$Core$Nil));
          }))()
          : (((($snips)[0] === "Cons") && ((($snips)[2])[0] === "Cons"))
            ? ((() => {
              const $first = ($snips)[1];
              const $second = (($snips)[2])[1];
              const $more = (($snips)[2])[2];
              const $6 = ({
                first: ($sd1$Compiler$Lexer$startsWithUpperChar)($first),
                second: ($sd1$Compiler$Lexer$startsWithUpperChar)($second),
              });
              return ((!($6.first) && !($6.second))
                ? ($lowerName)($core$Maybe$Nothing, $first, (sp_cons)($second, $more))
                : (($6.first && !($6.second))
                  ? ($lowerName)(($core$Maybe$Just)($first), $second, $more)
                  : (($6.first && $6.second)
                    ? ((sp_not_equal)($more, $core$Core$Nil)
                      ? ((__re__ = ($sd1$Compiler$Lexer$addError)("Types and constructors can't have .attributes", $state)), ($state = (__re__)[1]), (__re__)[0])
                      : ($upperName)(($core$Maybe$Just)($first), $second))
                    : ((!($6.first) && $6.second)
                      ? ((__re__ = ($sd1$Compiler$Lexer$addError)("Something wrong with uppercases?", $state)), ($state = (__re__)[1]), (__re__)[0])
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 353:12', (sp_toHuman)($6))))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 334:6', (sp_toHuman)($snips)))))),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$addWordToken = (($buffer, $modifier, $state) => {
  const $start = ((__re__ = (basics_cloneUni)($state.tokenStart)), ($state.tokenStart = (__re__)[1]), (__re__)[0]);
  const $end = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  const $ds = ((sp_equal)($modifier, $sd1$Types$Token$NameNoModifier)
    ? 0
    : 1);
  const $chunk = (text_slice)(($start + $ds), $end, $buffer);
  const $maybeKeywordKind = (("fn" === $chunk)
    ? ($core$Maybe$Just)($sd1$Types$Token$Fn)
    : (("if" === $chunk)
      ? ($core$Maybe$Just)($sd1$Types$Token$If)
      : (("then" === $chunk)
        ? ($core$Maybe$Just)($sd1$Types$Token$Then)
        : (("else" === $chunk)
          ? ($core$Maybe$Just)($sd1$Types$Token$Else)
          : (("try" === $chunk)
            ? ($core$Maybe$Just)($sd1$Types$Token$Try)
            : (("as" === $chunk)
              ? ($core$Maybe$Just)($sd1$Types$Token$As)
              : (("with" === $chunk)
                ? ($core$Maybe$Just)($sd1$Types$Token$With)
                : (("and" === $chunk)
                  ? ($core$Maybe$Just)(($sd1$Types$Token$Binop)($sd1$Prelude$and_))
                  : (("or" === $chunk)
                    ? ($core$Maybe$Just)(($sd1$Types$Token$Binop)($sd1$Prelude$or_))
                    : (("__" === $chunk)
                      ? ($core$Maybe$Just)($sd1$Types$Token$ArgumentPlaceholder)
                      : (true
                        ? $core$Maybe$Nothing
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 390:8', (sp_toHuman)($chunk)))))))))))));
  const $4 = ({
    first: $maybeKeywordKind,
    second: $modifier,
  });
  return ([
    (((($4.first)[0] === "Just") && (($4.second)[0] === "NameNoModifier"))
      ? ((() => {
        const $kind = ($4.first)[1];
        return ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, $kind, $state)), ($state = (__re__)[1]), (__re__)[0]);
      }))()
      : ((($4.first)[0] === "Just")
        ? ((() => {
          const $kind = ($4.first)[1];
          return ((__re__ = ($sd1$Compiler$Lexer$addError)(($chunk + " as a keyword, you can't really use it this way"), $state)), ($state = (__re__)[1]), (__re__)[0]);
        }))()
        : (true
          ? ((__re__ = ($sd1$Compiler$Lexer$addLowerOrUpperWord)($start, $end, $modifier, $chunk, $state)), ($state = (__re__)[1]), (__re__)[0])
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 403:4', (sp_toHuman)($4))))),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$isNumber = ((() => {
  const $re = (text_startsWithRegex)("[0-9_.]");
  return (($char) => {
    return (sp_not_equal)(($re)($char), "");
  });
}))();

const $sd1$Compiler$Lexer$isSquiggle = (($char) => {
  return (("=" === $char)
    ? true
    : ((":" === $char)
      ? true
      : (("*" === $char)
        ? true
        : (("+" === $char)
          ? true
          : (("-" === $char)
            ? true
            : (("/" === $char)
              ? true
              : ((">" === $char)
                ? true
                : (("<" === $char)
                  ? true
                  : (("!" === $char)
                    ? true
                    : (("?" === $char)
                      ? true
                      : (("&" === $char)
                        ? true
                        : (("^" === $char)
                          ? true
                          : (("@" === $char)
                            ? true
                            : (("$" === $char)
                              ? true
                              : (true
                                ? false
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 445:4', (sp_toHuman)($char)))))))))))))))));
});

const $sd1$Compiler$Lexer$isWordBody = ((() => {
  const $re = (text_startsWithRegex)("[a-zA-Z./_0-9]");
  return (($char) => {
    return (sp_not_equal)(($re)($char), "");
  });
}))();

const $sd1$Compiler$Lexer$isWordStart = ((() => {
  const $re = (text_startsWithRegex)("[a-zA-Z._]");
  return (($char) => {
    return (sp_not_equal)(($re)($char), "");
  });
}))();

const $sd1$Compiler$Lexer$setMode = (($mode, $state) => {
  return ([
    ($state.mode = (basics_cloneImm)($mode)),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$tryIndent = (($buffer, $indentChar, $char, $state) => {
  return ([
    (((sp_equal)($char, $indentChar) || (sp_equal)($char, ""))
      ? null
      : (((sp_equal)($char, " ") || (sp_equal)($char, "\t"))
        ? ((__re__ = ($sd1$Compiler$Lexer$addError)("mixing tabs and spaces!", $state)), ($state = (__re__)[1]), (__re__)[0])
        : ((sp_equal)($char, "\n")
          ? ((() => {
            ($state.tokenStart = (((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]) + 1));
            return ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Indent, $state)), ($state = (__re__)[1]), (__re__)[0]);
          }))()
          : ((sp_equal)($char, "#")
            ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$LineComment, $state)), ($state = (__re__)[1]), (__re__)[0])
            : ((() => {
              ($state.lineIndent = ((__re__ = (basics_cloneUni)($state.column)), ($state.column = (__re__)[1]), (__re__)[0]));
              ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
              return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
            }))())))),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$unindent = (($raw) => {
  return (($core$Basics$not)((text_startsWith)("\n", $raw))
    ? $raw
    : ((() => {
      const $multilineText = (text_dropLeft)(1, $raw);
      const $lines = (text_split)("\n", $multilineText);
      const $countLeadingSpaces = ((() => {
        const $re = (text_startsWithRegex)("[ ]*");
        return (($line) => {
          return (text_length)(($re)($line));
        });
      }))();
      const $minLead = ((($1) => {
        return ($core$Maybe$withDefault)(0, $1);
      }))(($core$List$minimum)(((($1) => {
        return ($core$List$map)($countLeadingSpaces, $1);
      }))(((($1) => {
        return ($core$List$filter)((($s) => {
          return (sp_not_equal)((text_trimLeft)($s), "");
        }), $1);
      }))($lines))));
      return ((($1) => {
        return ((text_replaceRegex)("\n[ ]*$"))("", $1);
      }))(((($1) => {
        return ($core$Text$join)("\n", $1);
      }))(((($1) => {
        return ($core$List$map)((($1) => {
          return (text_dropLeft)($minLead, $1);
        }), $1);
      }))($lines)));
    }))());
});

const $sd1$Compiler$Lexer$lexOne = (($buffer, $char, $state) => {
  const $pos = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  let $4 = ((__re__ = (basics_cloneUni)($state.mode)), ($state.mode = (__re__)[1]), (__re__)[0]);
  return ([
    ((($4)[0] === "Indent")
      ? ((() => {
        let $5 = ((__re__ = (basics_cloneUni)($state.tabsOrSpaces)), ($state.tabsOrSpaces = (__re__)[1]), (__re__)[0]);
        return ((($5)[0] === "Tabs")
          ? ((__re__ = ($sd1$Compiler$Lexer$tryIndent)($buffer, "\t", $char, $state)), ($state = (__re__)[1]), (__re__)[0])
          : ((($5)[0] === "Spaces")
            ? ((__re__ = ($sd1$Compiler$Lexer$tryIndent)($buffer, " ", $char, $state)), ($state = (__re__)[1]), (__re__)[0])
            : ((($5)[0] === "NoTabsOrSpacesYet")
              ? ((" " === $char)
                ? ((() => {
                  ($state.tabsOrSpaces = $sd1$Compiler$Lexer$Spaces);
                  return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                }))()
                : (("\t" === $char)
                  ? ((() => {
                    ($state.tabsOrSpaces = $sd1$Compiler$Lexer$Tabs);
                    return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  }))()
                  : (true
                    ? ((__re__ = ($sd1$Compiler$Lexer$tryIndent)($buffer, " ", $char, $state)), ($state = (__re__)[1]), (__re__)[0])
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 533:14', (sp_toHuman)($char)))))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 525:10', (sp_toHuman)($5)))));
      }))()
      : ((($4)[0] === "Default")
        ? (("" === $char)
          ? null
          : (("." === $char)
            ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Dot_One, $state)), ($state = (__re__)[1]), (__re__)[0])
            : (("#" === $char)
              ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$LineComment, $state)), ($state = (__re__)[1]), (__re__)[0])
              : (("[" === $char)
                ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$ContentOpeningBlockComment, $state)), ($state = (__re__)[1]), (__re__)[0])
                : (("\"" === $char)
                  ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$ContentOpeningQuotes_One, $state)), ($state = (__re__)[1]), (__re__)[0])
                  : (("\n" === $char)
                    ? ((() => {
                      ($state.tokenStart = (((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]) + 1));
                      ($state.soFarThereAreNoTokensInThisLine = true);
                      return ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Indent, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    }))()
                    : ((" " === $char)
                      ? ($state.tokenStart = (((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]) + 1))
                      : (true
                        ? ((() => {
                          ($state.tokenStart = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]));
                          return (($sd1$Compiler$Lexer$isWordStart)($char)
                            ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$Word)($sd1$Types$Token$NameNoModifier), $state)), ($state = (__re__)[1]), (__re__)[0])
                            : (($sd1$Compiler$Lexer$isNumber)($char)
                              ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$NumberLiteral, $state)), ($state = (__re__)[1]), (__re__)[0])
                              : (($sd1$Compiler$Lexer$isSquiggle)($char)
                                ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Squiggles, $state)), ($state = (__re__)[1]), (__re__)[0])
                                : ((__re__ = ($sd1$Compiler$Lexer$addParenOrCommaToken)($char, $state)), ($state = (__re__)[1]), (__re__)[0]))));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 544:10', (sp_toHuman)($char))))))))))
        : ((($4)[0] === "Dot_One")
          ? ((sp_equal)($char, ".")
            ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Dot_Two, $state)), ($state = (__re__)[1]), (__re__)[0])
            : (($sd1$Compiler$Lexer$isWordStart)($char)
              ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$Word)($sd1$Types$Token$NameStartsWithDot), $state)), ($state = (__re__)[1]), (__re__)[0])
              : (($sd1$Compiler$Lexer$isNumber)($char)
                ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$NumberLiteral, $state)), ($state = (__re__)[1]), (__re__)[0])
                : ((__re__ = ($sd1$Compiler$Lexer$addError)("no idea what this is", $state)), ($state = (__re__)[1]), (__re__)[0]))))
          : ((($4)[0] === "Dot_Two")
            ? ((sp_equal)($char, ".")
              ? ((() => {
                ((__re__ = ($sd1$Compiler$Lexer$relAddToken)((0 - 1), 1, $sd1$Types$Token$ThreeDots, $state)), ($state = (__re__)[1]), (__re__)[0]);
                return ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
              }))()
              : ((() => {
                ((__re__ = ($sd1$Compiler$Lexer$relAddToken)((0 - 1), 1, ($sd1$Types$Token$Binop)($sd1$Prelude$textConcat), $state)), ($state = (__re__)[1]), (__re__)[0]);
                ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
              }))())
            : ((($4)[0] === "Mutable")
              ? (($sd1$Compiler$Lexer$isSquiggle)($char)
                ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Squiggles, $state)), ($state = (__re__)[1]), (__re__)[0])
                : ((__re__ = ($sd1$Compiler$Lexer$addError)("no idea what this is", $state)), ($state = (__re__)[1]), (__re__)[0]))
              : ((($4)[0] === "Word")
                ? ((() => {
                  const $modifier = ($4)[1];
                  return (($sd1$Compiler$Lexer$isWordBody)($char)
                    ? null
                    : ((() => {
                      ((__re__ = ($sd1$Compiler$Lexer$addWordToken)($buffer, $modifier, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    }))());
                }))()
                : ((($4)[0] === "NumberLiteral")
                  ? (($sd1$Compiler$Lexer$isNumber)($char)
                    ? null
                    : ((sp_equal)($char, "%")
                      ? ((() => {
                        ((__re__ = ($sd1$Compiler$Lexer$addNumberToken)(true, $buffer, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        return ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      }))()
                      : ((() => {
                        ((__re__ = ($sd1$Compiler$Lexer$addNumberToken)(false, $buffer, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      }))()))
                  : ((($4)[0] === "Squiggles")
                    ? (($sd1$Compiler$Lexer$isSquiggle)($char)
                      ? null
                      : ((() => {
                        ((__re__ = ($sd1$Compiler$Lexer$addSquiggleToken)($buffer, (sp_equal)($char, " "), $state)), ($state = (__re__)[1]), (__re__)[0]);
                        ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      }))())
                    : ((($4)[0] === "ContentOpeningQuotes_One")
                      ? ((sp_equal)($char, "\"")
                        ? ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$ContentOpeningQuotes_Two, $state)), ($state = (__re__)[1]), (__re__)[0])
                        : ((sp_equal)($char, "")
                          ? ((__re__ = ($sd1$Compiler$Lexer$addError)("there's no closing quotes", $state)), ($state = (__re__)[1]), (__re__)[0])
                          : ((() => {
                            ($state.tokenStart = (((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]) - 1));
                            ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$SingleQuote)(({
                              lastEscape: -(1),
                            })), $state)), ($state = (__re__)[1]), (__re__)[0]);
                            return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          }))()))
                      : ((($4)[0] === "ContentOpeningQuotes_Two")
                        ? ((sp_equal)($char, "\"")
                          ? ((() => {
                            ($state.tokenStart = (((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]) - 2));
                            return ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$TripleQuote)(({
                              closingQuotes: 0,
                              lastEscape: -(1),
                            })), $state)), ($state = (__re__)[1]), (__re__)[0]);
                          }))()
                          : ((() => {
                            ((__re__ = ($sd1$Compiler$Lexer$relAddToken)((0 - 2), 0, ($sd1$Types$Token$TextLiteral)(""), $state)), ($state = (__re__)[1]), (__re__)[0]);
                            ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          }))())
                        : ((($4)[0] === "SingleQuote")
                          ? ((() => {
                            const $lastEscape = ($4)[1].lastEscape;
                            const $previousIsEscape = (sp_equal)($pos, ($lastEscape + 1));
                            return ((sp_equal)($char, "")
                              ? ((__re__ = ($sd1$Compiler$Lexer$addError)("there's no closing quotes", $state)), ($state = (__re__)[1]), (__re__)[0])
                              : ($previousIsEscape
                                ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$SingleQuote)(({
                                  lastEscape: $lastEscape,
                                })), $state)), ($state = (__re__)[1]), (__re__)[0])
                                : (("\"" === $char)
                                  ? ((() => {
                                    const $start = ((__re__ = (basics_cloneUni)($state.tokenStart)), ($state.tokenStart = (__re__)[1]), (__re__)[0]);
                                    const $end = ($pos + 1);
                                    const $value = ((($2) => {
                                      return ($core$Text$replace)("\\\"", "\"", $2);
                                    }))(((($2) => {
                                      return (text_slice)(($start + 1), ($end - 1), $2);
                                    }))($buffer));
                                    ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, ($sd1$Types$Token$TextLiteral)($value), $state)), ($state = (__re__)[1]), (__re__)[0]);
                                    return ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                  }))()
                                  : (("\\" === $char)
                                    ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$SingleQuote)(({
                                      lastEscape: $pos,
                                    })), $state)), ($state = (__re__)[1]), (__re__)[0])
                                    : (true
                                      ? null
                                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 679:12', (sp_toHuman)($char)))))));
                          }))()
                          : ((($4)[0] === "TripleQuote")
                            ? ((() => {
                              const $closingQuotes = ($4)[1].closingQuotes;
                              const $lastEscape = ($4)[1].lastEscape;
                              const $previousIsEscape = (sp_equal)($pos, ($lastEscape + 1));
                              return ((sp_equal)($char, "")
                                ? ((__re__ = ($sd1$Compiler$Lexer$addError)("unterminated triple quotes", $state)), ($state = (__re__)[1]), (__re__)[0])
                                : ($previousIsEscape
                                  ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$TripleQuote)(({
                                    closingQuotes: 0,
                                    lastEscape: $lastEscape,
                                  })), $state)), ($state = (__re__)[1]), (__re__)[0])
                                  : (("\"" === $char)
                                    ? ((sp_equal)($closingQuotes, 2)
                                      ? ((() => {
                                        const $start = ((__re__ = (basics_cloneUni)($state.tokenStart)), ($state.tokenStart = (__re__)[1]), (__re__)[0]);
                                        const $end = ($pos + 1);
                                        const $value = ($sd1$Compiler$Lexer$unindent)(((($2) => {
                                          return (text_slice)(($start + 3), ($end - 3), $2);
                                        }))($buffer));
                                        ((__re__ = ($sd1$Compiler$Lexer$absAddToken)($start, $end, ($sd1$Types$Token$TextLiteral)($value), $state)), ($state = (__re__)[1]), (__re__)[0]);
                                        return ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                      }))()
                                      : ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$TripleQuote)(({
                                        closingQuotes: ($closingQuotes + 1),
                                        lastEscape: $lastEscape,
                                      })), $state)), ($state = (__re__)[1]), (__re__)[0]))
                                    : (("\\" === $char)
                                      ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$TripleQuote)(({
                                        closingQuotes: 0,
                                        lastEscape: $pos,
                                      })), $state)), ($state = (__re__)[1]), (__re__)[0])
                                      : (true
                                        ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$TripleQuote)(({
                                          closingQuotes: 0,
                                          lastEscape: $lastEscape,
                                        })), $state)), ($state = (__re__)[1]), (__re__)[0])
                                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 719:13', (sp_toHuman)($char)))))));
                            }))()
                            : ((($4)[0] === "LineComment")
                              ? (((sp_equal)($char, "\n") || (sp_equal)($char, ""))
                                ? ((() => {
                                  ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                  return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                }))()
                                : null)
                              : ((($4)[0] === "ContentOpeningBlockComment")
                                ? ((sp_equal)($char, "#")
                                  ? ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$BlockComment)(({
                                    nesting: 1,
                                    previous: "",
                                  })), $state)), ($state = (__re__)[1]), (__re__)[0])
                                  : ((() => {
                                    ((__re__ = ($sd1$Compiler$Lexer$relAddToken)((0 - 1), 0, ($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Open), $state)), ($state = (__re__)[1]), (__re__)[0]);
                                    ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                    return ((__re__ = ($sd1$Compiler$Lexer$lexOne)($buffer, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                  }))())
                                : ((($4)[0] === "BlockComment")
                                  ? ((() => {
                                    const $nesting = ($4)[1].nesting;
                                    const $previous = ($4)[1].previous;
                                    const $continueWithDeltaNesting = (($dn) => {
                                      return ((__re__ = ($sd1$Compiler$Lexer$setMode)(($sd1$Compiler$Lexer$BlockComment)(({
                                        nesting: ($nesting + $dn),
                                        previous: $char,
                                      })), $state)), ($state = (__re__)[1]), (__re__)[0]);
                                    });
                                    const $5 = ({
                                      first: $previous,
                                      second: $char,
                                    });
                                    return ((("[" === $5.first) && ("#" === $5.second))
                                      ? ($continueWithDeltaNesting)(1)
                                      : ((("#" === $5.first) && ("]" === $5.second))
                                        ? (($nesting > 1)
                                          ? ($continueWithDeltaNesting)((0 - 1))
                                          : ((__re__ = ($sd1$Compiler$Lexer$setMode)($sd1$Compiler$Lexer$Default, $state)), ($state = (__re__)[1]), (__re__)[0]))
                                        : (("" === $5.second)
                                          ? ((__re__ = ($sd1$Compiler$Lexer$addError)("unterminated block comment", $state)), ($state = (__re__)[1]), (__re__)[0])
                                          : (true
                                            ? ($continueWithDeltaNesting)(0)
                                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 765:10', (sp_toHuman)($5))))));
                                  }))()
                                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 522:4', (sp_toHuman)($4))))))))))))))))),
    $state,
  ]);
});

const $sd1$Compiler$Lexer$readStateInit = (($moduleCode) => {
  return ({
    column: 0,
    errors: (array_fromList)($core$Core$Nil),
    indentStack: (array_fromList)($core$Core$Nil),
    indentStartsABlock: true,
    line: 0,
    lineIndent: 0,
    mode: $sd1$Compiler$Lexer$Indent,
    nextPos: 0,
    sections: (array_fromList)($core$Core$Nil),
    soFarThereAreNoTokensInThisLine: true,
    tabsOrSpaces: $sd1$Compiler$Lexer$NoTabsOrSpacesYet,
    tokenStart: 0,
    tokens: (array_fromList)($core$Core$Nil),
  });
});

const $sd1$Compiler$Lexer$lexer = (($module) => {
  (sp_benchStart)(null);
  const $moduleCode = $module.content;
  let $state = ($sd1$Compiler$Lexer$readStateInit)($moduleCode);
  (text_forEach)($moduleCode, (($char) => {
    ((__re__ = ($sd1$Compiler$Lexer$lexOne)($moduleCode, $char, $state)), ($state = (__re__)[1]), (__re__)[0]);
    ($state.nextPos += 1);
    return ((sp_equal)($char, "\n")
      ? ((() => {
        ($state.line += 1);
        return ($state.column = 0);
      }))()
      : ($state.column += 1));
  }));
  ((__re__ = ($sd1$Compiler$Lexer$lexOne)($moduleCode, "", $state)), ($state = (__re__)[1]), (__re__)[0]);
  (sp_benchStop)("lexer");
  const $2 = ((__re__ = (array_toList)($state.errors)), ($state.errors = (__re__)[1]), (__re__)[0]);
  return ((($2)[0] === "Nil")
    ? ((() => {
      ((__re__ = ($sd1$Compiler$Lexer$closeOpenBlocks)($state)), ($state = (__re__)[1]), (__re__)[0]);
      return ($core$Result$Ok)(((__re__ = (array_toList)($state.sections)), ($state.sections = (__re__)[1]), (__re__)[0]));
    }))()
    : (true
      ? ((() => {
        const $errors = $2;
        return ($core$Result$Err)(($sd1$Compiler$Error$Nested)(((($1) => {
          return ($core$List$map)((($e) => {
            return ($e)($module);
          }), $1);
        }))($errors)));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 844:4', (sp_toHuman)($2))));
});

const $sd1$Compiler$Error$res = (($mod, $pos, $desc) => {
  return ($core$Result$Err)(($sd1$Compiler$Error$Simple)($mod, $pos, $desc));
});

const $sd1$Compiler$Parser$makeError = (($env, $readState, $message) => {
  const $p = ((($readState)[0] === "Nil")
    ? ($sd1$Types$Pos$P)(0, 1)
    : (((($readState)[0] === "Cons") && ((($readState)[1])[0] === "Token"))
      ? ((() => {
        const $comment = (($readState)[1])[1];
        const $start = (($readState)[1])[2];
        const $end = (($readState)[1])[3];
        const $k = (($readState)[1])[4];
        const $rest = ($readState)[2];
        return ($sd1$Types$Pos$P)($start, $end);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 667:8', (sp_toHuman)($readState))));
  return ($sd1$Compiler$Error$res)($env.errorModule, $p, ($core$Core$Cons)($message, $core$Core$Nil));
});

const $sd1$SPLib$Parser$accept = (($a) => {
  return (($rejections, $readState) => {
    return ({
      first: $rejections,
      second: ($sd1$SPLib$Parser$Accepted)($readState, $a),
    });
  });
});

const $sd1$Compiler$Parser$ok = $sd1$SPLib$Parser$accept;

const $sd1$SPLib$Parser$andThen = (($chainedParser) => {
  return (($firstParser) => {
    return (($re0, $readState) => {
      const $5 = ($firstParser)($re0, $readState);
      return ((($5.second)[0] === "Accepted")
        ? ((() => {
          const $re1 = $5.first;
          const $nextReadState = ($5.second)[1];
          const $a = ($5.second)[2];
          return (($chainedParser)($a))($re1, $nextReadState);
        }))()
        : ((($5.second)[0] === "Rejected")
          ? ((() => {
            const $re1 = $5.first;
            return ({
              first: $re1,
              second: $sd1$SPLib$Parser$Rejected,
            });
          }))()
          : ((($5.second)[0] === "Aborted")
            ? ((() => {
              const $re1 = $5.first;
              const $rs = ($5.second)[1];
              const $e = ($5.second)[2];
              return ({
                first: $re1,
                second: ($sd1$SPLib$Parser$Aborted)($rs, $e),
              });
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 86:4', (sp_toHuman)($5)))));
    });
  });
});

const $sd1$Compiler$Parser$on = $sd1$SPLib$Parser$andThen;

const $sd1$SPLib$Parser$consumeOne = (($rejections, $readState) => {
  return ((($readState)[0] === "Nil")
    ? ({
      first: (sp_cons)($readState, $rejections),
      second: $sd1$SPLib$Parser$Rejected,
    })
    : ((($readState)[0] === "Cons")
      ? ((() => {
        const $token = ($readState)[1];
        const $nextState = ($readState)[2];
        return ({
          first: $rejections,
          second: ($sd1$SPLib$Parser$Accepted)($nextState, $token),
        });
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 69:4', (sp_toHuman)($readState))));
});

const $sd1$Compiler$Parser$oneToken = $sd1$SPLib$Parser$consumeOne;

const $sd1$SPLib$Parser$reject = (($rejections, $readState) => {
  return ({
    first: (sp_cons)($readState, $rejections),
    second: $sd1$SPLib$Parser$Rejected,
  });
});

const $sd1$Compiler$Parser$kind = (($targetKind) => {
  return (($sd1$Compiler$Parser$on)((($token) => {
    const $3 = $token;
    const $k = ($3)[4];
    return ((sp_equal)($targetKind, $k)
      ? ($sd1$Compiler$Parser$ok)($token)
      : $sd1$SPLib$Parser$reject);
  })))($sd1$Compiler$Parser$oneToken);
});

const $sd1$Compiler$Parser$binaryOperators = (($group) => {
  return (($sd1$Compiler$Parser$on)((($2) => {
    const $c = ($2)[1];
    const $s = ($2)[2];
    const $e = ($2)[3];
    const $k = ($2)[4];
    return ((($k)[0] === "Binop")
      ? ((() => {
        const $op = ($k)[1];
        return ((sp_equal)($op.precedence, $group)
          ? ($sd1$Compiler$Parser$ok)($op)
          : $sd1$SPLib$Parser$reject);
      }))()
      : (true
        ? $sd1$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 627:4', (sp_toHuman)($k))));
  })))($sd1$Compiler$Parser$oneToken);
});

const $sd1$SPLib$Parser$here = (($rejections, $readState) => {
  return ({
    first: $rejections,
    second: ($sd1$SPLib$Parser$Accepted)($readState, $readState),
  });
});

const $sd1$Compiler$Parser$here = (($sd1$Compiler$Parser$on)((($tokens) => {
  return ($sd1$Compiler$Parser$ok)((((($tokens)[0] === "Cons") && ((($tokens)[1])[0] === "Token"))
    ? ((() => {
      const $c = (($tokens)[1])[1];
      const $mod = (($tokens)[1])[2];
      const $start = (($tokens)[1])[3];
      const $end = (($tokens)[1])[4];
      const $rest = ($tokens)[2];
      return $start;
    }))()
    : ((($tokens)[0] === "Nil")
      ? 0
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 35:8', (sp_toHuman)($tokens)))));
})))($sd1$SPLib$Parser$here);

const $sd1$Compiler$Parser$pos = (($env, $start, $end) => {
  return ($env.stripLocations
    ? $sd1$Types$Pos$T
    : ($sd1$Types$Pos$P)($start, $end));
});

const $sd1$SPLib$Parser$surroundWith = (($left, $right, $parser) => {
  return (($sd1$SPLib$Parser$andThen)(((_0) => {
    return (($sd1$SPLib$Parser$andThen)((($p) => {
      return (($sd1$SPLib$Parser$andThen)(((_0) => {
        return ($sd1$SPLib$Parser$accept)($p);
      })))($right);
    })))($parser);
  })))($left);
});

const $sd1$Compiler$Parser$surroundStrict = (($left, $right, $p) => {
  return ($sd1$SPLib$Parser$surroundWith)(($sd1$Compiler$Parser$kind)($left), ($sd1$Compiler$Parser$kind)($right), $p);
});

const $sd1$Compiler$Parser$block = (($2) => {
  return ($sd1$Compiler$Parser$surroundStrict)($sd1$Types$Token$BlockStart, $sd1$Types$Token$BlockEnd, $2);
});

const $sd1$Compiler$Parser$discardFirst = (($a, $b) => {
  return (($sd1$Compiler$Parser$on)(((_0) => {
    return $b;
  })))($a);
});

const $sd1$Compiler$Parser$sib = (($1) => {
  return ($sd1$Compiler$Parser$discardFirst)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine), $1);
});

const $sd1$SPLib$Parser$oneOf = (($ps) => {
  return (($rejections, $readState) => {
    return ((($ps)[0] === "Nil")
      ? ({
        first: $rejections,
        second: $sd1$SPLib$Parser$Rejected,
      })
      : ((($ps)[0] === "Cons")
        ? ((() => {
          const $headParser = ($ps)[1];
          const $tailParsers = ($ps)[2];
          const $4 = ($headParser)($rejections, $readState);
          return ((($4.second)[0] === "Rejected")
            ? ((() => {
              const $re1 = $4.first;
              return (($sd1$SPLib$Parser$oneOf)($tailParsers))($re1, $readState);
            }))()
            : (true
              ? ((() => {
                const $acceptedOrAborted = $4;
                return $acceptedOrAborted;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 150:12', (sp_toHuman)($4))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 145:4', (sp_toHuman)($ps))));
  });
});

const $sd1$Compiler$Parser$sepListAtSep = (($sep, $item) => {
  return (($sd1$Compiler$Parser$on)((($sep0) => {
    const $theParserStillSucks = ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$block)(($sd1$Compiler$Parser$sepListAtItem)($sep, $item)), ($core$Core$Cons)(($sd1$Compiler$Parser$sib)(($sd1$Compiler$Parser$sepListAtItem)($sep, $item)), ($core$Core$Cons)(($sd1$Compiler$Parser$sepListAtItem)($sep, $item), $core$Core$Nil))));
    return (($sd1$Compiler$Parser$on)((($4) => {
      const $item0 = $4.first;
      const $tail = $4.second;
      return ($sd1$Compiler$Parser$ok)((sp_cons)(({
        first: $sep0,
        second: $item0,
      }), $tail));
    })))($theParserStillSucks);
  })))($sep);
});

const $sd1$Compiler$Parser$sepListAtItem = (($sep, $item) => {
  return (($sd1$Compiler$Parser$on)((($item0) => {
    const $theParserStillSucks = ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$block)(($sd1$Compiler$Parser$sepListAtSep)($sep, $item)), ($core$Core$Cons)(($sd1$Compiler$Parser$sib)(($sd1$Compiler$Parser$sepListAtSep)($sep, $item)), ($core$Core$Cons)(($sd1$Compiler$Parser$sepListAtSep)($sep, $item), ($core$Core$Cons)(($sd1$Compiler$Parser$ok)($core$Core$Nil), $core$Core$Nil)))));
    return (($sd1$Compiler$Parser$on)((($sepsAndItems) => {
      return ($sd1$Compiler$Parser$ok)(({
        first: $item0,
        second: $sepsAndItems,
      }));
    })))($theParserStillSucks);
  })))($item);
});

const $sd1$Compiler$Parser$sepList = $sd1$Compiler$Parser$sepListAtItem;

const $sd1$Compiler$Parser$binopsOr = (($env, $group) => {
  return (($higher) => {
    return (($sd1$Compiler$Parser$on)((($start) => {
      return (($sd1$Compiler$Parser$on)((($5) => {
        const $head = $5.first;
        const $sepTail = $5.second;
        return (($sd1$Compiler$Parser$on)((($end) => {
          return ((sp_equal)($sepTail, $core$Core$Nil)
            ? ($sd1$Compiler$Parser$ok)($head)
            : ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Expression)(($sd1$Compiler$Parser$pos)($env, $start, $end), ($sd1$Types$FormattableAst$Binop)($group, ({
              first: $head,
              second: $sepTail,
            })))));
        })))($sd1$Compiler$Parser$here);
      })))(($sd1$Compiler$Parser$sepList)(($sd1$Compiler$Parser$binaryOperators)($group), $higher));
    })))($sd1$Compiler$Parser$here);
  });
});

const $sd1$SPLib$Parser$thenWithDefault = (($fallbackParser, $chainedParser) => {
  return (($firstParser) => {
    return (($re0, $readState) => {
      const $6 = ($firstParser)($re0, $readState);
      return ((($6.second)[0] === "Aborted")
        ? ((() => {
          const $re1 = $6.first;
          const $rs = ($6.second)[1];
          const $reason = ($6.second)[2];
          return ({
            first: $re1,
            second: ($sd1$SPLib$Parser$Aborted)($rs, $reason),
          });
        }))()
        : ((($6.second)[0] === "Rejected")
          ? ((() => {
            const $re1 = $6.first;
            return ($fallbackParser)($re1, $readState);
          }))()
          : ((($6.second)[0] === "Accepted")
            ? ((() => {
              const $re1 = $6.first;
              const $nextReadState = ($6.second)[1];
              const $a = ($6.second)[2];
              return (($chainedParser)($a))($re1, $nextReadState);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 102:4', (sp_toHuman)($6)))));
    });
  });
});

const $sd1$SPLib$Parser$zeroOrMore = (($p) => {
  return (($sd1$SPLib$Parser$thenWithDefault)(($sd1$SPLib$Parser$accept)($core$Core$Nil), (($head) => {
    return (($sd1$SPLib$Parser$andThen)((($tail) => {
      return ($sd1$SPLib$Parser$accept)((sp_cons)($head, $tail));
    })))(($sd1$SPLib$Parser$zeroOrMore)($p));
  })))($p);
});

const $sd1$Compiler$Parser$oomSeparatedBy = (($sep, $pa) => {
  return (($sd1$Compiler$Parser$on)((($head) => {
    return (($sd1$Compiler$Parser$on)((($tail) => {
      return ($sd1$Compiler$Parser$ok)((sp_cons)($head, $tail));
    })))(($sd1$SPLib$Parser$zeroOrMore)(($sd1$Compiler$Parser$discardFirst)($sep, $pa)));
  })))($pa);
});

const $sd1$Compiler$Parser$siblingStatements = (($env) => {
  return (($sd1$Compiler$Parser$on)((($start) => {
    return (($sd1$Compiler$Parser$on)((($stats) => {
      return (($sd1$Compiler$Parser$on)((($end) => {
        return (((($stats)[0] === "Cons") && (((($stats)[1])[0] === "Evaluation") && ((($stats)[2])[0] === "Nil")))
          ? ((() => {
            const $expr = (($stats)[1])[1];
            return ($sd1$Compiler$Parser$ok)($expr);
          }))()
          : (true
            ? ((() => {
              const $many = $stats;
              return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Expression)(($sd1$Compiler$Parser$pos)($env, $start, $end), ($sd1$Types$FormattableAst$Statements)($stats)));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 243:2', (sp_toHuman)($stats))));
      })))($sd1$Compiler$Parser$here);
    })))(($sd1$Compiler$Parser$oomSeparatedBy)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine), ($sd1$Compiler$Parser$statement)($env)));
  })))($sd1$Compiler$Parser$here);
});

const $sd1$Compiler$Parser$alignedOrInlineStatements = (($env) => {
  return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$block)(($sd1$Compiler$Parser$siblingStatements)($env)), ($core$Core$Cons)(($sd1$Compiler$Parser$sib)(($sd1$Compiler$Parser$siblingStatements)($env)), ($core$Core$Cons)(($sd1$Compiler$Parser$expr)($env), $core$Core$Nil))));
});

const $sd1$Compiler$Parser$discardSecond = (($a, $b) => {
  return (($sd1$Compiler$Parser$on)((($aa) => {
    return (($sd1$Compiler$Parser$on)(((_0) => {
      return ($sd1$Compiler$Parser$ok)($aa);
    })))($b);
  })))($a);
});

const $sd1$Compiler$Parser$indentedOrInlineStatements = (($env) => {
  return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$block)(($sd1$Compiler$Parser$siblingStatements)($env)), ($core$Core$Cons)(($sd1$Compiler$Parser$expr)($env), $core$Core$Nil)));
});

const $sd1$Compiler$Parser$inlineOrBelowOrIndented = (($p) => {
  return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$block)($p), ($core$Core$Cons)(($sd1$Compiler$Parser$sib)($p), ($core$Core$Cons)($p, $core$Core$Nil))));
});

const $sd1$SPLib$Parser$maybe = (($p) => {
  return (($sd1$SPLib$Parser$thenWithDefault)(($sd1$SPLib$Parser$accept)($core$Maybe$Nothing), (($x) => {
    return ($sd1$SPLib$Parser$accept)(($core$Maybe$Just)($x));
  })))($p);
});

const $sd1$Compiler$Parser$maybe = $sd1$SPLib$Parser$maybe;

const $sd1$Compiler$Parser$maybeNewLine = (($1) => {
  return ($sd1$Compiler$Parser$discardFirst)(($sd1$SPLib$Parser$maybe)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine)), $1);
});

const $sd1$Compiler$Parser$rawList = (($item) => {
  const $sibsep = ($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Comma));
  return ($sd1$Compiler$Parser$discardFirst)(($sd1$SPLib$Parser$maybe)($sibsep), ($sd1$Compiler$Parser$oomSeparatedBy)($sibsep, $item));
});

const $sd1$Compiler$Parser$exprWithLeftDelimiter = (($env, $tokenKind) => {
  return ((($tokenKind)[0] === "Word")
    ? ((() => {
      const $word = ($tokenKind)[1];
      return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)((($sd1$Compiler$Parser$on)((($t) => {
        return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Variable)(({
          maybeType: ($core$Maybe$Just)($t),
          word: $word,
        })));
      })))(($sd1$Compiler$Parser$discardFirst)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$As), ($sd1$Compiler$Parser$expr)($env))), ($core$Core$Cons)(($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Variable)(({
        maybeType: $core$Maybe$Nothing,
        word: $word,
      }))), $core$Core$Nil)));
    }))()
    : ((($tokenKind)[0] === "ArgumentPlaceholder")
      ? ($sd1$Compiler$Parser$ok)($sd1$Types$FormattableAst$ArgumentPlaceholder)
      : ((($tokenKind)[0] === "NumberLiteral")
        ? ((() => {
          const $isPercent = ($tokenKind)[1];
          const $s = ($tokenKind)[2];
          return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)((($sd1$Compiler$Parser$on)((($e) => {
            return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Poly)($s, $e));
          })))(($sd1$Compiler$Parser$discardFirst)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$UniquenessPolymorphismBinop), ($sd1$Compiler$Parser$expr)($env))), ($core$Core$Cons)(($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$LiteralNumber)($isPercent, $s)), $core$Core$Nil)));
        }))()
        : ((($tokenKind)[0] === "TextLiteral")
          ? ((() => {
            const $s = ($tokenKind)[1];
            return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$LiteralText)($s));
          }))()
          : (((($tokenKind)[0] === "RoundParen") && ((($tokenKind)[1])[0] === "Open"))
            ? (($sd1$Compiler$Parser$on)((($3) => {
              const $pos = ($3)[1];
              const $expr_ = ($3)[2];
              return (($sd1$Compiler$Parser$on)(((_0) => {
                return ($sd1$Compiler$Parser$ok)($expr_);
              })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)(($sd1$Types$Token$RoundParen)($sd1$Types$Token$Closed))));
            })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$expr)($env)))
            : (((($tokenKind)[0] === "SquareBracket") && ((($tokenKind)[1])[0] === "Open"))
              ? ((() => {
                const $item = (($sd1$Compiler$Parser$on)((($maybeDots) => {
                  return (($sd1$Compiler$Parser$on)((($exp) => {
                    return ($sd1$Compiler$Parser$ok)(({
                      first: (sp_not_equal)($maybeDots, $core$Maybe$Nothing),
                      second: $exp,
                    }));
                  })))(($sd1$Compiler$Parser$expr)($env));
                })))(($sd1$Compiler$Parser$maybe)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$ThreeDots)));
                return (($sd1$Compiler$Parser$on)((($exps) => {
                  return (($sd1$Compiler$Parser$on)(((_0) => {
                    return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$List)(($core$Maybe$withDefault)($core$Core$Nil, $exps)));
                  })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Closed))));
                })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$maybe)(($sd1$Compiler$Parser$rawList)($item))));
              }))()
              : (((($tokenKind)[0] === "CurlyBrace") && ((($tokenKind)[1])[0] === "Open"))
                ? ((() => {
                  const $extension = ($sd1$Compiler$Parser$discardSecond)(($sd1$Compiler$Parser$maybe)(($sd1$Compiler$Parser$expr)($env)), ($sd1$Compiler$Parser$kind)($sd1$Types$Token$With));
                  const $attribute = (($sd1$Compiler$Parser$on)(((_0) => {
                    return (($sd1$Compiler$Parser$on)((($name) => {
                      return (($sd1$Compiler$Parser$on)((($maybeExpr) => {
                        return ($sd1$Compiler$Parser$ok)(({
                          maybeExpr: $maybeExpr,
                          name: $name,
                        }));
                      })))(($sd1$Compiler$Parser$maybe)(($sd1$Compiler$Parser$discardFirst)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Defop), ($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$expr)($env)))));
                    })))(($sd1$Compiler$Parser$expr)($env));
                  })))(($sd1$Compiler$Parser$maybe)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine)));
                  return (($sd1$Compiler$Parser$on)((($maybeExtension) => {
                    return (($sd1$Compiler$Parser$on)((($attrs) => {
                      return (($sd1$Compiler$Parser$on)(((_0) => {
                        return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Record)(({
                          attrs: ($core$Maybe$withDefault)($core$Core$Nil, $attrs),
                          maybeExtension: $maybeExtension,
                        })));
                      })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)(($sd1$Types$Token$CurlyBrace)($sd1$Types$Token$Closed))));
                    })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$maybe)(($sd1$Compiler$Parser$rawList)($attribute))));
                  })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$maybe)($extension)));
                }))()
                : ((($tokenKind)[0] === "Fn")
                  ? (($sd1$Compiler$Parser$on)((($args) => {
                    return (($sd1$Compiler$Parser$on)(((_0) => {
                      return (($sd1$Compiler$Parser$on)((($body) => {
                        return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Fn)($args, $body));
                      })))(($sd1$Compiler$Parser$alignedOrInlineStatements)($env));
                    })))(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Colon));
                  })))(($sd1$Compiler$Parser$rawList)(($sd1$Compiler$Parser$expr)($env)))
                  : ((($tokenKind)[0] === "If")
                    ? (($sd1$Compiler$Parser$on)((($condition) => {
                      return (($sd1$Compiler$Parser$on)(((_0) => {
                        return (($sd1$Compiler$Parser$on)((($true) => {
                          return (($sd1$Compiler$Parser$on)(((_0) => {
                            return (($sd1$Compiler$Parser$on)((($false) => {
                              return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$If)(({
                                condition: $condition,
                                false: $false,
                                true: $true,
                              })));
                            })))(($sd1$Compiler$Parser$alignedOrInlineStatements)($env));
                          })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Else)));
                        })))(($sd1$Compiler$Parser$alignedOrInlineStatements)($env));
                      })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Then)));
                    })))(($sd1$Compiler$Parser$expr)($env))
                    : ((($tokenKind)[0] === "Try")
                      ? ((() => {
                        const $maybeNewLineKind = (($k) => {
                          return ($sd1$Compiler$Parser$maybeNewLine)(($sd1$Compiler$Parser$kind)($k));
                        });
                        const $patternAndValue = (($sd1$Compiler$Parser$on)((($p) => {
                          return (($sd1$Compiler$Parser$on)(((_0) => {
                            return (($sd1$Compiler$Parser$on)((($value) => {
                              return ($sd1$Compiler$Parser$ok)(({
                                first: $p,
                                second: $value,
                              }));
                            })))(($sd1$Compiler$Parser$indentedOrInlineStatements)($env));
                          })))(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Colon));
                        })))(($sd1$Compiler$Parser$expr)($env));
                        return (($sd1$Compiler$Parser$on)((($value) => {
                          return (($sd1$Compiler$Parser$on)(((_0) => {
                            return (($sd1$Compiler$Parser$on)((($patterns) => {
                              return (($sd1$Compiler$Parser$on)((($end) => {
                                return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Try)(({
                                  patterns: $patterns,
                                  value: $value,
                                })));
                              })))($sd1$Compiler$Parser$here);
                            })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$rawList)($patternAndValue)));
                          })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$As)));
                        })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$expr)($env)));
                      }))()
                      : (true
                        ? $sd1$SPLib$Parser$reject
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 334:4', (sp_toHuman)($tokenKind)))))))))))));
});

const $sd1$Compiler$Parser$maybeWithDefault = (($a, $p) => {
  return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)($p, ($core$Core$Cons)(($sd1$Compiler$Parser$ok)($a), $core$Core$Nil)));
});

const $sd1$Compiler$Parser$recInlineOrIndentedOrBelow = (($higher, $accum) => {
  return (($sd1$Compiler$Parser$on)((($h) => {
    const $r = (sp_cons)($h, $accum);
    return ((($1) => {
      return ($sd1$Compiler$Parser$maybeWithDefault)($r, $1);
    }))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$recInlineOrIndentedOrBelow)($higher, $r)));
  })))($higher);
});

const $sd1$Compiler$Parser$unaryOperator = (($sd1$Compiler$Parser$on)((($token) => {
  return (((($token)[0] === "Token") && ((($token)[4])[0] === "Unop"))
    ? ((() => {
      const $c = ($token)[1];
      const $s = ($token)[2];
      const $e = ($token)[3];
      const $op = (($token)[4])[1];
      return ($sd1$SPLib$Parser$accept)(({
        first: $op,
        second: $token,
      }));
    }))()
    : (true
      ? $sd1$SPLib$Parser$reject
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 528:4', (sp_toHuman)($token))));
})))($sd1$Compiler$Parser$oneToken);

const $sd1$Compiler$Parser$unopsOr = (($env, $higher) => {
  return (($sd1$Compiler$Parser$on)((($maybeUnary) => {
    return (($sd1$Compiler$Parser$on)((($right) => {
      return (($sd1$Compiler$Parser$on)((($end) => {
        return (((($maybeUnary)[0] === "Just") && ((($maybeUnary)[1].second)[0] === "Token"))
          ? ((() => {
            const $op = ($maybeUnary)[1].first;
            const $start = (($maybeUnary)[1].second)[2];
            return ($sd1$Compiler$Parser$ok)(((($1) => {
              return ($sd1$Types$FormattableAst$Expression)(($sd1$Compiler$Parser$pos)($env, $start, $end), $1);
            }))(($sd1$Types$FormattableAst$Unop)($op, $right)));
          }))()
          : ((($maybeUnary)[0] === "Nothing")
            ? ($sd1$Compiler$Parser$ok)($right)
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 516:4', (sp_toHuman)($maybeUnary))));
      })))($sd1$Compiler$Parser$here);
    })))($higher);
  })))(($sd1$SPLib$Parser$maybe)($sd1$Compiler$Parser$unaryOperator));
});

const $sd1$Compiler$Parser$functionApplicationOr = (($env, $higher) => {
  const $recInlineOrIndented = (($accum) => {
    const $p = ((sp_equal)($accum, $core$Core$Nil)
      ? $higher
      : ($sd1$Compiler$Parser$unopsOr)($env, $higher));
    return (($sd1$Compiler$Parser$on)((($h) => {
      const $r = (sp_cons)($h, $accum);
      return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$block)(($sd1$Compiler$Parser$recInlineOrIndentedOrBelow)($higher, $r)), ($core$Core$Cons)(($recInlineOrIndented)($r), ($core$Core$Cons)(($sd1$Compiler$Parser$ok)($r), $core$Core$Nil))));
    })))($p);
  });
  return (($sd1$Compiler$Parser$on)((($start) => {
    return (($sd1$Compiler$Parser$on)((($reversedArgs) => {
      return (($sd1$Compiler$Parser$on)((($end) => {
        const $6 = ($core$List$reverse)($reversedArgs);
        return ((($6)[0] === "Nil")
          ? $sd1$SPLib$Parser$reject
          : (((($6)[0] === "Cons") && ((($6)[2])[0] === "Nil"))
            ? ((() => {
              const $fnExpression = ($6)[1];
              return ($sd1$Compiler$Parser$ok)($fnExpression);
            }))()
            : ((($6)[0] === "Cons")
              ? ((() => {
                const $fnExpression = ($6)[1];
                const $args = ($6)[2];
                return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Expression)(($sd1$Compiler$Parser$pos)($env, $start, $end), ($sd1$Types$FormattableAst$Call)($fnExpression, $args)));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 497:4', (sp_toHuman)($6)))));
      })))($sd1$Compiler$Parser$here);
    })))(($recInlineOrIndented)($core$Core$Nil));
  })))($sd1$Compiler$Parser$here);
});

const $sd1$SPLib$Parser$expression = (($term, $ops) => {
  return ((($ops)[0] === "Nil")
    ? $term
    : ((($ops)[0] === "Cons")
      ? ((() => {
        const $op = ($ops)[1];
        const $rest = ($ops)[2];
        return ($sd1$SPLib$Parser$expression)(($op)($term), $rest);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 206:4', (sp_toHuman)($ops))));
});

const $sd1$Compiler$Parser$expr = (($env) => {
  const $expressionWithLeftDelimiter = (($sd1$Compiler$Parser$on)((($2) => {
    const $comment = ($2)[1];
    const $start = ($2)[2];
    const $end = ($2)[3];
    const $k = ($2)[4];
    return (($sd1$Compiler$Parser$on)((($expr_) => {
      return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Expression)(($sd1$Compiler$Parser$pos)($env, $start, $end), $expr_));
    })))(($sd1$Compiler$Parser$exprWithLeftDelimiter)($env, $k));
  })))($sd1$Compiler$Parser$oneToken);
  return ($sd1$SPLib$Parser$expression)($expressionWithLeftDelimiter, ($core$Core$Cons)((($1) => {
    return ($sd1$Compiler$Parser$functionApplicationOr)($env, $1);
  }), ($core$Core$Cons)((($1) => {
    return ($sd1$Compiler$Parser$unopsOr)($env, $1);
  }), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Exponential), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Multiplicative), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Addittive), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Comparison), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Logical), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Tuple), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Cons), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Pipe), ($core$Core$Cons)(($sd1$Compiler$Parser$binopsOr)($env, $sd1$Types$Op$Mutop), $core$Core$Nil))))))))))));
});

const $sd1$Compiler$Parser$word = (($env) => {
  return (($sd1$Compiler$Parser$on)((($2) => {
    const $comment = ($2)[1];
    const $start = ($2)[2];
    const $end = ($2)[3];
    const $kind = ($2)[4];
    return ((($kind)[0] === "Word")
      ? ((() => {
        const $w = ($kind)[1];
        return ($sd1$Compiler$Parser$ok)(($sd1$Types$Pos$At)(($sd1$Compiler$Parser$pos)($env, $start, $end), $w));
      }))()
      : (true
        ? $sd1$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 178:4', (sp_toHuman)($kind))));
  })))($sd1$Compiler$Parser$oneToken);
});

const $sd1$Compiler$Parser$aliasDef = (($env) => {
  const $aliasWord = ({
    attrPath: $core$Core$Nil,
    isUpper: false,
    maybeModule: $core$Maybe$Nothing,
    modifier: $sd1$Types$Token$NameNoModifier,
    name: "alias",
  });
  return (($sd1$Compiler$Parser$on)(((_0) => {
    return (($sd1$Compiler$Parser$on)((($name) => {
      return (($sd1$Compiler$Parser$on)((($args) => {
        return (($sd1$Compiler$Parser$on)(((_0) => {
          return (($sd1$Compiler$Parser$on)((($type) => {
            return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$AliasDef)(({
              args: $args,
              name: $name,
              type: $type,
            })));
          })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$expr)($env)));
        })))(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Defop));
      })))(($sd1$SPLib$Parser$zeroOrMore)(($sd1$Compiler$Parser$word)($env)));
    })))(($sd1$Compiler$Parser$word)($env));
  })))(($sd1$Compiler$Parser$kind)(($sd1$Types$Token$Word)($aliasWord)));
});

const $sd1$SPLib$Parser$abort = (($error) => {
  return (($rejections, $readState) => {
    return ({
      first: $rejections,
      second: ($sd1$SPLib$Parser$Aborted)($readState, $error),
    });
  });
});

const $sd1$Compiler$Parser$nonFunction = (($env) => {
  return (($sd1$Compiler$Parser$on)(((_0) => {
    return (($sd1$Compiler$Parser$on)((($names) => {
      return (($sd1$Compiler$Parser$on)((($4) => {
        const $literal = ($4)[2];
        return ((sp_not_equal)($literal.name, "NonFunction")
          ? ($sd1$SPLib$Parser$abort)("Only NonFunction is supported for now")
          : ($sd1$Compiler$Parser$ok)($names));
      })))(($sd1$Compiler$Parser$word)($env));
    })))(($sd1$Compiler$Parser$rawList)(($sd1$Compiler$Parser$word)($env)));
  })))(($sd1$Compiler$Parser$kind)($sd1$Types$Token$With));
});

const $sd1$Compiler$Parser$definition = (($env) => {
  return (($sd1$Compiler$Parser$on)((($start) => {
    return (($sd1$Compiler$Parser$on)((($p) => {
      return (($sd1$Compiler$Parser$on)((($nf) => {
        return (($sd1$Compiler$Parser$on)((($defModifier) => {
          return (($sd1$Compiler$Parser$on)((($body) => {
            return (($sd1$Compiler$Parser$on)((($end) => {
              return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$ValueDef)(({
                body: $body,
                nonFn: ($core$Maybe$withDefault)($core$Core$Nil, $nf),
                pattern: $p,
              })));
            })))($sd1$Compiler$Parser$here);
          })))(($sd1$Compiler$Parser$indentedOrInlineStatements)($env));
        })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Defop)));
      })))(($sd1$SPLib$Parser$maybe)(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$nonFunction)($env))));
    })))(($sd1$Compiler$Parser$expr)($env));
  })))($sd1$Compiler$Parser$here);
});

const $sd1$Compiler$Parser$unionDef = (($env) => {
  const $unionWord = ({
    attrPath: $core$Core$Nil,
    isUpper: false,
    maybeModule: $core$Maybe$Nothing,
    modifier: $sd1$Types$Token$NameNoModifier,
    name: "union",
  });
  return (($sd1$Compiler$Parser$on)(((_0) => {
    return (($sd1$Compiler$Parser$on)((($name) => {
      return (($sd1$Compiler$Parser$on)((($args) => {
        return (($sd1$Compiler$Parser$on)(((_0) => {
          return (($sd1$Compiler$Parser$on)((($constructors) => {
            return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$UnionDef)(({
              args: $args,
              constructors: $constructors,
              name: $name,
            })));
          })))(($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$rawList)(($sd1$Compiler$Parser$expr)($env))));
        })))(($sd1$Compiler$Parser$kind)($sd1$Types$Token$Defop));
      })))(($sd1$SPLib$Parser$zeroOrMore)(($sd1$Compiler$Parser$word)($env)));
    })))(($sd1$Compiler$Parser$word)($env));
  })))(($sd1$Compiler$Parser$kind)(($sd1$Types$Token$Word)($unionWord)));
});

const $sd1$SPLib$Parser$breakCircularDefinition = (($a) => {
  return (($sd1$SPLib$Parser$andThen)($a))(($sd1$SPLib$Parser$accept)(null));
});

const $sd1$Compiler$Parser$statement = (($env) => {
  return ($sd1$SPLib$Parser$breakCircularDefinition)(((_0) => {
    return (($sd1$Compiler$Parser$on)(((_0) => {
      return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$aliasDef)($env), ($core$Core$Cons)(($sd1$Compiler$Parser$unionDef)($env), ($core$Core$Cons)(($sd1$Compiler$Parser$definition)($env), ($core$Core$Cons)((($sd1$Compiler$Parser$on)((($e) => {
        return ($sd1$Compiler$Parser$ok)(($sd1$Types$FormattableAst$Evaluation)($e));
      })))(($sd1$Compiler$Parser$expr)($env)), $core$Core$Nil)))));
    })))(($sd1$SPLib$Parser$maybe)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine)));
  }));
});

const $sd1$SPLib$Parser$without = (($p) => {
  return (($sd1$SPLib$Parser$thenWithDefault)(($sd1$SPLib$Parser$accept)(null), ((_0) => {
    return $sd1$SPLib$Parser$reject;
  })))($p);
});

const $sd1$SPLib$Parser$end = ($sd1$SPLib$Parser$without)($sd1$SPLib$Parser$consumeOne);

const $sd1$Compiler$Parser$rootStatement = (($env) => {
  const $fillers = ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$BlockEnd), ($core$Core$Cons)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine), $core$Core$Nil)));
  return (($sd1$Compiler$Parser$on)(((_0) => {
    return (($sd1$Compiler$Parser$on)((($maybeStatement) => {
      return (($sd1$Compiler$Parser$on)(((_0) => {
        return (($sd1$Compiler$Parser$on)(((_0) => {
          return ($sd1$SPLib$Parser$accept)($maybeStatement);
        })))($sd1$SPLib$Parser$end);
      })))(($sd1$SPLib$Parser$zeroOrMore)($fillers));
    })))(($sd1$SPLib$Parser$maybe)(($sd1$Compiler$Parser$statement)($env)));
  })))(($sd1$SPLib$Parser$zeroOrMore)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$NewSiblingLine)));
});

const $sd1$SPLib$Parser$runParser = (($parser, $readState) => {
  return ($parser)(($core$Core$Cons)($readState, $core$Core$Nil), $readState);
});

const $sd1$Compiler$Parser$parse = (($env, $tokens) => {
  const $3 = ($sd1$SPLib$Parser$runParser)(($sd1$Compiler$Parser$rootStatement)($env), $tokens);
  const $outcome = $3.second;
  const $failureStates = $3.first;
  return ((($outcome)[0] === "Accepted")
    ? ((() => {
      const $readState = ($outcome)[1];
      const $output = ($outcome)[2];
      return ($core$Result$Ok)($output);
    }))()
    : ((($outcome)[0] === "Aborted")
      ? ((() => {
        const $readState = ($outcome)[1];
        const $message = ($outcome)[2];
        return ($sd1$Compiler$Parser$makeError)($env, $readState, $message);
      }))()
      : ((($outcome)[0] === "Rejected")
        ? ((() => {
          const $findMin = (($readState, $best) => {
            return ((($core$List$length)($readState) < ($core$List$length)($best))
              ? $readState
              : $best);
          });
          const $readState = ($core$List$for)($tokens, $failureStates, $findMin);
          const $message = ((($readState)[0] === "Nil")
            ? "I got to the end of the statement and I can't make sense of it. =("
            : (true
              ? "I got stuck parsing here. =("
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 695:16', (sp_toHuman)($readState))));
          return ($sd1$Compiler$Parser$makeError)($env, $readState, $message);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 680:4', (sp_toHuman)($outcome)))));
});

const $sd1$Compiler$Parser$textToFormattableModule = (($env) => {
  const $tokensResult = ($sd1$Compiler$Lexer$lexer)($env.errorModule);
  return (($core$Result$onOk)((($rootStatements) => {
    (sp_benchStart)(null);
    const $result = ((($1) => {
      return ($core$Result$map)((($1) => {
        return ($core$List$filterMap)($core$Basics$identity, $1);
      }), $1);
    }))(((($1) => {
      return ($core$List$mapRes)((($1) => {
        return ($sd1$Compiler$Parser$parse)($env, $1);
      }), $1);
    }))($rootStatements));
    (sp_benchStop)("parse");
    return $result;
  })))($tokensResult);
});

const $sd1$SPON$run = (($readerA, $errorModule, $statements) => {
  const $4 = ($readerA)($statements);
  return (((($4)[0] === "Accepted") && ((($4)[1])[0] === "Nil"))
    ? ((() => {
      const $a = ($4)[2];
      return ($core$Result$Ok)($a);
    }))()
    : (((($4)[0] === "Accepted") && ((($4)[1])[0] === "Cons"))
      ? ((() => {
        const $head = (($4)[1])[1];
        const $tail = (($4)[1])[2];
        const $a = ($4)[2];
        return ($sd1$Compiler$Error$res)($errorModule, ($sd1$Types$FormattableAst$statementPos)($head), ($core$Core$Cons)("unread statements", $core$Core$Nil));
      }))()
      : (((($4)[0] === "Rejected") && ((($4)[1])[0] === "At"))
        ? ((() => {
          const $pos = (($4)[1])[1];
          const $r = (($4)[1])[2];
          return ($sd1$Compiler$Error$res)($errorModule, $pos, ($core$Core$Cons)($r, $core$Core$Nil));
        }))()
        : (((($4)[0] === "Failed") && ((($4)[1])[0] === "At"))
          ? ((() => {
            const $pos = (($4)[1])[1];
            const $r = (($4)[1])[2];
            return ($sd1$Compiler$Error$res)($errorModule, $pos, ($core$Core$Cons)($r, $core$Core$Nil));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 52:4', (sp_toHuman)($4))))));
});

const $sd1$SPON$read = (($reader, $fsPath, $content) => {
  const $errorModule = ({
    content: $content,
    fsPath: $fsPath,
  });
  return (($core$Result$onOk)((($2) => {
    return ($sd1$SPON$run)($reader, $errorModule, $2);
  })))(($sd1$Compiler$Parser$textToFormattableModule)(({
    errorModule: $errorModule,
    stripLocations: false,
  })));
});

const $sd1$ModulesFile$textToModulesFile = (($sponName, $sponContent) => {
  const $insert = (($rootEntry, $mf) => {
    return ((($rootEntry)[0] === "Lib")
      ? ((() => {
        const $lib = ($rootEntry)[1];
        const $0 = $mf;
        return (Object.assign)({}, $0, ({
          libraries: (sp_cons)($lib, $mf.libraries),
        }));
      }))()
      : ((($rootEntry)[0] === "Dir")
        ? ((() => {
          const $dir = ($rootEntry)[1];
          const $0 = $mf;
          return (Object.assign)({}, $0, ({
            sourceDirs: (sp_cons)($dir, $mf.sourceDirs),
          }));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/ModulesFile.sp 158:8', (sp_toHuman)($rootEntry))));
  });
  return ((($1) => {
    return ($core$Result$map)((($rootEntries) => {
      return ($core$List$for)($sd1$ModulesFile$initModulesFile, $rootEntries, $insert);
    }), $1);
  }))(((($2) => {
    return ($sd1$SPON$read)($sd1$ModulesFile$modulesFileReader, $sponName, $2);
  }))($sponContent));
});

const $sd1$Compile$loadModulesFile = (($platform, $projectRoot) => {
  const $path = ($posix$Path$resolve)(($core$Core$Cons)($projectRoot, ($core$Core$Cons)($sd1$Compile$modulesFileName, $core$Core$Nil)));
  (sp_log)("Metafile: ", $path);
  return (($posix$IO$onResult)((($result) => {
    const $modulesAsText = ((($result)[0] === "Ok")
      ? ((() => {
        const $f = ($result)[1];
        return $f;
      }))()
      : ((($result)[0] === "Err")
        ? ((() => {
          (sp_log)("Using default modules.sp", "");
          return $platform.defaultModules;
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 63:8', (sp_toHuman)($result))));
    return ($sd1$Compile$resToIo)(($sd1$ModulesFile$textToModulesFile)($sd1$Compile$modulesFileName, $modulesAsText));
  })))(($posix$IO$readFile)($path));
});

const $sd1$Compile$updateSourceDir = (($fileNames, $orig) => {
  const $insertModuleName = (($name, $sd) => {
    const $5 = ($core$List$find)((($m) => {
      return (sp_equal)($m.path, $name);
    }), $sd.modules);
    return ((($5)[0] === "Just")
      ? $sd
      : ((($5)[0] === "Nothing")
        ? ((() => {
          const $0 = $sd;
          return (Object.assign)({}, $0, ({
            modules: (sp_cons)(({
              globalTypes: $core$Core$Nil,
              globalValues: $core$Core$Nil,
              path: $name,
              visibleAs: $name,
            }), $0.modules),
          }));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 188:8', (sp_toHuman)($5))));
  });
  return ($core$List$for)($orig, $fileNames, $insertModuleName);
});

const $sd1$ModulesFile$insertModule = (($source, $mod, $meta) => {
  const $visibleAs = $mod.visibleAs;
  const $umr = ($sd1$Types$Meta$UMR)($source, $mod.path);
  const $insertGlobal = (($varName, $d) => {
    return ((($1) => {
      return ($core$Dict$insert)($varName, $1, $d);
    }))(((($1) => {
      return ($sd1$Types$Meta$USR)($umr, $1);
    }))($varName));
  });
  const $0 = $meta;
  return (Object.assign)({}, $0, ({
    globalTypes: ($core$List$for)($meta.globalTypes, $mod.globalTypes, $insertGlobal),
    globalValues: ($core$List$for)($meta.globalValues, $mod.globalValues, $insertGlobal),
    moduleVisibleAsToUmr: ($core$Dict$insert)($visibleAs, $umr, $meta.moduleVisibleAsToUmr),
    umrToModuleVisibleAs: ($core$Dict$insert)($umr, $visibleAs, $meta.umrToModuleVisibleAs),
  }));
});

const $sd1$ModulesFile$insertLibrary = (($lib, $meta) => {
  const $umr = ((() => {
    const $3 = $lib.source;
    return (("core:prelude" === $3)
      ? $sd1$Types$Meta$Core
      : (("core:browser" === $3)
        ? $sd1$Types$Meta$Browser
        : (("core:posix" === $3)
          ? $sd1$Types$Meta$Posix
          : (true
            ? (sp_todo)(("Library source `" + ($lib.source + "` is not supported.")))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/ModulesFile.sp 55:8', (sp_toHuman)($3))))));
  }))();
  return ($core$List$for)($meta, $lib.modules, (($1, $2) => {
    return ($sd1$ModulesFile$insertModule)($umr, $1, $2);
  }));
});

const $sd1$ModulesFile$insertModules = (($sd, $m) => {
  const $n = ($m.sourceDirIdCounter + 1);
  const $id = ("sd" + (text_fromNumber)($n));
  return ((($0) => {
    return ($core$List$for)($0, $sd.modules, (($1, $2) => {
      return ($sd1$ModulesFile$insertModule)(($sd1$Types$Meta$SourceDirId)($id), $1, $2);
    }));
  }))(((() => {
    const $0 = $m;
    return (Object.assign)({}, $0, ({
      sourceDirIdCounter: $n,
      sourceDirIdToPath: ($core$Dict$insert)($id, $sd.path, $0.sourceDirIdToPath),
    }));
  }))());
});

const $sd1$Types$Meta$init = ({
  globalTypes: $core$Dict$empty,
  globalValues: $core$Dict$empty,
  moduleVisibleAsToUmr: $core$Dict$empty,
  sourceDirIdCounter: 0,
  sourceDirIdToPath: $core$Dict$empty,
  umrToModuleVisibleAs: $core$Dict$empty,
});

const $sd1$ModulesFile$toMeta = (($mf) => {
  return ((($0) => {
    return ($core$List$for)($0, $mf.sourceDirs, $sd1$ModulesFile$insertModules);
  }))(((($0) => {
    return ($core$List$for)($0, $mf.libraries, $sd1$ModulesFile$insertLibrary);
  }))($sd1$Types$Meta$init));
});

const $sd1$Compile$loadMeta = (($env, $platform, $entryModuleDir, $projectRoot) => {
  return (($posix$IO$onSuccess)((($modulesFileRaw) => {
    const $resolvedDirs = ((($1) => {
      return ($core$List$map)((($sd) => {
        const $0 = $sd;
        return (Object.assign)({}, $0, ({
          path: ($posix$Path$resolve)(($core$Core$Cons)($projectRoot, ($core$Core$Cons)($0.path, $core$Core$Nil))),
        }));
      }), $1);
    }))($modulesFileRaw.sourceDirs);
    const $allDirs = (($core$List$any)((($sd) => {
      return (sp_equal)($sd.path, $entryModuleDir);
    }), $resolvedDirs)
      ? $resolvedDirs
      : (sp_cons)(({
        modules: $core$Core$Nil,
        path: $entryModuleDir,
      }), $resolvedDirs));
    const $modulesFile = ((() => {
      const $0 = $modulesFileRaw;
      return (Object.assign)({}, $0, ({
        sourceDirs: $allDirs,
      }));
    }))();
    const $getAllSourceDirLists = ($posix$IO$parallel)(((($1) => {
      return ($core$List$map)((($sd) => {
        return ($sd1$Compile$listSourceDir)($sd.path, "");
      }), $1);
    }))($modulesFile.sourceDirs));
    return (($posix$IO$onSuccess)((($allSourceDirLists) => {
      const $updatedSourceDirs = ($core$List$map2)($sd1$Compile$updateSourceDir, $allSourceDirLists, $modulesFile.sourceDirs);
      return ($posix$IO$succeed)(($sd1$ModulesFile$toMeta)(((() => {
        const $0 = $modulesFile;
        return (Object.assign)({}, $0, ({
          sourceDirs: $updatedSourceDirs,
        }));
      }))()));
    })))($getAllSourceDirLists);
  })))(($sd1$Compile$loadModulesFile)($platform, $projectRoot));
});

const $sd1$Compile$onResSuccess = (($f) => {
  return (($res) => {
    return ((($0) => {
      return (($posix$IO$onSuccess)($f))($0);
    }))(($sd1$Compile$resToIo)($res));
  });
});

const $sd1$Compile$searchAncestorDirectories = (($isWantedFile, $searchDir) => {
  return (($posix$IO$onResult)((($result) => {
    return ((($result)[0] === "Err")
      ? ($posix$IO$succeed)($core$Maybe$Nothing)
      : ((($result)[0] === "Ok")
        ? ((() => {
          const $dirContents = ($result)[1];
          return (($core$List$any)($isWantedFile, $dirContents)
            ? ($posix$IO$succeed)(($core$Maybe$Just)($searchDir))
            : ((() => {
              const $parent = ($posix$Path$resolve)(($core$Core$Cons)($searchDir, ($core$Core$Cons)("..", $core$Core$Nil)));
              return ((sp_equal)($parent, $searchDir)
                ? ($posix$IO$succeed)($core$Maybe$Nothing)
                : ((($1) => {
                  return ($sd1$Compile$searchAncestorDirectories)($isWantedFile, $1);
                }))($parent));
            }))());
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 241:4', (sp_toHuman)($result))));
  })))(($posix$IO$readDir)($searchDir));
});

const $sd1$Compile$umrToFileName = (($meta, $corePath, $umr) => {
  const $4 = $umr;
  const $name = ($4)[2];
  const $source = ($4)[1];
  return ((($source)[0] === "Core")
    ? ($posix$Path$resolve)((sp_cons)($corePath, (sp_cons)("core", ((($1) => {
      return (text_split)("/", $1);
    }))(($name + ".sp")))))
    : ((($source)[0] === "Posix")
      ? ($posix$Path$resolve)((sp_cons)($corePath, (sp_cons)("posix", ((($1) => {
        return (text_split)("/", $1);
      }))(($name + ".sp")))))
      : ((($source)[0] === "Browser")
        ? ($posix$Path$resolve)((sp_cons)($corePath, (sp_cons)("browser", ((($1) => {
          return (text_split)("/", $1);
        }))(($name + ".sp")))))
        : ((($source)[0] === "SourceDirId")
          ? ((() => {
            const $id = ($source)[1];
            const $5 = ($core$Dict$get)($id, $meta.sourceDirIdToPath);
            return ((($5)[0] === "Nothing")
              ? (sp_todo)(("invalid sourceDirId " + $id))
              : ((($5)[0] === "Just")
                ? ((() => {
                  const $path = ($5)[1];
                  return ($posix$Path$resolve)(($core$Core$Cons)($path, ($core$Core$Cons)(($name + ".sp"), $core$Core$Nil)));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 149:12', (sp_toHuman)($5))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 137:4', (sp_toHuman)($source))))));
});

const $sd1$Types$TypedAst$toRaw = (($par) => {
  return ((($par)[0] === "ParRe")
    ? ((() => {
      const $raw = ($par)[1];
      return $raw;
    }))()
    : ((($par)[0] === "ParSp")
      ? ((() => {
        const $full = ($par)[1];
        return $full.raw;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 313:4', (sp_toHuman)($par))));
});

const $sd1$Types$TypedAst$typeTyvars = (($type) => {
  return ((($type)[0] === "TypeExact")
    ? ((() => {
      const $usr = ($type)[1];
      const $args = ($type)[2];
      return ($core$List$for)($core$Dict$empty, $args, (($a, $acc) => {
        return ($core$Dict$join)(($sd1$Types$TypedAst$typeTyvars)($a), $acc);
      }));
    }))()
    : ((($type)[0] === "TypeVar")
      ? ((() => {
        const $id = ($type)[1];
        return ($core$Dict$ofOne)($id, null);
      }))()
      : (((($type)[0] === "TypeRecord") && ((($type)[1])[0] === "Nothing"))
        ? ((() => {
          const $attrs = ($type)[2];
          return ($core$Dict$for)($core$Dict$empty, $attrs, (($k, $a, $d) => {
            return ($core$Dict$join)(($sd1$Types$TypedAst$typeTyvars)($a), $d);
          }));
        }))()
        : (((($type)[0] === "TypeRecord") && ((($type)[1])[0] === "Just"))
          ? ((() => {
            const $id = (($type)[1])[1];
            const $attrs = ($type)[2];
            return ((($0) => {
              return ($core$Dict$for)($0, $attrs, (($k, $a, $d) => {
                return ($core$Dict$join)(($sd1$Types$TypedAst$typeTyvars)($a), $d);
              }));
            }))(($core$Dict$ofOne)($id, null));
          }))()
          : ((($type)[0] === "TypeError")
            ? $core$Dict$empty
            : ((($type)[0] === "TypeFn")
              ? ((() => {
                const $ins = ($type)[1];
                const $out = ($type)[2];
                return ((($0) => {
                  return ($core$List$for)($0, $ins, (($in, $a) => {
                    return ($core$Dict$join)(($sd1$Types$TypedAst$typeTyvars)(($sd1$Types$TypedAst$toRaw)($in)), $a);
                  }));
                }))(($sd1$Types$TypedAst$typeTyvars)($out.raw));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 342:4', (sp_toHuman)($type))))))));
});

const $sd1$Compiler$Compiler$exposedValueToUsrAndInstance = (($1) => {
  const $usr = $1.first;
  const $exposed = $1.second;
  const $2 = $exposed;
  const $raw = $2.raw;
  const $makeTyvar = (($tyvarId, _1) => {
    return ({
      allowFunctions: true,
      generalizedAt: $sd1$Types$Pos$N,
      generalizedFor: ($sd1$Types$Ast$RefGlobal)($usr),
      originalName: "",
    });
  });
  const $freeTyvars = ((($1) => {
    return ($core$Dict$map)($makeTyvar, $1);
  }))(($sd1$Types$TypedAst$typeTyvars)($raw));
  return ({
    first: $usr,
    second: ({
      definedAt: $sd1$Types$Pos$N,
      freeTyvars: $freeTyvars,
      freeUnivars: $core$Dict$empty,
      type: ({
        raw: $raw,
        uni: $sd1$Types$Ast$Imm,
      }),
    }),
  });
});

const $sd1$Types$TypedAst$mapPars = (($f, $pars) => {
  const $zzz = (($par) => {
    return ((($par)[0] === "ParRe")
      ? ((() => {
        const $raw = ($par)[1];
        return ($sd1$Types$TypedAst$ParRe)(($f)($raw));
      }))()
      : ((($par)[0] === "ParSp")
        ? ((() => {
          const $full = ($par)[1];
          return ($sd1$Types$TypedAst$ParSp)(((() => {
            const $0 = $full;
            return (Object.assign)({}, $0, ({
              raw: ($f)($0.raw),
            }));
          }))());
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 323:8', (sp_toHuman)($par))));
  });
  return ($core$List$map)($zzz, $pars);
});

const $sd1$Types$TypedAst$normalizeTyvarId = (($hash, $id) => {
  const $3 = ((__re__ = (hash_get)($hash, $id)), ($hash = (__re__)[1]), (__re__)[0]);
  return ([
    ((($3)[0] === "Just")
      ? ((() => {
        const $nid = ($3)[1];
        return $nid;
      }))()
      : ((($3)[0] === "Nothing")
        ? ((() => {
          let $maxId = 0;
          ((__re__ = (hash_each)($hash, (($k, $v) => {
            return (($v > ((__re__ = (basics_cloneUni)($maxId)), ($maxId = (__re__)[1]), (__re__)[0]))
              ? ($maxId = (basics_cloneImm)($v))
              : null);
          }))), ($hash = (__re__)[1]), (__re__)[0]);
          const $nid = ($maxId + 1);
          ((__re__ = (hash_insert)($hash, $id, $nid)), ($hash = (__re__)[1]), (__re__)[0]);
          return $nid;
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 366:4', (sp_toHuman)($3)))),
    $hash,
  ]);
});

const $sd1$Types$TypedAst$normalizeType = (($hash, $type) => {
  return ([
    ((($type)[0] === "TypeExact")
      ? ((() => {
        const $usr = ($type)[1];
        const $args = ($type)[2];
        return ($sd1$Types$TypedAst$TypeExact)($usr, ($core$List$map)((($1) => {
          return ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $1)), ($hash = (__re__)[1]), (__re__)[0]);
        }), $args));
      }))()
      : ((($type)[0] === "TypeFn")
        ? ((() => {
          const $pars = ($type)[1];
          const $out = ($type)[2];
          return ($sd1$Types$TypedAst$TypeFn)(($sd1$Types$TypedAst$mapPars)((($1) => {
            return ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $1)), ($hash = (__re__)[1]), (__re__)[0]);
          }), $pars), ((() => {
            const $0 = $out;
            return (Object.assign)({}, $0, ({
              raw: ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $0.raw)), ($hash = (__re__)[1]), (__re__)[0]),
            }));
          }))());
        }))()
        : (((($type)[0] === "TypeRecord") && ((($type)[1])[0] === "Nothing"))
          ? ((() => {
            const $attrs = ($type)[2];
            return ($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, ($core$Dict$map)((($k, $v) => {
              return ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $v)), ($hash = (__re__)[1]), (__re__)[0]);
            }), $attrs));
          }))()
          : (((($type)[0] === "TypeRecord") && ((($type)[1])[0] === "Just"))
            ? ((() => {
              const $id = (($type)[1])[1];
              const $attrs = ($type)[2];
              return ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(((__re__ = ($sd1$Types$TypedAst$normalizeTyvarId)($hash, $id)), ($hash = (__re__)[1]), (__re__)[0])), ($core$Dict$map)((($k, $v) => {
                return ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $v)), ($hash = (__re__)[1]), (__re__)[0]);
              }), $attrs));
            }))()
            : ((($type)[0] === "TypeVar")
              ? ((() => {
                const $id = ($type)[1];
                return ($sd1$Types$TypedAst$TypeVar)(((__re__ = ($sd1$Types$TypedAst$normalizeTyvarId)($hash, $id)), ($hash = (__re__)[1]), (__re__)[0]));
              }))()
              : ((($type)[0] === "TypeError")
                ? $sd1$Types$TypedAst$TypeError
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 384:4', (sp_toHuman)($type)))))))),
    $hash,
  ]);
});

const $sd1$Types$TypedAst$patternNames = (($p) => {
  return (((($p)[0] === "PatternAny") && ((($p)[2].maybeName)[0] === "Nothing"))
    ? ((() => {
      const $pos = ($p)[1];
      return $core$Dict$empty;
    }))()
    : (((($p)[0] === "PatternAny") && ((($p)[2].maybeName)[0] === "Just"))
      ? ((() => {
        const $pos = ($p)[1];
        const $n = (($p)[2].maybeName)[1];
        const $type = ($p)[2].type;
        return ($core$Dict$ofOne)($n, ({
          pos: $pos,
          type: $type,
        }));
      }))()
      : ((($p)[0] === "PatternLiteralNumber")
        ? ((() => {
          const $pos = ($p)[1];
          return $core$Dict$empty;
        }))()
        : ((($p)[0] === "PatternLiteralText")
          ? ((() => {
            const $pos = ($p)[1];
            return $core$Dict$empty;
          }))()
          : ((($p)[0] === "PatternConstructor")
            ? ((() => {
              const $pos = ($p)[1];
              const $usr = ($p)[2];
              const $ps = ($p)[3];
              return ($core$List$for)($core$Dict$empty, $ps, (($x, $a) => {
                return ((($0) => {
                  return ($core$Dict$join)($0, $a);
                }))(($sd1$Types$TypedAst$patternNames)($x));
              }));
            }))()
            : ((($p)[0] === "PatternRecord")
              ? ((() => {
                const $pos = ($p)[1];
                const $ps = ($p)[2];
                return ($core$Dict$for)($core$Dict$empty, $ps, (($k, $3, $a) => {
                  const $pa = $3.first;
                  const $ty = $3.second;
                  return ((($1) => {
                    return ($core$Dict$join)($a, $1);
                  }))(($sd1$Types$TypedAst$patternNames)($pa));
                }));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 331:4', (sp_toHuman)($p))))))));
});

const $sd1$Compiler$Compiler$findMainType = (($umr, $modules) => {
  const $3 = ($core$List$find)((($mod) => {
    return (sp_equal)($mod.umr, $umr);
  }), $modules);
  return ((($3)[0] === "Nothing")
    ? ($core$Result$Err)(($sd1$Compiler$Error$Raw)(($core$Core$Cons)("The entry module should be:", ($core$Core$Cons)("", ($core$Core$Cons)("", ($core$Core$Cons)("But I could not find any module matching that.", $core$Core$Nil))))))
    : ((($3)[0] === "Just")
      ? ((() => {
        const $mod = ($3)[1];
        const $getMain = (($def) => {
          return ($core$Dict$get)("main", ($sd1$Types$TypedAst$patternNames)($def.pattern));
        });
        const $4 = ((($1) => {
          return ($core$List$filterMap)($getMain, $1);
        }))(($core$Dict$values)($mod.valueDefs));
        return (((($4)[0] === "Cons") && ((($4)[2])[0] === "Nil"))
          ? ((() => {
            const $type = ($4)[1].type;
            let $hash = (hash_fromList)($core$Core$Nil);
            return ($core$Result$Ok)(((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $type.raw)), ($hash = (__re__)[1]), (__re__)[0]));
          }))()
          : (true
            ? ($core$Result$Err)(($sd1$Compiler$Error$Raw)(($core$Core$Cons)("does not seem to contain a `main` definition.", ($core$Core$Cons)("", ($core$Core$Cons)("I need this to know where your program starts!", $core$Core$Nil)))))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Compiler.sp 71:12', (sp_toHuman)($4))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Compiler.sp 54:4', (sp_toHuman)($3))));
});

const $sd1$Compiler$MakeCanonical$erroro = (($ro, $pos, $msg) => {
  return ($sd1$Compiler$Error$res)($ro.errorModule, $pos, $msg);
});

const $sd1$Compiler$MakeCanonical$initEnv = (($ro) => {
  return ({
    maybeShorthandTarget: $core$Maybe$Nothing,
    nextGeneratedVariableName: 0,
    nonFn: $core$Dict$empty,
    nonRootValues: $core$Dict$empty,
    ro: $ro,
  });
});

const $sd1$Compiler$MakeCanonical$translateAttributeName = (($ro, $1) => {
  const $pos = ($1)[1];
  const $expr_ = ($1)[2];
  return ((($expr_)[0] === "Variable")
    ? ((() => {
      const $maybeType = ($expr_)[1].maybeType;
      const $word = ($expr_)[1].word;
      return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("attribute names can't start with a dot", $core$Core$Nil))
        : ($word.isUpper
          ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("attribute names must be lower case", $core$Core$Nil))
          : ((sp_not_equal)($word.maybeModule, $core$Maybe$Nothing)
            ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("attribute names must be single words", $core$Core$Nil))
            : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
              ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("attribute names can't contain dots", $core$Core$Nil))
              : ($core$Result$Ok)(({
                first: $pos,
                second: $word.name,
                third: $maybeType,
              }))))));
    }))()
    : (true
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Expecting an attribute name here", $core$Core$Nil))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 396:4', (sp_toHuman)($expr_))));
});

const $sd1$Compiler$MakeCanonical$translateAndInsertRecordAttributeType = (($ro, $faAttr, $caAttrs) => {
  return (($core$Result$onOk)((($4) => {
    const $pos = $4.first;
    const $name = $4.second;
    const $maybeFaType = $4.third;
    return (($core$Dict$member)($name, $caAttrs)
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(("Duplicate attribute name: " + $name), $core$Core$Nil))
      : ((($maybeFaType)[0] === "Nothing")
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(("I need to see the type of this attribute, `" + ($name + " as TheType`")), $core$Core$Nil))
        : ((($maybeFaType)[0] === "Just")
          ? ((() => {
            const $faType = ($maybeFaType)[1];
            return (($core$Result$onOk)((($caType) => {
              return ((sp_not_equal)($faAttr.maybeExpr, $core$Maybe$Nothing)
                ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("I'm expecting a type here; `=` is for assignign values", $core$Core$Nil))
                : ($core$Result$Ok)(((($2) => {
                  return ($core$Dict$insert)($name, $caType, $2);
                }))($caAttrs)));
            })))(((($1) => {
              return ($sd1$Compiler$MakeCanonical$translateRawType)($ro, $1);
            }))($faType));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1370:8', (sp_toHuman)($maybeFaType)))));
  })))(($sd1$Compiler$MakeCanonical$translateAttributeName)($ro, $faAttr.name));
});

const $sd1$Compiler$MakeCanonical$translatePoly = (($ro, $expr) => {
  const $3 = $expr;
  const $expr_ = ($3)[2];
  const $pos = ($3)[1];
  return (((($expr_)[0] === "Unop") && ((($expr_)[1])[0] === "UnopUnique"))
    ? ((() => {
      const $e = ($expr_)[2];
      return ($core$Result$Ok)(({
        first: $sd1$Types$Ast$Uni,
        second: $e,
      }));
    }))()
    : ((($expr_)[0] === "Poly")
      ? ((() => {
        const $numberAsString = ($expr_)[1];
        const $e = ($expr_)[2];
        const $4 = (text_toNumber)($numberAsString);
        return ((($4)[0] === "Nothing")
          ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("I need an integer number here", $core$Core$Nil))
          : ((($4)[0] === "Just")
            ? ((() => {
              const $n = ($4)[1];
              return ($core$Result$Ok)(({
                first: ($sd1$Types$Ast$Depends)($n),
                second: $e,
              }));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1445:12', (sp_toHuman)($4))));
      }))()
      : (true
        ? ($core$Result$Ok)(({
          first: $sd1$Types$Ast$Imm,
          second: $expr,
        }))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1438:4', (sp_toHuman)($expr_)))));
});

const $sd1$Compiler$MakeCanonical$translateFullType = (($ro, $expr) => {
  return (($core$Result$onOk)((($3) => {
    const $uni = $3.first;
    const $e = $3.second;
    return (($core$Result$onOk)((($raw) => {
      return ($core$Result$Ok)(({
        raw: $raw,
        uni: $uni,
      }));
    })))(($sd1$Compiler$MakeCanonical$translateRawType)($ro, $e));
  })))(((($1) => {
    return ($sd1$Compiler$MakeCanonical$translatePoly)($ro, $1);
  }))($expr));
});

const $sd1$Compiler$MakeCanonical$maybeForeignUsr = (($getter, $ro, $pos, $maybeModule, $name) => {
  return ((($maybeModule)[0] === "Nothing")
    ? ($core$Result$Ok)(($core$Dict$get)($name, ($getter)($ro.meta)))
    : ((($maybeModule)[0] === "Just")
      ? ((() => {
        const $moduleName = ($maybeModule)[1];
        const $6 = ($core$Dict$get)($moduleName, $ro.meta.moduleVisibleAsToUmr);
        return ((($6)[0] === "Just")
          ? ((() => {
            const $umr = ($6)[1];
            return ($core$Result$Ok)(($core$Maybe$Just)(($sd1$Types$Meta$USR)($umr, $name)));
          }))()
          : ((($6)[0] === "Nothing")
            ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(("I can't find the module `" + ($moduleName + "`")), $core$Core$Nil))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 82:12', (sp_toHuman)($6))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 76:4', (sp_toHuman)($maybeModule))));
});

const $sd1$Compiler$MakeCanonical$resolveToUsr = (($getter, $ro, $pos, $maybeModule, $name) => {
  return ((($1) => {
    return ($core$Result$map)((($1) => {
      return ($core$Maybe$withDefault)(($sd1$Types$Meta$USR)($ro.umr, $name), $1);
    }), $1);
  }))(($sd1$Compiler$MakeCanonical$maybeForeignUsr)($getter, $ro, $pos, $maybeModule, $name));
});

const $sd1$Compiler$MakeCanonical$resolveToTypeUsr = (($1, $2, $3, $4) => {
  return ($sd1$Compiler$MakeCanonical$resolveToUsr)((($m) => {
    return $m.globalTypes;
  }), $1, $2, $3, $4);
});

const $sd1$Compiler$MakeCanonical$translateNamedType = (($ro, $pos, $word, $caArgs) => {
  return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
    ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("I was expecting a type name here =|", $core$Core$Nil))
    : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Type names have no attributes to access", $core$Core$Nil))
      : (($core$Result$onOk)((($usr) => {
        return ($core$Result$Ok)(($sd1$Types$CanonicalAst$TypeNamed)($pos, $usr, $caArgs));
      })))(($sd1$Compiler$MakeCanonical$resolveToTypeUsr)($ro, $pos, $word.maybeModule, $word.name))));
});

const $sd1$Types$FormattableAst$sepToList = (($1) => {
  const $head = $1.first;
  const $tuples = $1.second;
  return (sp_cons)($head, ($core$List$map)($core$Tuple$second, $tuples));
});

const $sd1$Types$Pos$range = (($a, $b) => {
  const $3 = ({
    first: $a,
    second: $b,
  });
  return (((($3.first)[0] === "P") && (($3.second)[0] === "P"))
    ? ((() => {
      const $sa = ($3.first)[1];
      const $ea = ($3.first)[2];
      const $sb = ($3.second)[1];
      const $eb = ($3.second)[2];
      return ($sd1$Types$Pos$P)(($core$Basics$min)($sa, $sb), ($core$Basics$max)($ea, $eb));
    }))()
    : ((($3.first)[0] === "P")
      ? $a
      : (true
        ? $b
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/Pos.sp 45:4', (sp_toHuman)($3)))));
});

const $sd1$Compiler$MakeCanonical$translateTuple = (($ro, $translate, $sepList) => {
  const $faExpressions = ($sd1$Types$FormattableAst$sepToList)($sepList);
  return (($core$Result$onOk)((($items) => {
    const $pos = ($core$List$for)($sd1$Types$Pos$G, $faExpressions, (($6, $z) => {
      const $p = ($6)[1];
      return ($sd1$Types$Pos$range)($p, $z);
    }));
    return (((($items)[0] === "Cons") && (((($items)[2])[0] === "Cons") && (((($items)[2])[2])[0] === "Nil")))
      ? ((() => {
        const $ca1 = ($items)[1];
        const $ca2 = (($items)[2])[1];
        return ($core$Result$Ok)(((($2) => {
          return ($core$Dict$insert)("second", $ca2, $2);
        }))(((($2) => {
          return ($core$Dict$insert)("first", $ca1, $2);
        }))($core$Dict$empty)));
      }))()
      : (((($items)[0] === "Cons") && (((($items)[2])[0] === "Cons") && ((((($items)[2])[2])[0] === "Cons") && ((((($items)[2])[2])[2])[0] === "Nil"))))
        ? ((() => {
          const $ca1 = ($items)[1];
          const $ca2 = (($items)[2])[1];
          const $ca3 = ((($items)[2])[2])[1];
          return ($core$Result$Ok)(((($2) => {
            return ($core$Dict$insert)("third", $ca3, $2);
          }))(((($2) => {
            return ($core$Dict$insert)("second", $ca2, $2);
          }))(((($2) => {
            return ($core$Dict$insert)("first", $ca1, $2);
          }))($core$Dict$empty))));
        }))()
        : (true
          ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("tuples can be only of size 2 or 3, use a record instead", $core$Core$Nil))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 536:4', (sp_toHuman)($items)))));
  })))(((($1) => {
    return ($core$List$mapRes)($translate, $1);
  }))($faExpressions));
});

const $sd1$Compiler$MakeCanonical$translateTypeFunctionParameter = (($ro, $expression) => {
  const $3 = $expression;
  const $expr_ = ($3)[2];
  return (((($expr_)[0] === "Unop") && ((($expr_)[1])[0] === "UnopRecycle"))
    ? ((() => {
      const $faOperand = ($expr_)[2];
      return ((($1) => {
        return ($core$Result$map)($sd1$Types$CanonicalAst$ParRe, $1);
      }))(((($1) => {
        return ($sd1$Compiler$MakeCanonical$translateRawType)($ro, $1);
      }))($faOperand));
    }))()
    : (true
      ? ((($1) => {
        return ($core$Result$map)($sd1$Types$CanonicalAst$ParSp, $1);
      }))(((($1) => {
        return ($sd1$Compiler$MakeCanonical$translateFullType)($ro, $1);
      }))($expression))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1421:4', (sp_toHuman)($expr_))));
});

const $sd1$Compiler$MakeCanonical$translateTypeVariable = (($ro, $pos, $word) => {
  return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
    ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("I was expecting a type variable name here =|", $core$Core$Nil))
    : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Type variables have no attributes to access", $core$Core$Nil))
      : ((sp_not_equal)($word.maybeModule, $core$Maybe$Nothing)
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("No point it getting tyvars from modules?", $core$Core$Nil))
        : ($core$Result$Ok)($word.name))));
});

const $sd1$Compiler$MakeCanonical$translateRawType = (($ro, $1) => {
  const $pos = ($1)[1];
  const $expr_ = ($1)[2];
  return ((($expr_)[0] === "Variable")
    ? ((() => {
      const $maybeType = ($expr_)[1].maybeType;
      const $word = ($expr_)[1].word;
      return ((sp_not_equal)($maybeType, $core$Maybe$Nothing)
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Can't really specify the type of a type.", $core$Core$Nil))
        : ($word.isUpper
          ? ($sd1$Compiler$MakeCanonical$translateNamedType)($ro, $pos, $word, $core$Core$Nil)
          : (($core$Result$onOk)((($tyvarName) => {
            return ($core$Result$Ok)(($sd1$Types$CanonicalAst$TypeAnnotationVariable)($pos, $tyvarName));
          })))(($sd1$Compiler$MakeCanonical$translateTypeVariable)($ro, $pos, $word))));
    }))()
    : (((($expr_)[0] === "Call") && ((($expr_)[1])[0] === "Expression"))
      ? ((() => {
        const $refPos = (($expr_)[1])[1];
        const $ref = (($expr_)[1])[2];
        const $faArgs = ($expr_)[2];
        return (((($ref)[0] === "Variable") && ((($ref)[1].maybeType)[0] === "Nothing"))
          ? ((() => {
            const $word = ($ref)[1].word;
            return (($core$Result$onOk)((($caArgs) => {
              return ($sd1$Compiler$MakeCanonical$translateNamedType)($ro, $refPos, $word, $caArgs);
            })))(((($1) => {
              return ($core$List$mapRes)((($1) => {
                return ($sd1$Compiler$MakeCanonical$translateRawType)($ro, $1);
              }), $1);
            }))($faArgs));
          }))()
          : (true
            ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $refPos, ($core$Core$Cons)("I was expecting a named type here", $core$Core$Nil))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1486:12', (sp_toHuman)($ref))));
      }))()
      : ((($expr_)[0] === "List")
        ? ((() => {
          const $dotsAndItems = ($expr_)[1];
          return ((($dotsAndItems)[0] === "Nil")
            ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("You need to specify the type of the List items", $core$Core$Nil))
            : (((($dotsAndItems)[0] === "Cons") && ((($dotsAndItems)[2])[0] === "Nil"))
              ? ((() => {
                const $hasDots = ($dotsAndItems)[1].first;
                const $faItem = ($dotsAndItems)[1].second;
                return ($hasDots
                  ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("No need to use dots here", $core$Core$Nil))
                  : (($core$Result$onOk)((($caItem) => {
                    return ($core$Result$Ok)(($sd1$Compiler$CoreTypes$list)($caItem));
                  })))(($sd1$Compiler$MakeCanonical$translateRawType)($ro, $faItem)));
              }))()
              : (true
                ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("List items must all have the same type, so you can specify only one type", $core$Core$Nil))
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1498:12', (sp_toHuman)($dotsAndItems)))));
        }))()
        : ((($expr_)[0] === "Record")
          ? ((() => {
            const $attrs = ($expr_)[1].attrs;
            const $maybeExtension = ($expr_)[1].maybeExtension;
            return ((sp_not_equal)($maybeExtension, $core$Maybe$Nothing)
              ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Experimentally, extensible type annotations are disabled", $core$Core$Nil))
              : (($core$Result$onOk)((($caAttrs) => {
                return ($core$Result$Ok)(((($1) => {
                  return ($sd1$Types$CanonicalAst$TypeRecord)($pos, $1);
                }))($caAttrs));
              })))(((($0) => {
                return ($core$List$forRes)($0, $attrs, (($1, $2) => {
                  return ($sd1$Compiler$MakeCanonical$translateAndInsertRecordAttributeType)($ro, $1, $2);
                }));
              }))($core$Dict$empty)));
          }))()
          : ((($expr_)[0] === "Fn")
            ? ((() => {
              const $faParams = ($expr_)[1];
              const $faReturn = ($expr_)[2];
              return (($core$Result$onOk)((($caParams) => {
                return (($core$Result$onOk)((($caReturn) => {
                  return ($core$Result$Ok)(($sd1$Types$CanonicalAst$TypeFn)($pos, $caParams, $caReturn));
                })))(((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateFullType)($ro, $1);
                }))($faReturn));
              })))(((($1) => {
                return ($core$List$mapRes)((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateTypeFunctionParameter)($ro, $1);
                }), $1);
              }))($faParams));
            }))()
            : (((($expr_)[0] === "Binop") && ((($expr_)[1])[0] === "Tuple"))
              ? ((() => {
                const $sepList = ($expr_)[2];
                return (($core$Result$onOk)((($recordAttrs) => {
                  return ($core$Result$Ok)(($sd1$Types$CanonicalAst$TypeRecord)($pos, $recordAttrs));
                })))(((($2) => {
                  return ($sd1$Compiler$MakeCanonical$translateTuple)($ro, (($1) => {
                    return ($sd1$Compiler$MakeCanonical$translateRawType)($ro, $1);
                  }), $2);
                }))($sepList));
              }))()
              : (true
                ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Not sure what's up with this type =|", ($core$Core$Cons)((sp_toHuman)($expr_), $core$Core$Nil)))
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1472:4', (sp_toHuman)($expr_)))))))));
});

const $sd1$Compiler$MakeCanonical$translateConstructor = (($ro, $unionType, $unionUsr, $2, $constructors) => {
  const $pos = ($2)[1];
  const $expr_ = ($2)[2];
  const $zzz = ((($expr_)[0] === "Variable")
    ? ((() => {
      const $var = ($expr_)[1];
      return ($core$Result$Ok)(({
        first: $var,
        second: $core$Core$Nil,
      }));
    }))()
    : (((($expr_)[0] === "Call") && (((($expr_)[1])[0] === "Expression") && (((($expr_)[1])[2])[0] === "Variable")))
      ? ((() => {
        const $var = ((($expr_)[1])[2])[1];
        const $pars = ($expr_)[2];
        return ($core$Result$Ok)(({
          first: $var,
          second: $pars,
        }));
      }))()
      : (true
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("I was expecting a constructor name here", $core$Core$Nil))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1562:8', (sp_toHuman)($expr_)))));
  return (($core$Result$onOk)((($6) => {
    const $maybeType = $6.first.maybeType;
    const $word = $6.first.word;
    const $faPars = $6.second;
    const $isValidName = ((sp_equal)($word.modifier, $sd1$Types$Token$NameNoModifier) && ($word.isUpper && ((sp_equal)($word.maybeModule, $core$Maybe$Nothing) && (sp_equal)($word.attrPath, $core$Core$Nil))));
    return (($core$Basics$not)($isValidName)
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("I need just an Uppercase word here", $core$Core$Nil))
      : (($core$Dict$member)($word.name, $constructors)
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(("constructor " + ($word.name + " is duplicate")), $core$Core$Nil))
        : (($core$Result$onOk)((($ins) => {
          const $c = ({
            ins: $ins,
            out: $unionType,
            pos: $pos,
            typeUsr: $unionUsr,
          });
          return ($core$Result$Ok)(((($2) => {
            return ($core$Dict$insert)($word.name, $c, $2);
          }))($constructors));
        })))(((($1) => {
          return ($core$List$mapRes)((($1) => {
            return ($sd1$Compiler$MakeCanonical$translateRawType)($ro, $1);
          }), $1);
        }))($faPars))));
  })))($zzz);
});

const $sd1$Compiler$MakeCanonical$addUnivarId = (($uni, $acc) => {
  return ((($uni)[0] === "Depends")
    ? ((() => {
      const $id = ($uni)[1];
      return ($core$Dict$insert)($id, null, $acc);
    }))()
    : (true
      ? $acc
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 285:4', (sp_toHuman)($uni))));
});

const $sd1$Compiler$MakeCanonical$addPar = (($parType, $acc) => {
  return ((($parType)[0] === "ParRe")
    ? ((() => {
      const $raw = ($parType)[1];
      return ($sd1$Compiler$MakeCanonical$addRawTypeUnivars)($raw, $acc);
    }))()
    : ((($parType)[0] === "ParSp")
      ? ((() => {
        const $full = ($parType)[1];
        return ((($1) => {
          return ($sd1$Compiler$MakeCanonical$addRawTypeUnivars)($full.raw, $1);
        }))(((($1) => {
          return ($sd1$Compiler$MakeCanonical$addUnivarId)($full.uni, $1);
        }))($acc));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 293:4', (sp_toHuman)($parType))));
});

const $sd1$Compiler$MakeCanonical$addRawTypeUnivars = (($raw, $acc) => {
  return ((($raw)[0] === "TypeNamed")
    ? ((() => {
      const $args = ($raw)[3];
      return ($core$List$for)($acc, $args, $sd1$Compiler$MakeCanonical$addRawTypeUnivars);
    }))()
    : ((($raw)[0] === "TypeRecord")
      ? ((() => {
        const $attrs = ($raw)[2];
        return ($core$Dict$for)($acc, $attrs, (($k, $v, $a) => {
          return ($sd1$Compiler$MakeCanonical$addRawTypeUnivars)($v, $a);
        }));
      }))()
      : ((($raw)[0] === "TypeAnnotationVariable")
        ? $acc
        : ((($raw)[0] === "TypeError")
          ? $acc
          : ((($raw)[0] === "TypeFn")
            ? ((() => {
              const $pars = ($raw)[2];
              const $out = ($raw)[3];
              return ((($0) => {
                return ($core$List$for)($0, $pars, $sd1$Compiler$MakeCanonical$addPar);
              }))(((($1) => {
                return ($sd1$Compiler$MakeCanonical$addRawTypeUnivars)($out.raw, $1);
              }))(((($1) => {
                return ($sd1$Compiler$MakeCanonical$addUnivarId)($out.uni, $1);
              }))($acc)));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 304:4', (sp_toHuman)($raw)))))));
});

const $sd1$Compiler$MakeCanonical$addPatternUnivars = (($pattern, $acc) => {
  return ((($pattern)[0] === "PatternConstructor")
    ? ((() => {
      const $args = ($pattern)[3];
      return ($core$List$for)($acc, $args, $sd1$Compiler$MakeCanonical$addPatternUnivars);
    }))()
    : ((($pattern)[0] === "PatternRecord")
      ? ((() => {
        const $attrs = ($pattern)[3];
        return ($core$Dict$for)($acc, $attrs, (($k, $v, $a) => {
          return ($sd1$Compiler$MakeCanonical$addPatternUnivars)($v, $a);
        }));
      }))()
      : (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeAnnotation)[0] === "Just"))
        ? ((() => {
          const $rawType = (($pattern)[2].maybeAnnotation)[1];
          const $maybeName = ($pattern)[2].maybeName;
          return ($sd1$Compiler$MakeCanonical$addRawTypeUnivars)($rawType, $acc);
        }))()
        : (true
          ? $acc
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 319:4', (sp_toHuman)($pattern))))));
});

const $sd1$Compiler$MakeCanonical$deps_init = ({
  cons: $core$Set$empty,
  types: $core$Set$empty,
  values: $core$Set$empty,
});

const $sd1$Compiler$MakeCanonical$argumentDeps = (($arg, $deps) => {
  return ((($arg)[0] === "ArgumentExpression")
    ? ((() => {
      const $e = ($arg)[1];
      return ($sd1$Compiler$MakeCanonical$expressionDeps)($e, $deps);
    }))()
    : ((($arg)[0] === "ArgumentRecycle")
      ? $deps
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 264:4', (sp_toHuman)($arg))));
});

const $sd1$Compiler$MakeCanonical$typeDeps = (($type, $acc) => {
  return ((($type)[0] === "TypeNamed")
    ? ((() => {
      const $usr = ($type)[2];
      const $args = ($type)[3];
      return ((($0) => {
        return ($core$List$for)($0, $args, $sd1$Compiler$MakeCanonical$typeDeps);
      }))(((($1) => {
        return ($core$Set$insert)($usr, $1);
      }))($acc));
    }))()
    : ((($type)[0] === "TypeAnnotationVariable")
      ? $acc
      : ((($type)[0] === "TypeRecord")
        ? ((() => {
          const $attrs = ($type)[2];
          return ($core$Dict$for)($acc, $attrs, (($k, $v, $a) => {
            return ($sd1$Compiler$MakeCanonical$typeDeps)($v, $a);
          }));
        }))()
        : ((($type)[0] === "TypeError")
          ? $acc
          : ((($type)[0] === "TypeFn")
            ? ((() => {
              const $params = ($type)[2];
              const $to = ($type)[3];
              return ((($0) => {
                return ($core$List$for)($0, $params, (($par, $z) => {
                  return ((($par)[0] === "ParRe")
                    ? ((() => {
                      const $raw = ($par)[1];
                      return ($sd1$Compiler$MakeCanonical$typeDeps)($raw, $z);
                    }))()
                    : ((($par)[0] === "ParSp")
                      ? ((() => {
                        const $full = ($par)[1];
                        return ($sd1$Compiler$MakeCanonical$typeDeps)($full.raw, $z);
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 155:16', (sp_toHuman)($par))));
                }));
              }))(((($1) => {
                return ($sd1$Compiler$MakeCanonical$typeDeps)($to.raw, $1);
              }))($acc));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 146:4', (sp_toHuman)($type)))))));
});

const $sd1$Compiler$MakeCanonical$patternDeps = (($pattern, $deps) => {
  return ((($pattern)[0] === "PatternConstructor")
    ? ((() => {
      const $usr = ($pattern)[2];
      const $ps = ($pattern)[3];
      return ($core$List$for)(((() => {
        const $0 = $deps;
        return (Object.assign)({}, $0, ({
          cons: ($core$Set$insert)($usr, $0.cons),
        }));
      }))(), $ps, $sd1$Compiler$MakeCanonical$patternDeps);
    }))()
    : ((($pattern)[0] === "PatternRecord")
      ? ((() => {
        const $completeness = ($pattern)[2];
        const $ps = ($pattern)[3];
        return ($core$Dict$for)($deps, $ps, (($k, $v, $a) => {
          return ($sd1$Compiler$MakeCanonical$patternDeps)($v, $a);
        }));
      }))()
      : (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeAnnotation)[0] === "Just"))
        ? ((() => {
          const $type = (($pattern)[2].maybeAnnotation)[1];
          const $0 = $deps;
          return (Object.assign)({}, $0, ({
            types: ($sd1$Compiler$MakeCanonical$typeDeps)($type, $0.types),
          }));
        }))()
        : (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeAnnotation)[0] === "Nothing"))
          ? $deps
          : ((($pattern)[0] === "PatternLiteralNumber")
            ? $deps
            : ((($pattern)[0] === "PatternLiteralText")
              ? $deps
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 179:4', (sp_toHuman)($pattern))))))));
});

const $sd1$Compiler$MakeCanonical$parameterDeps = (($par, $deps) => {
  return ((($par)[0] === "ParameterPattern")
    ? ((() => {
      const $pa = ($par)[2];
      return ($sd1$Compiler$MakeCanonical$patternDeps)($pa, $deps);
    }))()
    : (true
      ? $deps
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 272:4', (sp_toHuman)($par))));
});

const $sd1$Compiler$MakeCanonical$expressionDeps = (($expression, $deps) => {
  return ((($expression)[0] === "LiteralNumber")
    ? $deps
    : ((($expression)[0] === "LiteralText")
      ? $deps
      : (((($expression)[0] === "Variable") && ((($expression)[2])[0] === "RefGlobal"))
        ? ((() => {
          const $usr = (($expression)[2])[1];
          const $0 = $deps;
          return (Object.assign)({}, $0, ({
            values: ($core$Set$insert)($usr, $0.values),
          }));
        }))()
        : ((($expression)[0] === "Variable")
          ? $deps
          : ((($expression)[0] === "Constructor")
            ? ((() => {
              const $usr = ($expression)[2];
              const $0 = $deps;
              return (Object.assign)({}, $0, ({
                cons: ($core$Set$insert)($usr, $0.cons),
              }));
            }))()
            : ((($expression)[0] === "Fn")
              ? ((() => {
                const $pars = ($expression)[2];
                const $body = ($expression)[3];
                return ((($1) => {
                  return ($sd1$Compiler$MakeCanonical$expressionDeps)($body, $1);
                }))(((($0) => {
                  return ($core$List$for)($0, $pars, $sd1$Compiler$MakeCanonical$parameterDeps);
                }))($deps));
              }))()
              : (((($expression)[0] === "Record") && ((($expression)[2])[0] === "Nothing"))
                ? ((() => {
                  const $exprByName = ($expression)[3];
                  return ($core$Dict$for)($deps, $exprByName, (($name, $v, $a) => {
                    return ($sd1$Compiler$MakeCanonical$expressionDeps)($v, $a);
                  }));
                }))()
                : (((($expression)[0] === "Record") && ((($expression)[2])[0] === "Just"))
                  ? ((() => {
                    const $expr = (($expression)[2])[1];
                    const $exprByName = ($expression)[3];
                    return ((($0) => {
                      return ($core$Dict$for)($0, $exprByName, (($name, $v, $a) => {
                        return ($sd1$Compiler$MakeCanonical$expressionDeps)($v, $a);
                      }));
                    }))(((($1) => {
                      return ($sd1$Compiler$MakeCanonical$expressionDeps)($expr, $1);
                    }))($deps));
                  }))()
                  : ((($expression)[0] === "Record")
                    ? ((() => {
                      const $exprByName = ($expression)[3];
                      return ($core$Dict$for)($deps, $exprByName, (($name, $v, $a) => {
                        return ($sd1$Compiler$MakeCanonical$expressionDeps)($v, $a);
                      }));
                    }))()
                    : ((($expression)[0] === "RecordAccess")
                      ? ((() => {
                        const $e = ($expression)[3];
                        return ($sd1$Compiler$MakeCanonical$expressionDeps)($e, $deps);
                      }))()
                      : ((($expression)[0] === "Call")
                        ? ((() => {
                          const $e0 = ($expression)[2];
                          const $args = ($expression)[3];
                          return ((($0) => {
                            return ($core$List$for)($0, $args, $sd1$Compiler$MakeCanonical$argumentDeps);
                          }))(((($1) => {
                            return ($sd1$Compiler$MakeCanonical$expressionDeps)($e0, $1);
                          }))($deps));
                        }))()
                        : ((($expression)[0] === "If")
                          ? ((() => {
                            const $args = ($expression)[2];
                            return ((($1) => {
                              return ($sd1$Compiler$MakeCanonical$expressionDeps)($args.false, $1);
                            }))(((($1) => {
                              return ($sd1$Compiler$MakeCanonical$expressionDeps)($args.true, $1);
                            }))(((($1) => {
                              return ($sd1$Compiler$MakeCanonical$expressionDeps)($args.condition, $1);
                            }))($deps)));
                          }))()
                          : ((($expression)[0] === "Try")
                            ? ((() => {
                              const $patternsAndExpressions = ($expression)[2].patternsAndExpressions;
                              const $value = ($expression)[2].value;
                              return ((($0) => {
                                return ($core$List$for)($0, $patternsAndExpressions, (($5, $d) => {
                                  const $u = $5.first;
                                  const $p = $5.second;
                                  const $b = $5.third;
                                  return ((($1) => {
                                    return ($sd1$Compiler$MakeCanonical$expressionDeps)($b, $1);
                                  }))(((($1) => {
                                    return ($sd1$Compiler$MakeCanonical$patternDeps)($p, $1);
                                  }))($d));
                                }));
                              }))(((($1) => {
                                return ($sd1$Compiler$MakeCanonical$expressionDeps)($value, $1);
                              }))($deps));
                            }))()
                            : ((($expression)[0] === "LetIn")
                              ? ((() => {
                                const $valueDef = ($expression)[1];
                                const $e = ($expression)[2];
                                return ((($1) => {
                                  return ($sd1$Compiler$MakeCanonical$expressionDeps)($e, $1);
                                }))(((($1) => {
                                  return ($sd1$Compiler$MakeCanonical$expressionDeps)($valueDef.body, $1);
                                }))(((($1) => {
                                  return ($sd1$Compiler$MakeCanonical$patternDeps)($valueDef.pattern, $1);
                                }))($deps)));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 203:4', (sp_toHuman)($expression))))))))))))))));
});

const $sd1$Compiler$CoreTypes$cons = ($sd1$Compiler$CoreTypes$makeUsr)("Cons");

const $sd1$Compiler$CoreTypes$nil = ($sd1$Compiler$CoreTypes$makeUsr)("Nil");

const $sd1$Compiler$MakeCanonical$error = (($env, $pos, $msg) => {
  return ($sd1$Compiler$Error$res)($env.ro.errorModule, $pos, $msg);
});

const $sd1$Compiler$MakeCanonical$translateArgument = (($env, $faExpr) => {
  return (((($faExpr)[0] === "Expression") && (((($faExpr)[2])[0] === "Unop") && ((((($faExpr)[2])[1])[0] === "UnopRecycle") && (((($faExpr)[2])[2])[0] === "Expression"))))
    ? ((() => {
      const $pos = ((($faExpr)[2])[2])[1];
      const $faOperand = ((($faExpr)[2])[2])[2];
      return ((($faOperand)[0] === "Variable")
        ? ((() => {
          const $maybeType = ($faOperand)[1].maybeType;
          const $word = ($faOperand)[1].word;
          return ((sp_not_equal)($maybeType, $core$Maybe$Nothing)
            ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Sorry, at least for now annotations are not supported here", $core$Core$Nil))
            : ((sp_not_equal)($word.maybeModule, $core$Maybe$Nothing)
              ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Only values declared inside a function scope can be mutated!", $core$Core$Nil))
              : ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
                ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("This can't start with .", $core$Core$Nil))
                : ($word.isUpper
                  ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Can't recycle constructors", $core$Core$Nil))
                  : ($core$Result$Ok)(($sd1$Types$CanonicalAst$ArgumentRecycle)($pos, $word.name, $word.attrPath))))));
        }))()
        : (true
          ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("I can recycle only variables!", $core$Core$Nil))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1145:12', (sp_toHuman)($faOperand))));
    }))()
    : (true
      ? (($core$Result$onOk)((($caExpr) => {
        return ($core$Result$Ok)(($sd1$Types$CanonicalAst$ArgumentExpression)($caExpr));
      })))(((($1) => {
        return ($sd1$Compiler$MakeCanonical$translateExpression)($env, $1);
      }))($faExpr))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1143:4', (sp_toHuman)($faExpr))));
});

const $sd1$Compiler$MakeCanonical$translateArgumentsAndPlaceholders = (($pos, $env, $faArgs) => {
  const $insertArg = (($faArg, $4) => {
    const $arity = $4.arity;
    const $caArgs = $4.caArgs;
    const $caPars = $4.caPars;
    const $6 = $faArg;
    const $faArg_ = ($6)[2];
    const $pos = ($6)[1];
    return ((($faArg_)[0] === "ArgumentPlaceholder")
      ? ((() => {
        const $name = (text_fromNumber)($arity);
        return ($core$Result$Ok)(({
          arity: ($arity + 1),
          caArgs: (sp_cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$Variable)($pos, ($sd1$Types$Ast$RefLocal)($name))), $caArgs),
          caPars: (sp_cons)(($sd1$Types$CanonicalAst$ParameterPlaceholder)($name, $arity), $caPars),
        }));
      }))()
      : (true
        ? (($core$Result$onOk)((($caArg) => {
          return ($core$Result$Ok)(({
            arity: ($arity + 1),
            caArgs: (sp_cons)($caArg, $caArgs),
            caPars: $caPars,
          }));
        })))(($sd1$Compiler$MakeCanonical$translateArgument)($env, $faArg))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 922:8', (sp_toHuman)($faArg_))));
  });
  return (($core$Result$onOk)((($4) => {
    const $arity = $4.arity;
    const $caArgs = $4.caArgs;
    const $caPars = $4.caPars;
    const $wrap = ((sp_equal)($caPars, $core$Core$Nil)
      ? $core$Basics$identity
      : (($call) => {
        return ($sd1$Types$CanonicalAst$Fn)($pos, ($core$List$reverse)($caPars), $call);
      }));
    return ($core$Result$Ok)(({
      first: ($core$List$reverse)($caArgs),
      second: $wrap,
    }));
  })))(((($0) => {
    return ($core$List$forRes)($0, $faArgs, $insertArg);
  }))(({
    arity: 0,
    caArgs: $core$Core$Nil,
    caPars: $core$Core$Nil,
  })));
});

const $sd1$Compiler$MakeCanonical$notAllSeparators = (($f, $ls) => {
  return ((($ls)[0] === "Nil")
    ? false
    : ((($ls)[0] === "Cons")
      ? ((() => {
        const $sep = ($ls)[1].first;
        const $item = ($ls)[1].second;
        const $tail = ($ls)[2];
        return (($f)($sep)
          ? ($sd1$Compiler$MakeCanonical$notAllSeparators)($f, $tail)
          : true);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1247:4', (sp_toHuman)($ls))));
});

const $sd1$Compiler$MakeCanonical$sameDirectionAs = (($a, $b) => {
  return ((sp_equal)($a.symbol, $b.symbol)
    ? true
    : ((() => {
      const $3 = $a.symbol;
      return ((">" === $3)
        ? (sp_equal)($b.symbol, ">=")
        : ((">=" === $3)
          ? (sp_equal)($b.symbol, ">")
          : (("<" === $3)
            ? (sp_equal)($b.symbol, "<=")
            : (("<=" === $3)
              ? (sp_equal)($b.symbol, "<")
              : (true
                ? false
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1265:8', (sp_toHuman)($3)))))));
    }))());
});

const $sd1$Compiler$MakeCanonical$makeBinop = (($ro, $pos, $left, $op, $right) => {
  return ((sp_equal)($op.symbol, $sd1$Prelude$sendRight.symbol)
    ? ((($right)[0] === "ArgumentExpression")
      ? ((() => {
        const $ref = ($right)[1];
        return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Call)($pos, $ref, ($core$Core$Cons)($left, $core$Core$Nil)));
      }))()
      : ((($right)[0] === "ArgumentRecycle")
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Can't >> to a recyclable", $core$Core$Nil))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1326:8', (sp_toHuman)($right))))
    : ((sp_equal)($op.symbol, $sd1$Prelude$sendLeft.symbol)
      ? ((($left)[0] === "ArgumentExpression")
        ? ((() => {
          const $ref = ($left)[1];
          return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Call)($pos, $ref, ($core$Core$Cons)($right, $core$Core$Nil)));
        }))()
        : ((($left)[0] === "ArgumentRecycle")
          ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Can't << to a recyclable", $core$Core$Nil))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1336:8', (sp_toHuman)($left))))
      : ($core$Result$Ok)(($sd1$Types$CanonicalAst$Call)($pos, ($sd1$Types$CanonicalAst$Variable)($pos, ($sd1$Types$Ast$RefGlobal)($op.usr)), ($core$Core$Cons)($left, ($core$Core$Cons)($right, $core$Core$Nil))))));
});

const $sd1$Compiler$MakeCanonical$translateBinopSepListRec = (($env, $pos, $leftAccum, $opsAndRight) => {
  return ((($opsAndRight)[0] === "Nil")
    ? ($core$Result$Ok)($leftAccum)
    : ((($opsAndRight)[0] === "Cons")
      ? ((() => {
        const $op = ($opsAndRight)[1].first;
        const $faRight = ($opsAndRight)[1].second;
        const $tail = ($opsAndRight)[2];
        return (($core$Result$onOk)((($caRight) => {
          return (($core$Result$onOk)((($binop) => {
            return ($sd1$Compiler$MakeCanonical$translateBinopSepListRec)($env, $pos, $binop, $tail);
          })))(($sd1$Compiler$MakeCanonical$makeBinop)($env.ro, $pos, ($sd1$Types$CanonicalAst$ArgumentExpression)($leftAccum), $op, $caRight));
        })))(($sd1$Compiler$MakeCanonical$translateArgument)($env, $faRight));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1305:4', (sp_toHuman)($opsAndRight))));
});

const $sd1$Compiler$MakeCanonical$translateBinopSepList_leftAssociative = (($env, $pos, $leftAccum, $opsAndRight) => {
  return (($core$Result$onOk)((($caLeftAccum) => {
    return ($sd1$Compiler$MakeCanonical$translateBinopSepListRec)($env, $pos, $caLeftAccum, $opsAndRight);
  })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $leftAccum));
});

const $sd1$Compiler$MakeCanonical$translateBinopSepList_rightAssociative = (($env, $pos, $left, $opsAndRight) => {
  return (($core$Result$onOk)((($caLeft) => {
    return ((($opsAndRight)[0] === "Nil")
      ? ($core$Result$Ok)($caLeft)
      : ((($opsAndRight)[0] === "Cons")
        ? ((() => {
          const $op = ($opsAndRight)[1].first;
          const $right = ($opsAndRight)[1].second;
          const $tail = ($opsAndRight)[2];
          return (($core$Result$onOk)((($caRight) => {
            return ($sd1$Compiler$MakeCanonical$makeBinop)($env.ro, $pos, ($sd1$Types$CanonicalAst$ArgumentExpression)($caLeft), $op, ($sd1$Types$CanonicalAst$ArgumentExpression)($caRight));
          })))(($sd1$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env, $pos, $right, $tail));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1285:4', (sp_toHuman)($opsAndRight))));
  })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $left));
});

const $sd1$Compiler$MakeCanonical$translateSimpleBinop = (($env, $pos, $left, $op, $right) => {
  return (($core$Result$onOk)((($l) => {
    return (($core$Result$onOk)((($r) => {
      return ($sd1$Compiler$MakeCanonical$makeBinop)($env.ro, $pos, $l, $op, $r);
    })))(($sd1$Compiler$MakeCanonical$translateArgument)($env, $right));
  })))(($sd1$Compiler$MakeCanonical$translateArgument)($env, $left));
});

const $sd1$Compiler$MakeCanonical$translateBinops = (($env, $pos, $group, $1) => {
  const $firstItem = $1.first;
  const $firstTail = $1.second;
  return ((($firstTail)[0] === "Nil")
    ? ($sd1$Compiler$MakeCanonical$translateExpression)($env, $firstItem)
    : (((($firstTail)[0] === "Cons") && ((($firstTail)[2])[0] === "Nil"))
      ? ((() => {
        const $firstSep = ($firstTail)[1].first;
        const $secondItem = ($firstTail)[1].second;
        return ((($group)[0] === "Tuple")
          ? (($core$Result$onOk)((($first) => {
            return (($core$Result$onOk)((($second) => {
              return ($core$Result$Ok)(((($2) => {
                return ($sd1$Types$CanonicalAst$Record)($pos, $core$Maybe$Nothing, $2);
              }))(((($2) => {
                return ($core$Dict$insert)("second", $second, $2);
              }))(((($2) => {
                return ($core$Dict$insert)("first", $first, $2);
              }))($core$Dict$empty))));
            })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $secondItem));
          })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $firstItem))
          : (true
            ? ($sd1$Compiler$MakeCanonical$translateSimpleBinop)($env, $pos, $firstItem, $firstSep, $secondItem)
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1178:12', (sp_toHuman)($group))));
      }))()
      : (((($firstTail)[0] === "Cons") && ((($firstTail)[2])[0] === "Cons"))
        ? ((() => {
          const $firstSep = ($firstTail)[1].first;
          const $secondItem = ($firstTail)[1].second;
          const $secondSep = (($firstTail)[2])[1].first;
          const $thirdItem = (($firstTail)[2])[1].second;
          const $thirdTail = (($firstTail)[2])[2];
          const $secondTail = (sp_cons)(({
            first: $secondSep,
            second: $thirdItem,
          }), $thirdTail);
          return ((($group)[0] === "Comparison")
            ? (($sd1$Compiler$MakeCanonical$notAllSeparators)((($1) => {
              return ($sd1$Compiler$MakeCanonical$sameDirectionAs)($firstSep, $1);
            }), $secondTail)
              ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("can't mix comparison ops with different direction", $core$Core$Nil))
              : ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("NI compops expansion", $core$Core$Nil)))
            : ((($group)[0] === "Logical")
              ? (($sd1$Compiler$MakeCanonical$notAllSeparators)((($x) => {
                return (sp_equal)($x, $firstSep);
              }), $secondTail)
                ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Mixing `and` and `or` is ambiguous. Use parens!", $core$Core$Nil))
                : ($sd1$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env, $pos, $firstItem, $firstTail))
              : ((($group)[0] === "Tuple")
                ? ((sp_not_equal)($thirdTail, $core$Core$Nil)
                  ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Tuples can't have more than 3 items, use a record instead.", $core$Core$Nil))
                  : (($core$Result$onOk)((($first) => {
                    return (($core$Result$onOk)((($second) => {
                      return (($core$Result$onOk)((($third) => {
                        return ($core$Result$Ok)(((($2) => {
                          return ($sd1$Types$CanonicalAst$Record)($pos, $core$Maybe$Nothing, $2);
                        }))(((($2) => {
                          return ($core$Dict$insert)("third", $third, $2);
                        }))(((($2) => {
                          return ($core$Dict$insert)("second", $second, $2);
                        }))(((($2) => {
                          return ($core$Dict$insert)("first", $first, $2);
                        }))($core$Dict$empty)))));
                      })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $thirdItem));
                    })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $secondItem));
                  })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $firstItem)))
                : ((($group)[0] === "Pipe")
                  ? (($sd1$Compiler$MakeCanonical$notAllSeparators)((($x) => {
                    return (sp_equal)($x, $firstSep);
                  }), $secondTail)
                    ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Mixing pipes is ambigous. Use parens.", $core$Core$Nil))
                    : ((sp_equal)($firstSep.associativity, $sd1$Types$Op$Right)
                      ? ($sd1$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env, $pos, $firstItem, $firstTail)
                      : ($sd1$Compiler$MakeCanonical$translateBinopSepList_leftAssociative)($env, $pos, $firstItem, $firstTail)))
                  : ((($group)[0] === "Mutop")
                    ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("mutops can't be chained", $core$Core$Nil))
                    : (true
                      ? ($sd1$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env, $pos, $firstItem, $firstTail)
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1196:12', (sp_toHuman)($group))))))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1173:4', (sp_toHuman)($firstTail)))));
});

const $sd1$Compiler$MakeCanonical$translateNumber = (($ro, $isPercent, $constructor, $pos, $numberAsText) => {
  const $6 = (text_toNumber)(($core$Text$replace)("_", "", $numberAsText));
  return ((($6)[0] === "Nothing")
    ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(("invalid number: `" + ($numberAsText + "`")), ($core$Core$Cons)("TODO link to documentation on valid number formats", $core$Core$Nil)))
    : ((($6)[0] === "Just")
      ? ((() => {
        const $n = ($6)[1];
        return ($core$Result$Ok)(($constructor)($pos, ($isPercent
          ? (sp_divide)($n, 100)
          : $n)));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1049:4', (sp_toHuman)($6))));
});

const $sd1$Compiler$MakeCanonical$translatePatternAny = (($env, $pos, $maybeFaType, $word) => {
  return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
    ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Record access shorthands", $core$Core$Nil))
    : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("To access attributes in pattern matching use { with theAttributeName = theVariableName }", $core$Core$Nil))
      : ((sp_not_equal)($word.maybeModule, $core$Maybe$Nothing)
        ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("You can't access modules here...", $core$Core$Nil))
        : ((() => {
          const $resultMaybeRaw = ((($maybeFaType)[0] === "Just")
            ? ((() => {
              const $faType = ($maybeFaType)[1];
              return ((($1) => {
                return ($core$Result$map)($core$Maybe$Just, $1);
              }))(((($1) => {
                return ($sd1$Compiler$MakeCanonical$translateRawType)($env.ro, $1);
              }))($faType));
            }))()
            : ((($maybeFaType)[0] === "Nothing")
              ? ($core$Result$Ok)($core$Maybe$Nothing)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 440:12', (sp_toHuman)($maybeFaType))));
          return (($core$Result$onOk)((($maybeRaw) => {
            const $maybeName = ((sp_equal)($word.name, "_")
              ? $core$Maybe$Nothing
              : ($core$Maybe$Just)($word.name));
            return ($core$Result$Ok)(($sd1$Types$CanonicalAst$PatternAny)($pos, ({
              maybeAnnotation: $maybeRaw,
              maybeName: $maybeName,
            })));
          })))($resultMaybeRaw);
        }))())));
});

const $sd1$Compiler$MakeCanonical$resolveToConstructorUsr = (($1, $2, $3, $4) => {
  return ($sd1$Compiler$MakeCanonical$resolveToUsr)((($m) => {
    return $m.globalValues;
  }), $1, $2, $3, $4);
});

const $sd1$Compiler$MakeCanonical$translatePatternConstructor = (($env, $pos, $word, $args) => {
  return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
    ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Constructor names cannot have modifiers", $core$Core$Nil))
    : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Constructors don't have attributes", $core$Core$Nil))
      : (($core$Result$onOk)((($usr) => {
        return ($core$Result$Ok)(($sd1$Types$CanonicalAst$PatternConstructor)($pos, $usr, $args));
      })))(($sd1$Compiler$MakeCanonical$resolveToConstructorUsr)($env.ro, $pos, $word.maybeModule, $word.name))));
});

const $sd1$Compiler$MakeCanonical$insertAttribute = (($env, $attr, $caAttrs) => {
  return (($core$Result$onOk)((($4) => {
    const $pos = $4.first;
    const $caName = $4.second;
    const $maybeFaType = $4.third;
    return (($core$Dict$member)($caName, $caAttrs)
      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)(("duplicate attribute name in pattern: " + $caName), $core$Core$Nil))
      : ((() => {
        const $5 = ({
          first: $attr.maybeExpr,
          second: $maybeFaType,
        });
        return (((($5.first)[0] === "Just") && ((($5.second)[0] === "Just") && ((($5.second)[1])[0] === "Expression")))
          ? ((() => {
            const $typePos = (($5.second)[1])[1];
            return ($sd1$Compiler$MakeCanonical$error)($env, $typePos, ($core$Core$Cons)("if you want to annotate the attribute, use { x = y as TheType }", $core$Core$Nil));
          }))()
          : (((($5.first)[0] === "Nothing") && (($5.second)[0] === "Just"))
            ? ((() => {
              const $faType = ($5.second)[1];
              return (($core$Result$onOk)((($caType) => {
                return ($core$Result$Ok)(((($2) => {
                  return ($core$Dict$insert)($caName, ($sd1$Types$CanonicalAst$PatternAny)($pos, ({
                    maybeAnnotation: ($core$Maybe$Just)($caType),
                    maybeName: ($core$Maybe$Just)($caName),
                  })), $2);
                }))($caAttrs));
              })))(($sd1$Compiler$MakeCanonical$translateRawType)($env.ro, $faType));
            }))()
            : (((($5.first)[0] === "Just") && (($5.second)[0] === "Nothing"))
              ? ((() => {
                const $faPattern = ($5.first)[1];
                return (($core$Result$onOk)((($caPattern) => {
                  return ($core$Result$Ok)(((($2) => {
                    return ($core$Dict$insert)($caName, $caPattern, $2);
                  }))($caAttrs));
                })))(((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $1);
                }))($faPattern));
              }))()
              : (((($5.first)[0] === "Nothing") && (($5.second)[0] === "Nothing"))
                ? ($core$Result$Ok)(((($2) => {
                  return ($core$Dict$insert)($caName, ($sd1$Types$CanonicalAst$PatternAny)($pos, ({
                    maybeAnnotation: $core$Maybe$Nothing,
                    maybeName: ($core$Maybe$Just)($caName),
                  })), $2);
                }))($caAttrs))
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 472:8', (sp_toHuman)($5))))));
      }))());
  })))(($sd1$Compiler$MakeCanonical$translateAttributeName)($env.ro, $attr.name));
});

const $sd1$Compiler$MakeCanonical$translatePatternRecord = (($env, $pos, $maybeMaybeExt, $attrs) => {
  const $zzz = (((($maybeMaybeExt)[0] === "Just") && (((($maybeMaybeExt)[1])[0] === "Just") && (((($maybeMaybeExt)[1])[1])[0] === "Expression")))
    ? ((() => {
      const $p = ((($maybeMaybeExt)[1])[1])[1];
      const $expr_ = ((($maybeMaybeExt)[1])[1])[2];
      return ($sd1$Compiler$MakeCanonical$error)($env, $p, ($core$Core$Cons)("Can't extend patterns", $core$Core$Nil));
    }))()
    : (((($maybeMaybeExt)[0] === "Just") && ((($maybeMaybeExt)[1])[0] === "Nothing"))
      ? ($core$Result$Ok)($sd1$Types$CanonicalAst$Partial)
      : ((($maybeMaybeExt)[0] === "Nothing")
        ? ($core$Result$Ok)($sd1$Types$CanonicalAst$Complete)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 503:8', (sp_toHuman)($maybeMaybeExt)))));
  return (($core$Result$onOk)((($completeness) => {
    return ((($1) => {
      return ($core$Result$map)((($x) => {
        return ($sd1$Types$CanonicalAst$PatternRecord)($pos, $completeness, $x);
      }), $1);
    }))(((($0) => {
      return ($core$List$forRes)($0, $attrs, (($1, $2) => {
        return ($sd1$Compiler$MakeCanonical$insertAttribute)($env, $1, $2);
      }));
    }))($core$Dict$empty));
  })))($zzz);
});

const $sd1$Types$CanonicalAst$patternPos = (($pa) => {
  return ((($pa)[0] === "PatternAny")
    ? ((() => {
      const $p = ($pa)[1];
      return $p;
    }))()
    : ((($pa)[0] === "PatternLiteralText")
      ? ((() => {
        const $p = ($pa)[1];
        return $p;
      }))()
      : ((($pa)[0] === "PatternLiteralNumber")
        ? ((() => {
          const $p = ($pa)[1];
          return $p;
        }))()
        : ((($pa)[0] === "PatternConstructor")
          ? ((() => {
            const $p = ($pa)[1];
            return $p;
          }))()
          : ((($pa)[0] === "PatternRecord")
            ? ((() => {
              const $p = ($pa)[1];
              return $p;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 195:4', (sp_toHuman)($pa)))))));
});

const $sd1$Compiler$MakeCanonical$translateRawPattern = (($env, $1) => {
  const $pos = ($1)[1];
  const $expr_ = ($1)[2];
  return ((($expr_)[0] === "Variable")
    ? ((() => {
      const $maybeType = ($expr_)[1].maybeType;
      const $word = ($expr_)[1].word;
      return ($word.isUpper
        ? ((sp_not_equal)($maybeType, $core$Maybe$Nothing)
          ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Pattern constructors can't have type annotations", $core$Core$Nil))
          : ($sd1$Compiler$MakeCanonical$translatePatternConstructor)($env, $pos, $word, $core$Core$Nil))
        : ($sd1$Compiler$MakeCanonical$translatePatternAny)($env, $pos, $maybeType, $word));
    }))()
    : (((($expr_)[0] === "Call") && ((($expr_)[1])[0] === "Expression"))
      ? ((() => {
        const $pos = (($expr_)[1])[1];
        const $ref = (($expr_)[1])[2];
        const $faArgs = ($expr_)[2];
        return ((($ref)[0] === "Variable")
          ? ((() => {
            const $maybeType = ($ref)[1].maybeType;
            const $word = ($ref)[1].word;
            return (($core$Basics$not)($word.isUpper)
              ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("I need an uppercase constructor name here", $core$Core$Nil))
              : ((sp_not_equal)($maybeType, $core$Maybe$Nothing)
                ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Constructors can't be annotated (yet? would it make sense?)", $core$Core$Nil))
                : (($core$Result$onOk)((($caPars) => {
                  return ($sd1$Compiler$MakeCanonical$translatePatternConstructor)($env, $pos, $word, $caPars);
                })))(((($1) => {
                  return ($core$List$mapRes)((($1) => {
                    return ($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $1);
                  }), $1);
                }))($faArgs))));
          }))()
          : (true
            ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("I was expecting a constructor name here", $core$Core$Nil))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 584:12', (sp_toHuman)($ref))));
      }))()
      : ((($expr_)[0] === "List")
        ? ((() => {
          const $faItems = ($expr_)[1];
          const $reversedFaItems = ($core$List$reverse)($faItems);
          const $pushItem = (($pattern, $last) => {
            return ($sd1$Types$CanonicalAst$PatternConstructor)(($sd1$Types$CanonicalAst$patternPos)($pattern), $sd1$Compiler$CoreTypes$cons, ($core$Core$Cons)($pattern, ($core$Core$Cons)($last, $core$Core$Nil)));
          });
          return ((($reversedFaItems)[0] === "Nil")
            ? ($core$Result$Ok)(($sd1$Types$CanonicalAst$PatternConstructor)($pos, $sd1$Compiler$CoreTypes$nil, $core$Core$Nil))
            : (((($reversedFaItems)[0] === "Cons") && ((($reversedFaItems)[1].second)[0] === "Expression"))
              ? ((() => {
                const $lastHasDots = ($reversedFaItems)[1].first;
                const $pos = (($reversedFaItems)[1].second)[1];
                const $lastFaExpr = (($reversedFaItems)[1].second)[2];
                const $reversedFaRest = ($reversedFaItems)[2];
                return (($core$List$any)($core$Tuple$first, $reversedFaRest)
                  ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("only the last item in a list can have ... triple dots", $core$Core$Nil))
                  : (($core$Basics$not)($lastHasDots)
                    ? (($core$Result$onOk)((($reversedCaItems) => {
                      return ($core$Result$Ok)(($core$List$for)(($sd1$Types$CanonicalAst$PatternConstructor)($pos, $sd1$Compiler$CoreTypes$nil, $core$Core$Nil), $reversedCaItems, $pushItem));
                    })))(((($1) => {
                      return ($core$List$mapRes)((($4) => {
                        const $hasDots = $4.first;
                        const $expr = $4.second;
                        return ($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $expr);
                      }), $1);
                    }))($reversedFaItems))
                    : (($core$Result$onOk)((($reversedCaRest) => {
                      return ((($lastFaExpr)[0] === "Variable")
                        ? ((() => {
                          const $maybeType = ($lastFaExpr)[1].maybeType;
                          const $word = ($lastFaExpr)[1].word;
                          return (($core$Result$onOk)((($caInit) => {
                            return ($core$Result$Ok)(($core$List$for)($caInit, $reversedCaRest, $pushItem));
                          })))(($sd1$Compiler$MakeCanonical$translatePatternAny)($env, $pos, $maybeType, $word));
                        }))()
                        : (true
                          ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("sorry, I don't understand the dots here...", $core$Core$Nil))
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 630:24', (sp_toHuman)($lastFaExpr))));
                    })))(((($1) => {
                      return ($core$List$mapRes)((($4) => {
                        const $hasDots = $4.first;
                        const $expr = $4.second;
                        return ($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $expr);
                      }), $1);
                    }))($reversedFaRest))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 608:12', (sp_toHuman)($reversedFaItems))));
        }))()
        : ((($expr_)[0] === "Record")
          ? ((() => {
            const $attrs = ($expr_)[1].attrs;
            const $maybeExtension = ($expr_)[1].maybeExtension;
            return ($sd1$Compiler$MakeCanonical$translatePatternRecord)($env, $pos, $maybeExtension, $attrs);
          }))()
          : (((($expr_)[0] === "Binop") && ((($expr_)[1])[0] === "Tuple"))
            ? ((() => {
              const $sepList = ($expr_)[2];
              return (($core$Result$onOk)((($recordAttrs) => {
                return ($core$Result$Ok)(($sd1$Types$CanonicalAst$PatternRecord)($pos, $sd1$Types$CanonicalAst$Complete, $recordAttrs));
              })))(((($2) => {
                return ($sd1$Compiler$MakeCanonical$translateTuple)($env.ro, (($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $1);
                }), $2);
              }))($sepList));
            }))()
            : (((($expr_)[0] === "Binop") && ((($expr_)[1])[0] === "Cons"))
              ? ((() => {
                const $sepList = ($expr_)[2];
                return (($core$Result$onOk)((($caPas) => {
                  const $4 = ($core$List$reverse)($caPas);
                  return ((($4)[0] === "Cons")
                    ? ((() => {
                      const $last = ($4)[1];
                      const $rest = ($4)[2];
                      return ($core$Result$Ok)(((($0) => {
                        return ($core$List$for)($0, $rest, (($item, $list) => {
                          return ($sd1$Types$CanonicalAst$PatternConstructor)($pos, $sd1$Compiler$CoreTypes$cons, ($core$Core$Cons)($item, ($core$Core$Cons)($list, $core$Core$Nil)));
                        }));
                      }))($last));
                    }))()
                    : ((($4)[0] === "Nil")
                      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("should not happen: empty cons pattern", $core$Core$Nil))
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 657:12', (sp_toHuman)($4))));
                })))(((($1) => {
                  return ($core$List$mapRes)((($1) => {
                    return ($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $1);
                  }), $1);
                }))(($sd1$Types$FormattableAst$sepToList)($sepList)));
              }))()
              : ((($expr_)[0] === "Binop")
                ? ((() => {
                  const $opPrecedence = ($expr_)[1];
                  const $sepList = ($expr_)[2];
                  return ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("This binop can't be used in pattern matching", $core$Core$Nil));
                }))()
                : ((($expr_)[0] === "LiteralText")
                  ? ((() => {
                    const $l = ($expr_)[1];
                    return ($core$Result$Ok)(($sd1$Types$CanonicalAst$PatternLiteralText)($pos, $l));
                  }))()
                  : ((($expr_)[0] === "LiteralNumber")
                    ? ((() => {
                      const $isPercent = ($expr_)[1];
                      const $l = ($expr_)[2];
                      return ($sd1$Compiler$MakeCanonical$translateNumber)($env.ro, $isPercent, $sd1$Types$CanonicalAst$PatternLiteralNumber, $pos, $l);
                    }))()
                    : ((($expr_)[0] === "Statements")
                      ? ((() => {
                        const $stats = ($expr_)[1];
                        return ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("WAT", $core$Core$Nil));
                      }))()
                      : ((($expr_)[0] === "Fn")
                        ? ((() => {
                          const $args = ($expr_)[1];
                          const $body = ($expr_)[2];
                          return ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Can't pattern match on functions. =(", $core$Core$Nil));
                        }))()
                        : ((($expr_)[0] === "Unop")
                          ? ((() => {
                            const $unop = ($expr_)[1];
                            const $expr = ($expr_)[2];
                            return ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("This op can't be used in pattern matching", $core$Core$Nil));
                          }))()
                          : ((($expr_)[0] === "If")
                            ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("if..then can't be used in pattern matching", $core$Core$Nil))
                            : ((($expr_)[0] === "Try")
                              ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("try..as can't be used in pattern matching", $core$Core$Nil))
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 571:4', (sp_toHuman)($expr_))))))))))))))));
});

const $sd1$Compiler$MakeCanonical$translateFullPattern = (($env, $expr) => {
  return (($core$Result$onOk)((($3) => {
    const $uni = $3.first;
    const $e = $3.second;
    return (($core$Result$onOk)((($caPa) => {
      return ($core$Result$Ok)(({
        first: $uni,
        second: $caPa,
      }));
    })))(($sd1$Compiler$MakeCanonical$translateRawPattern)($env, $e));
  })))(((($1) => {
    return ($sd1$Compiler$MakeCanonical$translatePoly)($env.ro, $1);
  }))($expr));
});

const $sd1$Compiler$MakeCanonical$translateParameter = (($env, $fa) => {
  const $3 = $fa;
  const $faExpr = ($3)[2];
  const $pos = ($3)[1];
  const $maybeRecycle = (((($faExpr)[0] === "Unop") && (((($faExpr)[1])[0] === "UnopRecycle") && ((($faExpr)[2])[0] === "Expression")))
    ? ((() => {
      const $p = (($faExpr)[2])[1];
      const $faOperand = (($faExpr)[2])[2];
      return (((($faOperand)[0] === "Variable") && ((($faOperand)[1].maybeType)[0] === "Nothing"))
        ? ((() => {
          const $word = ($faOperand)[1].word;
          return ($core$Result$Ok)(($core$Maybe$Just)($word));
        }))()
        : (true
          ? ($sd1$Compiler$MakeCanonical$error)($env, $p, ($core$Core$Cons)("@ should be followed by a variable name to recycle!", $core$Core$Nil))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1016:16', (sp_toHuman)($faOperand))));
    }))()
    : (true
      ? ($core$Result$Ok)($core$Maybe$Nothing)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1014:8', (sp_toHuman)($faExpr))));
  return (($core$Result$onOk)((($maybeWord) => {
    return ((($maybeWord)[0] === "Just")
      ? ((() => {
        const $word = ($maybeWord)[1];
        const $isValid = ((sp_equal)($word.modifier, $sd1$Types$Token$NameNoModifier) && (($core$Basics$not)($word.isUpper) && ((sp_equal)($word.maybeModule, $core$Maybe$Nothing) && (sp_equal)($word.attrPath, $core$Core$Nil))));
        return (($core$Basics$not)($isValid)
          ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("I was expecting a local variable name here... =|", $core$Core$Nil))
          : ($core$Result$Ok)(($sd1$Types$CanonicalAst$ParameterRecycle)($pos, $word.name)));
      }))()
      : ((($maybeWord)[0] === "Nothing")
        ? (($core$Result$onOk)((($5) => {
          const $uni = $5.first;
          const $ca = $5.second;
          return ($core$Result$Ok)(($sd1$Types$CanonicalAst$ParameterPattern)($uni, $ca));
        })))(($sd1$Compiler$MakeCanonical$translateFullPattern)($env, $fa))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1028:4', (sp_toHuman)($maybeWord))));
  })))($maybeRecycle);
});

const $sd1$Compiler$MakeCanonical$translateAndInsertRecordAttribute = (($env, $attr, $caAttrsAccum) => {
  return (($core$Result$onOk)((($4) => {
    const $pos = $4.first;
    const $caName = $4.second;
    const $maybeFaType = $4.third;
    return (($core$Dict$member)($caName, $caAttrsAccum)
      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)(("duplicate attribute: " + $caName), $core$Core$Nil))
      : (($core$Result$onOk)((($caExpr) => {
        return ($core$Result$Ok)(((($2) => {
          return ($core$Dict$insert)($caName, $caExpr, $2);
        }))($caAttrsAccum));
      })))(((($1) => {
        return ($sd1$Compiler$MakeCanonical$translateExpression)($env, $1);
      }))(((($1) => {
        return ($core$Maybe$withDefault)($attr.name, $1);
      }))($attr.maybeExpr))));
  })))(($sd1$Compiler$MakeCanonical$translateAttributeName)($env.ro, $attr.name));
});

const $sd1$Compiler$MakeCanonical$translateRecord = (($env, $pos, $maybeMaybeExtension, $attrs) => {
  const $zzz = (((($maybeMaybeExtension)[0] === "Just") && ((($maybeMaybeExtension)[1])[0] === "Just"))
    ? ((() => {
      const $ext = (($maybeMaybeExtension)[1])[1];
      return ((($1) => {
        return ($core$Result$map)($core$Maybe$Just, $1);
      }))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $ext));
    }))()
    : (((($maybeMaybeExtension)[0] === "Just") && ((($maybeMaybeExtension)[1])[0] === "Nothing"))
      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("I need to know what record you are updating", $core$Core$Nil))
      : ((($maybeMaybeExtension)[0] === "Nothing")
        ? ($core$Result$Ok)($core$Maybe$Nothing)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1065:8', (sp_toHuman)($maybeMaybeExtension)))));
  return (($core$Result$onOk)((($maybeCaExt) => {
    return ((($maybeCaExt)[0] === "Nothing")
      ? (($core$Result$onOk)((($caAttrs) => {
        return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Record)($pos, $core$Maybe$Nothing, $caAttrs));
      })))(((($0) => {
        return ($core$List$forRes)($0, $attrs, (($1, $2) => {
          return ($sd1$Compiler$MakeCanonical$translateAndInsertRecordAttribute)(((() => {
            const $0 = $env;
            return (Object.assign)({}, $0, ({
              maybeShorthandTarget: $core$Maybe$Nothing,
            }));
          }))(), $1, $2);
        }));
      }))($core$Dict$empty))
      : ((($maybeCaExt)[0] === "Just")
        ? ((() => {
          const $caExt = ($maybeCaExt)[1];
          const $varName = (text_fromNumber)($env.nextGeneratedVariableName);
          const $var = ($sd1$Types$CanonicalAst$Variable)($sd1$Types$Pos$G, ($sd1$Types$Ast$RefLocal)($varName));
          const $newEnv = ((() => {
            const $0 = $env;
            return (Object.assign)({}, $0, ({
              maybeShorthandTarget: ($core$Maybe$Just)($var),
              nextGeneratedVariableName: ($0.nextGeneratedVariableName + 1),
            }));
          }))();
          return (($core$Result$onOk)((($caAttrs) => {
            const $def = ({
              body: $caExt,
              directConsDeps: $core$Dict$empty,
              directTypeDeps: $core$Dict$empty,
              directValueDeps: $core$Dict$empty,
              native: false,
              pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Types$Pos$G, ({
                maybeAnnotation: $core$Maybe$Nothing,
                maybeName: ($core$Maybe$Just)($varName),
              })),
              tyvars: $core$Dict$empty,
              uni: $sd1$Types$Ast$Imm,
              univars: $core$Dict$empty,
            });
            return ($core$Result$Ok)(((($1) => {
              return ($sd1$Types$CanonicalAst$LetIn)($def, $1);
            }))(((($2) => {
              return ($sd1$Types$CanonicalAst$Record)($pos, ($core$Maybe$Just)($var), $2);
            }))($caAttrs)));
          })))(((($0) => {
            return ($core$List$forRes)($0, $attrs, (($1, $2) => {
              return ($sd1$Compiler$MakeCanonical$translateAndInsertRecordAttribute)($newEnv, $1, $2);
            }));
          }))($core$Dict$empty));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1073:4', (sp_toHuman)($maybeCaExt))));
  })))($zzz);
});

const $sd1$Compiler$CoreTypes$noneValue = ($sd1$Compiler$CoreTypes$makeUsr)($sd1$Compiler$CoreTypes$noneName);

const $sd1$Types$CanonicalAst$patternNames = (($p) => {
  return (((($p)[0] === "PatternAny") && ((($p)[2].maybeName)[0] === "Nothing"))
    ? ((() => {
      const $pos = ($p)[1];
      return $core$Dict$empty;
    }))()
    : (((($p)[0] === "PatternAny") && ((($p)[2].maybeName)[0] === "Just"))
      ? ((() => {
        const $pos = ($p)[1];
        const $maybeAnnotation = ($p)[2].maybeAnnotation;
        const $n = (($p)[2].maybeName)[1];
        return ($core$Dict$ofOne)($n, ({
          maybeAnnotation: $maybeAnnotation,
          pos: $pos,
        }));
      }))()
      : ((($p)[0] === "PatternLiteralNumber")
        ? ((() => {
          const $pos = ($p)[1];
          return $core$Dict$empty;
        }))()
        : ((($p)[0] === "PatternLiteralText")
          ? ((() => {
            const $pos = ($p)[1];
            return $core$Dict$empty;
          }))()
          : ((($p)[0] === "PatternConstructor")
            ? ((() => {
              const $pos = ($p)[1];
              const $path = ($p)[2];
              const $ps = ($p)[3];
              return ($core$List$for)($core$Dict$empty, $ps, (($x, $a) => {
                return ((($1) => {
                  return ($core$Dict$join)($a, $1);
                }))(($sd1$Types$CanonicalAst$patternNames)($x));
              }));
            }))()
            : ((($p)[0] === "PatternRecord")
              ? ((() => {
                const $pos = ($p)[1];
                const $ps = ($p)[3];
                return ($core$Dict$for)($core$Dict$empty, $ps, (($k, $v, $a) => {
                  return ((($1) => {
                    return ($core$Dict$join)($a, $1);
                  }))(($sd1$Types$CanonicalAst$patternNames)($v));
                }));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 216:4', (sp_toHuman)($p))))))));
});

const $sd1$Compiler$MakeCanonical$translateStatements = (($env, $stats) => {
  return ((($stats)[0] === "Nil")
    ? ($core$Result$Ok)(((($1) => {
      return ($sd1$Types$CanonicalAst$Constructor)($sd1$Types$Pos$G, $1);
    }))($sd1$Compiler$CoreTypes$noneValue))
    : (((($stats)[0] === "Cons") && (((($stats)[1])[0] === "Evaluation") && ((($stats)[2])[0] === "Nil")))
      ? ((() => {
        const $faExpression = (($stats)[1])[1];
        return ($sd1$Compiler$MakeCanonical$translateExpression)($env, $faExpression);
      }))()
      : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "Evaluation"))
        ? ((() => {
          const $faExpr = (($stats)[1])[1];
          const $tail = ($stats)[2];
          return (($core$Result$onOk)((($caExpr) => {
            const $caDef = ({
              body: $caExpr,
              directConsDeps: $core$Dict$empty,
              directTypeDeps: $core$Dict$empty,
              directValueDeps: $core$Dict$empty,
              native: false,
              pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Types$Pos$G, ({
                maybeAnnotation: $core$Maybe$Nothing,
                maybeName: $core$Maybe$Nothing,
              })),
              tyvars: $core$Dict$empty,
              uni: $sd1$Types$Ast$Imm,
              univars: $core$Dict$empty,
            });
            return (($core$Result$onOk)((($acc) => {
              return ($core$Result$Ok)(($sd1$Types$CanonicalAst$LetIn)($caDef, $acc));
            })))(((($1) => {
              return ($sd1$Compiler$MakeCanonical$translateStatements)($env, $1);
            }))($tail));
          })))(((($1) => {
            return ($sd1$Compiler$MakeCanonical$translateExpression)($env, $1);
          }))($faExpr));
        }))()
        : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "ValueDef"))
          ? ((() => {
            const $fa = (($stats)[1])[1];
            const $tail = ($stats)[2];
            return (($core$Result$onOk)((($caDef) => {
              const $newEnv = ((() => {
                const $0 = $env;
                return (Object.assign)({}, $0, ({
                  nonRootValues: ($core$Dict$join)(($sd1$Types$CanonicalAst$patternNames)($caDef.pattern), $0.nonRootValues),
                }));
              }))();
              return (($core$Result$onOk)((($acc) => {
                return ($core$Result$Ok)(($sd1$Types$CanonicalAst$LetIn)($caDef, $acc));
              })))(((($1) => {
                return ($sd1$Compiler$MakeCanonical$translateStatements)($newEnv, $1);
              }))($tail));
            })))(((($2) => {
              return ($sd1$Compiler$MakeCanonical$translateDefinition)(false, $env, $2);
            }))($fa));
          }))()
          : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "AliasDef"))
            ? ((() => {
              const $fa = (($stats)[1])[1];
              const $tail = ($stats)[2];
              const $3 = $fa.name;
              const $pos = ($3)[1];
              return ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Aliases can be declared only in the root scope", $core$Core$Nil));
            }))()
            : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "UnionDef"))
              ? ((() => {
                const $fa = (($stats)[1])[1];
                const $tail = ($stats)[2];
                const $3 = $fa.name;
                const $pos = ($3)[1];
                return ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Types can be declared only in the root scope", $core$Core$Nil));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 699:4', (sp_toHuman)($stats))))))));
});

const $sd1$Compiler$MakeCanonical$resolveToValueRef = (($ro, $pos, $declaredInsideFunction, $maybeModule, $name) => {
  const $6 = ($sd1$Compiler$MakeCanonical$maybeForeignUsr)((($m) => {
    return $m.globalValues;
  }), $ro, $pos, $maybeModule, $name);
  return ((($6)[0] === "Err")
    ? ((() => {
      const $e = ($6)[1];
      return ($core$Result$Err)($e);
    }))()
    : (((($6)[0] === "Ok") && ((($6)[1])[0] === "Just"))
      ? ((() => {
        const $usr = (($6)[1])[1];
        return ($core$Result$Ok)(($sd1$Types$Ast$RefGlobal)($usr));
      }))()
      : (((($6)[0] === "Ok") && ((($6)[1])[0] === "Nothing"))
        ? ($declaredInsideFunction
          ? ($core$Result$Ok)(($sd1$Types$Ast$RefLocal)($name))
          : ($core$Result$Ok)(($sd1$Types$Ast$RefGlobal)(($sd1$Types$Meta$USR)($ro.umr, $name))))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 112:4', (sp_toHuman)($6)))));
});

const $sd1$Compiler$MakeCanonical$translateVariable = (($env, $pos, $maybeType, $word) => {
  const $5 = $word.modifier;
  return ((($5)[0] === "NameStartsWithDot")
    ? (($word.isUpper || (sp_not_equal)($word.maybeModule, $core$Maybe$Nothing))
      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("record attribute names must start with a lowercase letter", $core$Core$Nil))
      : ((() => {
        const $6 = $env.maybeShorthandTarget;
        return ((($6)[0] === "Nothing")
          ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("Record update shorthands must be used inside a record update such as", ($core$Core$Cons)(("    { aRecord with anAttribute = doSomethingWith ." + (($core$Text$join)(".", $word.attrPath) + " }")), ($core$Core$Cons)("but we are not inside a record update!", $core$Core$Nil))))
          : ((($6)[0] === "Just")
            ? ((() => {
              const $shorthandTarget = ($6)[1];
              return ($core$Result$Ok)(((($0) => {
                return ($core$List$for)($0, (sp_cons)($word.name, $word.attrPath), (($attrName, $expr) => {
                  return ($sd1$Types$CanonicalAst$RecordAccess)($pos, $attrName, $expr);
                }));
              }))($shorthandTarget));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 972:16', (sp_toHuman)($6))));
      }))())
    : ((($5)[0] === "NameNoModifier")
      ? ($word.isUpper
        ? ((sp_not_equal)($word.attrPath, $core$Core$Nil)
          ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("something's wrong with the lexer?", $core$Core$Nil))
          : (($core$Result$onOk)((($usr) => {
            return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Constructor)($pos, $usr));
          })))(($sd1$Compiler$MakeCanonical$resolveToConstructorUsr)($env.ro, $pos, $word.maybeModule, $word.name)))
        : ((() => {
          const $declaredInsideFunction = ($core$Dict$member)($word.name, $env.nonRootValues);
          return (($core$Result$onOk)((($usr) => {
            return ($core$Result$Ok)(((($0) => {
              return ($core$List$for)($0, $word.attrPath, (($1, $2) => {
                return ($sd1$Types$CanonicalAst$RecordAccess)($pos, $1, $2);
              }));
            }))(($sd1$Types$CanonicalAst$Variable)($pos, $usr)));
          })))(($sd1$Compiler$MakeCanonical$resolveToValueRef)($env.ro, $pos, $declaredInsideFunction, $word.maybeModule, $word.name));
        }))())
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 967:4', (sp_toHuman)($5))));
});

const $sd1$Prelude$unaryMinus = ({
  symbol: "0 -",
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)($sd1$Compiler$CoreTypes$number, $core$Core$Nil), $sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$numberUsr)("unaryMinus"),
});

const $sd1$Compiler$MakeCanonical$translateExpression = (($env, $1) => {
  const $pos = ($1)[1];
  const $expr_ = ($1)[2];
  return ((($expr_)[0] === "LiteralNumber")
    ? ((() => {
      const $isPercent = ($expr_)[1];
      const $str = ($expr_)[2];
      return ($sd1$Compiler$MakeCanonical$translateNumber)($env.ro, $isPercent, $sd1$Types$CanonicalAst$LiteralNumber, $pos, $str);
    }))()
    : ((($expr_)[0] === "LiteralText")
      ? ((() => {
        const $v = ($expr_)[1];
        return ($core$Result$Ok)(($sd1$Types$CanonicalAst$LiteralText)($pos, $v));
      }))()
      : ((($expr_)[0] === "Statements")
        ? ((() => {
          const $stats = ($expr_)[1];
          return ($sd1$Compiler$MakeCanonical$translateStatements)($env, $stats);
        }))()
        : ((($expr_)[0] === "Variable")
          ? ((() => {
            const $maybeType = ($expr_)[1].maybeType;
            const $word = ($expr_)[1].word;
            return ($sd1$Compiler$MakeCanonical$translateVariable)($env, $pos, $maybeType, $word);
          }))()
          : ((($expr_)[0] === "Fn")
            ? ((() => {
              const $faParams = ($expr_)[1];
              const $faBody = ($expr_)[2];
              return (($core$Result$onOk)((($caParams) => {
                const $zzz = ((($0) => {
                  return ($core$List$forRes)($0, $caParams, (($par, $envX) => {
                    const $names = ((($par)[0] === "ParameterPattern")
                      ? ((() => {
                        const $uni = ($par)[1];
                        const $pa = ($par)[2];
                        return ($sd1$Types$CanonicalAst$patternNames)($pa);
                      }))()
                      : ((($par)[0] === "ParameterRecycle")
                        ? ((() => {
                          const $name = ($par)[2];
                          return ($core$Dict$ofOne)($name, ({
                            maybeAnnotation: $core$Maybe$Nothing,
                            pos: $pos,
                          }));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 786:24', (sp_toHuman)($par))));
                    const $duplicates = ($core$Dict$keys)(($core$Dict$intersect)($names, $envX.nonRootValues));
                    return ((sp_not_equal)($duplicates, $core$Core$Nil)
                      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)(("parameters shadows these values: " + ($core$Text$join)(",", $duplicates)), $core$Core$Nil))
                      : ($core$Result$Ok)(((() => {
                        const $0 = $envX;
                        return (Object.assign)({}, $0, ({
                          nonRootValues: ($core$Dict$join)($names, $0.nonRootValues),
                        }));
                      }))()));
                  }));
                }))($env);
                return (($core$Result$onOk)((($localEnv) => {
                  return (($core$Result$onOk)((($caBody) => {
                    return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Fn)($pos, $caParams, $caBody));
                  })))(((($1) => {
                    return ($sd1$Compiler$MakeCanonical$translateExpression)($localEnv, $1);
                  }))($faBody));
                })))($zzz);
              })))(((($1) => {
                return ($core$List$mapRes)((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateParameter)($env, $1);
                }), $1);
              }))($faParams));
            }))()
            : ((($expr_)[0] === "Call")
              ? ((() => {
                const $faRef = ($expr_)[1];
                const $faArgs = ($expr_)[2];
                return (($core$Result$onOk)((($caRef) => {
                  return (($core$Result$onOk)((($4) => {
                    const $caArgs = $4.first;
                    const $wrap = $4.second;
                    return ($core$Result$Ok)(($wrap)(($sd1$Types$CanonicalAst$Call)($pos, $caRef, $caArgs)));
                  })))(((($2) => {
                    return ($sd1$Compiler$MakeCanonical$translateArgumentsAndPlaceholders)($pos, $env, $2);
                  }))($faArgs));
                })))(((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateExpression)($env, $1);
                }))($faRef));
              }))()
              : ((($expr_)[0] === "If")
                ? ((() => {
                  const $condition = ($expr_)[1].condition;
                  const $false = ($expr_)[1].false;
                  const $true = ($expr_)[1].true;
                  return (($core$Result$onOk)((($c) => {
                    return (($core$Result$onOk)((($t) => {
                      return (($core$Result$onOk)((($f) => {
                        return ($core$Result$Ok)(((($1) => {
                          return ($sd1$Types$CanonicalAst$If)($pos, $1);
                        }))(({
                          condition: $c,
                          false: $f,
                          true: $t,
                        })));
                      })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $false));
                    })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $true));
                  })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $condition));
                }))()
                : ((($expr_)[0] === "Unop")
                  ? ((() => {
                    const $opId = ($expr_)[1];
                    const $faOperand = ($expr_)[2];
                    return ((($opId)[0] === "UnopUnique")
                      ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("can't use ! here because REASONS", $core$Core$Nil))
                      : ((($opId)[0] === "UnopRecycle")
                        ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("can recycle only in function calls!", $core$Core$Nil))
                        : ((($opId)[0] === "UnopPlus")
                          ? ($sd1$Compiler$MakeCanonical$translateExpression)($env, $faOperand)
                          : ((($opId)[0] === "UnopMinus")
                            ? (($core$Result$onOk)((($caOperand) => {
                              return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Call)($pos, ($sd1$Types$CanonicalAst$Variable)($pos, ($sd1$Types$Ast$RefGlobal)($sd1$Prelude$unaryMinus.usr)), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)($caOperand), $core$Core$Nil)));
                            })))(((($1) => {
                              return ($sd1$Compiler$MakeCanonical$translateExpression)($env, $1);
                            }))($faOperand))
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 834:12', (sp_toHuman)($opId))))));
                  }))()
                  : ((($expr_)[0] === "Binop")
                    ? ((() => {
                      const $group = ($expr_)[1];
                      const $sepList = ($expr_)[2];
                      return ($sd1$Compiler$MakeCanonical$translateBinops)($env, $pos, $group, $sepList);
                    }))()
                    : ((($expr_)[0] === "Record")
                      ? ((() => {
                        const $attrs = ($expr_)[1].attrs;
                        const $maybeExtension = ($expr_)[1].maybeExtension;
                        return ($sd1$Compiler$MakeCanonical$translateRecord)($env, $pos, $maybeExtension, $attrs);
                      }))()
                      : ((($expr_)[0] === "List")
                        ? ((() => {
                          const $faDotsAndItems = ($expr_)[1];
                          const $rev = ($core$List$reverse)($faDotsAndItems);
                          return ((($rev)[0] === "Nil")
                            ? ($core$Result$Ok)(($sd1$Types$CanonicalAst$Constructor)($pos, $sd1$Compiler$CoreTypes$nil))
                            : ((($rev)[0] === "Cons")
                              ? ((() => {
                                const $hasDots = ($rev)[1].first;
                                const $head = ($rev)[1].second;
                                const $rest = ($rev)[2];
                                return (($core$List$any)($core$Tuple$first, $rest)
                                  ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("can use dots only on the last element (for now?)", $core$Core$Nil))
                                  : ((() => {
                                    const $3 = ($hasDots
                                      ? ({
                                        first: $head,
                                        second: $rest,
                                      })
                                      : ({
                                        first: ($sd1$Types$FormattableAst$Expression)($pos, ($sd1$Types$FormattableAst$List)($core$Core$Nil)),
                                        second: $rev,
                                      }));
                                    const $revItems = $3.second;
                                    const $init = $3.first;
                                    return (($core$Result$onOk)((($caInit) => {
                                      return ((($0) => {
                                        return ($core$List$forRes)($0, $revItems, (($7, $acc) => {
                                          const $faItem = $7.second;
                                          return (($core$Result$onOk)((($caItem) => {
                                            return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Call)($pos, ($sd1$Types$CanonicalAst$Constructor)($pos, $sd1$Compiler$CoreTypes$cons), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)($caItem), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)($acc), $core$Core$Nil))));
                                          })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $faItem));
                                        }));
                                      }))($caInit);
                                    })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $init));
                                  }))());
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 857:12', (sp_toHuman)($rev))));
                        }))()
                        : ((($expr_)[0] === "Try")
                          ? ((() => {
                            const $patterns = ($expr_)[1].patterns;
                            const $value = ($expr_)[1].value;
                            const $translatePatternAndStatements = (($3) => {
                              const $faPattern = $3.first;
                              const $faExpression = $3.second;
                              return (($core$Result$onOk)((($4) => {
                                const $uni = $4.first;
                                const $caPattern = $4.second;
                                return (($core$Result$onOk)((($block) => {
                                  return ($core$Result$Ok)(({
                                    first: $uni,
                                    second: $caPattern,
                                    third: $block,
                                  }));
                                })))(((($1) => {
                                  return ($sd1$Compiler$MakeCanonical$translateExpression)(((() => {
                                    const $0 = $env;
                                    return (Object.assign)({}, $0, ({
                                      nonRootValues: ($core$Dict$join)(($sd1$Types$CanonicalAst$patternNames)($caPattern), $env.nonRootValues),
                                    }));
                                  }))(), $1);
                                }))($faExpression));
                              })))(((($1) => {
                                return ($sd1$Compiler$MakeCanonical$translateFullPattern)($env, $1);
                              }))($faPattern));
                            });
                            return (($core$Result$onOk)((($caValue) => {
                              return (($core$Result$onOk)((($patternsAndExpressions) => {
                                return ($core$Result$Ok)(($sd1$Types$CanonicalAst$Try)($pos, ({
                                  patternsAndExpressions: $patternsAndExpressions,
                                  value: $caValue,
                                })));
                              })))(((($1) => {
                                return ($core$List$mapRes)($translatePatternAndStatements, $1);
                              }))($patterns));
                            })))(($sd1$Compiler$MakeCanonical$translateExpression)($env, $value));
                          }))()
                          : (true
                            ? ($sd1$Compiler$MakeCanonical$error)($env, $pos, ($core$Core$Cons)("something's wrong here...", ($core$Core$Cons)((sp_toHuman)($expr_), $core$Core$Nil)))
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 765:4', (sp_toHuman)($expr_)))))))))))))));
});

const $sd1$Compiler$MakeCanonical$translateTypeParameter = (($ro, $1) => {
  const $pos = ($1)[1];
  const $word = ($1)[2];
  return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
    ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Can't start with .", $core$Core$Nil))
    : ($word.isUpper
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("type params must start with a lowercase letter", $core$Core$Nil))
      : ((sp_not_equal)($word.maybeModule, $core$Maybe$Nothing)
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("why modules here?", $core$Core$Nil))
        : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
          ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("why attrs here?", $core$Core$Nil))
          : ($core$Result$Ok)(($sd1$Types$Pos$At)($pos, $word.name))))));
});

const $sd1$Types$CanonicalAst$parTypeToRaw = (($p) => {
  return ((($p)[0] === "ParRe")
    ? ((() => {
      const $raw = ($p)[1];
      return $raw;
    }))()
    : ((($p)[0] === "ParSp")
      ? ((() => {
        const $full = ($p)[1];
        return $full.raw;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 173:4', (sp_toHuman)($p))));
});

const $sd1$Types$CanonicalAst$typeTyvars = (($raw) => {
  const $fromList = (($list) => {
    return ($core$List$for)($core$Dict$empty, $list, (($item, $acc) => {
      return ($core$Dict$join)($acc, ($sd1$Types$CanonicalAst$typeTyvars)($item));
    }));
  });
  return ((($raw)[0] === "TypeNamed")
    ? ((() => {
      const $args = ($raw)[3];
      return ($fromList)($args);
    }))()
    : ((($raw)[0] === "TypeFn")
      ? ((() => {
        const $pars = ($raw)[2];
        const $to = ($raw)[3];
        return ($fromList)((sp_cons)($to.raw, ($core$List$map)($sd1$Types$CanonicalAst$parTypeToRaw, $pars)));
      }))()
      : ((($raw)[0] === "TypeRecord")
        ? ((() => {
          const $attrs = ($raw)[2];
          return ($fromList)(($core$Dict$values)($attrs));
        }))()
        : ((($raw)[0] === "TypeAnnotationVariable")
          ? ((() => {
            const $pos = ($raw)[1];
            const $name = ($raw)[2];
            return ($core$Dict$ofOne)($name, $pos);
          }))()
          : ((($raw)[0] === "TypeError")
            ? $core$Dict$empty
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 185:4', (sp_toHuman)($raw)))))));
});

const $sd1$Types$CanonicalAst$patternTyvars = (($pa) => {
  return (((($pa)[0] === "PatternAny") && ((($pa)[2].maybeAnnotation)[0] === "Just"))
    ? ((() => {
      const $t = (($pa)[2].maybeAnnotation)[1];
      return ($sd1$Types$CanonicalAst$typeTyvars)($t);
    }))()
    : (((($pa)[0] === "PatternAny") && ((($pa)[2].maybeAnnotation)[0] === "Nothing"))
      ? $core$Dict$empty
      : ((($pa)[0] === "PatternLiteralText")
        ? $core$Dict$empty
        : ((($pa)[0] === "PatternLiteralNumber")
          ? $core$Dict$empty
          : ((($pa)[0] === "PatternConstructor")
            ? ((() => {
              const $args = ($pa)[3];
              return ($core$List$for)($core$Dict$empty, $args, (($arg, $acc) => {
                return ($core$Dict$join)($acc, ($sd1$Types$CanonicalAst$patternTyvars)($arg));
              }));
            }))()
            : ((($pa)[0] === "PatternRecord")
              ? ((() => {
                const $attrs = ($pa)[3];
                return ($core$Dict$for)($core$Dict$empty, $attrs, (($k, $arg, $acc) => {
                  return ($core$Dict$join)($acc, ($sd1$Types$CanonicalAst$patternTyvars)($arg));
                }));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 205:4', (sp_toHuman)($pa))))))));
});

const $sd1$Compiler$MakeCanonical$translateDefinition = (($isRoot, $env, $fa) => {
  return (($core$Result$onOk)((($4) => {
    const $uni = $4.first;
    const $pattern = $4.second;
    return (($core$Result$onOk)((($nonFn) => {
      const $univars = ((($1) => {
        return ($sd1$Compiler$MakeCanonical$addPatternUnivars)($pattern, $1);
      }))(((($1) => {
        return ($sd1$Compiler$MakeCanonical$addUnivarId)($uni, $1);
      }))($core$Dict$empty));
      const $tyvars = ((($1) => {
        return ($core$Dict$map)((($tyvarName, $pos) => {
          return ({
            allowFunctions: ($core$List$all)((($9) => {
              const $name = ($9)[2];
              return (sp_not_equal)($name, $tyvarName);
            }), $nonFn),
          });
        }), $1);
      }))(($sd1$Types$CanonicalAst$patternTyvars)($pattern));
      const $nonRootValues1 = ($isRoot
        ? $env.nonRootValues
        : ($core$Dict$join)(($sd1$Types$CanonicalAst$patternNames)($pattern), $env.nonRootValues));
      const $localEnv0 = ((() => {
        const $0 = $env;
        return (Object.assign)({}, $0, ({
          nonRootValues: $nonRootValues1,
        }));
      }))();
      return (($core$Result$onOk)((($body) => {
        const $deps = ($isRoot
          ? ((($1) => {
            return ($sd1$Compiler$MakeCanonical$expressionDeps)($body, $1);
          }))(((($1) => {
            return ($sd1$Compiler$MakeCanonical$patternDeps)($pattern, $1);
          }))($sd1$Compiler$MakeCanonical$deps_init))
          : $sd1$Compiler$MakeCanonical$deps_init);
        return ($core$Result$Ok)(({
          body: $body,
          directConsDeps: $deps.cons,
          directTypeDeps: $deps.types,
          directValueDeps: $deps.values,
          native: false,
          pattern: $pattern,
          tyvars: $tyvars,
          uni: $uni,
          univars: $univars,
        }));
      })))(((($1) => {
        return ($sd1$Compiler$MakeCanonical$translateExpression)($localEnv0, $1);
      }))($fa.body));
    })))(((($1) => {
      return ($core$List$mapRes)((($1) => {
        return ($sd1$Compiler$MakeCanonical$translateTypeParameter)($env.ro, $1);
      }), $1);
    }))($fa.nonFn));
  })))(((($1) => {
    return ($sd1$Compiler$MakeCanonical$translateFullPattern)($env, $1);
  }))($fa.pattern));
});

const $sd1$Compiler$MakeCanonical$translateTypeName = (($ro, $1) => {
  const $pos = ($1)[1];
  const $word = ($1)[2];
  return ((sp_not_equal)($word.modifier, $sd1$Types$Token$NameNoModifier)
    ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Can't start with .", $core$Core$Nil))
    : (($core$Basics$not)($word.isUpper)
      ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("type names must start with Uppercase letter", $core$Core$Nil))
      : ((sp_not_equal)($word.maybeModule, $core$Maybe$Nothing)
        ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("why modules here?", $core$Core$Nil))
        : ((sp_not_equal)($word.attrPath, $core$Core$Nil)
          ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("why attrs here?", $core$Core$Nil))
          : ($core$Result$Ok)($word.name)))));
});

const $sd1$Compiler$MakeCanonical$insertRootStatement = (($ro, $faStatement, $caModule) => {
  return (((($faStatement)[0] === "Evaluation") && ((($faStatement)[1])[0] === "Expression"))
    ? ((() => {
      const $pos = (($faStatement)[1])[1];
      return ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)("Root Evaluations don't really do much =|", $core$Core$Nil));
    }))()
    : ((($faStatement)[0] === "ValueDef")
      ? ((() => {
        const $d = ($faStatement)[1];
        return (($core$Result$onOk)((($def) => {
          return ((sp_not_equal)($def.uni, $sd1$Types$Ast$Imm)
            ? ($sd1$Compiler$MakeCanonical$erroro)($ro, ($sd1$Types$CanonicalAst$patternPos)($def.pattern), ($core$Core$Cons)("Unique values can be declared only inside functions.", $core$Core$Nil))
            : ($core$Result$Ok)(((() => {
              const $0 = $caModule;
              return (Object.assign)({}, $0, ({
                valueDefs: ($core$Dict$insert)($def.pattern, $def, $0.valueDefs),
              }));
            }))()));
        })))(((($2) => {
          return ($sd1$Compiler$MakeCanonical$translateDefinition)(true, ($sd1$Compiler$MakeCanonical$initEnv)($ro), $2);
        }))($d));
      }))()
      : ((($faStatement)[0] === "AliasDef")
        ? ((() => {
          const $fa = ($faStatement)[1];
          return (($core$Result$onOk)((($name) => {
            return ((($core$Dict$member)($name, $caModule.aliasDefs) || ($core$Dict$member)($name, $caModule.unionDefs))
              ? ((() => {
                const $5 = $fa.name;
                const $pos = ($5)[1];
                return ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(($name + " declared twice!"), $core$Core$Nil));
              }))()
              : (($core$Result$onOk)((($caPars) => {
                return (($core$Result$onOk)((($type) => {
                  const $aliasDef = ({
                    directTypeDeps: ($sd1$Compiler$MakeCanonical$typeDeps)($type, $core$Set$empty),
                    pars: $caPars,
                    type: $type,
                    usr: ($sd1$Types$Meta$USR)($ro.umr, $name),
                  });
                  return ($core$Result$Ok)(((() => {
                    const $0 = $caModule;
                    return (Object.assign)({}, $0, ({
                      aliasDefs: ($core$Dict$insert)($name, $aliasDef, $0.aliasDefs),
                    }));
                  }))());
                })))(((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateRawType)($ro, $1);
                }))($fa.type));
              })))(((($1) => {
                return ($core$List$mapRes)((($1) => {
                  return ($sd1$Compiler$MakeCanonical$translateTypeParameter)($ro, $1);
                }), $1);
              }))($fa.args)));
          })))(($sd1$Compiler$MakeCanonical$translateTypeName)($ro, $fa.name));
        }))()
        : ((($faStatement)[0] === "UnionDef")
          ? ((() => {
            const $fa = ($faStatement)[1];
            const $4 = $fa.name;
            const $pos = ($4)[1];
            return (($core$Result$onOk)((($name) => {
              return ((($core$Dict$member)($name, $caModule.aliasDefs) || ($core$Dict$member)($name, $caModule.unionDefs))
                ? ($sd1$Compiler$MakeCanonical$erroro)($ro, $pos, ($core$Core$Cons)(($name + " declared twice!"), $core$Core$Nil))
                : (($core$Result$onOk)((($caPars) => {
                  const $usr = ($sd1$Types$Meta$USR)($ro.umr, $name);
                  const $type = ((($2) => {
                    return ($sd1$Types$CanonicalAst$TypeNamed)($pos, $usr, $2);
                  }))(((($1) => {
                    return ($core$List$map)((($8) => {
                      const $p = ($8)[1];
                      const $n = ($8)[2];
                      return ($sd1$Types$CanonicalAst$TypeAnnotationVariable)($p, $n);
                    }), $1);
                  }))($caPars));
                  return (($core$Result$onOk)((($constructors) => {
                    const $unionDef = ({
                      constructors: $constructors,
                      directTypeDeps: ($core$Dict$for)($core$Set$empty, $constructors, (($k, $c, $z) => {
                        return ($core$List$for)($z, $c.ins, $sd1$Compiler$MakeCanonical$typeDeps);
                      })),
                      pars: $caPars,
                      usr: $usr,
                    });
                    return ($core$Result$Ok)(((() => {
                      const $0 = $caModule;
                      return (Object.assign)({}, $0, ({
                        unionDefs: ($core$Dict$insert)($name, $unionDef, $0.unionDefs),
                      }));
                    }))());
                  })))(((($0) => {
                    return ($core$List$forRes)($0, $fa.constructors, (($3, $4) => {
                      return ($sd1$Compiler$MakeCanonical$translateConstructor)($ro, $type, $usr, $3, $4);
                    }));
                  }))($core$Dict$empty));
                })))(((($1) => {
                  return ($core$List$mapRes)((($1) => {
                    return ($sd1$Compiler$MakeCanonical$translateTypeParameter)($ro, $1);
                  }), $1);
                }))($fa.args)));
            })))(($sd1$Compiler$MakeCanonical$translateTypeName)($ro, $fa.name));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1647:4', (sp_toHuman)($faStatement))))));
});

const $sd1$Types$CanonicalAst$initModule = (($fsPath, $umr, $asText) => {
  return ({
    aliasDefs: $core$Dict$empty,
    asText: $asText,
    fsPath: $fsPath,
    umr: $umr,
    unionDefs: $core$Dict$empty,
    valueDefs: $core$Dict$empty,
  });
});

const $sd1$Compiler$MakeCanonical$translateModule = (($ro, $faModule) => {
  (sp_benchStart)(null);
  const $module = ($sd1$Types$CanonicalAst$initModule)($ro.errorModule.fsPath, $ro.umr, $ro.errorModule.content);
  return ((($2) => {
    return ($core$Basics$btw)(sp_benchStop, "translateModule", $2);
  }))(((($0) => {
    return ($core$List$forRes)($0, $faModule, (($1, $2) => {
      return ($sd1$Compiler$MakeCanonical$insertRootStatement)($ro, $1, $2);
    }));
  }))($module));
});

const $sd1$Compiler$MakeCanonical$textToCanonicalModule = (($stripLocations, $ro) => {
  return (($core$Result$onOk)((($faModule) => {
    return ($sd1$Compiler$MakeCanonical$translateModule)($ro, $faModule);
  })))(($sd1$Compiler$Parser$textToFormattableModule)(({
    errorModule: $ro.errorModule,
    stripLocations: $stripLocations,
  })));
});

const $sd1$Compiler$MakeEmittable$circularIsError = (($globalDefsByName, $usrs) => {
  const $zzz = (($usr) => {
    const $4 = ($core$Dict$get)($usr, $globalDefsByName);
    return ((($4)[0] === "Nothing")
      ? false
      : ((($4)[0] === "Just")
        ? ((() => {
          const $globalDef = ($4)[1];
          const $5 = $globalDef.expr;
          return ((($5)[0] === "Fn")
            ? false
            : (true
              ? true
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 364:14', (sp_toHuman)($5))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 358:6', (sp_toHuman)($4))));
  });
  return ($core$List$any)($zzz, $usrs);
});

const $sd1$Compiler$MakeEmittable$generateName = (($env) => {
  return ({
    first: (text_fromNumber)(($env.genVarCounter + 1)),
    second: ((() => {
      const $0 = $env;
      return (Object.assign)({}, $0, ({
        genVarCounter: (1 + $0.genVarCounter),
      }));
    }))(),
  });
});

const $sd1$Compiler$MakeEmittable$pickMainName = (($pattern) => {
  return (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeName)[0] === "Just"))
    ? ((() => {
      const $pos = ($pattern)[1];
      const $name = (($pattern)[2].maybeName)[1];
      const $type = ($pattern)[2].type;
      return ($sd1$Compiler$MakeEmittable$TrivialPattern)($name, $type);
    }))()
    : (true
      ? ((sp_not_equal)(($sd1$Types$TypedAst$patternNames)($pattern), $core$Dict$empty)
        ? $sd1$Compiler$MakeEmittable$GenerateName
        : $sd1$Compiler$MakeEmittable$NoNamedVariables)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 27:4', (sp_toHuman)($pattern))));
});

const $sd1$Compiler$MakeEmittable$testPattern = (($pattern, $valueToTest, $accum) => {
  return ((($pattern)[0] === "PatternAny")
    ? $accum
    : ((($pattern)[0] === "PatternLiteralText")
      ? ((() => {
        const $text = ($pattern)[2];
        return (sp_cons)(($sd1$Types$EmittableAst$ShallowEqual)(($sd1$Types$EmittableAst$LiteralText)($text), $valueToTest), $accum);
      }))()
      : ((($pattern)[0] === "PatternLiteralNumber")
        ? ((() => {
          const $num = ($pattern)[2];
          return (sp_cons)(($sd1$Types$EmittableAst$ShallowEqual)(($sd1$Types$EmittableAst$LiteralNumber)($num), $valueToTest), $accum);
        }))()
        : (((($pattern)[0] === "PatternConstructor") && ((($pattern)[2])[0] === "USR"))
          ? ((() => {
            const $umr = (($pattern)[2])[1];
            const $name = (($pattern)[2])[2];
            const $pas = ($pattern)[3];
            return ((($0) => {
              return ($core$List$indexedFor)($0, $pas, (($index, $argPattern, $a) => {
                return ($sd1$Compiler$MakeEmittable$testPattern)($argPattern, ($sd1$Types$EmittableAst$ConstructorAccess)($index, $valueToTest), $a);
              }));
            }))((sp_cons)(($sd1$Types$EmittableAst$IsConstructor)($name, $valueToTest), $accum));
          }))()
          : ((($pattern)[0] === "PatternRecord")
            ? ((() => {
              const $attrs = ($pattern)[2];
              return ((($0) => {
                return ($core$Dict$for)($0, $attrs, (($name, $6, $a) => {
                  const $pa = $6.first;
                  const $type = $6.second;
                  return ($sd1$Compiler$MakeEmittable$testPattern)($pa, ($sd1$Types$EmittableAst$RecordAccess)($name, $valueToTest), $a);
                }));
              }))($accum);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 75:4', (sp_toHuman)($pattern)))))));
});

const $sd1$Compiler$MakeEmittable$translateArgAndType = (($env, $taArg) => {
  return ((($taArg)[0] === "ArgumentExpression")
    ? ((() => {
      const $fullType = ($taArg)[1];
      const $exp = ($taArg)[2];
      return ($sd1$Types$EmittableAst$ArgumentSpend)($fullType, ($sd1$Compiler$MakeEmittable$translateExpression)($env, $exp));
    }))()
    : ((($taArg)[0] === "ArgumentRecycle")
      ? ((() => {
        const $pos = ($taArg)[1];
        const $rawType = ($taArg)[2];
        const $attrPath = ($taArg)[3];
        const $name = ($taArg)[4];
        return ($sd1$Types$EmittableAst$ArgumentRecycle)($rawType, $attrPath, $name);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 135:4', (sp_toHuman)($taArg))));
});

const $sd1$Compiler$MakeEmittable$translatePatternRec = (($pattern, $accessExpr, $accum) => {
  return (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeName)[0] === "Nothing"))
    ? ((() => {
      const $type = ($pattern)[2].type;
      return $accum;
    }))()
    : (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeName)[0] === "Just"))
      ? ((() => {
        const $name = (($pattern)[2].maybeName)[1];
        const $type = ($pattern)[2].type;
        return (sp_cons)(({
          first: $type,
          second: $name,
          third: $accessExpr,
        }), $accum);
      }))()
      : ((($pattern)[0] === "PatternLiteralNumber")
        ? $accum
        : ((($pattern)[0] === "PatternLiteralText")
          ? $accum
          : ((($pattern)[0] === "PatternConstructor")
            ? ((() => {
              const $path = ($pattern)[2];
              const $pas = ($pattern)[3];
              return ((($0) => {
                return ($core$List$indexedFor)($0, $pas, (($index, $pa, $a) => {
                  return ($sd1$Compiler$MakeEmittable$translatePatternRec)($pa, ($sd1$Types$EmittableAst$ConstructorAccess)($index, $accessExpr), $a);
                }));
              }))($accum);
            }))()
            : ((($pattern)[0] === "PatternRecord")
              ? ((() => {
                const $attrs = ($pattern)[2];
                return ((($0) => {
                  return ($core$Dict$for)($0, $attrs, (($name, $6, $a) => {
                    const $pa = $6.first;
                    const $type = $6.second;
                    return ($sd1$Compiler$MakeEmittable$translatePatternRec)($pa, ($sd1$Types$EmittableAst$RecordAccess)($name, $accessExpr), $a);
                  }));
                }))($accum);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 51:4', (sp_toHuman)($pattern))))))));
});

const $sd1$Compiler$MakeEmittable$translatePattern = (($pattern, $accessExpr) => {
  return ($sd1$Compiler$MakeEmittable$translatePatternRec)($pattern, $accessExpr, $core$Core$Nil);
});

const $sd1$Compiler$MakeEmittable$translateParameter = (($env, $bodyAcc, $param) => {
  return ((($param)[0] === "ParameterRecycle")
    ? ((() => {
      const $pos = ($param)[1];
      const $rawType = ($param)[2];
      const $name = ($param)[3];
      return ({
        first: $bodyAcc,
        second: ({
          first: true,
          second: ($core$Maybe$Just)($name),
        }),
      });
    }))()
    : ((($param)[0] === "ParameterPattern")
      ? ((() => {
        const $fullType = ($param)[1];
        const $pa = ($param)[2];
        const $4 = ($sd1$Compiler$MakeEmittable$pickMainName)($pa);
        return ((($4)[0] === "NoNamedVariables")
          ? ({
            first: $bodyAcc,
            second: ({
              first: false,
              second: $core$Maybe$Nothing,
            }),
          })
          : ((($4)[0] === "TrivialPattern")
            ? ((() => {
              const $argName = ($4)[1];
              const $type = ($4)[2];
              return ({
                first: $bodyAcc,
                second: ({
                  first: false,
                  second: ($core$Maybe$Just)($argName),
                }),
              });
            }))()
            : ((($4)[0] === "GenerateName")
              ? ((() => {
                const $5 = ($sd1$Compiler$MakeEmittable$generateName)($env);
                const $newEnv = $5.second;
                const $mainName = $5.first;
                const $namesAndExpressions = ($sd1$Compiler$MakeEmittable$translatePattern)($pa, ($sd1$Types$EmittableAst$Variable)(($sd1$Types$Ast$RefLocal)($mainName)));
                const $wrapWithArgumentLetIn = (($7, $inExpression) => {
                  const $type = $7.first;
                  const $varName = $7.second;
                  const $letExpression = $7.third;
                  return ($sd1$Types$EmittableAst$LetIn)(({
                    inExpression: $inExpression,
                    letExpression: $letExpression,
                    maybeName: ($core$Maybe$Just)($varName),
                    type: $type,
                  }));
                });
                return ({
                  first: ($core$List$for)($bodyAcc, $namesAndExpressions, $wrapWithArgumentLetIn),
                  second: ({
                    first: false,
                    second: ($core$Maybe$Just)($mainName),
                  }),
                });
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 105:12', (sp_toHuman)($4)))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 100:4', (sp_toHuman)($param))));
});

const $sd1$Compiler$MakeEmittable$translateExpression = (($env, $expression) => {
  return ((($expression)[0] === "LiteralNumber")
    ? ((() => {
      const $num = ($expression)[2];
      return ($sd1$Types$EmittableAst$LiteralNumber)($num);
    }))()
    : ((($expression)[0] === "LiteralText")
      ? ((() => {
        const $text = ($expression)[2];
        return ($sd1$Types$EmittableAst$LiteralText)($text);
      }))()
      : ((($expression)[0] === "Variable")
        ? ((() => {
          const $ref = ($expression)[2];
          return ($sd1$Types$EmittableAst$Variable)($ref);
        }))()
        : ((($expression)[0] === "Constructor")
          ? ((() => {
            const $usr = ($expression)[2];
            return ($sd1$Types$EmittableAst$Constructor)($usr);
          }))()
          : ((($expression)[0] === "RecordAccess")
            ? ((() => {
              const $attrName = ($expression)[2];
              const $exp = ($expression)[3];
              return ($sd1$Types$EmittableAst$RecordAccess)($attrName, ($sd1$Compiler$MakeEmittable$translateExpression)($env, $exp));
            }))()
            : ((($expression)[0] === "Fn")
              ? ((() => {
                const $pos = ($expression)[1];
                const $taPars = ($expression)[2];
                const $body = ($expression)[3];
                const $bodyT = ($expression)[4];
                const $eaBody = ($sd1$Compiler$MakeEmittable$translateExpression)(((() => {
                  const $0 = $env;
                  return (Object.assign)({}, $0, ({
                    genVarCounter: (($core$List$length)($taPars) + $0.genVarCounter),
                  }));
                }))(), $body);
                const $3 = ((($0) => {
                  return ($core$List$forReversed)($0, $taPars, (($taPar, $5) => {
                    const $bodyAcc = $5.first;
                    const $eaParsAcc = $5.second;
                    const $7 = ((() => {
                      const $newEnv = ((() => {
                        const $0 = $env;
                        return (Object.assign)({}, $0, ({
                          genVarCounter: (($core$List$length)($eaParsAcc) + $0.genVarCounter),
                        }));
                      }))();
                      return ($sd1$Compiler$MakeEmittable$translateParameter)($newEnv, $bodyAcc, $taPar);
                    }))();
                    const $eaPar = $7.second;
                    const $bodyX = $7.first;
                    return ({
                      first: $bodyX,
                      second: (sp_cons)($eaPar, $eaParsAcc),
                    });
                  }));
                }))(({
                  first: $eaBody,
                  second: $core$Core$Nil,
                }));
                const $eaPars = $3.second;
                const $wrappedBody = $3.first;
                return ($sd1$Types$EmittableAst$Fn)($eaPars, $wrappedBody);
              }))()
              : ((($expression)[0] === "Record")
                ? ((() => {
                  const $extends = ($expression)[2];
                  const $attrs = ($expression)[3];
                  return ((($1) => {
                    return ($sd1$Types$EmittableAst$LiteralRecord)(($core$Maybe$map)((($1) => {
                      return ($sd1$Compiler$MakeEmittable$translateExpression)($env, $1);
                    }), $extends), $1);
                  }))(((($1) => {
                    return ($core$List$map)((($1) => {
                      return ($core$Tuple$mapSecond)((($1) => {
                        return ($sd1$Compiler$MakeEmittable$translateExpression)($env, $1);
                      }), $1);
                    }), $1);
                  }))(((($1) => {
                    return (list_sortBy)($core$Tuple$first, $1);
                  }))(($core$Dict$toList)($attrs))));
                }))()
                : ((($expression)[0] === "Call")
                  ? ((() => {
                    const $ref = ($expression)[2];
                    const $argsAndTypes = ($expression)[3];
                    return ($sd1$Types$EmittableAst$Call)(($sd1$Compiler$MakeEmittable$translateExpression)($env, $ref), ($core$List$map)((($1) => {
                      return ($sd1$Compiler$MakeEmittable$translateArgAndType)($env, $1);
                    }), $argsAndTypes));
                  }))()
                  : ((($expression)[0] === "If")
                    ? ((() => {
                      const $ar = ($expression)[2];
                      return ($sd1$Types$EmittableAst$Conditional)(($sd1$Compiler$MakeEmittable$translateExpression)($env, $ar.condition), ($sd1$Compiler$MakeEmittable$translateExpression)($env, $ar.true), ($sd1$Compiler$MakeEmittable$translateExpression)($env, $ar.false));
                    }))()
                    : ((($expression)[0] === "Try")
                      ? ((() => {
                        const $pos = ($expression)[1];
                        const $patternsAndExpressions = ($expression)[2].patternsAndExpressions;
                        const $value = ($expression)[2].value;
                        const $valueType = ($expression)[2].valueType;
                        const $3 = ((() => {
                          const $4 = ({
                            first: $value,
                            second: $valueType.uni,
                          });
                          return (((($4.first)[0] === "Variable") && (($4.second)[0] === "Imm"))
                            ? ((() => {
                              const $ref = ($4.first)[2];
                              return ({
                                first: ($sd1$Types$EmittableAst$Variable)($ref),
                                second: $core$Basics$identity,
                                third: $env,
                              });
                            }))()
                            : (true
                              ? ((() => {
                                const $5 = ($sd1$Compiler$MakeEmittable$generateName)($env);
                                const $newEnv = $5.second;
                                const $tryName = $5.first;
                                const $wrap = (($tryExpression) => {
                                  return ($sd1$Types$EmittableAst$LetIn)(({
                                    inExpression: $tryExpression,
                                    letExpression: ($sd1$Compiler$MakeEmittable$translateExpression)($newEnv, $value),
                                    maybeName: ($core$Maybe$Just)($tryName),
                                    type: $valueType,
                                  }));
                                });
                                return ({
                                  first: ($sd1$Types$EmittableAst$Variable)(($sd1$Types$Ast$RefLocal)($tryName)),
                                  second: $wrap,
                                  third: $newEnv,
                                });
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 198:16', (sp_toHuman)($4))));
                        }))();
                        const $newEnv = $3.third;
                        const $wrapWithLetIn = $3.second;
                        const $valueExpression = $3.first;
                        const $addTryPatternAndBlock = (($5, $nextTryExpression) => {
                          const $pattern = $5.first;
                          const $block = $5.second;
                          const $testIfPatternMatches = ($sd1$Types$EmittableAst$And)(($core$List$reverse)(($sd1$Compiler$MakeEmittable$testPattern)($pattern, $valueExpression, $core$Core$Nil)));
                          const $namesAndExpressions = ($sd1$Compiler$MakeEmittable$translatePattern)($pattern, $valueExpression);
                          const $whenConditionMatches = ((($0) => {
                            return ($core$List$for)($0, $namesAndExpressions, (($8, $inExpression) => {
                              const $type = $8.first;
                              const $name = $8.second;
                              const $letExpression = $8.third;
                              return ($sd1$Types$EmittableAst$LetIn)(({
                                inExpression: $inExpression,
                                letExpression: $letExpression,
                                maybeName: ($core$Maybe$Just)($name),
                                type: $type,
                              }));
                            }));
                          }))(($sd1$Compiler$MakeEmittable$translateExpression)($newEnv, $block));
                          return ($sd1$Types$EmittableAst$Conditional)($testIfPatternMatches, $whenConditionMatches, $nextTryExpression);
                        });
                        const $default = ((() => {
                          const $human = ($sd1$Compiler$Error$posToHuman)(({
                            content: $env.module.asText,
                            fsPath: $env.module.fsPath,
                          }), $pos);
                          return ($sd1$Types$EmittableAst$MissingPattern)($human.location, $valueExpression);
                        }))();
                        return ($wrapWithLetIn)(((($0) => {
                          return ($core$List$forReversed)($0, $patternsAndExpressions, $addTryPatternAndBlock);
                        }))($default));
                      }))()
                      : ((($expression)[0] === "LetIn")
                        ? ((() => {
                          const $valueDef = ($expression)[1];
                          const $e = ($expression)[2];
                          const $bodyType = ($expression)[3];
                          const $3 = ($sd1$Compiler$MakeEmittable$pickMainName)($valueDef.pattern);
                          return ((($3)[0] === "NoNamedVariables")
                            ? ($sd1$Types$EmittableAst$LetIn)(({
                              inExpression: ($sd1$Compiler$MakeEmittable$translateExpression)($env, $e),
                              letExpression: ($sd1$Compiler$MakeEmittable$translateExpression)($env, $valueDef.body),
                              maybeName: $core$Maybe$Nothing,
                              type: $valueDef.type,
                            }))
                            : ((($3)[0] === "TrivialPattern")
                              ? ((() => {
                                const $defName = ($3)[1];
                                const $type = ($3)[2];
                                return ($sd1$Types$EmittableAst$LetIn)(({
                                  inExpression: ($sd1$Compiler$MakeEmittable$translateExpression)($env, $e),
                                  letExpression: ($sd1$Compiler$MakeEmittable$translateExpression)($env, $valueDef.body),
                                  maybeName: ($core$Maybe$Just)($defName),
                                  type: $type,
                                }));
                              }))()
                              : ((($3)[0] === "GenerateName")
                                ? ((() => {
                                  const $4 = ($sd1$Compiler$MakeEmittable$generateName)($env);
                                  const $newEnv = $4.second;
                                  const $mainName = $4.first;
                                  const $namesAndExpressions = ($sd1$Compiler$MakeEmittable$translatePattern)($valueDef.pattern, ($sd1$Types$EmittableAst$Variable)(($sd1$Types$Ast$RefLocal)($mainName)));
                                  const $wrapWithUnpackedPatternVar = (($6, $inExpression) => {
                                    const $type = $6.first;
                                    const $name = $6.second;
                                    const $letExpression = $6.third;
                                    return ($sd1$Types$EmittableAst$LetIn)(({
                                      inExpression: $inExpression,
                                      letExpression: $letExpression,
                                      maybeName: ($core$Maybe$Just)($name),
                                      type: $type,
                                    }));
                                  });
                                  const $wrapWithActualLetIn = (($inExpression) => {
                                    return ($sd1$Types$EmittableAst$LetIn)(({
                                      inExpression: $inExpression,
                                      letExpression: ($sd1$Compiler$MakeEmittable$translateExpression)($newEnv, $valueDef.body),
                                      maybeName: ($core$Maybe$Just)($mainName),
                                      type: $valueDef.type,
                                    }));
                                  });
                                  return ($wrapWithActualLetIn)(((($0) => {
                                    return ($core$List$forReversed)($0, $namesAndExpressions, $wrapWithUnpackedPatternVar);
                                  }))(($sd1$Compiler$MakeEmittable$translateExpression)($newEnv, $e)));
                                }))()
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 248:12', (sp_toHuman)($3)))));
                        }))()
                        : ((($expression)[0] === "DestroyIn")
                          ? ((() => {
                            const $name = ($expression)[1];
                            const $e = ($expression)[2];
                            return ($sd1$Compiler$MakeEmittable$translateExpression)($env, $e);
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 146:4', (sp_toHuman)($expression))))))))))))));
});

const $sd1$Compiler$MakeEmittable$translateRootValueDef = (($env, $def, $accum) => {
  const $deps = $def.directValueDeps;
  const $4 = ($sd1$Compiler$MakeEmittable$pickMainName)($def.pattern);
  return ((($4)[0] === "NoNamedVariables")
    ? $accum
    : ((($4)[0] === "TrivialPattern")
      ? ((() => {
        const $name = ($4)[1];
        const $type = ($4)[2];
        const $usr = ($sd1$Types$Meta$USR)($env.module.umr, $name);
        return ($core$Dict$insert)($usr, ({
          deps: $deps,
          expr: ($sd1$Compiler$MakeEmittable$translateExpression)($env, $def.body),
          usr: $usr,
        }), $accum);
      }))()
      : ((($4)[0] === "GenerateName")
        ? ((() => {
          const $5 = ($sd1$Compiler$MakeEmittable$generateName)($env);
          const $newEnv = $5.second;
          const $mainName = $5.first;
          const $mainUsr = ($sd1$Types$Meta$USR)($env.module.umr, $mainName);
          const $mainDef = ({
            deps: $deps,
            expr: ($sd1$Compiler$MakeEmittable$translateExpression)($newEnv, $def.body),
            usr: $mainUsr,
          });
          return ((($0) => {
            return ($core$List$for)($0, ($sd1$Compiler$MakeEmittable$translatePattern)($def.pattern, ($sd1$Types$EmittableAst$Variable)(($sd1$Types$Ast$RefGlobal)($mainUsr))), (($8, $z) => {
              const $type = $8.first;
              const $name = $8.second;
              const $expr = $8.third;
              const $subUsr = ($sd1$Types$Meta$USR)($env.module.umr, $name);
              return ($core$Dict$insert)($subUsr, ({
                deps: ($core$Set$ofOne)($mainUsr),
                expr: $expr,
                usr: $subUsr,
              }), $z);
            }));
          }))(((($2) => {
            return ($core$Dict$insert)($mainUsr, $mainDef, $2);
          }))($accum));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 310:4', (sp_toHuman)($4)))));
});

const $sd1$RefHierarchy$resolve = (($getEdges, $target, $path, $state0) => {
  return (($core$List$member)($target, $state0.resolved)
    ? $state0
    : (($core$List$member)($target, $path)
      ? ((() => {
        const $circ = (sp_cons)($target, ($core$List$takeWhile)((($key) => {
          return (sp_not_equal)($key, $target);
        }), $path));
        const $0 = $state0;
        return (Object.assign)({}, $0, ({
          circular: ($core$Dict$insert)(($core$Set$fromList)($circ), $circ, $0.circular),
        }));
      }))()
      : ((() => {
        const $s = ((($0) => {
          return ($core$Dict$for)($0, ($getEdges)($target), (($a, _1, $d) => {
            return ($sd1$RefHierarchy$resolve)($getEdges, $a, (sp_cons)($target, $path), $d);
          }));
        }))($state0);
        const $0 = $s;
        return (Object.assign)({}, $0, ({
          resolved: (sp_cons)($target, $0.resolved),
        }));
      }))()));
});

const $sd1$RefHierarchy$reorder = (($nodeToEdges, $nodesById) => {
  const $keyToEdges = (($id) => {
    const $4 = ($core$Dict$get)($id, $nodesById);
    return ((($4)[0] === "Nothing")
      ? $core$Set$empty
      : ((($4)[0] === "Just")
        ? ((() => {
          const $node = ($4)[1];
          return ($nodeToEdges)($node);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/RefHierarchy.sp 44:8', (sp_toHuman)($4))));
  });
  const $state0 = ({
    circular: $core$Dict$empty,
    resolved: $core$Core$Nil,
  });
  const $stateF = ((($0) => {
    return ($core$Dict$for)($0, $nodesById, (($k, $v, $d) => {
      return ($sd1$RefHierarchy$resolve)($keyToEdges, $k, $core$Core$Nil, $d);
    }));
  }))($state0);
  return ({
    first: ($core$Dict$values)($stateF.circular),
    second: ($core$List$reverse)($stateF.resolved),
  });
});

const $sd1$Compiler$MakeEmittable$translateAll = (($entryModule, $modules) => {
  (sp_benchStart)(null);
  const $globalDefsByName = ((($0) => {
    return ($core$List$for)($0, $modules, (($module, $d) => {
      return ($core$Dict$for)($d, $module.valueDefs, ((_0, $def, $a) => {
        const $env = ({
          genVarCounter: 0,
          module: $module,
        });
        return ($sd1$Compiler$MakeEmittable$translateRootValueDef)($env, $def, $a);
      }));
    }));
  }))($core$Dict$empty);
  const $3 = ($sd1$RefHierarchy$reorder)((($globalDef) => {
    return $globalDef.deps;
  }), $globalDefsByName);
  const $reorderedNames = $3.second;
  const $circulars = $3.first;
  (sp_benchStop)("makeEmittable");
  const $errors = ((($1) => {
    return ($core$List$filter)((($1) => {
      return ($sd1$Compiler$MakeEmittable$circularIsError)($globalDefsByName, $1);
    }), $1);
  }))($circulars);
  return ((sp_not_equal)($errors, $core$Core$Nil)
    ? (sp_todo)("translateAll: Err errors")
    : ($core$Result$Ok)(({
      defs: ($core$List$filterMap)((($name) => {
        return ($core$Dict$get)($name, $globalDefsByName);
      }), $reorderedNames),
      entryUsr: ($sd1$Types$Meta$USR)($entryModule, "main"),
    })));
});

const $sd1$Compiler$TypeCheck$bug = (($msg) => {
  return (sp_todo)(("Compiler bug: " + $msg));
});

const $sd1$Compiler$TypeCheck$addErError = (($equality, $message, $state) => {
  const $0 = $state;
  return (Object.assign)({}, $0, ({
    errors: (sp_cons)(({
      first: $equality,
      second: $message,
    }), $0.errors),
  }));
});

const $sd1$Compiler$TypeCheck$addEquality = (($env, $pos, $why, $t1, $t2, $state) => {
  return ([
    ((($1) => {
      return ((__re__ = (array_push)($state.equalities, $1)), ($state.equalities = (__re__)[1]), (__re__)[0]);
    }))(($sd1$Compiler$TypeCheck$Equality)($env.context, $pos, $why, $t1, $t2)),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$addError = (($env, $pos, $error, $state) => {
  return ([
    ((__re__ = (array_push)($state.errors, ({
      first: $pos,
      second: $env.context,
      third: $error,
    }))), ($state.errors = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$addErrorIf = (($test, $env, $pos, $error, $state) => {
  return ([
    ($test
      ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $error, $state)), ($state = (__re__)[1]), (__re__)[0])
      : null),
    $state,
  ]);
});

const $sd1$Types$TypedAst$resolveUni = (($uniSub, $uni) => {
  return ((($uni)[0] === "Depends")
    ? ((() => {
      const $id = ($uni)[1];
      const $3 = ($uniSub)($id);
      return ((($3)[0] === "Nothing")
        ? $uni
        : ((($3)[0] === "Just")
          ? ((() => {
            const $u = ($3)[1];
            return $u;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 155:12', (sp_toHuman)($3))));
    }))()
    : (true
      ? $uni
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 153:4', (sp_toHuman)($uni))));
});

const $sd1$Types$TypedAst$resolveFull = (($saf, $1) => {
  const $raw = $1.raw;
  const $uni = $1.uni;
  return ({
    raw: ($sd1$Types$TypedAst$resolveRaw)($saf, $raw),
    uni: ($sd1$Types$TypedAst$resolveUni)($saf.uni, $uni),
  });
});

const $sd1$Types$TypedAst$resolveParType = (($saf, $par) => {
  return ((($par)[0] === "ParRe")
    ? ((() => {
      const $raw = ($par)[1];
      return ($sd1$Types$TypedAst$ParRe)(($sd1$Types$TypedAst$resolveRaw)($saf, $raw));
    }))()
    : ((($par)[0] === "ParSp")
      ? ((() => {
        const $full = ($par)[1];
        return ($sd1$Types$TypedAst$ParSp)(($sd1$Types$TypedAst$resolveFull)($saf, $full));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 165:4', (sp_toHuman)($par))));
});

const $sd1$Types$TypedAst$resolveRaw = (($saf, $raw) => {
  const $rec = (($1) => {
    return ($sd1$Types$TypedAst$resolveRaw)($saf, $1);
  });
  return ((($raw)[0] === "TypeVar")
    ? ((() => {
      const $id = ($raw)[1];
      const $3 = ($saf.ty)($id);
      return ((($3)[0] === "Nothing")
        ? $raw
        : ((($3)[0] === "Just")
          ? ((() => {
            const $replacement = ($3)[1];
            return $replacement;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 187:12', (sp_toHuman)($3))));
    }))()
    : ((($raw)[0] === "TypeExact")
      ? ((() => {
        const $usr = ($raw)[1];
        const $pars = ($raw)[2];
        return ($sd1$Types$TypedAst$TypeExact)($usr, ($core$List$map)($rec, $pars));
      }))()
      : ((($raw)[0] === "TypeFn")
        ? ((() => {
          const $pars = ($raw)[1];
          const $out = ($raw)[2];
          return ($sd1$Types$TypedAst$TypeFn)(($core$List$map)((($1) => {
            return ($sd1$Types$TypedAst$resolveParType)($saf, $1);
          }), $pars), ($sd1$Types$TypedAst$resolveFull)($saf, $out));
        }))()
        : (((($raw)[0] === "TypeRecord") && ((($raw)[1])[0] === "Nothing"))
          ? ((() => {
            const $attrs = ($raw)[2];
            return ($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, ($core$Dict$map)((($k, $v) => {
              return ($rec)($v);
            }), $attrs));
          }))()
          : (((($raw)[0] === "TypeRecord") && ((($raw)[1])[0] === "Just"))
            ? ((() => {
              const $id = (($raw)[1])[1];
              const $attrs = ($raw)[2];
              const $3 = ($saf.ty)($id);
              return ((($3)[0] === "Just")
                ? ((() => {
                  const $replacement = ($3)[1];
                  return $replacement;
                }))()
                : ((($3)[0] === "Nothing")
                  ? ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($id), ($core$Dict$map)((($k, $v) => {
                    return ($rec)($v);
                  }), $attrs))
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 203:12', (sp_toHuman)($3))));
            }))()
            : ((($raw)[0] === "TypeError")
              ? $sd1$Types$TypedAst$TypeError
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 185:4', (sp_toHuman)($raw))))))));
});

const $sd1$Compiler$TypeCheck$applySubstitutionToType = (($tyvarId, $replacingType, $originalType) => {
  const $subsAsFns = ({
    ty: (($id) => {
      return ((sp_equal)($id, $tyvarId)
        ? ($core$Maybe$Just)($replacingType)
        : $core$Maybe$Nothing);
    }),
    uni: ((_0) => {
      return $core$Maybe$Nothing;
    }),
  });
  return ($sd1$Types$TypedAst$resolveRaw)($subsAsFns, $originalType);
});

const $sd1$Compiler$TypeCheck$newTyvarId = (($state) => {
  ($state.lastUnificationVarId += 1);
  return ([
    ((__re__ = (basics_cloneUni)($state.lastUnificationVarId)), ($state.lastUnificationVarId = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$replaceUnivarRec = (($old, $new, $raw) => {
  const $doRaw = (($2) => {
    return ($sd1$Compiler$TypeCheck$replaceUnivarRec)($old, $new, $2);
  });
  return ((($raw)[0] === "TypeExact")
    ? ((() => {
      const $usr = ($raw)[1];
      const $args = ($raw)[2];
      return ($sd1$Types$TypedAst$TypeExact)($usr, ($core$List$map)($doRaw, $args));
    }))()
    : ((($raw)[0] === "TypeRecord")
      ? ((() => {
        const $maybeExt = ($raw)[1];
        const $attrs = ($raw)[2];
        return ($sd1$Types$TypedAst$TypeRecord)($maybeExt, ($core$Dict$map)((($k, $v) => {
          return ($doRaw)($v);
        }), $attrs));
      }))()
      : ((($raw)[0] === "TypeError")
        ? $sd1$Types$TypedAst$TypeError
        : ((($raw)[0] === "TypeVar")
          ? ((() => {
            const $id = ($raw)[1];
            return ($sd1$Types$TypedAst$TypeVar)($id);
          }))()
          : ((($raw)[0] === "TypeFn")
            ? ((() => {
              const $ins = ($raw)[1];
              const $out = ($raw)[2];
              const $doUni = (($uni) => {
                return ((($uni)[0] === "Depends")
                  ? ((() => {
                    const $id = ($uni)[1];
                    return ((sp_equal)($id, $old)
                      ? $new
                      : $uni);
                  }))()
                  : (true
                    ? $uni
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 348:16', (sp_toHuman)($uni))));
              });
              const $mapPar = (($par) => {
                return ((($par)[0] === "ParRe")
                  ? ((() => {
                    const $r = ($par)[1];
                    return ($sd1$Types$TypedAst$ParRe)(($doRaw)($r));
                  }))()
                  : ((($par)[0] === "ParSp")
                    ? ((() => {
                      const $f = ($par)[1];
                      return ($sd1$Types$TypedAst$ParSp)(({
                        raw: ($doRaw)($f.raw),
                        uni: ($doUni)($f.uni),
                      }));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 354:16', (sp_toHuman)($par))));
              });
              return ($sd1$Types$TypedAst$TypeFn)(($core$List$map)($mapPar, $ins), ({
                raw: ($doRaw)($out.raw),
                uni: ($doUni)($out.uni),
              }));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 331:4', (sp_toHuman)($raw)))))));
});

const $sd1$Compiler$TypeCheck$generalize = (($env, $pos, $ref, $instance, $state) => {
  const $replaceUnivar = (($originalUnivarId, _1, $r) => {
    const $newUnivarId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
    return ($sd1$Compiler$TypeCheck$replaceUnivarRec)($originalUnivarId, ($sd1$Types$Ast$Depends)($newUnivarId), $r);
  });
  const $raw = ((($0) => {
    return ($core$Dict$for)($0, $instance.freeTyvars, (($originalTyvarId, $tyvar, $a) => {
      const $generalizedTyvarId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
      ((__re__ = (hash_insert)($state.tyvarsById, $generalizedTyvarId, ((() => {
        const $0 = $tyvar;
        return (Object.assign)({}, $0, ({
          generalizedAt: $pos,
          generalizedFor: $ref,
        }));
      }))())), ($state.tyvarsById = (__re__)[1]), (__re__)[0]);
      return ($sd1$Compiler$TypeCheck$applySubstitutionToType)($originalTyvarId, ($sd1$Types$TypedAst$TypeVar)($generalizedTyvarId), $a);
    }));
  }))(((($0) => {
    return ($core$Dict$for)($0, $instance.freeUnivars, $replaceUnivar);
  }))($instance.type.raw));
  const $0 = $instance.type;
  return ([
    (Object.assign)({}, $0, ({
      raw: $raw,
    })),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$getConstructorByUsr = (($usr, $env) => {
  return ($core$Dict$get)($usr, $env.constructors);
});

const $sd1$Compiler$TypeCheck$coreTypeNumber = ($sd1$Types$TypedAst$TypeExact)($sd1$Compiler$CoreTypes$numberDef.usr, $core$Core$Nil);

const $sd1$Compiler$TypeCheck$coreTypeText = ($sd1$Types$TypedAst$TypeExact)($sd1$Compiler$CoreTypes$textDef.usr, $core$Core$Nil);

const $sd1$Compiler$TypeCheck$newRawType = (($state) => {
  return ([
    ($sd1$Types$TypedAst$TypeVar)(((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0])),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$expandTyvarsInType = (($tyvarIdsToType, $state, $type) => {
  const $rec = (($2) => {
    return ((__re__ = ($sd1$Compiler$TypeCheck$expandTyvarsInType)($tyvarIdsToType, $state, $2)), ($state = (__re__)[1]), (__re__)[0]);
  });
  return ([
    ((($type)[0] === "TypeExact")
      ? ((() => {
        const $usr = ($type)[1];
        const $args = ($type)[2];
        return ($sd1$Types$TypedAst$TypeExact)($usr, ($core$List$map)($rec, $args));
      }))()
      : ((($type)[0] === "TypeFn")
        ? ((() => {
          const $ins = ($type)[1];
          const $out = ($type)[2];
          return ($sd1$Types$TypedAst$TypeFn)(($sd1$Types$TypedAst$mapPars)($rec, $ins), ((() => {
            const $0 = $out;
            return (Object.assign)({}, $0, ({
              raw: ($rec)($0.raw),
            }));
          }))());
        }))()
        : (((($type)[0] === "TypeRecord") && ((($type)[1])[0] === "Nothing"))
          ? ((() => {
            const $attrs = ($type)[2];
            return ($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, ($core$Dict$map)((($k, $v) => {
              return ($rec)($v);
            }), $attrs));
          }))()
          : ((($type)[0] === "TypeVar")
            ? ((() => {
              const $id = ($type)[1];
              const $4 = ($core$Dict$get)($id, $tyvarIdsToType);
              return ((($4)[0] === "Nothing")
                ? ($sd1$Compiler$TypeCheck$bug)("this is not supposed to happen")
                : ((($4)[0] === "Just")
                  ? ((() => {
                    const $ty = ($4)[1];
                    return $ty;
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 383:12', (sp_toHuman)($4))));
            }))()
            : (((($type)[0] === "TypeRecord") && ((($type)[1])[0] === "Just"))
              ? ((() => {
                const $id = (($type)[1])[1];
                const $attrs = ($type)[2];
                return ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($id), ($core$Dict$map)((($k, $v) => {
                  return ($rec)($v);
                }), $attrs));
              }))()
              : ((($type)[0] === "TypeError")
                ? $sd1$Types$TypedAst$TypeError
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 372:4', (sp_toHuman)($type)))))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$translateUni = (($originalIdToNewId, $originalUni) => {
  return ((($originalUni)[0] === "Depends")
    ? ((() => {
      const $originalId = ($originalUni)[1];
      const $3 = ($core$Dict$get)($originalId, $originalIdToNewId);
      return ((($3)[0] === "Just")
        ? ((() => {
          const $newId = ($3)[1];
          return ($sd1$Types$Ast$Depends)($newId);
        }))()
        : ((($3)[0] === "Nothing")
          ? $originalUni
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 400:12', (sp_toHuman)($3))));
    }))()
    : (true
      ? $originalUni
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 398:4', (sp_toHuman)($originalUni))));
});

const $sd1$Compiler$TypeCheck$translateFullType = (($env, $argsByName, $originalIdToNewId, $state, $caFull) => {
  return ([
    ({
      raw: ((__re__ = ($sd1$Compiler$TypeCheck$translateRawType)($env, $argsByName, $originalIdToNewId, $state, $caFull.raw)), ($state = (__re__)[1]), (__re__)[0]),
      uni: ($sd1$Compiler$TypeCheck$translateUni)($originalIdToNewId, $caFull.uni),
    }),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$translateRawType = (($env, $argsByName, $originalIdToNewId, $state, $caType) => {
  const $rec = (($4) => {
    return ((__re__ = ($sd1$Compiler$TypeCheck$translateRawType)($env, $argsByName, $originalIdToNewId, $state, $4)), ($state = (__re__)[1]), (__re__)[0]);
  });
  return ([
    ((($caType)[0] === "TypeFn")
      ? ((() => {
        const $pos = ($caType)[1];
        const $caPars = ($caType)[2];
        const $caOut = ($caType)[3];
        const $zzz = (($caPar) => {
          return ((($caPar)[0] === "ParRe")
            ? ((() => {
              const $caRaw = ($caPar)[1];
              return ($sd1$Types$TypedAst$ParRe)(($rec)($caRaw));
            }))()
            : ((($caPar)[0] === "ParSp")
              ? ((() => {
                const $caFull = ($caPar)[1];
                return ($sd1$Types$TypedAst$ParSp)(((__re__ = ($sd1$Compiler$TypeCheck$translateFullType)($env, $argsByName, $originalIdToNewId, $state, $caFull)), ($state = (__re__)[1]), (__re__)[0]));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 430:20', (sp_toHuman)($caPar))));
        });
        const $taArgs = ($core$List$map)($zzz, $caPars);
        return ($sd1$Types$TypedAst$TypeFn)($taArgs, ((__re__ = ($sd1$Compiler$TypeCheck$translateFullType)($env, $argsByName, $originalIdToNewId, $state, $caOut)), ($state = (__re__)[1]), (__re__)[0]));
      }))()
      : ((($caType)[0] === "TypeRecord")
        ? ((() => {
          const $pos = ($caType)[1];
          const $caAttrs = ($caType)[2];
          return ($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, ($core$Dict$map)((($name, $v) => {
            return ($rec)($v);
          }), $caAttrs));
        }))()
        : ((($caType)[0] === "TypeAnnotationVariable")
          ? ((() => {
            const $pos = ($caType)[1];
            const $name = ($caType)[2];
            const $6 = ($core$Dict$get)($name, $argsByName);
            return ((($6)[0] === "Nothing")
              ? ((() => {
                ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorUndefinedTypeVariable)($name), $state)), ($state = (__re__)[1]), (__re__)[0]);
                return $sd1$Types$TypedAst$TypeError;
              }))()
              : ((($6)[0] === "Just")
                ? ((() => {
                  const $raw = ($6)[1];
                  return $raw;
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 445:12', (sp_toHuman)($6))));
          }))()
          : ((($caType)[0] === "TypeNamed")
            ? ((() => {
              const $pos = ($caType)[1];
              const $usr = ($caType)[2];
              const $pars = ($caType)[3];
              const $expandedPars = ($core$List$map)($rec, $pars);
              const $6 = ($core$Dict$get)($usr, $env.expandedAliases);
              return ((($6)[0] === "Nothing")
                ? ($sd1$Types$TypedAst$TypeExact)($usr, $expandedPars)
                : ((($6)[0] === "Just")
                  ? ((() => {
                    const $expandedAlias = ($6)[1];
                    return ((sp_not_equal)(($core$List$length)($expandedAlias.pars), ($core$List$length)($expandedPars))
                      ? ((() => {
                        ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorWrongNumberOfTypeArguments)($usr, $expandedAlias.pars, $expandedPars), $state)), ($state = (__re__)[1]), (__re__)[0]);
                        return $sd1$Types$TypedAst$TypeError;
                      }))()
                      : ((() => {
                        const $tyvarIdsToType = ($core$Dict$fromList)(($core$List$map2)($core$Tuple$pair, $expandedAlias.pars, $expandedPars));
                        return ((__re__ = ($sd1$Compiler$TypeCheck$expandTyvarsInType)($tyvarIdsToType, $state, $expandedAlias.type)), ($state = (__re__)[1]), (__re__)[0]);
                      }))());
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 459:12', (sp_toHuman)($6))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 425:4', (sp_toHuman)($caType)))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$translateAnnotation = (($env, $state, $ca) => {
  const $nameToType = ($core$Dict$map)((($k, $v) => {
    return ($sd1$Types$TypedAst$TypeVar)($v);
  }), $env.annotatedTyvarsByName);
  return ([
    ((__re__ = ($sd1$Compiler$TypeCheck$translateRawType)($env, $nameToType, $env.annotatedUnivarsByOriginalId, $state, $ca)), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferPatternAny = (($env, $pos, $uni, $2, $state) => {
  const $maybeAnnotation = $2.maybeAnnotation;
  const $maybeName = $2.maybeName;
  const $raw = ((($maybeAnnotation)[0] === "Nothing")
    ? ((__re__ = ($sd1$Compiler$TypeCheck$newRawType)($state)), ($state = (__re__)[1]), (__re__)[0])
    : ((($maybeAnnotation)[0] === "Just")
      ? ((() => {
        const $annotation = ($maybeAnnotation)[1];
        return ((__re__ = ($sd1$Compiler$TypeCheck$translateAnnotation)($env, $state, $annotation)), ($state = (__re__)[1]), (__re__)[0]);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1576:8', (sp_toHuman)($maybeAnnotation))));
  const $type = ({
    raw: $raw,
    uni: $uni,
  });
  const $envWithVariable = ((($maybeName)[0] === "Nothing")
    ? $env
    : ((($maybeName)[0] === "Just")
      ? ((() => {
        const $name = ($maybeName)[1];
        const $variable = ({
          definedAt: $pos,
          freeTyvars: $core$Dict$empty,
          freeUnivars: $core$Dict$empty,
          type: $type,
        });
        const $0 = $env;
        return (Object.assign)({}, $0, ({
          variables: ($core$Dict$insert)(($sd1$Types$Ast$RefLocal)($name), $variable, $0.variables),
        }));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1587:8', (sp_toHuman)($maybeName))));
  const $typedPattern = ($sd1$Types$TypedAst$PatternAny)($pos, ({
    maybeName: $maybeName,
    type: $type,
  }));
  return ([
    ({
      env: $envWithVariable,
      maybeFullAnnotation: $maybeAnnotation,
      patternType: $raw,
      typedPattern: $typedPattern,
    }),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$list_eachWithIndex2 = (($index, $aa, $bb, $f) => {
  const $5 = ({
    first: $aa,
    second: $bb,
  });
  return (((($5.first)[0] === "Cons") && (($5.second)[0] === "Cons"))
    ? ((() => {
      const $a = ($5.first)[1];
      const $at = ($5.first)[2];
      const $b = ($5.second)[1];
      const $bt = ($5.second)[2];
      ($f)($index, $a, $b);
      return ($sd1$Compiler$TypeCheck$list_eachWithIndex2)(($index + 1), $at, $bt, $f);
    }))()
    : (true
      ? null
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 57:4', (sp_toHuman)($5))));
});

const $sd1$Compiler$TypeCheck$inferPattern = (($env, $uni, $pattern, $state) => {
  return ([
    ((($pattern)[0] === "PatternAny")
      ? ((() => {
        const $pos = ($pattern)[1];
        const $args = ($pattern)[2];
        return ((__re__ = ($sd1$Compiler$TypeCheck$inferPatternAny)($env, $pos, $uni, $args, $state)), ($state = (__re__)[1]), (__re__)[0]);
      }))()
      : ((($pattern)[0] === "PatternLiteralText")
        ? ((() => {
          const $pos = ($pattern)[1];
          const $text = ($pattern)[2];
          return ({
            env: $env,
            maybeFullAnnotation: $core$Maybe$Nothing,
            patternType: $sd1$Compiler$TypeCheck$coreTypeText,
            typedPattern: ($sd1$Types$TypedAst$PatternLiteralText)($pos, $text),
          });
        }))()
        : ((($pattern)[0] === "PatternLiteralNumber")
          ? ((() => {
            const $pos = ($pattern)[1];
            const $n = ($pattern)[2];
            return ({
              env: $env,
              maybeFullAnnotation: $core$Maybe$Nothing,
              patternType: $sd1$Compiler$TypeCheck$coreTypeNumber,
              typedPattern: ($sd1$Types$TypedAst$PatternLiteralNumber)($pos, $n),
            });
          }))()
          : ((($pattern)[0] === "PatternConstructor")
            ? ((() => {
              const $pos = ($pattern)[1];
              const $usr = ($pattern)[2];
              const $arguments = ($pattern)[3];
              const $5 = ((($0) => {
                return ($core$List$forReversed)($0, $arguments, (($arg, $7) => {
                  const $argOuts = $7.first;
                  const $envX = $7.second;
                  const $out = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($envX, $uni, $arg, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  return ({
                    first: (sp_cons)($out, $argOuts),
                    second: $out.env,
                  });
                }));
              }))(({
                first: $core$Core$Nil,
                second: $env,
              }));
              const $newEnv = $5.second;
              const $argumentOuts = $5.first;
              const $typedArguments = ($core$List$map)((($out) => {
                return $out.typedPattern;
              }), $argumentOuts);
              const $argumentTypes = ($core$List$map)((($out) => {
                return $out.patternType;
              }), $argumentOuts);
              const $finalType = ((() => {
                const $6 = ($sd1$Compiler$TypeCheck$getConstructorByUsr)($usr, $env);
                return ((($6)[0] === "Nothing")
                  ? ((() => {
                    ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorConstructorNotFound)($usr), $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return $sd1$Types$TypedAst$TypeError;
                  }))()
                  : ((($6)[0] === "Just")
                    ? ((() => {
                      const $cons = ($6)[1];
                      const $x = ((__re__ = ($sd1$Compiler$TypeCheck$generalize)($env, $pos, ($sd1$Types$Ast$RefGlobal)($usr), $cons, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      const $7 = ((() => {
                        const $8 = $x.raw;
                        return ((($8)[0] === "TypeFn")
                          ? ((() => {
                            const $ins = ($8)[1];
                            const $out = ($8)[2];
                            return ({
                              first: $ins,
                              second: $out.raw,
                            });
                          }))()
                          : (true
                            ? ({
                              first: $core$Core$Nil,
                              second: $x.raw,
                            })
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1523:28', (sp_toHuman)($8))));
                      }))();
                      const $returnType = $7.second;
                      const $parTypes = $7.first;
                      ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)(($core$List$length)($parTypes), ($core$List$length)($arguments)), $env, $pos, $sd1$Compiler$TypeCheck$ErrorWrongNumberOfConstructorArguments, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      ($sd1$Compiler$TypeCheck$list_eachWithIndex2)(0, $parTypes, $argumentTypes, (($index, $parType, $argType) => {
                        return ((($parType)[0] === "ParRe")
                          ? ((() => {
                            const $raw = ($parType)[1];
                            return ($sd1$Compiler$TypeCheck$bug)("cons can't recycle?!");
                          }))()
                          : ((($parType)[0] === "ParSp")
                            ? ((() => {
                              const $full = ($parType)[1];
                              return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, ($sd1$Compiler$TypeCheck$Why_Argument)($index), $full.raw, $argType, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            }))()
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1530:28', (sp_toHuman)($parType))));
                      }));
                      return $returnType;
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1512:16', (sp_toHuman)($6))));
              }))();
              return ({
                env: $newEnv,
                maybeFullAnnotation: $core$Maybe$Nothing,
                patternType: $finalType,
                typedPattern: ($sd1$Types$TypedAst$PatternConstructor)($pos, $usr, $typedArguments),
              });
            }))()
            : ((($pattern)[0] === "PatternRecord")
              ? ((() => {
                const $pos = ($pattern)[1];
                const $completeness = ($pattern)[2];
                const $pas = ($pattern)[3];
                const $5 = ((($0) => {
                  return ($core$Dict$for)($0, $pas, (($name, $pa, $7) => {
                    const $dict = $7.first;
                    const $envX = $7.second;
                    const $out = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($envX, $uni, $pa, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return ({
                      first: ($core$Dict$insert)($name, $out, $dict),
                      second: $out.env,
                    });
                  }));
                }))(({
                  first: $core$Dict$empty,
                  second: $env,
                }));
                const $newEnv = $5.second;
                const $outs = $5.first;
                const $patternExt = ((($completeness)[0] === "Complete")
                  ? $core$Maybe$Nothing
                  : ((($completeness)[0] === "Partial")
                    ? ($core$Maybe$Just)(((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]))
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1556:16', (sp_toHuman)($completeness))));
                const $raw = ($sd1$Types$TypedAst$TypeRecord)($patternExt, ((($1) => {
                  return ($core$Dict$map)((($name, $out) => {
                    return $out.patternType;
                  }), $1);
                }))($outs));
                return ({
                  env: $newEnv,
                  maybeFullAnnotation: $core$Maybe$Nothing,
                  patternType: $raw,
                  typedPattern: ($sd1$Types$TypedAst$PatternRecord)($pos, ((($1) => {
                    return ($core$Dict$map)((($k, $o) => {
                      return ({
                        first: $o.typedPattern,
                        second: $o.patternType,
                      });
                    }), $1);
                  }))($outs)),
                });
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1472:4', (sp_toHuman)($pattern))))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$fullTypeError = ({
  raw: $sd1$Types$TypedAst$TypeError,
  uni: $sd1$Types$Ast$Uni,
});

const $sd1$Compiler$TypeCheck$patternError = (($pos) => {
  return ($sd1$Types$TypedAst$PatternAny)($pos, ({
    maybeName: $core$Maybe$Nothing,
    type: $sd1$Compiler$TypeCheck$fullTypeError,
  }));
});

const $sd1$Compiler$TypeCheck$checkPatternConstructor = (($env, $pos, $expectedType, $usr, $arguments, $state) => {
  const $insertArgsOnError = (($0) => {
    return ($core$List$for)($0, $arguments, (($arg, $envX) => {
      const $out = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($envX, $expectedType.uni, $arg, $state)), ($state = (__re__)[1]), (__re__)[0]);
      return $out.env;
    }));
  });
  const $7 = ($sd1$Compiler$TypeCheck$getConstructorByUsr)($usr, $env);
  return ([
    ((($7)[0] === "Nothing")
      ? ((() => {
        ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorConstructorNotFound)($usr), $state)), ($state = (__re__)[1]), (__re__)[0]);
        return ({
          first: ($sd1$Compiler$TypeCheck$patternError)($pos),
          second: ($insertArgsOnError)($env),
        });
      }))()
      : ((($7)[0] === "Just")
        ? ((() => {
          const $instance = ($7)[1];
          const $fullType_ = ((__re__ = ($sd1$Compiler$TypeCheck$generalize)($env, $pos, ($sd1$Types$Ast$RefGlobal)($usr), ((() => {
            const $0 = $instance;
            return (Object.assign)({}, $0, ({
              freeUnivars: $core$Dict$empty,
            }));
          }))(), $state)), ($state = (__re__)[1]), (__re__)[0]);
          const $fullType = ((() => {
            const $0 = $fullType_;
            return (Object.assign)({}, $0, ({
              raw: ($sd1$Compiler$TypeCheck$replaceUnivarRec)(1, $expectedType.uni, $0.raw),
            }));
          }))();
          const $8 = ((() => {
            const $9 = $fullType.raw;
            return ((($9)[0] === "TypeFn")
              ? ((() => {
                const $ax = ($9)[1];
                const $o = ($9)[2];
                return ({
                  first: $ax,
                  second: $o,
                });
              }))()
              : (true
                ? ({
                  first: $core$Core$Nil,
                  second: $fullType,
                })
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1736:16', (sp_toHuman)($9))));
          }))();
          const $requiredOut = $8.second;
          const $requiredParTypes = $8.first;
          return ((sp_not_equal)(($core$List$length)($arguments), ($core$List$length)($requiredParTypes))
            ? ((() => {
              ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorWrongNumberOfConstructorArguments, $state)), ($state = (__re__)[1]), (__re__)[0]);
              return ({
                first: ($sd1$Compiler$TypeCheck$patternError)($pos),
                second: ($insertArgsOnError)($env),
              });
            }))()
            : ((() => {
              const $checkArg = (($10, $9) => {
                const $arg = $10.first;
                const $parType = $10.second;
                const $envX = $9.first;
                const $args = $9.second;
                const $11 = ((($parType)[0] === "ParSp")
                  ? ((() => {
                    const $full = ($parType)[1];
                    return ((__re__ = ($sd1$Compiler$TypeCheck$checkPattern)($envX, $full, $arg, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  }))()
                  : ((($parType)[0] === "ParRe")
                    ? ((() => {
                      const $raw = ($parType)[1];
                      return ($sd1$Compiler$TypeCheck$bug)("should not happen???");
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1748:28', (sp_toHuman)($parType))));
                const $envX1 = $11.second;
                const $taArg = $11.first;
                return ({
                  first: $envX1,
                  second: (sp_cons)($taArg, $args),
                });
              });
              const $9 = ((($0) => {
                return ($core$List$forReversed)($0, ($core$List$map2)($core$Tuple$pair, $arguments, $requiredParTypes), $checkArg);
              }))(({
                first: $env,
                second: $core$Core$Nil,
              }));
              const $typedArgs = $9.second;
              const $newEnv = $9.first;
              ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_CalledAsFunction, $requiredOut.raw, $expectedType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
              return ({
                first: ($sd1$Types$TypedAst$PatternConstructor)($pos, $usr, $typedArgs),
                second: $newEnv,
              });
            }))());
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1720:4', (sp_toHuman)($7)))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$checkPatternRecord = (($env, $pos, $expectedType, $completeness, $pas, $state) => {
  const $7 = $expectedType;
  const $uni = $7.uni;
  const $8 = $expectedType.raw;
  return ([
    (((($8)[0] === "TypeRecord") && ((($8)[1])[0] === "Nothing"))
      ? ((() => {
        const $attrs = ($8)[2];
        const $9 = ($core$Dict$onlyBothOnly)($pas, $attrs);
        const $typeOnly = $9.third;
        const $both = $9.second;
        const $paOnly = $9.first;
        ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($paOnly, $core$Dict$empty), $env, $pos, $sd1$Compiler$TypeCheck$ErrorRecordHasAttributesNotInAnnotation, $state)), ($state = (__re__)[1]), (__re__)[0]);
        ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)(((sp_not_equal)($typeOnly, $core$Dict$empty) && (sp_equal)($completeness, $sd1$Types$CanonicalAst$Complete)), $env, $pos, $sd1$Compiler$TypeCheck$ErrorRecordIsMissingAttibutesInAnnotation, $state)), ($state = (__re__)[1]), (__re__)[0]);
        const $10 = ((($0) => {
          return ($core$Dict$for)($0, $both, (($name, $13, $12) => {
            const $pa = $13.first;
            const $raw = $13.second;
            const $acc = $12.first;
            const $envX = $12.second;
            const $15 = ((__re__ = ($sd1$Compiler$TypeCheck$checkPattern)(((() => {
              const $0 = $envX;
              return (Object.assign)({}, $0, ({
                context: ($sd1$Compiler$TypeCheck$Context_AttributeName)($name, $env.context),
              }));
            }))(), ({
              raw: $raw,
              uni: $uni,
            }), $pa, $state)), ($state = (__re__)[1]), (__re__)[0]);
            const $envX0 = $15.second;
            const $taPa = $15.first;
            return ({
              first: ($core$Dict$insert)($name, ({
                first: $taPa,
                second: $raw,
              }), $acc),
              second: ((() => {
                const $0 = $envX0;
                return (Object.assign)({}, $0, ({
                  context: $env.context,
                }));
              }))(),
            });
          }));
        }))(({
          first: $core$Dict$empty,
          second: $env,
        }));
        const $envF = $10.second;
        const $taPas = $10.first;
        return ({
          first: ($sd1$Types$TypedAst$PatternRecord)($pos, $taPas),
          second: $envF,
        });
      }))()
      : (((($8)[0] === "TypeRecord") && ((($8)[1])[0] === "Just"))
        ? ((() => {
          const $tyvarId = (($8)[1])[1];
          const $a = ($8)[2];
          return ($sd1$Compiler$TypeCheck$bug)("can't annotate extensible types");
        }))()
        : (true
          ? ((() => {
            ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorNotCompatibleWithRecord, $state)), ($state = (__re__)[1]), (__re__)[0]);
            const $envF = ((($0) => {
              return ($core$Dict$for)($0, $pas, (($name, $pa, $envX) => {
                const $out = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($envX, $expectedType.uni, $pa, $state)), ($state = (__re__)[1]), (__re__)[0]);
                return $out.env;
              }));
            }))($env);
            return ({
              first: ($sd1$Compiler$TypeCheck$patternError)($pos),
              second: $envF,
            });
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1671:4', (sp_toHuman)($8))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$checkPattern = (($env, $expectedType, $pattern, $state) => {
  const $5 = ({
    first: $pattern,
    second: $expectedType.raw,
  });
  return ([
    ((($5.first)[0] === "PatternAny")
      ? ((() => {
        const $pos = ($5.first)[1];
        const $maybeAnnotation = ($5.first)[2].maybeAnnotation;
        const $maybeName = ($5.first)[2].maybeName;
        const $newEnv = ((($maybeName)[0] === "Nothing")
          ? $env
          : ((($maybeName)[0] === "Just")
            ? ((() => {
              const $name = ($maybeName)[1];
              const $variable = ({
                definedAt: $pos,
                freeTyvars: $core$Dict$empty,
                freeUnivars: $core$Dict$empty,
                type: $expectedType,
              });
              const $0 = $env;
              return (Object.assign)({}, $0, ({
                variables: ($core$Dict$insert)(($sd1$Types$Ast$RefLocal)($name), $variable, $0.variables),
              }));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1623:16', (sp_toHuman)($maybeName))));
        return ({
          first: ($sd1$Types$TypedAst$PatternAny)($pos, ({
            maybeName: $maybeName,
            type: $expectedType,
          })),
          second: $newEnv,
        });
      }))()
      : (((($5.first)[0] === "PatternLiteralText") && ((($5.second)[0] === "TypeExact") && ((($5.second)[2])[0] === "Nil")))
        ? ((() => {
          const $pos = ($5.first)[1];
          const $text = ($5.first)[2];
          const $typeUsr = ($5.second)[1];
          ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($typeUsr, $sd1$Compiler$CoreTypes$textDef.usr), $env, $pos, ($sd1$Compiler$TypeCheck$ErrorIncompatiblePattern)($pattern, $expectedType), $state)), ($state = (__re__)[1]), (__re__)[0]);
          return ({
            first: ($sd1$Types$TypedAst$PatternLiteralText)($pos, $text),
            second: $env,
          });
        }))()
        : (((($5.first)[0] === "PatternLiteralNumber") && ((($5.second)[0] === "TypeExact") && ((($5.second)[2])[0] === "Nil")))
          ? ((() => {
            const $pos = ($5.first)[1];
            const $text = ($5.first)[2];
            const $typeUsr = ($5.second)[1];
            ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($typeUsr, $sd1$Compiler$CoreTypes$numberDef.usr), $env, $pos, ($sd1$Compiler$TypeCheck$ErrorIncompatiblePattern)($pattern, $expectedType), $state)), ($state = (__re__)[1]), (__re__)[0]);
            return ({
              first: ($sd1$Types$TypedAst$PatternLiteralNumber)($pos, $text),
              second: $env,
            });
          }))()
          : ((($5.first)[0] === "PatternConstructor")
            ? ((() => {
              const $pos = ($5.first)[1];
              const $usr = ($5.first)[2];
              const $arguments = ($5.first)[3];
              return ((__re__ = ($sd1$Compiler$TypeCheck$checkPatternConstructor)($env, $pos, $expectedType, $usr, $arguments, $state)), ($state = (__re__)[1]), (__re__)[0]);
            }))()
            : ((($5.first)[0] === "PatternRecord")
              ? ((() => {
                const $pos = ($5.first)[1];
                const $completeness = ($5.first)[2];
                const $pas = ($5.first)[3];
                return ((__re__ = ($sd1$Compiler$TypeCheck$checkPatternRecord)($env, $pos, $expectedType, $completeness, $pas, $state)), ($state = (__re__)[1]), (__re__)[0]);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1619:4', (sp_toHuman)($5))))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$checkParameter = (($env, $expectedParType, $par, $state) => {
  return ([
    ((($par)[0] === "ParameterPattern")
      ? ((() => {
        const $originalUni = ($par)[1];
        const $pa = ($par)[2];
        const $5 = ((($expectedParType)[0] === "ParRe")
          ? ((() => {
            ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, ($sd1$Types$CanonicalAst$patternPos)($pa), $sd1$Compiler$TypeCheck$ErrorRecyclingDoesNotMatch, $state)), ($state = (__re__)[1]), (__re__)[0]);
            const $o = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($env, $sd1$Types$Ast$Uni, $pa, $state)), ($state = (__re__)[1]), (__re__)[0]);
            return ({
              first: ({
                raw: $o.patternType,
                uni: $sd1$Types$Ast$Uni,
              }),
              second: ({
                first: $o.typedPattern,
                second: $o.env,
              }),
            });
          }))()
          : ((($expectedParType)[0] === "ParSp")
            ? ((() => {
              const $full = ($expectedParType)[1];
              const $uni = ($sd1$Compiler$TypeCheck$translateUni)($env.annotatedUnivarsByOriginalId, $originalUni);
              ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($uni, $full.uni), $env, ($sd1$Types$CanonicalAst$patternPos)($pa), ($sd1$Compiler$TypeCheck$ErrorUniquenessDoesNotMatchParameter)($uni, $full), $state)), ($state = (__re__)[1]), (__re__)[0]);
              return ({
                first: $full,
                second: ((__re__ = ($sd1$Compiler$TypeCheck$checkPattern)($env, $full, $pa, $state)), ($state = (__re__)[1]), (__re__)[0]),
              });
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1086:16', (sp_toHuman)($expectedParType))));
        const $env1 = $5.second.second;
        const $typedPa = $5.second.first;
        const $fullType = $5.first;
        return ({
          first: ($sd1$Types$TypedAst$ParameterPattern)($fullType, $typedPa),
          second: $env1,
        });
      }))()
      : ((($par)[0] === "ParameterPlaceholder")
        ? ((() => {
          const $name = ($par)[1];
          const $num = ($par)[2];
          return ((($expectedParType)[0] === "ParRe")
            ? (sp_todo)("TA.ParRe")
            : ((($expectedParType)[0] === "ParSp")
              ? ((() => {
                const $type = ($expectedParType)[1];
                const $pa = ($sd1$Types$TypedAst$PatternAny)($sd1$Types$Pos$G, ({
                  maybeName: ($core$Maybe$Just)($name),
                  type: $type,
                }));
                const $variable = ({
                  definedAt: $sd1$Types$Pos$G,
                  freeTyvars: $core$Dict$empty,
                  freeUnivars: $core$Dict$empty,
                  type: $type,
                });
                return ({
                  first: ($sd1$Types$TypedAst$ParameterPattern)($type, $pa),
                  second: ((() => {
                    const $0 = $env;
                    return (Object.assign)({}, $0, ({
                      variables: ($core$Dict$insert)(($sd1$Types$Ast$RefLocal)($name), $variable, $0.variables),
                    }));
                  }))(),
                });
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1101:12', (sp_toHuman)($expectedParType))));
        }))()
        : ((($par)[0] === "ParameterRecycle")
          ? ((() => {
            const $pos = ($par)[1];
            const $name = ($par)[2];
            const $expectedRaw = ((($expectedParType)[0] === "ParSp")
              ? ((() => {
                const $full = ($expectedParType)[1];
                ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorRecyclingDoesNotMatch, $state)), ($state = (__re__)[1]), (__re__)[0]);
                return $sd1$Types$TypedAst$TypeError;
              }))()
              : ((($expectedParType)[0] === "ParRe")
                ? ((() => {
                  const $raw = ($expectedParType)[1];
                  return $raw;
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1121:16', (sp_toHuman)($expectedParType))));
            const $variable = ({
              definedAt: $pos,
              freeTyvars: $core$Dict$empty,
              freeUnivars: $core$Dict$empty,
              type: ({
                raw: $expectedRaw,
                uni: $sd1$Types$Ast$Uni,
              }),
            });
            const $localEnv = ((() => {
              const $0 = $env;
              return (Object.assign)({}, $0, ({
                variables: ($core$Dict$insert)(($sd1$Types$Ast$RefLocal)($name), $variable, $0.variables),
              }));
            }))();
            return ({
              first: ($sd1$Types$TypedAst$ParameterRecycle)($pos, $expectedRaw, $name),
              second: $localEnv,
            });
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1081:4', (sp_toHuman)($par))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$addConstraint = (($env, $pos, $id, $uni, $state) => {
  const $eq = ({
    context: $sd1$Compiler$TypeCheck$Context_Global,
    id: $id,
    pos: $pos,
    uni: $uni,
    why: "-",
  });
  return ([
    ((__re__ = (array_push)($state.univarEqualities, $eq)), ($state.univarEqualities = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$uniCanBeCastTo = (($1) => {
  const $given = $1.given;
  const $required = $1.required;
  const $2 = ({
    first: $given,
    second: $required,
  });
  return ((($2.second)[0] === "Imm")
    ? $sd1$Compiler$TypeCheck$CanBeCastYes
    : ((($2.first)[0] === "Uni")
      ? $sd1$Compiler$TypeCheck$CanBeCastYes
      : (((($2.first)[0] === "Imm") && (($2.second)[0] === "Uni"))
        ? ($sd1$Compiler$TypeCheck$CanBeCastNo)($core$Core$Nil)
        : (((($2.first)[0] === "Depends") && (($2.second)[0] === "Uni"))
          ? ((() => {
            const $a = ($2.first)[1];
            return ($sd1$Compiler$TypeCheck$CanBeCastNo)(($core$Core$Cons)(({
              first: $a,
              second: $sd1$Types$Ast$Uni,
            }), $core$Core$Nil));
          }))()
          : (((($2.first)[0] === "Depends") && (($2.second)[0] === "Depends"))
            ? ((() => {
              const $a = ($2.first)[1];
              const $b = ($2.second)[1];
              return ((sp_equal)($a, $b)
                ? $sd1$Compiler$TypeCheck$CanBeCastYes
                : ($sd1$Compiler$TypeCheck$CanBeCastNo)(($core$Core$Cons)(({
                  first: $a,
                  second: ($sd1$Types$Ast$Depends)($b),
                }), ($core$Core$Cons)(({
                  first: $b,
                  second: ($sd1$Types$Ast$Depends)($a),
                }), $core$Core$Nil))));
            }))()
            : (((($2.first)[0] === "Imm") && (($2.second)[0] === "Depends"))
              ? ((() => {
                const $b = ($2.second)[1];
                return ($sd1$Compiler$TypeCheck$CanBeCastNo)(($core$Core$Cons)(({
                  first: $b,
                  second: $sd1$Types$Ast$Imm,
                }), $core$Core$Nil));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 498:4', (sp_toHuman)($2))))))));
});

const $sd1$Compiler$TypeCheck$checkUni = (($env, $pos, $2, $state) => {
  const $fix = $2.fix;
  const $mut = $2.mut;
  const $5 = ($sd1$Compiler$TypeCheck$uniCanBeCastTo)(({
    given: $mut,
    required: $fix,
  }));
  return ([
    ((($5)[0] === "CanBeCastYes")
      ? null
      : (((($5)[0] === "CanBeCastNo") && ((($5)[1])[0] === "Nil"))
        ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorUniquenessDoesNotMatch)(({
          fix: $fix,
          mut: $mut,
        })), $state)), ($state = (__re__)[1]), (__re__)[0])
        : (((($5)[0] === "CanBeCastNo") && ((($5)[1])[0] === "Cons"))
          ? ((() => {
            const $univarId = (($5)[1])[1].first;
            const $uni = (($5)[1])[1].second;
            const $tail = (($5)[1])[2];
            return ((__re__ = ($sd1$Compiler$TypeCheck$addConstraint)($env, $pos, $univarId, $uni, $state)), ($state = (__re__)[1]), (__re__)[0]);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 528:4', (sp_toHuman)($5))))),
    $state,
  ]);
});

const $sd1$Compiler$CoreTypes$boolDef = ((() => {
  const $usr = ($sd1$Compiler$CoreTypes$makeUsr)("Bool");
  return ({
    constructors: ((($2) => {
      return ($core$Dict$insert)("False", ({
        ins: $core$Core$Nil,
        out: $sd1$Compiler$CoreTypes$bool,
        pos: $sd1$Compiler$CoreTypes$p,
        typeUsr: $usr,
      }), $2);
    }))(((($2) => {
      return ($core$Dict$insert)("True", ({
        ins: $core$Core$Nil,
        out: $sd1$Compiler$CoreTypes$bool,
        pos: $sd1$Compiler$CoreTypes$p,
        typeUsr: $usr,
      }), $2);
    }))($core$Dict$empty)),
    directTypeDeps: $core$Set$empty,
    pars: $core$Core$Nil,
    usr: $usr,
  });
}))();

const $sd1$Compiler$TypeCheck$coreTypeBool = ($sd1$Types$TypedAst$TypeExact)($sd1$Compiler$CoreTypes$boolDef.usr, $core$Core$Nil);

const $sd1$Compiler$TypeCheck$getVariableByRef = (($ref, $env) => {
  return ($core$Dict$get)($ref, $env.variables);
});

const $sd1$Compiler$TypeCheck$inferUni = (($a, $b) => {
  const $3 = ({
    first: $a,
    second: $b,
  });
  return ((($3.first)[0] === "Imm")
    ? $sd1$Types$Ast$Imm
    : ((($3.second)[0] === "Imm")
      ? $sd1$Types$Ast$Imm
      : (((($3.first)[0] === "Depends") && (($3.second)[0] === "Depends"))
        ? ((() => {
          const $aId = ($3.first)[1];
          const $bId = ($3.second)[1];
          return $sd1$Types$Ast$Imm;
        }))()
        : ((($3.second)[0] === "Depends")
          ? $b
          : (true
            ? $sd1$Types$Ast$Uni
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 579:4', (sp_toHuman)($3)))))));
});

const $sd1$Types$CanonicalAst$expressionPos = (($exp) => {
  return ((($exp)[0] === "LiteralNumber")
    ? ((() => {
      const $p = ($exp)[1];
      return $p;
    }))()
    : ((($exp)[0] === "LiteralText")
      ? ((() => {
        const $p = ($exp)[1];
        return $p;
      }))()
      : ((($exp)[0] === "Variable")
        ? ((() => {
          const $p = ($exp)[1];
          return $p;
        }))()
        : ((($exp)[0] === "Constructor")
          ? ((() => {
            const $p = ($exp)[1];
            return $p;
          }))()
          : ((($exp)[0] === "Fn")
            ? ((() => {
              const $p = ($exp)[1];
              return $p;
            }))()
            : ((($exp)[0] === "Call")
              ? ((() => {
                const $p = ($exp)[1];
                return $p;
              }))()
              : ((($exp)[0] === "Record")
                ? ((() => {
                  const $p = ($exp)[1];
                  return $p;
                }))()
                : ((($exp)[0] === "RecordAccess")
                  ? ((() => {
                    const $p = ($exp)[1];
                    return $p;
                  }))()
                  : ((($exp)[0] === "LetIn")
                    ? ((() => {
                      const $def = ($exp)[1];
                      const $e = ($exp)[2];
                      return ($sd1$Types$CanonicalAst$expressionPos)($e);
                    }))()
                    : ((($exp)[0] === "If")
                      ? ((() => {
                        const $p = ($exp)[1];
                        return $p;
                      }))()
                      : ((($exp)[0] === "Try")
                        ? ((() => {
                          const $p = ($exp)[1];
                          return $p;
                        }))()
                        : ((($exp)[0] === "DestroyIn")
                          ? $sd1$Types$Pos$G
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 227:4', (sp_toHuman)($exp))))))))))))));
});

const $sd1$Compiler$TypeCheck$doTry = (($env, $pos, $expectedRaw, $value, $caPatternsAndExpressions, $state) => {
  const $7 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $value, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $valueType = $7.second;
  const $typedValue = $7.first;
  const $8 = ((($0) => {
    return ($core$List$forReversed)($0, $caPatternsAndExpressions, (($11, $10) => {
      const $u = $11.first;
      const $pa = $11.second;
      const $exp = $11.third;
      const $uniX = $10.first;
      const $acc = $10.second;
      const $patternOut = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($env, $u, $pa, $state)), ($state = (__re__)[1]), (__re__)[0]);
      ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_TryPattern, $patternOut.patternType, $valueType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
      ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($env, $pos, ({
        fix: $u,
        mut: $valueType.uni,
      }), $state)), ($state = (__re__)[1]), (__re__)[0]);
      const $newEnv = ((() => {
        const $0 = $patternOut.env;
        return (Object.assign)({}, $0, ({
          context: $sd1$Compiler$TypeCheck$Context_TryBranch,
        }));
      }))();
      const $12 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($newEnv, $exp, $state)), ($state = (__re__)[1]), (__re__)[0]);
      const $expressionType = $12.second;
      const $typedExpression = $12.first;
      ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($newEnv, ($sd1$Types$CanonicalAst$expressionPos)($exp), $sd1$Compiler$TypeCheck$Why_TryExpression, $expectedRaw, $expressionType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
      const $uf = ($sd1$Compiler$TypeCheck$inferUni)($uniX, $expressionType.uni);
      const $l = (sp_cons)(({
        first: $patternOut.typedPattern,
        second: $typedExpression,
      }), $acc);
      return ({
        first: $uf,
        second: $l,
      });
    }));
  }))(({
    first: $sd1$Types$Ast$Uni,
    second: $core$Core$Nil,
  }));
  const $patternsAndExpressions = $8.second;
  const $uni = $8.first;
  return ([
    ({
      first: ($sd1$Types$TypedAst$Try)($pos, ({
        patternsAndExpressions: $patternsAndExpressions,
        value: $typedValue,
        valueType: $valueType,
      })),
      second: ({
        raw: $expectedRaw,
        uni: $uni,
      }),
    }),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferParam = (($env, $parIndex, $par, $state) => {
  return ([
    ((($par)[0] === "ParameterRecycle")
      ? ((() => {
        const $pos = ($par)[1];
        const $name = ($par)[2];
        const $raw = ($sd1$Types$TypedAst$TypeVar)(((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]));
        const $instance = ({
          definedAt: $pos,
          freeTyvars: $core$Dict$empty,
          freeUnivars: $core$Dict$empty,
          type: ({
            raw: $raw,
            uni: $sd1$Types$Ast$Uni,
          }),
        });
        const $newEnv = ((() => {
          const $0 = $env;
          return (Object.assign)({}, $0, ({
            variables: ($core$Dict$insert)(($sd1$Types$Ast$RefLocal)($name), $instance, $0.variables),
          }));
        }))();
        return ({
          first: ($sd1$Types$TypedAst$ParameterRecycle)($pos, $raw, $name),
          second: ($sd1$Types$TypedAst$ParRe)($raw),
          third: $newEnv,
        });
      }))()
      : ((($par)[0] === "ParameterPattern")
        ? ((() => {
          const $uni = ($par)[1];
          const $pa = ($par)[2];
          const $out = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)($env, $uni, $pa, $state)), ($state = (__re__)[1]), (__re__)[0]);
          const $full = ({
            raw: $out.patternType,
            uni: $uni,
          });
          return ({
            first: ($sd1$Types$TypedAst$ParameterPattern)($full, $out.typedPattern),
            second: ($sd1$Types$TypedAst$ParSp)($full),
            third: $out.env,
          });
        }))()
        : ((($par)[0] === "ParameterPlaceholder")
          ? ((() => {
            const $name = ($par)[1];
            const $num = ($par)[2];
            const $univarId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
            const $raw = ((__re__ = ($sd1$Compiler$TypeCheck$newRawType)($state)), ($state = (__re__)[1]), (__re__)[0]);
            const $type = ({
              raw: $raw,
              uni: ($sd1$Types$Ast$Depends)($univarId),
            });
            const $instance = ({
              definedAt: $sd1$Types$Pos$G,
              freeTyvars: $core$Dict$empty,
              freeUnivars: $core$Dict$empty,
              type: $type,
            });
            const $newEnv = ((() => {
              const $0 = $env;
              return (Object.assign)({}, $0, ({
                variables: ($core$Dict$insert)(($sd1$Types$Ast$RefLocal)($name), $instance, $0.variables),
              }));
            }))();
            const $pa = ($sd1$Types$TypedAst$PatternAny)($sd1$Types$Pos$G, ({
              maybeName: ($core$Maybe$Just)($name),
              type: $type,
            }));
            return ({
              first: ($sd1$Types$TypedAst$ParameterPattern)($type, $pa),
              second: ($sd1$Types$TypedAst$ParSp)($type),
              third: $newEnv,
            });
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 865:4', (sp_toHuman)($par))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferFn = (($env, $pos, $caPars, $body, $state) => {
  let $typedPars = (array_fromList)($core$Core$Nil);
  let $parTypes = (array_fromList)($core$Core$Nil);
  let $parIndex = 0;
  const $newEnv = ($core$List$for)($env, $caPars, (($par, $envX) => {
    const $8 = ((__re__ = ($sd1$Compiler$TypeCheck$inferParam)($envX, ((__re__ = (basics_cloneUni)($parIndex)), ($parIndex = (__re__)[1]), (__re__)[0]), $par, $state)), ($state = (__re__)[1]), (__re__)[0]);
    const $envX1 = $8.third;
    const $parType = $8.second;
    const $typedPar = $8.first;
    ((__re__ = (array_push)($typedPars, $typedPar)), ($typedPars = (__re__)[1]), (__re__)[0]);
    ((__re__ = (array_push)($parTypes, $parType)), ($parTypes = (__re__)[1]), (__re__)[0]);
    ($parIndex += 1);
    return $envX1;
  }));
  const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)(((() => {
    const $0 = $newEnv;
    return (Object.assign)({}, $0, ({
      context: ($sd1$Compiler$TypeCheck$Context_FnBody)($pos, $env.context),
    }));
  }))(), $body, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $bodyType = $6.second;
  const $typedBody = $6.first;
  const $type = ($sd1$Types$TypedAst$TypeFn)(((__re__ = (array_toList)($parTypes)), ($parTypes = (__re__)[1]), (__re__)[0]), $bodyType);
  const $exp = ($sd1$Types$TypedAst$Fn)($pos, ((__re__ = (array_toList)($typedPars)), ($typedPars = (__re__)[1]), (__re__)[0]), $typedBody, $bodyType);
  return ([
    ({
      first: $exp,
      second: ({
        raw: $type,
        uni: $sd1$Types$Ast$Uni,
      }),
    }),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferRecord = (($env, $pos, $maybeExt, $caAttrs, $state) => {
  const $taAttrs = ($core$Dict$map)((($name, $value) => {
    return ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)(((() => {
      const $0 = $env;
      return (Object.assign)({}, $0, ({
        context: ($sd1$Compiler$TypeCheck$Context_Argument)($name, $0.context),
      }));
    }))(), $value, $state)), ($state = (__re__)[1]), (__re__)[0]);
  }), $caAttrs);
  const $typedAttrs = ($core$Dict$map)((($k, $v) => {
    return ($core$Tuple$first)($v);
  }), $taAttrs);
  const $attrTypes = ($core$Dict$map)((($k, $6) => {
    const $t = $6.second;
    return $t.raw;
  }), $taAttrs);
  const $uni = ((($0) => {
    return ($core$Dict$for)($0, $taAttrs, (($k, $8, $u) => {
      const $full = $8.second;
      return ($sd1$Compiler$TypeCheck$inferUni)($full.uni, $u);
    }));
  }))($sd1$Types$Ast$Uni);
  return ([
    ((($maybeExt)[0] === "Nothing")
      ? ({
        first: ($sd1$Types$TypedAst$Record)($pos, $core$Maybe$Nothing, $typedAttrs),
        second: ({
          raw: ($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, $attrTypes),
          uni: $uni,
        }),
      })
      : ((($maybeExt)[0] === "Just")
        ? ((() => {
          const $caExt = ($maybeExt)[1];
          const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $caExt, $state)), ($state = (__re__)[1]), (__re__)[0]);
          const $extType = $6.second;
          const $typedExt = $6.first;
          const $finalType = ((() => {
            const $7 = $extType.raw;
            return (((($7)[0] === "TypeRecord") && ((($7)[1])[0] === "Nothing"))
              ? ((() => {
                const $fixedTypes = ($7)[2];
                ($core$Dict$each)($attrTypes, (($name, $valueType) => {
                  const $10 = ($core$Dict$get)($name, $fixedTypes);
                  return ((($10)[0] === "Nothing")
                    ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorRecordDoesNotHaveAttribute)($name), $state)), ($state = (__re__)[1]), (__re__)[0])
                    : ((($10)[0] === "Just")
                      ? ((() => {
                        const $ty = ($10)[1];
                        return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_Record, $ty, $valueType, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1034:28', (sp_toHuman)($10))));
                }));
                return $extType.raw;
              }))()
              : (((($7)[0] === "TypeRecord") && ((($7)[1])[0] === "Just"))
                ? ((() => {
                  const $tyvarId = (($7)[1])[1];
                  const $extensionAttrTypes = ($7)[2];
                  const $8 = ($core$Dict$onlyBothOnly)($attrTypes, $extensionAttrTypes);
                  const $extensionOnly = $8.third;
                  const $both = $8.second;
                  const $expressionOnly = $8.first;
                  ($core$Dict$each)($both, (($name, $9) => {
                    const $inAttr = $9.first;
                    const $extAttr = $9.second;
                    return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_Record, $inAttr, $extAttr, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  }));
                  const $newExtId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
                  return ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($newExtId), ($core$Dict$join)($attrTypes, $extensionOnly));
                }))()
                : ((($7)[0] === "TypeVar")
                  ? ((() => {
                    const $id = ($7)[1];
                    const $ty = ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0])), $attrTypes);
                    ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_RecordExt, $extType.raw, $ty, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return $ty;
                  }))()
                  : (true
                    ? ((() => {
                      ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorNotCompatibleWithRecord, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      return $sd1$Types$TypedAst$TypeError;
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1031:16', (sp_toHuman)($7))))));
          }))();
          return ({
            first: ($sd1$Types$TypedAst$Record)($pos, ($core$Maybe$Just)($typedExt), $typedAttrs),
            second: ({
              raw: $finalType,
              uni: ($sd1$Compiler$TypeCheck$inferUni)($uni, $extType.uni),
            }),
          });
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1021:4', (sp_toHuman)($maybeExt)))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferRecordAccess = (($env, $pos, $attrName, $inferredType, $state) => {
  return ([
    (((($inferredType)[0] === "TypeRecord") && ((($inferredType)[1])[0] === "Nothing"))
      ? ((() => {
        const $attrTypes = ($inferredType)[2];
        const $6 = ($core$Dict$get)($attrName, $attrTypes);
        return ((($6)[0] === "Just")
          ? ((() => {
            const $type = ($6)[1];
            return $type;
          }))()
          : ((($6)[0] === "Nothing")
            ? ((() => {
              ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorRecordDoesNotHaveAttribute)($attrName), $state)), ($state = (__re__)[1]), (__re__)[0]);
              return $sd1$Types$TypedAst$TypeError;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 958:12', (sp_toHuman)($6))));
      }))()
      : (((($inferredType)[0] === "TypeRecord") && ((($inferredType)[1])[0] === "Just"))
        ? ((() => {
          const $tyvarId = (($inferredType)[1])[1];
          const $extensionAttrTypes = ($inferredType)[2];
          const $6 = ($core$Dict$get)($attrName, $extensionAttrTypes);
          return ((($6)[0] === "Just")
            ? ((() => {
              const $type = ($6)[1];
              return $type;
            }))()
            : ((($6)[0] === "Nothing")
              ? ((() => {
                const $newExtId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
                const $newAttrType = ((__re__ = ($sd1$Compiler$TypeCheck$newRawType)($state)), ($state = (__re__)[1]), (__re__)[0]);
                const $type = ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($newExtId), ($core$Dict$insert)($attrName, $newAttrType, $extensionAttrTypes));
                ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_RecordAccess, ($sd1$Types$TypedAst$TypeVar)($tyvarId), $type, $state)), ($state = (__re__)[1]), (__re__)[0]);
                return $newAttrType;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 967:12', (sp_toHuman)($6))));
        }))()
        : ((($inferredType)[0] === "TypeVar")
          ? ((() => {
            const $id = ($inferredType)[1];
            const $newExtId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
            const $newAttrType = ($sd1$Types$TypedAst$TypeVar)(((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]));
            const $type = ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($newExtId), ($core$Dict$ofOne)($attrName, $newAttrType));
            ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_RecordAccess, $inferredType, $type, $state)), ($state = (__re__)[1]), (__re__)[0]);
            return $newAttrType;
          }))()
          : (true
            ? ((() => {
              ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorTryingToAccessAttributeOfNonRecord)($attrName, $inferredType), $state)), ($state = (__re__)[1]), (__re__)[0]);
              return $sd1$Types$TypedAst$TypeError;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 956:4', (sp_toHuman)($inferredType)))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferExpression = (($env, $caExpression, $state) => {
  return ([
    ((($caExpression)[0] === "LiteralNumber")
      ? ((() => {
        const $pos = ($caExpression)[1];
        const $n = ($caExpression)[2];
        return ({
          first: ($sd1$Types$TypedAst$LiteralNumber)($pos, $n),
          second: ({
            raw: $sd1$Compiler$TypeCheck$coreTypeNumber,
            uni: $sd1$Types$Ast$Uni,
          }),
        });
      }))()
      : ((($caExpression)[0] === "LiteralText")
        ? ((() => {
          const $pos = ($caExpression)[1];
          const $text = ($caExpression)[2];
          return ({
            first: ($sd1$Types$TypedAst$LiteralText)($pos, $text),
            second: ({
              raw: $sd1$Compiler$TypeCheck$coreTypeText,
              uni: $sd1$Types$Ast$Uni,
            }),
          });
        }))()
        : ((($caExpression)[0] === "Variable")
          ? ((() => {
            const $pos = ($caExpression)[1];
            const $ref = ($caExpression)[2];
            const $ty = ((() => {
              const $4 = ($sd1$Compiler$TypeCheck$getVariableByRef)($ref, $env);
              return ((($4)[0] === "Nothing")
                ? ((() => {
                  ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorVariableNotFound)($ref), $state)), ($state = (__re__)[1]), (__re__)[0]);
                  return $sd1$Compiler$TypeCheck$fullTypeError;
                }))()
                : ((($4)[0] === "Just")
                  ? ((() => {
                    const $var = ($4)[1];
                    const $t = ((__re__ = ($sd1$Compiler$TypeCheck$generalize)($env, $pos, $ref, $var, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return $t;
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 728:16', (sp_toHuman)($4))));
            }))();
            return ({
              first: ($sd1$Types$TypedAst$Variable)($pos, $ref),
              second: $ty,
            });
          }))()
          : ((($caExpression)[0] === "Constructor")
            ? ((() => {
              const $pos = ($caExpression)[1];
              const $usr = ($caExpression)[2];
              const $ty = ((() => {
                const $4 = ($sd1$Compiler$TypeCheck$getConstructorByUsr)($usr, $env);
                return ((($4)[0] === "Nothing")
                  ? ((() => {
                    ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorConstructorNotFound)($usr), $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return $sd1$Compiler$TypeCheck$fullTypeError;
                  }))()
                  : ((($4)[0] === "Just")
                    ? ((() => {
                      const $cons = ($4)[1];
                      return ((__re__ = ($sd1$Compiler$TypeCheck$generalize)($env, $pos, ($sd1$Types$Ast$RefGlobal)($usr), $cons, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 744:16', (sp_toHuman)($4))));
              }))();
              return ({
                first: ($sd1$Types$TypedAst$Constructor)($pos, $usr),
                second: ((() => {
                  const $0 = $ty;
                  return (Object.assign)({}, $0, ({
                    uni: $sd1$Types$Ast$Uni,
                  }));
                }))(),
              });
            }))()
            : ((($caExpression)[0] === "Fn")
              ? ((() => {
                const $pos = ($caExpression)[1];
                const $caPars = ($caExpression)[2];
                const $body = ($caExpression)[3];
                return ((__re__ = ($sd1$Compiler$TypeCheck$inferFn)($env, $pos, $caPars, $body, $state)), ($state = (__re__)[1]), (__re__)[0]);
              }))()
              : ((($caExpression)[0] === "Call")
                ? ((() => {
                  const $pos = ($caExpression)[1];
                  const $reference = ($caExpression)[2];
                  const $args = ($caExpression)[3];
                  return ((__re__ = ($sd1$Compiler$TypeCheck$doCall)($env, $pos, $core$Maybe$Nothing, $reference, $args, $state)), ($state = (__re__)[1]), (__re__)[0]);
                }))()
                : ((($caExpression)[0] === "Record")
                  ? ((() => {
                    const $pos = ($caExpression)[1];
                    const $maybeExt = ($caExpression)[2];
                    const $attrs = ($caExpression)[3];
                    return ((__re__ = ($sd1$Compiler$TypeCheck$inferRecord)($env, $pos, $maybeExt, $attrs, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  }))()
                  : ((($caExpression)[0] === "RecordAccess")
                    ? ((() => {
                      const $pos = ($caExpression)[1];
                      const $attrName = ($caExpression)[2];
                      const $recordExpression = ($caExpression)[3];
                      const $4 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $recordExpression, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      const $inferredType = $4.second;
                      const $typedExpr = $4.first;
                      return ({
                        first: ($sd1$Types$TypedAst$RecordAccess)($pos, $attrName, $typedExpr),
                        second: ((() => {
                          const $0 = $inferredType;
                          return (Object.assign)({}, $0, ({
                            raw: ((__re__ = ($sd1$Compiler$TypeCheck$inferRecordAccess)($env, $pos, $attrName, $0.raw, $state)), ($state = (__re__)[1]), (__re__)[0]),
                          }));
                        }))(),
                      });
                    }))()
                    : ((($caExpression)[0] === "LetIn")
                      ? ((() => {
                        const $def = ($caExpression)[1];
                        const $rest = ($caExpression)[2];
                        const $4 = ((__re__ = ($sd1$Compiler$TypeCheck$doDefinition)($sd1$Types$Ast$RefLocal, $env, $def, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        const $defEnv = $4.second;
                        const $typedDef = $4.first;
                        const $5 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($defEnv, $rest, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        const $restType = $5.second;
                        const $typedRest = $5.first;
                        return ({
                          first: ($sd1$Types$TypedAst$LetIn)($typedDef, $typedRest, $restType),
                          second: $restType,
                        });
                      }))()
                      : ((($caExpression)[0] === "If")
                        ? ((() => {
                          const $pos = ($caExpression)[1];
                          const $condition = ($caExpression)[2].condition;
                          const $false = ($caExpression)[2].false;
                          const $true = ($caExpression)[2].true;
                          const $4 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)(((() => {
                            const $0 = $env;
                            return (Object.assign)({}, $0, ({
                              context: $sd1$Compiler$TypeCheck$Context_IfCondition,
                            }));
                          }))(), $condition, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          const $conditionType = $4.second;
                          const $typedCondition = $4.first;
                          ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_IfCondition, $sd1$Compiler$TypeCheck$coreTypeBool, $conditionType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          const $5 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)(((() => {
                            const $0 = $env;
                            return (Object.assign)({}, $0, ({
                              context: $sd1$Compiler$TypeCheck$Context_IfTrue,
                            }));
                          }))(), $true, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          const $trueType = $5.second;
                          const $typedTrue = $5.first;
                          const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)(((() => {
                            const $0 = $env;
                            return (Object.assign)({}, $0, ({
                              context: $sd1$Compiler$TypeCheck$Context_IfFalse,
                            }));
                          }))(), $false, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          const $falseType = $6.second;
                          const $typedFalse = $6.first;
                          ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_IfBranches, $trueType.raw, $falseType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          const $expression = ($sd1$Types$TypedAst$If)($pos, ({
                            condition: $typedCondition,
                            false: $typedFalse,
                            true: $typedTrue,
                          }));
                          const $uni = ($sd1$Compiler$TypeCheck$inferUni)($trueType.uni, $falseType.uni);
                          return ({
                            first: $expression,
                            second: ({
                              raw: $trueType.raw,
                              uni: $uni,
                            }),
                          });
                        }))()
                        : ((($caExpression)[0] === "Try")
                          ? ((() => {
                            const $pos = ($caExpression)[1];
                            const $patternsAndExpressions = ($caExpression)[2].patternsAndExpressions;
                            const $value = ($caExpression)[2].value;
                            return ((__re__ = ($sd1$Compiler$TypeCheck$doTry)($env, $pos, ((__re__ = ($sd1$Compiler$TypeCheck$newRawType)($state)), ($state = (__re__)[1]), (__re__)[0]), $value, $patternsAndExpressions, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          }))()
                          : ((($caExpression)[0] === "DestroyIn")
                            ? ((() => {
                              const $name = ($caExpression)[1];
                              const $expression = ($caExpression)[2];
                              const $4 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $expression, $state)), ($state = (__re__)[1]), (__re__)[0]);
                              const $expressionType = $4.second;
                              const $typedExpression = $4.first;
                              return ({
                                first: ($sd1$Types$TypedAst$DestroyIn)($name, $typedExpression),
                                second: $expressionType,
                              });
                            }))()
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 715:4', (sp_toHuman)($caExpression)))))))))))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$inferArgument = (($env, $arg, $state) => {
  return ([
    ((($arg)[0] === "ArgumentExpression")
      ? ((() => {
        const $exp = ($arg)[1];
        const $4 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $exp, $state)), ($state = (__re__)[1]), (__re__)[0]);
        const $expType = $4.second;
        const $typedExp = $4.first;
        return ($sd1$Types$TypedAst$ArgumentExpression)($expType, $typedExp);
      }))()
      : ((($arg)[0] === "ArgumentRecycle")
        ? ((() => {
          const $pos = ($arg)[1];
          const $name = ($arg)[2];
          const $attrPath = ($arg)[3];
          const $ref = ($sd1$Types$Ast$RefLocal)($name);
          const $raw = ((() => {
            const $4 = ($sd1$Compiler$TypeCheck$getVariableByRef)($ref, $env);
            return ((($4)[0] === "Nothing")
              ? ((() => {
                ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorVariableNotFound)($ref), $state)), ($state = (__re__)[1]), (__re__)[0]);
                return $sd1$Types$TypedAst$TypeError;
              }))()
              : ((($4)[0] === "Just")
                ? ((() => {
                  const $var = ($4)[1];
                  return ((($0) => {
                    return ($core$List$for)($0, $attrPath, (($attrName, $tyAcc) => {
                      return ((__re__ = ($sd1$Compiler$TypeCheck$inferRecordAccess)($env, $pos, $attrName, $tyAcc, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    }));
                  }))($var.type.raw);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1439:16', (sp_toHuman)($4))));
          }))();
          return ($sd1$Types$TypedAst$ArgumentRecycle)($pos, $raw, $attrPath, $name);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1427:4', (sp_toHuman)($arg)))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$doCall = (($env, $pos, $maybeExpectedType, $reference, $givenArgs, $state) => {
  const $7 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $reference, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $inferredReferenceType = $7.second;
  const $typedReference = $7.first;
  const $typedArguments = ((($1) => {
    return ($core$List$map)((($arg) => {
      return ((__re__ = ($sd1$Compiler$TypeCheck$inferArgument)($env, $arg, $state)), ($state = (__re__)[1]), (__re__)[0]);
    }), $1);
  }))($givenArgs);
  const $toTypeArg = (($arg) => {
    return ((($arg)[0] === "ArgumentExpression")
      ? ((() => {
        const $full = ($arg)[1];
        return ($sd1$Types$TypedAst$ParSp)($full);
      }))()
      : ((($arg)[0] === "ArgumentRecycle")
        ? ((() => {
          const $raw = ($arg)[2];
          return ($sd1$Types$TypedAst$ParRe)($raw);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1360:8', (sp_toHuman)($arg))));
  });
  const $expectedReturnType = ((() => {
    const $8 = $inferredReferenceType.raw;
    return ((($8)[0] === "TypeFn")
      ? ((() => {
        const $parTypes = ($8)[1];
        const $outType = ($8)[2];
        const $given = ($core$List$length)($typedArguments);
        const $expected = ($core$List$length)($parTypes);
        return ((sp_not_equal)($expected, $given)
          ? ((() => {
            ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorWrongNumberOfArguments)(({
              expected: $expected,
              given: $given,
              reference: $reference,
            })), $state)), ($state = (__re__)[1]), (__re__)[0]);
            return $sd1$Compiler$TypeCheck$fullTypeError;
          }))()
          : ((() => {
            ($sd1$Compiler$TypeCheck$list_eachWithIndex2)(0, $typedArguments, $parTypes, (($index, $givenArg, $parType) => {
              const $12 = ({
                first: $givenArg,
                second: $parType,
              });
              return (((($12.first)[0] === "ArgumentRecycle") && (($12.second)[0] === "ParRe"))
                ? ((() => {
                  const $p = ($12.first)[1];
                  const $givenRaw = ($12.first)[2];
                  const $attrPath = ($12.first)[3];
                  const $name = ($12.first)[4];
                  const $inferredRaw = ($12.second)[1];
                  const $13 = ($sd1$Compiler$TypeCheck$getVariableByRef)(($sd1$Types$Ast$RefLocal)($name), $env);
                  return ((($13)[0] === "Nothing")
                    ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $p, ($sd1$Compiler$TypeCheck$ErrorVariableNotFound)(($sd1$Types$Ast$RefLocal)($name)), $state)), ($state = (__re__)[1]), (__re__)[0])
                    : ((($13)[0] === "Just")
                      ? ((() => {
                        const $instance = ($13)[1];
                        ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($instance.type.uni, $sd1$Types$Ast$Uni), $env, $p, $sd1$Compiler$TypeCheck$ErrorShouldBeUnique, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, ($sd1$Compiler$TypeCheck$Why_Argument)($index), $givenRaw, $inferredRaw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1377:32', (sp_toHuman)($13))));
                }))()
                : (((($12.first)[0] === "ArgumentExpression") && (($12.second)[0] === "ParSp"))
                  ? ((() => {
                    const $givenFull = ($12.first)[1];
                    const $expr = ($12.first)[2];
                    const $inferredFull = ($12.second)[1];
                    ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($env, $pos, ({
                      fix: $inferredFull.uni,
                      mut: $givenFull.uni,
                    }), $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, ($sd1$Compiler$TypeCheck$Why_Argument)($index), $inferredFull.raw, $givenFull.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  }))()
                  : (true
                    ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorUniquenessDoesNotMatchArgument, $state)), ($state = (__re__)[1]), (__re__)[0])
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1375:24', (sp_toHuman)($12)))));
            }));
            return ((($maybeExpectedType)[0] === "Nothing")
              ? $outType
              : ((($maybeExpectedType)[0] === "Just")
                ? ((() => {
                  const $e = ($maybeExpectedType)[1];
                  ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($env, $pos, ({
                    fix: $e.uni,
                    mut: $outType.uni,
                  }), $state)), ($state = (__re__)[1]), (__re__)[0]);
                  ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_Annotation, $outType.raw, $e.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  return $e;
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1393:20', (sp_toHuman)($maybeExpectedType))));
          }))());
      }))()
      : ((($8)[0] === "TypeVar")
        ? ((() => {
          const $id = ($8)[1];
          const $returnType = ((($maybeExpectedType)[0] === "Just")
            ? ((() => {
              const $e = ($maybeExpectedType)[1];
              return $e;
            }))()
            : ((($maybeExpectedType)[0] === "Nothing")
              ? ({
                raw: ((__re__ = ($sd1$Compiler$TypeCheck$newRawType)($state)), ($state = (__re__)[1]), (__re__)[0]),
                uni: $sd1$Types$Ast$Imm,
              })
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1402:20', (sp_toHuman)($maybeExpectedType))));
          const $refTy = ($sd1$Types$TypedAst$TypeFn)(($core$List$map)($toTypeArg, $typedArguments), $returnType);
          ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_CalledAsFunction, $refTy, $inferredReferenceType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
          return $returnType;
        }))()
        : ((($8)[0] === "TypeError")
          ? $sd1$Compiler$TypeCheck$fullTypeError
          : (true
            ? ((() => {
              const $z = $8;
              ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorCallingANonFunction)($z), $state)), ($state = (__re__)[1]), (__re__)[0]);
              return $sd1$Compiler$TypeCheck$fullTypeError;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1365:8', (sp_toHuman)($8))))));
  }))();
  return ([
    ({
      first: ($sd1$Types$TypedAst$Call)($pos, $typedReference, $typedArguments),
      second: $expectedReturnType,
    }),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$checkExpression = (($env, $expectedType, $caExpression, $state) => {
  const $5 = ({
    first: $caExpression,
    second: $expectedType.raw,
  });
  return ([
    (((($5.first)[0] === "LiteralNumber") && ((($5.second)[0] === "TypeExact") && ((($5.second)[2])[0] === "Nil")))
      ? ((() => {
        const $pos = ($5.first)[1];
        const $n = ($5.first)[2];
        const $typeUsr = ($5.second)[1];
        ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($typeUsr, $sd1$Compiler$CoreTypes$numberDef.usr), $env, $pos, ($sd1$Compiler$TypeCheck$ErrorIncompatibleTypes)($caExpression, $expectedType), $state)), ($state = (__re__)[1]), (__re__)[0]);
        return ($sd1$Types$TypedAst$LiteralNumber)($pos, $n);
      }))()
      : (((($5.first)[0] === "LiteralText") && ((($5.second)[0] === "TypeExact") && ((($5.second)[2])[0] === "Nil")))
        ? ((() => {
          const $pos = ($5.first)[1];
          const $text = ($5.first)[2];
          const $typeUsr = ($5.second)[1];
          ((__re__ = ($sd1$Compiler$TypeCheck$addErrorIf)((sp_not_equal)($typeUsr, $sd1$Compiler$CoreTypes$textDef.usr), $env, $pos, ($sd1$Compiler$TypeCheck$ErrorIncompatibleTypes)($caExpression, $expectedType), $state)), ($state = (__re__)[1]), (__re__)[0]);
          return ($sd1$Types$TypedAst$LiteralText)($pos, $text);
        }))()
        : ((($5.first)[0] === "Variable")
          ? ((() => {
            const $pos = ($5.first)[1];
            const $ref = ($5.first)[2];
            const $__bleh__ = ((() => {
              const $6 = ($sd1$Compiler$TypeCheck$getVariableByRef)($ref, $env);
              return ((($6)[0] === "Nothing")
                ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorVariableNotFound)($ref), $state)), ($state = (__re__)[1]), (__re__)[0])
                : ((($6)[0] === "Just")
                  ? ((() => {
                    const $var = ($6)[1];
                    const $full = ((__re__ = ($sd1$Compiler$TypeCheck$generalize)($env, $pos, $ref, $var, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($env, $pos, ({
                      fix: $expectedType.uni,
                      mut: $full.uni,
                    }), $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_Annotation, $full.raw, $expectedType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1166:16', (sp_toHuman)($6))));
            }))();
            return ($sd1$Types$TypedAst$Variable)($pos, $ref);
          }))()
          : ((($5.first)[0] === "Constructor")
            ? ((() => {
              const $pos = ($5.first)[1];
              const $usr = ($5.first)[2];
              const $bleh = ((() => {
                const $6 = ($sd1$Compiler$TypeCheck$getConstructorByUsr)($usr, $env);
                return ((($6)[0] === "Nothing")
                  ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorConstructorNotFound)($usr), $state)), ($state = (__re__)[1]), (__re__)[0])
                  : ((($6)[0] === "Just")
                    ? ((() => {
                      const $cons = ($6)[1];
                      const $full = ((__re__ = ($sd1$Compiler$TypeCheck$generalize)($env, $pos, ($sd1$Types$Ast$RefGlobal)($usr), $cons, $state)), ($state = (__re__)[1]), (__re__)[0]);
                      return ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_Annotation, $full.raw, $expectedType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1180:16', (sp_toHuman)($6))));
              }))();
              return ($sd1$Types$TypedAst$Constructor)($pos, $usr);
            }))()
            : (((($5.first)[0] === "Fn") && (($5.second)[0] === "TypeFn"))
              ? ((() => {
                const $pos = ($5.first)[1];
                const $pars = ($5.first)[2];
                const $body = ($5.first)[3];
                const $parTypes = ($5.second)[1];
                const $out = ($5.second)[2];
                return ((sp_not_equal)(($core$List$length)($pars), ($core$List$length)($parTypes))
                  ? ((() => {
                    ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorWrongNumberOfParameters, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return ($sd1$Types$TypedAst$Error)($pos);
                  }))()
                  : ((() => {
                    let $typedPars = (array_fromList)($core$Core$Nil);
                    let $parIndex = 0;
                    const $localEnv = ((($0) => {
                      return ($core$List$for)($0, ($core$List$map2)($core$Tuple$pair, $pars, $parTypes), (($8, $envX) => {
                        const $par = $8.first;
                        const $parType = $8.second;
                        const $9 = ((__re__ = ($sd1$Compiler$TypeCheck$checkParameter)(((() => {
                          const $0 = $envX;
                          return (Object.assign)({}, $0, ({
                            context: ($sd1$Compiler$TypeCheck$Context_FnPar)(((__re__ = (basics_cloneUni)($parIndex)), ($parIndex = (__re__)[1]), (__re__)[0]), $0.context),
                          }));
                        }))(), $parType, $par, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        const $envX1 = $9.second;
                        const $typedPar = $9.first;
                        ((__re__ = (array_push)($typedPars, $typedPar)), ($typedPars = (__re__)[1]), (__re__)[0]);
                        ($parIndex += 1);
                        const $0 = $envX1;
                        return (Object.assign)({}, $0, ({
                          context: $envX.context,
                        }));
                      }));
                    }))($env);
                    const $typedBody = ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)($localEnv, $out, $body, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    return ($sd1$Types$TypedAst$Fn)($pos, ((__re__ = (array_toList)($typedPars)), ($typedPars = (__re__)[1]), (__re__)[0]), $typedBody, $out);
                  }))());
              }))()
              : ((($5.first)[0] === "Call")
                ? ((() => {
                  const $pos = ($5.first)[1];
                  const $reference = ($5.first)[2];
                  const $args = ($5.first)[3];
                  return ($core$Tuple$first)(((__re__ = ($sd1$Compiler$TypeCheck$doCall)($env, $pos, ($core$Maybe$Just)($expectedType), $reference, $args, $state)), ($state = (__re__)[1]), (__re__)[0]));
                }))()
                : (((($5.first)[0] === "Record") && (((($5.first)[2])[0] === "Just") && ((($5.second)[0] === "TypeRecord") && ((($5.second)[1])[0] === "Nothing"))))
                  ? ((() => {
                    const $pos = ($5.first)[1];
                    const $ext = (($5.first)[2])[1];
                    const $valueByName = ($5.first)[3];
                    const $typeByName = ($5.second)[2];
                    const $typedExt = ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)($env, $expectedType, $ext, $state)), ($state = (__re__)[1]), (__re__)[0]);
                    const $zzz = (($attrName, $attrExpr) => {
                      const $8 = ($core$Dict$get)($attrName, $typeByName);
                      return ((($8)[0] === "Nothing")
                        ? ((() => {
                          ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorRecordHasAttributesNotInAnnotation, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          return ($core$Tuple$first)(((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $attrExpr, $state)), ($state = (__re__)[1]), (__re__)[0]));
                        }))()
                        : ((($8)[0] === "Just")
                          ? ((() => {
                            const $attrType = ($8)[1];
                            const $fullAttrType = ({
                              raw: $attrType,
                              uni: $expectedType.uni,
                            });
                            return ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)(((() => {
                              const $0 = $env;
                              return (Object.assign)({}, $0, ({
                                context: ($sd1$Compiler$TypeCheck$Context_AttributeName)($attrName, $0.context),
                              }));
                            }))(), $fullAttrType, $attrExpr, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1237:20', (sp_toHuman)($8))));
                    });
                    const $typedValueByName = ($core$Dict$map)($zzz, $valueByName);
                    return ($sd1$Types$TypedAst$Record)($pos, ($core$Maybe$Just)($typedExt), $typedValueByName);
                  }))()
                  : (((($5.first)[0] === "Record") && (((($5.first)[2])[0] === "Nothing") && ((($5.second)[0] === "TypeRecord") && ((($5.second)[1])[0] === "Nothing"))))
                    ? ((() => {
                      const $pos = ($5.first)[1];
                      const $valueByName = ($5.first)[3];
                      const $typeByName = ($5.second)[2];
                      const $6 = ($core$Dict$onlyBothOnly)($valueByName, $typeByName);
                      const $bOnly = $6.third;
                      const $both = $6.second;
                      const $aOnly = $6.first;
                      ((sp_not_equal)($aOnly, $core$Dict$empty)
                        ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorRecordHasAttributesNotInAnnotation, $state)), ($state = (__re__)[1]), (__re__)[0])
                        : ((sp_not_equal)($bOnly, $core$Dict$empty)
                          ? ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, $sd1$Compiler$TypeCheck$ErrorRecordIsMissingAttibutesInAnnotation, $state)), ($state = (__re__)[1]), (__re__)[0])
                          : null));
                      const $typedAttrs = ((($1) => {
                        return ($core$Dict$map)((($name, $8) => {
                          const $value = $8.first;
                          const $type = $8.second;
                          return ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)($env, ({
                            raw: $type,
                            uni: $expectedType.uni,
                          }), $value, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        }), $1);
                      }))($both);
                      return ($sd1$Types$TypedAst$Record)($pos, $core$Maybe$Nothing, $typedAttrs);
                    }))()
                    : ((($5.first)[0] === "RecordAccess")
                      ? ((() => {
                        const $pos = ($5.first)[1];
                        const $attrName = ($5.first)[2];
                        const $exp = ($5.first)[3];
                        const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($env, $exp, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        const $expressionType = $6.second;
                        const $typedExpression = $6.first;
                        const $newId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
                        const $requiredType = ((($1) => {
                          return ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($newId), $1);
                        }))(((($1) => {
                          return ($core$Dict$ofOne)($attrName, $1);
                        }))($expectedType.raw));
                        ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_RecordAccess, $expressionType.raw, $requiredType, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($env, $pos, ({
                          fix: $expectedType.uni,
                          mut: $expressionType.uni,
                        }), $state)), ($state = (__re__)[1]), (__re__)[0]);
                        return ($sd1$Types$TypedAst$RecordAccess)($pos, $attrName, $typedExpression);
                      }))()
                      : ((($5.first)[0] === "LetIn")
                        ? ((() => {
                          const $def = ($5.first)[1];
                          const $rest = ($5.first)[2];
                          const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$doDefinition)($sd1$Types$Ast$RefLocal, $env, $def, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          const $defEnv = $6.second;
                          const $typedDef = $6.first;
                          const $typedRest = ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)($defEnv, $expectedType, $rest, $state)), ($state = (__re__)[1]), (__re__)[0]);
                          return ($sd1$Types$TypedAst$LetIn)($typedDef, $typedRest, $expectedType);
                        }))()
                        : ((($5.first)[0] === "If")
                          ? ((() => {
                            const $pos = ($5.first)[1];
                            const $condition = ($5.first)[2].condition;
                            const $false = ($5.first)[2].false;
                            const $true = ($5.first)[2].true;
                            const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)(((() => {
                              const $0 = $env;
                              return (Object.assign)({}, $0, ({
                                context: $sd1$Compiler$TypeCheck$Context_IfCondition,
                              }));
                            }))(), $condition, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            const $conditionType = $6.second;
                            const $typedCondition = $6.first;
                            ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($env, $pos, $sd1$Compiler$TypeCheck$Why_IfCondition, $sd1$Compiler$TypeCheck$coreTypeBool, $conditionType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            const $typedTrue = ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)(((() => {
                              const $0 = $env;
                              return (Object.assign)({}, $0, ({
                                context: $sd1$Compiler$TypeCheck$Context_IfTrue,
                              }));
                            }))(), $expectedType, $true, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            const $typedFalse = ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)(((() => {
                              const $0 = $env;
                              return (Object.assign)({}, $0, ({
                                context: $sd1$Compiler$TypeCheck$Context_IfFalse,
                              }));
                            }))(), $expectedType, $false, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            return ($sd1$Types$TypedAst$If)($pos, ({
                              condition: $typedCondition,
                              false: $typedFalse,
                              true: $typedTrue,
                            }));
                          }))()
                          : ((($5.first)[0] === "Try")
                            ? ((() => {
                              const $pos = ($5.first)[1];
                              const $patternsAndExpressions = ($5.first)[2].patternsAndExpressions;
                              const $value = ($5.first)[2].value;
                              const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$doTry)($env, $pos, $expectedType.raw, $value, $patternsAndExpressions, $state)), ($state = (__re__)[1]), (__re__)[0]);
                              const $fullType = $6.second;
                              const $typedExp = $6.first;
                              ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($env, $pos, ({
                                fix: $expectedType.uni,
                                mut: $fullType.uni,
                              }), $state)), ($state = (__re__)[1]), (__re__)[0]);
                              return $typedExp;
                            }))()
                            : ((($5.first)[0] === "DestroyIn")
                              ? ((() => {
                                const $name = ($5.first)[1];
                                const $exp = ($5.first)[2];
                                return ((($1) => {
                                  return ($sd1$Types$TypedAst$DestroyIn)($name, $1);
                                }))(((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)($env, $expectedType, $exp, $state)), ($state = (__re__)[1]), (__re__)[0]));
                              }))()
                              : ((($5.second)[0] === "TypeError")
                                ? ($sd1$Types$TypedAst$Error)(($sd1$Types$CanonicalAst$expressionPos)($caExpression))
                                : (true
                                  ? ((() => {
                                    const $pos = ($sd1$Types$CanonicalAst$expressionPos)($caExpression);
                                    ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $pos, ($sd1$Compiler$TypeCheck$ErrorIncompatibleTypes)($caExpression, $expectedType), $state)), ($state = (__re__)[1]), (__re__)[0]);
                                    return ($sd1$Types$TypedAst$Error)($pos);
                                  }))()
                                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1147:4', (sp_toHuman)($5))))))))))))))))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$doDefinition = (($nameToRef, $env, $def, $state) => {
  const $5 = ((($0) => {
    return ($core$Dict$for)($0, $def.univars, (($originalId, _1, $7) => {
      const $fus = $7.first;
      const $aus = $7.second;
      const $10 = ($core$Dict$get)($originalId, $env.annotatedUnivarsByOriginalId);
      return ((($10)[0] === "Just")
        ? ({
          first: $fus,
          second: $aus,
        })
        : ((($10)[0] === "Nothing")
          ? ((() => {
            const $univarId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
            const $univar = ({
              originalId: $originalId,
            });
            return ({
              first: ($core$Dict$insert)($univarId, $univar, $fus),
              second: ($core$Dict$insert)($originalId, $univarId, $aus),
            });
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 604:12', (sp_toHuman)($10))));
    }));
  }))(({
    first: $core$Dict$empty,
    second: $env.annotatedUnivarsByOriginalId,
  }));
  const $annotatedUnivarsByOriginalId = $5.second;
  const $freeUnivars = $5.first;
  const $6 = ((($0) => {
    return ($core$Dict$for)($0, $def.tyvars, (($tyvarName, $caTyvar, $8) => {
      const $ftById = $8.first;
      const $atByName = $8.second;
      const $11 = ($core$Dict$get)($tyvarName, $env.annotatedTyvarsByName);
      return ((($11)[0] === "Just")
        ? ({
          first: $ftById,
          second: $atByName,
        })
        : ((($11)[0] === "Nothing")
          ? ((() => {
            const $tyvarId = ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]);
            const $tyvar = ({
              allowFunctions: $caTyvar.allowFunctions,
              generalizedAt: $sd1$Types$Pos$G,
              generalizedFor: ($sd1$Types$Ast$RefLocal)(""),
              originalName: $tyvarName,
            });
            return ({
              first: ($core$Dict$insert)($tyvarId, $tyvar, $ftById),
              second: ($core$Dict$insert)($tyvarName, $tyvarId, $atByName),
            });
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 620:12', (sp_toHuman)($11))));
    }));
  }))(({
    first: $core$Dict$empty,
    second: $env.annotatedTyvarsByName,
  }));
  const $annotatedTyvarsByName = $6.second;
  const $freeTyvars = $6.first;
  const $uni = $def.uni;
  const $patternOut = ((__re__ = ($sd1$Compiler$TypeCheck$inferPattern)(((() => {
    const $0 = $env;
    return (Object.assign)({}, $0, ({
      annotatedTyvarsByName: $annotatedTyvarsByName,
      annotatedUnivarsByOriginalId: $annotatedUnivarsByOriginalId,
    }));
  }))(), $uni, $def.pattern, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $envWithContext = ((() => {
    const $0 = $patternOut.env;
    return (Object.assign)({}, $0, ({
      context: ($sd1$Compiler$TypeCheck$Context_LetInBody)(($core$Dict$keys)(($sd1$Types$TypedAst$patternNames)($patternOut.typedPattern))),
    }));
  }))();
  const $7 = ($def.native
    ? ({
      first: ($sd1$Types$TypedAst$LiteralText)($sd1$Types$Pos$N, "native"),
      second: ({
        raw: $patternOut.patternType,
        uni: $uni,
      }),
    })
    : ((() => {
      const $8 = $patternOut.maybeFullAnnotation;
      return ((($8)[0] === "Just")
        ? ((() => {
          const $annotationType = ($8)[1];
          const $raw = ((__re__ = ($sd1$Compiler$TypeCheck$translateAnnotation)($envWithContext, $state, $annotationType)), ($state = (__re__)[1]), (__re__)[0]);
          const $full = ({
            raw: $raw,
            uni: $uni,
          });
          return ({
            first: ((__re__ = ($sd1$Compiler$TypeCheck$checkExpression)($envWithContext, $full, $def.body, $state)), ($state = (__re__)[1]), (__re__)[0]),
            second: $full,
          });
        }))()
        : ((($8)[0] === "Nothing")
          ? ((() => {
            const $9 = ((__re__ = ($sd1$Compiler$TypeCheck$inferExpression)($envWithContext, $def.body, $state)), ($state = (__re__)[1]), (__re__)[0]);
            const $inferredType = $9.second;
            const $typed = $9.first;
            const $pos = ($sd1$Types$CanonicalAst$patternPos)($def.pattern);
            ((__re__ = ($sd1$Compiler$TypeCheck$addEquality)($envWithContext, $pos, $sd1$Compiler$TypeCheck$Why_LetIn, $patternOut.patternType, $inferredType.raw, $state)), ($state = (__re__)[1]), (__re__)[0]);
            ((__re__ = ($sd1$Compiler$TypeCheck$checkUni)($envWithContext, $pos, ({
              fix: $uni,
              mut: $inferredType.uni,
            }), $state)), ($state = (__re__)[1]), (__re__)[0]);
            return ({
              first: $typed,
              second: $inferredType,
            });
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 658:12', (sp_toHuman)($8))));
    }))());
  const $bodyType = $7.second;
  const $typedBody = $7.first;
  const $instance = (($name, $8) => {
    const $pos = $8.pos;
    const $type = $8.type;
    return ({
      definedAt: $pos,
      freeTyvars: $freeTyvars,
      freeUnivars: $freeUnivars,
      type: $type,
    });
  });
  const $type = ((() => {
    const $0 = $bodyType;
    return (Object.assign)({}, $0, ({
      uni: $def.uni,
    }));
  }))();
  return ([
    ({
      first: ({
        body: $typedBody,
        directValueDeps: $def.directValueDeps,
        freeTyvars: $freeTyvars,
        freeUnivars: $freeUnivars,
        isFullyAnnotated: (sp_not_equal)($patternOut.maybeFullAnnotation, $core$Maybe$Nothing),
        native: $def.native,
        pattern: $patternOut.typedPattern,
        type: $type,
      }),
      second: ((() => {
        const $0 = $patternOut.env;
        return (Object.assign)({}, $0, ({
          annotatedTyvarsByName: $env.annotatedTyvarsByName,
          annotatedUnivarsByOriginalId: $env.annotatedUnivarsByOriginalId,
          variables: ((($0) => {
            return ($core$Dict$for)($0, ($sd1$Types$TypedAst$patternNames)($patternOut.typedPattern), (($name, $stuff, $vars) => {
              return ($core$Dict$insert)(($nameToRef)($name), ($instance)($name, $stuff), $vars);
            }));
          }))($0.variables),
        }));
      }))(),
    }),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$initState = (($lastUnificationVarId) => {
  return ({
    equalities: (array_fromList)($core$Core$Nil),
    errors: (array_fromList)($core$Core$Nil),
    lastUnificationVarId: $lastUnificationVarId,
    tyvarsById: (hash_fromList)($core$Core$Nil),
    univarEqualities: (array_fromList)($core$Core$Nil),
    univarsById: (hash_fromList)($core$Core$Nil),
  });
});

const $sd1$Compiler$TypeCheck$makeInferenceAndCheckError = (($env, $1) => {
  const $pos = $1.first;
  const $context = $1.second;
  const $error = $1.third;
  return ($sd1$Compiler$Error$Simple)($env.errorModule, $pos, ($core$Core$Cons)((sp_toHuman)($error), ($core$Core$Cons)((sp_toHuman)($context), $core$Core$Nil)));
});

const $sd1$Human$Type$display = (($indent, $tree) => {
  return ((($tree)[0] === "Span")
    ? ((() => {
      const $content = ($tree)[2];
      return ($indent + $content);
    }))()
    : (((($tree)[0] === "Block") && ((($tree)[1])[0] === "NotIndented"))
      ? ((() => {
        const $content = ($tree)[2];
        return ((($1) => {
          return ($core$Text$join)("\n", $1);
        }))(((($1) => {
          return ($core$List$map)((($1) => {
            return ($sd1$Human$Type$display)($indent, $1);
          }), $1);
        }))($content));
      }))()
      : (((($tree)[0] === "Block") && ((($tree)[1])[0] === "IndentedWithHeader"))
        ? ((() => {
          const $header = (($tree)[1])[1];
          const $content = ($tree)[2];
          const $head = ($indent + $header);
          const $tail = ((($1) => {
            return ($core$List$map)((($1) => {
              return ($sd1$Human$Type$display)(($indent + "    "), $1);
            }), $1);
          }))($content);
          return ((($1) => {
            return ($core$Text$join)("\n", $1);
          }))((sp_cons)($head, $tail));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 45:4', (sp_toHuman)($tree)))));
});

const $sd1$Human$Type$maybeSpan = (($complexityAcc, $textAcc, $content) => {
  return (($complexityAcc > 20)
    ? $core$Maybe$Nothing
    : ((($content)[0] === "Nil")
      ? ($core$Maybe$Just)(((($1) => {
        return ($sd1$Human$Type$Span)($complexityAcc, $1);
      }))(((($1) => {
        return ($core$Text$join)("", $1);
      }))(($core$List$reverse)($textAcc))))
      : (((($content)[0] === "Cons") && ((($content)[1])[0] === "Span"))
        ? ((() => {
          const $complexity = (($content)[1])[1];
          const $snippet = (($content)[1])[2];
          const $tail = ($content)[2];
          return ($sd1$Human$Type$maybeSpan)(($complexity + $complexityAcc), (sp_cons)($snippet, $textAcc), $tail);
        }))()
        : (true
          ? $core$Maybe$Nothing
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 72:6', (sp_toHuman)($content))))));
});

const $sd1$Human$Type$text = (($text) => {
  return ($sd1$Human$Type$Span)((text_length)($text), $text);
});

const $sd1$Human$Type$rowOrIndented = (($header, $content) => {
  const $3 = ($sd1$Human$Type$maybeSpan)(0, $core$Core$Nil, (sp_cons)(($sd1$Human$Type$text)(($header + " ")), $content));
  return ((($3)[0] === "Just")
    ? ((() => {
      const $span = ($3)[1];
      return $span;
    }))()
    : ((($3)[0] === "Nothing")
      ? ($sd1$Human$Type$Block)(($sd1$Human$Type$IndentedWithHeader)($header), $content)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 116:4', (sp_toHuman)($3))));
});

const $sd1$Human$Type$uniToText = (($env, $uni) => {
  return ((($uni)[0] === "Imm")
    ? ""
    : ((($uni)[0] === "Uni")
      ? "!"
      : ((($uni)[0] === "Depends")
        ? ((() => {
          const $n = ($uni)[1];
          return ((text_fromNumber)($n) + "?");
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 196:4', (sp_toHuman)($uni)))));
});

const $sd1$Human$Type$doFullType = (($env, $1) => {
  const $raw = $1.raw;
  const $uni = $1.uni;
  return ($sd1$Human$Type$rowOrIndented)(($sd1$Human$Type$uniToText)($env, $uni), ($core$Core$Cons)(($sd1$Human$Type$doRawType)($env, $raw), $core$Core$Nil));
});

const $sd1$Human$Type$rowOrHead = $sd1$Human$Type$rowOrIndented;

const $sd1$Human$Type$doParType = (($env, $parType) => {
  return ((($parType)[0] === "ParSp")
    ? ((() => {
      const $full = ($parType)[1];
      return ($sd1$Human$Type$doFullType)($env, $full);
    }))()
    : ((($parType)[0] === "ParRe")
      ? ((() => {
        const $raw = ($parType)[1];
        return ($sd1$Human$Type$rowOrHead)("@", ($core$Core$Cons)(($sd1$Human$Type$doRawType)($env, $raw), $core$Core$Nil));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 285:4', (sp_toHuman)($parType))));
});

const $sd1$Human$Type$list_intersperse = (($separator, $items, $acc) => {
  return ((($items)[0] === "Nil")
    ? ($core$List$reverse)($acc)
    : (((($items)[0] === "Cons") && ((($items)[2])[0] === "Nil"))
      ? ((() => {
        const $last = ($items)[1];
        return ($core$List$reverse)((sp_cons)($last, $acc));
      }))()
      : ((($items)[0] === "Cons")
        ? ((() => {
          const $head = ($items)[1];
          const $tail = ($items)[2];
          return ($sd1$Human$Type$list_intersperse)($separator, $tail, (sp_cons)($separator, (sp_cons)($head, $acc)));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 8:4', (sp_toHuman)($items)))));
});

const $sd1$Human$Type$list = (($1) => {
  const $close = $1.close;
  const $items = $1.items;
  const $open = $1.open;
  const $separator = $1.separator;
  const $zero = ((text_length)($open) + (text_length)($close));
  const $2 = ($sd1$Human$Type$maybeSpan)($zero, ($core$Core$Cons)(($open + " "), $core$Core$Nil), ($sd1$Human$Type$list_intersperse)(($sd1$Human$Type$text)($separator), $items, $core$Core$Nil));
  return (((($2)[0] === "Just") && ((($2)[1])[0] === "Span"))
    ? ((() => {
      const $com = (($2)[1])[1];
      const $t = (($2)[1])[2];
      return ($sd1$Human$Type$Span)($com, ($t + $close));
    }))()
    : (true
      ? ((($1) => {
        return ($sd1$Human$Type$Block)($sd1$Human$Type$NotIndented, $1);
      }))(($core$List$concat)(($core$Core$Cons)(($core$Core$Cons)(($sd1$Human$Type$text)($open), $core$Core$Nil), ($core$Core$Cons)(((($1) => {
        return ($core$List$map)((($c) => {
          return ($sd1$Human$Type$rowOrHead)($separator, ($core$Core$Cons)($c, $core$Core$Nil));
        }), $1);
      }))($items), ($core$Core$Cons)(($core$Core$Cons)(($sd1$Human$Type$text)((" " + $close)), $core$Core$Nil), $core$Core$Nil)))))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 157:4', (sp_toHuman)($2))));
});

const $sd1$Human$Type$separatedBy = (($separator, $content) => {
  const $3 = ($sd1$Human$Type$maybeSpan)(0, $core$Core$Nil, ($sd1$Human$Type$list_intersperse)(($sd1$Human$Type$text)($separator), $content, $core$Core$Nil));
  return ((($3)[0] === "Just")
    ? ((() => {
      const $span = ($3)[1];
      return $span;
    }))()
    : ((($3)[0] === "Nothing")
      ? ((($1) => {
        return ($sd1$Human$Type$Block)($sd1$Human$Type$NotIndented, $1);
      }))(((($1) => {
        return ($core$List$map)((($c) => {
          return ($sd1$Human$Type$rowOrIndented)($separator, ($core$Core$Cons)($c, $core$Core$Nil));
        }), $1);
      }))($content))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 141:4', (sp_toHuman)($3))));
});

const $sd1$Human$Type$tyvarIdToText = (($env, $id) => {
  return (text_fromNumber)($id);
});

const $sd1$Human$Type$usrToText = (($env, $usr) => {
  const $3 = $usr;
  const $name = ($3)[2];
  const $umr = ($3)[1];
  return $name;
});

const $sd1$Human$Type$doRawType = (($env, $raw) => {
  return ((($raw)[0] === "TypeExact")
    ? ((() => {
      const $usr = ($raw)[1];
      const $raws = ($raw)[2];
      return ($sd1$Human$Type$rowOrIndented)(($sd1$Human$Type$usrToText)($env, $usr), ($core$List$map)((($1) => {
        return ($sd1$Human$Type$doRawType)($env, $1);
      }), $raws));
    }))()
    : ((($raw)[0] === "TypeFn")
      ? ((() => {
        const $parTypes = ($raw)[1];
        const $full = ($raw)[2];
        return ((($1) => {
          return ($sd1$Human$Type$rowOrIndented)("fn", $1);
        }))(($core$Core$Cons)(($sd1$Human$Type$separatedBy)(",", ($core$List$map)((($1) => {
          return ($sd1$Human$Type$doParType)($env, $1);
        }), $parTypes)), ($core$Core$Cons)(($sd1$Human$Type$rowOrIndented)(":", ($core$Core$Cons)(($sd1$Human$Type$doFullType)($env, $full), $core$Core$Nil)), $core$Core$Nil)));
      }))()
      : ((($raw)[0] === "TypeVar")
        ? ((() => {
          const $tyvarId = ($raw)[1];
          return ($sd1$Human$Type$text)((text_fromNumber)($tyvarId));
        }))()
        : ((($raw)[0] === "TypeRecord")
          ? ((() => {
            const $maybeExtId = ($raw)[1];
            const $attrs = ($raw)[2];
            const $doAttr = (($3) => {
              const $name = $3.first;
              const $r = $3.second;
              return ($sd1$Human$Type$rowOrHead)(($name + " as"), ($core$Core$Cons)(($sd1$Human$Type$doRawType)($env, $r), $core$Core$Nil));
            });
            const $open = ((($maybeExtId)[0] === "Just")
              ? ((() => {
                const $tyvarId = ($maybeExtId)[1];
                return ("{ " + (($sd1$Human$Type$tyvarIdToText)($env, $tyvarId) + " with"));
              }))()
              : ((($maybeExtId)[0] === "Nothing")
                ? "{"
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 258:16', (sp_toHuman)($maybeExtId))));
            return ($sd1$Human$Type$list)(({
              close: "}",
              items: ((($1) => {
                return ($core$List$map)($doAttr, $1);
              }))(($core$Dict$toList)($attrs)),
              open: $open,
              separator: ",",
            }));
          }))()
          : ((($raw)[0] === "TypeError")
            ? ($sd1$Human$Type$text)("???")
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/Type.sp 205:4', (sp_toHuman)($raw)))))));
});

const $sd1$Compiler$TypeCheck$typeToHuman = (($env, $raw) => {
  return ((($1) => {
    return ($sd1$Human$Type$display)("", $1);
  }))(((($1) => {
    return ($sd1$Human$Type$doRawType)({}, $1);
  }))($raw));
});

const $sd1$Compiler$TypeCheck$makeResolutionError = (($env, $caModule, $1) => {
  const $context = ($1.first)[1];
  const $pos = ($1.first)[2];
  const $why = ($1.first)[3];
  const $t1 = ($1.first)[4];
  const $t2 = ($1.first)[5];
  const $message = $1.second;
  return ($sd1$Compiler$Error$Simple)($env.errorModule, $pos, ($core$Core$Cons)($message, ($core$Core$Cons)((sp_toHuman)($context), ($core$Core$Cons)((sp_toHuman)($why), ($core$Core$Cons)(($sd1$Compiler$TypeCheck$typeToHuman)($env, $t1), ($core$Core$Cons)("", ($core$Core$Cons)(($sd1$Compiler$TypeCheck$typeToHuman)($env, $t2), $core$Core$Nil)))))));
});

const $sd1$Compiler$TypeCheck$addErErrorIf = (($test, $equality, $message, $state) => {
  return ($test
    ? ((() => {
      const $0 = $state;
      return (Object.assign)({}, $0, ({
        errors: (sp_cons)(({
          first: $equality,
          second: $message,
        }), $0.errors),
      }));
    }))()
    : $state);
});

const $sd1$Compiler$TypeCheck$compareParTypes = (($currentEquality, $index, $p1, $p2, $state0) => {
  const $6 = $currentEquality;
  const $f2 = ($6)[5];
  const $f1 = ($6)[4];
  const $why = ($6)[3];
  const $pos = ($6)[2];
  const $context = ($6)[1];
  const $7 = ({
    first: $p1,
    second: $p2,
  });
  return (((($7.first)[0] === "ParRe") && (($7.second)[0] === "ParRe"))
    ? ((() => {
      const $raw1 = ($7.first)[1];
      const $raw2 = ($7.second)[1];
      const $eq = ($sd1$Compiler$TypeCheck$Equality)($context, $pos, ($sd1$Compiler$TypeCheck$Why_FunctionInput)($index, $why), $raw1, $raw2);
      const $0 = $state0;
      return (Object.assign)({}, $0, ({
        equalities: (sp_cons)($eq, $0.equalities),
      }));
    }))()
    : (((($7.first)[0] === "ParSp") && (($7.second)[0] === "ParSp"))
      ? ((() => {
        const $full1 = ($7.first)[1];
        const $full2 = ($7.second)[1];
        const $eq = ($sd1$Compiler$TypeCheck$Equality)($context, $pos, ($sd1$Compiler$TypeCheck$Why_FunctionInput)($index, $why), $full1.raw, $full2.raw);
        const $state1 = ((() => {
          const $8 = ($sd1$Compiler$TypeCheck$uniCanBeCastTo)(({
            given: $full1.uni,
            required: $full2.uni,
          }));
          return ((($8)[0] === "CanBeCastYes")
            ? $state0
            : (((($8)[0] === "CanBeCastNo") && ((($8)[1])[0] === "Nil"))
              ? ((($2) => {
                return ($sd1$Compiler$TypeCheck$addErError)($currentEquality, ("Function call par " + ((text_fromNumber)($index) + " with wrong uniqueness")), $2);
              }))($state0)
              : (((($8)[0] === "CanBeCastNo") && ((($8)[1])[0] === "Cons"))
                ? ((() => {
                  const $id = (($8)[1])[1].first;
                  const $uni = (($8)[1])[1].second;
                  const $tail = (($8)[1])[2];
                  const $0 = $state0;
                  return (Object.assign)({}, $0, ({
                    univarEqualities: (sp_cons)(({
                      context: $context,
                      id: $id,
                      pos: $pos,
                      uni: $uni,
                      why: "fn arg",
                    }), $0.univarEqualities),
                  }));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2340:16', (sp_toHuman)($8)))));
        }))();
        const $0 = $state1;
        return (Object.assign)({}, $0, ({
          equalities: (sp_cons)($eq, $0.equalities),
        }));
      }))()
      : (true
        ? ($sd1$Compiler$TypeCheck$addErError)(($sd1$Compiler$TypeCheck$Equality)($context, $pos, ($sd1$Compiler$TypeCheck$Why_FunctionInput)($index, $why), $f1, $f2), "recycling does not match", $state0)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2328:4', (sp_toHuman)($7)))));
});

const $sd1$Compiler$TypeCheck$list_forWithIndex2 = (($c, $index, $aa, $bb, $f) => {
  const $6 = ({
    first: $aa,
    second: $bb,
  });
  return (((($6.first)[0] === "Cons") && (($6.second)[0] === "Cons"))
    ? ((() => {
      const $a = ($6.first)[1];
      const $at = ($6.first)[2];
      const $b = ($6.second)[1];
      const $bt = ($6.second)[2];
      return ($sd1$Compiler$TypeCheck$list_forWithIndex2)(($f)($index, $a, $b, $c), ($index + 1), $at, $bt, $f);
    }))()
    : (true
      ? $c
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 68:4', (sp_toHuman)($6))));
});

const $sd1$Compiler$TypeCheck$occurs = (($tyvarId, $type) => {
  const $rec = (($1) => {
    return ($sd1$Compiler$TypeCheck$occurs)($tyvarId, $1);
  });
  return ((($type)[0] === "TypeFn")
    ? ((() => {
      const $ins = ($type)[1];
      const $out = ($type)[2];
      return (($core$List$any)((($t) => {
        return ($rec)(($sd1$Types$TypedAst$toRaw)($t));
      }), $ins) || ($rec)($out.raw));
    }))()
    : ((($type)[0] === "TypeVar")
      ? ((() => {
        const $id = ($type)[1];
        return (sp_equal)($id, $tyvarId);
      }))()
      : ((($type)[0] === "TypeExact")
        ? ((() => {
          const $usr = ($type)[1];
          const $args = ($type)[2];
          return ($core$List$any)($rec, $args);
        }))()
        : ((($type)[0] === "TypeRecord")
          ? ((() => {
            const $attrs = ($type)[2];
            return ($core$Dict$any)((($k, $v) => {
              return ($rec)($v);
            }), $attrs);
          }))()
          : ((($type)[0] === "TypeError")
            ? false
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2520:4', (sp_toHuman)($type)))))));
});

const $sd1$Compiler$TypeCheck$replaceUnificationVariable = (($equality, $tyvarId, $replacingType, $state) => {
  const $isSame = ((($replacingType)[0] === "TypeVar")
    ? ((() => {
      const $tyvarId2 = ($replacingType)[1];
      return (sp_equal)($tyvarId, $tyvarId2);
    }))()
    : (true
      ? false
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2486:8', (sp_toHuman)($replacingType))));
  return ($isSame
    ? $state
    : (($sd1$Compiler$TypeCheck$occurs)($tyvarId, $replacingType)
      ? ($sd1$Compiler$TypeCheck$addErError)($equality, "circular!?", $state)
      : ((() => {
        const $zzz = (($5) => {
          const $context = ($5)[1];
          const $pos = ($5)[2];
          const $why = ($5)[3];
          const $t1 = ($5)[4];
          const $t2 = ($5)[5];
          return ($sd1$Compiler$TypeCheck$Equality)($context, $pos, $why, ($sd1$Compiler$TypeCheck$applySubstitutionToType)($tyvarId, $replacingType, $t1), ($sd1$Compiler$TypeCheck$applySubstitutionToType)($tyvarId, $replacingType, $t2));
        });
        const $equalities = ($core$List$map)($zzz, $state.equalities);
        const $substitutions = ((($2) => {
          return ($core$Dict$insert)($tyvarId, $replacingType, $2);
        }))(((($1) => {
          return ($core$Dict$map)(((_0, $type) => {
            return ($sd1$Compiler$TypeCheck$applySubstitutionToType)($tyvarId, $replacingType, $type);
          }), $1);
        }))($state.substitutions));
        const $0 = $state;
        return (Object.assign)({}, $0, ({
          equalities: $equalities,
          substitutions: $substitutions,
        }));
      }))()));
});

const $sd1$Compiler$TypeCheck$solveRecordExt = (($equality, $swapEquality, $tyvar1, $attrs1, $attrs2, $state) => {
  const $7 = $equality;
  const $why = ($7)[3];
  const $pos = ($7)[2];
  const $context = ($7)[1];
  const $newState = ((($0) => {
    return ($core$Dict$for)($0, $attrs1, (($name, $type1, $s) => {
      const $12 = ($core$Dict$get)($name, $attrs2);
      return ((($12)[0] === "Nothing")
        ? ((($2) => {
          return ($sd1$Compiler$TypeCheck$addErError)($equality, ("missing attribute " + $name), $2);
        }))($state)
        : ((($12)[0] === "Just")
          ? ((() => {
            const $type2 = ($12)[1];
            const $13 = ($swapEquality
              ? ({
                first: $type2,
                second: $type1,
              })
              : ({
                first: $type1,
                second: $type2,
              }));
            const $b = $13.second;
            const $a = $13.first;
            const $0 = $state;
            return (Object.assign)({}, $0, ({
              equalities: (sp_cons)(($sd1$Compiler$TypeCheck$Equality)(($sd1$Compiler$TypeCheck$Context_AttributeName)($name, $context), $pos, $why, $a, $b), $0.equalities),
            }));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2472:12', (sp_toHuman)($12))));
    }));
  }))($state);
  return ($sd1$Compiler$TypeCheck$replaceUnificationVariable)($equality, $tyvar1, ($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, $attrs2), $newState);
});

const $sd1$Compiler$TypeCheck$solveOneEquality = (($head, $state) => {
  const $3 = $head;
  const $raw2 = ($3)[5];
  const $raw1 = ($3)[4];
  const $why = ($3)[3];
  const $pos = ($3)[2];
  const $context = ($3)[1];
  const $4 = ({
    first: $raw1,
    second: $raw2,
  });
  return ((($4.first)[0] === "TypeVar")
    ? ((() => {
      const $tyvarId = ($4.first)[1];
      const $t2 = $4.second;
      return ((($3) => {
        return ($sd1$Compiler$TypeCheck$replaceUnificationVariable)($head, $tyvarId, $t2, $3);
      }))($state);
    }))()
    : ((($4.second)[0] === "TypeVar")
      ? ((() => {
        const $t1 = $4.first;
        const $tyvarId = ($4.second)[1];
        return ((($3) => {
          return ($sd1$Compiler$TypeCheck$replaceUnificationVariable)($head, $tyvarId, $t1, $3);
        }))($state);
      }))()
      : (((($4.first)[0] === "TypeExact") && (($4.second)[0] === "TypeExact"))
        ? ((() => {
          const $usr1 = ($4.first)[1];
          const $args1 = ($4.first)[2];
          const $usr2 = ($4.second)[1];
          const $args2 = ($4.second)[2];
          return ((sp_not_equal)($usr1, $usr2)
            ? ($sd1$Compiler$TypeCheck$addErError)($head, "types are incompatible2", $state)
            : ((() => {
              const $newEqualities = ($core$List$indexedMap2)((($index, $a, $b) => {
                return ($sd1$Compiler$TypeCheck$Equality)($context, $pos, ($sd1$Compiler$TypeCheck$Why_TypeArgument)($usr1, $index, $why), $a, $b);
              }), $args2, $args1);
              const $0 = $state;
              return (Object.assign)({}, $0, ({
                equalities: ($core$List$append)($0.equalities, $newEqualities),
              }));
            }))());
        }))()
        : (((($4.first)[0] === "TypeFn") && (($4.second)[0] === "TypeFn"))
          ? ((() => {
            const $pars1 = ($4.first)[1];
            const $out1 = ($4.first)[2];
            const $pars2 = ($4.second)[1];
            const $out2 = ($4.second)[2];
            return ((sp_not_equal)(($core$List$length)($pars1), ($core$List$length)($pars2))
              ? ((($2) => {
                return ($sd1$Compiler$TypeCheck$addErError)($head, "functions expect a different number of arguments", $2);
              }))($state)
              : ((() => {
                const $outEquality = ($sd1$Compiler$TypeCheck$Equality)($context, $pos, ($sd1$Compiler$TypeCheck$Why_FunctionOutput)($why), $out1.raw, $out2.raw);
                const $s1 = ((() => {
                  const $5 = ($sd1$Compiler$TypeCheck$uniCanBeCastTo)(({
                    given: $out2.uni,
                    required: $out1.uni,
                  }));
                  return ((($5)[0] === "CanBeCastYes")
                    ? $state
                    : (((($5)[0] === "CanBeCastNo") && ((($5)[1])[0] === "Nil"))
                      ? ((($2) => {
                        return ($sd1$Compiler$TypeCheck$addErError)($head, "the function return type have different uniqueness", $2);
                      }))($state)
                      : (((($5)[0] === "CanBeCastNo") && ((($5)[1])[0] === "Cons"))
                        ? ((() => {
                          const $id = (($5)[1])[1].first;
                          const $uni = (($5)[1])[1].second;
                          const $tail = (($5)[1])[2];
                          const $0 = $state;
                          return (Object.assign)({}, $0, ({
                            univarEqualities: (sp_cons)(({
                              context: $context,
                              id: $id,
                              pos: $pos,
                              uni: $uni,
                              why: "fn out",
                            }), $0.univarEqualities),
                          }));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2401:28', (sp_toHuman)($5)))));
                }))();
                return ((($0) => {
                  return ($sd1$Compiler$TypeCheck$list_forWithIndex2)($0, 0, $pars1, $pars2, (($1, $2, $3, $4) => {
                    return ($sd1$Compiler$TypeCheck$compareParTypes)($head, $1, $2, $3, $4);
                  }));
                }))(((() => {
                  const $0 = $s1;
                  return (Object.assign)({}, $0, ({
                    equalities: (sp_cons)($outEquality, $0.equalities),
                  }));
                }))());
              }))());
          }))()
          : (((($4.first)[0] === "TypeRecord") && (((($4.first)[1])[0] === "Nothing") && ((($4.second)[0] === "TypeRecord") && ((($4.second)[1])[0] === "Nothing"))))
            ? ((() => {
              const $attrs1 = ($4.first)[2];
              const $attrs2 = ($4.second)[2];
              const $5 = ($core$Dict$onlyBothOnly)($attrs1, $attrs2);
              const $only2 = $5.third;
              const $both = $5.second;
              const $only1 = $5.first;
              const $equalities = ((($0) => {
                return ($core$Dict$for)($0, $both, (($attrName, $8, $eqs) => {
                  const $attrType1 = $8.first;
                  const $attrType2 = $8.second;
                  return (sp_cons)(($sd1$Compiler$TypeCheck$Equality)($context, $pos, ($sd1$Compiler$TypeCheck$Why_Attribute)($why), $attrType1, $attrType2), $eqs);
                }));
              }))($state.equalities);
              return ((($3) => {
                return ($sd1$Compiler$TypeCheck$addErErrorIf)(((sp_not_equal)($only1, $core$Dict$empty) || (sp_not_equal)($only2, $core$Dict$empty)), $head, "record attrs don't match", $3);
              }))(((() => {
                const $0 = $state;
                return (Object.assign)({}, $0, ({
                  equalities: $equalities,
                }));
              }))());
            }))()
            : (((($4.first)[0] === "TypeRecord") && (((($4.first)[1])[0] === "Just") && ((($4.second)[0] === "TypeRecord") && ((($4.second)[1])[0] === "Nothing"))))
              ? ((() => {
                const $tyvar1 = (($4.first)[1])[1];
                const $attrs1 = ($4.first)[2];
                const $attrs2 = ($4.second)[2];
                return ($sd1$Compiler$TypeCheck$solveRecordExt)($head, false, $tyvar1, $attrs1, $attrs2, $state);
              }))()
              : (((($4.first)[0] === "TypeRecord") && (((($4.first)[1])[0] === "Nothing") && ((($4.second)[0] === "TypeRecord") && ((($4.second)[1])[0] === "Just"))))
                ? ((() => {
                  const $attrs1 = ($4.first)[2];
                  const $tyvar2 = (($4.second)[1])[1];
                  const $attrs2 = ($4.second)[2];
                  return ($sd1$Compiler$TypeCheck$solveRecordExt)($head, true, $tyvar2, $attrs2, $attrs1, $state);
                }))()
                : (((($4.first)[0] === "TypeRecord") && (((($4.first)[1])[0] === "Just") && ((($4.second)[0] === "TypeRecord") && ((($4.second)[1])[0] === "Just"))))
                  ? ((() => {
                    const $tyvar1 = (($4.first)[1])[1];
                    const $attrs1 = ($4.first)[2];
                    const $tyvar2 = (($4.second)[1])[1];
                    const $attrs2 = ($4.second)[2];
                    const $5 = ($core$Dict$onlyBothOnly)($attrs1, $attrs2);
                    const $only2 = $5.third;
                    const $both = $5.second;
                    const $only1 = $5.first;
                    const $newTyvarId = ($state.lastUnificationVarId + 1);
                    const $newType = ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)($newTyvarId), ($core$Dict$join)($attrs1, $only2));
                    return ((($0) => {
                      return ($core$Dict$for)($0, $both, (($name, $8, $s) => {
                        const $t1 = $8.first;
                        const $t2 = $8.second;
                        const $0 = $s;
                        return (Object.assign)({}, $0, ({
                          equalities: (sp_cons)(($sd1$Compiler$TypeCheck$Equality)(($sd1$Compiler$TypeCheck$Context_AttributeName)($name, $context), $pos, $why, $t1, $t1), $0.equalities),
                        }));
                      }));
                    }))(((($3) => {
                      return ($sd1$Compiler$TypeCheck$replaceUnificationVariable)($head, $tyvar2, $newType, $3);
                    }))(((($3) => {
                      return ($sd1$Compiler$TypeCheck$replaceUnificationVariable)($head, $tyvar1, $newType, $3);
                    }))(((() => {
                      const $0 = $state;
                      return (Object.assign)({}, $0, ({
                        lastUnificationVarId: $newTyvarId,
                      }));
                    }))())));
                  }))()
                  : ((($4.first)[0] === "TypeError")
                    ? $state
                    : ((($4.second)[0] === "TypeError")
                      ? $state
                      : (true
                        ? ((($2) => {
                          return ($sd1$Compiler$TypeCheck$addErError)($head, "types are incompatible1", $2);
                        }))($state)
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2370:14', (sp_toHuman)($4)))))))))))));
});

const $sd1$Compiler$TypeCheck$solveEqualities = (($oldState) => {
  const $2 = $oldState.equalities;
  return ((($2)[0] === "Nil")
    ? $oldState
    : ((($2)[0] === "Cons")
      ? ((() => {
        const $head = ($2)[1];
        const $tail = ($2)[2];
        return ($sd1$Compiler$TypeCheck$solveEqualities)(((($1) => {
          return ($sd1$Compiler$TypeCheck$solveOneEquality)($head, $1);
        }))(((() => {
          const $0 = $oldState;
          return (Object.assign)({}, $0, ({
            equalities: $tail,
          }));
        }))()));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2354:4', (sp_toHuman)($2))));
});

const $sd1$Compiler$TypeCheck$addSub = (($newId, $newUni, $subs) => {
  const $replace = (($id, $uni) => {
    return ((($uni)[0] === "Depends")
      ? ((() => {
        const $blah = ($uni)[1];
        return ((sp_equal)($blah, $newId)
          ? $newUni
          : $uni);
      }))()
      : (true
        ? $uni
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2248:8', (sp_toHuman)($uni))));
  });
  return ((($2) => {
    return ($core$Dict$insert)($newId, $newUni, $2);
  }))(((($1) => {
    return ($core$Dict$map)($replace, $1);
  }))($subs));
});

const $sd1$Compiler$TypeCheck$solveUniquenessConstraint = (($env, $eq, $state, $subs) => {
  const $5 = ($core$Dict$get)($eq.id, $subs);
  return ([
    ((($5)[0] === "Nothing")
      ? ($sd1$Compiler$TypeCheck$addSub)($eq.id, $eq.uni, $subs)
      : ((($5)[0] === "Just")
        ? ((() => {
          const $subUni = ($5)[1];
          return ((sp_equal)($subUni, $eq.uni)
            ? $subs
            : ((() => {
              const $6 = ({
                first: $subUni,
                second: $eq.uni,
              });
              return ((($6.first)[0] === "Depends")
                ? ((() => {
                  const $subId = ($6.first)[1];
                  return ($sd1$Compiler$TypeCheck$addSub)($subId, $eq.uni, $subs);
                }))()
                : ((($6.second)[0] === "Depends")
                  ? ((() => {
                    const $newId = ($6.second)[1];
                    return ($sd1$Compiler$TypeCheck$addSub)($newId, $subUni, $subs);
                  }))()
                  : (true
                    ? ((() => {
                      ((__re__ = ($sd1$Compiler$TypeCheck$addError)($env, $eq.pos, ($sd1$Compiler$TypeCheck$ErrorUnresolvableUniqueness)($eq, $subUni), $state)), ($state = (__re__)[1]), (__re__)[0]);
                      return $subs;
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2269:16', (sp_toHuman)($6)))));
            }))());
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2260:4', (sp_toHuman)($5)))),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$solveUniquenessConstraints = (($env, $eqs, $state, $subs) => {
  return ([
    ((($eqs)[0] === "Nil")
      ? $subs
      : ((($eqs)[0] === "Cons")
        ? ((() => {
          const $head = ($eqs)[1];
          const $tail = ($eqs)[2];
          return ((($3) => {
            return ((__re__ = ($sd1$Compiler$TypeCheck$solveUniquenessConstraints)($env, $tail, $state, $3)), ($state = (__re__)[1]), (__re__)[0]);
          }))(((($3) => {
            return ((__re__ = ($sd1$Compiler$TypeCheck$solveUniquenessConstraint)($env, $head, $state, $3)), ($state = (__re__)[1]), (__re__)[0]);
          }))($subs));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2284:4', (sp_toHuman)($eqs)))),
    $state,
  ]);
});

const $sd1$Types$TypedAst$resolveArg = (($saf, $arg) => {
  return ((($arg)[0] === "ArgumentExpression")
    ? ((() => {
      const $full = ($arg)[1];
      const $expr = ($arg)[2];
      return ($sd1$Types$TypedAst$ArgumentExpression)(($sd1$Types$TypedAst$resolveFull)($saf, $full), ($sd1$Types$TypedAst$resolveExpression)($saf, $expr));
    }))()
    : ((($arg)[0] === "ArgumentRecycle")
      ? ((() => {
        const $p = ($arg)[1];
        const $raw = ($arg)[2];
        const $attrPath = ($arg)[3];
        const $name = ($arg)[4];
        return ($sd1$Types$TypedAst$ArgumentRecycle)($p, ($sd1$Types$TypedAst$resolveRaw)($saf, $raw), $attrPath, $name);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 213:4', (sp_toHuman)($arg))));
});

const $sd1$Types$TypedAst$resolvePattern = (($saf, $pattern) => {
  return ((($pattern)[0] === "PatternLiteralNumber")
    ? ((() => {
      const $pos = ($pattern)[1];
      return $pattern;
    }))()
    : ((($pattern)[0] === "PatternLiteralText")
      ? ((() => {
        const $pos = ($pattern)[1];
        return $pattern;
      }))()
      : ((($pattern)[0] === "PatternAny")
        ? ((() => {
          const $pos = ($pattern)[1];
          const $stuff = ($pattern)[2];
          return ($sd1$Types$TypedAst$PatternAny)($pos, ((() => {
            const $0 = $stuff;
            return (Object.assign)({}, $0, ({
              type: ($sd1$Types$TypedAst$resolveFull)($saf, $0.type),
            }));
          }))());
        }))()
        : ((($pattern)[0] === "PatternConstructor")
          ? ((() => {
            const $pos = ($pattern)[1];
            const $usr = ($pattern)[2];
            const $ps = ($pattern)[3];
            return ($sd1$Types$TypedAst$PatternConstructor)($pos, $usr, ($core$List$map)((($1) => {
              return ($sd1$Types$TypedAst$resolvePattern)($saf, $1);
            }), $ps));
          }))()
          : ((($pattern)[0] === "PatternRecord")
            ? ((() => {
              const $pos = ($pattern)[1];
              const $ps = ($pattern)[2];
              return ($sd1$Types$TypedAst$PatternRecord)($pos, ($core$Dict$map)((($k, $3) => {
                const $p = $3.first;
                const $t = $3.second;
                return ({
                  first: ($sd1$Types$TypedAst$resolvePattern)($saf, $p),
                  second: ($sd1$Types$TypedAst$resolveRaw)($saf, $t),
                });
              }), $ps));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 278:4', (sp_toHuman)($pattern)))))));
});

const $sd1$Types$TypedAst$resolvePar = (($saf, $par) => {
  return ((($par)[0] === "ParameterPattern")
    ? ((() => {
      const $full = ($par)[1];
      const $pa = ($par)[2];
      return ($sd1$Types$TypedAst$ParameterPattern)(($sd1$Types$TypedAst$resolveFull)($saf, $full), ($sd1$Types$TypedAst$resolvePattern)($saf, $pa));
    }))()
    : ((($par)[0] === "ParameterRecycle")
      ? ((() => {
        const $p = ($par)[1];
        const $raw = ($par)[2];
        const $name = ($par)[3];
        return ($sd1$Types$TypedAst$ParameterRecycle)($p, ($sd1$Types$TypedAst$resolveRaw)($saf, $raw), $name);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 223:4', (sp_toHuman)($par))));
});

const $sd1$Types$TypedAst$resolveExpression = (($saf, $expression) => {
  const $rec = (($1) => {
    return ($sd1$Types$TypedAst$resolveExpression)($saf, $1);
  });
  return ((($expression)[0] === "LiteralNumber")
    ? $expression
    : ((($expression)[0] === "LiteralText")
      ? $expression
      : ((($expression)[0] === "Variable")
        ? $expression
        : ((($expression)[0] === "Constructor")
          ? $expression
          : ((($expression)[0] === "Fn")
            ? ((() => {
              const $p = ($expression)[1];
              const $pars = ($expression)[2];
              const $body = ($expression)[3];
              const $bodyType = ($expression)[4];
              return ($sd1$Types$TypedAst$Fn)($p, ($core$List$map)((($1) => {
                return ($sd1$Types$TypedAst$resolvePar)($saf, $1);
              }), $pars), ($rec)($body), ($sd1$Types$TypedAst$resolveFull)($saf, $bodyType));
            }))()
            : ((($expression)[0] === "Call")
              ? ((() => {
                const $p = ($expression)[1];
                const $ref = ($expression)[2];
                const $args = ($expression)[3];
                return ($sd1$Types$TypedAst$Call)($p, ($rec)($ref), ($core$List$map)((($1) => {
                  return ($sd1$Types$TypedAst$resolveArg)($saf, $1);
                }), $args));
              }))()
              : ((($expression)[0] === "Record")
                ? ((() => {
                  const $p = ($expression)[1];
                  const $maybeExt = ($expression)[2];
                  const $attrs = ($expression)[3];
                  return ($sd1$Types$TypedAst$Record)($p, ($core$Maybe$map)($rec, $maybeExt), ($core$Dict$map)((($k, $v) => {
                    return ($rec)($v);
                  }), $attrs));
                }))()
                : ((($expression)[0] === "RecordAccess")
                  ? ((() => {
                    const $p = ($expression)[1];
                    const $name = ($expression)[2];
                    const $exp = ($expression)[3];
                    return ($sd1$Types$TypedAst$RecordAccess)($p, $name, ($rec)($exp));
                  }))()
                  : ((($expression)[0] === "LetIn")
                    ? ((() => {
                      const $def = ($expression)[1];
                      const $rest = ($expression)[2];
                      const $restType = ($expression)[3];
                      return ($sd1$Types$TypedAst$LetIn)(($sd1$Types$TypedAst$resolveValueDef)($saf, $def), ($rec)($rest), ($sd1$Types$TypedAst$resolveFull)($saf, $restType));
                    }))()
                    : ((($expression)[0] === "If")
                      ? ((() => {
                        const $p = ($expression)[1];
                        const $condition = ($expression)[2].condition;
                        const $false = ($expression)[2].false;
                        const $true = ($expression)[2].true;
                        return ($sd1$Types$TypedAst$If)($p, ({
                          condition: ($rec)($condition),
                          false: ($rec)($false),
                          true: ($rec)($true),
                        }));
                      }))()
                      : ((($expression)[0] === "Try")
                        ? ((() => {
                          const $p = ($expression)[1];
                          const $patternsAndExpressions = ($expression)[2].patternsAndExpressions;
                          const $value = ($expression)[2].value;
                          const $valueType = ($expression)[2].valueType;
                          return ($sd1$Types$TypedAst$Try)($p, ({
                            patternsAndExpressions: ($core$List$map)((($2) => {
                              return ($core$Tuple$mapBoth)((($1) => {
                                return ($sd1$Types$TypedAst$resolvePattern)($saf, $1);
                              }), $rec, $2);
                            }), $patternsAndExpressions),
                            value: ($rec)($value),
                            valueType: ($sd1$Types$TypedAst$resolveFull)($saf, $valueType),
                          }));
                        }))()
                        : ((($expression)[0] === "DestroyIn")
                          ? ((() => {
                            const $n = ($expression)[1];
                            const $e = ($expression)[2];
                            return ($sd1$Types$TypedAst$DestroyIn)($n, ($rec)($e));
                          }))()
                          : ((($expression)[0] === "Error")
                            ? ((() => {
                              const $p = ($expression)[1];
                              return $expression;
                            }))()
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 236:4', (sp_toHuman)($expression)))))))))))))));
});

const $sd1$Types$TypedAst$resolveValueDef = (($saf, $def) => {
  const $0 = $def;
  return (Object.assign)({}, $0, ({
    body: ($sd1$Types$TypedAst$resolveExpression)($saf, $0.body),
    pattern: ($sd1$Types$TypedAst$resolvePattern)($saf, $0.pattern),
    type: ($sd1$Types$TypedAst$resolveFull)($saf, $0.type),
  }));
});

const $sd1$Compiler$TypeCheck$doRootDefinition = (($lastUnificationVarId, $errors, $module, $env, $def) => {
  let $state = ($sd1$Compiler$TypeCheck$initState)(((__re__ = (basics_cloneUni)($lastUnificationVarId)), ($lastUnificationVarId = (__re__)[1]), (__re__)[0]));
  const $nameToRef = (($name) => {
    return ($sd1$Types$Ast$RefGlobal)(($sd1$Types$Meta$USR)($module.umr, $name));
  });
  (sp_benchStart)(null);
  const $6 = ((__re__ = ($sd1$Compiler$TypeCheck$doDefinition)($nameToRef, $env, $def, $state)), ($state = (__re__)[1]), (__re__)[0]);
  const $envF = $6.second;
  const $typedDef = $6.first;
  (sp_benchStop)("type inference");
  (sp_benchStart)(null);
  const $erState0 = ({
    equalities: ((__re__ = (array_toList)($state.equalities)), ($state.equalities = (__re__)[1]), (__re__)[0]),
    errors: $core$Core$Nil,
    lastUnificationVarId: ((__re__ = (basics_cloneUni)($state.lastUnificationVarId)), ($state.lastUnificationVarId = (__re__)[1]), (__re__)[0]),
    substitutions: $core$Dict$empty,
    univarEqualities: ((__re__ = (array_toList)($state.univarEqualities)), ($state.univarEqualities = (__re__)[1]), (__re__)[0]),
  });
  const $erStateF = ((($st) => {
    return ($core$List$for)($st, $st.equalities, (($eq, $s) => {
      return ($sd1$Compiler$TypeCheck$addErError)($eq, "unresolved", $s);
    }));
  }))(($sd1$Compiler$TypeCheck$solveEqualities)($erState0));
  (sp_benchStop)("equalities resolution");
  (sp_benchStart)(null);
  const $univarSubs = ((__re__ = ($sd1$Compiler$TypeCheck$solveUniquenessConstraints)($env, $erStateF.univarEqualities, $state, $core$Dict$empty)), ($state = (__re__)[1]), (__re__)[0]);
  (sp_benchStop)("uniqueness resolution");
  (sp_benchStart)(null);
  const $subsAsFns = ({
    ty: (($tyvarId) => {
      return ($core$Dict$get)($tyvarId, $erStateF.substitutions);
    }),
    uni: (($univarId) => {
      return ($core$Dict$get)($univarId, $univarSubs);
    }),
  });
  const $resolvedValueDef = ($sd1$Types$TypedAst$resolveValueDef)($subsAsFns, $typedDef);
  (sp_benchStop)("def resolution");
  ($lastUnificationVarId = ((__re__ = (basics_cloneUni)($state.lastUnificationVarId)), ($state.lastUnificationVarId = (__re__)[1]), (__re__)[0]));
  ((__re__ = (array_each)($state.errors, (($err) => {
    return ((__re__ = (array_push)($errors, ($sd1$Compiler$TypeCheck$makeInferenceAndCheckError)($env, $err))), ($errors = (__re__)[1]), (__re__)[0]);
  }))), ($state.errors = (__re__)[1]), (__re__)[0]);
  ($core$List$each)($erStateF.errors, (($err) => {
    return ((__re__ = (array_push)($errors, ($sd1$Compiler$TypeCheck$makeResolutionError)($env, $module, $err))), ($errors = (__re__)[1]), (__re__)[0]);
  }));
  return ([
    ({
      first: $resolvedValueDef,
      second: $envF,
    }),
    $lastUnificationVarId,
    $errors,
  ]);
});

const $sd1$Compiler$TypeCheck$insertAnnotatedAndNonAnnotated = (($pa, $def, $1) => {
  const $ann = $1.first;
  const $nonAnn = $1.second;
  const $isFullyAnnotated = ((($1) => {
    return ($core$List$all)((($stuff) => {
      return (sp_not_equal)($stuff.maybeAnnotation, $core$Maybe$Nothing);
    }), $1);
  }))(($core$Dict$values)(($sd1$Types$CanonicalAst$patternNames)($pa)));
  return ($isFullyAnnotated
    ? ({
      first: (sp_cons)($def, $ann),
      second: $nonAnn,
    })
    : ({
      first: $ann,
      second: (sp_cons)($def, $nonAnn),
    }));
});

const $sd1$Compiler$TypeCheck$doModule = (($lastUnificationVarId, $globalEnv, $caModule) => {
  const $env = ((() => {
    const $0 = $globalEnv;
    return (Object.assign)({}, $0, ({
      errorModule: ({
        content: $caModule.asText,
        fsPath: $caModule.fsPath,
      }),
    }));
  }))();
  const $4 = ($core$Dict$for)(({
    first: $core$Core$Nil,
    second: $core$Core$Nil,
  }), $caModule.valueDefs, $sd1$Compiler$TypeCheck$insertAnnotatedAndNonAnnotated);
  const $nonAnnotated = $4.second;
  const $annotated = $4.first;
  ((($core$List$length)($nonAnnotated) > 1)
    ? ($sd1$Compiler$TypeCheck$bug)("Right now the compiler supports only one root-level non-annotated value per module. =(")
    : null);
  const $allOrdered = ($core$List$concat)(($core$Core$Cons)($nonAnnotated, ($core$Core$Cons)($annotated, $core$Core$Nil)));
  let $errors = (array_fromList)($core$Core$Nil);
  const $5 = ((($0) => {
    return ($core$List$for)($0, $allOrdered, (($def, $7) => {
      const $accum = $7.first;
      const $env0 = $7.second;
      const $9 = ((__re__ = ($sd1$Compiler$TypeCheck$doRootDefinition)($lastUnificationVarId, $errors, $caModule, $env0, $def)), ($lastUnificationVarId = (__re__)[1]), ($errors = (__re__)[2]), (__re__)[0]);
      const $env1 = $9.second;
      const $typedDef = $9.first;
      return ({
        first: ($core$Dict$insert)($def.pattern, $typedDef, $accum),
        second: $env1,
      });
    }));
  }))(({
    first: $core$Dict$empty,
    second: $env,
  }));
  const $envF = $5.second;
  const $valueDefs = $5.first;
  const $typedModule = ({
    asText: $caModule.asText,
    fsPath: $caModule.fsPath,
    umr: $caModule.umr,
    valueDefs: $valueDefs,
  });
  const $errs = ((__re__ = (array_toList)($errors)), ($errors = (__re__)[1]), (__re__)[0]);
  return ([
    ((sp_equal)($errs, $core$Core$Nil)
      ? ($core$Result$Ok)($typedModule)
      : ($core$Result$Err)(($sd1$Compiler$Error$Nested)($errs))),
    $lastUnificationVarId,
  ]);
});

const $sd1$Compiler$CoreTypes$listDef = ((() => {
  const $usr = ($sd1$Compiler$CoreTypes$makeUsr)("List");
  const $item = ($sd1$Types$CanonicalAst$TypeAnnotationVariable)($sd1$Compiler$CoreTypes$p, "item");
  const $nilDef = ({
    ins: $core$Core$Nil,
    out: ($sd1$Compiler$CoreTypes$list)($item),
    pos: $sd1$Compiler$CoreTypes$p,
    typeUsr: $usr,
  });
  const $consDef = ({
    ins: ($core$Core$Cons)($item, ($core$Core$Cons)(($sd1$Compiler$CoreTypes$list)($item), $core$Core$Nil)),
    out: ($sd1$Compiler$CoreTypes$list)($item),
    pos: $sd1$Compiler$CoreTypes$p,
    typeUsr: $usr,
  });
  return ({
    constructors: ((($2) => {
      return ($core$Dict$insert)("Cons", $consDef, $2);
    }))(((($2) => {
      return ($core$Dict$insert)("Nil", $nilDef, $2);
    }))($core$Dict$empty)),
    directTypeDeps: $core$Set$empty,
    pars: ($core$Core$Cons)(($sd1$Types$Pos$At)($sd1$Types$Pos$G, "item"), $core$Core$Nil),
    usr: $usr,
  });
}))();

const $sd1$Compiler$CoreTypes$noneDef = ((() => {
  const $usr = ($sd1$Compiler$CoreTypes$makeUsr)($sd1$Compiler$CoreTypes$noneName);
  return ({
    constructors: ($core$Dict$ofOne)($sd1$Compiler$CoreTypes$noneName, ({
      ins: $core$Core$Nil,
      out: $sd1$Compiler$CoreTypes$none,
      pos: $sd1$Compiler$CoreTypes$p,
      typeUsr: $usr,
    })),
    directTypeDeps: $core$Set$empty,
    pars: $core$Core$Nil,
    usr: $usr,
  });
}))();

const $sd1$Compiler$CoreTypes$allDefs = ($core$Core$Cons)($sd1$Compiler$CoreTypes$noneDef, ($core$Core$Cons)($sd1$Compiler$CoreTypes$boolDef, ($core$Core$Cons)($sd1$Compiler$CoreTypes$listDef, ($core$Core$Cons)($sd1$Compiler$CoreTypes$textDef, ($core$Core$Cons)($sd1$Compiler$CoreTypes$numberDef, $core$Core$Nil)))));

const $sd1$Compiler$TypeCheck$addValueToGlobalEnv = (($state, $umr, $def, $env) => {
  const $nameToIdAndClasses = ((() => {
    const $zzzz = (($name, $5) => {
      const $allowFunctions = $5.allowFunctions;
      return ({
        first: ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]),
        second: ({
          allowFunctions: $allowFunctions,
          generalizedAt: $sd1$Types$Pos$G,
          generalizedFor: ($sd1$Types$Ast$RefLocal)(""),
          originalName: $name,
        }),
      });
    });
    return ($core$Dict$map)($zzzz, $def.tyvars);
  }))();
  const $nameToType = ($core$Dict$map)((($k, $5) => {
    const $id = $5.first;
    const $classes = $5.second;
    return ($sd1$Types$TypedAst$TypeVar)($id);
  }), $nameToIdAndClasses);
  const $tyvarIdToClasses = ($core$Dict$fromList)(($core$Dict$values)($nameToIdAndClasses));
  const $originalIdToNewIdAndUnivar = ((($1) => {
    return ($core$Dict$map)((($originalId, _1) => {
      return ({
        first: ((__re__ = ($sd1$Compiler$TypeCheck$newTyvarId)($state)), ($state = (__re__)[1]), (__re__)[0]),
        second: ({
          originalId: $originalId,
        }),
      });
    }), $1);
  }))($def.univars);
  const $originalIdToUniqueness = ((($1) => {
    return ($core$Dict$map)((($originalId, $6) => {
      const $newId = $6.first;
      const $univar = $6.second;
      return $newId;
    }), $1);
  }))($originalIdToNewIdAndUnivar);
  const $freeUnivars = ($core$Dict$fromList)(($core$Dict$values)($originalIdToNewIdAndUnivar));
  return ([
    ($core$Dict$for)($env, ($sd1$Types$CanonicalAst$patternNames)($def.pattern), (($valueName, $valueStuff, $envX) => {
      const $8 = $valueStuff.maybeAnnotation;
      return ((($8)[0] === "Nothing")
        ? $envX
        : ((($8)[0] === "Just")
          ? ((() => {
            const $annotation = ($8)[1];
            const $raw = ((__re__ = ($sd1$Compiler$TypeCheck$translateRawType)($env, $nameToType, $originalIdToUniqueness, $state, $annotation)), ($state = (__re__)[1]), (__re__)[0]);
            const $instance = ({
              definedAt: $valueStuff.pos,
              freeTyvars: ($core$Dict$intersect)($tyvarIdToClasses, ($sd1$Types$TypedAst$typeTyvars)($raw)),
              freeUnivars: $freeUnivars,
              type: ({
                raw: $raw,
                uni: $sd1$Types$Ast$Imm,
              }),
            });
            const $ref = ($sd1$Types$Ast$RefGlobal)(((($1) => {
              return ($sd1$Types$Meta$USR)($umr, $1);
            }))($valueName));
            const $0 = $envX;
            return (Object.assign)({}, $0, ({
              variables: ($core$Dict$insert)($ref, $instance, $0.variables),
            }));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2049:8', (sp_toHuman)($8))));
    })),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$addCoreValueToCoreEnv = (($state, $2, $env) => {
  const $nonFn = $2.nonFn;
  const $raw = $2.raw;
  const $usr = $2.usr;
  const $4 = $usr;
  const $name = ($4)[2];
  const $umr = ($4)[1];
  const $zzz = (($tyvarName, $pos) => {
    return ({
      allowFunctions: ($core$Basics$not)(($core$Dict$member)($tyvarName, $nonFn)),
    });
  });
  const $tyvars = ((($1) => {
    return ($core$Dict$map)($zzz, $1);
  }))(($sd1$Types$CanonicalAst$typeTyvars)($raw));
  return ([
    ((__re__ = ($sd1$Compiler$TypeCheck$addValueToGlobalEnv)($state, $umr, ({
      body: ($sd1$Types$CanonicalAst$LiteralText)($sd1$Types$Pos$N, $name),
      directConsDeps: $core$Dict$empty,
      directTypeDeps: $core$Dict$empty,
      directValueDeps: $core$Dict$empty,
      native: true,
      pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Types$Pos$N, ({
        maybeAnnotation: ($core$Maybe$Just)($raw),
        maybeName: ($core$Maybe$Just)($name),
      })),
      tyvars: $tyvars,
      uni: $sd1$Types$Ast$Imm,
      univars: $core$Dict$empty,
    }), $env)), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$addConstructorToGlobalEnv = (($state, $paramsByName, $freeTyvars, $name, $caConstructor, $env) => {
  const $7 = $caConstructor.typeUsr;
  const $umr = ($7)[1];
  const $ins = ((($1) => {
    return ($core$List$map)((($in) => {
      return ($sd1$Types$CanonicalAst$ParSp)(({
        raw: $in,
        uni: ($sd1$Types$Ast$Depends)(1),
      }));
    }), $1);
  }))($caConstructor.ins);
  const $caRaw = ((sp_equal)($ins, $core$Core$Nil)
    ? $caConstructor.out
    : ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$G, $ins, ({
      raw: $caConstructor.out,
      uni: ($sd1$Types$Ast$Depends)(1),
    })));
  const $raw = ((__re__ = ($sd1$Compiler$TypeCheck$translateRawType)($env, $paramsByName, $core$Dict$empty, $state, $caRaw)), ($state = (__re__)[1]), (__re__)[0]);
  const $consTyvars = ($sd1$Types$TypedAst$typeTyvars)($raw);
  const $taConstructor = ({
    definedAt: $sd1$Types$Pos$G,
    freeTyvars: ((($1) => {
      return ($core$Dict$filter)((($k, $v) => {
        return ($core$Dict$member)($k, $consTyvars);
      }), $1);
    }))($freeTyvars),
    freeUnivars: ($core$Dict$ofOne)(1, ({
      originalId: 1,
    })),
    type: ($sd1$Types$Ast$toImm)($raw),
  });
  const $0 = $env;
  return ([
    (Object.assign)({}, $0, ({
      constructors: ($core$Dict$insert)(($sd1$Types$Meta$USR)($umr, $name), $taConstructor, $0.constructors),
    })),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$addUnionTypeAndConstructorsToGlobalEnv = (($state, _1, $caUnionDef, $env) => {
  const $paramsByName = ($core$Dict$fromList)(((($1) => {
    return ($core$List$indexedMap)((($index, $6) => {
      const $pos = ($6)[1];
      const $name = ($6)[2];
      return ({
        first: $name,
        second: ($sd1$Types$TypedAst$TypeVar)(-($index)),
      });
    }), $1);
  }))($caUnionDef.pars));
  const $makeTyvar = (($index, $5) => {
    const $pos = ($5)[1];
    const $name = ($5)[2];
    return ({
      first: -($index),
      second: ({
        allowFunctions: true,
        generalizedAt: $sd1$Types$Pos$G,
        generalizedFor: ($sd1$Types$Ast$RefLocal)(""),
        originalName: $name,
      }),
    });
  });
  const $freeTyvars = ($core$Dict$fromList)(((($1) => {
    return ($core$List$indexedMap)($makeTyvar, $1);
  }))($caUnionDef.pars));
  return ([
    ((($0) => {
      return ($core$Dict$for)($0, $caUnionDef.constructors, (($3, $4, $5) => {
        return ((__re__ = ($sd1$Compiler$TypeCheck$addConstructorToGlobalEnv)($state, $paramsByName, $freeTyvars, $3, $4, $5)), ($state = (__re__)[1]), (__re__)[0]);
      }));
    }))(((() => {
      const $0 = $env;
      return (Object.assign)({}, $0, ({
        exactTypes: ($core$Dict$insert)($caUnionDef.usr, $caUnionDef.pars, $0.exactTypes),
      }));
    }))()),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$initEnv = ({
  annotatedTyvarsByName: $core$Dict$empty,
  annotatedUnivarsByOriginalId: $core$Dict$empty,
  constructors: $core$Dict$empty,
  context: $sd1$Compiler$TypeCheck$Context_Global,
  errorModule: ({
    content: "",
    fsPath: "<internal>",
  }),
  exactTypes: $core$Dict$empty,
  expandedAliases: $core$Dict$empty,
  variables: $core$Dict$empty,
});

const $sd1$Compiler$TypeCheck$namedParsToIdParsAndDict = (($atPars) => {
  const $idPars = ((($1) => {
    return ($core$List$indexedMap)((($index, $atName) => {
      return -($index);
    }), $1);
  }))($atPars);
  const $typeByName = ($core$Dict$fromList)(((($1) => {
    return ($core$List$indexedMap)((($index, $3) => {
      const $pos = ($3)[1];
      const $name = ($3)[2];
      return ({
        first: $name,
        second: ($sd1$Types$TypedAst$TypeVar)(-($index)),
      });
    }), $1);
  }))($atPars));
  return ({
    first: $idPars,
    second: $typeByName,
  });
});

const $sd1$Compiler$TypeCheck$expandAndInsertAlias = (($state, $allAliases, $usr, $aliasAccum) => {
  const $aliasDef = ((() => {
    const $5 = ($core$Dict$get)($usr, $allAliases);
    return ((($5)[0] === "Just")
      ? ((() => {
        const $def = ($5)[1];
        return $def;
      }))()
      : ((($5)[0] === "Nothing")
        ? ($sd1$Compiler$TypeCheck$bug)("alias not found")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2154:8', (sp_toHuman)($5))));
  }))();
  const $5 = ($sd1$Compiler$TypeCheck$namedParsToIdParsAndDict)($aliasDef.pars);
  const $typeByName = $5.second;
  const $pars = $5.first;
  const $originalIdToNewId = $core$Dict$empty;
  const $type = ((__re__ = ($sd1$Compiler$TypeCheck$translateRawType)(((() => {
    const $0 = $sd1$Compiler$TypeCheck$initEnv;
    return (Object.assign)({}, $0, ({
      expandedAliases: $aliasAccum,
    }));
  }))(), $typeByName, $originalIdToNewId, $state, $aliasDef.type)), ($state = (__re__)[1]), (__re__)[0]);
  return ([
    ($core$Dict$insert)($usr, ({
      pars: $pars,
      type: $type,
    }), $aliasAccum),
    $state,
  ]);
});

const $sd1$Compiler$TypeCheck$getAliasDependencies = (($allAliases, $aliasDef) => {
  return ((($1) => {
    return ($core$Dict$map)((($k, $v) => {
      return null;
    }), $1);
  }))(((($1) => {
    return ($core$Dict$filter)((($usr, _1) => {
      return ($core$Dict$member)($usr, $allAliases);
    }), $1);
  }))($aliasDef.directTypeDeps));
});

const $sd1$Prelude$compare = ({
  nonFn: ($core$Core$Cons)("a", $core$Core$Nil),
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)(($sd1$Prelude$tyVar)("a"), ($core$Core$Cons)(($sd1$Prelude$tyVar)("a"), $core$Core$Nil)), $sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$coreUsr)("compare"),
});

const $sd1$Prelude$debugUsr = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Debug"), $1);
});

const $sd1$Prelude$debugBenchStart = ({
  nonFn: $core$Core$Nil,
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)($sd1$Compiler$CoreTypes$none, $core$Core$Nil), $sd1$Compiler$CoreTypes$none),
  usr: ($sd1$Prelude$debugUsr)("benchStart"),
});

const $sd1$Prelude$debugBenchStop = ({
  nonFn: $core$Core$Nil,
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)($sd1$Compiler$CoreTypes$text, $core$Core$Nil), $sd1$Compiler$CoreTypes$none),
  usr: ($sd1$Prelude$debugUsr)("benchStop"),
});

const $sd1$Prelude$debugLog = ({
  nonFn: $core$Core$Nil,
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)($sd1$Compiler$CoreTypes$text, ($core$Core$Cons)(($sd1$Prelude$tyVar)("a"), $core$Core$Nil)), ($sd1$Prelude$tyVar)("a")),
  usr: ($sd1$Prelude$debugUsr)("log"),
});

const $sd1$Prelude$debugToHuman = ({
  nonFn: $core$Core$Nil,
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)(($sd1$Prelude$tyVar)("a"), $core$Core$Nil), $sd1$Compiler$CoreTypes$text),
  usr: ($sd1$Prelude$debugUsr)("toHuman"),
});

const $sd1$Prelude$debugTodo = ({
  nonFn: $core$Core$Nil,
  type: ($sd1$Types$CanonicalAst$TypeFn)($sd1$Types$Pos$N, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParSp)(({
    raw: $sd1$Compiler$CoreTypes$text,
    uni: $sd1$Types$Ast$Imm,
  })), $core$Core$Nil), ({
    raw: ($sd1$Types$CanonicalAst$TypeAnnotationVariable)($sd1$Types$Pos$N, "a"),
    uni: $sd1$Types$Ast$Uni,
  })),
  usr: ($sd1$Prelude$debugUsr)("todo"),
});

const $sd1$Prelude$functions = ($core$Core$Cons)($sd1$Prelude$compare, ($core$Core$Cons)($sd1$Prelude$debugTodo, ($core$Core$Cons)($sd1$Prelude$debugLog, ($core$Core$Cons)($sd1$Prelude$debugToHuman, ($core$Core$Cons)($sd1$Prelude$debugBenchStart, ($core$Core$Cons)($sd1$Prelude$debugBenchStop, $core$Core$Nil))))));

const $sd1$Prelude$insertInModule = (($usr, $raw, $nonFnAsList, $list) => {
  const $nonFn = ($core$Set$fromList)($nonFnAsList);
  return ($core$Core$Cons)(({
    nonFn: $nonFn,
    raw: $raw,
    usr: $usr,
  }), $list);
});

const $sd1$Prelude$insertBinop = (($binop, $m) => {
  return ($sd1$Prelude$insertInModule)($binop.usr, $binop.type, $binop.nonFn, $m);
});

const $sd1$Prelude$insertFunction = (($function, $m) => {
  return ($sd1$Prelude$insertInModule)($function.usr, $function.type, $function.nonFn, $m);
});

const $sd1$Prelude$insertUnop = (($unop, $m) => {
  return ($sd1$Prelude$insertInModule)($unop.usr, $unop.type, $core$Core$Nil, $m);
});

const $sd1$Prelude$unaryPlus = ({
  symbol: "0 +",
  type: ($sd1$Prelude$tyFn)(($core$Core$Cons)($sd1$Compiler$CoreTypes$number, $core$Core$Nil), $sd1$Compiler$CoreTypes$number),
  usr: ($sd1$Prelude$numberUsr)("unaryPlus"),
});

const $sd1$Prelude$allCoreValues = ((($0) => {
  return ($core$List$for)($0, $sd1$Prelude$functions, $sd1$Prelude$insertFunction);
}))(((($0) => {
  return ($core$List$for)($0, $sd1$Prelude$binops, $sd1$Prelude$insertBinop);
}))(((($1) => {
  return ($sd1$Prelude$insertUnop)($sd1$Prelude$unaryMinus, $1);
}))(((($1) => {
  return ($sd1$Prelude$insertUnop)($sd1$Prelude$unaryPlus, $1);
}))($core$Core$Nil))));

const $sd1$Compiler$TypeCheck$initStateAndGlobalEnv = (($externalValues, $allModules) => {
  let $state = ($sd1$Compiler$TypeCheck$initState)(0);
  const $allAliases = ((($0) => {
    return ($core$List$for)($0, $allModules, (($mod, $zz) => {
      return ($core$Dict$for)($zz, $mod.aliasDefs, (($name, $aliasDef, $d) => {
        return ($core$Dict$insert)($aliasDef.usr, $aliasDef, $d);
      }));
    }));
  }))($core$Dict$empty);
  const $3 = ($sd1$RefHierarchy$reorder)((($1) => {
    return ($sd1$Compiler$TypeCheck$getAliasDependencies)($allAliases, $1);
  }), $allAliases);
  const $orderedAliases = $3.second;
  const $circulars = $3.first;
  ((sp_not_equal)($circulars, $core$Core$Nil)
    ? ((() => {
      (sp_log)("=========> ERROR circulars aliases!!", $circulars);
      return ($core$List$each)($circulars, (($circular) => {
        return ((__re__ = (array_push)($state.errors, ({
          first: $sd1$Types$Pos$G,
          second: $sd1$Compiler$TypeCheck$Context_Global,
          third: ($sd1$Compiler$TypeCheck$ErrorCircularAlias)($circular),
        }))), ($state.errors = (__re__)[1]), (__re__)[0]);
      }));
    }))()
    : null);
  const $expandedAliases = ((($0) => {
    return ($core$List$for)($0, $orderedAliases, (($2, $3) => {
      return ((__re__ = ($sd1$Compiler$TypeCheck$expandAndInsertAlias)($state, $allAliases, $2, $3)), ($state = (__re__)[1]), (__re__)[0]);
    }));
  }))($core$Dict$empty);
  const $doStuff = (($caModule, $env) => {
    return ((($0) => {
      return ($core$Dict$for)($0, $caModule.valueDefs, (($pattern, $v, $a) => {
        return ((__re__ = ($sd1$Compiler$TypeCheck$addValueToGlobalEnv)($state, $caModule.umr, $v, $a)), ($state = (__re__)[1]), (__re__)[0]);
      }));
    }))(((($0) => {
      return ($core$Dict$for)($0, $caModule.unionDefs, (($1, $2, $3) => {
        return ((__re__ = ($sd1$Compiler$TypeCheck$addUnionTypeAndConstructorsToGlobalEnv)($state, $1, $2, $3)), ($state = (__re__)[1]), (__re__)[0]);
      }));
    }))($env));
  });
  const $env = ((($0) => {
    return ($core$List$for)($0, $allModules, $doStuff);
  }))(((($0) => {
    return ($core$List$for)($0, $externalValues, (($6, $e) => {
      const $usr = $6.first;
      const $instance = $6.second;
      const $0 = $e;
      return (Object.assign)({}, $0, ({
        variables: ($core$Dict$insert)(($sd1$Types$Ast$RefGlobal)($usr), $instance, $0.variables),
      }));
    }));
  }))(((($0) => {
    return ($core$List$for)($0, $sd1$Prelude$allCoreValues, (($1, $2) => {
      return ((__re__ = ($sd1$Compiler$TypeCheck$addCoreValueToCoreEnv)($state, $1, $2)), ($state = (__re__)[1]), (__re__)[0]);
    }));
  }))(((($0) => {
    return ($core$List$for)($0, $sd1$Compiler$CoreTypes$allDefs, (($2, $3) => {
      return ((__re__ = ($sd1$Compiler$TypeCheck$addUnionTypeAndConstructorsToGlobalEnv)($state, null, $2, $3)), ($state = (__re__)[1]), (__re__)[0]);
    }));
  }))(((() => {
    const $0 = $sd1$Compiler$TypeCheck$initEnv;
    return (Object.assign)({}, $0, ({
      expandedAliases: $expandedAliases,
    }));
  }))()))));
  const $4 = ((__re__ = (array_toList)($state.errors)), ($state.errors = (__re__)[1]), (__re__)[0]);
  return ((($4)[0] === "Nil")
    ? ($core$Result$Ok)(({
      first: $state.lastUnificationVarId,
      second: $env,
    }))
    : (true
      ? ((() => {
        const $list = $4;
        return ($core$Result$Err)(($sd1$Compiler$Error$Nested)(((($1) => {
          return ($core$List$map)((($1) => {
            return ($sd1$Compiler$TypeCheck$makeInferenceAndCheckError)($env, $1);
          }), $1);
        }))($list)));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2225:4', (sp_toHuman)($4))));
});

const $sd1$Compiler$UniquenessCheck$addPatternToEnv = (($state, $pattern, $env) => {
  const $names = ($sd1$Types$TypedAst$patternNames)($pattern);
  const $insertVariable = (($name, $5, $z) => {
    const $pos = $5.pos;
    const $type = $5.type;
    const $mode = ((sp_equal)($type.uni, $sd1$Types$Ast$Imm)
      ? $sd1$Compiler$UniquenessCheck$Immutable
      : ($sd1$Compiler$UniquenessCheck$Unique)($sd1$Compiler$UniquenessCheck$Available));
    const $variable = ({
      definedAt: $pos,
      mode: $mode,
      name: $name,
      required: $core$Dict$empty,
      type: $type,
    });
    return ($core$Dict$insert)($name, $variable, $z);
  });
  const $localEnv = ((() => {
    const $0 = $env;
    return (Object.assign)({}, $0, ({
      variables: ($core$Dict$for)($0.variables, $names, $insertVariable),
    }));
  }))();
  const $uniques = ((($1) => {
    return ($core$Dict$map)((($n, $s) => {
      return $s.pos;
    }), $1);
  }))(((($1) => {
    return ($core$Dict$filter)((($n, $s) => {
      return (sp_not_equal)($s.type.uni, $sd1$Types$Ast$Imm);
    }), $1);
  }))($names));
  return ([
    ({
      first: ($core$Dict$keys)($names),
      second: $uniques,
      third: $localEnv,
    }),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$consumeInEnv = (($spent, $env) => {
  const $translate = (($name, $variable) => {
    const $5 = ($core$Dict$get)($name, $spent);
    return ((($5)[0] === "Nothing")
      ? $variable
      : ((($5)[0] === "Just")
        ? ((() => {
          const $pos = ($5)[1];
          const $0 = $variable;
          return (Object.assign)({}, $0, ({
            mode: ($sd1$Compiler$UniquenessCheck$Unique)(($sd1$Compiler$UniquenessCheck$ConsumedAt)($pos)),
          }));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 265:12', (sp_toHuman)($5))));
  });
  const $0 = $env;
  return (Object.assign)({}, $0, ({
    variables: ($core$Dict$map)($translate, $0.variables),
  }));
});

const $sd1$Compiler$UniquenessCheck$addError = (($env, $pos, $state, $messageConstructor) => {
  return ([
    ((__re__ = (array_push)($state.errors, ($sd1$Compiler$Error$Simple)($env.errorModule, $pos, $messageConstructor))), ($state.errors = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorMutatingAConsumed = (($env, $name, $p2, $p1, $state) => {
  const $6 = ($sd1$Compiler$Error$posToHuman)($env.errorModule, $p2);
  const $location = $6.location;
  const $block = $6.block;
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $p1, $state, ($core$Core$Cons)(("This code spends the unique variable `" + ($name + ("`, but `" + ($name + "` is being used again here:")))), ($core$Core$Cons)("", ($core$Core$Cons)($block, ($core$Core$Cons)("", ($core$Core$Cons)("If you want to use a unique more than once, you need to use a function that recycles it.", ($core$Core$Cons)("TODO: link to uniqueness wiki page", $core$Core$Nil)))))))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorMutatingAnImmutable = (($env, $name, $p, $state) => {
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $p, $state, ($core$Core$Cons)(($name + " is immutable, but you are trying to mutate it"), $core$Core$Nil))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorMutatingTwice = (($env, $name, $p1, $p2, $state) => {
  const $6 = ($sd1$Compiler$Error$posToHuman)($env.errorModule, $p2);
  const $location = $6.location;
  const $block = $6.block;
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $p1, $state, ($core$Core$Cons)(($name + " is already being mutated here: "), ($core$Core$Cons)($block, ($core$Core$Cons)("You can't use the same unique twice in the same function call", ($core$Core$Cons)("TODO: link to wiki explaining why", $core$Core$Nil)))))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorReferencingConsumedVariable = (($env, $name, $pos, $consumedPos, $state) => {
  const $6 = ($sd1$Compiler$Error$posToHuman)($env.errorModule, $pos);
  const $location = $6.location;
  const $block = $6.block;
  const $cons = ($sd1$Compiler$Error$posToHuman)($env.errorModule, $consumedPos);
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $pos, $state, ($core$Core$Cons)(("You can't reference again the variable `" + ($name + "` because it was used already here:")), ($core$Core$Cons)($cons.block, $core$Core$Nil)))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorUndefinedVariable = (($env, $p, $name, $state) => {
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $p, $state, ($core$Core$Cons)(("undefined variable: " + $name), $core$Core$Nil))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$doArgument = (($env, $state, $pos, $doneSoFar) => {
  const $5 = $doneSoFar.resolved;
  return ([
    ((($5)[0] === "ArgumentExpression")
      ? ((() => {
        const $fullType = ($5)[1];
        const $expr = ($5)[2];
        const $doneExpression = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $expr)), ($state = (__re__)[1]), (__re__)[0]);
        ($core$Dict$each)($doneExpression.spent, (($name, $p1) => {
          const $8 = ($core$Dict$get)($name, $doneSoFar.spent);
          return ((($8)[0] === "Nothing")
            ? null
            : ((($8)[0] === "Just")
              ? ((() => {
                const $p2 = ($8)[1];
                return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorReferencingConsumedVariable)($env, $name, $p1, $p2, $state)), ($state = (__re__)[1]), (__re__)[0]);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 374:16', (sp_toHuman)($8))));
        }));
        return ({
          recycled: $doneSoFar.recycled,
          required: ($core$Dict$join)($doneExpression.required, $doneSoFar.required),
          resolved: ($sd1$Types$TypedAst$ArgumentExpression)($fullType, $doneExpression.resolved),
          spent: ($core$Dict$join)($doneExpression.spent, $doneSoFar.spent),
        });
      }))()
      : ((($5)[0] === "ArgumentRecycle")
        ? ((() => {
          const $p1 = ($5)[1];
          const $rawType = ($5)[2];
          const $attrPath = ($5)[3];
          const $name = ($5)[4];
          const $x = ((() => {
            const $6 = ($core$Dict$get)($name, $env.variables);
            return ((($6)[0] === "Nothing")
              ? ((__re__ = ($sd1$Compiler$UniquenessCheck$errorUndefinedVariable)($env, $p1, $name, $state)), ($state = (__re__)[1]), (__re__)[0])
              : ((($6)[0] === "Just")
                ? ((() => {
                  const $variable = ($6)[1];
                  const $7 = $variable.mode;
                  return (((($7)[0] === "Unique") && ((($7)[1])[0] === "Available"))
                    ? null
                    : ((($7)[0] === "Immutable")
                      ? ((__re__ = ($sd1$Compiler$UniquenessCheck$errorMutatingAnImmutable)($env, $name, $p1, $state)), ($state = (__re__)[1]), (__re__)[0])
                      : (((($7)[0] === "Unique") && ((($7)[1])[0] === "ConsumedAt"))
                        ? ((() => {
                          const $p2 = (($7)[1])[1];
                          return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorMutatingAConsumed)($env, $name, $p1, $p2, $state)), ($state = (__re__)[1]), (__re__)[0]);
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 394:20', (sp_toHuman)($7)))));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 391:14', (sp_toHuman)($6))));
          }))();
          const $y = ((() => {
            const $6 = ($core$Dict$get)($name, $doneSoFar.recycled);
            return ((($6)[0] === "Nothing")
              ? null
              : ((($6)[0] === "Just")
                ? ((() => {
                  const $p2 = ($6)[1];
                  return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorMutatingTwice)($env, $name, $p1, $p2, $state)), ($state = (__re__)[1]), (__re__)[0]);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 400:14', (sp_toHuman)($6))));
          }))();
          const $0 = $doneSoFar;
          return (Object.assign)({}, $0, ({
            recycled: ($core$Dict$insert)($name, $p1, $doneSoFar.recycled),
            resolved: ($sd1$Types$TypedAst$ArgumentRecycle)($p1, $rawType, $attrPath, $name),
          }));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 366:4', (sp_toHuman)($5)))),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorTaintedCallRecyclesFunctions = (($env, $callPos, $name, $required, $state) => {
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $callPos, $state, ($core$Core$Cons)(("This function call could allow some unique values (" + (((($1) => {
      return ($core$Text$join)(", ", $1);
    }))(($core$Dict$keys)($required)) + ")")), ($core$Core$Cons)(("to be recycled by a functions contained in the argument `" + ($name + "` outside of the scope where they were declared.")), ($core$Core$Cons)("This would be BAD. [TODO link to wiki]", ($core$Core$Cons)("TODO improve this explanation.", $core$Core$Nil)))))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$uniOutInit = (($a) => {
  return ({
    recycled: $core$Dict$empty,
    required: $core$Dict$empty,
    resolved: $a,
    spent: $core$Dict$empty,
  });
});

const $sd1$Compiler$UniquenessCheck$uniOutMap = (($f, $1) => {
  const $recycled = $1.recycled;
  const $required = $1.required;
  const $resolved = $1.resolved;
  const $spent = $1.spent;
  return ({
    recycled: $recycled,
    required: $required,
    resolved: ($f)($resolved),
    spent: $spent,
  });
});

const $sd1$Types$TypedAst$typeAllowsFunctions = (($testId, $type) => {
  return ((($type)[0] === "TypeFn")
    ? ((() => {
      const $ins = ($type)[1];
      const $out = ($type)[2];
      return true;
    }))()
    : ((($type)[0] === "TypeVar")
      ? ((() => {
        const $id = ($type)[1];
        return ($testId)($id);
      }))()
      : ((($type)[0] === "TypeExact")
        ? ((() => {
          const $usr = ($type)[1];
          const $args = ($type)[2];
          return ($core$List$any)((($1) => {
            return ($sd1$Types$TypedAst$typeAllowsFunctions)($testId, $1);
          }), $args);
        }))()
        : ((($type)[0] === "TypeRecord")
          ? ((() => {
            const $attrs = ($type)[2];
            return ($core$Dict$any)((($k, $v) => {
              return ($sd1$Types$TypedAst$typeAllowsFunctions)($testId, $v);
            }), $attrs);
          }))()
          : ((($type)[0] === "TypeError")
            ? true
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/TypedAst.sp 355:4', (sp_toHuman)($type)))))));
});

const $sd1$Compiler$UniquenessCheck$doCall = (($env, $state, $pos, $reference, $arguments) => {
  const $doneReference = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $reference)), ($state = (__re__)[1]), (__re__)[0]);
  const $doneArgs = ((($0) => {
    return ($core$List$forReversed)($0, $arguments, (($arg, $acc) => {
      return ((($1) => {
        return ($sd1$Compiler$UniquenessCheck$uniOutMap)((($resolvedArg) => {
          return (sp_cons)($resolvedArg, $acc.resolved);
        }), $1);
      }))(((($3) => {
        return ((__re__ = ($sd1$Compiler$UniquenessCheck$doArgument)($env, $state, $pos, $3)), ($state = (__re__)[1]), (__re__)[0]);
      }))(((($1) => {
        return ($sd1$Compiler$UniquenessCheck$uniOutMap)(((_0) => {
          return $arg;
        }), $1);
      }))($acc)));
    }));
  }))(($sd1$Compiler$UniquenessCheck$uniOutInit)($core$Core$Nil));
  const $asRecyclingFunction = (($arg) => {
    return ((($arg)[0] === "ArgumentRecycle")
      ? ((() => {
        const $p = ($arg)[1];
        const $raw = ($arg)[2];
        const $path = ($arg)[3];
        const $name = ($arg)[4];
        return (($sd1$Types$TypedAst$typeAllowsFunctions)((($tyvarId) => {
          return false;
        }), $raw)
          ? ($core$Maybe$Just)($name)
          : $core$Maybe$Nothing);
      }))()
      : ((($arg)[0] === "ArgumentExpression")
        ? $core$Maybe$Nothing
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 331:8', (sp_toHuman)($arg))));
  });
  (((sp_not_equal)($doneArgs.required, $core$Dict$empty) || (sp_not_equal)($doneReference.required, $core$Dict$empty))
    ? ($core$List$each)(($core$List$filterMap)($asRecyclingFunction, $arguments), (($name) => {
      return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorTaintedCallRecyclesFunctions)($env, $pos, $name, ($core$Dict$join)($doneArgs.required, $doneReference.required), $state)), ($state = (__re__)[1]), (__re__)[0]);
    }))
    : null);
  return ([
    ({
      recycled: ($core$Dict$join)($doneReference.recycled, $doneArgs.recycled),
      required: $doneArgs.required,
      resolved: ($sd1$Types$TypedAst$Call)($pos, $doneReference.resolved, $doneArgs.resolved),
      spent: ($core$Dict$join)($doneReference.spent, $doneArgs.spent),
    }),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$doParameter = (($state, $par, $acc) => {
  return ([
    ((($par)[0] === "ParameterPattern")
      ? ((() => {
        const $fullType = ($par)[1];
        const $pa = ($par)[2];
        const $4 = ((__re__ = ($sd1$Compiler$UniquenessCheck$addPatternToEnv)($state, $pa, $acc.localEnv)), ($state = (__re__)[1]), (__re__)[0]);
        const $localEnv = $4.third;
        const $uniques = $4.second;
        const $addedVars = $4.first;
        const $0 = $acc;
        return (Object.assign)({}, $0, ({
          localEnv: $localEnv,
          parsToBeSpent: ($core$Dict$join)($uniques, $0.parsToBeSpent),
        }));
      }))()
      : ((($par)[0] === "ParameterRecycle")
        ? ((() => {
          const $pos = ($par)[1];
          const $rawType = ($par)[2];
          const $name = ($par)[3];
          const $var = ({
            definedAt: $pos,
            mode: ($sd1$Compiler$UniquenessCheck$Unique)($sd1$Compiler$UniquenessCheck$Available),
            name: $name,
            required: $core$Dict$empty,
            type: ({
              raw: $rawType,
              uni: $sd1$Types$Ast$Uni,
            }),
          });
          const $0 = $acc;
          return (Object.assign)({}, $0, ({
            localEnv: ((() => {
              const $1 = $acc.localEnv;
              return (Object.assign)({}, $1, ({
                variables: ($core$Dict$insert)($name, $var, $1.variables),
              }));
            }))(),
            parsToBeRecycled: ($core$Dict$insert)($name, $pos, $0.parsToBeRecycled),
          }));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 423:4', (sp_toHuman)($par)))),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorConsumingRecycledParameters = (($env, $pos, $spentThatShouldHaveBeenRecycled, $state) => {
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $pos, $state, ($core$Core$Cons)("errorConsumingRecycledParameters", ($core$Core$Cons)((sp_toHuman)($spentThatShouldHaveBeenRecycled), $core$Core$Nil)))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorFunctionsCannotConsumeParentUniques = (($env, $functionPos, $spentFromParent, $state) => {
  const $zzz = (($5) => {
    const $name = $5.first;
    const $spentPos = $5.second;
    const $6 = ($sd1$Compiler$Error$posToHuman)($env.errorModule, $spentPos);
    const $location = $6.location;
    const $block = $6.block;
    return ($block + "\n");
  });
  const $blocks = ((($1) => {
    return ($core$List$map)($zzz, $1);
  }))(((($1) => {
    return (list_sortBy)($core$Tuple$second, $1);
  }))(($core$Dict$toList)($spentFromParent)));
  return ([
    ((($3) => {
      return ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $functionPos, $state, $3)), ($state = (__re__)[1]), (__re__)[0]);
    }))(($core$List$concat)(($core$Core$Cons)(($core$Core$Cons)(("This function is spending the unique variable `" + (((($1) => {
      return ($core$Text$join)("`, `", $1);
    }))(($core$Dict$keys)($spentFromParent)) + "`")), $core$Core$Nil), ($core$Core$Cons)(($core$Core$Cons)("", $core$Core$Nil), ($core$Core$Cons)($blocks, ($core$Core$Cons)(($core$Core$Cons)("However, functions cannot spend uniques that were declared outside their body.", $core$Core$Nil), $core$Core$Nil)))))),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope = (($env, $name, $2, $state) => {
  const $fnPos = $2.fnPos;
  const $usedAt = $2.usedAt;
  const $5 = ($sd1$Compiler$Error$posToHuman)($env.errorModule, $usedAt);
  const $location = $5.location;
  const $block = $5.block;
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $fnPos, $state, ($core$Core$Cons)(("This expression needs to access the unique variable `" + ($name + "` because it uses it here:")), ($core$Core$Cons)("", ($core$Core$Cons)($block, ($core$Core$Cons)("", ($core$Core$Cons)(("The problem is that returning a function from the expression could allow accessing `" + ($name + ("` from outside of where `" + ($name + "` was declared.")))), ($core$Core$Cons)("This would be BAD. [TODO link to wiki]", $core$Core$Nil)))))))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$doFn = (($env, $pos, $state, $pars, $body, $bodyType) => {
  const $7 = ((($0) => {
    return ($core$List$for)($0, $pars, (($1, $2) => {
      return ((__re__ = ($sd1$Compiler$UniquenessCheck$doParameter)($state, $1, $2)), ($state = (__re__)[1]), (__re__)[0]);
    }));
  }))(({
    localEnv: $env,
    parsToBeRecycled: $core$Dict$empty,
    parsToBeSpent: $core$Dict$empty,
  }));
  const $parsToBeSpent = $7.parsToBeSpent;
  const $parsToBeRecycled = $7.parsToBeRecycled;
  const $localEnv = $7.localEnv;
  const $doneBody = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($localEnv, $state, $body)), ($state = (__re__)[1]), (__re__)[0]);
  const $exprWithDestruction = ((($0) => {
    return ($core$Dict$for)($0, $parsToBeSpent, (($name, _1, $exp) => {
      return (($core$Dict$member)($name, $doneBody.spent)
        ? $exp
        : ($sd1$Types$TypedAst$DestroyIn)($name, $exp));
    }));
  }))($doneBody.resolved);
  const $spentThatShouldHaveBeenRecycled = ($core$Dict$intersect)($doneBody.spent, $parsToBeRecycled);
  ((sp_not_equal)($spentThatShouldHaveBeenRecycled, $core$Dict$empty)
    ? ((__re__ = ($sd1$Compiler$UniquenessCheck$errorConsumingRecycledParameters)($env, $pos, $spentThatShouldHaveBeenRecycled, $state)), ($state = (__re__)[1]), (__re__)[0])
    : null);
  const $spentFromParent = ($core$Dict$diff)($doneBody.spent, $parsToBeSpent);
  (((sp_equal)($spentThatShouldHaveBeenRecycled, $core$Dict$empty) && (sp_not_equal)($spentFromParent, $core$Dict$empty))
    ? ((__re__ = ($sd1$Compiler$UniquenessCheck$errorFunctionsCannotConsumeParentUniques)($env, $pos, $spentFromParent, $state)), ($state = (__re__)[1]), (__re__)[0])
    : null);
  const $required = ((($0) => {
    return ($core$Dict$diff)($0, $parsToBeRecycled);
  }))(((($1) => {
    return ($core$Dict$join)($doneBody.required, $1);
  }))(((($1) => {
    return ($core$Dict$map)((($k, $usedAt) => {
      return ({
        fnPos: $pos,
        usedAt: $usedAt,
      });
    }), $1);
  }))($doneBody.recycled)));
  (($sd1$Types$TypedAst$typeAllowsFunctions)((($tyvarId) => {
    return false;
  }), $bodyType.raw)
    ? ($core$Dict$each)(($core$Dict$join)($parsToBeRecycled, $parsToBeSpent), (($varName, $parPos) => {
      const $10 = ($core$Dict$get)($varName, $doneBody.required);
      return ((($10)[0] === "Nothing")
        ? null
        : ((($10)[0] === "Just")
          ? ((() => {
            const $r = ($10)[1];
            return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope)($env, $varName, $r, $state)), ($state = (__re__)[1]), (__re__)[0]);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 771:12', (sp_toHuman)($10))));
    }))
    : null);
  return ([
    ({
      recycled: ($core$Dict$diff)($doneBody.recycled, $parsToBeRecycled),
      required: $required,
      resolved: ($sd1$Types$TypedAst$Fn)($pos, $pars, $exprWithDestruction, $bodyType),
      spent: $core$Dict$empty,
    }),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$requireInEnv = (($varNames, $required, $env) => {
  const $0 = $env;
  return (Object.assign)({}, $0, ({
    variables: ((($0) => {
      return ($core$List$for)($0, $varNames, (($name, $a) => {
        return ($core$Dict$update)($name, (($1) => {
          return ($core$Maybe$map)((($var) => {
            const $1 = $var;
            return (Object.assign)({}, $1, ({
              required: $required,
            }));
          }), $1);
        }), $a);
      }));
    }))($0.variables),
  }));
});

const $sd1$Compiler$UniquenessCheck$doExpression = (($env, $state, $expression) => {
  const $re = ($sd1$Compiler$UniquenessCheck$uniOutInit)($expression);
  return ([
    ((($expression)[0] === "LiteralText")
      ? ((() => {
        const $pos = ($expression)[1];
        const $l = ($expression)[2];
        return $re;
      }))()
      : ((($expression)[0] === "LiteralNumber")
        ? ((() => {
          const $pos = ($expression)[1];
          const $l = ($expression)[2];
          return $re;
        }))()
        : (((($expression)[0] === "Variable") && ((($expression)[2])[0] === "RefGlobal"))
          ? ((() => {
            const $pos = ($expression)[1];
            return $re;
          }))()
          : (((($expression)[0] === "Variable") && ((($expression)[2])[0] === "RefLocal"))
            ? ((() => {
              const $pos = ($expression)[1];
              const $name = (($expression)[2])[1];
              const $4 = ($core$Dict$get)($name, $env.variables);
              return ((($4)[0] === "Nothing")
                ? ((() => {
                  ((__re__ = ($sd1$Compiler$UniquenessCheck$errorUndefinedVariable)($env, $pos, $name, $state)), ($state = (__re__)[1]), (__re__)[0]);
                  return $re;
                }))()
                : ((($4)[0] === "Just")
                  ? ((() => {
                    const $variable = ($4)[1];
                    const $5 = $variable.mode;
                    return ((($5)[0] === "Immutable")
                      ? ({
                        recycled: $core$Dict$empty,
                        required: $variable.required,
                        resolved: $expression,
                        spent: $core$Dict$empty,
                      })
                      : (((($5)[0] === "Unique") && ((($5)[1])[0] === "Available"))
                        ? ({
                          recycled: $core$Dict$empty,
                          required: $variable.required,
                          resolved: $expression,
                          spent: ($core$Dict$ofOne)($name, $pos),
                        })
                        : (((($5)[0] === "Unique") && ((($5)[1])[0] === "ConsumedAt"))
                          ? ((() => {
                            const $consumedPos = (($5)[1])[1];
                            ((__re__ = ($sd1$Compiler$UniquenessCheck$errorReferencingConsumedVariable)($env, $name, $pos, $consumedPos, $state)), ($state = (__re__)[1]), (__re__)[0]);
                            return ({
                              recycled: $core$Dict$empty,
                              required: $variable.required,
                              resolved: $expression,
                              spent: ($core$Dict$ofOne)($name, $pos),
                            });
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 473:20', (sp_toHuman)($5)))));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 467:12', (sp_toHuman)($4))));
            }))()
            : ((($expression)[0] === "Constructor")
              ? ((() => {
                const $pos = ($expression)[1];
                const $usr = ($expression)[2];
                return $re;
              }))()
              : ((($expression)[0] === "Fn")
                ? ((() => {
                  const $pos = ($expression)[1];
                  const $pars = ($expression)[2];
                  const $body = ($expression)[3];
                  const $bodyType = ($expression)[4];
                  return ((__re__ = ($sd1$Compiler$UniquenessCheck$doFn)($env, $pos, $state, $pars, $body, $bodyType)), ($state = (__re__)[1]), (__re__)[0]);
                }))()
                : ((($expression)[0] === "Call")
                  ? ((() => {
                    const $pos = ($expression)[1];
                    const $reference = ($expression)[2];
                    const $arguments = ($expression)[3];
                    return ((__re__ = ($sd1$Compiler$UniquenessCheck$doCall)($env, $state, $pos, $reference, $arguments)), ($state = (__re__)[1]), (__re__)[0]);
                  }))()
                  : ((($expression)[0] === "If")
                    ? ((() => {
                      const $pos = ($expression)[1];
                      const $condition = ($expression)[2].condition;
                      const $false = ($expression)[2].false;
                      const $true = ($expression)[2].true;
                      const $doneCondition = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $condition)), ($state = (__re__)[1]), (__re__)[0]);
                      const $newEnv = ($sd1$Compiler$UniquenessCheck$consumeInEnv)($doneCondition.spent, $env);
                      const $doneTrue = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($newEnv, $state, $true)), ($state = (__re__)[1]), (__re__)[0]);
                      const $doneFalse = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($newEnv, $state, $false)), ($state = (__re__)[1]), (__re__)[0]);
                      const $finalTrueExpression = ((($0) => {
                        return ($core$Dict$for)($0, $doneFalse.spent, (($name, _1, $exp) => {
                          return (($core$Dict$member)($name, $doneTrue.spent)
                            ? $exp
                            : ($sd1$Types$TypedAst$DestroyIn)($name, $exp));
                        }));
                      }))($doneTrue.resolved);
                      const $finalFalseExpression = ((($0) => {
                        return ($core$Dict$for)($0, $doneTrue.spent, (($name, _1, $exp) => {
                          return (($core$Dict$member)($name, $doneFalse.spent)
                            ? $exp
                            : ($sd1$Types$TypedAst$DestroyIn)($name, $exp));
                        }));
                      }))($doneFalse.resolved);
                      const $finalExpression = ($sd1$Types$TypedAst$If)($pos, ({
                        condition: $doneCondition.resolved,
                        false: $finalFalseExpression,
                        true: $finalTrueExpression,
                      }));
                      return ({
                        recycled: ((($1) => {
                          return ($core$Dict$join)($doneFalse.recycled, $1);
                        }))(((($1) => {
                          return ($core$Dict$join)($doneTrue.recycled, $1);
                        }))($doneCondition.recycled)),
                        required: ((($1) => {
                          return ($core$Dict$join)($doneFalse.required, $1);
                        }))(((($1) => {
                          return ($core$Dict$join)($doneTrue.required, $1);
                        }))($doneCondition.required)),
                        resolved: $finalExpression,
                        spent: ((($1) => {
                          return ($core$Dict$join)($doneFalse.spent, $1);
                        }))(((($1) => {
                          return ($core$Dict$join)($doneTrue.spent, $1);
                        }))($doneCondition.spent)),
                      });
                    }))()
                    : ((($expression)[0] === "Try")
                      ? ((() => {
                        const $pos = ($expression)[1];
                        const $patternsAndExpressions = ($expression)[2].patternsAndExpressions;
                        const $value = ($expression)[2].value;
                        const $valueType = ($expression)[2].valueType;
                        const $doneValue = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $value)), ($state = (__re__)[1]), (__re__)[0]);
                        const $newEnv = ($sd1$Compiler$UniquenessCheck$consumeInEnv)($doneValue.spent, $env);
                        const $zzz = (($4) => {
                          const $pattern = $4.first;
                          const $block = $4.second;
                          const $5 = ((__re__ = ($sd1$Compiler$UniquenessCheck$addPatternToEnv)($state, $pattern, $newEnv)), ($state = (__re__)[1]), (__re__)[0]);
                          const $env0 = $5.third;
                          const $mutables_should_be_empty = $5.second;
                          const $addedVars = $5.first;
                          const $localEnv = ($sd1$Compiler$UniquenessCheck$requireInEnv)($addedVars, $doneValue.required, $env0);
                          return ((($1) => {
                            return ($sd1$Compiler$UniquenessCheck$uniOutMap)((($expr) => {
                              return ({
                                first: $pattern,
                                second: $expr,
                              });
                            }), $1);
                          }))(((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($localEnv, $state, $block)), ($state = (__re__)[1]), (__re__)[0]));
                        });
                        const $donePatternsAndBlocks = ((($1) => {
                          return ($core$List$map)($zzz, $1);
                        }))($patternsAndExpressions);
                        const $allRecycled = ((($0) => {
                          return ($core$List$for)($0, $donePatternsAndBlocks, (($d, $a) => {
                            return ($core$Dict$join)($d.recycled, $a);
                          }));
                        }))($core$Dict$empty);
                        const $allRequired = ((($0) => {
                          return ($core$List$for)($0, $donePatternsAndBlocks, (($d, $a) => {
                            return ($core$Dict$join)($d.required, $a);
                          }));
                        }))($core$Dict$empty);
                        const $allSpent = ((($0) => {
                          return ($core$List$for)($0, $donePatternsAndBlocks, (($d, $a) => {
                            return ($core$Dict$join)($d.spent, $a);
                          }));
                        }))($core$Dict$empty);
                        const $newPatternsAndBlocks = ((() => {
                          const $xxx = (($4) => {
                            const $recycled = $4.recycled;
                            const $required = $4.required;
                            const $pattern = $4.resolved.first;
                            const $blockExpression = $4.resolved.second;
                            const $spent = $4.spent;
                            const $finalBlock = ((($0) => {
                              return ($core$Dict$for)($0, $allSpent, (($name, _1, $exp) => {
                                return (($core$Dict$member)($name, $spent)
                                  ? $exp
                                  : ($sd1$Types$TypedAst$DestroyIn)($name, $exp));
                              }));
                            }))($blockExpression);
                            return ({
                              first: $pattern,
                              second: $finalBlock,
                            });
                          });
                          return ($core$List$map)($xxx, $donePatternsAndBlocks);
                        }))();
                        return ({
                          recycled: $allRecycled,
                          required: $allRequired,
                          resolved: ($sd1$Types$TypedAst$Try)($pos, ({
                            patternsAndExpressions: $newPatternsAndBlocks,
                            value: $doneValue.resolved,
                            valueType: $valueType,
                          })),
                          spent: $allSpent,
                        });
                      }))()
                      : ((($expression)[0] === "Record")
                        ? ((() => {
                          const $pos = ($expression)[1];
                          const $maybeExtending = ($expression)[2];
                          const $attrValueByName = ($expression)[3];
                          const $doneExt = ((($maybeExtending)[0] === "Nothing")
                            ? ($sd1$Compiler$UniquenessCheck$uniOutInit)($core$Maybe$Nothing)
                            : ((($maybeExtending)[0] === "Just")
                              ? ((() => {
                                const $extending = ($maybeExtending)[1];
                                return ((($1) => {
                                  return ($sd1$Compiler$UniquenessCheck$uniOutMap)($core$Maybe$Just, $1);
                                }))(((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $extending)), ($state = (__re__)[1]), (__re__)[0]));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 614:16', (sp_toHuman)($maybeExtending))));
                          const $doneAttrs = ((($0) => {
                            return ($core$Dict$for)($0, $attrValueByName, (($name, $value, $doneSoFar) => {
                              const $8 = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $value)), ($state = (__re__)[1]), (__re__)[0]);
                              const $spent = $8.spent;
                              const $resolved = $8.resolved;
                              const $required = $8.required;
                              const $recycled = $8.recycled;
                              const $consumedTwice = ($core$Dict$merge)((($k, $v, $d) => {
                                return $d;
                              }), (($k, $a, $b, $d) => {
                                return ($core$Dict$insert)($k, ({
                                  first: $a,
                                  second: $b,
                                }), $d);
                              }), (($k, $v, $d) => {
                                return $d;
                              }), $spent, $doneSoFar.spent, $core$Dict$empty);
                              ($core$Dict$each)($consumedTwice, (($n, $9) => {
                                const $p1 = $9.first;
                                const $p2 = $9.second;
                                return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorReferencingConsumedVariable)($env, $n, $p1, $p2, $state)), ($state = (__re__)[1]), (__re__)[0]);
                              }));
                              return ({
                                recycled: ($core$Dict$join)($recycled, $doneSoFar.recycled),
                                required: ($core$Dict$join)($required, $doneSoFar.required),
                                resolved: ($core$Dict$insert)($name, $resolved, $doneSoFar.resolved),
                                spent: ($core$Dict$join)($spent, $doneSoFar.spent),
                              });
                            }));
                          }))(($sd1$Compiler$UniquenessCheck$uniOutInit)($core$Dict$empty));
                          return ({
                            recycled: ($core$Dict$join)($doneExt.recycled, $doneAttrs.recycled),
                            required: ($core$Dict$join)($doneExt.required, $doneAttrs.required),
                            resolved: ($sd1$Types$TypedAst$Record)($pos, $doneExt.resolved, $doneAttrs.resolved),
                            spent: ($core$Dict$join)($doneExt.spent, $doneAttrs.spent),
                          });
                        }))()
                        : ((($expression)[0] === "RecordAccess")
                          ? ((() => {
                            const $pos = ($expression)[1];
                            const $name = ($expression)[2];
                            const $expr = ($expression)[3];
                            return ((($1) => {
                              return ($sd1$Compiler$UniquenessCheck$uniOutMap)((($2) => {
                                return ($sd1$Types$TypedAst$RecordAccess)($pos, $name, $2);
                              }), $1);
                            }))(((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $expr)), ($state = (__re__)[1]), (__re__)[0]));
                          }))()
                          : ((($expression)[0] === "LetIn")
                            ? ((() => {
                              const $valueDef = ($expression)[1];
                              const $rest = ($expression)[2];
                              const $restType = ($expression)[3];
                              const $4 = ((__re__ = ($sd1$Compiler$UniquenessCheck$addPatternToEnv)($state, $valueDef.pattern, $env)), ($state = (__re__)[1]), (__re__)[0]);
                              const $env1 = $4.third;
                              const $uniques = $4.second;
                              const $addedVars = $4.first;
                              const $doneDefBody = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env1, $state, $valueDef.body)), ($state = (__re__)[1]), (__re__)[0]);
                              const $localEnv = ((($2) => {
                                return ($sd1$Compiler$UniquenessCheck$requireInEnv)($addedVars, $doneDefBody.required, $2);
                              }))(((($1) => {
                                return ($sd1$Compiler$UniquenessCheck$consumeInEnv)($doneDefBody.spent, $1);
                              }))($env1));
                              const $doneExpression = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($localEnv, $state, $rest)), ($state = (__re__)[1]), (__re__)[0]);
                              (($sd1$Types$TypedAst$typeAllowsFunctions)((($tyvarId) => {
                                return false;
                              }), $restType.raw)
                                ? ($core$List$each)($addedVars, (($varName) => {
                                  const $6 = ($core$Dict$get)($varName, $doneExpression.required);
                                  return ((($6)[0] === "Nothing")
                                    ? null
                                    : ((($6)[0] === "Just")
                                      ? ((() => {
                                        const $r = ($6)[1];
                                        return ((__re__ = ($sd1$Compiler$UniquenessCheck$errorReturnExpressionRequiresUniquesDefinedInTheCurrentScope)($env, $varName, $r, $state)), ($state = (__re__)[1]), (__re__)[0]);
                                      }))()
                                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 687:20', (sp_toHuman)($6))));
                                }))
                                : null);
                              const $finalExpression = ((($0) => {
                                return ($core$Dict$for)($0, $uniques, (($name, $pos, $exp) => {
                                  const $9 = ($core$Dict$get)($name, $doneExpression.spent);
                                  return ((($9)[0] === "Just")
                                    ? $exp
                                    : ((($9)[0] === "Nothing")
                                      ? ($sd1$Types$TypedAst$DestroyIn)($name, $exp)
                                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 696:20', (sp_toHuman)($9))));
                                }));
                              }))(($sd1$Types$TypedAst$LetIn)(((() => {
                                const $0 = $valueDef;
                                return (Object.assign)({}, $0, ({
                                  body: $doneDefBody.resolved,
                                }));
                              }))(), $doneExpression.resolved, $restType));
                              const $spent = ((($1) => {
                                return ($core$Dict$join)($doneDefBody.spent, $1);
                              }))(((($0) => {
                                return ($core$Dict$for)($0, $uniques, (($name, _1, $d) => {
                                  return ($core$Dict$remove)($name, $d);
                                }));
                              }))($doneExpression.spent));
                              return ({
                                recycled: ($core$Dict$join)($doneDefBody.recycled, $doneExpression.recycled),
                                required: $doneExpression.required,
                                resolved: $finalExpression,
                                spent: $spent,
                              });
                            }))()
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/UniquenessCheck.sp 456:4', (sp_toHuman)($expression)))))))))))))),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$doModule = (($module) => {
  (sp_benchStart)(null);
  let $state = ({
    errors: (array_fromList)($core$Core$Nil),
  });
  const $env = ({
    errorModule: ({
      content: $module.asText,
      fsPath: $module.fsPath,
    }),
    variables: $core$Dict$empty,
  });
  const $do = ((_0, $def) => {
    const $doneExpression = ((__re__ = ($sd1$Compiler$UniquenessCheck$doExpression)($env, $state, $def.body)), ($state = (__re__)[1]), (__re__)[0]);
    const $0 = $def;
    return (Object.assign)({}, $0, ({
      body: $doneExpression.resolved,
    }));
  });
  const $newModule = ((() => {
    const $0 = $module;
    return (Object.assign)({}, $0, ({
      valueDefs: ($core$Dict$map)($do, $0.valueDefs),
    }));
  }))();
  const $errors = ((__re__ = (array_toList)($state.errors)), ($state.errors = (__re__)[1]), (__re__)[0]);
  (sp_benchStop)("uniqueness check");
  return ((sp_equal)($errors, $core$Core$Nil)
    ? ($core$Result$Ok)($newModule)
    : ($core$Result$Err)(($sd1$Compiler$Error$Nested)($errors)));
});

const $sd1$Compiler$Compiler$compileModules = (($pars) => {
  (sp_log)("Loading modules...", "");
  const $loadModule = (($2) => {
    const $umr = $2.first;
    const $content = $2.second;
    return ($sd1$Compiler$MakeCanonical$textToCanonicalModule)(false, ({
      errorModule: ({
        content: $content,
        fsPath: ($pars.umrToFsPath)($umr),
      }),
      meta: $pars.meta,
      umr: $umr,
    }));
  });
  return (($core$Result$onOk)((($userModules) => {
    (sp_log)("Type checking...", "");
    return (($core$Result$onOk)((($3) => {
      const $luv = $3.first;
      const $typeCheckGlobalEnv = $3.second;
      let $lastUnificationVarId = (basics_cloneImm)($luv);
      return (($core$Result$onOk)((($typedModules) => {
        (sp_log)("Uniqueness check...", "");
        return (($core$Result$onOk)((($modulesWithDestruction) => {
          (sp_log)("Emittable AST...", "");
          return (($core$Result$onOk)((($6) => {
            const $defs = $6.defs;
            const $entryUsr = $6.entryUsr;
            return (($core$Result$onOk)((($type) => {
              const $constructors = ($core$Dict$toList)(($core$Dict$map)((($k, $v) => {
                return $v.type;
              }), $typeCheckGlobalEnv.constructors));
              let $externalValues = (array_fromList)($core$Core$Nil);
              ($core$List$each)($pars.exposedValues, (($8) => {
                const $usr = $8.first;
                const $self = $8.second;
                return ((__re__ = (array_push)($externalValues, ({
                  self: $self,
                  usr: $usr,
                }))), ($externalValues = (__re__)[1]), (__re__)[0]);
              }));
              return ($core$Result$Ok)(({
                constructors: $constructors,
                defs: $defs,
                entryUsr: $entryUsr,
                externalValues: $externalValues,
                type: $type,
              }));
            })))(($sd1$Compiler$Compiler$findMainType)($pars.entryModule, $modulesWithDestruction));
          })))(((($1) => {
            return ($sd1$Compiler$MakeEmittable$translateAll)($pars.entryModule, $1);
          }))($modulesWithDestruction));
        })))(((($1) => {
          return ($core$List$mapRes)((($0) => {
            return ($sd1$Compiler$UniquenessCheck$doModule)($0);
          }), $1);
        }))($typedModules));
      })))(((($1) => {
        return ($core$List$mapRes)((($2) => {
          return ((__re__ = ($sd1$Compiler$TypeCheck$doModule)($lastUnificationVarId, $typeCheckGlobalEnv, $2)), ($lastUnificationVarId = (__re__)[1]), (__re__)[0]);
        }), $1);
      }))($userModules));
    })))(((($1) => {
      return ($sd1$Compiler$TypeCheck$initStateAndGlobalEnv)(($core$List$map)($sd1$Compiler$Compiler$exposedValueToUsrAndInstance, $pars.exposedValues), $1);
    }))($userModules));
  })))(((($1) => {
    return ($core$List$mapRes)($loadModule, $1);
  }))($pars.modules));
});

const $sd1$Compile$compileMain = (($pars) => {
  const $entryModulePath = ($posix$Path$resolve)(($core$Core$Cons)($pars.entryModulePath, $core$Core$Nil));
  const $entryModuleDir = ($posix$Path$dirname)($entryModulePath);
  return (($posix$IO$onSuccess)((($maybeProjectRoot) => {
    const $projectRoot = ($core$Maybe$withDefault)($entryModuleDir, $maybeProjectRoot);
    return (($posix$IO$onSuccess)((($meta) => {
      const $maybeEntryUmr = ((($1) => {
        return ($core$List$find)((($umr) => {
          return (sp_equal)(($sd1$Compile$umrToFileName)($meta, "", $umr), $entryModulePath);
        }), $1);
      }))(($core$Dict$values)($meta.moduleVisibleAsToUmr));
      const $entryUmr = ((($maybeEntryUmr)[0] === "Nothing")
        ? (sp_todo)(("Error: you are asking me to compile module " + ($entryModulePath + " but I can't find it anywhere.")))
        : ((($maybeEntryUmr)[0] === "Just")
          ? ((() => {
            const $umr = ($maybeEntryUmr)[1];
            return $umr;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 317:8', (sp_toHuman)($maybeEntryUmr))));
      return (($posix$IO$onSuccess)((($maybeCorelibParent) => {
        const $corePath = ((($maybeCorelibParent)[0] === "Nothing")
          ? (sp_todo)(("Error: I expect to find the " + ($sd1$Compile$libDirectoryName + (" directory next to the spcc executable " + ($pars.selfPath + " but I can't find it.")))))
          : ((($maybeCorelibParent)[0] === "Just")
            ? ((() => {
              const $p = ($maybeCorelibParent)[1];
              return ($posix$Path$resolve)(($core$Core$Cons)($p, ($core$Core$Cons)($sd1$Compile$libDirectoryName, $core$Core$Nil)));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 334:8', (sp_toHuman)($maybeCorelibParent))));
        const $outputFile = ($core$Maybe$withDefault)($pars.platform.defaultOutputPath, $pars.maybeOutputPath);
        const $loadFile = (($umr) => {
          return (($posix$IO$onSuccess)((($content) => {
            return ($posix$IO$succeed)(({
              first: $umr,
              second: $content,
            }));
          })))(($posix$IO$readFile)(($sd1$Compile$umrToFileName)($meta, $corePath, $umr)));
        });
        return (($posix$IO$onSuccess)((($modulesAsText) => {
          return (($sd1$Compile$onResSuccess)((($compileModulesOut) => {
            return (($posix$IO$onSuccess)(((_0) => {
              return ((($0) => {
                return ($posix$IO$writeStdout)($0);
              }))(("---> " + ($outputFile + " written. =)")));
            })))(((($1) => {
              return ($posix$IO$writeFile)($outputFile, $1);
            }))(($pars.platform.makeExecutable)($compileModulesOut)));
          })))(($sd1$Compiler$Compiler$compileModules)(({
            entryModule: $entryUmr,
            exposedValues: $core$Core$Nil,
            meta: $meta,
            modules: $modulesAsText,
            umrToFsPath: (($2) => {
              return ($sd1$Compile$umrToFileName)($meta, $corePath, $2);
            }),
          })));
        })))(($posix$IO$parallel)(((($1) => {
          return ($core$List$map)($loadFile, $1);
        }))(($core$Dict$values)($meta.moduleVisibleAsToUmr))));
      })))(((($1) => {
        return ($sd1$Compile$searchAncestorDirectories)((($5) => {
          const $isDirectory = $5.first;
          const $fileName = $5.second;
          return ($isDirectory && (sp_equal)($fileName, $sd1$Compile$libDirectoryName));
        }), $1);
      }))(($posix$Path$dirname)(($posix$Path$resolve)(($core$Core$Cons)($pars.selfPath, $core$Core$Nil)))));
    })))(($sd1$Compile$loadMeta)($pars.env, $pars.platform, $entryModuleDir, $projectRoot));
  })))(((($1) => {
    return ($sd1$Compile$searchAncestorDirectories)((($3) => {
      const $isDirectory = $3.first;
      const $fileName = $3.second;
      return (($core$Basics$not)($isDirectory) && (sp_equal)($fileName, $sd1$Compile$modulesFileName));
    }), $1);
  }))($entryModuleDir));
});

const $sd1$Compile$coreDirName = "core";

const $sd1$Compile$getMainName = (($targetsByName) => {
  const $2 = ($core$Dict$toList)($targetsByName);
  return ((($2)[0] === "Cons")
    ? ((() => {
      const $name = ($2)[1].first;
      const $type = ($2)[1].second;
      return ($core$Result$Ok)($name);
    }))()
    : (true
      ? ((() => {
        const $z = $2;
        (sp_log)("-->", $z);
        return (sp_todo)("main type should be IO Int");
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 266:4', (sp_toHuman)($2))));
});

const $sd1$Compile$loadModule = (($meta, $umr, $fileName) => {
  return (($posix$IO$onSuccess)((($moduleAsText) => {
    const $params = ({
      errorModule: ({
        content: $moduleAsText,
        fsPath: $fileName,
      }),
      meta: $meta,
      umr: $umr,
    });
    return ($sd1$Compile$resToIo)(($sd1$Compiler$MakeCanonical$textToCanonicalModule)(false, $params));
  })))(($posix$IO$readFile)($fileName));
});

const $sd1$Compiler$CoreTypes$false = ($sd1$Compiler$CoreTypes$makeUsr)("False");

const $sd1$Compiler$CoreTypes$true = ($sd1$Compiler$CoreTypes$makeUsr)("True");

const $sd1$Compiler$CoreTypes$usrToVariable = (($u) => {
  return ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$CoreTypes$p, ($sd1$Types$Ast$RefGlobal)($u));
});

const $sd1$Compiler$Error$emph = (($1) => {
  return ($sd1$Compiler$Error$formatWrap)("emphasys", $1);
});

const $sd1$Compiler$Lexer$addOneIndentToken = (($kind, $state) => {
  const $pos = ((__re__ = ($sd1$Compiler$Lexer$getPos)($state)), ($state = (__re__)[1]), (__re__)[0]);
  return ([
    ((__re__ = (array_push)($state.tokens, ($sd1$Types$Token$Token)($sd1$Types$Token$N, $pos, $pos, $kind))), ($state.tokens = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Test$codeTest = (($toText, $title, $code, $functionToTest, $ce) => {
  const $6 = $ce;
  const $toMaybeError = ($6)[1];
  return ($sd1$Test$Single)($title, $code, ((_0) => {
    return ($sd1$Test$maybeToOutcome)(((($1) => {
      return ($toMaybeError)($toText, $1);
    }))(($functionToTest)($code)));
  }));
});

const $sd1$Compiler$Lexer_Test$codeTest = (($1, $2, $3, $4) => {
  return ($sd1$Test$codeTest)(sp_toHuman, $1, $2, $3, $4);
});

const $sd1$Compiler$TestHelpers$errorModule = (($content) => {
  return ({
    content: $content,
    fsPath: "<Test>",
  });
});

const $sd1$Compiler$TestHelpers$formattedToStrippedText = (($formatted) => {
  const $strip = (($fmt) => {
    return ((($fmt)[0] === "FormattedText_Default")
      ? ((() => {
        const $t = ($fmt)[1];
        return $t;
      }))()
      : ((($fmt)[0] === "FormattedText_Emphasys")
        ? ((() => {
          const $t = ($fmt)[1];
          return $t;
        }))()
        : ((($fmt)[0] === "FormattedText_Warning")
          ? ((() => {
            const $t = ($fmt)[1];
            return $t;
          }))()
          : ((($fmt)[0] === "FormattedText_Decoration")
            ? ((() => {
              const $t = ($fmt)[1];
              return $t;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TestHelpers.sp 38:8', (sp_toHuman)($fmt))))));
  });
  return ((($1) => {
    return ($core$Text$join)("", $1);
  }))(((($1) => {
    return ($core$List$map)($strip, $1);
  }))($formatted));
});

const $sd1$Compiler$TestHelpers$resErrorToStrippedText = (($res) => {
  const $errorToText = (($e) => {
    return ($sd1$Compiler$TestHelpers$formattedToStrippedText)(($sd1$Compiler$Error$toFormattedText)($e));
  });
  return ($core$Result$mapError)($errorToText, $res);
});

const $sd1$Compiler$Lexer_Test$lexTokens = (($s) => {
  return ($sd1$Compiler$TestHelpers$resErrorToStrippedText)(($sd1$Compiler$Lexer$lexer)(($sd1$Compiler$TestHelpers$errorModule)($s)));
});

const $sd1$Compiler$Lexer_Test$lowerName = (($name) => {
  return ($sd1$Types$Token$Word)(({
    attrPath: $core$Core$Nil,
    isUpper: false,
    maybeModule: $core$Maybe$Nothing,
    modifier: $sd1$Types$Token$NameNoModifier,
    name: $name,
  }));
});

const $sd1$Compiler$Lexer_Test$n = $sd1$Types$Token$N;

const $sd1$Compiler$Lexer_Test$comments = ($sd1$Test$Group)("Comments", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("[reg] statement after comment", "\n#\na = 1\n", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 3, 3, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 3, 4, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 5, 6, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 7, 8, $3);
}))(($sd1$Types$Token$NumberLiteral)(false, "1")), $core$Core$Nil)))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("[reg] nested comments allow a spurious newline?", "\n[#[##]#]\na = 1\n", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 10, 10, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 10, 11, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 12, 13, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 14, 15, $3);
}))(($sd1$Types$Token$NumberLiteral)(false, "1")), $core$Core$Nil)))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("Single line", "# hello", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)($core$Core$Nil, $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("Multi line", "[# single line #]\n\na [# inline #] = 1\n\n[#\n    multi line\n#]\n\n[# [# nested #] #]", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 19, 19, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 19, 20, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 34, 35, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 36, 37, $3);
}))(($sd1$Types$Token$NumberLiteral)(false, "1")), $core$Core$Nil)))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("brackets", "[]", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 1, $3);
}))(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Open)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 1, 2, $3);
}))(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Closed)), $core$Core$Nil))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("[reg] Inline comments should not break a block", "allTests = [\n    , a\n#\n    ]", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 8, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("allTests")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 9, 10, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 11, 12, $3);
}))(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Open)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 17, 18, $3);
}))($sd1$Types$Token$Comma), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 19, 20, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 27, 28, $3);
}))(($sd1$Types$Token$SquareBracket)($sd1$Types$Token$Closed)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 28, 28, $3);
}))($sd1$Types$Token$BlockEnd), $core$Core$Nil)))))))), $core$Core$Nil))), $core$Core$Nil)))))));

const $sd1$Compiler$Lexer_Test$upperName = (($name) => {
  return ($sd1$Types$Token$Word)(({
    attrPath: $core$Core$Nil,
    isUpper: true,
    maybeModule: $core$Maybe$Nothing,
    modifier: $sd1$Types$Token$NameNoModifier,
    name: $name,
  }));
});

const $sd1$Compiler$Lexer_Test$indentation = ($sd1$Test$Group)("Blocks, sibling lines, indentation", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("1", "\na =\n 1\nb = 1", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 1, 1, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 1, 2, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 3, 4, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 6, 6, $3);
}))($sd1$Types$Token$BlockStart), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 6, 7, $3);
}))(($sd1$Types$Token$NumberLiteral)(false, "1")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 8, 8, $3);
}))($sd1$Types$Token$BlockEnd), $core$Core$Nil)))))), ($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 8, 8, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 8, 9, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("b")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 10, 11, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 12, 13, $3);
}))(($sd1$Types$Token$NumberLiteral)(false, "1")), $core$Core$Nil)))), $core$Core$Nil)))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("[reg] spurious spaces in front of field name", "module =\n   importAs =\n      SPCore\n   globalTypes =\n      None", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 6, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("module")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 7, 8, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 12, 12, $3);
}))($sd1$Types$Token$BlockStart), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 12, 20, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("importAs")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 21, 22, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 29, 29, $3);
}))($sd1$Types$Token$BlockStart), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 29, 35, $3);
}))(($sd1$Compiler$Lexer_Test$upperName)("SPCore")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 39, 39, $3);
}))($sd1$Types$Token$BlockEnd), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 39, 39, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 39, 50, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("globalTypes")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 51, 52, $3);
}))($sd1$Types$Token$Defop), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 59, 59, $3);
}))($sd1$Types$Token$BlockStart), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 59, 63, $3);
}))(($sd1$Compiler$Lexer_Test$upperName)("None")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 63, 63, $3);
}))($sd1$Types$Token$BlockEnd), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 63, 63, $3);
}))($sd1$Types$Token$BlockEnd), $core$Core$Nil)))))))))))))))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("Blocks and not", ($core$Text$join)("\n", ($core$Core$Cons)("module =", ($core$Core$Cons)("   i =", ($core$Core$Cons)("        j", ($core$Core$Cons)("            >> k", ($core$Core$Cons)("            >> s", ($core$Core$Cons)("", ($core$Core$Cons)("   importAs =", ($core$Core$Cons)("      SPCore", ($core$Core$Cons)("", ($core$Core$Cons)("   globalTypes =", ($core$Core$Cons)("      None", ($core$Core$Cons)("", ($core$Core$Cons)("   a +     # no block start!", ($core$Core$Cons)("        b   # no sibling", ($core$Core$Cons)("        c", ($core$Core$Cons)("", ($core$Core$Cons)("   d =     # block start", ($core$Core$Cons)("        e   # sibling!", ($core$Core$Cons)("        f", ($core$Core$Cons)("", ($core$Core$Cons)("   g = h", $core$Core$Nil)))))))))))))))))))))), $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 6, ($sd1$Compiler$Lexer_Test$lowerName)("module")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 7, 8, $sd1$Types$Token$Defop), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 12, 12, $sd1$Types$Token$BlockStart), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 12, 13, ($sd1$Compiler$Lexer_Test$lowerName)("i")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 14, 15, $sd1$Types$Token$Defop), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 24, 24, $sd1$Types$Token$BlockStart), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 24, 25, ($sd1$Compiler$Lexer_Test$lowerName)("j")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 38, 40, ($sd1$Types$Token$Binop)($sd1$Prelude$sendRight)), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 41, 42, ($sd1$Compiler$Lexer_Test$lowerName)("k")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 55, 57, ($sd1$Types$Token$Binop)($sd1$Prelude$sendRight)), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 58, 59, ($sd1$Compiler$Lexer_Test$lowerName)("s")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 64, 64, $sd1$Types$Token$BlockEnd), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 64, 64, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 64, 72, ($sd1$Compiler$Lexer_Test$lowerName)("importAs")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 73, 74, $sd1$Types$Token$Defop), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 81, 81, $sd1$Types$Token$BlockStart), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 81, 87, ($sd1$Compiler$Lexer_Test$upperName)("SPCore")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 92, 92, $sd1$Types$Token$BlockEnd), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 92, 92, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 92, 103, ($sd1$Compiler$Lexer_Test$lowerName)("globalTypes")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 104, 105, $sd1$Types$Token$Defop), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 112, 112, $sd1$Types$Token$BlockStart), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 112, 116, ($sd1$Compiler$Lexer_Test$upperName)("None")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 121, 121, $sd1$Types$Token$BlockEnd), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 121, 121, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 121, 122, ($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 123, 124, ($sd1$Types$Token$Binop)($sd1$Prelude$add)), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 155, 156, ($sd1$Compiler$Lexer_Test$lowerName)("b")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 180, 181, ($sd1$Compiler$Lexer_Test$lowerName)("c")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 186, 186, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 186, 187, ($sd1$Compiler$Lexer_Test$lowerName)("d")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 188, 189, $sd1$Types$Token$Defop), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 216, 216, $sd1$Types$Token$BlockStart), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 216, 217, ($sd1$Compiler$Lexer_Test$lowerName)("e")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 239, 239, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 239, 240, ($sd1$Compiler$Lexer_Test$lowerName)("f")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 245, 245, $sd1$Types$Token$BlockEnd), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 245, 245, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 245, 246, ($sd1$Compiler$Lexer_Test$lowerName)("g")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 247, 248, $sd1$Types$Token$Defop), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 249, 250, ($sd1$Compiler$Lexer_Test$lowerName)("h")), ($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 250, 250, $sd1$Types$Token$BlockEnd), $core$Core$Nil))))))))))))))))))))))))))))))))))))))))))), $core$Core$Nil))), $core$Core$Nil))));

const $sd1$Compiler$Lexer_Test$keywords = ($sd1$Test$Group)("keywords", $core$Core$Nil);

const $sd1$Compiler$Lexer_Test$lexTokensAndDrop = (($name) => {
  return (($s) => {
    return ((($1) => {
      return ($core$Result$map)((($1) => {
        return ($core$List$map)((($1) => {
          return ($core$List$drop)($name, $1);
        }), $1);
      }), $1);
    }))(($sd1$Compiler$Lexer_Test$lexTokens)($s));
  });
});

const $sd1$Compiler$Lexer_Test$numberLiterals = ($sd1$Test$Group)("Number literals", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("Percent", "10%", ($sd1$Compiler$Lexer_Test$lexTokensAndDrop)(1), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 2, $3);
}))(($sd1$Types$Token$NumberLiteral)(true, "10")), $core$Core$Nil), $core$Core$Nil))), $core$Core$Nil));

const $sd1$Compiler$Lexer_Test$ops = ($sd1$Test$Group)("Operators", $core$Core$Nil);

const $sd1$Test$errorContains = (($snippets) => {
  return ($sd1$Test$CodeExpectation)((($toText, $result) => {
    return ((($result)[0] === "Ok")
      ? ((() => {
        const $ok = ($result)[1];
        return ($core$Maybe$Just)(("I was expecting an error, but got: Ok " + ($toText)($ok)));
      }))()
      : ((($result)[0] === "Err")
        ? ((() => {
          const $e = ($result)[1];
          const $missing = ((($1) => {
            return ($core$List$filter)((($sn) => {
              return ($core$Basics$not)(($core$Text$contains)($sn, $e));
            }), $1);
          }))($snippets);
          return ((sp_equal)($missing, $core$Core$Nil)
            ? $core$Maybe$Nothing
            : ((() => {
              const $indentedError = ((($1) => {
                return ($core$Text$join)("\n", $1);
              }))(((($1) => {
                return ($core$List$map)((($l) => {
                  return ("    " + $l);
                }), $1);
              }))(((($1) => {
                return (text_split)("\n", $1);
              }))($e)));
              return ($core$Maybe$Just)(("Error message:\n\n" + ($indentedError + ("\n\nis missing snippets: " + ($core$Text$join)(", ", $missing)))));
            }))());
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 103:6', (sp_toHuman)($result))));
  }));
});

const $sd1$Compiler$Lexer_Test$position = ($sd1$Test$Group)("Position", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("[reg] ops position", "blah <>", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$errorContains)(($core$Core$Cons)("blah <>", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("[reg] ops position, with newline", "blah <>\n", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$errorContains)(($core$Core$Cons)("blah <>", $core$Core$Nil))), $core$Core$Nil)));

const $sd1$Compiler$Lexer_Test$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $sd1$Compiler$Lexer_Test$textLiterals = ($sd1$Test$Group)("Text literals", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("Empty Text", "\"\"", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 2, $3);
}))(($sd1$Types$Token$TextLiteral)("")), $core$Core$Nil)), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("Followed by colon", "\"n\":\n", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 3, $3);
}))(($sd1$Types$Token$TextLiteral)("n")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 3, 4, $3);
}))($sd1$Types$Token$Colon), $core$Core$Nil))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$valueTest)("Unindent function", ((_0) => {
  return ((($0) => {
    return ($sd1$Compiler$Lexer$unindent)($0);
  }))(((($1) => {
    return ($core$Text$join)("", $1);
  }))(($core$Core$Cons)("\n", ($core$Core$Cons)("  a\n", ($core$Core$Cons)("      \n", ($core$Core$Cons)("\n", ($core$Core$Cons)("  b\n", ($core$Core$Cons)("  ", $core$Core$Nil))))))));
}), ($sd1$Test$isOkAndEqualTo)(($core$Text$join)("", ($core$Core$Cons)("a\n", ($core$Core$Cons)("    \n", ($core$Core$Cons)("\n", ($core$Core$Cons)("b", $core$Core$Nil))))))), $core$Core$Nil))));

const $sd1$Compiler$Lexer_Test$unaryAddittiveOps = ($sd1$Test$Group)("Unary addittive ops", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("-a", "-a", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 1, $3);
}))(($sd1$Types$Token$Unop)($sd1$Types$Op$UnopMinus)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 1, 2, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), $core$Core$Nil))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("a - -a", "a - -a", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 1, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 2, 3, $3);
}))(($sd1$Types$Token$Binop)($sd1$Prelude$subtract)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 4, 5, $3);
}))(($sd1$Types$Token$Unop)($sd1$Types$Op$UnopMinus)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 5, 6, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), $core$Core$Nil))))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("a-a", "a-a", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $3);
}))($sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 1, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 1, 2, $3);
}))(($sd1$Types$Token$Unop)($sd1$Types$Op$UnopMinus)), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 2, 3, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("a")), $core$Core$Nil)))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("-=", "-=", $sd1$Compiler$Lexer_Test$lexTokens, ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 0, $sd1$Types$Token$NewSiblingLine), ($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 2, $3);
}))(($sd1$Types$Token$Binop)($sd1$Prelude$mutableSubtract)), $core$Core$Nil)), $core$Core$Nil))), $core$Core$Nil)))));

const $sd1$Compiler$Lexer_Test$underscores = ($sd1$Test$Group)("Underscores", ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("'_' as a Name", "_", ($sd1$Compiler$Lexer_Test$lexTokensAndDrop)(1), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 1, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("_")), $core$Core$Nil), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("'_10_20' as a Name", "_10_20", ($sd1$Compiler$Lexer_Test$lexTokensAndDrop)(1), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 6, $3);
}))(($sd1$Compiler$Lexer_Test$lowerName)("_10_20")), $core$Core$Nil), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$Lexer_Test$codeTest)("'10_20' as a Number", "10_20", ($sd1$Compiler$Lexer_Test$lexTokensAndDrop)(1), ($sd1$Test$isOkAndEqualTo)(($core$Core$Cons)(($core$Core$Cons)(((($3) => {
  return ($sd1$Types$Token$Token)($sd1$Compiler$Lexer_Test$n, 0, 5, $3);
}))(($sd1$Types$Token$NumberLiteral)(false, "10_20")), $core$Core$Nil), $core$Core$Nil))), $core$Core$Nil))));

const $sd1$Compiler$Lexer_Test$tests = ($sd1$Test$Group)("Lexer", ($core$Core$Cons)($sd1$Compiler$Lexer_Test$keywords, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$ops, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$unaryAddittiveOps, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$indentation, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$comments, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$underscores, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$position, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$textLiterals, ($core$Core$Cons)($sd1$Compiler$Lexer_Test$numberLiterals, $core$Core$Nil))))))))));

const $sd1$Compiler$MakeCanonical_Test$codeTest = (($1, $2, $3, $4) => {
  return ($sd1$Test$codeTest)(sp_toHuman, $1, $2, $3, $4);
});

const $sd1$DefaultModules$asText = "library =\n    source = \"core:prelude\"\n\n    module =\n        path = Core\n        importAs = Core\n        globalTypes =\n            None\n            Bool\n            Text\n            Number\n        globalValues =\n            None\n            True\n            False\n            mut\n\n    module =\n        path = List\n        importAs = List\n\n    module =\n        path = Maybe\n        importAs = Maybe\n        globalTypes =\n            Maybe\n        globalValues =\n           Just\n           Nothing\n\n    module =\n        path = Text\n        importAs = Text\n\n    module =\n        path = Tuple\n        importAs = Tuple\n\n    module =\n        path = Debug\n        importAs = Debug\n        globalValues =\n            log\n            todo\n\n    module =\n        path = Basics\n        globalTypes =\n            Int\n        globalValues =\n            assert\n            clamp\n            identity\n            modBy\n            min\n            max\n\n    module =\n        path = Dict\n        importAs = Dict\n        globalTypes = Dict\n\n    module =\n        path = Set\n        importAs = Set\n        globalTypes = Set\n\n    module =\n        path = Result\n        importAs = Result\n        globalTypes = Result\n        globalValues =\n            Ok\n            Err";

const $sd1$ModulesFile$textToMeta = (($sponName, $sponContent) => {
  return ((($1) => {
    return ($core$Result$map)($sd1$ModulesFile$toMeta, $1);
  }))(((($1) => {
    return ($sd1$ModulesFile$textToModulesFile)($sponName, $1);
  }))($sponContent));
});

const $sd1$Compiler$TestHelpers$meta = ((() => {
  const $metaResult = ((($1) => {
    return ($core$Result$mapError)((($e) => {
      return ($sd1$Compiler$TestHelpers$formattedToStrippedText)(($sd1$Compiler$Error$toFormattedText)($e));
    }), $1);
  }))(((($1) => {
    return ($sd1$ModulesFile$textToMeta)("DefaultModules", $1);
  }))($sd1$DefaultModules$asText));
  return ((($metaResult)[0] === "Err")
    ? ((() => {
      const $e = ($metaResult)[1];
      (sp_log)(("Error in DefaultModules.sp: " + $e), null);
      return (sp_todo)("error loading DefaultModules.sp");
    }))()
    : ((($metaResult)[0] === "Ok")
      ? ((() => {
        const $m = ($metaResult)[1];
        return $m;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TestHelpers.sp 71:4', (sp_toHuman)($metaResult))));
}))();

const $sd1$Compiler$TestHelpers$moduleName = "(test)";

const $sd1$Compiler$TestHelpers$source = ($sd1$Types$Meta$SourceDirId)("<Test>");

const $sd1$Compiler$TestHelpers$moduleUmr = ($sd1$Types$Meta$UMR)($sd1$Compiler$TestHelpers$source, $sd1$Compiler$TestHelpers$moduleName);

const $sd1$Compiler$MakeCanonical_Test$params = (($errorModule) => {
  return ({
    errorModule: $errorModule,
    meta: $sd1$Compiler$TestHelpers$meta,
    umr: $sd1$Compiler$TestHelpers$moduleUmr,
  });
});

const $sd1$Compiler$MakeCanonical_Test$textToModule = (($code) => {
  return ($sd1$Compiler$TestHelpers$resErrorToStrippedText)(((($1) => {
    return ($sd1$Compiler$MakeCanonical$textToCanonicalModule)(true, $1);
  }))(($sd1$Compiler$MakeCanonical_Test$params)(($sd1$Compiler$TestHelpers$errorModule)($code))));
});

const $sd1$Compiler$MakeCanonical_Test$firstDefinition = (($code) => {
  return (($core$Result$onOk)((($mod) => {
    return ((($1) => {
      return ($core$Result$fromMaybe)("firstDefinition fail", $1);
    }))(($core$List$head)(($core$Dict$values)($mod.valueDefs)));
  })))(($sd1$Compiler$MakeCanonical_Test$textToModule)($code));
});

const $sd1$Compiler$MakeCanonical_Test$firstEvaluation = (($name) => {
  return (($code) => {
    return (($core$Result$onOk)((($def) => {
      return ($core$Result$Ok)($def.body);
    })))(($sd1$Compiler$MakeCanonical_Test$firstDefinition)($code));
  });
});

const $sd1$Test$isOk = ($sd1$Test$CodeExpectation)((($toText, $result) => {
  return ((($result)[0] === "Err")
    ? ((() => {
      const $e = ($result)[1];
      return ($core$Maybe$Just)($e);
    }))()
    : ((($result)[0] === "Ok")
      ? ((() => {
        const $actualOk = ($result)[1];
        return $core$Maybe$Nothing;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 70:8', (sp_toHuman)($result))));
}));

const $sd1$Compiler$MakeCanonical_Test$annotations = ($sd1$Test$Group)("Annotations", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("annotation on unique value", "x =\n  !a as Number =\n    3\n  a", $sd1$Compiler$MakeCanonical_Test$firstDefinition, $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("annotation on immutable value", "b as Number =\n  3", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("b"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("annotation of recycling function", "b as fn @Result e a: !Result e a =\n  3", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("b"), $sd1$Test$isOk), $core$Core$Nil))));

const $sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps = (($code) => {
  return ((($1) => {
    return ($core$Result$map)((($v) => {
      const $0 = $v;
      return (Object.assign)({}, $0, ({
        directConsDeps: $core$Dict$empty,
        directTypeDeps: $core$Dict$empty,
        directValueDeps: $core$Dict$empty,
      }));
    }), $1);
  }))(($sd1$Compiler$MakeCanonical_Test$firstDefinition)($code));
});

const $sd1$Compiler$MakeCanonical_Test$p = $sd1$Types$Pos$T;

const $sd1$Compiler$MakeCanonical_Test$argumentPlaceholders = ($sd1$Test$Group)("Argument placeholders", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("Base", "f = f __ __", $sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps, ($sd1$Test$isOkAndEqualTo)(({
  body: ($sd1$Types$CanonicalAst$Fn)($sd1$Compiler$MakeCanonical_Test$p, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParameterPlaceholder)("0", 0), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParameterPlaceholder)("1", 1), $core$Core$Nil)), ($sd1$Types$CanonicalAst$Call)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$Ast$RefGlobal)(($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)(($sd1$Types$Meta$SourceDirId)("<Test>"), "(test)"), "f"))), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$Ast$RefLocal)("0"))), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$Ast$RefLocal)("1"))), $core$Core$Nil)))),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  native: false,
  pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
    maybeAnnotation: $core$Maybe$Nothing,
    maybeName: ($core$Maybe$Just)("f"),
  })),
  tyvars: $core$Dict$empty,
  uni: $sd1$Types$Ast$Imm,
  univars: $core$Dict$empty,
}))), $core$Core$Nil));

const $sd1$Test$freeform = (($test) => {
  return ($sd1$Test$CodeExpectation)((($toText, $result) => {
    return ((($result)[0] === "Err")
      ? ((() => {
        const $e = ($result)[1];
        return ($core$Maybe$Just)($e);
      }))()
      : ((($result)[0] === "Ok")
        ? ((() => {
          const $actualOk = ($result)[1];
          return ($test)($actualOk);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 63:4', (sp_toHuman)($result))));
  }));
});

const $sd1$Compiler$MakeCanonical_Test$shouldHaveSameAB = (($getter) => {
  return ($sd1$Test$freeform)((($2) => {
    const $a = $2.first;
    const $b = $2.second;
    return ((sp_equal)(($getter)($a), ($getter)($b))
      ? $core$Maybe$Nothing
      : ($core$Maybe$Just)(((($1) => {
        return ($core$Text$join)("\n", $1);
      }))(($core$Core$Cons)("The two don't match:", ($core$Core$Cons)((sp_toHuman)(($getter)($a)), ($core$Core$Cons)((sp_toHuman)(($getter)($b)), $core$Core$Nil))))));
  }));
});

const $sd1$Compiler$MakeCanonical_Test$transformAB = (($code) => {
  const $findAB = (($mod) => {
    const $3 = ((($1) => {
      return (list_sortBy)((($def) => {
        return $def.pattern;
      }), $1);
    }))(($core$Dict$values)($mod.valueDefs));
    return (((($3)[0] === "Cons") && (((($3)[2])[0] === "Cons") && (((($3)[2])[2])[0] === "Nil")))
      ? ((() => {
        const $a = ($3)[1];
        const $b = (($3)[2])[1];
        return ($core$Maybe$Just)(({
          first: $a,
          second: $b,
        }));
      }))()
      : (true
        ? $core$Maybe$Nothing
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical_Test.sp 82:8', (sp_toHuman)($3))));
  });
  return (($core$Result$onOk)((($x) => {
    return ((($1) => {
      return ($core$Result$fromMaybe)("findAB fail", $1);
    }))(($findAB)($x));
  })))(($sd1$Compiler$MakeCanonical_Test$textToModule)($code));
});

const $sd1$Compiler$MakeCanonical_Test$binops = ($sd1$Test$Group)("Binops", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("left associativity", "a = v >> f >> g\nb = (v >> f) >> g", $sd1$Compiler$MakeCanonical_Test$transformAB, ($sd1$Compiler$MakeCanonical_Test$shouldHaveSameAB)((($x) => {
  return $x.body;
}))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("right associativity", "a = v :: f :: g\nb = v :: (f :: g)", $sd1$Compiler$MakeCanonical_Test$transformAB, ($sd1$Compiler$MakeCanonical_Test$shouldHaveSameAB)((($x) => {
  return $x.body;
}))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("precedence", "a = 1 + 2 * 3 + 4\nb = 1 + (2 * 3) + 4", $sd1$Compiler$MakeCanonical_Test$transformAB, ($sd1$Compiler$MakeCanonical_Test$shouldHaveSameAB)((($x) => {
  return $x.body;
}))), $core$Core$Nil))));

const $sd1$Compiler$TestHelpers$rootLocal = (($name) => {
  return ($sd1$Types$Ast$RefGlobal)(($sd1$Types$Meta$USR)($sd1$Compiler$TestHelpers$moduleUmr, $name));
});

const $sd1$Compiler$MakeCanonical_Test$functions = ($sd1$Test$Group)("Functions", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("[rec] function with call", "a =\n    fn x:\n        add x 1", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("f"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$Fn)($sd1$Compiler$MakeCanonical_Test$p, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParameterPattern)($sd1$Types$Ast$Imm, ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
  maybeAnnotation: $core$Maybe$Nothing,
  maybeName: ($core$Maybe$Just)("x"),
}))), $core$Core$Nil), ($sd1$Types$CanonicalAst$Call)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("add")), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$Ast$RefLocal)("x"))), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 1)), $core$Core$Nil)))))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("[rec] function with two arguments", "f =\n  fn a, b: 1", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("f"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$Fn)($sd1$Compiler$MakeCanonical_Test$p, ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParameterPattern)($sd1$Types$Ast$Imm, ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
  maybeAnnotation: $core$Maybe$Nothing,
  maybeName: ($core$Maybe$Just)("a"),
}))), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ParameterPattern)($sd1$Types$Ast$Imm, ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
  maybeAnnotation: $core$Maybe$Nothing,
  maybeName: ($core$Maybe$Just)("b"),
}))), $core$Core$Nil)), ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 1)))), $core$Core$Nil)));

const $sd1$Types$Meta$spCoreUmr = ($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "Core");

const $sd1$Types$Meta$spCoreUSR = (($1) => {
  return ($sd1$Types$Meta$USR)($sd1$Types$Meta$spCoreUmr, $1);
});

const $sd1$Compiler$TestHelpers$caBool = ($sd1$Types$CanonicalAst$TypeNamed)($sd1$Types$Pos$T, ($sd1$Types$Meta$spCoreUSR)("Bool"), $core$Core$Nil);

const $sd1$Compiler$MakeCanonical_Test$lists = ($sd1$Test$Group)("Lists", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("list type sugar", "l as [ Bool ] =\n  l", $sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps, ($sd1$Test$isOkAndEqualTo)(({
  body: ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("l")),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  native: false,
  pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
    maybeAnnotation: ($core$Maybe$Just)(($sd1$Compiler$CoreTypes$list)($sd1$Compiler$TestHelpers$caBool)),
    maybeName: ($core$Maybe$Just)("l"),
  })),
  tyvars: $core$Dict$empty,
  uni: $sd1$Types$Ast$Imm,
  univars: $core$Dict$empty,
}))), $core$Core$Nil));

const $sd1$Compiler$MakeCanonical_Test$moduleAndAttributePaths = ((() => {
  const $accept = (($s) => {
    return ($sd1$Compiler$MakeCanonical_Test$codeTest)($s, ("a = " + $s), $sd1$Compiler$MakeCanonical_Test$firstDefinition, $sd1$Test$isOk);
  });
  const $reject = (($s, $m) => {
    return ($sd1$Compiler$MakeCanonical_Test$codeTest)($s, ("a = " + $s), $sd1$Compiler$MakeCanonical_Test$firstDefinition, ($sd1$Test$errorContains)(($core$Core$Cons)($m, $core$Core$Nil)));
  });
  return ($sd1$Test$Group)("Module and Attribute Paths", ($core$Core$Cons)(($accept)("blah.blah.blah"), ($core$Core$Cons)(($reject)("Blah.Blah.blah", "constructor"), ($core$Core$Cons)(($reject)("blah.Blah.blah", "case"), ($core$Core$Cons)(($reject)("List.blah.Blah", "lower"), ($core$Core$Cons)(($reject)("List..blah", "space"), ($core$Core$Cons)(($reject)(".Blah", "upper"), ($core$Core$Cons)(($reject)(".blah.blah", "shorthand"), ($core$Core$Cons)(($reject)(".blah", "shorthand"), ($core$Core$Cons)(($reject)("...", ""), ($core$Core$Cons)(($accept)("x .. y"), $core$Core$Nil)))))))))));
}))();

const $sd1$Compiler$MakeCanonical_Test$nonFunction = ($sd1$Test$Group)("NonFunction", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("one", "funz as a with a NonFunction =\n    1", $sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps, ($sd1$Test$isOkAndEqualTo)(({
  body: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 1),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  native: false,
  pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
    maybeAnnotation: ($core$Maybe$Just)(($sd1$Types$CanonicalAst$TypeAnnotationVariable)($sd1$Compiler$MakeCanonical_Test$p, "a")),
    maybeName: ($core$Maybe$Just)("funz"),
  })),
  tyvars: ($core$Dict$ofOne)("a", ({
    allowFunctions: false,
  })),
  uni: $sd1$Types$Ast$Imm,
  univars: $core$Dict$empty,
}))), $core$Core$Nil));

const $sd1$Compiler$MakeCanonical_Test$numbers = ($sd1$Test$Group)("Numbers", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("Percent", "a = 1%", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 0.01))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("Underscore", "a = 1_000_000", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, (1000 * 1000)))), $core$Core$Nil)));

const $sd1$Compiler$MakeCanonical_Test$patterns = ($sd1$Test$Group)("Patterns", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("Record patterns can be partial", "a =\n  { with c } = d", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("[reg] record patterns are NOT extensible", "a =\n  { b with c } = d", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$errorContains)(($core$Core$Cons)("extend pattern", $core$Core$Nil))), $core$Core$Nil)));

const $sd1$Compiler$MakeCanonical_Test$pipes = ($sd1$Test$Group)("Pipes", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("sendLeft is inlined", "a = thing >> function", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$Call)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("function")), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("thing"))), $core$Core$Nil)))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("sendRight is inlined", "a = function << thing", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$Call)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("function")), ($core$Core$Cons)(($sd1$Types$CanonicalAst$ArgumentExpression)(($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("thing"))), $core$Core$Nil)))), $core$Core$Nil)));

const $sd1$Compiler$MakeCanonical_Test$polymorphicUniques = ($sd1$Test$Group)("Polymorphic Uniques", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("In pattern", "scope =\n    1?f as a = meh", $sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps, ($sd1$Test$isOkAndEqualTo)(({
  body: ($sd1$Types$CanonicalAst$LetIn)(({
    body: ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("meh")),
    directConsDeps: $core$Dict$empty,
    directTypeDeps: $core$Dict$empty,
    directValueDeps: $core$Dict$empty,
    native: false,
    pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
      maybeAnnotation: ($core$Maybe$Just)(($sd1$Types$CanonicalAst$TypeAnnotationVariable)($sd1$Compiler$MakeCanonical_Test$p, "a")),
      maybeName: ($core$Maybe$Just)("f"),
    })),
    tyvars: ($core$Dict$ofOne)("a", ({
      allowFunctions: true,
    })),
    uni: ($sd1$Types$Ast$Depends)(1),
    univars: ($core$Dict$ofOne)(1, null),
  }), ($sd1$Types$CanonicalAst$Constructor)($sd1$Types$Pos$G, $sd1$Compiler$CoreTypes$noneValue)),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  native: false,
  pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
    maybeAnnotation: $core$Maybe$Nothing,
    maybeName: ($core$Maybe$Just)("scope"),
  })),
  tyvars: $core$Dict$empty,
  uni: $sd1$Types$Ast$Imm,
  univars: $core$Dict$empty,
}))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("In annotation", "isOk as fn (fn 1?a: 2?Re error b), 1?Re error a: 2?Re error b = meh", (($t) => {
  return ((($1) => {
    return ($core$Result$map)((($x) => {
      return $x.univars;
    }), $1);
  }))(($sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps)($t));
}), ($sd1$Test$isOkAndEqualTo)(($core$Dict$fromList)(($core$Core$Cons)(({
  first: 1,
  second: null,
}), ($core$Core$Cons)(({
  first: 2,
  second: null,
}), $core$Core$Nil))))), $core$Core$Nil)));

const $sd1$Compiler$MakeCanonical_Test$valueDef = (($name, $body) => {
  return ({
    body: $body,
    directConsDeps: $core$Dict$empty,
    directTypeDeps: $core$Dict$empty,
    directValueDeps: $core$Dict$empty,
    native: false,
    pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Types$Pos$G, ({
      maybeAnnotation: $core$Maybe$Nothing,
      maybeName: ($core$Maybe$Just)($name),
    })),
    tyvars: $core$Dict$empty,
    uni: $sd1$Types$Ast$Imm,
    univars: $core$Dict$empty,
  });
});

const $sd1$Compiler$MakeCanonical_Test$records = ($sd1$Test$Group)("Records", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("functional update", "a = { m with b, c = 1 }", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$LetIn)(($sd1$Compiler$MakeCanonical_Test$valueDef)("0", ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("m"))), ($sd1$Types$CanonicalAst$Record)($sd1$Compiler$MakeCanonical_Test$p, ($core$Maybe$Just)(($sd1$Types$CanonicalAst$Variable)($sd1$Types$Pos$G, ($sd1$Types$Ast$RefLocal)("0"))), ($core$Dict$fromList)(($core$Core$Cons)(({
  first: "c",
  second: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 1),
}), ($core$Core$Cons)(({
  first: "b",
  second: ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("b")),
}), $core$Core$Nil))))))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("update shorthand", "b = { a with y = .x }", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("b"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$LetIn)(($sd1$Compiler$MakeCanonical_Test$valueDef)("0", ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("a"))), ($sd1$Types$CanonicalAst$Record)($sd1$Compiler$MakeCanonical_Test$p, ($core$Maybe$Just)(($sd1$Types$CanonicalAst$Variable)($sd1$Types$Pos$G, ($sd1$Types$Ast$RefLocal)("0"))), ($core$Dict$fromList)(($core$Core$Cons)(({
  first: "y",
  second: ($sd1$Types$CanonicalAst$RecordAccess)($sd1$Compiler$MakeCanonical_Test$p, "x", ($sd1$Types$CanonicalAst$Variable)($sd1$Types$Pos$G, ($sd1$Types$Ast$RefLocal)("0"))),
}), $core$Core$Nil)))))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("annotation, extensible", "a as { b with x as Bool } =\n  a", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$errorContains)(($core$Core$Cons)("disabled", $core$Core$Nil))), $core$Core$Nil))));

const $sd1$Compiler$TestHelpers$caNumber = ($sd1$Types$CanonicalAst$TypeNamed)($sd1$Types$Pos$T, ($sd1$Types$Meta$spCoreUSR)("Number"), $core$Core$Nil);

const $sd1$Compiler$MakeCanonical_Test$tuples = ($sd1$Test$Group)("Tuples", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("tuple2", "a = 1 & 2", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$Record)($sd1$Compiler$MakeCanonical_Test$p, $core$Maybe$Nothing, ($core$Dict$fromList)(($core$Core$Cons)(({
  first: "first",
  second: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 1),
}), ($core$Core$Cons)(({
  first: "second",
  second: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 2),
}), $core$Core$Nil)))))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("tuple3", "a = 1 & 2 & 3", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$isOkAndEqualTo)(($sd1$Types$CanonicalAst$Record)($sd1$Compiler$MakeCanonical_Test$p, $core$Maybe$Nothing, ($core$Dict$fromList)(($core$Core$Cons)(({
  first: "first",
  second: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 1),
}), ($core$Core$Cons)(({
  first: "second",
  second: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 2),
}), ($core$Core$Cons)(({
  first: "third",
  second: ($sd1$Types$CanonicalAst$LiteralNumber)($sd1$Compiler$MakeCanonical_Test$p, 3),
}), $core$Core$Nil))))))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("tuple4", "a = 1 & 2 & 3 & 4", ($sd1$Compiler$MakeCanonical_Test$firstEvaluation)("a"), ($sd1$Test$errorContains)(($core$Core$Cons)("use a record", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("tuple2 type", "a as Number & Number =\n  a", $sd1$Compiler$MakeCanonical_Test$firstDefinitionStripDeps, ($sd1$Test$isOkAndEqualTo)(({
  body: ($sd1$Types$CanonicalAst$Variable)($sd1$Compiler$MakeCanonical_Test$p, ($sd1$Compiler$TestHelpers$rootLocal)("a")),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  native: false,
  pattern: ($sd1$Types$CanonicalAst$PatternAny)($sd1$Compiler$MakeCanonical_Test$p, ({
    maybeAnnotation: ($core$Maybe$Just)(((($1) => {
      return ($sd1$Types$CanonicalAst$TypeRecord)($sd1$Compiler$MakeCanonical_Test$p, $1);
    }))(((($2) => {
      return ($core$Dict$insert)("second", $sd1$Compiler$TestHelpers$caNumber, $2);
    }))(((($2) => {
      return ($core$Dict$insert)("first", $sd1$Compiler$TestHelpers$caNumber, $2);
    }))($core$Dict$empty)))),
    maybeName: ($core$Maybe$Just)("a"),
  })),
  tyvars: $core$Dict$empty,
  uni: $sd1$Types$Ast$Imm,
  univars: $core$Dict$empty,
}))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("tuple4, type", "a as Blah & Blah & Blah & Blah =\n  a", $sd1$Compiler$MakeCanonical_Test$firstDefinition, ($sd1$Test$errorContains)(($core$Core$Cons)("use a record", $core$Core$Nil))), $core$Core$Nil))))));

const $sd1$Compiler$MakeCanonical_Test$unionTypes = ($sd1$Test$Group)("Union types", ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("tuples op precedence", "union A = X Bool & Bool", $sd1$Compiler$MakeCanonical_Test$textToModule, ($sd1$Test$errorContains)(($core$Core$Cons)("expecting a constructor", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("Tuples op precedence works with parens", "union A = X (Bool & Bool)", $sd1$Compiler$MakeCanonical_Test$textToModule, $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$MakeCanonical_Test$codeTest)("[reg] Should reject uppercase arg name", "union Outcome Token output = A", $sd1$Compiler$MakeCanonical_Test$textToModule, ($sd1$Test$errorContains)(($core$Core$Cons)("must start with a lowercase", $core$Core$Nil))), $core$Core$Nil))));

const $sd1$Compiler$MakeCanonical_Test$tests = ($sd1$Test$Group)("MakeCanonical", ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$unionTypes, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$binops, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$tuples, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$lists, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$moduleAndAttributePaths, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$records, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$patterns, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$annotations, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$pipes, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$functions, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$nonFunction, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$argumentPlaceholders, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$polymorphicUniques, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$numbers, $core$Core$Nil)))))))))))))));

const $sd1$Compiler$Parser$surroundMultiline = (($left, $right, $content) => {
  return ($sd1$Compiler$Parser$discardFirst)(($sd1$Compiler$Parser$kind)($left), ($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$discardSecond)($content, ($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$kind)($right)))));
});

const $sd1$Compiler$Parser$typeAnnotation = (($env) => {
  return ($sd1$Compiler$Parser$discardFirst)(($sd1$Compiler$Parser$kind)($sd1$Types$Token$As), ($sd1$Compiler$Parser$inlineOrBelowOrIndented)(($sd1$Compiler$Parser$expr)($env)));
});

const $sd1$Compiler$Parser_Test$codeTest = (($1, $2, $3, $4) => {
  return ($sd1$Test$codeTest)(sp_toHuman, $1, $2, $3, $4);
});

const $sd1$Compiler$Parser_Test$p = $sd1$Types$Pos$T;

const $sd1$Compiler$Parser_Test$tests = ($sd1$Test$Group)("Parser", $core$Core$Nil);

const $sd1$Compiler$Parser_Test$zot = 1;

const $sd1$Compiler$TestHelpers$caList = (($itemType) => {
  return ($sd1$Types$CanonicalAst$TypeNamed)($sd1$Types$Pos$T, ($sd1$Types$Meta$spCoreUSR)("List"), ($core$Core$Cons)($itemType, $core$Core$Nil));
});

const $sd1$Compiler$TestHelpers$caNone = ($sd1$Types$CanonicalAst$TypeNamed)($sd1$Types$Pos$T, ($sd1$Types$Meta$spCoreUSR)("None"), $core$Core$Nil);

const $sd1$Compiler$TestHelpers$localType = (($1) => {
  return ($sd1$Types$Meta$USR)($sd1$Compiler$TestHelpers$moduleUmr, $1);
});

const $sd1$Compiler$TestHelpers$taBool = ($sd1$Types$TypedAst$TypeExact)(($sd1$Types$Meta$spCoreUSR)("Bool"), $core$Core$Nil);

const $sd1$Compiler$TestHelpers$taFunction = (($from, $to) => {
  return ($sd1$Types$TypedAst$TypeFn)(($core$List$map)((($t) => {
    return ($sd1$Types$TypedAst$ParSp)(($sd1$Types$Ast$toImm)($t));
  }), $from), ($sd1$Types$Ast$toImm)($to));
});

const $sd1$Compiler$TestHelpers$taList = (($item) => {
  return ($sd1$Types$TypedAst$TypeExact)(($sd1$Types$Meta$spCoreUSR)("List"), ($core$Core$Cons)($item, $core$Core$Nil));
});

const $sd1$Compiler$TestHelpers$taNone = ($sd1$Types$TypedAst$TypeExact)(($sd1$Types$Meta$spCoreUSR)("None"), $core$Core$Nil);

const $sd1$Compiler$TestHelpers$taNumber = ($sd1$Types$TypedAst$TypeExact)(($sd1$Types$Meta$spCoreUSR)("Number"), $core$Core$Nil);

const $sd1$Compiler$TestHelpers$taTyvar = (($0) => {
  return ($sd1$Types$TypedAst$TypeVar)($0);
});

const $sd1$Compiler$TestHelpers$taTyvarImm = (($0) => {
  return ($sd1$Types$TypedAst$TypeVar)($0);
});

const $sd1$Compiler$TypeCheck_Test$outToHuman = (($out) => {
  return ((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(($core$Core$Cons)(("  tyvars = " + (sp_toHuman)(($core$Dict$toList)($out.freeTyvars))), ($core$Core$Cons)(("  type = " + ($sd1$Human$Type$display)("", ($sd1$Human$Type$doRawType)({}, $out.type))), $core$Core$Nil)));
});

const $sd1$Compiler$TypeCheck_Test$codeTest = (($1, $2, $3, $4) => {
  return ($sd1$Test$codeTest)($sd1$Compiler$TypeCheck_Test$outToHuman, $1, $2, $3, $4);
});

const $sd1$Compiler$TypeCheck_Test$freeTyvarsAnnotated = (($ids) => {
  return ((($0) => {
    return ($core$List$for)($0, $ids, (($4, $d) => {
      const $id = $4.first;
      const $originalName = $4.second;
      return ($core$Dict$insert)($id, ({
        allowFunctions: true,
        generalizedAt: $sd1$Types$Pos$G,
        generalizedFor: ($sd1$Types$Ast$RefLocal)(""),
        originalName: $originalName,
      }), $d);
    }));
  }))($core$Dict$empty);
});

const $sd1$Compiler$TypeCheck_Test$normalizeOut = (($out) => {
  let $hash = (hash_fromList)($core$Core$Nil);
  return ({
    freeTyvars: ($core$Dict$for)($core$Dict$empty, $out.freeTyvars, (($id, $tc, $d) => {
      return ($core$Dict$insert)(((__re__ = ($sd1$Types$TypedAst$normalizeTyvarId)($hash, $id)), ($hash = (__re__)[1]), (__re__)[0]), $tc, $d);
    })),
    type: ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $out.type)), ($hash = (__re__)[1]), (__re__)[0]),
  });
});

const $sd1$Compiler$TypeCheck_Test$infer = (($targetName) => {
  return (($code) => {
    const $params = ({
      errorModule: ($sd1$Compiler$TestHelpers$errorModule)($code),
      meta: $sd1$Compiler$TestHelpers$meta,
      umr: $sd1$Compiler$TestHelpers$moduleUmr,
    });
    return (($core$Result$onOk)((($caModule) => {
      return (($core$Result$onOk)((($4) => {
        const $luv = $4.first;
        const $typeCheckGlobalEnv_ = $4.second;
        const $typeCheckGlobalEnv = ((() => {
          const $0 = $typeCheckGlobalEnv_;
          return (Object.assign)({}, $0, ({
            variables: ((($2) => {
              return ($core$Dict$insert)(($sd1$Types$Ast$RefGlobal)(($sd1$Types$Meta$USR)($sd1$Compiler$TestHelpers$moduleUmr, "reset")), ({
                definedAt: $sd1$Types$Pos$T,
                freeTyvars: $core$Dict$empty,
                freeUnivars: $core$Dict$empty,
                type: ($sd1$Types$Ast$toImm)(($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($sd1$Compiler$TestHelpers$taNumber, $core$Core$Nil), $sd1$Compiler$TestHelpers$taNone)),
              }), $2);
            }))(((($2) => {
              return ($core$Dict$insert)(($sd1$Types$Ast$RefGlobal)(($sd1$Types$Meta$USR)($sd1$Compiler$TestHelpers$moduleUmr, "add")), ({
                definedAt: $sd1$Types$Pos$T,
                freeTyvars: $core$Dict$empty,
                freeUnivars: $core$Dict$empty,
                type: ($sd1$Types$Ast$toImm)(($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($sd1$Compiler$TestHelpers$taNumber, ($core$Core$Cons)($sd1$Compiler$TestHelpers$taNumber, $core$Core$Nil)), $sd1$Compiler$TestHelpers$taNumber)),
              }), $2);
            }))($0.variables)),
          }));
        }))();
        let $lastUnificationVarId = (basics_cloneImm)($luv);
        return (($core$Result$onOk)((($taModule) => {
          return (($core$Result$onOk)((($moduleWithDestroy) => {
            const $toMatch = (($7) => {
              const $pattern = $7.first;
              const $def = $7.second;
              return (((($pattern)[0] === "PatternAny") && ((($pattern)[2].maybeName)[0] === "Just"))
                ? ((() => {
                  const $maybeAnnotation = ($pattern)[2].maybeAnnotation;
                  const $name = (($pattern)[2].maybeName)[1];
                  return ((sp_equal)($name, $targetName)
                    ? ($core$Maybe$Just)($def)
                    : $core$Maybe$Nothing);
                }))()
                : (true
                  ? $core$Maybe$Nothing
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck_Test.sp 131:8', (sp_toHuman)($pattern))));
            });
            const $matches = ((($1) => {
              return ($core$List$filterMap)($toMatch, $1);
            }))(($core$Dict$toList)($moduleWithDestroy.valueDefs));
            return ((($matches)[0] === "Nil")
              ? ($core$Result$Err)("dict fail")
              : ((($matches)[0] === "Cons")
                ? ((() => {
                  const $def = ($matches)[1];
                  const $tail = ($matches)[2];
                  return ($core$Result$Ok)(($sd1$Compiler$TypeCheck_Test$normalizeOut)(({
                    freeTyvars: $def.freeTyvars,
                    type: $def.type.raw,
                  })));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck_Test.sp 142:4', (sp_toHuman)($matches))));
          })))(($sd1$Compiler$TestHelpers$resErrorToStrippedText)(($sd1$Compiler$UniquenessCheck$doModule)($taModule)));
        })))(($sd1$Compiler$TestHelpers$resErrorToStrippedText)(((($2) => {
          return ((__re__ = ($sd1$Compiler$TypeCheck$doModule)($lastUnificationVarId, $typeCheckGlobalEnv, $2)), ($lastUnificationVarId = (__re__)[1]), (__re__)[0]);
        }))($caModule)));
      })))(($sd1$Compiler$TestHelpers$resErrorToStrippedText)(((($1) => {
        return ($sd1$Compiler$TypeCheck$initStateAndGlobalEnv)($core$Core$Nil, $1);
      }))(($core$Core$Cons)($caModule, $core$Core$Nil))));
    })))(($sd1$Compiler$TestHelpers$resErrorToStrippedText)(((($1) => {
      return ($sd1$Compiler$MakeCanonical$textToCanonicalModule)(true, $1);
    }))($params)));
  });
});

const $sd1$Compiler$TypeCheck_Test$tyvar = $sd1$Compiler$TestHelpers$taTyvar;

const $sd1$Compiler$TypeCheck_Test$functions = ($sd1$Test$Group)("functions", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Known function with correct params", "a = add 3 1", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: $sd1$Compiler$TestHelpers$taNumber,
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Known function with wrong *number* of args", "a = add False", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$errorContains)(($core$Core$Cons)("Number", ($core$Core$Cons)("Arguments", $core$Core$Nil)))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Known function with wrong params", "a = add False 1", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$errorContains)(($core$Core$Cons)("Bool", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Function inference 1", "a = fn x: add x 1", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($sd1$Compiler$TestHelpers$taNumber, $core$Core$Nil), $sd1$Compiler$TestHelpers$taNumber),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Function inference 2: same as 1, but with swapped args", "a = fn x: add 1 x", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($sd1$Compiler$TestHelpers$taNumber, $core$Core$Nil), $sd1$Compiler$TestHelpers$taNumber),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] fn had type None", "a = fn x: 1", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeFn)(($core$Core$Cons)(($sd1$Types$TypedAst$ParSp)(($sd1$Types$Ast$toImm)(($sd1$Compiler$TypeCheck_Test$tyvar)(1))), $core$Core$Nil), ($sd1$Types$Ast$toUni)($sd1$Compiler$TestHelpers$taNumber)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Multiple arguments are correctly inferred", "a = fn x, y, z: x + y + z", ($sd1$Compiler$TypeCheck_Test$infer)("a"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Annotation should be consistent with mutability", "f as fn @Number: Number = fn a:\n  a", ($sd1$Compiler$TypeCheck_Test$infer)("f"), ($sd1$Test$errorContains)(($core$Core$Cons)("RecyclingDoesNotMatch", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] on is missing tyvars?", "andThen as [a] = []\n\non = andThen", ($sd1$Compiler$TypeCheck_Test$infer)("on"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: $sd1$Compiler$TestHelpers$taBool,
}))), $core$Core$Nil))))))))));

const $sd1$Compiler$TypeCheck_Test$higherOrderTypes = ($sd1$Test$Group)("higher order types", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Parse precedence", "union T a = T a\n\na as fn T a: T a =\n    fn l: l", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: ($sd1$Compiler$TypeCheck_Test$freeTyvarsAnnotated)(($core$Core$Cons)(({
    first: 1,
    second: "a",
  }), $core$Core$Nil)),
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)(($sd1$Types$TypedAst$TypeExact)(($sd1$Compiler$TestHelpers$localType)("T"), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$tyvar)(1), $core$Core$Nil)), $core$Core$Nil), ($sd1$Types$TypedAst$TypeExact)(($sd1$Compiler$TestHelpers$localType)("T"), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$tyvar)(1), $core$Core$Nil))),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Union type constructors", "union X a = L\nl = L", ($sd1$Compiler$TypeCheck_Test$infer)("l"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeExact)(($sd1$Compiler$TestHelpers$localType)("X"), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$tyvar)(1), $core$Core$Nil)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] type check mistakes a union type with free tyvars for a free tyvar?", "union O r e o = O r e o\n\nrun as fn (fn r: O r e o), r: O r e o =\n   fn rToOreo, r:\n   rToOreo r", ($sd1$Compiler$TypeCheck_Test$infer)("run"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Wrong should be Text", "union O o = O Text o\n\nfun as Number: Text: O wrong = _: a:\n    O a a", ($sd1$Compiler$TypeCheck_Test$infer)("fun"), ($sd1$Test$errorContains)(($core$Core$Cons)("wrong", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Should complain about undefined type argument", "union O a = O Text output\nx = 1", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)(($core$Core$Cons)("output", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Named vars can't be refined?", "union Wrap a = W a\n\nf as fn a: Wrap a =\n    fn a: a", ($sd1$Compiler$TypeCheck_Test$infer)("f"), ($sd1$Test$errorContains)(($core$Core$Cons)("Wrap", $core$Core$Nil))), $core$Core$Nil)))))));

const $sd1$Compiler$TypeCheck_Test$if_else = ($sd1$Test$Group)("if..else", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("basic functionality", "x =\n    fn q:\n    if q then 1\n    else 2", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeFn)(($core$Core$Cons)(($sd1$Types$TypedAst$ParSp)(($sd1$Types$Ast$toImm)($sd1$Compiler$TestHelpers$taBool)), $core$Core$Nil), ($sd1$Types$Ast$toUni)($sd1$Compiler$TestHelpers$taNumber)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("rejects non-bool conditions", "x =\n    fn q:\n    if 1 then 1\n    else 2", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)(($core$Core$Cons)("Bool", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("rejects non-matching blocks", "x =\n    fn q:\n    if q then 2\n    else False", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)(($core$Core$Cons)("Number", $core$Core$Nil))), $core$Core$Nil))));

const $sd1$Compiler$TypeCheck_Test$misc = ($sd1$Test$Group)("Misc", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Placeholder work with unique args", "stuff as fn !Number: Number = todo \"\"\nv =\n    1 >> stuff __", ($sd1$Compiler$TypeCheck_Test$infer)("v"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: $sd1$Compiler$TestHelpers$taNumber,
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] named tyvars should not \"bleed\" to other definitions", "union DD q =\n    , RBEmpty_elm_builtin\n\nempty as DD key =\n    RBEmpty_elm_builtin\n\nmerge as fn (fn key, b, res: res), res: res =\n  fn rightStep, initialResult:\n\n  stepState as fn key, b, [key & a] & res: [key & a] & res =\n    fn rKey, rValue, q:\n    try q.first as\n      , []: q\n\n  initialResult", ($sd1$Compiler$TypeCheck_Test$infer)("merge"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Constructors not being generalized led to tyvar bleed", "union DD a b = Blah\n\nddget as fn a, DD a b: DD a b =\n    fn a, b:\n    Blah\n\nformatSnippet as Text =\n    try [\"\"] as\n        , [\"emphasys\", s]: s\n\nfmtBlock as Text =\n    try ddget 1 Blah as\n        , Blah:\n            \"\"", ($sd1$Compiler$TypeCheck_Test$infer)("formatSnippet"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Non-annotated variables are not correctly inserted", "n = 3\n\nz as Number = n + 1", ($sd1$Compiler$TypeCheck_Test$infer)("z"), $sd1$Test$isOk), $core$Core$Nil)))));

const $sd1$Compiler$TypeCheck_Test$nonFunction = ($sd1$Test$Group)("NonFunction", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("SKIP (burnedout) Basic functionality", "blah as fn [a]: [a] with a NonFunction =\n  fn a:\n  a\n\nmeh =\n    blah [fn x: x]", ($sd1$Compiler$TypeCheck_Test$infer)("meh"), ($sd1$Test$errorContains)(($core$Core$Cons)("ErrorTypeAllowsFunctions", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("SKIP (burnedout) Constraint is enforced with annotation", "blah as fn [a]: [a] with a NonFunction =\n  fn a: a\n\nmeh as fn b: b =\n    fn a: blah a", ($sd1$Compiler$TypeCheck_Test$infer)("meh"), ($sd1$Test$errorContains)(($core$Core$Cons)("ErrorTypeAllowsFunctions", $core$Core$Nil))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("SKIP (burnedout) Constraint is enforced without annotation", "blah as fn [a]: [a] with a NonFunction =\n    fn a: a\n\nmeh =\n    fn a: blah a", ($sd1$Compiler$TypeCheck_Test$infer)("meh"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: ($core$Dict$ofOne)(1, ({
    allowFunctions: false,
    generalizedAt: $sd1$Types$Pos$G,
    generalizedFor: ($sd1$Types$Ast$RefLocal)(""),
    originalName: "",
  })),
  type: $sd1$Compiler$TestHelpers$taNumber,
}))), $core$Core$Nil))));

const $sd1$Compiler$TypeCheck_Test$patterns = ($sd1$Test$Group)("Patterns", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Constructor unpacking", "union Z a = Z a\n\nidentityFunction =\n   fn a:\n   Z b = Z a\n   b", ($sd1$Compiler$TypeCheck_Test$infer)("identityFunction"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$tyvar)(1), $core$Core$Nil), ($sd1$Compiler$TypeCheck_Test$tyvar)(1)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("List unpacking", "x =\n   fn q:\n   [ first, second ] = q\n   first", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)(($sd1$Compiler$TestHelpers$taList)(($sd1$Compiler$TypeCheck_Test$tyvar)(1)), $core$Core$Nil), ($sd1$Compiler$TypeCheck_Test$tyvar)(1)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Records are correctly unpacked", "x =\n    fn q:\n    { first } = q\n    first", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)(($sd1$Types$TypedAst$TypeRecord)($core$Maybe$Nothing, ($core$Dict$fromList)(($core$Core$Cons)(({
    first: "first",
    second: ($sd1$Compiler$TypeCheck_Test$tyvar)(1),
  }), $core$Core$Nil))), $core$Core$Nil), ($sd1$Compiler$TypeCheck_Test$tyvar)(1)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Constructors should instantiate their variable types", "each as fn [a], (fn a: b): None =\n    fn ls, f:\n    try ls as\n        , Core.Nil: None\n\nresult =\n    1 :: Core.Nil = Core.Nil", ($sd1$Compiler$TypeCheck_Test$infer)("result"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Trying to check against an inferred value?", "tuple as Text & Number =\n    \"\" & 1\n\nx =\n    (a as Text) & (b as Number) =\n        tuple", ($sd1$Compiler$TypeCheck_Test$infer)("x"), $sd1$Test$isOk), $core$Core$Nil))))));

const $sd1$Compiler$TypeCheck_Test$records = ($sd1$Test$Group)("Records", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Attribute access", "a = fn b: b.meh.blah", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)(($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(1), ($core$Dict$ofOne)("meh", ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(2), ($core$Dict$ofOne)("blah", ($sd1$Compiler$TypeCheck_Test$tyvar)(3))))), $core$Core$Nil), ($sd1$Compiler$TypeCheck_Test$tyvar)(3)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Attribute mutation", "a = fn @b: @b.meh.blah += 1", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeFn)(($core$Core$Cons)(($sd1$Types$TypedAst$ParRe)(($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(1), ($core$Dict$ofOne)("meh", ($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(2), ($core$Dict$ofOne)("blah", $sd1$Compiler$TestHelpers$taNumber))))), $core$Core$Nil), ($sd1$Types$Ast$toImm)($sd1$Compiler$TestHelpers$taNone)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Tuple3 direct item mutability", "x =\n    !a = 3 & False & 2\n\n    @a.third += 1", ($sd1$Compiler$TypeCheck_Test$infer)("x"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Tuple2 direct item mutability, annotated", "x =\n   fn _:\n   !a as Number & Number =\n     1 & 2\n\n   @a.first += 1", ($sd1$Compiler$TypeCheck_Test$infer)("x"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("functional update", "a = fn b: { b with x = 1 }", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(((($re) => {
  return ({
    freeTyvars: $core$Dict$empty,
    type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($re, $core$Core$Nil), $re),
  });
}))(($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(1), ($core$Dict$ofOne)("x", $sd1$Compiler$TestHelpers$taNumber))))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("SKIP(needs reordering) instantiate and refine inferred records", "a = fn t: { t with x = 1 }\nc = a", ($sd1$Compiler$TypeCheck_Test$infer)("c"), ($sd1$Test$isOkAndEqualTo)(((($re) => {
  return ({
    freeTyvars: $core$Dict$empty,
    type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($re, $core$Core$Nil), $re),
  });
}))(($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(1), ($core$Dict$ofOne)("x", $sd1$Compiler$TestHelpers$taNumber))))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] excessive forallness in records", "x =\n  fn q:\n  a = q.first\n  a", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$isOkAndEqualTo)(((($re) => {
  return ({
    freeTyvars: $core$Dict$empty,
    type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)($re, $core$Core$Nil), ($sd1$Compiler$TypeCheck_Test$tyvar)(2)),
  });
}))(($sd1$Types$TypedAst$TypeRecord)(($core$Maybe$Just)(1), ($core$Dict$ofOne)("first", ($sd1$Compiler$TypeCheck_Test$tyvar)(2)))))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] refineType when the record has a non-extensible alias", "alias A = { c as Number, d as Number }\n\nupd as fn A: A = fn a:\n  { a with c = .c + 1 }", ($sd1$Compiler$TypeCheck_Test$infer)("upd"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] infinite recursion on addSubstitution/unify_", "alias B = { l as [Text] }\n\nreadOne as fn B: (Text & B) =\n    fn b:\n    try b.l as\n        , []: \"\" & b\n        , [h, ...t]: h & { b with l = t }", ($sd1$Compiler$TypeCheck_Test$infer)("readOne"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] unifyToNonExtensibleRecord correctly substitutes the record extension", "alias R = { x as Number, y as Number }\n\nrec as fn R: R =\n    fn s:\n\n    if True then\n        { s with y = .y }\n    else\n        rec { s with y = .y }", ($sd1$Compiler$TypeCheck_Test$infer)("rec"), $sd1$Test$isOk), $core$Core$Nil)))))))))));

const $sd1$Compiler$TypeCheck_Test$statements = ($sd1$Test$Group)("statements", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Statement blocks should return the last statement's type", "a =\n  3\n  False", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: $sd1$Compiler$TestHelpers$taBool,
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Definition statement return type None", "a =\n  f = fn x: 3", ($sd1$Compiler$TypeCheck_Test$infer)("a"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: $sd1$Compiler$TestHelpers$taNone,
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Definition statement with annotation return type None", "a as None =\n  f = 3", ($sd1$Compiler$TypeCheck_Test$infer)("a"), $sd1$Test$isOk), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] Annotated declarations are actually typechecked", "x as None =\n    q = 1 + \"\"", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)($core$Core$Nil)), $core$Core$Nil)))));

const $sd1$Compiler$TypeCheck_Test$try_as = ($sd1$Test$Group)("try..as", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("basic functionality", "x =\n    fn q:\n    try q as\n        , True: 2\n        , _: 3", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeFn)(($core$Core$Cons)(($sd1$Types$TypedAst$ParSp)(($sd1$Types$Ast$toImm)($sd1$Compiler$TestHelpers$taBool)), $core$Core$Nil), ($sd1$Types$Ast$toUni)($sd1$Compiler$TestHelpers$taNumber)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("rejects non-matching patterns", "x =\n    fn q:\n    try q as\n        , True: 2\n        , []: 3", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)(($core$Core$Cons)("List", ($core$Core$Cons)("Bool", $core$Core$Nil)))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("rejects non-matching blocks", "x =\n    fn q:\n    try q as\n        , True: 2\n        , False: False", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)(($core$Core$Cons)("Number", ($core$Core$Cons)("Bool", $core$Core$Nil)))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("[reg] actually infers blocks", "x as Number =\n    try \"\" as\n        , \"\": y", ($sd1$Compiler$TypeCheck_Test$infer)("x"), ($sd1$Test$errorContains)(($core$Core$Cons)("y", $core$Core$Nil))), $core$Core$Nil)))));

const $sd1$Compiler$TypeCheck_Test$variableTypes = ($sd1$Test$Group)("Variable types", ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Identity", "id as fn a: a =\n  fn a: a", ($sd1$Compiler$TypeCheck_Test$infer)("id"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: ($sd1$Compiler$TypeCheck_Test$freeTyvarsAnnotated)(($core$Core$Cons)(({
    first: 1,
    second: "a",
  }), $core$Core$Nil)),
  type: ($sd1$Compiler$TestHelpers$taFunction)(($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$tyvar)(1), $core$Core$Nil), ($sd1$Compiler$TypeCheck_Test$tyvar)(1)),
}))), ($core$Core$Cons)(($sd1$Compiler$TypeCheck_Test$codeTest)("Annotated vars are instantiated when referenced", "q as [item] =\n  Core.Nil\n\nr as [Text] =\n      q", ($sd1$Compiler$TypeCheck_Test$infer)("r"), $sd1$Test$isOk), $core$Core$Nil)));

const $sd1$Compiler$TypeCheck_Test$tests = ($sd1$Test$Group)("TypeCheck", ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$functions, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$statements, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$variableTypes, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$higherOrderTypes, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$records, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$patterns, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$try_as, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$if_else, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$nonFunction, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$misc, $core$Core$Nil)))))))))));

const $sd1$Compiler$UniquenessCheck$errorConsumingRecycledParameter = (($env, $name, $pos, $state) => {
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $pos, $state, ($core$Core$Cons)(($name + " is passed as recycled, but the function wants to spend it"), $core$Core$Nil))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Compiler$UniquenessCheck$errorUniqueHasImmType = (($env, $name, $pos, $type, $state) => {
  return ([
    ((__re__ = ($sd1$Compiler$UniquenessCheck$addError)($env, $pos, $state, ($core$Core$Cons)(("Variable `" + ($name + "` is unique, but its type is:")), ($core$Core$Cons)((sp_toHuman)($type), $core$Core$Nil)))), ($state = (__re__)[1]), (__re__)[0]),
    $state,
  ]);
});

const $sd1$Human$Type$stack = (($content) => {
  return ($sd1$Human$Type$Block)($sd1$Human$Type$NotIndented, $content);
});

const $sd1$RefHierarchy_Test$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $sd1$RefHierarchy_Test$canonicalJsTest = ($sd1$RefHierarchy_Test$valueTest)("[reg] THIS SHOULD BE IN CANONICALTOJS", ((_0) => {
  return (basics_compare)(null, null);
}), ($sd1$Test$isOkAndEqualTo)(0));

const $sd1$RefHierarchy_Test$graph1 = ((() => {
  const $x = (($k, $l) => {
    return ({
      first: $k,
      second: ({
        first: $k,
        second: ($core$Set$fromList)($l),
      }),
    });
  });
  return ($core$Dict$fromList)(($core$Core$Cons)(($x)("a", ($core$Core$Cons)("b", ($core$Core$Cons)("d", $core$Core$Nil))), ($core$Core$Cons)(($x)("b", ($core$Core$Cons)("c", ($core$Core$Cons)("e", $core$Core$Nil))), ($core$Core$Cons)(($x)("c", ($core$Core$Cons)("e", ($core$Core$Cons)("d", $core$Core$Nil))), ($core$Core$Cons)(($x)("d", $core$Core$Nil), ($core$Core$Cons)(($x)("e", $core$Core$Nil), $core$Core$Nil))))));
}))();

const $sd1$RefHierarchy_Test$graph2 = ((() => {
  const $x = (($k, $l) => {
    return ({
      first: $k,
      second: ({
        first: $k,
        second: ($core$Set$fromList)($l),
      }),
    });
  });
  return ($core$Dict$fromList)(($core$Core$Cons)(($x)("a", ($core$Core$Cons)("b", ($core$Core$Cons)("d", $core$Core$Nil))), ($core$Core$Cons)(($x)("b", ($core$Core$Cons)("c", ($core$Core$Cons)("e", $core$Core$Nil))), ($core$Core$Cons)(($x)("c", ($core$Core$Cons)("e", ($core$Core$Cons)("d", $core$Core$Nil))), ($core$Core$Cons)(($x)("d", ($core$Core$Cons)("b", $core$Core$Nil)), ($core$Core$Cons)(($x)("e", $core$Core$Nil), $core$Core$Nil))))));
}))();

const $sd1$RefHierarchy_Test$tests = ($sd1$Test$Group)("RefHierarchy", ($core$Core$Cons)($sd1$RefHierarchy_Test$canonicalJsTest, ($core$Core$Cons)(($sd1$RefHierarchy_Test$valueTest)("Basic", ((_0) => {
  return ($sd1$RefHierarchy$reorder)($core$Tuple$second, $sd1$RefHierarchy_Test$graph1);
}), ($sd1$Test$isOkAndEqualTo)(({
  first: $core$Core$Nil,
  second: ($core$Core$Cons)("d", ($core$Core$Cons)("e", ($core$Core$Cons)("c", ($core$Core$Cons)("b", ($core$Core$Cons)("a", $core$Core$Nil))))),
}))), ($core$Core$Cons)(($sd1$RefHierarchy_Test$valueTest)("Circular", ((_0) => {
  return ($sd1$RefHierarchy$reorder)($core$Tuple$second, $sd1$RefHierarchy_Test$graph2);
}), ($sd1$Test$isOkAndEqualTo)(({
  first: ($core$Core$Cons)(($core$Core$Cons)("b", ($core$Core$Cons)("d", ($core$Core$Cons)("c", $core$Core$Nil))), $core$Core$Nil),
  second: ($core$Core$Cons)("d", ($core$Core$Cons)("e", ($core$Core$Cons)("c", ($core$Core$Cons)("b", ($core$Core$Cons)("a", $core$Core$Nil))))),
}))), $core$Core$Nil))));

const $sd2$Uniqueness$codeTest = (($1, $2, $3, $4) => {
  return ($sd1$Test$codeTest)(sp_toHuman, $1, $2, $3, $4);
});

const $sd2$Uniqueness$infer = $sd1$Compiler$TypeCheck_Test$infer;

const $sd2$Uniqueness$howDoesItLookLike = ($sd1$Test$Group)("How does mutability look like?", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Example: maintaining mutable state", "average as fn [Number]: Number =\n    fn numbers:\n\n    # Unique values can be changed in place, ie, \"mutated\"\n    !total as Number =\n        0\n\n    !count as Number =\n        0\n\n    (todo \"List.each\") numbers fn number:\n        @total += number\n        @count += 1\n\n    # In Squarepants division by 0 yields 0\n    total / count", ($sd2$Uniqueness$infer)("average"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("SKIP (needs IO in the test env) Example: File IO", "logToFile as fn @IO, Text: Result IO.Error None =\n    fn @io, content:\n\n    IO.openFile @io IO.Append \"blah.log\"\n    >> isOk fn @fileDescriptor:\n\n    IO.writeFile @io content @fileDescriptor\n\n    # fileDescriptor is automatically closed here", ($sd2$Uniqueness$infer)("logToFile"), $sd1$Test$isOk), $core$Core$Nil)));

const $sd2$Uniqueness$mutation = ($sd1$Test$Group)("Mutation", ($core$Core$Cons)(($sd1$Test$Group)("Uniques can be mutated in place", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Mutation does NOT consume the unique", "scope =\n    !x = 1\n    @x += 1\n    @x += 1", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Recycling requires the unique not to be spent", "scope =\n    !x = 1\n    (todo \"consume\") x\n    @x += 1", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("used again here", $core$Core$Nil))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Test$Group)("A function can be defined to mutate its arguments", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("base", "funz as fn @Number: None =\n    fn @a:\n    @a += 3\n\nscope =\n    !x = 0\n    funz @x\n    funz @x", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), $core$Core$Nil)), ($core$Core$Cons)(($sd1$Test$Group)("Calling a function that recycles a unique variable temporarily consumes the variable.", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("base", "scope =\n    !x = 0\n    (todo \"funz\") @x @x", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("twice", $core$Core$Nil))), $core$Core$Nil)), $core$Core$Nil))));

const $sd2$Uniqueness$parentScope = ($sd1$Test$Group)("Recycling a variable in the parent scope", ($core$Core$Cons)(($sd1$Test$Group)("A function that recycles any unique belonging to an ancestor scope \"requires\" that unique.", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("LetIns cannot return functions with requirements", "scope =\n    !x =\n        1\n\n    f =\n        fn n:\n        @x += n\n        None\n\n    f", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("x", ($core$Core$Cons)("from outside", $core$Core$Nil)))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Functions cannot return functions with UNIQUE requirements", "f =\n    fn !x:\n    fn n:\n    @x += n\n    None", ($sd2$Uniqueness$infer)("f"), ($sd1$Test$errorContains)(($core$Core$Cons)("x", ($core$Core$Cons)("from outside", $core$Core$Nil)))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Functions cannot return functions with RECYCLED requirements", "f =\n    fn @x:\n    fn n:\n    @x += n\n    None", ($sd2$Uniqueness$infer)("f"), ($sd1$Test$errorContains)(($core$Core$Cons)("x", ($core$Core$Cons)("from outside", $core$Core$Nil)))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("The Array Test", "union Array a = Meh\n\narray_push as fn a, @Array a: None = todo \"\"\n\naddFunctions as fn @Array (fn Int: Int): None =\n    fn @functions:\n\n    !x =\n        1\n\n    f as fn Int: Int =\n        fn n:\n        @x += 1\n        n\n\n    array_push f @functions\n    None", ($sd2$Uniqueness$infer)("addFunctions"), ($sd1$Test$errorContains)(($core$Core$Cons)("x", ($core$Core$Cons)("outside", $core$Core$Nil)))), $core$Core$Nil))))), $core$Core$Nil));

const $sd2$Uniqueness$polymorphism = ($sd1$Test$Group)("Polymorphism", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Basic syntax", "fun as fn (fn 1?a: 2?b), 1?a: 2?b =\n    fn f, 1?a:\n\n    f a", ($sd2$Uniqueness$infer)("fun"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("A function that returns a Uni can be used in place of a function that returns an Imm", "meh as fn (fn None: Number): Number =\n    fn f: f None\n\nblah = meh (fn None: 1)", ($sd2$Uniqueness$infer)("blah"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("A function that returns an Imm CANNOT be used in place of a function that returns an Uni", "meh as fn (fn None: !Number): !Number =\n    fn f: f None\n\nx as Number = 1\n\nblah = meh (fn None: x)", ($sd2$Uniqueness$infer)("blah"), ($sd1$Test$errorContains)(($core$Core$Cons)("return", ($core$Core$Cons)("uniqueness", $core$Core$Nil)))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("a Uni, b Uni", "union Re error payload = Er error, Okk payload\nisOkk as fn (fn 1?a: 2?Re error b), 1?Re error a: 2?Re error b = todo \"\"\n\nscope =\n    !v = isOkk (fn !a: Okk 0) (Okk 0)", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("a Uni, b Imm", "union Result_ error payload = Err_ error, Ok_ payload\nisOk_ as fn (fn 1?a: 2?Result_ error b), 1?Result_ error a: 2?Result_ error b = todo \"\"\nimmB as Number = 1\n\nv = isOk_ (fn !a: Ok_ immB) (Ok_ 0)", ($sd2$Uniqueness$infer)("v"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("No annotation", "na = fn 0?x: x", ($sd2$Uniqueness$infer)("na"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeFn)(($core$Core$Cons)(($sd1$Types$TypedAst$ParSp)(({
    raw: ($sd1$Types$TypedAst$TypeVar)(1),
    uni: ($sd1$Types$Ast$Depends)(0),
  })), $core$Core$Nil), ({
    raw: ($sd1$Types$TypedAst$TypeVar)(1),
    uni: ($sd1$Types$Ast$Depends)(0),
  })),
}))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Generalization", "na as fn 1?a: 1?a =\n    fn 1?x: x\n\nscope as None =\n    !uni = na 0\n\nnone as None =\n    na scope", ($sd2$Uniqueness$infer)("na"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("[rec] variable without any uniqueness flag should be imm", "scope =\n    num = 1\n\n    x as Number = num + 1\n    y as Number = num + 2", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), $core$Core$Nil)))))))));

const $sd2$Uniqueness$records = ($sd1$Test$Group)("Records", ($core$Core$Cons)(($sd1$Test$Group)("The attribute of a mutable record can be accessed as a mutable:", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Simple case", "scope =\n    !record = { x = 0, y = 0 }\n    @record.x += 3", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Reject double reference", "scope =\n    !record = { x = 0, y = 0 }\n    (todo \"\") @record.x @record.y", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("same unique twice in the same function call", $core$Core$Nil))), $core$Core$Nil))), $core$Core$Nil));

const $sd2$Uniqueness$unions = ($sd1$Test$Group)("Unions", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Uniques inside immutables are converted to immutables", "union Z a = Z a\nx = Z 0", ($sd2$Uniqueness$infer)("x"), ($sd1$Test$isOkAndEqualTo)(({
  freeTyvars: $core$Dict$empty,
  type: ($sd1$Types$TypedAst$TypeExact)(($sd1$Compiler$TestHelpers$localType)("Z"), ($core$Core$Cons)($sd1$Compiler$TestHelpers$taNumber, $core$Core$Nil)),
}))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("[reg] Lists of immutables", "i as Number = 1\nx = [ i, i ]", ($sd2$Uniqueness$infer)("x"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("[reg] solveOneEquality can receive switched given/required when evaluating a cast?", "z as [fn None: None] = (fn None: None) :: []", ($sd2$Uniqueness$infer)("z"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("LetIn: Unpack immutable to immutable", "union Z a = Z a\nscope =\n    x = Z 0\n    (Z y) = x", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("LetIn: Unpack unique to immutable", "union Z a = Z a\nscope =\n    !x = Z 0\n    (Z y) = x", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("LetIn: Unpack unique to unique", "union Z a = Z a\nscope =\n    !x = Z 0\n    #!(Z y) = x\n    #@y += 1", ($sd2$Uniqueness$infer)("scope"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("LetIn: Unpack immutable to unique", "union Z a = Z a\nscope =\n    x = Z 0\n    !(Z y) = x", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("y", ($core$Core$Cons)("Unique", $core$Core$Nil)))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Fn: Unpack immutable to immutable", "union Z a = Z a\nf as fn Z a: Z a =\n     fn Z a: Z a", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Fn: Unpack unique to immutable", "union Z a = Z a\nf as fn !(Z a): Z a =\n     fn !(Z a): Z a", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Fn: Unpack unique to unique", "union Z a = Z a\nf as fn !(Z a): !(Z a) =\n     fn !(Z a): Z a", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Fn: Unpack immutable to unique", "union Z a = Z a\nf as fn Z a: !(Z a) =\n     fn Z a: Z a", ($sd2$Uniqueness$infer)("f"), ($sd1$Test$errorContains)(($core$Core$Cons)("Unique", $core$Core$Nil))), $core$Core$Nil))))))))))));

const $sd2$Uniqueness$uniquenessTyping = ($sd1$Test$Group)("Uniqueness Typing", ($core$Core$Cons)(($sd1$Test$Group)("All literal expressions allow uniqueness", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("failure", "f as fn Number: !Number = fn x: x", ($sd2$Uniqueness$infer)("f"), ($sd1$Test$errorContains)(($core$Core$Cons)("ErrorUniquenessDoesNotMatch", $core$Core$Nil))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Number", "f as fn a: !Number = fn _: 1", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Text", "f as fn a: !Text = fn _: \"meh\" ", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Record", "f as fn a: !{} = fn _: {}", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Constructor 1", "f as fn a: !Bool = fn _: True", ($sd2$Uniqueness$infer)("f"), $sd1$Test$isOk), $core$Core$Nil)))))), ($core$Core$Cons)(($sd1$Test$Group)("Conversions", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Immutables cannot be used in place of uniques 2", "scope =\n    x = 1\n    @x += 1", ($sd2$Uniqueness$infer)("a"), ($sd1$Test$errorContains)(($core$Core$Cons)("ErrorShouldBeUnique", $core$Core$Nil))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("Uniques can be implicitly transformed in immutables", "a as Number = 1", ($sd2$Uniqueness$infer)("a"), $sd1$Test$isOk), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Test$Group)("A variable with mutable type must be explicitly declared as mutable with `!`", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("1", "z =\n    !a as Number = 1", ($sd2$Uniqueness$infer)("z"), $sd1$Test$isOk), $core$Core$Nil)), ($core$Core$Cons)(($sd1$Test$Group)("Referencing a mutable variable \"spends\" it", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("base", "scope =\n    !x =\n        1\n\n    !y =\n        # The first time we do it it works!\n        x\n\n    !z =\n        # But here `x` is now spent, so we get a compiler error!\n        x", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("used already here", $core$Core$Nil))), ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("tuple", "scope =\n    !x =\n        1\n\n    !y =\n        x & x", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("used already here", $core$Core$Nil))), $core$Core$Nil))), ($core$Core$Cons)(($sd1$Test$Group)("A function cannot consume uniques outside its own scope.", ($core$Core$Cons)(($sd2$Uniqueness$codeTest)("base", "scope =\n    !x = 1\n    fn z: x", ($sd2$Uniqueness$infer)("scope"), ($sd1$Test$errorContains)(($core$Core$Cons)("outside their body", ($core$Core$Cons)("x", $core$Core$Nil)))), $core$Core$Nil)), $core$Core$Nil))))));

const $sd2$Uniqueness$specs = ($sd1$Test$Group)("Uniqueness", ($core$Core$Cons)($sd2$Uniqueness$howDoesItLookLike, ($core$Core$Cons)($sd2$Uniqueness$uniquenessTyping, ($core$Core$Cons)($sd2$Uniqueness$mutation, ($core$Core$Cons)($sd2$Uniqueness$parentScope, ($core$Core$Cons)($sd2$Uniqueness$polymorphism, ($core$Core$Cons)($sd2$Uniqueness$unions, ($core$Core$Cons)($sd2$Uniqueness$records, $core$Core$Nil))))))));

const $sd1$Main$allTests = ($core$Core$Cons)($sd1$Compiler$Lexer_Test$tests, ($core$Core$Cons)($sd1$Compiler$Parser_Test$tests, ($core$Core$Cons)($sd1$Compiler$MakeCanonical_Test$tests, ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$tests, ($core$Core$Cons)($core$Hash_Test$tests, ($core$Core$Cons)($core$Array_Test$tests, ($core$Core$Cons)($core$List_Test$tests, ($core$Core$Cons)($core$Dict_Test$tests, ($core$Core$Cons)($sd1$RefHierarchy_Test$tests, ($core$Core$Cons)($sd2$Uniqueness$specs, $core$Core$Nil))))))))));

const $sd1$Platforms$Browser$virtualDomModule = (($1) => {
  return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Browser, "VirtualDom"), $1);
});

const $sd1$Platforms$Browser$overrides = ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsCreateTextNode"),
  second: "virtualDom_jsCreateTextNode",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsCreateElement"),
  second: "virtualDom_jsCreateElement",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsReplaceWith"),
  second: "virtualDom_jsReplaceWith",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsAppendChild"),
  second: "virtualDom_jsAppendChild",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsSetProperty"),
  second: "virtualDom_jsSetProperty",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsSetAttribute"),
  second: "virtualDom_jsSetAttribute",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsRemoveAttribute"),
  second: "virtualDom_jsRemoveAttribute",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsAddEventListener"),
  second: "virtualDom_jsAddEventListener",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("jsRemoveEventListener"),
  second: "virtualDom_jsRemoveEventListener",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("eventToText"),
  second: "virtualDom_eventToText",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("eventToFloat"),
  second: "virtualDom_eventToFloat",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("setChild"),
  second: "virtualDom_setChild",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("removeAllChildrenStartingFromIndex"),
  second: "virtualDom_removeAllChildrenStartingFromIndex",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("drawCanvas"),
  second: "virtualDom_drawCanvas",
}), ($core$Core$Cons)(({
  first: ($sd1$Platforms$Browser$virtualDomModule)("setViewportOf"),
  second: "virtualDom_setViewportOf",
}), $core$Core$Nil)))))))))))))));

const $sd1$Targets$Javascript$EmittableToJs$accessAttrs = (($attrPath, $e) => {
  return ($core$List$for)($e, $attrPath, $sd1$Types$JavascriptAst$AccessWithDot);
});

const $sd1$Targets$Javascript$EmittableToJs$accessArrayIndex = (($index, $j) => {
  return ((($0) => {
    return ($sd1$Types$JavascriptAst$AccessWithBrackets)($0, $j);
  }))(($sd1$Types$JavascriptAst$Literal)((text_fromNumber)($index)));
});

const $sd1$Targets$Javascript$EmittableToJs$literalString = (($str) => {
  const $escaped = ((($2) => {
    return ($core$Text$replace)("\"", "\\\"", $2);
  }))(((($2) => {
    return ($core$Text$replace)("\n", "\\n", $2);
  }))($str));
  return ($sd1$Types$JavascriptAst$Literal)(("\"" + ($escaped + "\"")));
});

const $sd1$Targets$Javascript$EmittableToJs$recycleTempVariable = ($sd1$Types$JavascriptAst$Var)("__re__");

const $sd1$Targets$Javascript$EmittableToJs$translateName = (($n) => {
  return ("$" + $n);
});

const $sd1$Targets$Javascript$EmittableToJs$makeCall = (($env, $jaRef, $args) => {
  const $call = ((($1) => {
    return ($sd1$Types$JavascriptAst$Call)($jaRef, $1);
  }))(((($1) => {
    return ($core$List$map)((($2) => {
      return ($sd1$Targets$Javascript$EmittableToJs$translateArg)(({
        nativeBinop: false,
      }), $env, $2);
    }), $1);
  }))($args));
  const $asRecycled = (($arg) => {
    return ((($arg)[0] === "ArgumentSpend")
      ? $core$Maybe$Nothing
      : ((($arg)[0] === "ArgumentRecycle")
        ? ((() => {
          const $rawType = ($arg)[1];
          const $attrPath = ($arg)[2];
          const $name = ($arg)[3];
          return ($core$Maybe$Just)(((($1) => {
            return ($sd1$Targets$Javascript$EmittableToJs$accessAttrs)($attrPath, $1);
          }))(($sd1$Types$JavascriptAst$Var)(($sd1$Targets$Javascript$EmittableToJs$translateName)($name))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 376:8', (sp_toHuman)($arg))));
  });
  const $recycledArgs = ($core$List$filterMap)($asRecycled, $args);
  return ((sp_equal)($recycledArgs, $core$Core$Nil)
    ? $call
    : ((() => {
      const $zzz = (($index, $arg) => {
        const $bracketIndex = ($sd1$Types$JavascriptAst$Literal)((text_fromNumber)(($index + 1)));
        return ($sd1$Types$JavascriptAst$Binop)("=", $arg, ($sd1$Types$JavascriptAst$AccessWithBrackets)($bracketIndex, $sd1$Targets$Javascript$EmittableToJs$recycleTempVariable));
      });
      return ($sd1$Types$JavascriptAst$Comma)(($core$List$concat)(($core$Core$Cons)(($core$Core$Cons)(($sd1$Types$JavascriptAst$Binop)("=", $sd1$Targets$Javascript$EmittableToJs$recycleTempVariable, $call), $core$Core$Nil), ($core$Core$Cons)(((($1) => {
        return ($core$List$indexedMap)($zzz, $1);
      }))($recycledArgs), ($core$Core$Cons)(($core$Core$Cons)(($sd1$Types$JavascriptAst$AccessWithBrackets)(($sd1$Types$JavascriptAst$Literal)("0"), $sd1$Targets$Javascript$EmittableToJs$recycleTempVariable), $core$Core$Nil), $core$Core$Nil)))));
    }))());
});

const $sd1$Targets$Javascript$EmittableToJs$translateSource = (($src) => {
  return ((($src)[0] === "Core")
    ? "core"
    : ((($src)[0] === "Posix")
      ? "posix"
      : ((($src)[0] === "Browser")
        ? "browser"
        : ((($src)[0] === "SourceDirId")
          ? ((() => {
            const $id = ($src)[1];
            return $id;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 673:4', (sp_toHuman)($src))))));
});

const $sd1$Targets$Javascript$EmittableToJs$translateUsr = (($1) => {
  const $source = (($1)[1])[1];
  const $modulePath = (($1)[1])[2];
  const $name = ($1)[2];
  return ("$" + (($sd1$Targets$Javascript$EmittableToJs$translateSource)($source) + ("$" + (($core$Text$replace)("/", "$", $modulePath) + ("$" + $name)))));
});

const $sd1$Targets$Javascript$EmittableToJs$maybeOverrideUsr = (($env, $usr) => {
  const $3 = ($core$Dict$get)($usr, $env.overrides);
  return (((($3)[0] === "Just") && ((($3)[1])[0] === "Override"))
    ? ((() => {
      const $call = (($3)[1])[1].call;
      const $value = (($3)[1])[1].value;
      return ($value)($env);
    }))()
    : ((($3)[0] === "Nothing")
      ? ($sd1$Types$JavascriptAst$Var)(($sd1$Targets$Javascript$EmittableToJs$translateUsr)($usr))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 259:4', (sp_toHuman)($3))));
});

const $sd1$Targets$Javascript$EmittableToJs$translateExpression = (($env, $eaExpression) => {
  return (((($eaExpression)[0] === "Variable") && ((($eaExpression)[1])[0] === "RefLocal"))
    ? ((() => {
      const $name = (($eaExpression)[1])[1];
      return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Var)(($sd1$Targets$Javascript$EmittableToJs$translateName)($name)));
    }))()
    : (((($eaExpression)[0] === "Variable") && ((($eaExpression)[1])[0] === "RefGlobal"))
      ? ((() => {
        const $usr = (($eaExpression)[1])[1];
        return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Targets$Javascript$EmittableToJs$maybeOverrideUsr)($env, $usr));
      }))()
      : ((($eaExpression)[0] === "Call")
        ? ((() => {
          const $ref = ($eaExpression)[1];
          const $args = ($eaExpression)[2];
          const $maybeNativeOverride = (((($ref)[0] === "Variable") && ((($ref)[1])[0] === "RefGlobal"))
            ? ((() => {
              const $usr = (($ref)[1])[1];
              return ($core$Dict$get)($usr, $env.overrides);
            }))()
            : (true
              ? $core$Maybe$Nothing
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 439:16', (sp_toHuman)($ref))));
          return (((($maybeNativeOverride)[0] === "Just") && ((($maybeNativeOverride)[1])[0] === "Override"))
            ? ((() => {
              const $call = (($maybeNativeOverride)[1])[1].call;
              return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($call)($env, $args));
            }))()
            : ((($maybeNativeOverride)[0] === "Nothing")
              ? ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Targets$Javascript$EmittableToJs$makeCall)($env, ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $ref), $args))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 443:12', (sp_toHuman)($maybeNativeOverride))));
        }))()
        : ((($eaExpression)[0] === "Fn")
          ? ((() => {
            const $eaArgs = ($eaExpression)[1];
            const $body = ($eaExpression)[2];
            const $argsWithNames = ((() => {
              const $zzz = (($index, $3) => {
                const $re = $3.first;
                const $maybeName = $3.second;
                return ((($maybeName)[0] === "Just")
                  ? ((() => {
                    const $name = ($maybeName)[1];
                    return ({
                      first: $re,
                      second: ($sd1$Targets$Javascript$EmittableToJs$translateName)($name),
                    });
                  }))()
                  : ((($maybeName)[0] === "Nothing")
                    ? ({
                      first: $re,
                      second: ("_" + (text_fromNumber)($index)),
                    })
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 457:20', (sp_toHuman)($maybeName))));
              });
              return ($core$List$indexedMap)($zzz, $eaArgs);
            }))();
            const $recycledPars = ((($1) => {
              return ($core$List$map)((($4) => {
                const $name = $4.second;
                return ($sd1$Types$JavascriptAst$Var)($name);
              }), $1);
            }))(((($1) => {
              return ($core$List$filter)($core$Tuple$first, $1);
            }))($argsWithNames));
            const $statementsRaw = ((() => {
              const $3 = ($sd1$Targets$Javascript$EmittableToJs$translateExpression)($env, $body);
              return ((($3)[0] === "Inline")
                ? ((() => {
                  const $expr = ($3)[1];
                  return ($core$Core$Cons)(($sd1$Types$JavascriptAst$Return)($expr), $core$Core$Nil);
                }))()
                : ((($3)[0] === "Block")
                  ? ((() => {
                    const $block = ($3)[1];
                    return $block;
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 469:16', (sp_toHuman)($3))));
            }))();
            const $statementsFinal = ((sp_equal)($recycledPars, $core$Core$Nil)
              ? $statementsRaw
              : ((() => {
                const $addRecycled = (($stat) => {
                  return ((($stat)[0] === "Return")
                    ? ((() => {
                      const $e = ($stat)[1];
                      return ($sd1$Types$JavascriptAst$Return)(($sd1$Types$JavascriptAst$Array)((sp_cons)($e, $recycledPars)));
                    }))()
                    : (true
                      ? $stat
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 483:24', (sp_toHuman)($stat))));
                });
                return ($core$List$map)($addRecycled, $statementsRaw);
              }))());
            return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$BlockLambda)(($core$List$map)($core$Tuple$second, $argsWithNames), $statementsFinal));
          }))()
          : ((($eaExpression)[0] === "LetIn")
            ? ((() => {
              const $inExpression = ($eaExpression)[1].inExpression;
              const $letExpression = ($eaExpression)[1].letExpression;
              const $maybeName = ($eaExpression)[1].maybeName;
              const $type = ($eaExpression)[1].type;
              const $inStatements = ((() => {
                const $3 = ($sd1$Targets$Javascript$EmittableToJs$translateExpression)($env, $inExpression);
                return ((($3)[0] === "Block")
                  ? ((() => {
                    const $stats = ($3)[1];
                    return $stats;
                  }))()
                  : ((($3)[0] === "Inline")
                    ? ((() => {
                      const $jaExpression = ($3)[1];
                      return ($core$Core$Cons)(($sd1$Types$JavascriptAst$Return)($jaExpression), $core$Core$Nil);
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 495:16', (sp_toHuman)($3))));
              }))();
              return ((($maybeName)[0] === "Nothing")
                ? ((() => {
                  const $3 = ($sd1$Targets$Javascript$EmittableToJs$translateExpression)($env, $letExpression);
                  return ((($3)[0] === "Inline")
                    ? ((() => {
                      const $expr = ($3)[1];
                      return ($sd1$Targets$Javascript$EmittableToJs$Block)((sp_cons)(($sd1$Types$JavascriptAst$Eval)($expr), $inStatements));
                    }))()
                    : ((($3)[0] === "Block")
                      ? ((() => {
                        const $stats = ($3)[1];
                        return ($sd1$Targets$Javascript$EmittableToJs$Block)(($core$List$concat)(($core$Core$Cons)($stats, ($core$Core$Cons)($inStatements, $core$Core$Nil))));
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 501:20', (sp_toHuman)($3))));
                }))()
                : ((($maybeName)[0] === "Just")
                  ? ((() => {
                    const $name = ($maybeName)[1];
                    const $letStatement = ((($2) => {
                      return ($sd1$Types$JavascriptAst$Define)((sp_equal)($type.uni, $sd1$Types$Ast$Uni), ($sd1$Targets$Javascript$EmittableToJs$translateName)($name), $2);
                    }))(((($1) => {
                      return ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $1);
                    }))($letExpression));
                    return ($sd1$Targets$Javascript$EmittableToJs$Block)((sp_cons)($letStatement, $inStatements));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 499:12', (sp_toHuman)($maybeName))));
            }))()
            : ((($eaExpression)[0] === "LiteralText")
              ? ((() => {
                const $string = ($eaExpression)[1];
                return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Targets$Javascript$EmittableToJs$literalString)($string));
              }))()
              : ((($eaExpression)[0] === "LiteralNumber")
                ? ((() => {
                  const $num = ($eaExpression)[1];
                  return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Literal)((text_fromNumber)($num)));
                }))()
                : ((($eaExpression)[0] === "Conditional")
                  ? ((() => {
                    const $test = ($eaExpression)[1];
                    const $true = ($eaExpression)[2];
                    const $false = ($eaExpression)[3];
                    return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Conditional)(($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $test), ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $true), ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $false)));
                  }))()
                  : ((($eaExpression)[0] === "And")
                    ? ((() => {
                      const $eaTests = ($eaExpression)[1];
                      const $jaTests = ($core$List$map)((($1) => {
                        return ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $1);
                      }), $eaTests);
                      const $3 = ($core$List$reverse)($jaTests);
                      return ((($3)[0] === "Nil")
                        ? ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Literal)("true"))
                        : ((($3)[0] === "Cons")
                          ? ((() => {
                            const $head = ($3)[1];
                            const $tail = ($3)[2];
                            return ($sd1$Targets$Javascript$EmittableToJs$Inline)(((($0) => {
                              return ($core$List$for)($0, $tail, (($test, $expr) => {
                                return ($sd1$Types$JavascriptAst$Binop)("&&", $test, $expr);
                              }));
                            }))($head));
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 537:12', (sp_toHuman)($3))));
                    }))()
                    : ((($eaExpression)[0] === "ShallowEqual")
                      ? ((() => {
                        const $a = ($eaExpression)[1];
                        const $b = ($eaExpression)[2];
                        return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Binop)("===", ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $a), ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $b)));
                      }))()
                      : ((($eaExpression)[0] === "LiteralArray")
                        ? ((() => {
                          const $items = ($eaExpression)[1];
                          return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Array)(((($1) => {
                            return ($core$List$map)((($1) => {
                              return ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $1);
                            }), $1);
                          }))($items)));
                        }))()
                        : ((($eaExpression)[0] === "ArrayAccess")
                          ? ((() => {
                            const $index = ($eaExpression)[1];
                            const $array = ($eaExpression)[2];
                            return ($sd1$Targets$Javascript$EmittableToJs$Inline)(((($1) => {
                              return ($sd1$Targets$Javascript$EmittableToJs$accessArrayIndex)($index, $1);
                            }))(((($1) => {
                              return ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $1);
                            }))($array)));
                          }))()
                          : ((($eaExpression)[0] === "Constructor")
                            ? ((() => {
                              const $usr = ($eaExpression)[1];
                              return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Targets$Javascript$EmittableToJs$maybeOverrideUsr)($env, $usr));
                            }))()
                            : ((($eaExpression)[0] === "ConstructorAccess")
                              ? ((() => {
                                const $argIndex = ($eaExpression)[1];
                                const $value = ($eaExpression)[2];
                                return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Targets$Javascript$EmittableToJs$accessArrayIndex)(($argIndex + 1), ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $value)));
                              }))()
                              : ((($eaExpression)[0] === "IsConstructor")
                                ? ((() => {
                                  const $name = ($eaExpression)[1];
                                  const $eaValue = ($eaExpression)[2];
                                  const $jaValue = ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $eaValue);
                                  const $jaExpr = (("True" === $name)
                                    ? $jaValue
                                    : (("False" === $name)
                                      ? ($sd1$Types$JavascriptAst$Unop)("!", $jaValue)
                                      : (true
                                        ? ($sd1$Types$JavascriptAst$Binop)("===", ($sd1$Targets$Javascript$EmittableToJs$accessArrayIndex)(0, $jaValue), ($sd1$Targets$Javascript$EmittableToJs$literalString)($name))
                                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 579:16', (sp_toHuman)($name)))));
                                  return ($sd1$Targets$Javascript$EmittableToJs$Inline)($jaExpr);
                                }))()
                                : ((($eaExpression)[0] === "LiteralRecord")
                                  ? ((() => {
                                    const $maybeExtend = ($eaExpression)[1];
                                    const $attrNamesAndValues = ($eaExpression)[2];
                                    const $obj = ($sd1$Types$JavascriptAst$Record)(((($0) => {
                                      return ($core$List$for)($0, $attrNamesAndValues, (($5, $d) => {
                                        const $name = $5.first;
                                        const $value = $5.second;
                                        return ($core$Dict$insert)($name, ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $value), $d);
                                      }));
                                    }))($core$Dict$empty));
                                    return ((($maybeExtend)[0] === "Nothing")
                                      ? ($sd1$Targets$Javascript$EmittableToJs$Inline)($obj)
                                      : ((($maybeExtend)[0] === "Just")
                                        ? ((() => {
                                          const $extend = ($maybeExtend)[1];
                                          return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$Call)(($sd1$Types$JavascriptAst$Var)("Object.assign"), ($core$Core$Cons)(($sd1$Types$JavascriptAst$Record)($core$Dict$empty), ($core$Core$Cons)(($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $extend), ($core$Core$Cons)($obj, $core$Core$Nil)))));
                                        }))()
                                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 599:12', (sp_toHuman)($maybeExtend))));
                                  }))()
                                  : ((($eaExpression)[0] === "RecordAccess")
                                    ? ((() => {
                                      const $attrName = ($eaExpression)[1];
                                      const $value = ($eaExpression)[2];
                                      return ($sd1$Targets$Javascript$EmittableToJs$Inline)(($sd1$Types$JavascriptAst$AccessWithDot)($attrName, ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $value)));
                                    }))()
                                    : ((($eaExpression)[0] === "MissingPattern")
                                      ? ((() => {
                                        const $location = ($eaExpression)[1];
                                        const $value = ($eaExpression)[2];
                                        return ($sd1$Targets$Javascript$EmittableToJs$Inline)(((($1) => {
                                          return ($sd1$Types$JavascriptAst$Call)(($sd1$Types$JavascriptAst$Literal)("sp_throw"), $1);
                                        }))(($core$Core$Cons)(($sd1$Types$JavascriptAst$Literal)("'Missing pattern in try..as'"), ($core$Core$Cons)(($sd1$Types$JavascriptAst$Literal)(("'" + ($location + "'"))), ($core$Core$Cons)(($sd1$Types$JavascriptAst$Call)(($sd1$Types$JavascriptAst$Literal)("sp_toHuman"), ($core$Core$Cons)(($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $value), $core$Core$Nil)), $core$Core$Nil)))));
                                      }))()
                                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 426:4', (sp_toHuman)($eaExpression))))))))))))))))))));
});

const $sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression = (($env, $expr) => {
  const $3 = ($sd1$Targets$Javascript$EmittableToJs$translateExpression)($env, $expr);
  return ((($3)[0] === "Inline")
    ? ((() => {
      const $e = ($3)[1];
      return $e;
    }))()
    : ((($3)[0] === "Block")
      ? ((() => {
        const $block = ($3)[1];
        return ($sd1$Types$JavascriptAst$Call)(($sd1$Types$JavascriptAst$BlockLambda)($core$Core$Nil, $block), $core$Core$Nil);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 358:4', (sp_toHuman)($3))));
});

const $sd1$Targets$Javascript$EmittableToJs$translateArg = (($stuff, $env, $eaExpression) => {
  return ((($eaExpression)[0] === "ArgumentSpend")
    ? ((() => {
      const $fullType = ($eaExpression)[1];
      const $e = ($eaExpression)[2];
      return ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $e);
    }))()
    : ((($eaExpression)[0] === "ArgumentRecycle")
      ? ((() => {
        const $rawType = ($eaExpression)[1];
        const $attrPath = ($eaExpression)[2];
        const $name = ($eaExpression)[3];
        return ($sd1$Targets$Javascript$EmittableToJs$accessAttrs)($attrPath, ($sd1$Types$JavascriptAst$Var)(($sd1$Targets$Javascript$EmittableToJs$translateName)($name)));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 275:4', (sp_toHuman)($eaExpression))));
});

const $sd1$Targets$Javascript$EmittableToJs$binop = (($jsOp) => {
  return ($sd1$Targets$Javascript$EmittableToJs$Override)(({
    call: (($env, $arguments) => {
      return (((($arguments)[0] === "Cons") && (((($arguments)[2])[0] === "Cons") && (((($arguments)[2])[2])[0] === "Nil")))
        ? ((() => {
          const $right = ($arguments)[1];
          const $left = (($arguments)[2])[1];
          return ($sd1$Types$JavascriptAst$Binop)($jsOp, ($sd1$Targets$Javascript$EmittableToJs$translateArg)(({
            nativeBinop: true,
          }), $env, $right), ($sd1$Targets$Javascript$EmittableToJs$translateArg)(({
            nativeBinop: true,
          }), $env, $left));
        }))()
        : (true
          ? (sp_todo)(("compiler bug: wrong number of arguments for binop" + (sp_toHuman)(({
            arguments: $arguments,
            jsOp: $jsOp,
          }))))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 143:8', (sp_toHuman)($arguments))));
    }),
    value: (($env) => {
      return (sp_todo)(("binop " + ($jsOp + " has no raw value")));
    }),
  }));
});

const $sd1$Targets$Javascript$EmittableToJs$constructor = (($jsValue) => {
  return ($sd1$Targets$Javascript$EmittableToJs$Override)(({
    call: (($env, $args) => {
      return ($sd1$Targets$Javascript$EmittableToJs$makeCall)($env, ($sd1$Types$JavascriptAst$Var)($jsValue), $args);
    }),
    value: (($env) => {
      return ($sd1$Types$JavascriptAst$Var)($jsValue);
    }),
  }));
});

const $sd1$Targets$Javascript$EmittableToJs$function = (($jaName) => {
  return ($sd1$Targets$Javascript$EmittableToJs$Override)(({
    call: (($env, $args) => {
      return ($sd1$Targets$Javascript$EmittableToJs$makeCall)($env, ($sd1$Types$JavascriptAst$Var)($jaName), $args);
    }),
    value: (($env) => {
      return ($sd1$Types$JavascriptAst$Var)($jaName);
    }),
  }));
});

const $sd1$Targets$Javascript$EmittableToJs$introspectOverride = ((() => {
  const $call = (($env, $eaArgs) => {
    return (((($eaArgs)[0] === "Cons") && (((($eaArgs)[1])[0] === "ArgumentSpend") && ((($eaArgs)[2])[0] === "Nil")))
      ? ((() => {
        const $raw = (($eaArgs)[1])[1].raw;
        const $e = (($eaArgs)[1])[2];
        const $expression = ($sd1$Types$JavascriptAst$Literal)((JSON.stringify)($e));
        const $type = ($sd1$Types$JavascriptAst$Literal)((JSON.stringify)($raw));
        const $nonFn = ($sd1$Types$JavascriptAst$Array)($core$Core$Nil);
        const $value = ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $e);
        return ($sd1$Types$JavascriptAst$Record)(($core$Dict$fromList)(($core$Core$Cons)(({
          first: "expression",
          second: $expression,
        }), ($core$Core$Cons)(({
          first: "raw",
          second: $type,
        }), ($core$Core$Cons)(({
          first: "nonFn",
          second: $nonFn,
        }), ($core$Core$Cons)(({
          first: "value",
          second: $value,
        }), $core$Core$Nil))))));
      }))()
      : (true
        ? (sp_todo)("introspectOverride BUG?!")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 184:8', (sp_toHuman)($eaArgs))));
  });
  return ($sd1$Targets$Javascript$EmittableToJs$Override)(({
    call: $call,
    value: (($env) => {
      return (sp_todo)("TODO: monomorphization is not yet implemented so `introspect` can only be called directly");
    }),
  }));
}))();

const $sd1$Targets$Javascript$EmittableToJs$loadOverride = ((() => {
  const $call = (($env, $eaArgs) => {
    const $jaArgs = ($core$List$map)((($2) => {
      return ($sd1$Targets$Javascript$EmittableToJs$translateArg)(({
        nativeBinop: false,
      }), $env, $2);
    }), $eaArgs);
    const $requestedTypeHumanized = (((($eaArgs)[0] === "Cons") && (((($eaArgs)[2])[0] === "Cons") && ((((($eaArgs)[2])[1])[0] === "ArgumentSpend") && (((((($eaArgs)[2])[1])[1].raw)[0] === "TypeFn") && ((((((($eaArgs)[2])[1])[1].raw)[1])[0] === "Cons") && (((((((($eaArgs)[2])[1])[1].raw)[1])[1])[0] === "ParSp") && (((((((($eaArgs)[2])[1])[1].raw)[1])[2])[0] === "Nil") && (((($eaArgs)[2])[2])[0] === "Nil"))))))))
      ? ((() => {
        const $compiledProgram = ($eaArgs)[1];
        const $compiledType = (((((($eaArgs)[2])[1])[1].raw)[1])[1])[1].raw;
        let $hash = (hash_fromList)($core$Core$Nil);
        return ($sd1$Targets$Javascript$EmittableToJs$literalString)((sp_toHuman)(((($1) => {
          return ((__re__ = ($sd1$Types$TypedAst$normalizeType)($hash, $1)), ($hash = (__re__)[1]), (__re__)[0]);
        }))($compiledType)));
      }))()
      : (true
        ? (sp_todo)("loadOverride BUG?!")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 232:12', (sp_toHuman)($eaArgs))));
    return ($sd1$Types$JavascriptAst$Call)(($sd1$Types$JavascriptAst$Var)("self_load"), ($core$Core$Cons)($requestedTypeHumanized, $jaArgs));
  });
  return ($sd1$Targets$Javascript$EmittableToJs$Override)(({
    call: $call,
    value: (($env) => {
      return (sp_todo)("TODO: load as value... I guess we need monomorphization?");
    }),
  }));
}))();

const $sd1$Targets$Javascript$EmittableToJs$unaryMinus = ($sd1$Targets$Javascript$EmittableToJs$Override)(({
  call: (($env, $arguments) => {
    return (((($arguments)[0] === "Cons") && (((($arguments)[1])[0] === "ArgumentSpend") && ((($arguments)[2])[0] === "Nil")))
      ? ((() => {
        const $fullType = (($arguments)[1])[1];
        const $arg = (($arguments)[1])[2];
        return ($sd1$Types$JavascriptAst$Unop)("-", ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $arg));
      }))()
      : (true
        ? (sp_todo)("compiler bug: wrong number of arguments for unop")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 126:8', (sp_toHuman)($arguments))));
  }),
  value: (($env) => {
    return (sp_todo)("unaryMinus has no raw value");
  }),
}));

const $sd1$Targets$Javascript$EmittableToJs$unaryPlus = ($sd1$Targets$Javascript$EmittableToJs$Override)(({
  call: (($env, $arguments) => {
    return (((($arguments)[0] === "Cons") && (((($arguments)[1])[0] === "ArgumentSpend") && ((($arguments)[2])[0] === "Nil")))
      ? ((() => {
        const $fullType = (($arguments)[1])[1];
        const $arg = (($arguments)[1])[2];
        return ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $arg);
      }))()
      : (true
        ? (sp_todo)("compiler bug: wrong number of arguments for unop")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 110:8', (sp_toHuman)($arguments))));
  }),
  value: (($env) => {
    return (sp_todo)("unaryPlus has no raw value");
  }),
}));

const $sd1$Targets$Javascript$EmittableToJs$coreOverrides = ((_0) => {
  const $corelib = (($m, $n) => {
    return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, $m), $n);
  });
  return ($core$Dict$fromList)(($core$Core$Cons)(({
    first: $sd1$Prelude$unaryPlus.usr,
    second: $sd1$Targets$Javascript$EmittableToJs$unaryPlus,
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$unaryMinus.usr,
    second: $sd1$Targets$Javascript$EmittableToJs$unaryMinus,
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$add.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("+"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$multiply.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("*"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$subtract.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("-"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$mutableAssign.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("="),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$mutableAdd.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("+="),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$mutableSubtract.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("-="),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$textConcat.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("+"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$greaterThan.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)(">"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$lesserThan.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("<"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$greaterOrEqualThan.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)(">="),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$lesserOrEqualThan.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("<="),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$or_.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("||"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$and_.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$binop)("&&"),
  }), ($core$Core$Cons)(({
    first: $sd1$Compiler$CoreTypes$true,
    second: ($sd1$Targets$Javascript$EmittableToJs$constructor)("true"),
  }), ($core$Core$Cons)(({
    first: $sd1$Compiler$CoreTypes$false,
    second: ($sd1$Targets$Javascript$EmittableToJs$constructor)("false"),
  }), ($core$Core$Cons)(({
    first: $sd1$Compiler$CoreTypes$noneValue,
    second: ($sd1$Targets$Javascript$EmittableToJs$constructor)("null"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$divide.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_divide"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$listCons.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_cons"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$equal.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_equal"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$notEqual.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_not_equal"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Basics", "modBy"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("basics_modBy"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Basics", "round"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("Math.round"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Basics", "cloneImm"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("basics_cloneImm"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Basics", "cloneUni"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("basics_cloneUni"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$debugLog.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_log"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$debugTodo.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_todo"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$debugToHuman.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_toHuman"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$debugBenchStart.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_benchStart"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$debugBenchStop.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("sp_benchStop"),
  }), ($core$Core$Cons)(({
    first: $sd1$Prelude$compare.usr,
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("basics_compare"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "fromNumber"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_fromNumber"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "toNumber"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_toNumber"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "split"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_split"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "length"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_length"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "slice"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_slice"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "startsWith"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_startsWith"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "startsWithRegex"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_startsWithRegex"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "replaceRegex"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_replaceRegex"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "trimLeft"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_trimLeft"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "dropLeft"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_dropLeft"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Text", "forEach"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("text_forEach"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Hash", "fromList"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("hash_fromList"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Hash", "insert"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("hash_insert"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Hash", "remove"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("hash_remove"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Hash", "get"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("hash_get"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Hash", "for"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("hash_for"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Hash", "each"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("hash_each"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "each"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_each"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "push"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_push"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "pop"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_pop"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "get"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_get"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "set"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_set"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "sortBy"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_sortBy"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "fromList"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_fromList"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Array", "toList"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("array_toList"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("List", "sortBy"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("list_sortBy"),
  }), ($core$Core$Cons)(({
    first: ($corelib)("Self", "load"),
    second: $sd1$Targets$Javascript$EmittableToJs$loadOverride,
  }), ($core$Core$Cons)(({
    first: ($corelib)("Self", "introspect"),
    second: $sd1$Targets$Javascript$EmittableToJs$introspectOverride,
  }), ($core$Core$Cons)(({
    first: ($corelib)("Self", "internalRepresentation"),
    second: ($sd1$Targets$Javascript$EmittableToJs$function)("JSON.stringify"),
  }), $core$Core$Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
});

const $sd1$Targets$Javascript$EmittableToJs$constructorArgumentName = (($i) => {
  return ("$" + (text_fromNumber)($i));
});

const $sd1$Targets$Javascript$EmittableToJs$translateConstructor = (($1) => {
  const $usr = $1.first;
  const $full = $1.second;
  const $taType = $full.raw;
  const $2 = $usr;
  const $slug = ($2)[2];
  const $arrayHead = ($sd1$Targets$Javascript$EmittableToJs$literalString)($slug);
  const $definitionBody = ((($taType)[0] === "TypeFn")
    ? ((() => {
      const $pars = ($taType)[1];
      const $out = ($taType)[2];
      const $argNames = ((($1) => {
        return ($core$List$indexedMap)((($index, $name) => {
          return ($sd1$Targets$Javascript$EmittableToJs$constructorArgumentName)(($index + 1));
        }), $1);
      }))($pars);
      return ((($1) => {
        return ($sd1$Types$JavascriptAst$SimpleLambda)($argNames, $1);
      }))(($sd1$Types$JavascriptAst$Array)((sp_cons)($arrayHead, ($core$List$map)($sd1$Types$JavascriptAst$Var, $argNames))));
    }))()
    : (true
      ? ($sd1$Types$JavascriptAst$Array)(($core$Core$Cons)($arrayHead, $core$Core$Nil))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 639:8', (sp_toHuman)($taType))));
  const $usrAsText = ($sd1$Targets$Javascript$EmittableToJs$translateUsr)($usr);
  return ($sd1$Types$JavascriptAst$Define)(false, $usrAsText, $definitionBody);
});

const $sd1$Targets$Javascript$EmittableToJs$translateDef = (($env, $def) => {
  const $3 = ($core$Dict$get)($def.usr, $env.overrides);
  return ((($3)[0] === "Just")
    ? $core$Maybe$Nothing
    : ((($3)[0] === "Nothing")
      ? ($core$Maybe$Just)(($sd1$Types$JavascriptAst$Define)(false, ($sd1$Targets$Javascript$EmittableToJs$translateUsr)($def.usr), ($sd1$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env, $def.expr)))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 661:4', (sp_toHuman)($3))));
});

const $sd1$Targets$Javascript$EmittableToJs$translateAll = (($pars) => {
  const $2 = $pars;
  const $platformOverrides = $2.platformOverrides;
  const $eaDefs = $2.eaDefs;
  const $constructors = $2.constructors;
  const $jaConstructors = ($core$List$map)((($0) => {
    return ($sd1$Targets$Javascript$EmittableToJs$translateConstructor)($0);
  }), $constructors);
  const $env = ({
    overrides: ($core$List$for)(($sd1$Targets$Javascript$EmittableToJs$coreOverrides)(null), $platformOverrides, (($4, $d) => {
      const $usr = $4.first;
      const $runtimeName = $4.second;
      return ($core$Dict$insert)($usr, ($sd1$Targets$Javascript$EmittableToJs$function)($runtimeName), $d);
    })),
  });
  const $jaStatements = ($core$List$filterMap)((($1) => {
    return ($sd1$Targets$Javascript$EmittableToJs$translateDef)($env, $1);
  }), $eaDefs);
  return ($core$List$concat)(($core$Core$Cons)($jaConstructors, ($core$Core$Cons)($jaStatements, $core$Core$Nil)));
});

const $sd1$Targets$Javascript$JsToText$id = (($level) => {
  return ($core$Text$repeat)($level, "  ");
});

const $sd1$Targets$Javascript$JsToText$emitBlock = (($l, $block) => {
  const $lines = ((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($1) => {
      return ($sd1$Targets$Javascript$JsToText$emitStatement)(($l + 1), $1);
    }), $1);
  }))($block));
  return ("{\n" + ($lines + ("\n" + (($sd1$Targets$Javascript$JsToText$id)($l) + "}"))));
});

const $sd1$Targets$Javascript$JsToText$emitExpr = (($l, $expression) => {
  return ((($expression)[0] === "Literal")
    ? ((() => {
      const $s = ($expression)[1];
      return $s;
    }))()
    : ((($expression)[0] === "Var")
      ? ((() => {
        const $n = ($expression)[1];
        return $n;
      }))()
      : ((($expression)[0] === "Call")
        ? ((() => {
          const $ref = ($expression)[1];
          const $args = ($expression)[2];
          return ("(" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $ref) + (")(" + (($core$Text$join)(", ", ($core$List$map)((($1) => {
            return ($sd1$Targets$Javascript$JsToText$emitExpr)($l, $1);
          }), $args)) + ")"))));
        }))()
        : ((($expression)[0] === "Unop")
          ? ((() => {
            const $op = ($expression)[1];
            const $left = ($expression)[2];
            return ($op + ("(" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $left) + ")")));
          }))()
          : ((($expression)[0] === "Binop")
            ? ((() => {
              const $op = ($expression)[1];
              const $left = ($expression)[2];
              const $right = ($expression)[3];
              return ("(" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $left) + (" " + ($op + (" " + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $right) + ")"))))));
            }))()
            : ((($expression)[0] === "Mutop")
              ? ((() => {
                const $op = ($expression)[1];
                const $yield = ($expression)[2];
                const $left = ($expression)[3];
                const $right = ($expression)[4];
                return ("(" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $left) + (" " + ($op + (" " + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $right) + (", " + ($yield + ")"))))))));
              }))()
              : ((($expression)[0] === "SimpleLambda")
                ? ((() => {
                  const $params = ($expression)[1];
                  const $expr = ($expression)[2];
                  return ("((" + (($core$Text$join)(", ", $params) + (") => " + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $expr) + ")"))));
                }))()
                : ((($expression)[0] === "BlockLambda")
                  ? ((() => {
                    const $params = ($expression)[1];
                    const $stats = ($expression)[2];
                    return ("((" + (($core$Text$join)(", ", $params) + (") => " + (($sd1$Targets$Javascript$JsToText$emitBlock)($l, $stats) + ")"))));
                  }))()
                  : ((($expression)[0] === "Record")
                    ? ((() => {
                      const $attrs = ($expression)[1];
                      return ((sp_equal)($attrs, $core$Dict$empty)
                        ? "{}"
                        : ((($a) => {
                          return ("({\n" + (($core$Text$join)("\n", $a) + ("\n" + (($sd1$Targets$Javascript$JsToText$id)($l) + "})"))));
                        }))(((($1) => {
                          return ($core$List$map)((($4) => {
                            const $key = $4.first;
                            const $value = $4.second;
                            return (($sd1$Targets$Javascript$JsToText$id)(($l + 1)) + ($key + (": " + (($sd1$Targets$Javascript$JsToText$emitExpr)(($l + 1), $value) + ","))));
                          }), $1);
                        }))(((($1) => {
                          return (list_sortBy)($core$Tuple$first, $1);
                        }))(($core$Dict$toList)($attrs)))));
                    }))()
                    : ((($expression)[0] === "AccessWithDot")
                      ? ((() => {
                        const $name = ($expression)[1];
                        const $e = ($expression)[2];
                        return (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $e) + ("." + $name));
                      }))()
                      : ((($expression)[0] === "AccessWithBrackets")
                        ? ((() => {
                          const $i = ($expression)[1];
                          const $expr = ($expression)[2];
                          return ("(" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $expr) + (")[" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $i) + "]"))));
                        }))()
                        : ((($expression)[0] === "Conditional")
                          ? ((() => {
                            const $p = ($expression)[1];
                            const $true = ($expression)[2];
                            const $false = ($expression)[3];
                            return (("(" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $p) + "\n")) + ((($sd1$Targets$Javascript$JsToText$id)(($l + 1)) + ("? " + ($sd1$Targets$Javascript$JsToText$emitExpr)(($l + 1), $true))) + ("\n" + ((($sd1$Targets$Javascript$JsToText$id)(($l + 1)) + (": " + ($sd1$Targets$Javascript$JsToText$emitExpr)(($l + 1), $false))) + ")"))));
                          }))()
                          : ((($expression)[0] === "Array")
                            ? ((() => {
                              const $items = ($expression)[1];
                              return ((sp_equal)($items, $core$Core$Nil)
                                ? "[]"
                                : ((($0) => {
                                  return ((($a) => {
                                    return ("([\n" + (($core$Text$join)("\n", $a) + ("\n" + (($sd1$Targets$Javascript$JsToText$id)($l) + "])"))));
                                  }))($0);
                                }))(((($1) => {
                                  return ($core$List$map)((($i) => {
                                    return (($sd1$Targets$Javascript$JsToText$id)(($l + 1)) + (($sd1$Targets$Javascript$JsToText$emitExpr)(($l + 1), $i) + ","));
                                  }), $1);
                                }))($items)));
                            }))()
                            : ((($expression)[0] === "Comma")
                              ? ((() => {
                                const $expr = ($expression)[1];
                                return ("(" + (($core$Text$join)(", ", ($core$List$map)((($1) => {
                                  return ($sd1$Targets$Javascript$JsToText$emitExpr)($l, $1);
                                }), $expr)) + ")"));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/JsToText.sp 43:4', (sp_toHuman)($expression))))))))))))))));
});

const $sd1$Targets$Javascript$JsToText$emitStatement = (($l, $stat) => {
  const $std = (($mid, $expr) => {
    return (($sd1$Targets$Javascript$JsToText$id)($l) + ($mid + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $expr) + ";")));
  });
  return ((($stat)[0] === "Eval")
    ? ((() => {
      const $e = ($stat)[1];
      return ($std)("", $e);
    }))()
    : ((($stat)[0] === "Return")
      ? ((() => {
        const $e = ($stat)[1];
        return ($std)("return ", $e);
      }))()
      : ((($stat)[0] === "Define")
        ? ((() => {
          const $isReassignable = ($stat)[1];
          const $name = ($stat)[2];
          const $e = ($stat)[3];
          const $modifier = ($isReassignable
            ? "let"
            : "const");
          return ($std)(($modifier + (" " + ($name + " = "))), $e);
        }))()
        : ((($stat)[0] === "If")
          ? ((() => {
            const $condition = ($stat)[1];
            const $block = ($stat)[2];
            return (($sd1$Targets$Javascript$JsToText$id)($l) + ("if (" + (($sd1$Targets$Javascript$JsToText$emitExpr)($l, $condition) + (") " + ($sd1$Targets$Javascript$JsToText$emitBlock)($l, $block)))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/JsToText.sp 15:4', (sp_toHuman)($stat))))));
});

const $sd1$Platforms$Browser$compile = (($out) => {
  (sp_log)("Creating JS AST...", "");
  const $jaStatements = ($sd1$Targets$Javascript$EmittableToJs$translateAll)(({
    constructors: $out.constructors,
    eaDefs: $out.defs,
    platformOverrides: $sd1$Platforms$Browser$overrides,
  }));
  (sp_log)("Emitting JS...", "");
  return ((($1) => {
    return ($core$Text$join)("\n\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($1) => {
      return ($sd1$Targets$Javascript$JsToText$emitStatement)(0, $1);
    }), $1);
  }))($jaStatements));
});

const $sd1$Platforms$Browser$footer = (($mainUsr) => {
  const $mainName = ($sd1$Targets$Javascript$EmittableToJs$translateUsr)($mainUsr);
  const $updateDomNode = ($sd1$Targets$Javascript$EmittableToJs$translateUsr)(($sd1$Platforms$Browser$virtualDomModule)("updateDomNode"));
  return ("\n// TODO these globals will be a hell of trouble if we want to run more than one app\nlet effects = [];\nlet oldVirtualDom = {}; // TODO this should be properly initialized\nlet model = null;\nlet elementId = null;\n\nfunction dispatch(msgResult) {\n    if (msgResult[0] === \"Ok\") {\n\n        const msg = msgResult[1];\n\n        model = " + ($mainName + (".update(effects, msg, model)[0];\n\n            // TODO set a flag and use requestAnimationFrame\n            updateDom();\n        } else {\n            console.log('rejecting msg: ', msgResult[1]);\n        }\n    }\n\n\n    function updateDom() {\n        const e = win.document.getElementById(elementId);\n\n        const newVirtualDom = " + ($mainName + (".view(model);\n\n        " + ($updateDomNode + ("(newVirtualDom, oldVirtualDom, e.childNodes[0]);\n\n        oldVirtualDom = newVirtualDom;\n\n        effects.forEach((e) => e());\n        effects = [];\n    }\n\n\n\n    function main(eid) {\n        elementId = eid;\n        model = " + ($mainName + ".init(effects)[0];\n        updateDom();\n    }\n\n\n\n\n\n    win.Squarepants = {\n        main: main,\n    };\n\n})(this);\n    "))))))));
});

const $sd1$Platforms$Browser$header = "(function (win) {\n";

const $sd1$Platforms$Browser$runtime = "const crawlObject = (path, type, object) => {\n\n    while(path[0] === 'Cons') {\n\n        const head = path[1];\n        const tail = path[2];\n\n        const o = object[head];\n\n        if (o === undefined) {\n            return [ 'Err', 'no field named: ' + head ];\n        }\n\n        object = o;\n        path = path[2];\n    }\n\n    return typeof object === type\n        ? [ 'Ok', object ]\n        : [ 'Err', 'wrong type: ' + typeof object ]\n        ;\n}\n\n\nconst virtualDom_eventToText = (path, event) => crawlObject(path, 'string', event);\nconst virtualDom_eventToFloat = (path, event) => crawlObject(path, 'number', event);\n\n// TODO ensure that those who must return None actually return None (ie, null)\nconst virtualDom_jsCreateTextNode = (content) => document.createTextNode(content);\nconst virtualDom_jsCreateElement = (tag) => document.createElement(tag);\nconst virtualDom_jsReplaceWith = (new_, old) => { old.replaceWith(new_); return new_; }\nconst virtualDom_jsAppendChild = (pars) => pars.parent.appendChild(pars.child);\nconst virtualDom_jsSetAttribute = (name, value, node) => node.setAttribute(name, value);\nconst virtualDom_jsRemoveAttribute = (name, node) => node.removeAttribute(name);\nconst virtualDom_jsSetProperty = (name, value, node) => node[name] = value;\n\n\nconst virtualDom_setChild = (upd, index, parentNode) => {\n    const child = parentNode.childNodes[index];\n    child && upd(child);\n};\n\n\nconst virtualDom_removeAllChildrenStartingFromIndex = (index, parentNode) => {\n    while(parentNode.childNodes[index]) {\n      parentNode.removeChild(parentNode.childNodes[index]);\n    }\n}\n\n\n// an EventHandler is a function that takes an Event and produces a msg\nconst virtualDom_jsAddEventListener = (eventName, handler, node) => {\n\n    node.squarepantsEventHandlers = node.squarepantsEventHandlers || {};\n\n    if (node.squarepantsEventHandlers[eventName]) {\n      node.removeEventListener(eventName, node.squarepantsEventHandlers[eventName]);\n    }\n\n    const onEvent = (event) => dispatch(handler(event));\n    node.squarepantsEventHandlers[eventName] = onEvent;\n    node.addEventListener(eventName, onEvent);\n};\n\nconst virtualDom_jsRemoveEventListener = (eventName, handler, node) => {\n    node.removeEventListener(eventName, node.squarepantsEventHandlers[eventName]);\n    node.squarepantsEventHandlers[eventName] = undefined;\n}\n\n\nconst virtualDom_setViewportOf = (id, top, left) => () => {\n    const e = document.getElementById(id);\n    if (!e) {\n        console.error('could not find element #' + id);\n        return\n    }\n\n    e.scrollTop = top;\n    e.scrollLeft = left;\n}\n\n\nconst virtualDom_drawCanvas = (canvasId, shaderFn) => () => {\n\n    const canvas = document.getElementById(canvasId);\n    if (!canvas) {\n        console.error('could not find canvas', canvasId);\n        return\n    }\n\n    const w = canvas.width;\n    const h = canvas.height;\n\n    const ctx = canvas.getContext('2d');\n    const imageData = ctx.createImageData(w, h);\n\n    for (let x = 0; x < w; x++) for (let y = 0; y < h; y++) {\n\n        const frag = shaderFn(x / (w - 1), 1 - y / (h - 1));\n\n        let j = (x + y * w) * 4;\n        imageData.data[j + 0] = frag.r * 255;\n        imageData.data[j + 1] = frag.g * 255;\n        imageData.data[j + 2] = frag.b * 255;\n        imageData.data[j + 3] = 255;\n    }\n\n    ctx.putImageData(imageData, 0, 0);\n};";

const $sd1$Targets$Javascript$Runtime$listCons = "Cons";

const $sd1$Targets$Javascript$Runtime$listNil = "Nil";

const $sd1$Targets$Javascript$Runtime$nativeDefinitions = ("let __re__;\n\n\nconst sp_clone = (src) => {\n if (Array.isArray(src))\n   return src.map(sp_clone);\n\n if (typeof(src) === 'object') {\n   const dest = {};\n   for (let k in src) { dest[k] = sp_clone(src[k]); }\n   return dest;\n }\n\n return src;\n}\n\n\n/*  HACK\n\n    TODO this is super brittle\n    once we have a proper Platform system in place, the platform can probably\n    use its internal Meta to figure out the proper constructor\n\n*/\nconst maybe_nothing = [ \"Nothing\" ];\nconst maybe_just = (a) => [ \"Just\", a ];\n\n\n//\n// Basic ops\n//\n\n\nconst sp_equal = (a, b) => {\n  if (a === b)\n    return true\n\n  if (Array.isArray(a)) {\n    if (!Array.isArray(b)) return false;\n\n    const l = a.length;\n    if (l !== b.length) return false;\n\n    let i = 0;\n    while (i < l) {\n      if (!sp_equal(a[i], b[i])) return false;\n      ++i;\n    }\n\n    return true;\n  }\n\n  if (typeof(a) === 'object') {\n    if (typeof(b) !== 'object') return false;\n\n    const keys = Object.keys(a);\n    const l = keys.length;\n    if (l !== Object.keys(b).length) return false;\n\n    let i = 0;\n    while (i < l) {\n      let k = keys[i];\n      if (!sp_equal(a[k], b[k])) return false;\n      ++i;\n    }\n\n    return true;\n  }\n\n  return false;\n}\n\n\nconst sp_not_equal = (a, b) => {\n  return !sp_equal(a, b);\n}\n\n\nconst basics_compare = (a, b) => {\n\n  // union type\n  if (Array.isArray(a)) {\n    // compare constructor names\n    if (a[0] > b[0]) return 1;\n    if (b[0] > a[0]) return -1;\n    for (let i = 1; i < a.length; i++) {\n        const cmp = basics_compare(a[i], b[i]);\n        if (cmp) return cmp;\n    }\n    return 0;\n  }\n\n  // None is represented as null\n  if (a === null)\n      return 0;\n\n  if (typeof a === 'object') {\n    const keys = Object.keys(a).sort();\n    for (let k of keys) {\n        const cmp = basics_compare(a[k], b[k]);\n        if (cmp) return cmp;\n    }\n    return 0;\n  }\n\n  if (a > b) return 1;\n  if (a < b) return -1;\n  return 0;\n}\n\nconst sp_divide = (left, right) => {\n  if (right === 0) return 0;\n  return left / right;\n}\n\n\n// TODO remove this and handle it like any other op?\nconst basics_modBy = (a, b) => b % a;\n\n\nconst basics_cloneImm = sp_clone;\n\n\nconst basics_cloneUni = (uni) =>\n    [ sp_clone(uni), uni ];\n\n\n//\n// Debug\n//\n\n\nconst sp_todo = (message) => {\n  throw new Error(\"TODO: \" + message);\n}\n\n\nconst sp_log = (message, thing) => {\n  console.log(message, sp_toHuman(thing));\n  return thing;\n}\n\n\nconst sp_throw = function (errorName) {\n    console.error(...arguments);\n    throw new Error(errorName);\n}\n\n\n//\n// Benchmarking\n//\n\n\nvar debug_benchStartTime = null;\nvar debug_benchStartStack = null;\nvar debug_benchEntries = {};\n\n\nconst pad = (l, s) => ' '.repeat(Math.max(0, l - s.length)) + s;\n\n\nconst fmt = (n) => {\n    const s = Math.floor(n) + '';\n    return s.slice(0, -3) + '.' + pad(3, s.slice(-3));\n}\n\n\n// TODO how should benchmark work in a browser?\ntypeof process !== 'undefined' && process.on('beforeExit', (code) => {\n    if (debug_benchStartStack !== null)\n        console.error(`ERROR: a benchmark has been started but not stopped!\nStart was at:${debug_benchStartStack}`);\n\n    const ks = Object.keys(debug_benchEntries);\n    if (ks.length) {\n        console.info(\"\");\n        console.info(\"Benchmark results:\");\n        ks.sort().forEach(k => {\n            const entry = debug_benchEntries[k];\n            console.info(\n                    'TotalTime:', pad(10, fmt(entry.dt )) + 's',\n                    '   ',\n                    'Runs:', pad(6, '' + entry.n),\n                    '   ',\n                    'Key:', k,\n            );\n        });\n    }\n});\n\n\nconst sp_benchStart = (none) => {\n    if (debug_benchStartStack !== null)\n        throw new Error(`\nbenchStart called when a benchmark is already ongoing!\nPrevious benchStart call was ${debug_benchStartStack}\n`);\n\n    debug_benchStartStack = new Error().stack;\n    debug_benchStartTime = performance.now();\n}\n\n\nconst sp_benchStop = (name) => {\n    const now = performance.now();\n\n    if (debug_benchStartStack === null)\n        throw new Error(\"benchStop called while no benchmark is ongoing!\");\n\n    debug_benchStartStack = null;\n\n    const dt = now - debug_benchStartTime;\n\n    const entry = debug_benchEntries[name] || { dt: 0, n: 0 };\n    entry.dt += dt;\n    entry.n += 1;\n    debug_benchEntries[name] = entry;\n}\n\n\n\n\n//\n// To Human\n//\n\n\nconst id = (n) => '    '.repeat(n);\n\n\nconst sp_toHuman = (a, l = 0) => {\n\n  if (Array.isArray(a))\n    return sp_toHumanAsList([], a, l) || sp_toHumanAsUnion(a, l);\n\n  if (typeof a === 'function') {\n    return '<fn ' + a.length + '>';\n  }\n\n  if (typeof a === 'object') {\n    let acc = '{\\n';\n    for (let key in a)\n        acc += id(l + 1) + key + ' = ' + sp_toHuman(a[key], l + 1) + '\\n';\n\n    return acc + id(l) + '}';\n  }\n\n  return JSON.stringify(a, null, 0);\n}\n\n\nconst sp_toHumanAsUnion = (a, l) => {\n\n  if (a.length === 1) {\n      return a[0];\n  }\n\n  let acc = a[0] + '\\n';\n\n  a.slice(1).forEach(arg => {\n\n      const sub = sp_toHuman(arg, l + 1);\n      if (!sub.startsWith('{') && sub.indexOf('\\n') > -1)\n          acc += id(l + 1) + '(' + sub + id(l + 1) + ')\\n';\n      else\n          acc += id(l + 1) + sub + '\\n';\n\n  })\n\n  return acc;\n}\n\n\nconst sp_toHumanAsList = (arrayAccum, list, l) => {\n  if (list[0] === '" + ($sd1$Targets$Javascript$Runtime$listCons + ("' && list.length === 3) {\n    arrayAccum.push(sp_toHuman(list[1], l));\n    return sp_toHumanAsList(arrayAccum, list[2], l);\n  }\n\n  if (list[0] === '" + ($sd1$Targets$Javascript$Runtime$listNil + ("')\n    return '[' + arrayAccum.join(', ') + ']';\n\n  return false;\n}\n\n\n//\n// Text\n//\n\n\nconst text_fromNumber = (n) => '' + n;\n\nconst text_toNumber = (t) => {\n    const n = +t;\n\n    return isNaN(n) ? maybe_nothing : maybe_just(n);\n}\n\nconst text_split = (separator, target) => arrayToListLow(target.split(separator));\n\nconst text_length = (s) => s.length;\n\nconst text_slice = (start, end, s) => s.slice(start, end);\n\nconst text_startsWith = (sub, s) => s.startsWith(sub);\n\nconst text_startsWithRegex = (regex) => {\n  let re;\n  try {\n    re = new RegExp('^' + regex);\n  } catch (e) {\n    return () => \"\"\n  }\n\n  return (s) => {\n    let m = s.match(re);\n    return m ? m[0] : \"\";\n  }\n}\n\nconst text_replaceRegex = (regex) => {\n  let re;\n  try {\n    re = new RegExp(regex, 'g');\n  } catch (e) {\n    return () => \"\"\n  }\n\n  return (replacer, s) => s.replace(re, replacer);\n}\n\nconst text_trimLeft = (s) => s.trimLeft();\n\nconst text_dropLeft = (n, s) => s.slice(n);\n\nconst text_forEach = (s, f) => {\n  for (let i of s) f(i);\n  return null;\n}\n\n\n//\n// Hashes\n//\n\nconst hash_fromList = (list) => {\n  const hash = {};\n\n  // TODO iteration instead of recursion\n  const rec = (ls) => {\n    if (ls[0] === '" + ($sd1$Targets$Javascript$Runtime$listNil + ("')\n      return hash;\n\n    const { first, second } = ls[1];\n\n    hash[JSON.stringify(first)] = [first, second];\n\n    return rec(ls[2]);\n  };\n\n  return rec(list);\n}\n\n\nconst hash_insert = (hash, key, value) => {\n    hash[JSON.stringify(key)] = [key, value];\n    return [null, hash];\n}\n\n\nconst hash_remove = (hash, key) => {\n    delete hash[JSON.stringify(key)];\n    return [null, hash];\n}\n\n\nconst hash_get = (hash, key) => {\n    const r = hash[JSON.stringify(key)];\n    return [r === undefined ? maybe_nothing : maybe_just(r[1]), hash];\n}\n\n\nconst hash_for = (hash, f, acc) => {\n    for (let k in hash) {\n        const kv = hash[k];\n        acc = f(kv[0], kv[1], acc);\n    }\n    return [acc, hash];\n}\n\n\nconst hash_each = (hash, f) => {\n    for (let k in hash) {\n        const kv = hash[k];\n        f(kv[0], kv[1]);\n    }\n    return [null, hash];\n}\n\n\n//\n// Arrays\n//\n\nconst array_each = (array, f) => {\n    array.forEach(f);\n    return [null, array];\n}\n\nconst array_push = (array, item) => {\n    array.push(item);\n    return [null, array];\n}\n\nconst array_pop = (a) => {\n    return [a.length ? maybe_just(a.pop()) : maybe_nothing, a];\n}\n\nconst array_get = (array, index) => {\n    const r = array[index];\n    return [r === undefined ? maybe_nothing : maybe_just(r), array];\n}\n\nconst array_set = (a, index, item) => {\n    if (index < 0) return false;\n    if (index >= a.length) return [false, a];\n    a[index] = item;\n    return [true, a];\n}\n\nconst array_sortBy = (arr, f) => {\n    arr.sort((a, b) => basics_compare(f(a), f(b)));\n    return [null, arr];\n}\n\nconst arrayToListLow = (arr) => {\n  const length = arr.length;\n  let list = [ '" + ($sd1$Targets$Javascript$Runtime$listNil + ("' ];\n  for (let i = length - 1; i >= 0; i--) {\n      list = [ '" + ($sd1$Targets$Javascript$Runtime$listCons + ("', arr[i], list ];\n  }\n  return list;\n}\n\nconst array_toList = (arr) => [arrayToListLow(arr), arr];\n\n\nconst arrayFromListLow = (list) => {\n  const array = [];\n  const rec = (ls) => {\n    if (ls[0] === '" + ($sd1$Targets$Javascript$Runtime$listNil + ("')\n      return array;\n\n    array.push(ls[1]);\n    return rec(ls[2]);\n  };\n\n  return rec(list);\n}\n\nconst array_fromList = arrayFromListLow;\n\n\n//\n// Lists\n//\n\n\nconst sp_cons = (item, list) => {\n  return [ '" + ($sd1$Targets$Javascript$Runtime$listCons + "', item, list];\n}\n\nconst list_sortBy = (f, list) => arrayToListLow(arrayFromListLow(list).sort((a, b) => basics_compare(f(a), f(b))));\n\n\n//\n// Dynamic loading\n//\nconst self_load = (requestedTypeHumanized, pars, variantConstructor) => {\n\n    const actualTypeHumanized = sp_toHuman(pars.type);\n    if (actualTypeHumanized !== requestedTypeHumanized) {\n        return [ 'Err', pars.type ];\n    }\n\n    // TODO using directly the source name sd1 is super fragile: must revisit this as soon as I have `Load.expose`\n    // TODO hoping that the state won't be mutated, once we have `Load.expose` maybe we don't need to lug the state around any more?\n    const translateUsr = $sd1$Targets$Javascript$EmittableToJs$translateUsr;\n    const js = $sd1$Platforms$Browser$compile(pars);\n\n    //   { name1, name2, name3, ... } = externals;\n    const unpackExterns = 'const { ' + pars.externalValues.map((e) => translateUsr(e.usr)).join(', ') + ' } = externs;';\n\n    const body = `{ ${unpackExterns}\n${js}; return ${translateUsr(pars.entryUsr)}; }`;\n\n    const arg = {};\n    pars.externalValues.forEach((e) => arg[translateUsr(e.usr)] = e.self.value);\n\n    return [ 'Ok', variantConstructor(Function('externs', body)(arg)) ];\n};\n    "))))))))))))));

const $sd1$Platforms$Browser$makeExecutable = (($out) => {
  const $compiledStatements = ($sd1$Platforms$Browser$compile)($out);
  return ($sd1$Platforms$Browser$header + ($sd1$Targets$Javascript$Runtime$nativeDefinitions + ($sd1$Platforms$Browser$runtime + ($compiledStatements + ($sd1$Platforms$Browser$footer)($out.entryUsr)))));
});

const $sd1$Platforms$Browser$modules = "\nlibrary =\n    source = \"core:browser\"\n\n    module =\n        path = Browser\n\n    module =\n        path = Html\n\n    module =\n        path = VirtualDom";

const $sd1$Platforms$Browser$platform = ({
  defaultModules: ($sd1$DefaultModules$asText + $sd1$Platforms$Browser$modules),
  defaultOutputPath: "index.js",
  makeExecutable: $sd1$Platforms$Browser$makeExecutable,
  name: "browser",
  quickstart: "TODO",
});

const $sd1$Platforms$Posix$header = "#!/usr/bin/env -S node --stack-size=65500 --max-old-space-size=4096\n\n//Error.stackTraceLimit = 100;\n\nconst { performance } = require('perf_hooks');\n\n";

const $sd1$Platforms$Posix$overrides = ((() => {
  const $ioModule = (($1) => {
    return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Posix, "IO"), $1);
  });
  const $pathModule = (($1) => {
    return ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Posix, "Path"), $1);
  });
  return ($core$Core$Cons)(({
    first: ($ioModule)("parallel"),
    second: "io_parallel",
  }), ($core$Core$Cons)(({
    first: ($ioModule)("readDir"),
    second: "io_readDir",
  }), ($core$Core$Cons)(({
    first: ($ioModule)("readFile"),
    second: "io_readFile",
  }), ($core$Core$Cons)(({
    first: ($ioModule)("writeFile"),
    second: "io_writeFile",
  }), ($core$Core$Cons)(({
    first: ($ioModule)("writeStdout"),
    second: "io_writeStdout",
  }), ($core$Core$Cons)(({
    first: ($ioModule)("writeStderr"),
    second: "io_writeStderr",
  }), ($core$Core$Cons)(({
    first: ($pathModule)("dirname"),
    second: "path_dirname",
  }), ($core$Core$Cons)(({
    first: ($pathModule)("resolve"),
    second: "path_resolve",
  }), $core$Core$Nil))))))));
}))();

const $sd1$Platforms$Posix$runtime = "\n//\n// Platform: IO\n//\nconst fs = require('fs');\nconst path = require('path');\n\nconst io_wrap = (f) => [ \"IO.IO\", f ];\n\nconst io_parallel = (iosAsList) => io_wrap((never) => {\n    // as [IO a]: IO [a]\n\n    const ios = arrayFromListLow(iosAsList);\n\n    // TODO actually run them in parallel!\n\n    let arr = [];\n    for (let io of ios) {\n        const r = io[1](never);\n        if (r[0] === \"Ok\")\n            arr.push(r[1]);\n        else\n            return $core$Result$Err(r[1]);\n    }\n\n    return $core$Result$Ok(arrayToListLow(arr));\n});\n\n\nconst io_readDir = (dirPath) => io_wrap((never) => {\n    // as Text: IO [Bool & Text]\n\n    var entries;\n    try {\n        entries = fs.readdirSync(dirPath, { withFileTypes: true });\n    } catch (e) {\n        return $core$Result$Err(e.message);\n    }\n\n    return $core$Result$Ok(arrayToListLow(entries.map((dirent) => ({\n        first: dirent.isDirectory(),\n        second: dirent.name,\n    }))));\n});\n\n\nconst io_readFile = (path) => io_wrap((never) => {\n    // as Text: IO Text\n\n    var content;\n    try {\n        content = fs.readFileSync(path, 'utf8');\n    } catch (e) {\n        return $core$Result$Err(e.message);\n    }\n\n    return $core$Result$Ok(content);\n});\n\n\nconst io_writeFile = (path, content) => io_wrap((never) => {\n    // as Text: Text: IO Int\n\n    try {\n        fs.writeFileSync(path, content);\n    } catch (e) {\n        return $core$Result$Err(e.message);\n    }\n\n    return $core$Result$Ok(0);\n});\n\n\nconst io_writeStdout = (content) => io_wrap((never) => {\n    // as Text: IO Int\n\n    console.info(content);\n    return $core$Result$Ok(0);\n});\n\n\nconst io_writeStderr = (content) => io_wrap((never) => {\n    // as Text: IO Int\n\n    console.error(content);\n    return $core$Result$Ok(-1);\n});\n\n\nconst path_resolve = (p) => path.resolve(...arrayFromListLow(p));\n\n\nconst path_dirname = path.dirname;\n";

const $sd1$Platforms$Posix$makeExecutable = (($out) => {
  const $entryName = ($sd1$Targets$Javascript$EmittableToJs$translateUsr)($out.entryUsr);
  const $callMain = ("const args = arrayToListLow(process.argv.slice(1));\nconst out = " + ($entryName + "({}, args)[1]('never');\n        if (out[0] === 'Ok') {\n            process.exitCode = out[1];\n        } else {\n            console.error(out[1]);\n            process.exitCode = 1;\n        }\n        "));
  const $compiledStatements = ((() => {
    (sp_log)("Creating JS AST...", "");
    const $jaStatements = ($sd1$Targets$Javascript$EmittableToJs$translateAll)(({
      constructors: $out.constructors,
      eaDefs: $out.defs,
      platformOverrides: $sd1$Platforms$Posix$overrides,
    }));
    (sp_log)("Emitting JS...", "");
    return ((($1) => {
      return ($core$Text$join)("\n\n", $1);
    }))(((($1) => {
      return ($core$List$map)((($1) => {
        return ($sd1$Targets$Javascript$JsToText$emitStatement)(0, $1);
      }), $1);
    }))($jaStatements));
  }))();
  return ($sd1$Platforms$Posix$header + ($sd1$Targets$Javascript$Runtime$nativeDefinitions + ($sd1$Platforms$Posix$runtime + ($compiledStatements + $callMain))));
});

const $sd1$Platforms$Posix$posixModules = "\nlibrary =\n    source = \"core:posix\"\n\n    module =\n        path = IO\n        globalTypes = IO\n\n    module =\n        path = Path";

const $sd1$Platforms$Posix$platform = ({
  defaultModules: ($sd1$DefaultModules$asText + $sd1$Platforms$Posix$posixModules),
  defaultOutputPath: "nodeExecutable.js",
  makeExecutable: $sd1$Platforms$Posix$makeExecutable,
  name: "posix",
  quickstart: "TODO",
});

const $sd1$Main$availablePlatforms = ($core$Core$Cons)($sd1$Platforms$Posix$platform, ($core$Core$Cons)($sd1$Platforms$Browser$platform, $core$Core$Nil));

const $sd1$Main$cliDefaults = ({
  platform: $sd1$Platforms$Posix$platform,
});

const $sd1$Main$parsePlatformName = (($maybeValue, $cliState) => {
  return ((($maybeValue)[0] === "Nothing")
    ? ($core$Result$Err)("Please specify a platform name, for example: `--platform=posix`")
    : ((($maybeValue)[0] === "Just")
      ? ((() => {
        const $value = ($maybeValue)[1];
        const $3 = ($core$List$find)((($p) => {
          return (sp_equal)($p.name, $value);
        }), $sd1$Main$availablePlatforms);
        return ((($3)[0] === "Nothing")
          ? ($core$Result$Err)(("I don't know this platform name: `" + ($value + ("`\n\n                    Valid platform names are:\n\n                    " + ((($1) => {
            return ($core$Text$join)("\n", $1);
          }))(($core$List$map)((($p) => {
            return ("    " + $p.name);
          }), $sd1$Main$availablePlatforms))))))
          : ((($3)[0] === "Just")
            ? ((() => {
              const $platform = ($3)[1];
              return ($core$Result$Ok)(((() => {
                const $0 = $cliState;
                return (Object.assign)({}, $0, ({
                  platform: $platform,
                }));
              }))());
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 159:12', (sp_toHuman)($3))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 154:4', (sp_toHuman)($maybeValue))));
});

const $sd1$Main$cliOptions = ($core$Core$Cons)(({
  info: "select build platform",
  name: "--platform",
  parser: $sd1$Main$parsePlatformName,
}), $core$Core$Nil);

const $sd1$Main$indent = (($s) => {
  return ((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($l) => {
      return ("  " + $l);
    }), $1);
  }))(((($1) => {
    return (text_split)("\n", $1);
  }))($s)));
});

const $sd1$Main$parseArguments = (($options, $args, $initState) => {
  const $4 = ($core$List$partition)((($1) => {
    return (text_startsWith)("--", $1);
  }), $args);
  const $others = $4.second;
  const $optionTexts = $4.first;
  const $findOption = (($optionText, $state) => {
    const $7 = (text_split)("=", $optionText);
    return ((($7)[0] === "Nil")
      ? ($core$Result$Ok)($state)
      : ((($7)[0] === "Cons")
        ? ((() => {
          const $optionName = ($7)[1];
          const $rest = ($7)[2];
          const $8 = ($core$List$find)((($o) => {
            return (sp_equal)($o.name, $optionName);
          }), $options);
          return ((($8)[0] === "Nothing")
            ? ($core$Result$Err)(("Unknown option " + $optionName))
            : ((($8)[0] === "Just")
              ? ((() => {
                const $option = ($8)[1];
                const $value = ((sp_equal)($rest, $core$Core$Nil)
                  ? $core$Maybe$Nothing
                  : ($core$Maybe$Just)(($core$Text$join)("=", $rest)));
                return ($option.parser)($value, $state);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 45:16', (sp_toHuman)($8))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 40:8', (sp_toHuman)($7))));
  });
  return ((($1) => {
    return ($core$Result$map)((($1) => {
      return ($core$Tuple$pair)($others, $1);
    }), $1);
  }))(((($0) => {
    return ($core$List$forRes)($0, $optionTexts, $findOption);
  }))($initState));
});

const $sd1$Main$order = (($outcome) => {
  return ((($outcome)[0] === "Success")
    ? 0
    : ((($outcome)[0] === "Skipped")
      ? 1
      : ((($outcome)[0] === "Error")
        ? 2
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 88:4', (sp_toHuman)($outcome)))));
});

const $sd1$Term$green = ($sd1$Term$color)("\x1b[32m");

const $sd1$Main$testOutcomeToText = (($name, $code, $outcome) => {
  return ((($outcome)[0] === "Success")
    ? ($sd1$Term$green)(("* PASS: " + $name))
    : ((($outcome)[0] === "Skipped")
      ? ($sd1$Term$yellow)(("* skip: " + $name))
      : ((($outcome)[0] === "Error")
        ? ((() => {
          const $error = ($outcome)[1];
          return (($sd1$Term$red)(("FAIL ! " + $name)) + ("\n" + (($sd1$Main$indent)($code) + ("\n" + ($sd1$Main$indent)($error)))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 75:4', (sp_toHuman)($outcome)))));
});

const $sd1$Test$getName = (($test) => {
  return ((($test)[0] === "Single")
    ? ((() => {
      const $n = ($test)[1];
      const $code = ($test)[2];
      const $f = ($test)[3];
      return $n;
    }))()
    : ((($test)[0] === "Group")
      ? ((() => {
        const $n = ($test)[1];
        const $ls = ($test)[2];
        return $n;
      }))()
      : ((($test)[0] === "NotNow")
        ? ((() => {
          const $t = ($test)[1];
          return ($sd1$Test$getName)($t);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 149:4', (sp_toHuman)($test)))));
});

const $sd1$Test$outcomesRec = (($path, $test, $accum) => {
  return ((($test)[0] === "Single")
    ? ((() => {
      const $name = ($test)[1];
      const $code = ($test)[2];
      const $f = ($test)[3];
      return (sp_cons)(({
        code: $code,
        getOutcome: $f,
        name: ($path + $name),
      }), $accum);
    }))()
    : ((($test)[0] === "NotNow")
      ? ((() => {
        const $t = ($test)[1];
        const $thing = ({
          code: "",
          getOutcome: ((_0) => {
            return $sd1$Test$Skipped;
          }),
          name: ($path + ($sd1$Test$getName)($t)),
        });
        return (sp_cons)($thing, $accum);
      }))()
      : ((($test)[0] === "Group")
        ? ((() => {
          const $pathSegment = ($test)[1];
          const $ts = ($test)[2];
          return ($core$List$for)($accum, $ts, (($1, $2) => {
            return ($sd1$Test$outcomesRec)(($path + ($pathSegment + " / ")), $1, $2);
          }));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 134:4', (sp_toHuman)($test)))));
});

const $sd1$Test$flattenAndRun = (($tests) => {
  const $flattened = ((($1) => {
    return ($core$List$map)((($r) => {
      return (($core$Text$contains)("SKIP", $r.name)
        ? ((() => {
          const $0 = $r;
          return (Object.assign)({}, $0, ({
            getOutcome: ((_0) => {
              return $sd1$Test$Skipped;
            }),
          }));
        }))()
        : $r);
    }), $1);
  }))(($sd1$Test$outcomesRec)("", ($sd1$Test$Group)("", $tests), $core$Core$Nil));
  const $onlies = ((($1) => {
    return ($core$List$filter)((($r) => {
      return ($core$Text$contains)("ONLY", $r.name);
    }), $1);
  }))($flattened);
  const $runnable = ((sp_not_equal)($onlies, $core$Core$Nil)
    ? $onlies
    : $flattened);
  const $runTest = (($r) => {
    const $3 = $r;
    const $name = $3.name;
    const $getOutcome = $3.getOutcome;
    const $code = $3.code;
    return ({
      code: $code,
      name: $name,
      outcome: ($getOutcome)(null),
    });
  });
  return ($core$List$map)($runTest, $runnable);
});

const $sd1$Main$selftestMain = ((_0) => {
  return ($posix$IO$writeStdout)(((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($x) => {
      return ($sd1$Main$testOutcomeToText)($x.name, $x.code, $x.outcome);
    }), $1);
  }))(((($1) => {
    return (list_sortBy)((($x) => {
      return ({
        first: ($sd1$Main$order)($x.outcome),
        second: $x.name,
      });
    }), $1);
  }))(($sd1$Test$flattenAndRun)($sd1$Main$allTests)))));
});

const $sd1$Main$main = (($env, $args) => {
  const $3 = ($sd1$Main$parseArguments)($sd1$Main$cliOptions, $args, $sd1$Main$cliDefaults);
  return ((($3)[0] === "Err")
    ? ((() => {
      const $message = ($3)[1];
      return ($posix$IO$fail)($message);
    }))()
    : ((($3)[0] === "Ok")
      ? ((() => {
        const $args = ($3)[1].first;
        const $cliState = ($3)[1].second;
        return (((($args)[0] === "Cons") && (((($args)[2])[0] === "Cons") && ("selftest" === (($args)[2])[1])))
          ? ((() => {
            const $self = ($args)[1];
            const $tail = (($args)[2])[2];
            return ($sd1$Main$selftestMain)(null);
          }))()
          : (((($args)[0] === "Cons") && ((($args)[2])[0] === "Cons"))
            ? ((() => {
              const $self = ($args)[1];
              const $head = (($args)[2])[1];
              const $tail = (($args)[2])[2];
              const $mainModulePath = $head;
              const $maybeOutputPath = ($core$List$head)($tail);
              return ($sd1$Compile$compileMain)(({
                entryModulePath: $mainModulePath,
                env: $env,
                maybeOutputPath: $maybeOutputPath,
                platform: $cliState.platform,
                selfPath: $self,
              }));
            }))()
            : (true
              ? ($posix$IO$writeStdout)("\nHi! This is the Squarepants compiler!\n\nTo compile something, write:\n\n    squarepants pathToMainModule.sp\n")
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 219:12', (sp_toHuman)($args)))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 214:4', (sp_toHuman)($3))));
});

const $sd1$Main$parseCli = (($args) => {
  return (((($args)[0] === "Cons") && (((($args)[2])[0] === "Cons") && ("selftest" === (($args)[2])[1])))
    ? ((() => {
      const $self = ($args)[1];
      const $tail = (($args)[2])[2];
      return $sd1$Main$Selftest;
    }))()
    : (((($args)[0] === "Cons") && ((($args)[2])[0] === "Cons"))
      ? ((() => {
        const $self = ($args)[1];
        const $head = (($args)[2])[1];
        const $tail = (($args)[2])[2];
        return ($sd1$Main$Compile)(({
          mainModulePath: $head,
          maybeOutputPath: ($core$List$head)($tail),
          self: $self,
        }));
      }))()
      : (true
        ? $sd1$Main$Help
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 190:4', (sp_toHuman)($args)))));
});

const $sd1$SPLib$Parser$higherOr = (($parser) => {
  return (($higher) => {
    return ($sd1$SPLib$Parser$oneOf)(($core$Core$Cons)($higher, ($core$Core$Cons)($parser, $core$Core$Nil)));
  });
});

const $sd1$SPLib$Parser$map = (($f, $p) => {
  return (($sd1$SPLib$Parser$andThen)((($b) => {
    return ($sd1$SPLib$Parser$accept)(($f)($b));
  })))($p);
});

const $sd1$SPLib$Parser$tuple2 = (($pa, $pb) => {
  return (($sd1$SPLib$Parser$andThen)((($a) => {
    return (($sd1$SPLib$Parser$andThen)((($b) => {
      return ($sd1$SPLib$Parser$accept)(({
        first: $a,
        second: $b,
      }));
    })))($pb);
  })))($pa);
});

const $sd1$SPLib$Parser$oneOrMore = (($p) => {
  return ($sd1$SPLib$Parser$tuple2)($p, ($sd1$SPLib$Parser$zeroOrMore)($p));
});

const $sd1$SPLib$Parser$tuple3 = (($pa, $pb, $pc) => {
  return (($sd1$SPLib$Parser$andThen)((($a) => {
    return (($sd1$SPLib$Parser$andThen)((($b) => {
      return (($sd1$SPLib$Parser$andThen)((($c) => {
        return ($sd1$SPLib$Parser$accept)(({
          first: $a,
          second: $b,
          third: $c,
        }));
      })))($pc);
    })))($pb);
  })))($pa);
});

const $sd1$SPON$logHead = (($statements) => {
  ((($statements)[0] === "Cons")
    ? ((() => {
      const $head = ($statements)[1];
      const $tail = ($statements)[2];
      (sp_log)("LOG", $head);
      return null;
    }))()
    : ((($statements)[0] === "Nil")
      ? (sp_log)("LOG", null)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 79:4', (sp_toHuman)($statements))));
  return ($sd1$SPON$Accepted)($statements, null);
});

const $sd1$TempMain$allTests = ($core$Core$Cons)($sd1$Compiler$TypeCheck_Test$tests, $core$Core$Nil);

const $sd1$TempMain$indent = (($s) => {
  return ((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($l) => {
      return ("  " + $l);
    }), $1);
  }))(((($1) => {
    return (text_split)("\n", $1);
  }))($s)));
});

const $sd1$TempMain$order = (($outcome) => {
  return ((($outcome)[0] === "Success")
    ? 0
    : ((($outcome)[0] === "Skipped")
      ? 1
      : ((($outcome)[0] === "Error")
        ? 2
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/TempMain.sp 43:4', (sp_toHuman)($outcome)))));
});

const $sd1$TempMain$testOutcomeToText = (($name, $code, $outcome) => {
  return ((($outcome)[0] === "Success")
    ? ($sd1$Term$green)(("* PASS: " + $name))
    : ((($outcome)[0] === "Skipped")
      ? ($sd1$Term$yellow)(("* skip: " + $name))
      : ((($outcome)[0] === "Error")
        ? ((() => {
          const $error = ($outcome)[1];
          return (($sd1$Term$red)(("FAIL ! " + $name)) + ("\n" + (($sd1$TempMain$indent)($code) + ("\n" + ($sd1$TempMain$indent)($error)))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/TempMain.sp 30:4', (sp_toHuman)($outcome)))));
});

const $sd1$TempMain$selftestMain = ((_0) => {
  return ($posix$IO$writeStdout)(((($1) => {
    return ($core$Text$join)("\n", $1);
  }))(((($1) => {
    return ($core$List$map)((($x) => {
      return ($sd1$TempMain$testOutcomeToText)($x.name, $x.code, $x.outcome);
    }), $1);
  }))(((($1) => {
    return (list_sortBy)((($x) => {
      return ({
        first: ($sd1$TempMain$order)($x.outcome),
        second: $x.name,
      });
    }), $1);
  }))(((($0) => {
    return ($sd1$Test$flattenAndRun)($0);
  }))($sd1$TempMain$allTests)))));
});

const $sd1$TempMain$main = (($env, $args) => {
  return ($sd1$TempMain$selftestMain)(null);
});

const $sd1$Test$errorsFirst = (($outcome) => {
  return ((($outcome)[0] === "Error")
    ? ((() => {
      const $e = ($outcome)[1];
      return -(1);
    }))()
    : ((($outcome)[0] === "Skipped")
      ? 0
      : ((($outcome)[0] === "Success")
        ? 1
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 186:4', (sp_toHuman)($outcome)))));
});

const $sd1$Types$Pos$drop = (($x) => {
  const $2 = $x;
  const $a = ($2)[2];
  const $pos = ($2)[1];
  return $a;
});

const $sd1$Types$Pos$end = (($pos) => {
  return ((($pos)[0] === "P")
    ? ((() => {
      const $s = ($pos)[1];
      const $e = ($pos)[2];
      return $e;
    }))()
    : (true
      ? 0
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/Pos.sp 38:4', (sp_toHuman)($pos))));
});

const $sd1$Types$Pos$start = (($pos) => {
  return ((($pos)[0] === "P")
    ? ((() => {
      const $s = ($pos)[1];
      const $e = ($pos)[2];
      return $s;
    }))()
    : (true
      ? 0
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/Pos.sp 31:4', (sp_toHuman)($pos))));
});

const $sd1$Types$TypedAst$initModule = (($fsPath, $asText, $umr) => {
  return ({
    asText: $asText,
    fsPath: $fsPath,
    umr: $umr,
    valueDefs: $core$Dict$empty,
  });
});

const $sd2$Uniqueness$valueTest = (($1, $2, $3) => {
  return ($sd1$Test$valueTest)(sp_toHuman, $1, $2, $3);
});

const $sd3$App$canvasQuickExample = "alias N = Number\n\nmain as fn N, N: { r as N, g as N, b as N } =\n    fn x, y:\n\n    {\n    , r = if x < 50% then 10% else 60%\n    , g = 50%\n    , b = y\n    }";

const $sd3$App$floatToPercent = (($n) => {
  return ((text_fromNumber)((Math.round)(($n * 100))) + "%");
});

const $sd3$App$htmlQuickExample = "main =\n   Html.div\n      [ Html.class \"col\" ]\n      [\n      , Html.button\n           [ Html.onClick \"Event:Apple\" ]\n           [ Html.text \"Apple\" ]\n\n      , Html.button\n           [ Html.onClick \"Pear click!\" ]\n           [ Html.text \"Pear\" ]\n      ]\n";

const $sd3$App$maybeUpdateCanvas = (($effects, $model) => {
  const $meh = ((() => {
    const $3 = $model.compiledCode;
    return ((($3)[0] === "CompiledShader")
      ? ((() => {
        const $shaderFn = ($3)[1];
        return ((__re__ = (array_push)($effects, (virtualDom_drawCanvas)("output", $shaderFn))), ($effects = (__re__)[1]), (__re__)[0]);
      }))()
      : (true
        ? null
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 133:8', (sp_toHuman)($3))));
  }))();
  return ([
    $model,
    $effects,
  ]);
});

const $sd3$CompileText$selfToExposed = (($self) => {
  const $2 = $self.expression;
  return (((($2)[0] === "Variable") && ((($2)[1])[0] === "RefGlobal"))
    ? ((() => {
      const $usr = (($2)[1])[1];
      return ({
        first: $usr,
        second: $self,
      });
    }))()
    : (true
      ? (sp_todo)(("can't create an USR for " + (sp_toHuman)($self.expression)))
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 71:4', (sp_toHuman)($2))));
});

const $sd3$CompileText$exposedValues = ((() => {
  const $l1 = ((($1) => {
    return ($core$List$map)($sd3$CompileText$selfToExposed, $1);
  }))(($core$Core$Cons)(({
    expression: ["Variable",["RefGlobal",["USR",["UMR",["Browser"],"Html"],"div"]]],
    nonFn: [],
    raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"List"],["Cons",["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"Attr"],["Cons",["TypeVar",2292],["Nil"]]],["Nil"]]],"uni":["Imm"]}],["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"List"],["Cons",["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"VirtualNode"],["Cons",["TypeVar",2292],["Nil"]]],["Nil"]]],"uni":["Imm"]}],["Nil"]]],{"raw":["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"VirtualNode"],["Cons",["TypeVar",2292],["Nil"]]],"uni":["Imm"]}],
    value: $browser$Html$div,
  }), ($core$Core$Cons)(({
    expression: ["Variable",["RefGlobal",["USR",["UMR",["Browser"],"Html"],"button"]]],
    nonFn: [],
    raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"List"],["Cons",["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"Attr"],["Cons",["TypeVar",2290],["Nil"]]],["Nil"]]],"uni":["Imm"]}],["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"List"],["Cons",["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"VirtualNode"],["Cons",["TypeVar",2290],["Nil"]]],["Nil"]]],"uni":["Imm"]}],["Nil"]]],{"raw":["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"VirtualNode"],["Cons",["TypeVar",2290],["Nil"]]],"uni":["Imm"]}],
    value: $browser$Html$button,
  }), ($core$Core$Cons)(({
    expression: ["Variable",["RefGlobal",["USR",["UMR",["Browser"],"Html"],"onClick"]]],
    nonFn: [],
    raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeVar",2288],"uni":["Imm"]}],["Nil"]],{"raw":["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"Attr"],["Cons",["TypeVar",2288],["Nil"]]],"uni":["Imm"]}],
    value: $browser$Html$onClick,
  }), ($core$Core$Cons)(({
    expression: ["Variable",["RefGlobal",["USR",["UMR",["Browser"],"Html"],"text"]]],
    nonFn: [],
    raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"Text"],["Nil"]],"uni":["Imm"]}],["Nil"]],{"raw":["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"VirtualNode"],["Cons",["TypeVar",2286],["Nil"]]],"uni":["Imm"]}],
    value: $browser$Html$text,
  }), ($core$Core$Cons)(({
    expression: ["Variable",["RefGlobal",["USR",["UMR",["Browser"],"Html"],"style"]]],
    nonFn: [],
    raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"Text"],["Nil"]],"uni":["Imm"]}],["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"Text"],["Nil"]],"uni":["Imm"]}],["Nil"]]],{"raw":["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"Attr"],["Cons",["TypeVar",2284],["Nil"]]],"uni":["Imm"]}],
    value: $browser$Html$style,
  }), ($core$Core$Cons)(({
    expression: ["Variable",["RefGlobal",["USR",["UMR",["Browser"],"Html"],"class"]]],
    nonFn: [],
    raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"Text"],["Nil"]],"uni":["Imm"]}],["Nil"]],{"raw":["TypeExact",["USR",["UMR",["Browser"],"VirtualDom"],"Attr"],["Cons",["TypeVar",2282],["Nil"]]],"uni":["Imm"]}],
    value: $browser$Html$class,
  }), $core$Core$Nil)))))));
  const $l2 = ($core$Core$Cons)(({
    first: ($sd1$Types$Meta$USR)(($sd1$Types$Meta$UMR)($sd1$Types$Meta$Core, "List"), "blah"),
    second: ({
      expression: ["Fn",["Cons",{"first":false,"second":["Just","x"]},["Nil"]],["LiteralNumber",0.1]],
      nonFn: [],
      raw: ["TypeFn",["Cons",["ParSp",{"raw":["TypeVar",2299],"uni":["Imm"]}],["Nil"]],{"raw":["TypeExact",["USR",["UMR",["Core"],"Core"],"Number"],["Nil"]],"uni":["Uni"]}],
      value: (($x) => {
        return 0.1;
      }),
    }),
  }), $core$Core$Nil);
  return ($core$List$concat)(($core$Core$Cons)($l1, ($core$Core$Cons)($l2, $core$Core$Nil)));
}))();

const $sd3$CompileText$meta = ((() => {
  const $1 = ($sd1$ModulesFile$textToModulesFile)("modules.sp", $sd1$Platforms$Browser$platform.defaultModules);
  return ((($1)[0] === "Ok")
    ? ((() => {
      const $m = ($1)[1];
      return ($sd1$ModulesFile$toMeta)($m);
    }))()
    : ((($1)[0] === "Err")
      ? ((() => {
        const $err = ($1)[1];
        const $errAsText = ((($1) => {
          return ($core$Text$join)("", $1);
        }))(((($1) => {
          return ($core$List$map)(sp_toHuman, $1);
        }))(($sd1$Compiler$Error$toFormattedText)($err)));
        (sp_log)($errAsText, "--");
        return (sp_todo)("This is a compiler bug, not your fault.");
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 50:2', (sp_toHuman)($1))));
}))();

const $sd3$CompileText$blue = (($t) => {
  return ($browser$Html$span)(($core$Core$Cons)(($browser$Html$class)("blue"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)($t), $core$Core$Nil));
});

const $sd3$CompileText$red = (($t) => {
  return ($browser$Html$span)(($core$Core$Cons)(($browser$Html$class)("red"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)($t), $core$Core$Nil));
});

const $sd3$CompileText$yellow = (($t) => {
  return ($browser$Html$span)(($core$Core$Cons)(($browser$Html$class)("yellow"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)($t), $core$Core$Nil));
});

const $sd3$CompileText$formattedToConsoleColoredText = (($formattedText) => {
  return ((($formattedText)[0] === "FormattedText_Default")
    ? ((() => {
      const $t = ($formattedText)[1];
      return ($browser$Html$text)($t);
    }))()
    : ((($formattedText)[0] === "FormattedText_Emphasys")
      ? ((() => {
        const $t = ($formattedText)[1];
        return ($sd3$CompileText$yellow)($t);
      }))()
      : ((($formattedText)[0] === "FormattedText_Warning")
        ? ((() => {
          const $t = ($formattedText)[1];
          return ($sd3$CompileText$red)($t);
        }))()
        : ((($formattedText)[0] === "FormattedText_Decoration")
          ? ((() => {
            const $t = ($formattedText)[1];
            return ($sd3$CompileText$blue)($t);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 19:4', (sp_toHuman)($formattedText))))));
});

const $sd3$CompileText$resToConsoleText = (($res) => {
  return ((($res)[0] === "Ok")
    ? ((() => {
      const $a = ($res)[1];
      return ($core$Result$Ok)($a);
    }))()
    : ((($res)[0] === "Err")
      ? ((() => {
        const $e = ($res)[1];
        return ($core$Result$Err)(((($1) => {
          return ($browser$Html$div)($core$Core$Nil, $1);
        }))(((($1) => {
          return ($core$List$map)($sd3$CompileText$formattedToConsoleColoredText, $1);
        }))(($sd1$Compiler$Error$toFormattedText)($e))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 28:4', (sp_toHuman)($res))));
});

const $sd3$CompileText$onResSuccess = (($f) => {
  return (($res) => {
    return (($core$Result$onOk)($f))(($sd3$CompileText$resToConsoleText)($res));
  });
});

const $sd3$CompileText$viewErrorWrongType = (($type) => {
  return ($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)("I don't know how to use this type:"), ($core$Core$Cons)(($browser$Html$text)(((($1) => {
    return ($sd1$Human$Type$display)("", $1);
  }))(((($1) => {
    return ($sd1$Human$Type$doRawType)({}, $1);
  }))($type))), $core$Core$Nil)));
});

const $sd3$CompileText$main = (($code) => {
  const $inputFileName = "user_input";
  const $entryModule = ($sd1$Types$Meta$UMR)(($sd1$Types$Meta$SourceDirId)($inputFileName), $inputFileName);
  return (($sd3$CompileText$onResSuccess)((($out) => {
    const $loadResult = (($core$Result$onErr)(((_0) => {
      return (($core$Result$onErr)(((_0) => {
        return (($core$Result$onErr)(((_0) => {
          return (self_load)("TypeExact\n    (USR\n        (UMR\n            Browser\n            \"VirtualDom\"\n        )\n        \"VirtualNode\"\n    )\n    ([TypeExact\n        (USR\n            (UMR\n                Core\n                \"Core\"\n            )\n            \"Text\"\n        )\n        []\n]    )\n", $out, $sd3$CompileText$CompiledHtml);
        })))((self_load)("TypeFn\n    ([ParSp\n        {\n            raw = TypeExact\n                (USR\n                    (UMR\n                        Core\n                        \"Core\"\n                    )\n                    \"Number\"\n                )\n                []\n\n            uni = Imm\n        }\n, ParSp\n        {\n            raw = TypeExact\n                (USR\n                    (UMR\n                        Core\n                        \"Core\"\n                    )\n                    \"Number\"\n                )\n                []\n\n            uni = Imm\n        }\n]    )\n    {\n        raw = TypeRecord\n            Nothing\n            (RBNode_elm_builtin\n                Black\n                \"g\"\n                (TypeExact\n                    (USR\n                        (UMR\n                            Core\n                            \"Core\"\n                        )\n                        \"Number\"\n                    )\n                    []\n                )\n                (RBNode_elm_builtin\n                    Black\n                    \"b\"\n                    (TypeExact\n                        (USR\n                            (UMR\n                                Core\n                                \"Core\"\n                            )\n                            \"Number\"\n                        )\n                        []\n                    )\n                    RBEmpty_elm_builtin\n                    RBEmpty_elm_builtin\n                )\n                (RBNode_elm_builtin\n                    Black\n                    \"r\"\n                    (TypeExact\n                        (USR\n                            (UMR\n                                Core\n                                \"Core\"\n                            )\n                            \"Number\"\n                        )\n                        []\n                    )\n                    RBEmpty_elm_builtin\n                    RBEmpty_elm_builtin\n                )\n            )\n\n        uni = Imm\n    }\n", $out, $sd3$CompileText$CompiledShader));
      })))((self_load)("TypeExact\n    (USR\n        (UMR\n            Core\n            \"Core\"\n        )\n        \"Text\"\n    )\n    []\n", $out, $sd3$CompileText$CompiledText));
    })))((self_load)("TypeExact\n    (USR\n        (UMR\n            Core\n            \"Core\"\n        )\n        \"Number\"\n    )\n    []\n", $out, $sd3$CompileText$CompiledNumber));
    return (($core$Result$onErr)((($actualCompiledType) => {
      return ($core$Result$Err)(($sd3$CompileText$viewErrorWrongType)($actualCompiledType));
    })))($loadResult);
  })))(($sd1$Compiler$Compiler$compileModules)(({
    entryModule: $entryModule,
    exposedValues: $sd3$CompileText$exposedValues,
    meta: $sd3$CompileText$meta,
    modules: ($core$Core$Cons)(({
      first: $entryModule,
      second: $code,
    }), $core$Core$Nil),
    umrToFsPath: ((_0) => {
      return $inputFileName;
    }),
  })));
});

const $sd3$App$init = (($effects) => {
  const $code = $sd3$App$htmlQuickExample;
  const $2 = ((() => {
    const $3 = ($sd3$CompileText$main)($code);
    return ((($3)[0] === "Ok")
      ? ((() => {
        const $c = ($3)[1];
        return ({
          first: $core$Maybe$Nothing,
          second: $c,
        });
      }))()
      : ((($3)[0] === "Err")
        ? ((() => {
          const $e = ($3)[1];
          return ({
            first: ($core$Maybe$Just)($e),
            second: ($sd3$CompileText$CompiledText)(""),
          });
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 76:8', (sp_toHuman)($3))));
  }))();
  const $compiledCode = $2.second;
  const $maybeError = $2.first;
  return ([
    ((($1) => {
      return ((__re__ = ($sd3$App$maybeUpdateCanvas)($effects, $1)), ($effects = (__re__)[1]), (__re__)[0]);
    }))(({
      code: $code,
      compiledCode: $compiledCode,
      embeddedInputs: $core$Core$Nil,
      maybeError: $maybeError,
      maybePosition: $core$Maybe$Nothing,
    })),
    $effects,
  ]);
});

const $sd3$App$syncScroll = (($effects, $top, $left) => {
  return ([
    ((__re__ = (array_push)($effects, (virtualDom_setViewportOf)("highlight", $top, $left))), ($effects = (__re__)[1]), (__re__)[0]),
    $effects,
  ]);
});

const $sd3$App$update = (($effects, $msg, $model) => {
  return ([
    ((($msg)[0] === "OnEmbeddedInput")
      ? ((() => {
        const $text = ($msg)[1];
        const $0 = $model;
        return (Object.assign)({}, $0, ({
          embeddedInputs: ($core$List$take)(10, ($core$Core$Cons)($text, $0.embeddedInputs)),
        }));
      }))()
      : ((($msg)[0] === "OnScroll")
        ? ((() => {
          const $top = ($msg)[1];
          const $left = ($msg)[2];
          ((__re__ = ($sd3$App$syncScroll)($effects, $top, $left)), ($effects = (__re__)[1]), (__re__)[0]);
          return $model;
        }))()
        : ((($msg)[0] === "OnInput")
          ? ((() => {
            const $code = ($msg)[1];
            const $4 = ($sd3$CompileText$main)($code);
            return ((($4)[0] === "Err")
              ? ((() => {
                const $error = ($4)[1];
                const $0 = $model;
                return (Object.assign)({}, $0, ({
                  code: $code,
                  maybeError: ($core$Maybe$Just)($error),
                }));
              }))()
              : ((($4)[0] === "Ok")
                ? ((() => {
                  const $compiledCode = ($4)[1];
                  return ((($1) => {
                    return ((__re__ = ($sd3$App$maybeUpdateCanvas)($effects, $1)), ($effects = (__re__)[1]), (__re__)[0]);
                  }))(((() => {
                    const $0 = $model;
                    return (Object.assign)({}, $0, ({
                      code: $code,
                      compiledCode: $compiledCode,
                      maybeError: $core$Maybe$Nothing,
                    }));
                  }))());
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 102:12', (sp_toHuman)($4))));
          }))()
          : ((($msg)[0] === "OnMouseMove")
            ? ((() => {
              const $x = ($msg)[1];
              const $y = ($msg)[2];
              const $0 = $model;
              return (Object.assign)({}, $0, ({
                maybePosition: ($core$Maybe$Just)(({
                  x: $x,
                  y: $y,
                })),
              }));
            }))()
            : ((($msg)[0] === "OnMouseLeave")
              ? ((() => {
                const $0 = $model;
                return (Object.assign)({}, $0, ({
                  maybePosition: $core$Maybe$Nothing,
                }));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 93:4', (sp_toHuman)($msg))))))),
    $effects,
  ]);
});

const $sd3$App$numberQuickExample = "main =\n    1 + 3 * 2";

const $sd3$App$textQuickExample = "main =\n    \"blah\" .. \"meh\"";

const $sd3$App$onMouseMove = (($event) => {
  return (($core$Result$onOk)((($clientWidth) => {
    return (($core$Result$onOk)((($clientHeight) => {
      return (($core$Result$onOk)((($offsetX) => {
        return (($core$Result$onOk)((($offsetY) => {
          const $w = ($clientWidth - 1);
          const $h = ($clientHeight - 1);
          const $x = (sp_divide)($offsetX, $w);
          const $y = (1 - (sp_divide)($offsetY, $h));
          return ($core$Result$Ok)(($sd3$App$OnMouseMove)($x, $y));
        })))((virtualDom_eventToFloat)(($core$Core$Cons)("offsetY", $core$Core$Nil), $event));
      })))((virtualDom_eventToFloat)(($core$Core$Cons)("offsetX", $core$Core$Nil), $event));
    })))((virtualDom_eventToFloat)(($core$Core$Cons)("target", ($core$Core$Cons)("clientHeight", $core$Core$Nil)), $event));
  })))((virtualDom_eventToFloat)(($core$Core$Cons)("target", ($core$Core$Cons)("clientWidth", $core$Core$Nil)), $event));
});

const $sd3$App$viewCompiledHtml = (($model, $html) => {
  return ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("h100 border"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$map)($sd3$App$OnEmbeddedInput, $html), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("h100 ml col"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mb"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("Last messages:"), $core$Core$Nil)), ($core$Core$Cons)(((sp_equal)($model.embeddedInputs, $core$Core$Nil)
    ? ($browser$Html$div)(($core$Core$Cons)(($browser$Html$style)("margin-bottom", "4px"), ($core$Core$Cons)(($browser$Html$style)("color", "gray"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)("(none yet)"), $core$Core$Nil))
    : ($browser$Html$code)(($core$Core$Cons)(($browser$Html$class)("ml"), $core$Core$Nil), ($core$List$map)((($i) => {
      return ($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)($i), $core$Core$Nil));
    }), $model.embeddedInputs))), $core$Core$Nil))), $core$Core$Nil));
});

const $sd3$App$viewCompiledOutput = (($model) => {
  return ($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("flex1 justify-center align-center h100 mt"), $core$Core$Nil), ((() => {
    const $2 = $model.compiledCode;
    return ((($2)[0] === "CompiledHtml")
      ? ((() => {
        const $html = ($2)[1];
        return ($sd3$App$viewCompiledHtml)($model, $html);
      }))()
      : ((($2)[0] === "CompiledNumber")
        ? ((() => {
          const $n = ($2)[1];
          return ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$style)("font-size", "400%"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)((text_fromNumber)($n)), $core$Core$Nil)), $core$Core$Nil);
        }))()
        : ((($2)[0] === "CompiledText")
          ? ((() => {
            const $t = ($2)[1];
            return ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$style)("font-size", "200%"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)($t), $core$Core$Nil)), $core$Core$Nil);
          }))()
          : ((($2)[0] === "CompiledShader")
            ? ((() => {
              const $shaderFn = ($2)[1];
              return ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("row"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$canvas)(($core$Core$Cons)(($browser$Html$class)("flex1"), ($core$Core$Cons)(($browser$Html$id)("output"), ($core$Core$Cons)(($browser$Html$height)("300"), ($core$Core$Cons)(($browser$Html$on)("mousemove", $sd3$App$onMouseMove), ($core$Core$Cons)(($browser$Html$on)("mouseleave", (($e) => {
                return ($core$Result$Ok)($sd3$App$OnMouseLeave);
              })), $core$Core$Nil)))))), ($core$Core$Cons)(((() => {
                const $3 = $model.maybePosition;
                return ((($3)[0] === "Nothing")
                  ? ($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("ml"), ($core$Core$Cons)(($browser$Html$style)("visibility", "hidden"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)("Program output:"), $core$Core$Nil))
                  : ((($3)[0] === "Just")
                    ? ((() => {
                      const $x = ($3)[1].x;
                      const $y = ($3)[1].y;
                      const $out = ($shaderFn)($x, $y);
                      return ($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("ml"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)("Program input:"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)(("x = " + ($sd3$App$floatToPercent)($x))), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)(("y = " + ($sd3$App$floatToPercent)($y))), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mt"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("Program output:"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)(("r = " + ($sd3$App$floatToPercent)($out.r))), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)(("g = " + ($sd3$App$floatToPercent)($out.g))), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)(("b = " + ($sd3$App$floatToPercent)($out.b))), $core$Core$Nil)), $core$Core$Nil))))))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 393:18', (sp_toHuman)($3))));
              }))(), $core$Core$Nil))), $core$Core$Nil);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 365:6', (sp_toHuman)($2))))));
  }))());
});

const $sd3$App$onScrollEvent = (($event) => {
  return (($core$Result$onOk)((($top) => {
    return (($core$Result$onOk)((($left) => {
      return ($core$Result$Ok)(($sd3$App$OnScroll)($top, $left));
    })))((virtualDom_eventToFloat)(($core$Core$Cons)("target", ($core$Core$Cons)("scrollLeft", $core$Core$Nil)), $event));
  })))((virtualDom_eventToFloat)(($core$Core$Cons)("target", ($core$Core$Cons)("scrollTop", $core$Core$Nil)), $event));
});

const $sd3$App$wordToClass = (($word) => {
  const $2 = $word.name;
  return (("alias" === $2)
    ? "keyword"
    : (("union" === $2)
      ? "keyword"
      : (true
        ? ($word.isUpper
          ? "upper"
          : "lower")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 173:4', (sp_toHuman)($2)))));
});

const $sd3$App$tokenToClass = (($kind) => {
  return ((($kind)[0] === "NewSiblingLine")
    ? ""
    : ((($kind)[0] === "BlockStart")
      ? ""
      : ((($kind)[0] === "BlockEnd")
        ? ""
        : ((($kind)[0] === "BadIndent")
          ? ""
          : ((($kind)[0] === "TextLiteral")
            ? "literal"
            : ((($kind)[0] === "NumberLiteral")
              ? "literal"
              : ((($kind)[0] === "Word")
                ? ((() => {
                  const $w = ($kind)[1];
                  return ($sd3$App$wordToClass)($w);
                }))()
                : ((($kind)[0] === "ArgumentPlaceholder")
                  ? "keyword"
                  : ((($kind)[0] === "UniquenessPolymorphismBinop")
                    ? "keyword"
                    : ((($kind)[0] === "Fn")
                      ? "keyword"
                      : ((($kind)[0] === "If")
                        ? "keyword"
                        : ((($kind)[0] === "Then")
                          ? "keyword"
                          : ((($kind)[0] === "Else")
                            ? "keyword"
                            : ((($kind)[0] === "Try")
                              ? "keyword"
                              : ((($kind)[0] === "As")
                                ? "keyword"
                                : ((($kind)[0] === "With")
                                  ? "keyword"
                                  : ((($kind)[0] === "Comma")
                                    ? "paren"
                                    : ((($kind)[0] === "Colon")
                                      ? "paren"
                                      : ((($kind)[0] === "ThreeDots")
                                        ? "paren"
                                        : ((($kind)[0] === "Defop")
                                          ? "op"
                                          : ((($kind)[0] === "Unop")
                                            ? "op"
                                            : ((($kind)[0] === "Binop")
                                              ? "op"
                                              : ((($kind)[0] === "RoundParen")
                                                ? "paren"
                                                : ((($kind)[0] === "SquareBracket")
                                                  ? "paren"
                                                  : ((($kind)[0] === "CurlyBrace")
                                                    ? "paren"
                                                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 182:4', (sp_toHuman)($kind)))))))))))))))))))))))))));
});

const $sd3$App$viewColorToken = (($code, $2, $1) => {
  const $comment = ($2)[1];
  const $tStart = ($2)[2];
  const $tEnd = ($2)[3];
  const $kind = ($2)[4];
  const $start = $1.first;
  const $accum = $1.second;
  const $slice = (text_slice)($start, $tEnd, $code);
  const $acc = ((text_startsWith)(" ", $slice)
    ? ($core$Core$Cons)(($browser$Html$span)(($core$Core$Cons)(($browser$Html$class)(($sd3$App$tokenToClass)($kind)), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)((text_dropLeft)(1, $slice)), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$span)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)(" "), $core$Core$Nil)), $accum))
    : ($core$Core$Cons)(($browser$Html$span)(($core$Core$Cons)(($browser$Html$class)(($sd3$App$tokenToClass)($kind)), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)($slice), $core$Core$Nil)), $accum));
  return ({
    first: $tEnd,
    second: $acc,
  });
});

const $sd3$App$viewEditor = (($model) => {
  const $code = $model.code;
  const $highlightedContent = ((() => {
    const $2 = ($sd1$Compiler$Lexer$lexer)(({
      content: $code,
      fsPath: "",
    }));
    return ((($2)[0] === "Err")
      ? ($core$Core$Cons)(($browser$Html$text)($code), $core$Core$Nil)
      : ((($2)[0] === "Ok")
        ? ((() => {
          const $tokens = ($2)[1];
          return ($core$List$reverse)(($core$Tuple$second)(((($1) => {
            return ($core$List$for)(({
              first: 0,
              second: $core$Core$Nil,
            }), $1, (($1, $2) => {
              return ($sd3$App$viewColorToken)($code, $1, $2);
            }));
          }))(($core$List$concat)($tokens))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 258:8', (sp_toHuman)($2))));
  }))();
  return ($browser$Html$div)(($core$Core$Cons)(($browser$Html$style)("position", "relative"), ($core$Core$Cons)(($browser$Html$style)("margin-bottom", "2em"), ($core$Core$Cons)(($browser$Html$class)("flex1 h100"), $core$Core$Nil))), ($core$Core$Cons)(($browser$Html$textarea)(($core$Core$Cons)(($browser$Html$class)("editing border"), ($core$Core$Cons)(($browser$Html$onInput)($sd3$App$OnInput), ($core$Core$Cons)(($browser$Html$on)("scroll", $sd3$App$onScrollEvent), ($core$Core$Cons)(($browser$Html$spellcheck)(false), ($core$Core$Cons)(($browser$Html$value)($code), $core$Core$Nil))))), $code), ($core$Core$Cons)(($browser$Html$pre)(($core$Core$Cons)(($browser$Html$id)("highlight"), ($core$Core$Cons)(($browser$Html$class)("highlighting border"), ($core$Core$Cons)(($browser$Html$ariaHidden)(true), $core$Core$Nil))), ($core$Core$Cons)(($browser$Html$code)(($core$Core$Cons)(($browser$Html$class)("highlighting-content"), $core$Core$Nil), $highlightedContent), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$style)("position", "absolute"), ($core$Core$Cons)(($browser$Html$style)("top", "calc(50% - 12px)"), ($core$Core$Cons)(($browser$Html$style)("left", "calc(100% - 15px)"), ($core$Core$Cons)(($browser$Html$style)("width", "0"), ($core$Core$Cons)(($browser$Html$style)("height", "0"), ($core$Core$Cons)(($browser$Html$style)("border", "25px solid transparent"), ($core$Core$Cons)(($browser$Html$style)("border-left", "25px solid lightgrey"), ($core$Core$Cons)(($browser$Html$class)("align-center justify-center"), $core$Core$Nil)))))))), ((sp_equal)($model.maybeError, $core$Maybe$Nothing)
    ? $core$Core$Nil
    : ($core$Core$Cons)(($browser$Html$a)(($core$Core$Cons)(($browser$Html$class)("pulse"), ($core$Core$Cons)(($browser$Html$style)("color", "red"), ($core$Core$Cons)(($browser$Html$style)("font-size", "130px"), ($core$Core$Cons)(($browser$Html$style)("position", "relative"), ($core$Core$Cons)(($browser$Html$style)("left", "-15px"), ($core$Core$Cons)(($browser$Html$style)("top", "-5px"), ($core$Core$Cons)(($browser$Html$style)("z-index", "2"), ($core$Core$Cons)(($browser$Html$style)("user-select", "none"), ($core$Core$Cons)(($browser$Html$style)("text-decoration", "none"), ($core$Core$Cons)(($browser$Html$href)("#error"), $core$Core$Nil)))))))))), ($core$Core$Cons)(($browser$Html$text)("×"), $core$Core$Nil)), $core$Core$Nil))), $core$Core$Nil))));
});

const $sd3$App$view = (($model) => {
  return ($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("col page-width"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mb"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("row align-center"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$img)(($core$Core$Cons)(($browser$Html$src)("https://xarvh.github.io/squarepants/logo/logo.svg"), ($core$Core$Cons)(($browser$Html$alt)("A stylized black Greek letter lambda, with square red pants, jumping happily"), ($core$Core$Cons)(($browser$Html$style)("width", "50px"), $core$Core$Nil)))), ($core$Core$Cons)(($browser$Html$h1)($core$Core$Nil, ($core$Core$Cons)(($browser$Html$text)("Squarepants "), ($core$Core$Cons)(($browser$Html$span)(($core$Core$Cons)(($browser$Html$class)("lineThrough"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("tutorial"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)(" demo"), $core$Core$Nil)))), $core$Core$Nil))), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mb"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$a)(($core$Core$Cons)(($browser$Html$href)("https://github.com/xarvh/squarepants#readme"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("Squarepants"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)(" is a small, scalable functional language for interactive apps, made for everyone."), $core$Core$Nil))), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mb"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("This page is entirely "), ($core$Core$Cons)(($browser$Html$a)(($core$Core$Cons)(($browser$Html$href)("https://github.com/xarvh/squarepants/blob/main/tutorial/App.sp"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("written"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)(" in Squarepants without any ad-hoc JavaScript."), $core$Core$Nil)))), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mb"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("(Also, both Squarepants and this page are under heavy development and are far from completed. Expect bugs.)"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mt mb"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("Try some quick examples: "), ($core$Core$Cons)(($browser$Html$button)(($core$Core$Cons)(($browser$Html$class)("ml"), ($core$Core$Cons)(($browser$Html$onClick)(($sd3$App$OnInput)($sd3$App$numberQuickExample)), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)("Numbers"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$button)(($core$Core$Cons)(($browser$Html$class)("ml"), ($core$Core$Cons)(($browser$Html$onClick)(($sd3$App$OnInput)($sd3$App$textQuickExample)), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)("Text"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$button)(($core$Core$Cons)(($browser$Html$class)("ml"), ($core$Core$Cons)(($browser$Html$onClick)(($sd3$App$OnInput)($sd3$App$htmlQuickExample)), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)("Html"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$button)(($core$Core$Cons)(($browser$Html$class)("ml"), ($core$Core$Cons)(($browser$Html$onClick)(($sd3$App$OnInput)($sd3$App$canvasQuickExample)), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$text)("Canvas"), $core$Core$Nil)), $core$Core$Nil)))))), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("align-center"), ($core$Core$Cons)(($browser$Html$style)("min-width", "900px"), ($core$Core$Cons)(($browser$Html$style)("height", "50vh"), $core$Core$Nil))), ($core$Core$Cons)(($sd3$App$viewEditor)($model), ($core$Core$Cons)(($sd3$App$viewCompiledOutput)($model), $core$Core$Nil))), ($core$Core$Cons)(((() => {
    const $2 = $model.maybeError;
    return ((($2)[0] === "Nothing")
      ? $browser$Html$none
      : ((($2)[0] === "Just")
        ? ((() => {
          const $error = ($2)[1];
          return ($browser$Html$div)(($core$Core$Cons)(($browser$Html$id)("error"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$div)(($core$Core$Cons)(($browser$Html$class)("mt red pulse"), $core$Core$Nil), ($core$Core$Cons)(($browser$Html$text)("Error!"), $core$Core$Nil)), ($core$Core$Cons)(($browser$Html$pre)(($core$Core$Cons)(($browser$Html$style)("margin-top", "0"), $core$Core$Nil), ($core$Core$Cons)($error, $core$Core$Nil)), $core$Core$Nil)));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/App.sp 489:10', (sp_toHuman)($2))));
  }))(), $core$Core$Nil))))))));
});

const $sd3$App$main = ({
  init: $sd3$App$init,
  update: $sd3$App$update,
  view: $sd3$App$view,
});
// TODO these globals will be a hell of trouble if we want to run more than one app
let effects = [];
let oldVirtualDom = {}; // TODO this should be properly initialized
let model = null;
let elementId = null;

function dispatch(msgResult) {
    if (msgResult[0] === "Ok") {

        const msg = msgResult[1];

        model = $sd3$App$main.update(effects, msg, model)[0];

            // TODO set a flag and use requestAnimationFrame
            updateDom();
        } else {
            console.log('rejecting msg: ', msgResult[1]);
        }
    }


    function updateDom() {
        const e = win.document.getElementById(elementId);

        const newVirtualDom = $sd3$App$main.view(model);

        $browser$VirtualDom$updateDomNode(newVirtualDom, oldVirtualDom, e.childNodes[0]);

        oldVirtualDom = newVirtualDom;

        effects.forEach((e) => e());
        effects = [];
    }



    function main(eid) {
        elementId = eid;
        model = $sd3$App$main.init(effects)[0];
        updateDom();
    }





    win.Squarepants = {
        main: main,
    };

})(this);
    