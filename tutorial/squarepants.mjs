squarepantsMain = (function() {
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


const sp_equal = (a) => (b) => {
  if (a === b)
    return true

  if (Array.isArray(a)) {
    if (!Array.isArray(b)) return false;

    const l = a.length;
    if (l !== b.length) return false;

    let i = 0;
    while (i < l) {
      if (!sp_equal(a[i])(b[i])) return false;
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
      if (!sp_equal(a[k])(b[k])) return false;
      ++i;
    }

    return true;
  }

  return false;
}


const sp_not_equal = (a) => (b) => {
  return !sp_equal(a)(b);
}


const sp_compare = (a, b) => {

  // union type
  if (Array.isArray(a)) {
    // compare constructor names
    if (a[0] > b[0]) return 1;
    if (b[0] > a[0]) return -1;
    for (let i = 1; i < a.length; i++) {
        const cmp = sp_compare(a[i], b[i]);
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
        const cmp = sp_compare(a[k], b[k]);
        if (cmp) return cmp;
    }
    return 0;
  }

  if (a > b) return 1;
  if (a < b) return -1;
  return 0;
}

const sp_divide = (right) => (left) => {
  if (right === 0) return 0;
  return left / right;
}


const basics_modBy = (a) => (b) => b % a;

const basics_compare = (a) => (b) => sp_compare(a, b);


//
// Debug
//


const sp_todo = (message) => {
  throw new Error("TODO: " + message);
}


const sp_log = (message) => (thing) => {
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


const sp_toHuman = (a) => {

  if (Array.isArray(a))
    return sp_toHumanAsList([], a) || sp_toHumanAsUnion(a);

  if (typeof a === 'function') {
    return '<function>';
  }

  if (typeof a === 'object') {
    let x = [];
    for (let i in a) x.push(i + ' = ' + sp_toHuman(a[i]));
    return '{' + x.join(', ') + '}';
  }

  return JSON.stringify(a, null, 0);
}


const sp_toHumanAsUnion = (a) => {
  return a[0] + ' ' + a.slice(1).map(arg => '(' + sp_toHuman(arg) + ')').join(' ');
}


const sp_toHumanAsList = (arrayAccum, list) => {
  if (list[0] === 'Cons') {
    arrayAccum.push(sp_toHuman(list[1]));
    return sp_toHumanAsList(arrayAccum, list[2]);
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

const text_split = (separator) => (target) => array_toList(target.split(separator));

const text_length = (s) => s.length;

const text_slice = (start) => (end) => (s) => s.slice(start, end);

const text_startsWith = (sub) => (s) => s.startsWith(sub);

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
    return () => () => ""
  }

  return (replacer) => (s) => s.replace(re, replacer);
}

const text_trimLeft = (s) => {
  return s.trimLeft();
}

const text_dropLeft = (n) => (s) => {
  return s.slice(n);
}

const text_forEach = (s) => (f) => {
  for (let i of s) f(i);
  return null;
}


//
// Hashes
//

const hash_empty = {};


const hash_insert = (hash) => (key) => (value) => {
    const h = hash.obj[hash.attr];
    h[JSON.stringify(key)] = [key, value];
    return null;
}


const hash_remove = (hash) => (key) => {
    const h = hash.obj[hash.attr];
    delete h[JSON.stringify(key)];
    return null;
}


const hash_get = (hash) => (key) => {
    const r = hash[JSON.stringify(key)];
    return r === undefined ? maybe_nothing : maybe_just(r[1]);
}


const hash_for = (hash) => (f) => (acc) => {
    for (let k in hash) {
        const kv = hash[k];
        acc = f(kv[0])(kv[1])(acc);
    }
    return acc;
}


const hash_each = (hash) => (f) => {
    for (let k in hash) {
        const kv = hash[k];
        f(kv[0])(kv[1]);
    }
    return null;
}


//
// Arrays
//

const array_push = (array) => (item) => {
    array.obj[array.attr].push(item);
    return null;
}

const array_pop = (array) => {
    const a = array.obj[array.attr];
    return a.length ? maybe_just(a.pop()) : maybe_nothing;
}

const array_get = (array) => (index) => {
    const r = array[index];
    return r === undefined ? maybe_nothing : maybe_just(r);
}

const array_set = (array) => (index) => (item) => {
    if (index < 0) return false;
    const a = array.obj[array.attr];
    if (index >= a.length) return false;
    a[index] = item;
    return true;
}

const array_sortBy = (array) => (f) => {
    const arr = array.obj[array.attr];
    arr.sort((a, b) => sp_compare(f(a), f(b)));
    return null;
}

const array_toList = (array) => {
  let length = array.length;
  let list = [ 'Nil' ];
  for (let i = length - 1; i >= 0; i--) {
      list = [ 'Cons', array[i], list ];
  }
  return list;
}

const array_fromList = (list) => {
  const array = [];
  const rec = (ls) => {
    if (ls[0] === 'Nil')
      return array;

    array.push(ls[1]);
    return rec(ls[2]);
  };

  return rec(list);
}



//
// Lists
//


const sp_cons = (list) => (item) => {
  return [ 'Cons', item, list];
}

const list_sortBy = (f) => (list) => array_toList(array_fromList(list).sort((a, b) => sp_compare(f(a), f(b))));
    const $core$Array$Array__ = (($1) => ([
  "Array__",
  $1,
]));

const $core$Core$Cons = (($1) => (($2) => ([
  "Cons",
  $1,
  $2,
])));

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

const $core$Dict$RBNode_elm_builtin = (($1) => (($2) => (($3) => (($4) => (($5) => ([
  "RBNode_elm_builtin",
  $1,
  $2,
  $3,
  $4,
  $5,
]))))));

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

const $$home$nw$stuff$unstable$lib$posix$IO$IO = (($1) => ([
  "IO",
  $1,
]));

const $$home$nw$stuff$unstable$lib$posix$IO$Never = (($1) => ([
  "Never",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Decoration = (($1) => ([
  "FormattedText_Decoration",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Default = (($1) => ([
  "FormattedText_Default",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Emphasys = (($1) => ([
  "FormattedText_Emphasys",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Warning = (($1) => ([
  "FormattedText_Warning",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$HighlightBlock = (($1) => ([
  "HighlightBlock",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$HighlightWord = (($1) => ([
  "HighlightWord",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$Nested = (($1) => ([
  "Nested",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Error$Simple = (($1) => (($2) => ([
  "Simple",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Compiler$Lexer$BlockComment = (($1) => ([
  "BlockComment",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Lexer$ContentOpeningBlockComment = ([
  "ContentOpeningBlockComment",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$ContentOpeningQuotes_One = ([
  "ContentOpeningQuotes_One",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$ContentOpeningQuotes_Two = ([
  "ContentOpeningQuotes_Two",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Default = ([
  "Default",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Dot = ([
  "Dot",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Indent = ([
  "Indent",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$LineComment = ([
  "LineComment",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Mutable = ([
  "Mutable",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$NoTabsOrSpacesYet = ([
  "NoTabsOrSpacesYet",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$NumberLiteral = ([
  "NumberLiteral",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$SingleQuote = (($1) => ([
  "SingleQuote",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Spaces = ([
  "Spaces",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Squiggles = ([
  "Squiggles",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Tabs = ([
  "Tabs",
]);

const $$home$nw$stuff$unstable$src$Compiler$Lexer$TripleQuote = (($1) => ([
  "TripleQuote",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$Lexer$Word = (($1) => ([
  "Word",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$DollarName = (($1) => ([
  "DollarName",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$NoNamedVariables = ([
  "NoNamedVariables",
]);

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$SafeMainName = (($1) => ([
  "SafeMainName",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$TrivialPattern = (($1) => ([
  "TrivialPattern",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$Cycle = (($1) => ([
  "Cycle",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleMutability = ([
  "IncompatibleMutability",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleRecords = (($1) => ([
  "IncompatibleRecords",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleTypes = ([
  "IncompatibleTypes",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$NI = (($1) => ([
  "NI",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$NonFunctionContainsFunction = (($1) => ([
  "NonFunctionContainsFunction",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$OkThisIsActuallyPossible = ([
  "OkThisIsActuallyPossible",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$SubstitutingAnnotation = (($1) => ([
  "SubstitutingAnnotation",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AnnotationSimple = ([
  "UnifyReason_AnnotationSimple",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AnnotationVsBlock = (($1) => (($2) => (($3) => ([
  "UnifyReason_AnnotationVsBlock",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AttributeAccess = (($1) => ([
  "UnifyReason_AttributeAccess",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AttributeUpdate = (($1) => ([
  "UnifyReason_AttributeUpdate",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_CallArgument = (($1) => ([
  "UnifyReason_CallArgument",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_ConstructorArgument = (($1) => ([
  "UnifyReason_ConstructorArgument",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_DefBlockVsPattern = ([
  "UnifyReason_DefBlockVsPattern",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IfBranches = ([
  "UnifyReason_IfBranches",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IfCondition = ([
  "UnifyReason_IfCondition",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IsBeingCalledAsAFunction = (($1) => (($2) => ([
  "UnifyReason_IsBeingCalledAsAFunction",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IsLambda = ([
  "UnifyReason_IsLambda",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_Override = ([
  "UnifyReason_Override",
]);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_TryBlock = (($1) => ([
  "UnifyReason_TryBlock",
  $1,
]));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_TryPattern = ([
  "UnifyReason_TryPattern",
]);

const $$home$nw$stuff$unstable$src$Main$Compile = (($1) => ([
  "Compile",
  $1,
]));

const $$home$nw$stuff$unstable$src$Main$Help = ([
  "Help",
]);

const $$home$nw$stuff$unstable$src$Main$Selftest = ([
  "Selftest",
]);

const $$home$nw$stuff$unstable$src$ModulesFile$Dir = (($1) => ([
  "Dir",
  $1,
]));

const $$home$nw$stuff$unstable$src$ModulesFile$Lib = (($1) => ([
  "Lib",
  $1,
]));

const $$home$nw$stuff$unstable$src$SPLib$Parser$Aborted = (($1) => (($2) => ([
  "Aborted",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$SPLib$Parser$Accepted = (($1) => (($2) => ([
  "Accepted",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$SPLib$Parser$Rejected = ([
  "Rejected",
]);

const $$home$nw$stuff$unstable$src$SPON$Accepted = (($1) => (($2) => ([
  "Accepted",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$SPON$Failed = (($1) => ([
  "Failed",
  $1,
]));

const $$home$nw$stuff$unstable$src$SPON$Rejected = (($1) => ([
  "Rejected",
  $1,
]));

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Block = (($1) => ([
  "Block",
  $1,
]));

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline = (($1) => ([
  "Inline",
  $1,
]));

const $$home$nw$stuff$unstable$src$Test$CodeExpectation = (($1) => ([
  "CodeExpectation",
  $1,
]));

const $$home$nw$stuff$unstable$src$Test$Error = (($1) => ([
  "Error",
  $1,
]));

const $$home$nw$stuff$unstable$src$Test$Group = (($1) => (($2) => ([
  "Group",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Test$NotNow = (($1) => ([
  "NotNow",
  $1,
]));

const $$home$nw$stuff$unstable$src$Test$Single = (($1) => (($2) => (($3) => ([
  "Single",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Test$Skipped = ([
  "Skipped",
]);

const $$home$nw$stuff$unstable$src$Test$Success = ([
  "Success",
]);

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression = (($1) => ([
  "ArgumentExpression",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentMutable = (($1) => (($2) => ([
  "ArgumentMutable",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$Call = (($1) => (($2) => (($3) => ([
  "Call",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$Constructor = (($1) => (($2) => ([
  "Constructor",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$If = (($1) => (($2) => ([
  "If",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$Lambda = (($1) => (($2) => (($3) => ([
  "Lambda",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$LetIn = (($1) => (($2) => ([
  "LetIn",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber = (($1) => (($2) => ([
  "LiteralNumber",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralText = (($1) => (($2) => ([
  "LiteralText",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$ParameterMutable = (($1) => (($2) => ([
  "ParameterMutable",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$ParameterPattern = (($1) => ([
  "ParameterPattern",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternAny = (($1) => (($2) => (($3) => ([
  "PatternAny",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternConstructor = (($1) => (($2) => (($3) => ([
  "PatternConstructor",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternLiteralNumber = (($1) => (($2) => ([
  "PatternLiteralNumber",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternLiteralText = (($1) => (($2) => ([
  "PatternLiteralText",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternRecord = (($1) => (($2) => ([
  "PatternRecord",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$Record = (($1) => (($2) => (($3) => ([
  "Record",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$RefBlock = (($1) => ([
  "RefBlock",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot = (($1) => ([
  "RefRoot",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$Try = (($1) => (($2) => (($3) => ([
  "Try",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeAlias = (($1) => (($2) => (($3) => ([
  "TypeAlias",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant = (($1) => (($2) => (($3) => ([
  "TypeConstant",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefAlias = (($1) => ([
  "TypeDefAlias",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefUnion = (($1) => ([
  "TypeDefUnion",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction = (($1) => (($2) => (($3) => (($4) => ([
  "TypeFunction",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord = (($1) => (($2) => (($3) => ([
  "TypeRecord",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable = (($1) => (($2) => ([
  "TypeVariable",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable = (($1) => (($2) => ([
  "Variable",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$And = (($1) => ([
  "And",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$ArrayAccess = (($1) => (($2) => ([
  "ArrayAccess",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Call = (($1) => (($2) => ([
  "Call",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Conditional = (($1) => (($2) => (($3) => ([
  "Conditional",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Constructor = (($1) => ([
  "Constructor",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$ConstructorAccess = (($1) => (($2) => ([
  "ConstructorAccess",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable = ([
  "Immutable",
]);

const $$home$nw$stuff$unstable$src$Types$EmittableAst$IsConstructor = (($1) => (($2) => ([
  "IsConstructor",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Lambda = (($1) => (($2) => ([
  "Lambda",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn = (($1) => ([
  "LetIn",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralArray = (($1) => ([
  "LiteralArray",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralNumber = (($1) => ([
  "LiteralNumber",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralRecord = (($1) => (($2) => ([
  "LiteralRecord",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralText = (($1) => ([
  "LiteralText",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$MissingPattern = (($1) => (($2) => ([
  "MissingPattern",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Mutable = ([
  "Mutable",
]);

const $$home$nw$stuff$unstable$src$Types$EmittableAst$RecordAccess = (($1) => (($2) => ([
  "RecordAccess",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$ShallowEqual = (($1) => (($2) => ([
  "ShallowEqual",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$EmittableAst$Variable = (($1) => (($2) => ([
  "Variable",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Binop = (($1) => (($2) => (($3) => ([
  "Binop",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Constructor = (($1) => (($2) => (($3) => ([
  "Constructor",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Definition = (($1) => (($2) => ([
  "Definition",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation = (($1) => (($2) => ([
  "Evaluation",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$FunctionCall = (($1) => (($2) => (($3) => ([
  "FunctionCall",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$If = (($1) => (($2) => ([
  "If",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda = (($1) => (($2) => (($3) => (($4) => ([
  "Lambda",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$List = (($1) => (($2) => ([
  "List",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber = (($1) => (($2) => ([
  "LiteralNumber",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralText = (($1) => (($2) => ([
  "LiteralText",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Mutable = (($1) => (($2) => (($3) => ([
  "Mutable",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny = (($1) => (($2) => (($3) => (($4) => ([
  "PatternAny",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternConstructor = (($1) => (($2) => (($3) => (($4) => ([
  "PatternConstructor",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternList = (($1) => (($2) => ([
  "PatternList",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternListCons = (($1) => (($2) => ([
  "PatternListCons",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternLiteralNumber = (($1) => (($2) => ([
  "PatternLiteralNumber",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternLiteralText = (($1) => (($2) => ([
  "PatternLiteralText",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternRecord = (($1) => (($2) => ([
  "PatternRecord",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PatternTuple = (($1) => (($2) => ([
  "PatternTuple",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$PrefixBinop = (($1) => (($2) => ([
  "PrefixBinop",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Record = (($1) => (($2) => ([
  "Record",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$RecordShorthand = (($1) => (($2) => ([
  "RecordShorthand",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Try = (($1) => (($2) => ([
  "Try",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeAlias = (($1) => ([
  "TypeAlias",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeConstant = (($1) => (($2) => (($3) => (($4) => ([
  "TypeConstant",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction = (($1) => (($2) => (($3) => (($4) => ([
  "TypeFunction",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeList = (($1) => (($2) => ([
  "TypeList",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeRecord = (($1) => (($2) => ([
  "TypeRecord",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeTuple = (($1) => (($2) => ([
  "TypeTuple",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$TypeVariable = (($1) => (($2) => ([
  "TypeVariable",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$UnionDef = (($1) => (($2) => ([
  "UnionDef",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Unop = (($1) => (($2) => (($3) => ([
  "Unop",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$FormattableAst$Variable = (($1) => (($2) => (($3) => (($4) => ([
  "Variable",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithBrackets = (($1) => (($2) => ([
  "AccessWithBrackets",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithDot = (($1) => (($2) => ([
  "AccessWithDot",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Array = (($1) => ([
  "Array",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop = (($1) => (($2) => (($3) => ([
  "Binop",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$BlockLambda = (($1) => (($2) => ([
  "BlockLambda",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Call = (($1) => (($2) => ([
  "Call",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Conditional = (($1) => (($2) => (($3) => ([
  "Conditional",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Define = (($1) => (($2) => ([
  "Define",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Eval = (($1) => ([
  "Eval",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$If = (($1) => (($2) => ([
  "If",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal = (($1) => ([
  "Literal",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Mutop = (($1) => (($2) => (($3) => (($4) => ([
  "Mutop",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Record = (($1) => ([
  "Record",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Return = (($1) => ([
  "Return",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda = (($1) => (($2) => ([
  "SimpleLambda",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Unop = (($1) => (($2) => ([
  "Unop",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$JavascriptAst$Var = (($1) => ([
  "Var",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Meta$Core = ([
  "Core",
]);

const $$home$nw$stuff$unstable$src$Types$Meta$Posix = ([
  "Posix",
]);

const $$home$nw$stuff$unstable$src$Types$Meta$SourceDir = (($1) => ([
  "SourceDir",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Meta$UMR = (($1) => (($2) => ([
  "UMR",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$Meta$USR = (($1) => (($2) => ([
  "USR",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$Op$Addittive = ([
  "Addittive",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Comparison = ([
  "Comparison",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Cons = ([
  "Cons",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Exponential = ([
  "Exponential",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Left = ([
  "Left",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Logical = ([
  "Logical",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Multiplicative = ([
  "Multiplicative",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Mutop = ([
  "Mutop",
]);

const $$home$nw$stuff$unstable$src$Types$Op$NonAssociative = ([
  "NonAssociative",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Pipe = ([
  "Pipe",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Right = ([
  "Right",
]);

const $$home$nw$stuff$unstable$src$Types$Op$Tuple = ([
  "Tuple",
]);

const $$home$nw$stuff$unstable$src$Types$Pos$At = (($1) => (($2) => ([
  "At",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$Pos$End = (($1) => ([
  "End",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Pos$G = ([
  "G",
]);

const $$home$nw$stuff$unstable$src$Types$Pos$I = (($1) => ([
  "I",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Pos$N = ([
  "N",
]);

const $$home$nw$stuff$unstable$src$Types$Pos$P = (($1) => (($2) => (($3) => ([
  "P",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$Pos$S = ([
  "S",
]);

const $$home$nw$stuff$unstable$src$Types$Pos$T = ([
  "T",
]);

const $$home$nw$stuff$unstable$src$Types$Token$As = ([
  "As",
]);

const $$home$nw$stuff$unstable$src$Types$Token$BadIndent = ([
  "BadIndent",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Binop = (($1) => ([
  "Binop",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$BlockEnd = ([
  "BlockEnd",
]);

const $$home$nw$stuff$unstable$src$Types$Token$BlockStart = ([
  "BlockStart",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Closed = ([
  "Closed",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Colon = ([
  "Colon",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Comma = ([
  "Comma",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Comment = ([
  "Comment",
]);

const $$home$nw$stuff$unstable$src$Types$Token$CurlyBrace = (($1) => ([
  "CurlyBrace",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$DefMutable = ([
  "DefMutable",
]);

const $$home$nw$stuff$unstable$src$Types$Token$DefNormal = ([
  "DefNormal",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Defop = (($1) => ([
  "Defop",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$Else = ([
  "Else",
]);

const $$home$nw$stuff$unstable$src$Types$Token$ErrorBlock = (($1) => ([
  "ErrorBlock",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$ErrorUnknownOp = (($1) => ([
  "ErrorUnknownOp",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$ErrorUnterminated = (($1) => ([
  "ErrorUnterminated",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$If = ([
  "If",
]);

const $$home$nw$stuff$unstable$src$Types$Token$LowerName = (($1) => (($2) => (($3) => (($4) => ([
  "LowerName",
  $1,
  $2,
  $3,
  $4,
])))));

const $$home$nw$stuff$unstable$src$Types$Token$MutableColon = ([
  "MutableColon",
]);

const $$home$nw$stuff$unstable$src$Types$Token$NameMutable = ([
  "NameMutable",
]);

const $$home$nw$stuff$unstable$src$Types$Token$NameNoModifier = ([
  "NameNoModifier",
]);

const $$home$nw$stuff$unstable$src$Types$Token$NameStartsWithDot = ([
  "NameStartsWithDot",
]);

const $$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine = ([
  "NewSiblingLine",
]);

const $$home$nw$stuff$unstable$src$Types$Token$NumberLiteral = (($1) => ([
  "NumberLiteral",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$Open = ([
  "Open",
]);

const $$home$nw$stuff$unstable$src$Types$Token$RoundParen = (($1) => ([
  "RoundParen",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$SquareBracket = (($1) => ([
  "SquareBracket",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$TextLiteral = (($1) => ([
  "TextLiteral",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$Then = ([
  "Then",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Token = (($1) => (($2) => (($3) => ([
  "Token",
  $1,
  $2,
  $3,
]))));

const $$home$nw$stuff$unstable$src$Types$Token$Try = ([
  "Try",
]);

const $$home$nw$stuff$unstable$src$Types$Token$Unop = (($1) => ([
  "Unop",
  $1,
]));

const $$home$nw$stuff$unstable$src$Types$Token$UpperName = (($1) => (($2) => ([
  "UpperName",
  $1,
  $2,
])));

const $$home$nw$stuff$unstable$src$Types$Token$With = ([
  "With",
]);

const $$home$nw$stuff$unstable$lib$posix$IO$_run = (($never) => {
  return (($r) => {
    const $$neverToResult = $r;
    const $neverToResult = ($$neverToResult)[1];
    return ($neverToResult)($never);
  });
});

const $$home$nw$stuff$unstable$lib$posix$IO$fail = (($message) => {
  return ($$home$nw$stuff$unstable$lib$posix$IO$IO)((($never) => {
    return ($core$Result$Err)($message);
  }));
});

const $$home$nw$stuff$unstable$lib$posix$IO$onResult = (($f) => {
  return (($m) => {
    return ($$home$nw$stuff$unstable$lib$posix$IO$IO)((($never) => {
      return (($$home$nw$stuff$unstable$lib$posix$IO$_run)($never))(($f)((($$home$nw$stuff$unstable$lib$posix$IO$_run)($never))($m)));
    }));
  });
});

const $$home$nw$stuff$unstable$lib$posix$IO$onSuccess = (($f) => {
  return (($m) => {
    return ($$home$nw$stuff$unstable$lib$posix$IO$IO)((($never) => {
      const $$try1 = (($$home$nw$stuff$unstable$lib$posix$IO$_run)($never))($m);
      return ((($$try1)[0] === "Ok")
        ? ((() => {
          const $a = ($$try1)[1];
          return (($$home$nw$stuff$unstable$lib$posix$IO$_run)($never))(($f)($a));
        }))()
        : ((($$try1)[0] === "Err")
          ? ((() => {
            const $e = ($$try1)[1];
            return ($core$Result$Err)($e);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/posix/IO.sp 40:8', (sp_toHuman)($$try1))));
    }));
  });
});

const $$home$nw$stuff$unstable$lib$posix$IO$parallel = (() => {
  return (sp_todo)("io.parallel");
});

const $$home$nw$stuff$unstable$lib$posix$IO$readDir = (() => {
  return (sp_todo)("io.readDir");
});

const $$home$nw$stuff$unstable$lib$posix$IO$readFile = (() => {
  return (sp_todo)("io.readFile");
});

const $$home$nw$stuff$unstable$lib$posix$IO$succeed = (($a) => {
  return ($$home$nw$stuff$unstable$lib$posix$IO$IO)((($never) => {
    return ($core$Result$Ok)($a);
  }));
});

const $$home$nw$stuff$unstable$lib$posix$IO$writeFile = (() => {
  return (sp_todo)("io.writeFile");
});

const $$home$nw$stuff$unstable$lib$posix$IO$writeStdout = (() => {
  return (sp_todo)("io.writeStdout");
});

const $$home$nw$stuff$unstable$lib$posix$Path$dirname = (() => {
  return (sp_todo)("Path.dirname");
});

const $$home$nw$stuff$unstable$lib$posix$Path$resolve = (() => {
  return (sp_todo)("Path.resolve");
});

const $core$Text$join = (($sep) => {
  return (($listOfText) => {
    return ((($listOfText)[0] === "Nil")
      ? ""
      : ((($listOfText)[0] === "Cons")
        ? ((() => {
          const $head = ($listOfText)[1];
          const $tail = ($listOfText)[2];
          const $rec = (($ls) => {
            return (($acc) => {
              return ((($ls)[0] === "Nil")
                ? $acc
                : ((($ls)[0] === "Cons")
                  ? ((() => {
                    const $h = ($ls)[1];
                    const $t = ($ls)[2];
                    return (($rec)($t))(($acc + ($sep + $h)));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Text.sp 135:12', (sp_toHuman)($ls))));
            });
          });
          return (($rec)($tail))($head);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Text.sp 127:4', (sp_toHuman)($listOfText))));
  });
});

const $core$Text$replace = (($toRemove) => {
  return (($toPut) => {
    return (($s) => {
      return (($core$Text$join)($toPut))(((text_split)($toRemove))($s));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compile$asModule = (($tuple) => {
  const $$isDirectory = $tuple;
  const $name = $$isDirectory.second;
  const $isDirectory = $$isDirectory.first;
  return (($isDirectory || ((sp_not_equal)($name))(((text_startsWithRegex)("[A-Z][a-zA-Z0-9_]*[.]sp$"))($name)))
    ? $core$Maybe$Nothing
    : ($core$Maybe$Just)(((($core$Text$replace)(".sp"))(""))($name)));
});

const $$home$nw$stuff$unstable$src$Compile$asModuleDirectory = (($tuple) => {
  const $$isDirectory = $tuple;
  const $name = $$isDirectory.second;
  const $isDirectory = $$isDirectory.first;
  return (($isDirectory && ((sp_equal)($name))(((text_startsWithRegex)("^[A-Z][a-zA-Z0-9_]*$"))($name)))
    ? ($core$Maybe$Just)($name)
    : $core$Maybe$Nothing);
});

const $$home$nw$stuff$unstable$src$Compile$libDirectoryName = "lib";

const $core$List$for = (($aList) => {
  return (($function) => {
    return (($init) => {
      return ((($aList)[0] === "Nil")
        ? $init
        : ((($aList)[0] === "Cons")
          ? ((() => {
            const $h = ($aList)[1];
            const $tail = ($aList)[2];
            return ((($core$List$for)($tail))($function))((($function)($h))($init));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 65:4', (sp_toHuman)($aList))));
    });
  });
});

const $core$List$reverse = (($aList) => {
  return ((($core$List$for)($aList))($core$Core$Cons))($core$Core$Nil);
});

const $core$List$forReversed = (($list) => {
  return (($f) => {
    return (($init) => {
      const $foldrHelper = (($acc) => {
        return (($ctr) => {
          return (($ls) => {
            return ((($ls)[0] === "Nil")
              ? $acc
              : ((($ls)[0] === "Cons")
                ? ((() => {
                  const $a = ($ls)[1];
                  const $r1 = ($ls)[2];
                  return ((($r1)[0] === "Nil")
                    ? (($f)($a))($acc)
                    : ((($r1)[0] === "Cons")
                      ? ((() => {
                        const $b = ($r1)[1];
                        const $r2 = ($r1)[2];
                        return ((($r2)[0] === "Nil")
                          ? (($f)($a))((($f)($b))($acc))
                          : ((($r2)[0] === "Cons")
                            ? ((() => {
                              const $c = ($r2)[1];
                              const $r3 = ($r2)[2];
                              return ((($r3)[0] === "Nil")
                                ? (($f)($a))((($f)($b))((($f)($c))($acc)))
                                : ((($r3)[0] === "Cons")
                                  ? ((() => {
                                    const $d = ($r3)[1];
                                    const $r4 = ($r3)[2];
                                    const $res = (($ctr > 500)
                                      ? ((($core$List$for)(($core$List$reverse)($r4)))($f))($acc)
                                      : ((($foldrHelper)($acc))(($ctr + 1)))($r4));
                                    return (($f)($a))((($f)($b))((($f)($c))((($f)($d))($res))));
                                  }))()
                                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 99:32', (sp_toHuman)($r3))));
                            }))()
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 94:24', (sp_toHuman)($r2))));
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 89:16', (sp_toHuman)($r1))));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 84:8', (sp_toHuman)($ls))));
          });
        });
      });
      return ((($foldrHelper)($init))(0))($list);
    });
  });
});

const $core$List$append = (($xs) => {
  return (($ys) => {
    return ((($ys)[0] === "Nil")
      ? $xs
      : (true
        ? ((($core$List$forReversed)($xs))($core$Core$Cons))($ys)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 189:2', (sp_toHuman)($ys))));
  });
});

const $core$List$concat = (($lists) => {
  return ((($core$List$forReversed)($lists))($core$List$append))($core$Core$Nil);
});

const $core$List$filterMap = (($f) => {
  return (($la) => {
    const $update = (($a) => {
      return (($acc) => {
        const $$try1 = ($f)($a);
        return ((($$try1)[0] === "Just")
          ? ((() => {
            const $b = ($$try1)[1];
            return ((sp_cons)($acc))($b);
          }))()
          : ((($$try1)[0] === "Nothing")
            ? $acc
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 309:6', (sp_toHuman)($$try1))));
      });
    });
    return ((($core$List$forReversed)($la))($update))($core$Core$Nil);
  });
});

const $core$List$map = (($f) => {
  return (($list) => {
    return ((($core$List$forReversed)($list))((($x) => {
      return (($acc) => {
        return ((sp_cons)($acc))(($f)($x));
      });
    })))($core$Core$Nil);
  });
});

const $$home$nw$stuff$unstable$src$Compile$listSourceDir = (($sourceDirRoot) => {
  return (($modulePathWithTrailingSlash) => {
    const $path = ($sourceDirRoot + ("/" + $modulePathWithTrailingSlash));
    return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($dirContents) => {
      const $directChildren = (($core$List$map)((($fileName) => {
        return ($modulePathWithTrailingSlash + $fileName);
      })))((($core$List$filterMap)($$home$nw$stuff$unstable$src$Compile$asModule))($dirContents));
      const $getDescendants = ($$home$nw$stuff$unstable$lib$posix$IO$parallel)((($core$List$map)((($subDir) => {
        return (($$home$nw$stuff$unstable$src$Compile$listSourceDir)($sourceDirRoot))(($modulePathWithTrailingSlash + ($subDir + "/")));
      })))((($core$List$filterMap)($$home$nw$stuff$unstable$src$Compile$asModuleDirectory))($dirContents)));
      return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($descendants) => {
        return ($$home$nw$stuff$unstable$lib$posix$IO$succeed)(($core$List$concat)((($core$Core$Cons)($directChildren))((($core$Core$Cons)(($core$List$concat)($descendants)))($core$Core$Nil))));
      })))($getDescendants);
    })))(($$home$nw$stuff$unstable$lib$posix$IO$readDir)($path));
  });
});

const $$home$nw$stuff$unstable$src$Compile$modulesFileName = "modules.sp";

const $$home$nw$stuff$unstable$src$Term$color = (($code) => {
  return (($text) => {
    return ($code + ($text + "\x1b[0m"));
  });
});

const $$home$nw$stuff$unstable$src$Term$blue = ($$home$nw$stuff$unstable$src$Term$color)("\x1b[34m");

const $$home$nw$stuff$unstable$src$Term$red = ($$home$nw$stuff$unstable$src$Term$color)("\x1b[31m");

const $$home$nw$stuff$unstable$src$Term$yellow = ($$home$nw$stuff$unstable$src$Term$color)("\x1b[33m");

const $$home$nw$stuff$unstable$src$Compile$formattedToConsoleColoredText = (($formattedText) => {
  return ((($formattedText)[0] === "FormattedText_Default")
    ? ((() => {
      const $t = ($formattedText)[1];
      return $t;
    }))()
    : ((($formattedText)[0] === "FormattedText_Emphasys")
      ? ((() => {
        const $t = ($formattedText)[1];
        return ($$home$nw$stuff$unstable$src$Term$yellow)($t);
      }))()
      : ((($formattedText)[0] === "FormattedText_Warning")
        ? ((() => {
          const $t = ($formattedText)[1];
          return ($$home$nw$stuff$unstable$src$Term$red)($t);
        }))()
        : ((($formattedText)[0] === "FormattedText_Decoration")
          ? ((() => {
            const $t = ($formattedText)[1];
            return ($$home$nw$stuff$unstable$src$Term$blue)($t);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 19:4', (sp_toHuman)($formattedText))))));
});

const $$home$nw$stuff$unstable$src$Compiler$Error$flatten = (($e) => {
  return (($accum) => {
    return ((($e)[0] === "Simple")
      ? ((() => {
        const $pos = ($e)[1];
        const $descr = ($e)[2];
        return ((sp_cons)($accum))(({
          first: $pos,
          second: $descr,
        }));
      }))()
      : ((($e)[0] === "Nested")
        ? ((() => {
          const $ls = ($e)[1];
          return ((($core$List$for)($ls))($$home$nw$stuff$unstable$src$Compiler$Error$flatten))($accum);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 104:4', (sp_toHuman)($e))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$formatSeparator = "$|$|$";

const $$home$nw$stuff$unstable$src$Compiler$Error$formatSuffix = "$`$`$";

const $core$List$indexedMap = (($f) => {
  const $rec = (($accum) => {
    return (($n) => {
      return (($list) => {
        return ((($list)[0] === "Nil")
          ? ($core$List$reverse)($accum)
          : ((($list)[0] === "Cons")
            ? ((() => {
              const $h = ($list)[1];
              const $t = ($list)[2];
              return ((($rec)(((sp_cons)($accum))((($f)($n))($h))))(($n + 1)))($t);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 180:8', (sp_toHuman)($list))));
      });
    });
  });
  return (($rec)($core$Core$Nil))(0);
});

const $$home$nw$stuff$unstable$src$Compiler$Error$breakDownText = (($text) => {
  const $formatSnippet = (($index) => {
    return (($snippet) => {
      return (((sp_equal)(0))(((basics_modBy)(2))($index))
        ? ($$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Default)($snippet)
        : ((() => {
          const $$try1 = ((text_split)($$home$nw$stuff$unstable$src$Compiler$Error$formatSuffix))($snippet);
          return (((($$try1)[0] === "Cons") && (("emphasys" === ($$try1)[1]) && (((($$try1)[2])[0] === "Cons") && (((($$try1)[2])[2])[0] === "Nil"))))
            ? ((() => {
              const $s = (($$try1)[2])[1];
              return ($$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Emphasys)($s);
            }))()
            : (((($$try1)[0] === "Cons") && (("warning" === ($$try1)[1]) && (((($$try1)[2])[0] === "Cons") && (((($$try1)[2])[2])[0] === "Nil"))))
              ? ((() => {
                const $s = (($$try1)[2])[1];
                return ($$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Warning)($s);
              }))()
              : (((($$try1)[0] === "Cons") && (("decoration" === ($$try1)[1]) && (((($$try1)[2])[0] === "Cons") && (((($$try1)[2])[2])[0] === "Nil"))))
                ? ((() => {
                  const $s = (($$try1)[2])[1];
                  return ($$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Decoration)($s);
                }))()
                : (true
                  ? ($$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Default)($snippet)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 69:10', (sp_toHuman)($$try1))))));
        }))());
    });
  });
  return (($core$List$indexedMap)($formatSnippet))(((text_split)($$home$nw$stuff$unstable$src$Compiler$Error$formatSeparator))($text));
});

const $$home$nw$stuff$unstable$src$Compiler$Error$formatWrap = (($fmtName) => {
  return (($text) => {
    return ($$home$nw$stuff$unstable$src$Compiler$Error$formatSeparator + ($fmtName + ($$home$nw$stuff$unstable$src$Compiler$Error$formatSuffix + ($text + $$home$nw$stuff$unstable$src$Compiler$Error$formatSeparator))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$deco = ($$home$nw$stuff$unstable$src$Compiler$Error$formatWrap)("decoration");

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
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 223:4', (sp_toHuman)($list)))));
});

const $core$List$length = (($list) => {
  return ((($core$List$for)($list))((() => {
    return (($a) => {
      return ($a + 1);
    });
  })))(0);
});

const $core$Maybe$map = (($f) => {
  return (($m) => {
    return ((($m)[0] === "Nothing")
      ? $core$Maybe$Nothing
      : ((($m)[0] === "Just")
        ? ((() => {
          const $v = ($m)[1];
          return ($core$Maybe$Just)(($f)($v));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 16:2', (sp_toHuman)($m))));
  });
});

const $core$Maybe$withDefault = (($default) => {
  return (($maybe) => {
    return ((($maybe)[0] === "Just")
      ? ((() => {
        const $v = ($maybe)[1];
        return $v;
      }))()
      : ((($maybe)[0] === "Nothing")
        ? $default
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 48:2', (sp_toHuman)($maybe))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$positionToLineAndColumn = (($s) => {
  return (($index) => {
    const $before = (((text_slice)(0))($index))($s);
    const $lines = ((text_split)("\n"))($before);
    const $lineNumber = ($core$List$length)($lines);
    const $colNumber = (($core$Maybe$withDefault)(0))((($core$Maybe$map)(text_length))(($core$List$last)($lines)));
    return ({
      col: $colNumber,
      line: $lineNumber,
    });
  });
});

const $core$Dict$balance = (($color) => {
  return (($key) => {
    return (($value) => {
      return (($left) => {
        return (($right) => {
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
                  return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($key))($value))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($lK))($lV))($lLeft))($lRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($rK))($rV))($rLeft))($rRight));
                }))()
                : (true
                  ? ((((($core$Dict$RBNode_elm_builtin)($color))($rK))($rV))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($key))($value))($left))($rLeft)))($rRight)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 117:6', (sp_toHuman)($left))));
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
                  return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($llK))($llV))($llLeft))($llRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($key))($value))($lRight))($right));
                }))()
                : (true
                  ? ((((($core$Dict$RBNode_elm_builtin)($color))($key))($value))($left))($right)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 130:6', (sp_toHuman)($left))))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 115:2', (sp_toHuman)($right))));
        });
      });
    });
  });
});

const $core$Dict$insertHelp = (($key) => {
  return (($value) => {
    return (($dict) => {
      return ((($dict)[0] === "RBEmpty_elm_builtin")
        ? ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($key))($value))($core$Dict$RBEmpty_elm_builtin))($core$Dict$RBEmpty_elm_builtin)
        : ((($dict)[0] === "RBNode_elm_builtin")
          ? ((() => {
            const $nColor = ($dict)[1];
            const $nKey = ($dict)[2];
            const $nValue = ($dict)[3];
            const $nLeft = ($dict)[4];
            const $nRight = ($dict)[5];
            const $$try1 = ((basics_compare)($key))($nKey);
            return ((1 === $$try1)
              ? ((((($core$Dict$balance)($nColor))($nKey))($nValue))($nLeft))(((($core$Dict$insertHelp)($key))($value))($nRight))
              : ((0 === $$try1)
                ? ((((($core$Dict$RBNode_elm_builtin)($nColor))($nKey))($value))($nLeft))($nRight)
                : (true
                  ? ((((($core$Dict$balance)($nColor))($nKey))($nValue))(((($core$Dict$insertHelp)($key))($value))($nLeft)))($nRight)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 101:6', (sp_toHuman)($$try1)))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 94:2', (sp_toHuman)($dict))));
    });
  });
});

const $core$Dict$insert = (($key) => {
  return (($value) => {
    return (($dict) => {
      const $$try1 = ((($core$Dict$insertHelp)($key))($value))($dict);
      return (((($$try1)[0] === "RBNode_elm_builtin") && ((($$try1)[1])[0] === "Red"))
        ? ((() => {
          const $k = ($$try1)[2];
          const $v = ($$try1)[3];
          const $l = ($$try1)[4];
          const $r = ($$try1)[5];
          return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))($l))($r);
        }))()
        : (true
          ? ((() => {
            const $x = $$try1;
            return $x;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 83:2', (sp_toHuman)($$try1))));
    });
  });
});

const $core$List$range = (($low) => {
  return (($high) => {
    const $rec = (($accum) => {
      return (($up) => {
        return (($up > $low)
          ? (($rec)(((sp_cons)($accum))($up)))(($up - 1))
          : (((sp_equal)($low))($up)
            ? ((sp_cons)($accum))($up)
            : $accum));
      });
    });
    return (($rec)($core$Core$Nil))($high);
  });
});

const $core$Set$insert = (($a) => {
  return (($core$Dict$insert)($a))(null);
});

const $$home$nw$stuff$unstable$src$Compiler$Error$highlightSplit = (($h) => {
  return (($x) => {
    const $$lines = $x;
    const $lines = $$lines.second;
    const $words = $$lines.first;
    return ((($h)[0] === "HighlightWord")
      ? ((() => {
        const $colEnd = ($h)[1].colEnd;
        const $colStart = ($h)[1].colStart;
        const $line = ($h)[1].line;
        return ({
          first: ((($core$Dict$insert)($line))(({
            first: $colStart,
            second: $colEnd,
          })))($words),
          second: $lines,
        });
      }))()
      : ((($h)[0] === "HighlightBlock")
        ? ((() => {
          const $lineEnd = ($h)[1].lineEnd;
          const $lineStart = ($h)[1].lineStart;
          return ({
            first: $words,
            second: ((($core$List$for)((($core$List$range)($lineStart))($lineEnd)))($core$Set$insert))($lines),
          });
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 141:4', (sp_toHuman)($h))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$warn = ($$home$nw$stuff$unstable$src$Compiler$Error$formatWrap)("warning");

const $core$Basics$max = (($a) => {
  return (($b) => {
    return (($a > $b)
      ? $a
      : $b);
  });
});

const $core$Dict$empty = $core$Dict$RBEmpty_elm_builtin;

const $core$Dict$get = (($targetKey) => {
  return (($dict) => {
    return ((($dict)[0] === "RBEmpty_elm_builtin")
      ? $core$Maybe$Nothing
      : ((($dict)[0] === "RBNode_elm_builtin")
        ? ((() => {
          const $key = ($dict)[2];
          const $value = ($dict)[3];
          const $left = ($dict)[4];
          const $right = ($dict)[5];
          const $$try1 = ((basics_compare)($targetKey))($key);
          return ((1 === $$try1)
            ? (($core$Dict$get)($targetKey))($right)
            : ((0 === $$try1)
              ? ($core$Maybe$Just)($value)
              : (true
                ? (($core$Dict$get)($targetKey))($left)
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 25:6', (sp_toHuman)($$try1)))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 20:2', (sp_toHuman)($dict))));
  });
});

const $core$Set$empty = $core$Dict$empty;

const $core$Dict$member = (($key) => {
  return (($dict) => {
    const $$try1 = (($core$Dict$get)($key))($dict);
    return ((($$try1)[0] === "Just")
      ? true
      : ((($$try1)[0] === "Nothing")
        ? false
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 42:2', (sp_toHuman)($$try1))));
  });
});

const $core$Set$member = $core$Dict$member;

const $core$List$repeat = (($n) => {
  return (($a) => {
    const $rec = (($c) => {
      return (($acc) => {
        return (($c > 0)
          ? (($rec)(($c - 1)))(((sp_cons)($acc))($a))
          : $acc);
      });
    });
    return (($rec)($n))($core$Core$Nil);
  });
});

const $core$Text$repeat = (($n) => {
  return (($s) => {
    return (($core$Text$join)(""))((($core$List$repeat)($n))($s));
  });
});

const $core$Text$padLeft = (($minLength) => {
  return (($pad) => {
    return (($s) => {
      const $textLength = (text_length)($s);
      return (($textLength < $minLength)
        ? ((() => {
          const $times = ((sp_divide)((text_length)($pad)))(($textLength - $minLength));
          return ((($core$Text$repeat)($times))($pad) + $s);
        }))()
        : $s);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$fmtBlock = (($start) => {
  return (($highlights) => {
    return (($ls) => {
      const $$highlightedLines = ((($core$List$for)($highlights))($$home$nw$stuff$unstable$src$Compiler$Error$highlightSplit))(({
        first: $core$Dict$empty,
        second: $core$Set$empty,
      }));
      const $highlightedLines = $$highlightedLines.second;
      const $highlightedWords = $$highlightedLines.first;
      const $pad = (text_length)((text_fromNumber)(($start + ($core$List$length)($ls))));
      const $wordHighlight = (($lineNumber) => {
        const $$try1 = (($core$Dict$get)($lineNumber))($highlightedWords);
        return ((($$try1)[0] === "Nothing")
          ? ""
          : ((($$try1)[0] === "Just")
            ? ((() => {
              const $s = ($$try1)[1].first;
              const $e = ($$try1)[1].second;
              return ("\n" + ((($core$Text$repeat)($pad))(" ") + ("   " + ((($core$Text$repeat)(($s - 1)))(" ") + ($$home$nw$stuff$unstable$src$Compiler$Error$warn)((($core$Text$repeat)((($core$Basics$max)(1))(($e - $s))))("^"))))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 162:8', (sp_toHuman)($$try1))));
      });
      const $lineDem = (($lineIndex) => {
        return ((($core$Set$member)($lineIndex))($highlightedLines)
          ? ($$home$nw$stuff$unstable$src$Compiler$Error$warn)(" > ")
          : " | ");
      });
      const $fmtLine = (($i) => {
        return (($line) => {
          const $index = ($i + $start);
          const $s = ((($core$Text$padLeft)($pad))(" "))((text_fromNumber)($index));
          return ($s + (($lineDem)($index) + ($line + ($wordHighlight)($index))));
        });
      });
      return ((($s) => {
        return ($s + "\n");
      }))((($core$Text$join)("\n"))((($core$List$indexedMap)($fmtLine))($ls)));
    });
  });
});

const $core$Basics$clamp = (($low) => {
  return (($high) => {
    return (($n) => {
      return (($n < $low)
        ? $low
        : (($n > $high)
          ? $high
          : $n));
    });
  });
});

const $core$List$drop = (($n) => {
  return (($ls) => {
    return (((sp_equal)(0))($n)
      ? $ls
      : ((($ls)[0] === "Nil")
        ? $core$Core$Nil
        : ((($ls)[0] === "Cons")
          ? ((() => {
            const $h = ($ls)[1];
            const $tail = ($ls)[2];
            return (($core$List$drop)(($n - 1)))($tail);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 362:6', (sp_toHuman)($ls)))));
  });
});

const $core$List$takeReverse = (($n) => {
  return (($list) => {
    return (($kept) => {
      return (($n < 1)
        ? $kept
        : ((($list)[0] === "Nil")
          ? $kept
          : ((($list)[0] === "Cons")
            ? ((() => {
              const $x = ($list)[1];
              const $xs = ($list)[2];
              return ((($core$List$takeReverse)(($n - 1)))($xs))((($core$Core$Cons)($x))($kept));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 273:4', (sp_toHuman)($list)))));
    });
  });
});

const $core$List$takeTailRec = (($n) => {
  return (($list) => {
    return ($core$List$reverse)(((($core$List$takeReverse)($n))($list))($core$Core$Nil));
  });
});

const $core$List$takeFast = (($ctr) => {
  return (($n) => {
    return (($list) => {
      return (($n < 1)
        ? $core$Core$Nil
        : ((() => {
          const $$try1 = ({
            first: $n,
            second: $list,
          });
          return ((($$try1.second)[0] === "Nil")
            ? $list
            : (((1 === $$try1.first) && (($$try1.second)[0] === "Cons"))
              ? ((() => {
                const $x = ($$try1.second)[1];
                return (($core$Core$Cons)($x))($core$Core$Nil);
              }))()
              : (((2 === $$try1.first) && ((($$try1.second)[0] === "Cons") && ((($$try1.second)[2])[0] === "Cons")))
                ? ((() => {
                  const $x = ($$try1.second)[1];
                  const $y = (($$try1.second)[2])[1];
                  return (($core$Core$Cons)($x))((($core$Core$Cons)($y))($core$Core$Nil));
                }))()
                : (((3 === $$try1.first) && ((($$try1.second)[0] === "Cons") && (((($$try1.second)[2])[0] === "Cons") && (((($$try1.second)[2])[2])[0] === "Cons"))))
                  ? ((() => {
                    const $x = ($$try1.second)[1];
                    const $y = (($$try1.second)[2])[1];
                    const $z = ((($$try1.second)[2])[2])[1];
                    return (($core$Core$Cons)($x))((($core$Core$Cons)($y))((($core$Core$Cons)($z))($core$Core$Nil)));
                  }))()
                  : (((($$try1.second)[0] === "Cons") && (((($$try1.second)[2])[0] === "Cons") && ((((($$try1.second)[2])[2])[0] === "Cons") && ((((($$try1.second)[2])[2])[2])[0] === "Cons"))))
                    ? ((() => {
                      const $x = ($$try1.second)[1];
                      const $y = (($$try1.second)[2])[1];
                      const $z = ((($$try1.second)[2])[2])[1];
                      const $w = (((($$try1.second)[2])[2])[2])[1];
                      const $tl = (((($$try1.second)[2])[2])[2])[2];
                      const $cons = $core$Core$Cons;
                      return (($ctr > 1000)
                        ? (($cons)($x))((($cons)($y))((($cons)($z))((($cons)($w))((($core$List$takeTailRec)(($n - 4)))($tl)))))
                        : (($cons)($x))((($cons)($y))((($cons)($z))((($cons)($w))(((($core$List$takeFast)(($ctr + 1)))(($n - 4)))($tl))))));
                    }))()
                    : (true
                      ? $list
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 239:4', (sp_toHuman)($$try1))))))));
        }))());
    });
  });
});

const $core$List$take = ($core$List$takeFast)(0);

const $$home$nw$stuff$unstable$src$Compiler$Error$showCodeBlock = (($code) => {
  return (($start) => {
    return (($end) => {
      return (($end.line < 0)
        ? ""
        : ((() => {
          const $highlight = (((sp_not_equal)($end.line))($start.line)
            ? ($$home$nw$stuff$unstable$src$Compiler$Error$HighlightBlock)(({
              lineEnd: $end.line,
              lineStart: $start.line,
            }))
            : ($$home$nw$stuff$unstable$src$Compiler$Error$HighlightWord)(({
              colEnd: $end.col,
              colStart: $start.col,
              line: $start.line,
            })));
          const $extraLines = 2;
          const $lines = ((text_split)("\n"))($code);
          const $maxLines = ($core$List$length)($lines);
          const $startLine = ((($core$Basics$clamp)(0))(($maxLines - 1)))(($start.line - ($extraLines - 1)));
          const $endLine = ((($core$Basics$clamp)(0))(($maxLines - 1)))(($end.line + $extraLines));
          const $size = (($core$Basics$max)(1))(($endLine - $startLine));
          return ((($$home$nw$stuff$unstable$src$Compiler$Error$fmtBlock)(($startLine + 1)))((($core$Core$Cons)($highlight))($core$Core$Nil)))((($core$List$take)($size))((($core$List$drop)($startLine))($lines)));
        }))());
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$posToHuman = (($eEnv) => {
  return (($pos) => {
    const $noBlock = (($loc) => {
      return ({
        block: "",
        location: $loc,
      });
    });
    return ((($pos)[0] === "P")
      ? ((() => {
        const $moduleName = ($pos)[1];
        const $startAsInt = ($pos)[2];
        const $endAsInt = ($pos)[3];
        const $$try2 = (($core$Dict$get)($moduleName))($eEnv.moduleByName);
        return ((($$try2)[0] === "Just")
          ? ((() => {
            const $mod = ($$try2)[1];
            const $start = (($$home$nw$stuff$unstable$src$Compiler$Error$positionToLineAndColumn)($mod.content))($startAsInt);
            const $end = (($$home$nw$stuff$unstable$src$Compiler$Error$positionToLineAndColumn)($mod.content))($endAsInt);
            return ({
              block: ((($$home$nw$stuff$unstable$src$Compiler$Error$showCodeBlock)($mod.content))($start))($end),
              location: ($mod.fsPath + (" " + ((text_fromNumber)($start.line) + (":" + (text_fromNumber)($start.col))))),
            });
          }))()
          : ((($$try2)[0] === "Nothing")
            ? ($noBlock)(("<The module name is `" + ($moduleName + "` but I can't find it. This as a compiler bug.>")))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 252:12', (sp_toHuman)($$try2))));
      }))()
      : ((($pos)[0] === "End")
        ? ((() => {
          const $moduleName = ($pos)[1];
          const $$try1 = (($core$Dict$get)($moduleName))($eEnv.moduleByName);
          return ((($$try1)[0] === "Just")
            ? ((() => {
              const $mod = ($$try1)[1];
              const $end = (($$home$nw$stuff$unstable$src$Compiler$Error$positionToLineAndColumn)($mod.content))(((text_length)($mod.content) - 1));
              const $start = ({
                col: 0,
                line: ($end.line - 8),
              });
              return ({
                block: ((($$home$nw$stuff$unstable$src$Compiler$Error$showCodeBlock)($mod.content))($start))($end),
                location: ($mod.fsPath + (" " + ((text_fromNumber)($end.line) + ":0 (end of file)"))),
              });
            }))()
            : ((($$try1)[0] === "Nothing")
              ? ($noBlock)(("<The module name is `" + ($moduleName + "` but I can't find it. This as a compiler bug.>")))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 268:12', (sp_toHuman)($$try1))));
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
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Error.sp 250:4', (sp_toHuman)($pos)))))))));
  });
});

const $core$List$concatMap = (($f) => {
  return (($list) => {
    return ($core$List$concat)((($core$List$map)($f))($list));
  });
});

const $core$Text$padRight = (($minLength) => {
  return (($pad) => {
    return (($s) => {
      const $textLength = (text_length)($s);
      return (($textLength < $minLength)
        ? ((() => {
          const $times = ((sp_divide)((text_length)($pad)))(($textLength - $minLength));
          return ($s + (($core$Text$repeat)($times))($pad));
        }))()
        : $s);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$toText = (($env) => {
  return (($pos) => {
    return (($desc) => {
      const $$block = (($$home$nw$stuff$unstable$src$Compiler$Error$posToHuman)($env))($pos);
      const $location = $$block.location;
      const $block = $$block.block;
      const $description = (($core$Text$join)("\n"))((($core$List$map)((($s) => {
        return ("  " + $s);
      })))((($core$List$concatMap)((text_split)("\n")))(((($d) => {
        return ((sp_cons)($d))($block);
      }))(($desc)($env)))));
      return ($$home$nw$stuff$unstable$src$Compiler$Error$breakDownText)((($core$Text$join)("\n"))((($core$Core$Cons)(""))((($core$Core$Cons)(""))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Error$deco)(((($core$Text$padRight)(50))("-"))(($location + " ")))))((($core$Core$Cons)(""))((($core$Core$Cons)($description))((($core$Core$Cons)(""))($core$Core$Nil))))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Error$toFormattedText = (($eenv) => {
  return (($e) => {
    const $newline = ($$home$nw$stuff$unstable$src$Compiler$Error$FormattedText_Default)("");
    const $tupleToFormattedText = (($x) => {
      const $$descr = $x;
      const $descr = $$descr.second;
      const $pos = $$descr.first;
      return ((($$home$nw$stuff$unstable$src$Compiler$Error$toText)($eenv))($pos))($descr);
    });
    return (($core$List$concatMap)($tupleToFormattedText))((($$home$nw$stuff$unstable$src$Compiler$Error$flatten)($e))($core$Core$Nil));
  });
});

const $$home$nw$stuff$unstable$src$Compile$resToIo = (($errorEnv) => {
  return (($res) => {
    return ((($res)[0] === "Ok")
      ? ((() => {
        const $a = ($res)[1];
        return ($$home$nw$stuff$unstable$lib$posix$IO$succeed)($a);
      }))()
      : ((($res)[0] === "Err")
        ? ((() => {
          const $e = ($res)[1];
          return ($$home$nw$stuff$unstable$lib$posix$IO$fail)((($core$Text$join)(""))((($core$List$map)($$home$nw$stuff$unstable$src$Compile$formattedToConsoleColoredText))((($$home$nw$stuff$unstable$src$Compiler$Error$toFormattedText)($errorEnv))($e))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 28:4', (sp_toHuman)($res))));
  });
});

const $$home$nw$stuff$unstable$src$ModulesFile$initModulesFile = ({
  libraries: $core$Core$Nil,
  sourceDirs: $core$Core$Nil,
});

const $$home$nw$stuff$unstable$src$SPON$posEnd = ($$home$nw$stuff$unstable$src$Types$Pos$End)("");

const $$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos = (($statement) => {
  return ((($statement)[0] === "Evaluation")
    ? ((() => {
      const $pos = ($statement)[1];
      return $pos;
    }))()
    : ((($statement)[0] === "Definition")
      ? ((() => {
        const $pos = ($statement)[1];
        return $pos;
      }))()
      : (((($statement)[0] === "TypeAlias") && ((($statement)[1].name)[0] === "At"))
        ? ((() => {
          const $args = ($statement)[1].args;
          const $pos = (($statement)[1].name)[1];
          const $ty = ($statement)[1].ty;
          return $pos;
        }))()
        : ((($statement)[0] === "UnionDef")
          ? ((() => {
            const $pos = ($statement)[1];
            return $pos;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 116:4', (sp_toHuman)($statement))))));
});

const $$home$nw$stuff$unstable$src$SPON$field = (($fieldName) => {
  return (($fieldReader) => {
    return (($statements) => {
      return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Definition") && ((((($statements)[1])[2].pattern)[0] === "PatternAny") && (!(((($statements)[1])[2].pattern)[2]) && ((((($statements)[1])[2].pattern)[4])[0] === "Nothing")))))
        ? ((() => {
          const $pos = (($statements)[1])[1];
          const $body = (($statements)[1])[2].body;
          const $modifier = (($statements)[1])[2].modifier;
          const $nonFn = (($statements)[1])[2].nonFn;
          const $name = ((($statements)[1])[2].pattern)[3];
          const $tail = ($statements)[2];
          return (((sp_equal)($fieldName))($name)
            ? ((() => {
              const $$try1 = ($fieldReader)($body);
              return ((($$try1)[0] === "Accepted")
                ? ((() => {
                  const $unreadStatements = ($$try1)[1];
                  const $a = ($$try1)[2];
                  return ((($unreadStatements)[0] === "Nil")
                    ? (($$home$nw$stuff$unstable$src$SPON$Accepted)($tail))($a)
                    : ((($unreadStatements)[0] === "Cons")
                      ? ((() => {
                        const $head = ($unreadStatements)[1];
                        return ($$home$nw$stuff$unstable$src$SPON$Failed)((($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($head)))(("Could not make sense of all the statements in field `" + ($fieldName + "`."))));
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 202:24', (sp_toHuman)($unreadStatements))));
                }))()
                : (true
                  ? ((() => {
                    const $otherwise = $$try1;
                    return $otherwise;
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 200:16', (sp_toHuman)($$try1))));
            }))()
            : ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)($pos))(("expecting `" + ($fieldName + " =`")))));
        }))()
        : ((($statements)[0] === "Cons")
          ? ((() => {
            const $head = ($statements)[1];
            const $tail = ($statements)[2];
            return ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($head)))("missing a simple assignment (ie `something = `)"));
          }))()
          : ((($statements)[0] === "Nil")
            ? ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$SPON$posEnd))("unexpected end of file"))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 197:4', (sp_toHuman)($statements)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$SPON$lowerOrUpperName = (($statements) => {
  return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Evaluation") && ((((($statements)[1])[2])[0] === "Variable") && (((((($statements)[1])[2])[2])[0] === "Nothing") && ((((($statements)[1])[2])[4])[0] === "Nil")))))
    ? ((() => {
      const $pos = ((($statements)[1])[2])[1];
      const $name = ((($statements)[1])[2])[3];
      const $tail = ($statements)[2];
      return (($$home$nw$stuff$unstable$src$SPON$Accepted)($tail))($name);
    }))()
    : (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Evaluation") && ((((($statements)[1])[2])[0] === "Constructor") && ((((($statements)[1])[2])[2])[0] === "Nothing"))))
      ? ((() => {
        const $pos = ((($statements)[1])[2])[1];
        const $name = ((($statements)[1])[2])[3];
        const $tail = ($statements)[2];
        return (($$home$nw$stuff$unstable$src$SPON$Accepted)($tail))($name);
      }))()
      : (((($statements)[0] === "Cons") && ((($statements)[2])[0] === "Nil"))
        ? ((() => {
          const $s = ($statements)[1];
          return ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($s)))("expecting an Uppercase or lowercase name"));
        }))()
        : (true
          ? ($$home$nw$stuff$unstable$src$SPON$Failed)((($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$SPON$posEnd))("expecting a single statement"))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 111:4', (sp_toHuman)($statements))))));
});

const $$home$nw$stuff$unstable$src$SPON$many = (($readerA) => {
  const $rec = (($accum) => {
    return (($statements) => {
      return (((sp_equal)($core$Core$Nil))($statements)
        ? (($$home$nw$stuff$unstable$src$SPON$Accepted)($core$Core$Nil))(($core$List$reverse)($accum))
        : ((() => {
          const $$try1 = ($readerA)($statements);
          return ((($$try1)[0] === "Accepted")
            ? ((() => {
              const $tail = ($$try1)[1];
              const $a = ($$try1)[2];
              return (($rec)(((sp_cons)($accum))($a)))($tail);
            }))()
            : ((($$try1)[0] === "Rejected")
              ? ((() => {
                const $e = ($$try1)[1];
                return ($$home$nw$stuff$unstable$src$SPON$Rejected)($e);
              }))()
              : ((($$try1)[0] === "Failed")
                ? ((() => {
                  const $e = ($$try1)[1];
                  return ($$home$nw$stuff$unstable$src$SPON$Failed)($e);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 168:12', (sp_toHuman)($$try1)))));
        }))());
    });
  });
  return ($rec)($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$SPON$maybe = (($readerA) => {
  return (($statements) => {
    const $$try1 = ($readerA)($statements);
    return ((($$try1)[0] === "Accepted")
      ? ((() => {
        const $tail = ($$try1)[1];
        const $a = ($$try1)[2];
        return (($$home$nw$stuff$unstable$src$SPON$Accepted)($tail))(($core$Maybe$Just)($a));
      }))()
      : ((($$try1)[0] === "Rejected")
        ? (($$home$nw$stuff$unstable$src$SPON$Accepted)($statements))($core$Maybe$Nothing)
        : ((($$try1)[0] === "Failed")
          ? ((() => {
            const $r = ($$try1)[1];
            return ($$home$nw$stuff$unstable$src$SPON$Failed)($r);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 183:4', (sp_toHuman)($$try1)))));
  });
});

const $$home$nw$stuff$unstable$src$SPON$onAcc = (($chainedReaderB) => {
  return (($readerA) => {
    return (($statements) => {
      const $$try1 = ($readerA)($statements);
      return ((($$try1)[0] === "Accepted")
        ? ((() => {
          const $newStatements = ($$try1)[1];
          const $a = ($$try1)[2];
          return (($chainedReaderB)($a))($newStatements);
        }))()
        : ((($$try1)[0] === "Rejected")
          ? ((() => {
            const $reason = ($$try1)[1];
            return ($$home$nw$stuff$unstable$src$SPON$Rejected)($reason);
          }))()
          : ((($$try1)[0] === "Failed")
            ? ((() => {
              const $reason = ($$try1)[1];
              return ($$home$nw$stuff$unstable$src$SPON$Failed)($reason);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 14:4', (sp_toHuman)($$try1)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$SPON$return = (($a) => {
  return (($statements) => {
    return (($$home$nw$stuff$unstable$src$SPON$Accepted)($statements))($a);
  });
});

const $$home$nw$stuff$unstable$src$SPON$upperName = (($statements) => {
  return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Evaluation") && ((((($statements)[1])[2])[0] === "Constructor") && ((((($statements)[1])[2])[2])[0] === "Nothing"))))
    ? ((() => {
      const $pos = ((($statements)[1])[2])[1];
      const $name = ((($statements)[1])[2])[3];
      const $tail = ($statements)[2];
      return (($$home$nw$stuff$unstable$src$SPON$Accepted)($tail))($name);
    }))()
    : (((($statements)[0] === "Cons") && ((($statements)[2])[0] === "Nil"))
      ? ((() => {
        const $s = ($statements)[1];
        return ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($s)))("expecting an Uppercase name"));
      }))()
      : (true
        ? ($$home$nw$stuff$unstable$src$SPON$Failed)((($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$SPON$posEnd))("expecting a statement"))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 98:4', (sp_toHuman)($statements)))));
});

const $$home$nw$stuff$unstable$src$ModulesFile$moduleReader = (($$home$nw$stuff$unstable$src$SPON$onAcc)((($path) => {
  return (($$home$nw$stuff$unstable$src$SPON$onAcc)((($visibleAs) => {
    return (($$home$nw$stuff$unstable$src$SPON$onAcc)((($globalTypes) => {
      return (($$home$nw$stuff$unstable$src$SPON$onAcc)((($globalValues) => {
        return ($$home$nw$stuff$unstable$src$SPON$return)(({
          globalTypes: (($core$Maybe$withDefault)($core$Core$Nil))($globalTypes),
          globalValues: (($core$Maybe$withDefault)($core$Core$Nil))($globalValues),
          path: $path,
          visibleAs: (($core$Maybe$withDefault)($path))($visibleAs),
        }));
      })))(($$home$nw$stuff$unstable$src$SPON$maybe)((($$home$nw$stuff$unstable$src$SPON$field)("globalValues"))(($$home$nw$stuff$unstable$src$SPON$many)($$home$nw$stuff$unstable$src$SPON$lowerOrUpperName))));
    })))(($$home$nw$stuff$unstable$src$SPON$maybe)((($$home$nw$stuff$unstable$src$SPON$field)("globalTypes"))(($$home$nw$stuff$unstable$src$SPON$many)($$home$nw$stuff$unstable$src$SPON$upperName))));
  })))(($$home$nw$stuff$unstable$src$SPON$maybe)((($$home$nw$stuff$unstable$src$SPON$field)("importAs"))($$home$nw$stuff$unstable$src$SPON$upperName)));
})))((($$home$nw$stuff$unstable$src$SPON$field)("path"))($$home$nw$stuff$unstable$src$SPON$upperName));

const $$home$nw$stuff$unstable$src$SPON$text = (($statements) => {
  return (((($statements)[0] === "Cons") && (((($statements)[1])[0] === "Evaluation") && ((((($statements)[1])[2])[0] === "LiteralText") && ((($statements)[2])[0] === "Nil"))))
    ? ((() => {
      const $pos = ((($statements)[1])[2])[1];
      const $t = ((($statements)[1])[2])[2];
      return (($$home$nw$stuff$unstable$src$SPON$Accepted)($core$Core$Nil))($t);
    }))()
    : (((($statements)[0] === "Cons") && ((($statements)[2])[0] === "Nil"))
      ? ((() => {
        const $s = ($statements)[1];
        return ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($s)))("expecting a text literal"));
      }))()
      : (true
        ? ($$home$nw$stuff$unstable$src$SPON$Failed)((($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$SPON$posEnd))("expecting a single statement"))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 85:4', (sp_toHuman)($statements)))));
});

const $$home$nw$stuff$unstable$src$ModulesFile$libraryReader = (($$home$nw$stuff$unstable$src$SPON$onAcc)((($source) => {
  return (($$home$nw$stuff$unstable$src$SPON$onAcc)((($modules) => {
    return ($$home$nw$stuff$unstable$src$SPON$return)(({
      modules: $modules,
      source: $source,
    }));
  })))(($$home$nw$stuff$unstable$src$SPON$many)((($$home$nw$stuff$unstable$src$SPON$field)("module"))($$home$nw$stuff$unstable$src$ModulesFile$moduleReader)));
})))((($$home$nw$stuff$unstable$src$SPON$field)("source"))($$home$nw$stuff$unstable$src$SPON$text));

const $$home$nw$stuff$unstable$src$ModulesFile$sourceDirectoryReader = (($$home$nw$stuff$unstable$src$SPON$onAcc)((($path) => {
  return (($$home$nw$stuff$unstable$src$SPON$onAcc)((($modules) => {
    return ($$home$nw$stuff$unstable$src$SPON$return)(({
      modules: $modules,
      path: $path,
    }));
  })))(($$home$nw$stuff$unstable$src$SPON$many)((($$home$nw$stuff$unstable$src$SPON$field)("module"))($$home$nw$stuff$unstable$src$ModulesFile$moduleReader)));
})))((($$home$nw$stuff$unstable$src$SPON$field)("path"))($$home$nw$stuff$unstable$src$SPON$text));

const $$home$nw$stuff$unstable$src$SPON$oneOf = (($readers) => {
  return (($statements) => {
    return ((($readers)[0] === "Nil")
      ? ((() => {
        const $pos = ((($statements)[0] === "Cons")
          ? ((() => {
            const $head = ($statements)[1];
            return ($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($head);
          }))()
          : (true
            ? $$home$nw$stuff$unstable$src$SPON$posEnd
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 136:16', (sp_toHuman)($statements))));
        return ($$home$nw$stuff$unstable$src$SPON$Rejected)((($$home$nw$stuff$unstable$src$Types$Pos$At)($pos))("options exhausted"));
      }))()
      : ((($readers)[0] === "Cons")
        ? ((() => {
          const $headReader = ($readers)[1];
          const $tail = ($readers)[2];
          const $$try1 = ($headReader)($statements);
          return ((($$try1)[0] === "Rejected")
            ? (($$home$nw$stuff$unstable$src$SPON$oneOf)($tail))($statements)
            : (true
              ? ((() => {
                const $otherwise = $$try1;
                return $otherwise;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 143:12', (sp_toHuman)($$try1))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 133:4', (sp_toHuman)($readers))));
  });
});

const $$home$nw$stuff$unstable$src$ModulesFile$modulesFileReader = ($$home$nw$stuff$unstable$src$SPON$many)(($$home$nw$stuff$unstable$src$SPON$oneOf)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$SPON$onAcc)((($lib) => {
  return ($$home$nw$stuff$unstable$src$SPON$return)(($$home$nw$stuff$unstable$src$ModulesFile$Lib)($lib));
})))((($$home$nw$stuff$unstable$src$SPON$field)("library"))($$home$nw$stuff$unstable$src$ModulesFile$libraryReader))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$SPON$onAcc)((($dir) => {
  return ($$home$nw$stuff$unstable$src$SPON$return)(($$home$nw$stuff$unstable$src$ModulesFile$Dir)($dir));
})))((($$home$nw$stuff$unstable$src$SPON$field)("sourceDir"))($$home$nw$stuff$unstable$src$ModulesFile$sourceDirectoryReader))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$Lexer$getPos = (($state) => {
  return (sp_clone)(($state.obj)[$state.attr].buffer.nextPos);
});

const $core$List$each = (($ls) => {
  return (($f) => {
    return ((($ls)[0] === "Nil")
      ? null
      : ((($ls)[0] === "Cons")
        ? ((() => {
          const $h = ($ls)[1];
          const $tail = ($ls)[2];
          ($f)($h);
          return (($core$List$each)($tail))($f);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 332:4', (sp_toHuman)($ls))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$closeOpenBlocks = (($state) => {
  const $pos = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
  return (($core$List$each)((sp_clone)(($state.obj)[$state.attr].indentStack)))((() => {
    return ((array_push)(({
      attr: "tokens",
      obj: ($state.obj)[$state.attr],
    })))(((($$home$nw$stuff$unstable$src$Types$Token$Token)($pos))($pos))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd));
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addIndentToken = (($pos) => {
  return (($kind) => {
    return (($state) => {
      return ((array_push)(({
        attr: "tokens",
        obj: ($state.obj)[$state.attr],
      })))(((($$home$nw$stuff$unstable$src$Types$Token$Token)($pos))($pos))($kind));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$updateIndent = (($start) => {
  return (($end) => {
    return (($kind) => {
      return (($state) => {
        const $manageIndent = (($head) => {
          return (((sp_clone)(($state.obj)[$state.attr].lineIndent) > $head.indent)
            ? ((() => {
              const $newIndent = ({
                indent: (sp_clone)(($state.obj)[$state.attr].lineIndent),
                isBlock: (sp_clone)(($state.obj)[$state.attr].indentStartsABlock),
              });
              (($state.obj)[$state.attr].indentStack = ((sp_cons)((sp_clone)(($state.obj)[$state.attr].indentStack)))($newIndent));
              return ((sp_clone)(($state.obj)[$state.attr].indentStartsABlock)
                ? ((($$home$nw$stuff$unstable$src$Compiler$Lexer$addIndentToken)($start))($$home$nw$stuff$unstable$src$Types$Token$BlockStart))($state)
                : null);
            }))()
            : (($head.isBlock && ((sp_not_equal)($$home$nw$stuff$unstable$src$Types$Token$Comment))($kind))
              ? ((($$home$nw$stuff$unstable$src$Compiler$Lexer$addIndentToken)($start))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine))($state)
              : null));
        });
        const $$try1 = (sp_clone)(($state.obj)[$state.attr].indentStack);
        return ((($$try1)[0] === "Cons")
          ? ((() => {
            const $head = ($$try1)[1];
            const $tail = ($$try1)[2];
            return (((sp_clone)(($state.obj)[$state.attr].lineIndent) < $head.indent)
              ? ((() => {
                (($state.obj)[$state.attr].indentStack = $tail);
                ($head.isBlock
                  ? ((($$home$nw$stuff$unstable$src$Compiler$Lexer$addIndentToken)($start))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd))($state)
                  : null);
                return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$updateIndent)($start))($end))($kind))($state);
              }))()
              : ($manageIndent)($head));
          }))()
          : ((($$try1)[0] === "Nil")
            ? ($manageIndent)(({
              indent: 0,
              isBlock: true,
            }))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 145:4', (sp_toHuman)($$try1))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken = (($start) => {
  return (($end) => {
    return (($kind) => {
      return (($state) => {
        ((sp_clone)(($state.obj)[$state.attr].soFarThereAreNoTokensInThisLine)
          ? ((() => {
            (($state.obj)[$state.attr].soFarThereAreNoTokensInThisLine = false);
            return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$updateIndent)($start))($end))($kind))($state);
          }))()
          : null);
        const $indentStartsABlock = ((($kind)[0] === "Then")
          ? true
          : ((($kind)[0] === "Else")
            ? true
            : ((($kind)[0] === "As")
              ? true
              : ((($kind)[0] === "Colon")
                ? true
                : ((($kind)[0] === "MutableColon")
                  ? true
                  : ((($kind)[0] === "Defop")
                    ? true
                    : ((($kind)[0] === "Comment")
                      ? (sp_clone)(($state.obj)[$state.attr].indentStartsABlock)
                      : (true
                        ? false
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 174:8', (sp_toHuman)($kind))))))))));
        (($state.obj)[$state.attr].indentStartsABlock = $indentStartsABlock);
        ((array_push)(({
          attr: "tokens",
          obj: ($state.obj)[$state.attr],
        })))(((($$home$nw$stuff$unstable$src$Types$Token$Token)($start))($end))($kind));
        return (($state.obj)[$state.attr].tokenStart = $end);
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addError = (($message) => {
  return (($state) => {
    const $end = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
    const $error = (($$home$nw$stuff$unstable$src$Compiler$Error$Simple)(((($$home$nw$stuff$unstable$src$Types$Pos$P)((sp_clone)(($state.obj)[$state.attr].moduleName)))((sp_clone)(($state.obj)[$state.attr].tokenStart)))($end)))((() => {
      return (($core$Core$Cons)($message))($core$Core$Nil);
    }));
    (($state.obj)[$state.attr].errors = ((sp_cons)((sp_clone)(($state.obj)[$state.attr].errors)))($error));
    return (($state.obj)[$state.attr].tokenStart = $end);
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Buffer$slice = (($start) => {
  return (($end) => {
    return (($b) => {
      return (((text_slice)($start))($end))($b.fullText);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$getChunk = (($state) => {
  const $start = (sp_clone)(($state.obj)[$state.attr].tokenStart);
  const $end = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
  return ({
    first: $start,
    second: $end,
    third: ((($$home$nw$stuff$unstable$src$SPLib$Buffer$slice)((sp_clone)(($state.obj)[$state.attr].tokenStart)))($end))((sp_clone)(($state.obj)[$state.attr].buffer)),
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addNumberToken = (($state) => {
  const $$chunk = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getChunk)($state);
  const $chunk = $$chunk.third;
  const $end = $$chunk.second;
  const $start = $$chunk.first;
  return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)($chunk)))($state);
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$relAddToken = (($ds) => {
  return (($de) => {
    return (($kind) => {
      return (($state) => {
        const $pos = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
        return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)(($pos + $ds)))(($pos + $de)))($kind))($state);
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addParenOrCommaToken = (($char) => {
  return (($state) => {
    const $add = (($kind) => {
      return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$relAddToken)(0))(1))($kind))($state);
    });
    return (("(" === $char)
      ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Open))
      : ((")" === $char)
        ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Closed))
        : (("[" === $char)
          ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open))
          : (("]" === $char)
            ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Closed))
            : (("{" === $char)
              ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$CurlyBrace)($$home$nw$stuff$unstable$src$Types$Token$Open))
              : (("}" === $char)
                ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$CurlyBrace)($$home$nw$stuff$unstable$src$Types$Token$Closed))
                : (("," === $char)
                  ? ($add)($$home$nw$stuff$unstable$src$Types$Token$Comma)
                  : (true
                    ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)(("I can't make sense of this piece of text: `" + ($char + "`"))))($state)
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 456:4', (sp_toHuman)($char))))))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$p = $$home$nw$stuff$unstable$src$Types$Pos$N;

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$defToType = (($def) => {
  return (($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$p))($def.usr);
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$umr = (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("Core");

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$umr);

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$numberDef = ({
  args: $core$Core$Nil,
  constructors: $core$Dict$empty,
  directTypeDeps: $core$Set$empty,
  usr: ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("Number"),
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$number = (($$home$nw$stuff$unstable$src$Compiler$CoreTypes$defToType)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$numberDef))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Prelude$numberUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("Number"));

const $$home$nw$stuff$unstable$src$Prelude$tyFun = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($$home$nw$stuff$unstable$src$Types$Pos$N);

const $$home$nw$stuff$unstable$src$Prelude$typeBinop = (($mutates) => {
  return (($left) => {
    return (($right) => {
      return (($return) => {
        return ((($$home$nw$stuff$unstable$src$Prelude$tyFun)($right))(false))(((($$home$nw$stuff$unstable$src$Prelude$tyFun)($left))($mutates))($return));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Prelude$typeBinopUniform = (($ty) => {
  return (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))($ty))($ty))($ty);
});

const $$home$nw$stuff$unstable$src$Prelude$add = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Addittive,
  symbol: "+",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("add"),
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$nameToType = (($name) => {
  return (($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$p))(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)($name));
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool = (($$home$nw$stuff$unstable$src$Compiler$CoreTypes$nameToType)("Bool"))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Prelude$coreUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("Core"));

const $$home$nw$stuff$unstable$src$Prelude$and_ = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Logical,
  symbol: "and",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("and_"),
});

const $$home$nw$stuff$unstable$src$Prelude$divide = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Multiplicative,
  symbol: "/",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("divide"),
});

const $$home$nw$stuff$unstable$src$Prelude$tyVar = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($$home$nw$stuff$unstable$src$Types$Pos$N);

const $$home$nw$stuff$unstable$src$Prelude$equal = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Comparison,
  symbol: "==",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("equal"),
});

const $$home$nw$stuff$unstable$src$Prelude$greaterOrEqualThan = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Comparison,
  symbol: ">=",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("greaterOrEqualThan"),
});

const $$home$nw$stuff$unstable$src$Prelude$greaterThan = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Comparison,
  symbol: ">",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("greaterThan"),
});

const $$home$nw$stuff$unstable$src$Prelude$lesserOrEqualThan = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Comparison,
  symbol: "<=",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("lesserOrEqualThan"),
});

const $$home$nw$stuff$unstable$src$Prelude$lesserThan = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Comparison,
  symbol: "<",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("lesserThan"),
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$list = (($item) => {
  return (($$home$nw$stuff$unstable$src$Compiler$CoreTypes$nameToType)("List"))((($core$Core$Cons)($item))($core$Core$Nil));
});

const $$home$nw$stuff$unstable$src$Prelude$listUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("List"));

const $$home$nw$stuff$unstable$src$Prelude$listCons = ((() => {
  const $item = ($$home$nw$stuff$unstable$src$Prelude$tyVar)("item");
  return ({
    associativity: $$home$nw$stuff$unstable$src$Types$Op$Right,
    nonFn: $core$Core$Nil,
    precedence: $$home$nw$stuff$unstable$src$Types$Op$Cons,
    symbol: "::",
    type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))($item))(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($item)))(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($item)),
    usr: ($$home$nw$stuff$unstable$src$Prelude$listUsr)("stack"),
  });
}))();

const $$home$nw$stuff$unstable$src$Prelude$multiply = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Multiplicative,
  symbol: "*",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("multiply"),
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneName = "None";

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$none = (($$home$nw$stuff$unstable$src$Compiler$CoreTypes$nameToType)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneName))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Prelude$mutableAdd = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$NonAssociative,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Mutop,
  symbol: "+=",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(true))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("mutableAdd"),
});

const $$home$nw$stuff$unstable$src$Prelude$mutableAssign = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Mutop,
  symbol: ":=",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(true))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("mutableAssign"),
});

const $$home$nw$stuff$unstable$src$Prelude$mutableSubtract = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$NonAssociative,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Mutop,
  symbol: "-=",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(true))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("mutableSubtract"),
});

const $$home$nw$stuff$unstable$src$Prelude$notEqual = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Comparison,
  symbol: "/=",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("notEqual"),
});

const $$home$nw$stuff$unstable$src$Prelude$or_ = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Logical,
  symbol: "or",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("or_"),
});

const $$home$nw$stuff$unstable$src$Prelude$sendLeft = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Pipe,
  symbol: "<<",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(((($$home$nw$stuff$unstable$src$Prelude$tyFun)(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("b"))))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("b")),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("sendLeft"),
});

const $$home$nw$stuff$unstable$src$Prelude$sendRight = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Pipe,
  symbol: ">>",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(((($$home$nw$stuff$unstable$src$Prelude$tyFun)(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("b"))))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("b")),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("sendRight"),
});

const $$home$nw$stuff$unstable$src$Prelude$subtract = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Left,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Addittive,
  symbol: "-",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("subtract"),
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$textDef = ({
  args: $core$Core$Nil,
  constructors: $core$Dict$empty,
  directTypeDeps: $core$Set$empty,
  usr: ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("Text"),
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$text = (($$home$nw$stuff$unstable$src$Compiler$CoreTypes$defToType)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$textDef))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Prelude$textUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("Text"));

const $$home$nw$stuff$unstable$src$Prelude$textConcat = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$Right,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Addittive,
  symbol: "..",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeBinopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text),
  usr: ($$home$nw$stuff$unstable$src$Prelude$textUsr)("concat"),
});

const $$home$nw$stuff$unstable$src$Prelude$tupleUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("Tuple"));

const $$home$nw$stuff$unstable$src$Prelude$tuple = ({
  associativity: $$home$nw$stuff$unstable$src$Types$Op$NonAssociative,
  nonFn: $core$Core$Nil,
  precedence: $$home$nw$stuff$unstable$src$Types$Op$Tuple,
  symbol: "&",
  type: (((($$home$nw$stuff$unstable$src$Prelude$typeBinop)(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("b")))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($$home$nw$stuff$unstable$src$Types$Pos$N))($core$Maybe$Nothing))(((($core$Dict$insert)("second"))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("b")))(((($core$Dict$insert)("first"))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))($core$Dict$empty)))),
  usr: ($$home$nw$stuff$unstable$src$Prelude$tupleUsr)("pair_$$$$"),
});

const $$home$nw$stuff$unstable$src$Prelude$binops = (($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$and_))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$or_))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$textConcat))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$listCons))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$tuple))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$add))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$subtract))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$multiply))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$divide))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$mutableAssign))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$mutableAdd))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$mutableSubtract))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$equal))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$notEqual))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$lesserThan))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$greaterThan))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$lesserOrEqualThan))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$greaterOrEqualThan))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$sendRight))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$sendLeft))($core$Core$Nil))))))))))))))))))));

const $$home$nw$stuff$unstable$src$Prelude$binopsBySymbol = ((($core$List$for)($$home$nw$stuff$unstable$src$Prelude$binops))((($bop) => {
  return (($core$Dict$insert)($bop.symbol))($bop);
})))($core$Dict$empty);

const $$home$nw$stuff$unstable$src$Prelude$typeUnopUniform = (($type) => {
  return ((($$home$nw$stuff$unstable$src$Prelude$tyFun)($type))(false))($type);
});

const $$home$nw$stuff$unstable$src$Prelude$unaryMinus = ({
  symbol: "0 -",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeUnopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("unaryMinus"),
});

const $$home$nw$stuff$unstable$src$Prelude$unaryPlus = ({
  symbol: "0 +",
  type: ($$home$nw$stuff$unstable$src$Prelude$typeUnopUniform)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
  usr: ($$home$nw$stuff$unstable$src$Prelude$numberUsr)("unaryPlus"),
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addSquiggleToken = (($nextIsSpace) => {
  return (($state) => {
    const $$chunk = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getChunk)($state);
    const $chunk = $$chunk.third;
    const $end = $$chunk.second;
    const $start = $$chunk.first;
    const $add = (($kind) => {
      return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))($kind))($state);
    });
    return ((":" === $chunk)
      ? ($add)($$home$nw$stuff$unstable$src$Types$Token$Colon)
      : (("@:" === $chunk)
        ? ($add)($$home$nw$stuff$unstable$src$Types$Token$MutableColon)
        : (("=" === $chunk)
          ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))
          : (("@=" === $chunk)
            ? ($add)(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefMutable))
            : (("-" === $chunk)
              ? ($add)(($nextIsSpace
                ? ($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$subtract)
                : ($$home$nw$stuff$unstable$src$Types$Token$Unop)($$home$nw$stuff$unstable$src$Prelude$unaryMinus)))
              : (("+" === $chunk)
                ? ($add)(($nextIsSpace
                  ? ($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$add)
                  : ($$home$nw$stuff$unstable$src$Types$Token$Unop)($$home$nw$stuff$unstable$src$Prelude$unaryPlus)))
                : (true
                  ? ((() => {
                    const $op = $chunk;
                    const $$try1 = (($core$Dict$get)($chunk))($$home$nw$stuff$unstable$src$Prelude$binopsBySymbol);
                    return ((($$try1)[0] === "Just")
                      ? ((() => {
                        const $binop = ($$try1)[1];
                        return ($add)(($$home$nw$stuff$unstable$src$Types$Token$Binop)($binop));
                      }))()
                      : ((($$try1)[0] === "Nothing")
                        ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)(("Invalid operator: `" + ($chunk + "`"))))($state)
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 440:12', (sp_toHuman)($$try1))));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 432:4', (sp_toHuman)($chunk)))))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$startsWithUpperChar = (($s) => {
  const $$try1 = ((text_startsWithRegex)("[A-Z]"))($s);
  return (("" === $$try1)
    ? false
    : (true
      ? true
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 264:4', (sp_toHuman)($$try1))));
});

const $core$List$any = (($fun) => {
  return (($list) => {
    return ((($list)[0] === "Nil")
      ? false
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $h = ($list)[1];
          const $t = ($list)[2];
          return (($fun)($h)
            ? true
            : (($core$List$any)($fun))($t));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 6:4', (sp_toHuman)($list))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addLowerOrUpperWord = (($start) => {
  return (($end) => {
    return (($modifier) => {
      return (($chunk) => {
        return (($state) => {
          const $upperName = (($maybeModule) => {
            return (($name) => {
              return ((($modifier)[0] === "NameNoModifier")
                ? (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))((($$home$nw$stuff$unstable$src$Types$Token$UpperName)($maybeModule))($name)))($state)
                : ((($modifier)[0] === "NameStartsWithDot")
                  ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("Types or constructors can't start with `.` and attribute names can't start with an uppercase letter. =|"))($state)
                  : ((($modifier)[0] === "NameMutable")
                    ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("Types or constructors can't be mutable on their own, only variables can!"))($state)
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 274:8', (sp_toHuman)($modifier)))));
            });
          });
          const $lowerName = (($maybeModule) => {
            return (($name) => {
              return (($attrs) => {
                return ((($core$List$any)($$home$nw$stuff$unstable$src$Compiler$Lexer$startsWithUpperChar))($attrs)
                  ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("attribute names must start with a lowercase letter"))($state)
                  : ((((sp_not_equal)($core$Maybe$Nothing))($maybeModule) && ((sp_not_equal)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($modifier))
                    ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("can't use . or @ modifier on an imported value"))($state)
                    : (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($modifier))($maybeModule))($name))($attrs)))($state)));
              });
            });
          });
          const $snips = ((text_split)("."))($chunk);
          return ((($core$List$any)((($s) => {
            return ((sp_equal)(""))($s);
          })))($snips)
            ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("use spaces around `..` to concatenate Text"))($state)
            : ((($snips)[0] === "Nil")
              ? (sp_todo)("should not happen")
              : (((($snips)[0] === "Cons") && ((($snips)[2])[0] === "Nil"))
                ? ((() => {
                  const $one = ($snips)[1];
                  return (($$home$nw$stuff$unstable$src$Compiler$Lexer$startsWithUpperChar)($one)
                    ? (($upperName)($core$Maybe$Nothing))($one)
                    : ((($lowerName)($core$Maybe$Nothing))($one))($core$Core$Nil));
                }))()
                : (((($snips)[0] === "Cons") && ((($snips)[2])[0] === "Cons"))
                  ? ((() => {
                    const $first = ($snips)[1];
                    const $second = (($snips)[2])[1];
                    const $more = (($snips)[2])[2];
                    const $$try1 = ({
                      first: ($$home$nw$stuff$unstable$src$Compiler$Lexer$startsWithUpperChar)($first),
                      second: ($$home$nw$stuff$unstable$src$Compiler$Lexer$startsWithUpperChar)($second),
                    });
                    return ((!($$try1.first) && !($$try1.second))
                      ? ((($lowerName)($core$Maybe$Nothing))($first))(((sp_cons)($more))($second))
                      : (($$try1.first && !($$try1.second))
                        ? ((($lowerName)(($core$Maybe$Just)($first)))($second))($more)
                        : (($$try1.first && $$try1.second)
                          ? (((sp_not_equal)($core$Core$Nil))($more)
                            ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("Types and constructors can't have .attributes"))($state)
                            : (($upperName)(($core$Maybe$Just)($first)))($second))
                          : ((!($$try1.first) && $$try1.second)
                            ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("Something wrong with uppercases?"))($state)
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 319:12', (sp_toHuman)($$try1))))));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 300:6', (sp_toHuman)($snips))))));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addWordToken = (($modifier) => {
  return (($state) => {
    const $start = (sp_clone)(($state.obj)[$state.attr].tokenStart);
    const $end = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
    const $ds = (((sp_equal)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($modifier)
      ? 0
      : 1);
    const $chunk = ((($$home$nw$stuff$unstable$src$SPLib$Buffer$slice)(((sp_clone)(($state.obj)[$state.attr].tokenStart) + $ds)))($end))((sp_clone)(($state.obj)[$state.attr].buffer));
    const $maybeKeywordKind = (("if" === $chunk)
      ? ($core$Maybe$Just)($$home$nw$stuff$unstable$src$Types$Token$If)
      : (("then" === $chunk)
        ? ($core$Maybe$Just)($$home$nw$stuff$unstable$src$Types$Token$Then)
        : (("else" === $chunk)
          ? ($core$Maybe$Just)($$home$nw$stuff$unstable$src$Types$Token$Else)
          : (("try" === $chunk)
            ? ($core$Maybe$Just)($$home$nw$stuff$unstable$src$Types$Token$Try)
            : (("as" === $chunk)
              ? ($core$Maybe$Just)($$home$nw$stuff$unstable$src$Types$Token$As)
              : (("with" === $chunk)
                ? ($core$Maybe$Just)($$home$nw$stuff$unstable$src$Types$Token$With)
                : (("and" === $chunk)
                  ? ($core$Maybe$Just)(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$and_))
                  : (("or" === $chunk)
                    ? ($core$Maybe$Just)(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$or_))
                    : (true
                      ? $core$Maybe$Nothing
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 356:8', (sp_toHuman)($chunk)))))))))));
    const $$try1 = ({
      first: $maybeKeywordKind,
      second: $modifier,
    });
    return (((($$try1.first)[0] === "Just") && (($$try1.second)[0] === "NameNoModifier"))
      ? ((() => {
        const $kind = ($$try1.first)[1];
        return (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))($kind))($state);
      }))()
      : ((($$try1.first)[0] === "Just")
        ? ((() => {
          const $kind = ($$try1.first)[1];
          return (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)(($chunk + " as a keyword, you can't really use it this way")))($state);
        }))()
        : (true
          ? ((((($$home$nw$stuff$unstable$src$Compiler$Lexer$addLowerOrUpperWord)($start))($end))($modifier))($chunk))($state)
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 367:4', (sp_toHuman)($$try1)))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$isNumber = (($char) => {
  return ((sp_not_equal)(""))(((text_startsWithRegex)("[0-9_.]"))($char));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$isSquiggle = (($char) => {
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
                    : (("&" === $char)
                      ? true
                      : (("^" === $char)
                        ? true
                        : (true
                          ? false
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 408:4', (sp_toHuman)($char))))))))))))));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$isWordBody = (($char) => {
  return ((sp_not_equal)(""))(((text_startsWithRegex)("[a-zA-Z./_0-9]"))($char));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$isWordStart = (($char) => {
  return ((sp_not_equal)(""))(((text_startsWithRegex)("[a-zA-Z._]"))($char));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$setMode = (($mode) => {
  return (($state) => {
    return (($state.obj)[$state.attr].mode = $mode);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$tryIndent = (($indentChar) => {
  return (($char) => {
    return (($state) => {
      return ((((sp_equal)($indentChar))($char) || ((sp_equal)(""))($char))
        ? null
        : ((((sp_equal)(" "))($char) || ((sp_equal)("\t"))($char))
          ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("mixing tabs and spaces!"))($state)
          : (((sp_equal)("\n"))($char)
            ? ((() => {
              (($state.obj)[$state.attr].tokenStart = (($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state) + 1));
              return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Indent))($state);
            }))()
            : (((sp_equal)("#"))($char)
              ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$LineComment))($state)
              : ((() => {
                (($state.obj)[$state.attr].lineIndent = (sp_clone)(($state.obj)[$state.attr].column));
                (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
              }))()))));
    });
  });
});

const $core$Basics$not = (($b) => {
  return ($b
    ? false
    : true);
});

const $core$List$filter = (($f) => {
  return (($ls) => {
    return ((($core$List$forReversed)($ls))((($item) => {
      return (($acc) => {
        return (($f)($item)
          ? ((sp_cons)($acc))($item)
          : $acc);
      });
    })))($core$Core$Nil);
  });
});

const $core$Basics$min = (($a) => {
  return (($b) => {
    return (($a < $b)
      ? $a
      : $b);
  });
});

const $core$List$minimum = (($list) => {
  return ((($list)[0] === "Cons")
    ? ((() => {
      const $x = ($list)[1];
      const $xs = ($list)[2];
      return ($core$Maybe$Just)(((($core$List$for)($xs))($core$Basics$min))($x));
    }))()
    : (true
      ? $core$Maybe$Nothing
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 370:4', (sp_toHuman)($list))));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$unindent = (($raw) => {
  return (($core$Basics$not)(((text_startsWith)("\n"))($raw))
    ? $raw
    : ((() => {
      const $multilineText = ((text_dropLeft)(1))($raw);
      const $lines = ((text_split)("\n"))($multilineText);
      const $countLeadingSpaces = (($line) => {
        return (text_length)(((text_startsWithRegex)("[ ]*"))($line));
      });
      const $minLead = (($core$Maybe$withDefault)(0))(($core$List$minimum)((($core$List$map)($countLeadingSpaces))((($core$List$filter)((($s) => {
        return ((sp_not_equal)(""))((text_trimLeft)($s));
      })))($lines))));
      return (((text_replaceRegex)("\n[ ]*$"))(""))((($core$Text$join)("\n"))((($core$List$map)((text_dropLeft)($minLead)))($lines)));
    }))());
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne = (($char) => {
  return (($state) => {
    const $pos = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
    const $$try1 = (sp_clone)(($state.obj)[$state.attr].mode);
    return ((($$try1)[0] === "Indent")
      ? ((() => {
        const $$try3 = (sp_clone)(($state.obj)[$state.attr].tabsOrSpaces);
        return ((($$try3)[0] === "Tabs")
          ? ((($$home$nw$stuff$unstable$src$Compiler$Lexer$tryIndent)("\t"))($char))($state)
          : ((($$try3)[0] === "Spaces")
            ? ((($$home$nw$stuff$unstable$src$Compiler$Lexer$tryIndent)(" "))($char))($state)
            : ((($$try3)[0] === "NoTabsOrSpacesYet")
              ? ((" " === $char)
                ? ((() => {
                  (($state.obj)[$state.attr].tabsOrSpaces = $$home$nw$stuff$unstable$src$Compiler$Lexer$Spaces);
                  return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                }))()
                : (("\t" === $char)
                  ? ((() => {
                    (($state.obj)[$state.attr].tabsOrSpaces = $$home$nw$stuff$unstable$src$Compiler$Lexer$Tabs);
                    return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                  }))()
                  : (true
                    ? ((($$home$nw$stuff$unstable$src$Compiler$Lexer$tryIndent)(" "))($char))($state)
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 492:14', (sp_toHuman)($char)))))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 484:10', (sp_toHuman)($$try3)))));
      }))()
      : ((($$try1)[0] === "Default")
        ? (("" === $char)
          ? null
          : (("." === $char)
            ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Dot))($state)
            : (("@" === $char)
              ? ((() => {
                (($state.obj)[$state.attr].tokenStart = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state));
                return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Mutable))($state);
              }))()
              : (("#" === $char)
                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$LineComment))($state)
                : (("[" === $char)
                  ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$ContentOpeningBlockComment))($state)
                  : (("\"" === $char)
                    ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$ContentOpeningQuotes_One))($state)
                    : (("\n" === $char)
                      ? ((() => {
                        (($state.obj)[$state.attr].tokenStart = (($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state) + 1));
                        (($state.obj)[$state.attr].soFarThereAreNoTokensInThisLine = true);
                        return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Indent))($state);
                      }))()
                      : ((" " === $char)
                        ? (($state.obj)[$state.attr].tokenStart = (($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state) + 1))
                        : (true
                          ? ((() => {
                            (($state.obj)[$state.attr].tokenStart = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state));
                            return (($$home$nw$stuff$unstable$src$Compiler$Lexer$isWordStart)($char)
                              ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$Word)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier)))($state)
                              : (($$home$nw$stuff$unstable$src$Compiler$Lexer$isNumber)($char)
                                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$NumberLiteral))($state)
                                : (($$home$nw$stuff$unstable$src$Compiler$Lexer$isSquiggle)($char)
                                  ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Squiggles))($state)
                                  : (($$home$nw$stuff$unstable$src$Compiler$Lexer$addParenOrCommaToken)($char))($state))));
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 503:10', (sp_toHuman)($char)))))))))))
        : ((($$try1)[0] === "Dot")
          ? (((sp_equal)("."))($char)
            ? ((() => {
              (((($$home$nw$stuff$unstable$src$Compiler$Lexer$relAddToken)((0 - 1)))(1))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$textConcat)))($state);
              return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
            }))()
            : (($$home$nw$stuff$unstable$src$Compiler$Lexer$isWordStart)($char)
              ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$Word)($$home$nw$stuff$unstable$src$Types$Token$NameStartsWithDot)))($state)
              : (($$home$nw$stuff$unstable$src$Compiler$Lexer$isNumber)($char)
                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$NumberLiteral))($state)
                : (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("no idea what this is"))($state))))
          : ((($$try1)[0] === "Mutable")
            ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$isWordStart)($char)
              ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$Word)($$home$nw$stuff$unstable$src$Types$Token$NameMutable)))($state)
              : (($$home$nw$stuff$unstable$src$Compiler$Lexer$isSquiggle)($char)
                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Squiggles))($state)
                : (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("no idea what this is"))($state)))
            : ((($$try1)[0] === "Word")
              ? ((() => {
                const $modifier = ($$try1)[1];
                return (($$home$nw$stuff$unstable$src$Compiler$Lexer$isWordBody)($char)
                  ? null
                  : ((() => {
                    (($$home$nw$stuff$unstable$src$Compiler$Lexer$addWordToken)($modifier))($state);
                    (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                    return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                  }))());
              }))()
              : ((($$try1)[0] === "NumberLiteral")
                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$isNumber)($char)
                  ? null
                  : ((() => {
                    ($$home$nw$stuff$unstable$src$Compiler$Lexer$addNumberToken)($state);
                    (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                    return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                  }))())
                : ((($$try1)[0] === "Squiggles")
                  ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$isSquiggle)($char)
                    ? null
                    : ((() => {
                      (($$home$nw$stuff$unstable$src$Compiler$Lexer$addSquiggleToken)(((sp_equal)(" "))($char)))($state);
                      (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                      return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                    }))())
                  : ((($$try1)[0] === "ContentOpeningQuotes_One")
                    ? (((sp_equal)("\""))($char)
                      ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$ContentOpeningQuotes_Two))($state)
                      : (((sp_equal)(""))($char)
                        ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("there's no closing quotes"))($state)
                        : ((() => {
                          (($state.obj)[$state.attr].tokenStart = (($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state) - 1));
                          (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$SingleQuote)(({
                            lastEscape: -(1),
                          }))))($state);
                          return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                        }))()))
                    : ((($$try1)[0] === "ContentOpeningQuotes_Two")
                      ? (((sp_equal)("\""))($char)
                        ? ((() => {
                          (($state.obj)[$state.attr].tokenStart = (($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state) - 2));
                          return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$TripleQuote)(({
                            closingQuotes: 0,
                            lastEscape: -(1),
                          }))))($state);
                        }))()
                        : ((() => {
                          (((($$home$nw$stuff$unstable$src$Compiler$Lexer$relAddToken)((0 - 2)))(0))(($$home$nw$stuff$unstable$src$Types$Token$TextLiteral)("")))($state);
                          (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                          return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                        }))())
                      : ((($$try1)[0] === "SingleQuote")
                        ? ((() => {
                          const $lastEscape = ($$try1)[1].lastEscape;
                          const $previousIsEscape = ((sp_equal)(($lastEscape + 1)))($pos);
                          return (((sp_equal)(""))($char)
                            ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("there's no closing quotes"))($state)
                            : ($previousIsEscape
                              ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$SingleQuote)(({
                                lastEscape: $lastEscape,
                              }))))($state)
                              : (("\"" === $char)
                                ? ((() => {
                                  const $start = (sp_clone)(($state.obj)[$state.attr].tokenStart);
                                  const $end = ($pos + 1);
                                  const $value = ((($core$Text$replace)("\\\""))("\""))(((($$home$nw$stuff$unstable$src$SPLib$Buffer$slice)(($start + 1)))(($end - 1)))((sp_clone)(($state.obj)[$state.attr].buffer)));
                                  (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))(($$home$nw$stuff$unstable$src$Types$Token$TextLiteral)($value)))($state);
                                  return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                                }))()
                                : (("\\" === $char)
                                  ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$SingleQuote)(({
                                    lastEscape: $pos,
                                  }))))($state)
                                  : (true
                                    ? null
                                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 629:12', (sp_toHuman)($char)))))));
                        }))()
                        : ((($$try1)[0] === "TripleQuote")
                          ? ((() => {
                            const $closingQuotes = ($$try1)[1].closingQuotes;
                            const $lastEscape = ($$try1)[1].lastEscape;
                            const $previousIsEscape = ((sp_equal)(($lastEscape + 1)))($pos);
                            return (((sp_equal)(""))($char)
                              ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("unterminated triple quotes"))($state)
                              : ($previousIsEscape
                                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$TripleQuote)(({
                                  closingQuotes: 0,
                                  lastEscape: $lastEscape,
                                }))))($state)
                                : (("\"" === $char)
                                  ? (((sp_equal)(2))($closingQuotes)
                                    ? ((() => {
                                      const $start = (sp_clone)(($state.obj)[$state.attr].tokenStart);
                                      const $end = ($pos + 1);
                                      const $value = ($$home$nw$stuff$unstable$src$Compiler$Lexer$unindent)(((($$home$nw$stuff$unstable$src$SPLib$Buffer$slice)(($start + 3)))(($end - 3)))((sp_clone)(($state.obj)[$state.attr].buffer)));
                                      (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)($start))($end))(($$home$nw$stuff$unstable$src$Types$Token$TextLiteral)($value)))($state);
                                      return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                                    }))()
                                    : (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$TripleQuote)(({
                                      closingQuotes: ($closingQuotes + 1),
                                      lastEscape: $lastEscape,
                                    }))))($state))
                                  : (("\\" === $char)
                                    ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$TripleQuote)(({
                                      closingQuotes: 0,
                                      lastEscape: $pos,
                                    }))))($state)
                                    : (true
                                      ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$TripleQuote)(({
                                        closingQuotes: 0,
                                        lastEscape: $lastEscape,
                                      }))))($state)
                                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 669:13', (sp_toHuman)($char)))))));
                          }))()
                          : ((($$try1)[0] === "LineComment")
                            ? ((((sp_equal)("\n"))($char) || ((sp_equal)(""))($char))
                              ? ((() => {
                                (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)((sp_clone)(($state.obj)[$state.attr].tokenStart)))(($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state)))($$home$nw$stuff$unstable$src$Types$Token$Comment))($state);
                                (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                                return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                              }))()
                              : null)
                            : ((($$try1)[0] === "ContentOpeningBlockComment")
                              ? (((sp_equal)("#"))($char)
                                ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$BlockComment)(({
                                  nesting: 1,
                                  previous: "",
                                }))))($state)
                                : ((() => {
                                  (((($$home$nw$stuff$unstable$src$Compiler$Lexer$relAddToken)((0 - 1)))(0))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open)))($state);
                                  (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                                  return (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
                                }))())
                              : ((($$try1)[0] === "BlockComment")
                                ? ((() => {
                                  const $nesting = ($$try1)[1].nesting;
                                  const $previous = ($$try1)[1].previous;
                                  const $continueWithDeltaNesting = (($dn) => {
                                    return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)(($$home$nw$stuff$unstable$src$Compiler$Lexer$BlockComment)(({
                                      nesting: ($nesting + $dn),
                                      previous: $char,
                                    }))))($state);
                                  });
                                  const $$try2 = ({
                                    first: $previous,
                                    second: $char,
                                  });
                                  return ((("[" === $$try2.first) && ("#" === $$try2.second))
                                    ? ($continueWithDeltaNesting)(1)
                                    : ((("#" === $$try2.first) && ("]" === $$try2.second))
                                      ? (($nesting > 1)
                                        ? ($continueWithDeltaNesting)((0 - 1))
                                        : ((() => {
                                          (((($$home$nw$stuff$unstable$src$Compiler$Lexer$absAddToken)((sp_clone)(($state.obj)[$state.attr].tokenStart)))(($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state)))($$home$nw$stuff$unstable$src$Types$Token$Comment))($state);
                                          return (($$home$nw$stuff$unstable$src$Compiler$Lexer$setMode)($$home$nw$stuff$unstable$src$Compiler$Lexer$Default))($state);
                                        }))())
                                      : (("" === $$try2.second)
                                        ? (($$home$nw$stuff$unstable$src$Compiler$Lexer$addError)("unterminated block comment"))($state)
                                        : (true
                                          ? ($continueWithDeltaNesting)(0)
                                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 713:10', (sp_toHuman)($$try2))))));
                                }))()
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Lexer.sp 481:4', (sp_toHuman)($$try1))))))))))))))));
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Buffer$init = (($s) => {
  return ({
    fullSize: (text_length)($s),
    fullText: $s,
    nextPos: 0,
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$readStateInit = (($moduleName) => {
  return (($moduleCode) => {
    return ({
      buffer: ($$home$nw$stuff$unstable$src$SPLib$Buffer$init)($moduleCode),
      column: 0,
      errors: $core$Core$Nil,
      indentStack: $core$Core$Nil,
      indentStartsABlock: true,
      line: 0,
      lineIndent: 0,
      mode: $$home$nw$stuff$unstable$src$Compiler$Lexer$Indent,
      moduleName: $moduleName,
      soFarThereAreNoTokensInThisLine: true,
      tabsOrSpaces: $$home$nw$stuff$unstable$src$Compiler$Lexer$NoTabsOrSpacesYet,
      tokenStart: 0,
      tokens: (array_fromList)($core$Core$Nil),
    });
  });
});

const $core$Basics$btw = (($f) => {
  return (($a) => {
    return (($c) => {
      ($f)($a);
      return $c;
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer$lexer = (($moduleName) => {
  return (($moduleCode) => {
    (sp_benchStart)(null);
    const $state = ({
      attr: "$",
      obj: ({
        $: (sp_clone)((($$home$nw$stuff$unstable$src$Compiler$Lexer$readStateInit)($moduleName))($moduleCode)),
      }),
    });
    ((text_forEach)($moduleCode))((($char) => {
      (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)($char))($state);
      (($state.obj)[$state.attr].buffer.nextPos += 1);
      return (((sp_equal)("\n"))($char)
        ? ((() => {
          (($state.obj)[$state.attr].line += 1);
          return (($state.obj)[$state.attr].column = 0);
        }))()
        : (($state.obj)[$state.attr].column += 1));
    }));
    (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexOne)(""))($state);
    return (((sp_equal)($core$Core$Nil))((sp_clone)(($state.obj)[$state.attr].errors))
      ? ((() => {
        ($$home$nw$stuff$unstable$src$Compiler$Lexer$closeOpenBlocks)($state);
        return ((($core$Basics$btw)(sp_benchStop))("lexer"))(($core$Result$Ok)((array_toList)((sp_clone)(($state.obj)[$state.attr].tokens))));
      }))()
      : ((($core$Basics$btw)(sp_benchStop))("lexer"))(($core$Result$Err)(($$home$nw$stuff$unstable$src$Compiler$Error$Nested)((sp_clone)(($state.obj)[$state.attr].errors)))));
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$andThen = (($chainedParser) => {
  return (($firstParser) => {
    return (($re0) => {
      return (($readState) => {
        const $$try1 = (($firstParser)($re0))($readState);
        return ((($$try1.second)[0] === "Accepted")
          ? ((() => {
            const $re1 = $$try1.first;
            const $nextReadState = ($$try1.second)[1];
            const $a = ($$try1.second)[2];
            return ((($chainedParser)($a))($re1))($nextReadState);
          }))()
          : ((($$try1.second)[0] === "Rejected")
            ? ((() => {
              const $re1 = $$try1.first;
              return ({
                first: $re1,
                second: $$home$nw$stuff$unstable$src$SPLib$Parser$Rejected,
              });
            }))()
            : ((($$try1.second)[0] === "Aborted")
              ? ((() => {
                const $re1 = $$try1.first;
                const $rs = ($$try1.second)[1];
                const $e = ($$try1.second)[2];
                return ({
                  first: $re1,
                  second: (($$home$nw$stuff$unstable$src$SPLib$Parser$Aborted)($rs))($e),
                });
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 85:4', (sp_toHuman)($$try1)))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$andThen = $$home$nw$stuff$unstable$src$SPLib$Parser$andThen;

const $$home$nw$stuff$unstable$src$SPLib$Parser$consumeOne = (($rejections) => {
  return (($readState) => {
    return ((($readState)[0] === "Nil")
      ? ({
        first: ((sp_cons)($rejections))($readState),
        second: $$home$nw$stuff$unstable$src$SPLib$Parser$Rejected,
      })
      : ((($readState)[0] === "Cons")
        ? ((() => {
          const $token = ($readState)[1];
          const $nextState = ($readState)[2];
          return ({
            first: $rejections,
            second: (($$home$nw$stuff$unstable$src$SPLib$Parser$Accepted)($nextState))($token),
          });
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 69:4', (sp_toHuman)($readState))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$oneToken = $$home$nw$stuff$unstable$src$SPLib$Parser$consumeOne;

const $$home$nw$stuff$unstable$src$SPLib$Parser$accept = (($a) => {
  return (($rejections) => {
    return (($readState) => {
      return ({
        first: $rejections,
        second: (($$home$nw$stuff$unstable$src$SPLib$Parser$Accepted)($readState))($a),
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$reject = (($rejections) => {
  return (($readState) => {
    return ({
      first: ((sp_cons)($rejections))($readState),
      second: $$home$nw$stuff$unstable$src$SPLib$Parser$Rejected,
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$kind = (($targetKind) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($token) => {
    const $$k = $token;
    const $k = ($$k)[3];
    return (((sp_equal)($k))($targetKind)
      ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($token)
      : $$home$nw$stuff$unstable$src$SPLib$Parser$reject);
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst = (($a) => {
  return (($b) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
      return $b;
    })))($a);
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$thenWithDefault = (($fallbackParser) => {
  return (($chainedParser) => {
    return (($firstParser) => {
      return (($re0) => {
        return (($readState) => {
          const $$try1 = (($firstParser)($re0))($readState);
          return ((($$try1.second)[0] === "Aborted")
            ? ((() => {
              const $re1 = $$try1.first;
              const $rs = ($$try1.second)[1];
              const $reason = ($$try1.second)[2];
              return ({
                first: $re1,
                second: (($$home$nw$stuff$unstable$src$SPLib$Parser$Aborted)($rs))($reason),
              });
            }))()
            : ((($$try1.second)[0] === "Rejected")
              ? ((() => {
                const $re1 = $$try1.first;
                return (($fallbackParser)($re1))($readState);
              }))()
              : ((($$try1.second)[0] === "Accepted")
                ? ((() => {
                  const $re1 = $$try1.first;
                  const $nextReadState = ($$try1.second)[1];
                  const $a = ($$try1.second)[2];
                  return ((($chainedParser)($a))($re1))($nextReadState);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 100:4', (sp_toHuman)($$try1)))));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore = (($p) => {
  return ((($$home$nw$stuff$unstable$src$SPLib$Parser$thenWithDefault)(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($core$Core$Nil)))((($head) => {
    return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($tail) => {
      return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((sp_cons)($tail))($head));
    })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($p));
  })))($p);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$oomSeparatedBy = (($sep) => {
  return (($pa) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($head) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($tail) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((sp_cons)($tail))($head));
      })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)((($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)($sep))($pa)));
    })))($pa);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$defop = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($token) => {
  return (((($token)[0] === "Token") && ((($token)[3])[0] === "Defop"))
    ? ((() => {
      const $mod = (($token)[3])[1];
      return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($mod);
    }))()
    : (true
      ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 194:4', (sp_toHuman)($token))));
})))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);

const $$home$nw$stuff$unstable$src$SPLib$Parser$here = (($rejections) => {
  return (($readState) => {
    return ({
      first: $rejections,
      second: (($$home$nw$stuff$unstable$src$SPLib$Parser$Accepted)($readState))($readState),
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$here = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($tokens) => {
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($tokens)[0] === "Cons") && ((($tokens)[1])[0] === "Token"))
    ? ((() => {
      const $mod = (($tokens)[1])[1];
      const $start = (($tokens)[1])[2];
      const $end = (($tokens)[1])[3];
      const $rest = ($tokens)[2];
      return $start;
    }))()
    : ((($tokens)[0] === "Nil")
      ? 0
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 47:9', (sp_toHuman)($tokens)))));
})))($$home$nw$stuff$unstable$src$SPLib$Parser$here);

const $$home$nw$stuff$unstable$src$SPLib$Parser$surroundWith = (($left) => {
  return (($right) => {
    return (($parser) => {
      return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((() => {
        return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($p) => {
          return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((() => {
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($p);
          })))($right);
        })))($parser);
      })))($left);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$surroundStrict = (($left) => {
  return (($right) => {
    return (($$home$nw$stuff$unstable$src$SPLib$Parser$surroundWith)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($left)))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($right));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$block = (($$home$nw$stuff$unstable$src$Compiler$Parser$surroundStrict)($$home$nw$stuff$unstable$src$Types$Token$BlockStart))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd);

const $$home$nw$stuff$unstable$src$Compiler$Parser$sib = ($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine));

const $$home$nw$stuff$unstable$src$SPLib$Parser$oneOf = (($ps) => {
  return (($rejections) => {
    return (($readState) => {
      return ((($ps)[0] === "Nil")
        ? ({
          first: $rejections,
          second: $$home$nw$stuff$unstable$src$SPLib$Parser$Rejected,
        })
        : ((($ps)[0] === "Cons")
          ? ((() => {
            const $headParser = ($ps)[1];
            const $tailParsers = ($ps)[2];
            const $$try1 = (($headParser)($rejections))($readState);
            return ((($$try1.second)[0] === "Rejected")
              ? ((() => {
                const $re1 = $$try1.first;
                return ((($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)($tailParsers))($re1))($readState);
              }))()
              : (true
                ? ((() => {
                  const $acceptedOrAborted = $$try1;
                  return $acceptedOrAborted;
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 147:12', (sp_toHuman)($$try1))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 142:4', (sp_toHuman)($ps))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented = (($p) => {
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$block)($p)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$sib)($p)))((($core$Core$Cons)($p))($core$Core$Nil))));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$binaryOperators = (($group) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$e) => {
    const $s = ($$e)[1];
    const $e = ($$e)[2];
    const $k = ($$e)[3];
    return ((($k)[0] === "Binop")
      ? ((() => {
        const $op = ($k)[1];
        return (((sp_equal)($group))($op.precedence)
          ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($op)
          : $$home$nw$stuff$unstable$src$SPLib$Parser$reject);
      }))()
      : (true
        ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1228:4', (sp_toHuman)($k))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$pos = (($env) => {
  return (($start) => {
    return (($end) => {
      return ($env.stripLocations
        ? $$home$nw$stuff$unstable$src$Types$Pos$T
        : ((($$home$nw$stuff$unstable$src$Types$Pos$P)($env.moduleName))($start))($end));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtSep = (($sep) => {
  return (($item) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($sep0) => {
      const $theParserStillSucks = ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$block)((($$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtItem)($sep))($item))))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$sib)((($$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtItem)($sep))($item))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtItem)($sep))($item)))($core$Core$Nil))));
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$item0) => {
        const $item0 = $$item0.first;
        const $tail = $$item0.second;
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((sp_cons)($tail))(({
          first: $sep0,
          second: $item0,
        })));
      })))($theParserStillSucks);
    })))($sep);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtItem = (($sep) => {
  return (($item) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($item0) => {
      const $theParserStillSucks = ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$block)((($$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtSep)($sep))($item))))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$sib)((($$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtSep)($sep))($item))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtSep)($sep))($item)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($core$Core$Nil)))($core$Core$Nil)))));
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($sepsAndItems) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
          first: $item0,
          second: $sepsAndItems,
        }));
      })))($theParserStillSucks);
    })))($item);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$sepList = $$home$nw$stuff$unstable$src$Compiler$Parser$sepListAtItem;

const $$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr = (($env) => {
  return (($group) => {
    return (($higher) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$head) => {
          const $head = $$head.first;
          const $sepTail = $$head.second;
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
            return (((sp_equal)($core$Core$Nil))($sepTail)
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($head)
              : ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((($$home$nw$stuff$unstable$src$Types$FormattableAst$Binop)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($group))(({
                first: $head,
                second: $sepTail,
              }))));
          })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
        })))((($$home$nw$stuff$unstable$src$Compiler$Parser$sepList)(($$home$nw$stuff$unstable$src$Compiler$Parser$binaryOperators)($group)))($higher));
      })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$discardSecond = (($a) => {
  return (($b) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($aa) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($aa);
      })))($b);
    })))($a);
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$tuple2 = (($pa) => {
  return (($pb) => {
    return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($a) => {
      return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($b) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
          first: $a,
          second: $b,
        }));
      })))($pb);
    })))($pa);
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$oneOrMore = (($p) => {
  return (($$home$nw$stuff$unstable$src$SPLib$Parser$tuple2)($p))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($p));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$lambdaBody = (($env) => {
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$h) => {
    const $h = $$h.first;
    const $t = $$h.second;
    return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((sp_cons)($t))($h));
  })))(($$home$nw$stuff$unstable$src$SPLib$Parser$oneOrMore)(($$home$nw$stuff$unstable$src$Compiler$Parser$sib)(($$home$nw$stuff$unstable$src$Compiler$Parser$statement)($env))))))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineStatementOrBlock)($env)))($core$Core$Nil)));
});

const $$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos = (($pa) => {
  return ((($pa)[0] === "PatternAny")
    ? ((() => {
      const $p = ($pa)[1];
      return $p;
    }))()
    : ((($pa)[0] === "PatternLiteralNumber")
      ? ((() => {
        const $p = ($pa)[1];
        return $p;
      }))()
      : ((($pa)[0] === "PatternLiteralText")
        ? ((() => {
          const $p = ($pa)[1];
          return $p;
        }))()
        : ((($pa)[0] === "PatternConstructor")
          ? ((() => {
            const $p = ($pa)[1];
            return $p;
          }))()
          : ((($pa)[0] === "PatternList")
            ? ((() => {
              const $p = ($pa)[1];
              return $p;
            }))()
            : ((($pa)[0] === "PatternListCons")
              ? ((() => {
                const $p = ($pa)[1];
                return $p;
              }))()
              : ((($pa)[0] === "PatternRecord")
                ? ((() => {
                  const $p = ($pa)[1];
                  return $p;
                }))()
                : ((($pa)[0] === "PatternTuple")
                  ? ((() => {
                    const $p = ($pa)[1];
                    return $p;
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 156:4', (sp_toHuman)($pa))))))))));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$lambdaParser = (($env) => {
  return (($mutable) => {
    return (($pa) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($body) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos)($pa)))($pa))($mutable))($body));
      })))(($$home$nw$stuff$unstable$src$Compiler$Parser$lambdaBody)($env));
    });
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$maybe = (($p) => {
  return ((($$home$nw$stuff$unstable$src$SPLib$Parser$thenWithDefault)(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($core$Maybe$Nothing)))((($x) => {
    return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(($core$Maybe$Just)($x));
  })))($p);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$rawList = (($item) => {
  const $sibsep = ($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$Comma));
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)($sibsep)))((($$home$nw$stuff$unstable$src$Compiler$Parser$oomSeparatedBy)($sibsep))($item));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$surroundMultiline = (($left) => {
  return (($right) => {
    return (($content) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($left)))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)((($$home$nw$stuff$unstable$src$Compiler$Parser$discardSecond)($content))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($right)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$list = (($env) => {
  return (($constructor) => {
    return (($main) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeLs) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
            const $theParserStillSucks = ((($maybeLs)[0] === "Just")
              ? ((() => {
                const $ls = ($maybeLs)[1];
                return $ls;
              }))()
              : ((($maybeLs)[0] === "Nothing")
                ? $core$Core$Nil
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 629:8', (sp_toHuman)($maybeLs))));
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($constructor)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($theParserStillSucks));
          })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
        })))(((($$home$nw$stuff$unstable$src$Compiler$Parser$surroundMultiline)(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open)))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Closed)))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$rawList)($main))));
      })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$parens = (($$home$nw$stuff$unstable$src$SPLib$Parser$surroundWith)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Open))))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Closed))));

const $$home$nw$stuff$unstable$src$Compiler$Parser$lowerNameBare = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($token) => {
    return (((($token)[0] === "Token") && (((($token)[3])[0] === "LowerName") && ((((($token)[3])[1])[0] === "NameNoModifier") && ((((($token)[3])[2])[0] === "Nothing") && (((($token)[3])[4])[0] === "Nil")))))
      ? ((() => {
        const $start = ($token)[1];
        const $end = ($token)[2];
        const $name = (($token)[3])[3];
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$Pos$At)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($name));
      }))()
      : (true
        ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 184:4', (sp_toHuman)($token))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$record = (($env) => {
  return (($assign) => {
    return (($constructor) => {
      return (($main) => {
        const $attrAssignment = (($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($assign)))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)($main));
        const $attr = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($name) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeAssignment) => {
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
              first: $name,
              second: $maybeAssignment,
            }));
          })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)($attrAssignment));
        })))(($$home$nw$stuff$unstable$src$Compiler$Parser$lowerNameBare)($env));
        const $updateTarget = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($h) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($h);
          })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$With));
        })))($main);
        const $content = (($start) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeUpdateTarget) => {
            return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($attrs) => {
              return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
                return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($constructor)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))(({
                  attrs: $attrs,
                  extends: $maybeUpdateTarget,
                })));
              })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
            })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$rawList)($attr)));
          })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)($updateTarget));
        });
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($s) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeRecord) => {
            return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($e) => {
              return ((($maybeRecord)[0] === "Just")
                ? ((() => {
                  const $re = ($maybeRecord)[1];
                  return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($re);
                }))()
                : ((($maybeRecord)[0] === "Nothing")
                  ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($constructor)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($s))($e)))(({
                    attrs: $core$Core$Nil,
                    extends: $core$Maybe$Nothing,
                  })))
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 676:4', (sp_toHuman)($maybeRecord))));
            })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
          })))(((($$home$nw$stuff$unstable$src$Compiler$Parser$surroundMultiline)(($$home$nw$stuff$unstable$src$Types$Token$CurlyBrace)($$home$nw$stuff$unstable$src$Types$Token$Open)))(($$home$nw$stuff$unstable$src$Types$Token$CurlyBrace)($$home$nw$stuff$unstable$src$Types$Token$Closed)))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($content)($s))));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeConstructorAppOr = (($env) => {
  return (($higher) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($ty) => {
      return (((($ty)[0] === "TypeConstant") && ((($ty)[4])[0] === "Nil"))
        ? ((() => {
          const $p1 = ($ty)[1];
          const $maybeModule = ($ty)[2];
          const $name = ($ty)[3];
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($args) => {
            return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end2) => {
              return (((sp_equal)($core$Core$Nil))($args)
                ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($ty)
                : ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeConstant)($p1))($maybeModule))($name))($args)));
            })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
          })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($higher));
        }))()
        : (true
          ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($ty)
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 962:4', (sp_toHuman)($ty))));
    })))($higher);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$arrow = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$end) => {
    const $start = ($$end)[1];
    const $end = ($$end)[2];
    const $k = ($$end)[3];
    return ((($k)[0] === "Colon")
      ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
        first: false,
        second: ((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end),
      }))
      : ((($k)[0] === "MutableColon")
        ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
          first: true,
          second: ((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end),
        }))
        : (true
          ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 947:4', (sp_toHuman)($k)))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeFunctionOr = (($env) => {
  return (($higher) => {
    const $arrowAndHigher = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$mutable) => {
      const $mutable = $$mutable.first;
      const $p = $$mutable.second;
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($h) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
          first: $mutable,
          second: $p,
          third: $h,
        }));
      })))($higher);
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$arrow)($env));
    const $fold = (($$nextIsMutable) => {
      const $nextIsMutable = $$nextIsMutable.first;
      const $p = $$nextIsMutable.second;
      const $ty = $$nextIsMutable.third;
      return (($$accum) => {
        const $thisIsMutable = $$accum.first;
        const $accum = $$accum.second;
        return ({
          first: $nextIsMutable,
          second: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction)($p))($ty))($thisIsMutable))($accum),
        });
      });
    });
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($fs) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($e) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($fe) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($es) => {
            const $firstPos = ((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($fs))($fe);
            const $reverseRec = (($a) => {
              return (($ls) => {
                return (($accum) => {
                  return ((($ls)[0] === "Nil")
                    ? ({
                      first: $a,
                      second: $accum,
                    })
                    : ((($ls)[0] === "Cons")
                      ? ((() => {
                        const $head = ($ls)[1];
                        const $tail = ($ls)[2];
                        return ((($reverseRec)($head))($tail))(((sp_cons)($accum))($a));
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 926:8', (sp_toHuman)($ls))));
                });
              });
            });
            const $$p = ((($reverseRec)(({
              first: false,
              second: $firstPos,
              third: $e,
            })))($es))($core$Core$Nil);
            const $reversedArgs = $$p.second;
            const $return = $$p.first.third;
            const $p = $$p.first.second;
            const $thisIsMutable = $$p.first.first;
            return ((($x) => {
              return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($x.second);
            }))(((($core$List$for)($reversedArgs))($fold))(({
              first: $thisIsMutable,
              second: $return,
            })));
          })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($arrowAndHigher));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
      })))($higher);
    })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeList = (($env) => {
  return (($main) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($t) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
          return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeList)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($t));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
      })))(((($$home$nw$stuff$unstable$src$Compiler$Parser$surroundStrict)(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open)))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Closed)))($main));
    })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeParens = (($main) => {
  return ((($$home$nw$stuff$unstable$src$Compiler$Parser$surroundStrict)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Open)))(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Closed)))($main);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeTerm = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$end) => {
    const $start = ($$end)[1];
    const $end = ($$end)[2];
    const $k = ($$end)[3];
    return ((($k)[0] === "UpperName")
      ? ((() => {
        const $maybeModule = ($k)[1];
        const $name = ($k)[2];
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeConstant)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($maybeModule))($name))($core$Core$Nil));
      }))()
      : (((($k)[0] === "LowerName") && (((($k)[1])[0] === "NameNoModifier") && (((($k)[2])[0] === "Nothing") && ((($k)[4])[0] === "Nil"))))
        ? ((() => {
          const $name = ($k)[3];
          return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeVariable)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($name));
        }))()
        : (true
          ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 833:4', (sp_toHuman)($k)))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeTupleOr = (($env) => {
  return (($higher) => {
    const $binopAndPrev = (($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$Compiler$Parser$binaryOperators)($$home$nw$stuff$unstable$src$Types$Op$Tuple)))($higher);
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($head) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($tail) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
            return (((sp_equal)($core$Core$Nil))($tail)
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($head)
              : ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeTuple)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))(((sp_cons)($tail))($head))));
          })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
        })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($binopAndPrev));
      })))($higher);
    })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$breakCircularDefinition = (($a) => {
  return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)($a))(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(null));
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$expression = (($term) => {
  return (($ops) => {
    return ((($ops)[0] === "Nil")
      ? $term
      : ((($ops)[0] === "Cons")
        ? ((() => {
          const $op = ($ops)[1];
          const $rest = ($ops)[2];
          return (($$home$nw$stuff$unstable$src$SPLib$Parser$expression)(($op)($term)))($rest);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPLib/Parser.sp 230:4', (sp_toHuman)($ops))));
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$higherOr = (($parser) => {
  return (($higher) => {
    return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)($higher))((($core$Core$Cons)($parser))($core$Core$Nil)));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeExpr = (($env) => {
  const $nest = ($$home$nw$stuff$unstable$src$SPLib$Parser$breakCircularDefinition)((() => {
    return ($$home$nw$stuff$unstable$src$Compiler$Parser$typeExpr)($env);
  }));
  const $higherOr = $$home$nw$stuff$unstable$src$SPLib$Parser$higherOr;
  return (($$home$nw$stuff$unstable$src$SPLib$Parser$expression)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeTerm)($env)))((($core$Core$Cons)(($higherOr)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeParens)($nest))))((($core$Core$Cons)(($higherOr)((($$home$nw$stuff$unstable$src$Compiler$Parser$typeList)($env))($nest))))((($core$Core$Cons)(($higherOr)((((($$home$nw$stuff$unstable$src$Compiler$Parser$record)($env))($$home$nw$stuff$unstable$src$Types$Token$As))($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeRecord))($nest))))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeConstructorAppOr)($env)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeTupleOr)($env)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeFunctionOr)($env)))($core$Core$Nil)))))));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeAnnotation = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$As)))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeExpr)($env)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$patternApplication = (($env) => {
  return (($param) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$end) => {
      const $start = ($$end)[1];
      const $end = ($$end)[2];
      const $k = ($$end)[3];
      const $p = ((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end);
      return ((($k)[0] === "NumberLiteral")
        ? ((() => {
          const $s = ($k)[1];
          return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternLiteralNumber)($p))($s));
        }))()
        : ((($k)[0] === "TextLiteral")
          ? ((() => {
            const $s = ($k)[1];
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternLiteralText)($p))($s));
          }))()
          : (((($k)[0] === "LowerName") && (((($k)[2])[0] === "Nothing") && ((($k)[4])[0] === "Nil")))
            ? ((() => {
              const $modifier = ($k)[1];
              const $name = ($k)[3];
              const $thingy = (($mutable) => {
                return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeTy) => {
                  return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($p))($mutable))($name))($maybeTy));
                })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeAnnotation)($env))));
              });
              return ((($modifier)[0] === "NameNoModifier")
                ? ($thingy)(false)
                : ((($modifier)[0] === "NameMutable")
                  ? ($thingy)(true)
                  : ((($modifier)[0] === "NameStartsWithDot")
                    ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1080:12', (sp_toHuman)($modifier)))));
            }))()
            : ((($k)[0] === "UpperName")
              ? ((() => {
                const $maybeModule = ($k)[1];
                const $name = ($k)[2];
                return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($params) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end1) => {
                    return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternConstructor)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end1)))($maybeModule))($name))($params));
                  })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
                })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($param));
              }))()
              : (true
                ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1063:4', (sp_toHuman)($k)))))));
    })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$functionParameter = (($env) => {
  return (($nest) => {
    return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$patternApplication)($env))($$home$nw$stuff$unstable$src$SPLib$Parser$reject)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$parens)($nest)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Compiler$Parser$list)($env))($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternList))($nest)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser$record)($env))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal)))($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternRecord))($nest)))($core$Core$Nil)))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$patternBinopOr = (($env) => {
  return (($precedenceGroup) => {
    return (($constructor) => {
      return (($higher) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$head) => {
            const $head = $$head.first;
            const $sepTail = $$head.second;
            return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
              return (((sp_equal)($core$Core$Nil))($sepTail)
                ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($head)
                : ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($constructor)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))(((sp_cons)((($core$List$map)((($x) => {
                  return $x.second;
                })))($sepTail)))($head))));
            })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
          })))((($$home$nw$stuff$unstable$src$Compiler$Parser$sepList)(($$home$nw$stuff$unstable$src$Compiler$Parser$binaryOperators)($precedenceGroup)))($higher));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$pattern = (($env) => {
  const $nest = ($$home$nw$stuff$unstable$src$SPLib$Parser$breakCircularDefinition)((() => {
    return ($$home$nw$stuff$unstable$src$Compiler$Parser$pattern)($env);
  }));
  const $higherOr = $$home$nw$stuff$unstable$src$SPLib$Parser$higherOr;
  return (($$home$nw$stuff$unstable$src$SPLib$Parser$expression)((($$home$nw$stuff$unstable$src$Compiler$Parser$patternApplication)($env))((($$home$nw$stuff$unstable$src$Compiler$Parser$functionParameter)($env))($nest))))((($core$Core$Cons)(($higherOr)(($$home$nw$stuff$unstable$src$Compiler$Parser$parens)($nest))))((($core$Core$Cons)(($higherOr)(((($$home$nw$stuff$unstable$src$Compiler$Parser$list)($env))($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternList))($nest))))((($core$Core$Cons)(($higherOr)((((($$home$nw$stuff$unstable$src$Compiler$Parser$record)($env))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal)))($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternRecord))($nest))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Compiler$Parser$patternBinopOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Cons))($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternListCons)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Compiler$Parser$patternBinopOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Tuple))($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternTuple)))($core$Core$Nil))))));
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$map = (($f) => {
  return (($p) => {
    return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($b) => {
      return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(($f)($b));
    })))($p);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$exprWithLeftDelimiter = (($env) => {
  const $colon = ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$SPLib$Parser$map)((() => {
    return false;
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$Colon))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$SPLib$Parser$map)((() => {
    return true;
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$MutableColon))))($core$Core$Nil)));
  const $maybeColon = ($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)($colon);
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$end) => {
    const $start = ($$end)[1];
    const $end = ($$end)[2];
    const $k = ($$end)[3];
    const $p = ((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end);
    return ((($k)[0] === "NumberLiteral")
      ? ((() => {
        const $s = ($k)[1];
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($mc) => {
          return ((($mc)[0] === "Nothing")
            ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($p))($s))
            : ((($mc)[0] === "Just")
              ? ((() => {
                const $mutable = ($mc)[1];
                return ((($$home$nw$stuff$unstable$src$Compiler$Parser$lambdaParser)($env))($mutable))((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternLiteralNumber)($p))($s));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 478:20', (sp_toHuman)($mc))));
        })))($maybeColon);
      }))()
      : ((($k)[0] === "TextLiteral")
        ? ((() => {
          const $s = ($k)[1];
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($mc) => {
            return ((($mc)[0] === "Nothing")
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralText)($p))($s))
              : ((($mc)[0] === "Just")
                ? ((() => {
                  const $mutable = ($mc)[1];
                  return ((($$home$nw$stuff$unstable$src$Compiler$Parser$lambdaParser)($env))($mutable))((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternLiteralText)($p))($s));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 485:20', (sp_toHuman)($mc))));
          })))($maybeColon);
        }))()
        : ((($k)[0] === "LowerName")
          ? ((() => {
            const $modifier = ($k)[1];
            const $maybeModule = ($k)[2];
            const $name = ($k)[3];
            const $attrs = ($k)[4];
            return ((($modifier)[0] === "NameMutable")
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((($$home$nw$stuff$unstable$src$Types$FormattableAst$Mutable)($p))($name))($attrs))
              : ((($modifier)[0] === "NameStartsWithDot")
                ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$RecordShorthand)($p))(((sp_cons)($attrs))($name)))
                : ((($modifier)[0] === "NameNoModifier")
                  ? (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($mc) => {
                    return ((($mc)[0] === "Nothing")
                      ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Variable)($p))($maybeModule))($name))($attrs))
                      : ((($mc)[0] === "Just")
                        ? ((() => {
                          const $mutable = ($mc)[1];
                          return ((($$home$nw$stuff$unstable$src$Compiler$Parser$lambdaParser)($env))($mutable))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($p))(false))($name))($core$Maybe$Nothing));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 499:28', (sp_toHuman)($mc))));
                  })))($maybeColon)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 490:16', (sp_toHuman)($modifier)))));
          }))()
          : ((($k)[0] === "UpperName")
            ? ((() => {
              const $maybeModule = ($k)[1];
              const $name = ($k)[2];
              return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($mc) => {
                return ((($mc)[0] === "Nothing")
                  ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((($$home$nw$stuff$unstable$src$Types$FormattableAst$Constructor)($p))($maybeModule))($name))
                  : ((($mc)[0] === "Just")
                    ? ((() => {
                      const $mutable = ($mc)[1];
                      return ((($$home$nw$stuff$unstable$src$Compiler$Parser$lambdaParser)($env))($mutable))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternConstructor)($p))($maybeModule))($name))($core$Core$Nil));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 506:20', (sp_toHuman)($mc))));
              })))($maybeColon);
            }))()
            : (((($k)[0] === "RoundParen") && ((($k)[1])[0] === "Open"))
              ? ((() => {
                const $paParser = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($pa) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
                    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($mutable) => {
                      return ((($$home$nw$stuff$unstable$src$Compiler$Parser$lambdaParser)($env))($mutable))($pa);
                    })))($colon);
                  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Closed)));
                })))(($$home$nw$stuff$unstable$src$Compiler$Parser$pattern)($env));
                const $exprParser = (($$home$nw$stuff$unstable$src$Compiler$Parser$discardSecond)(($$home$nw$stuff$unstable$src$Compiler$Parser$expr)($env)))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)(($$home$nw$stuff$unstable$src$Types$Token$RoundParen)($$home$nw$stuff$unstable$src$Types$Token$Closed)));
                return ($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)($paParser))((($core$Core$Cons)($exprParser))($core$Core$Nil))));
              }))()
              : (true
                ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 475:8', (sp_toHuman)($k))))))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$maybeWithDefault = (($a) => {
  return (($p) => {
    return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)($p))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($a)))($core$Core$Nil)));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$recInlineOrIndentedOrBelow = (($higher) => {
  return (($accum) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($h) => {
      const $r = ((sp_cons)($accum))($h);
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$maybeWithDefault)($r))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)((($$home$nw$stuff$unstable$src$Compiler$Parser$recInlineOrIndentedOrBelow)($higher))($r)));
    })))($higher);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$functionApplicationOr = (($env) => {
  return (($higher) => {
    const $recInlineOrIndented = (($accum) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($h) => {
        const $r = ((sp_cons)($accum))($h);
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$block)((($$home$nw$stuff$unstable$src$Compiler$Parser$recInlineOrIndentedOrBelow)($higher))($r))))((($core$Core$Cons)(($recInlineOrIndented)($r)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($r)))($core$Core$Nil))));
      })))($higher);
    });
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($reversedArgs) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
          const $$try1 = ($core$List$reverse)($reversedArgs);
          return ((($$try1)[0] === "Nil")
            ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
            : (((($$try1)[0] === "Cons") && ((($$try1)[2])[0] === "Nil"))
              ? ((() => {
                const $fnExpression = ($$try1)[1];
                return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($fnExpression);
              }))()
              : ((($$try1)[0] === "Cons")
                ? ((() => {
                  const $fnExpression = ($$try1)[1];
                  const $args = ($$try1)[2];
                  return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((($$home$nw$stuff$unstable$src$Types$FormattableAst$FunctionCall)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($fnExpression))($args));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1152:4', (sp_toHuman)($$try1)))));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
      })))(($recInlineOrIndented)($core$Core$Nil));
    })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$abort = (($error) => {
  return (($rejections) => {
    return (($readState) => {
      return ({
        first: $rejections,
        second: (($$home$nw$stuff$unstable$src$SPLib$Parser$Aborted)($readState))($error),
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$if_ = (($env) => {
  const $maybeNewLine = (($k) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine))))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($k));
  });
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$start) => {
    const $start = ($$start)[1];
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($condition) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeThen) => {
        return (((sp_equal)($core$Maybe$Nothing))($maybeThen)
          ? ($$home$nw$stuff$unstable$src$SPLib$Parser$abort)("`if` should be followed by a `then` but I can't find it")
          : (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($true) => {
            return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
              return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
                return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($false) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
                    return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$If)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))(({
                      condition: $condition,
                      false: $false,
                      isCompact: false,
                      true: $true,
                    })));
                  })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
                })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineStatementOrBlock)($env));
              })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$Colon)));
            })))(($maybeNewLine)($$home$nw$stuff$unstable$src$Types$Token$Else));
          })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineStatementOrBlock)($env)));
      })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($maybeNewLine)($$home$nw$stuff$unstable$src$Types$Token$Then)));
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$expr)($env));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$If));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$try_ = (($env) => {
  const $maybeNewLine = ($$home$nw$stuff$unstable$src$Compiler$Parser$discardFirst)(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)));
  const $maybeNewLineKind = (($k) => {
    return ($maybeNewLine)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($k));
  });
  const $patternAndAccept = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($p) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($accept) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
          first: $p,
          second: $accept,
        }));
      })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineStatementOrBlock)($env));
    })))(($maybeNewLineKind)($$home$nw$stuff$unstable$src$Types$Token$Colon));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$pattern)($env));
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$start) => {
    const $start = ($$start)[1];
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($value) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($patterns) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Try)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))(({
              isCompact: false,
              patterns: $patterns,
              value: $value,
            })));
          })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
        })))(($$home$nw$stuff$unstable$src$Compiler$Parser$block)(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)(($maybeNewLine)($patternAndAccept))));
      })))(($maybeNewLineKind)($$home$nw$stuff$unstable$src$Types$Token$As));
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$expr)($env));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$Try));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$unaryOperator = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($token) => {
  return (((($token)[0] === "Token") && ((($token)[3])[0] === "Unop"))
    ? ((() => {
      const $s = ($token)[1];
      const $e = ($token)[2];
      const $op = (($token)[3])[1];
      return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
        first: $op,
        second: $token,
      }));
    }))()
    : (true
      ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1186:4', (sp_toHuman)($token))));
})))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);

const $$home$nw$stuff$unstable$src$Compiler$Parser$unopsOr = (($env) => {
  return (($higher) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($maybeUnary) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($right) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
          return (((($maybeUnary)[0] === "Just") && ((($maybeUnary)[1].second)[0] === "Token"))
            ? ((() => {
              const $op = ($maybeUnary)[1].first;
              const $start = (($maybeUnary)[1].second)[1];
              return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(((($$home$nw$stuff$unstable$src$Types$FormattableAst$Unop)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($op))($right));
            }))()
            : ((($maybeUnary)[0] === "Nothing")
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)($right)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1175:4', (sp_toHuman)($maybeUnary))));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
      })))($higher);
    })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)($$home$nw$stuff$unstable$src$Compiler$Parser$unaryOperator));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$expr = (($env) => {
  const $higherOr = $$home$nw$stuff$unstable$src$SPLib$Parser$higherOr;
  const $nest = ($$home$nw$stuff$unstable$src$SPLib$Parser$breakCircularDefinition)((() => {
    return ($$home$nw$stuff$unstable$src$Compiler$Parser$expr)($env);
  }));
  return (($$home$nw$stuff$unstable$src$SPLib$Parser$expression)(($$home$nw$stuff$unstable$src$Compiler$Parser$exprWithLeftDelimiter)($env)))((($core$Core$Cons)(($higherOr)(((($$home$nw$stuff$unstable$src$Compiler$Parser$list)($env))($$home$nw$stuff$unstable$src$Types$FormattableAst$List))($nest))))((($core$Core$Cons)(($higherOr)((((($$home$nw$stuff$unstable$src$Compiler$Parser$record)($env))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal)))($$home$nw$stuff$unstable$src$Types$FormattableAst$Record))($nest))))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$unopsOr)($env)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$functionApplicationOr)($env)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Exponential)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Multiplicative)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Addittive)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Comparison)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Logical)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Tuple)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Cons)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Pipe)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$binopsOr)($env))($$home$nw$stuff$unstable$src$Types$Op$Mutop)))((($core$Core$Cons)(($higherOr)(($$home$nw$stuff$unstable$src$Compiler$Parser$if_)($env))))((($core$Core$Cons)(($higherOr)(($$home$nw$stuff$unstable$src$Compiler$Parser$try_)($env))))($core$Core$Nil))))))))))))))));
});

const $$home$nw$stuff$unstable$src$Types$FormattableAst$expressionPos = (($expr) => {
  return ((($expr)[0] === "LiteralText")
    ? ((() => {
      const $p = ($expr)[1];
      return $p;
    }))()
    : ((($expr)[0] === "LiteralNumber")
      ? ((() => {
        const $p = ($expr)[1];
        return $p;
      }))()
      : ((($expr)[0] === "Variable")
        ? ((() => {
          const $p = ($expr)[1];
          return $p;
        }))()
        : ((($expr)[0] === "Constructor")
          ? ((() => {
            const $p = ($expr)[1];
            return $p;
          }))()
          : ((($expr)[0] === "Mutable")
            ? ((() => {
              const $p = ($expr)[1];
              return $p;
            }))()
            : ((($expr)[0] === "PrefixBinop")
              ? ((() => {
                const $p = ($expr)[1];
                return $p;
              }))()
              : ((($expr)[0] === "Lambda")
                ? ((() => {
                  const $p = ($expr)[1];
                  return $p;
                }))()
                : ((($expr)[0] === "FunctionCall")
                  ? ((() => {
                    const $p = ($expr)[1];
                    return $p;
                  }))()
                  : ((($expr)[0] === "Binop")
                    ? ((() => {
                      const $p = ($expr)[1];
                      return $p;
                    }))()
                    : ((($expr)[0] === "Unop")
                      ? ((() => {
                        const $p = ($expr)[1];
                        return $p;
                      }))()
                      : ((($expr)[0] === "If")
                        ? ((() => {
                          const $p = ($expr)[1];
                          return $p;
                        }))()
                        : ((($expr)[0] === "Try")
                          ? ((() => {
                            const $p = ($expr)[1];
                            return $p;
                          }))()
                          : ((($expr)[0] === "Record")
                            ? ((() => {
                              const $p = ($expr)[1];
                              return $p;
                            }))()
                            : ((($expr)[0] === "RecordShorthand")
                              ? ((() => {
                                const $p = ($expr)[1];
                                return $p;
                              }))()
                              : ((($expr)[0] === "List")
                                ? ((() => {
                                  const $p = ($expr)[1];
                                  return $p;
                                }))()
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 136:4', (sp_toHuman)($expr)))))))))))))))));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$inlineStatementOrBlock = (($env) => {
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($e) => {
    return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)(($$home$nw$stuff$unstable$src$Types$FormattableAst$expressionPos)($e)))($e)))($core$Core$Nil));
  })))(($$home$nw$stuff$unstable$src$SPLib$Parser$breakCircularDefinition)((() => {
    return ($$home$nw$stuff$unstable$src$Compiler$Parser$expr)($env);
  })))))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$block)((($$home$nw$stuff$unstable$src$Compiler$Parser$oomSeparatedBy)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))(($$home$nw$stuff$unstable$src$Compiler$Parser$statement)($env)))))($core$Core$Nil)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$upperNameBare = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($token) => {
    return (((($token)[0] === "Token") && (((($token)[3])[0] === "UpperName") && (((($token)[3])[1])[0] === "Nothing")))
      ? ((() => {
        const $start = ($token)[1];
        const $end = ($token)[2];
        const $name = (($token)[3])[2];
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$Pos$At)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($name));
      }))()
      : (true
        ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 174:4', (sp_toHuman)($token))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Types$Pos$drop = (($x) => {
  const $$a = $x;
  const $a = ($$a)[2];
  const $pos = ($$a)[1];
  return $a;
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$nonFunction = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($nf) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$n) => {
        const $n = ($$n)[2];
        return (((sp_equal)("NonFunction"))($n)
          ? ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($core$List$map)($$home$nw$stuff$unstable$src$Types$Pos$drop))($nf))
          : ($$home$nw$stuff$unstable$src$SPLib$Parser$abort)("Only NonFunction is supported for now"));
      })))(($$home$nw$stuff$unstable$src$Compiler$Parser$upperNameBare)($env));
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$rawList)(($$home$nw$stuff$unstable$src$Compiler$Parser$lowerNameBare)($env)));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$With));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$definition = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($start) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($p) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($nf) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($defModifier) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($body) => {
            return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($end) => {
              return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Definition)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))(({
                body: $body,
                modifier: $defModifier,
                nonFn: (($core$Maybe$withDefault)($core$Core$Nil))($nf),
                pattern: $p,
              })));
            })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
          })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineStatementOrBlock)($env));
        })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)($$home$nw$stuff$unstable$src$Compiler$Parser$defop));
      })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$nonFunction)($env))));
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$pattern)($env));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$here);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$errorShouldUseDefNormalHere = "You should use a normal `=` here.";

const $$home$nw$stuff$unstable$src$Compiler$Parser$typeAlias = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($name) => {
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($args) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($defModifier) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($ty) => {
            return (((sp_not_equal)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))($defModifier)
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$abort)($$home$nw$stuff$unstable$src$Compiler$Parser$errorShouldUseDefNormalHere)
              : ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeAlias)(({
                args: $args,
                name: $name,
                ty: $ty,
              }))));
          })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeExpr)($env)));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$defop);
      })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)(($$home$nw$stuff$unstable$src$Compiler$Parser$lowerNameBare)($env)));
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$upperNameBare)($env));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("alias"))($core$Core$Nil)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$unionConstructor = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($type) => {
    return (((($type)[0] === "TypeConstant") && ((($type)[2])[0] === "Nothing"))
      ? ((() => {
        const $p = ($type)[1];
        const $name = ($type)[3];
        const $args = ($type)[4];
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
          first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($p))($name),
          second: $args,
        }));
      }))()
      : (true
        ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 403:4', (sp_toHuman)($type))));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$typeExpr)($env));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$unionDef = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$name) => {
      const $p = ($$name)[1];
      const $name = ($$name)[2];
      return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($args) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($defModifier) => {
          return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($cons) => {
            return (((sp_not_equal)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))($defModifier)
              ? ($$home$nw$stuff$unstable$src$SPLib$Parser$abort)($$home$nw$stuff$unstable$src$Compiler$Parser$errorShouldUseDefNormalHere)
              : ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$UnionDef)($p))(({
                args: (($core$List$map)($$home$nw$stuff$unstable$src$Types$Pos$drop))($args),
                constructors: $cons,
                name: $name,
              }))));
          })))(($$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrBelowOrIndented)(($$home$nw$stuff$unstable$src$Compiler$Parser$rawList)(($$home$nw$stuff$unstable$src$Compiler$Parser$unionConstructor)($env))));
        })))($$home$nw$stuff$unstable$src$Compiler$Parser$defop);
      })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)(($$home$nw$stuff$unstable$src$Compiler$Parser$lowerNameBare)($env)));
    })))(($$home$nw$stuff$unstable$src$Compiler$Parser$upperNameBare)($env));
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("union"))($core$Core$Nil)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$statement = (($env) => {
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$breakCircularDefinition)((() => {
    return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
      return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$typeAlias)($env)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$unionDef)($env)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$definition)($env)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($e) => {
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)(($$home$nw$stuff$unstable$src$Types$FormattableAst$expressionPos)($e)))($e));
      })))(($$home$nw$stuff$unstable$src$Compiler$Parser$expr)($env))))($core$Core$Nil)))));
    })))(($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)));
  }));
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$without = (($p) => {
  return ((($$home$nw$stuff$unstable$src$SPLib$Parser$thenWithDefault)(($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(null)))((() => {
    return $$home$nw$stuff$unstable$src$SPLib$Parser$reject;
  })))($p);
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$end = ($$home$nw$stuff$unstable$src$SPLib$Parser$without)($$home$nw$stuff$unstable$src$SPLib$Parser$consumeOne);

const $$home$nw$stuff$unstable$src$Compiler$Parser$module_ = (($env) => {
  const $start = ($$home$nw$stuff$unstable$src$SPLib$Parser$maybe)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine));
  const $e = ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))($core$Core$Nil)));
  const $zzz = (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((() => {
    return $$home$nw$stuff$unstable$src$SPLib$Parser$end;
  })))(($$home$nw$stuff$unstable$src$SPLib$Parser$zeroOrMore)($e));
  const $statements = (($$home$nw$stuff$unstable$src$Compiler$Parser$oomSeparatedBy)(($$home$nw$stuff$unstable$src$Compiler$Parser$kind)($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))(($$home$nw$stuff$unstable$src$Compiler$Parser$statement)($env));
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)((($$home$nw$stuff$unstable$src$SPLib$Parser$map)((() => {
    return $core$Core$Nil;
  })))($$home$nw$stuff$unstable$src$SPLib$Parser$end)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$SPLib$Parser$surroundWith)($start))($zzz))($statements)))($core$Core$Nil)));
});

const $$home$nw$stuff$unstable$src$Compiler$Error$res = (($pos) => {
  return (($desc) => {
    return ($core$Result$Err)((($$home$nw$stuff$unstable$src$Compiler$Error$Simple)($pos))($desc));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$makeError = (($moduleName) => {
  return (($readState) => {
    return (($message) => {
      const $p = ((($readState)[0] === "Nil")
        ? ((($$home$nw$stuff$unstable$src$Types$Pos$P)($moduleName))(0))(1)
        : (((($readState)[0] === "Cons") && ((($readState)[1])[0] === "Token"))
          ? ((() => {
            const $start = (($readState)[1])[1];
            const $end = (($readState)[1])[2];
            const $k = (($readState)[1])[3];
            const $rest = ($readState)[2];
            return ((($$home$nw$stuff$unstable$src$Types$Pos$P)($moduleName))($start))($end);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 69:8', (sp_toHuman)($readState))));
      return (($$home$nw$stuff$unstable$src$Compiler$Error$res)($p))((($eenv) => {
        return (($core$Core$Cons)($message))($core$Core$Nil);
      }));
    });
  });
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$runParser = (($parser) => {
  return (($readState) => {
    return (($parser)((($core$Core$Cons)($readState))($core$Core$Nil)))($readState);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$runParser = (($moduleName) => {
  return (($parser) => {
    return (($tokens) => {
      const $$failureStates = (($$home$nw$stuff$unstable$src$SPLib$Parser$runParser)($parser))((($core$List$filter)((($$e) => {
        const $s = ($$e)[1];
        const $e = ($$e)[2];
        const $k = ($$e)[3];
        return ((sp_not_equal)($$home$nw$stuff$unstable$src$Types$Token$Comment))($k);
      })))($tokens));
      const $outcome = $$failureStates.second;
      const $failureStates = $$failureStates.first;
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
            return ((($$home$nw$stuff$unstable$src$Compiler$Parser$makeError)($moduleName))($readState))($message);
          }))()
          : ((($outcome)[0] === "Rejected")
            ? ((() => {
              const $findMin = (($readState) => {
                return (($best) => {
                  return ((($core$List$length)($readState) < ($core$List$length)($best))
                    ? $readState
                    : $best);
                });
              });
              const $readState = ((($core$List$for)($failureStates))($findMin))($tokens);
              const $message = ((($readState)[0] === "Nil")
                ? "I got to the end of file and I can't make sense of it. =("
                : (true
                  ? "I got stuck parsing here. =("
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 118:16', (sp_toHuman)($readState))));
              return ((($$home$nw$stuff$unstable$src$Compiler$Parser$makeError)($moduleName))($readState))($message);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 103:4', (sp_toHuman)($outcome)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$parse = (($stripLocations) => {
  return (($moduleName) => {
    return (($tokens) => {
      const $parser = ($$home$nw$stuff$unstable$src$Compiler$Parser$module_)(({
        moduleName: $moduleName,
        stripLocations: $stripLocations,
      }));
      return ((($$home$nw$stuff$unstable$src$Compiler$Parser$runParser)($moduleName))($parser))($tokens);
    });
  });
});

const $$home$nw$stuff$unstable$src$SPON$unhackPosEnd = (($moduleName) => {
  return (($pos) => {
    return ((($pos)[0] === "End")
      ? ($$home$nw$stuff$unstable$src$Types$Pos$End)($moduleName)
      : (true
        ? $pos
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 37:4', (sp_toHuman)($pos))));
  });
});

const $$home$nw$stuff$unstable$src$SPON$run = (($readerA) => {
  return (($sponName) => {
    return (($statements) => {
      const $$try1 = ($readerA)($statements);
      return (((($$try1)[0] === "Accepted") && ((($$try1)[1])[0] === "Nil"))
        ? ((() => {
          const $a = ($$try1)[2];
          return ($core$Result$Ok)($a);
        }))()
        : (((($$try1)[0] === "Accepted") && ((($$try1)[1])[0] === "Cons"))
          ? ((() => {
            const $head = (($$try1)[1])[1];
            const $tail = (($$try1)[1])[2];
            const $a = ($$try1)[2];
            return (($$home$nw$stuff$unstable$src$Compiler$Error$res)(($$home$nw$stuff$unstable$src$Types$FormattableAst$statementPos)($head)))((() => {
              return (($core$Core$Cons)("unread statements"))($core$Core$Nil);
            }));
          }))()
          : (((($$try1)[0] === "Rejected") && ((($$try1)[1])[0] === "At"))
            ? ((() => {
              const $pos = (($$try1)[1])[1];
              const $r = (($$try1)[1])[2];
              return (($$home$nw$stuff$unstable$src$Compiler$Error$res)((($$home$nw$stuff$unstable$src$SPON$unhackPosEnd)($sponName))($pos)))((() => {
                return (($core$Core$Cons)($r))($core$Core$Nil);
              }));
            }))()
            : (((($$try1)[0] === "Failed") && ((($$try1)[1])[0] === "At"))
              ? ((() => {
                const $pos = (($$try1)[1])[1];
                const $r = (($$try1)[1])[2];
                return (($$home$nw$stuff$unstable$src$Compiler$Error$res)((($$home$nw$stuff$unstable$src$SPON$unhackPosEnd)($sponName))($pos)))((() => {
                  return (($core$Core$Cons)($r))($core$Core$Nil);
                }));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 44:4', (sp_toHuman)($$try1))))));
    });
  });
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

const $$home$nw$stuff$unstable$src$SPON$read = (($reader) => {
  return (($sponName) => {
    return (($sponContent) => {
      return (($core$Result$onOk)((($$home$nw$stuff$unstable$src$SPON$run)($reader))($sponName)))((($core$Result$onOk)((($$home$nw$stuff$unstable$src$Compiler$Parser$parse)(false))($sponName)))((($$home$nw$stuff$unstable$src$Compiler$Lexer$lexer)($sponName))($sponContent)));
    });
  });
});

const $core$Result$map = (($f) => {
  return (($result) => {
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
});

const $$home$nw$stuff$unstable$src$ModulesFile$textToModulesFile = (($sponName) => {
  return (($sponContent) => {
    const $insert = (($rootEntry) => {
      return (($mf) => {
        return ((($rootEntry)[0] === "Lib")
          ? ((() => {
            const $lib = ($rootEntry)[1];
            return (Object.assign)({}, $mf, ({
              libraries: ((sp_cons)($mf.libraries))($lib),
            }));
          }))()
          : ((($rootEntry)[0] === "Dir")
            ? ((() => {
              const $dir = ($rootEntry)[1];
              return (Object.assign)({}, $mf, ({
                sourceDirs: ((sp_cons)($mf.sourceDirs))($dir),
              }));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/ModulesFile.sp 153:8', (sp_toHuman)($rootEntry))));
      });
    });
    return (($core$Result$map)((($rootEntries) => {
      return ((($core$List$for)($rootEntries))($insert))($$home$nw$stuff$unstable$src$ModulesFile$initModulesFile);
    })))(((($$home$nw$stuff$unstable$src$SPON$read)($$home$nw$stuff$unstable$src$ModulesFile$modulesFileReader))($sponName))($sponContent));
  });
});

const $core$Dict$singleton = (($key) => {
  return (($value) => {
    return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($key))($value))($core$Dict$RBEmpty_elm_builtin))($core$Dict$RBEmpty_elm_builtin);
  });
});

const $$home$nw$stuff$unstable$src$Compile$loadModulesFile = (($platform) => {
  return (($projectRoot) => {
    const $path = ($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($projectRoot))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compile$modulesFileName))($core$Core$Nil)));
    ((sp_log)("Metafile: "))($path);
    return (($$home$nw$stuff$unstable$lib$posix$IO$onResult)((($result) => {
      const $modulesAsText = ((($result)[0] === "Ok")
        ? ((() => {
          const $f = ($result)[1];
          return $f;
        }))()
        : ((($result)[0] === "Err")
          ? ((() => {
            (sp_log)("Using default modules.sp");
            return $platform.defaultModules;
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 62:8', (sp_toHuman)($result))));
      const $eenv = ({
        moduleByName: (($core$Dict$singleton)($$home$nw$stuff$unstable$src$Compile$modulesFileName))(({
          content: $modulesAsText,
          fsPath: $$home$nw$stuff$unstable$src$Compile$modulesFileName,
        })),
      });
      return (($$home$nw$stuff$unstable$src$Compile$resToIo)($eenv))((($$home$nw$stuff$unstable$src$ModulesFile$textToModulesFile)($$home$nw$stuff$unstable$src$Compile$modulesFileName))($modulesAsText));
    })))(($$home$nw$stuff$unstable$lib$posix$IO$readFile)($path));
  });
});

const $core$List$find = (($test) => {
  return (($list) => {
    return ((($list)[0] === "Nil")
      ? $core$Maybe$Nothing
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $h = ($list)[1];
          const $t = ($list)[2];
          return (($test)($h)
            ? ($core$Maybe$Just)($h)
            : (($core$List$find)($test))($t));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 24:4', (sp_toHuman)($list))));
  });
});

const $$home$nw$stuff$unstable$src$Compile$updateSourceDir = (($fileNames) => {
  const $insertModuleName = (($name) => {
    return (($sd) => {
      const $$try1 = (($core$List$find)((($m) => {
        return ((sp_equal)($name))($m.path);
      })))($sd.modules);
      return ((($$try1)[0] === "Just")
        ? $sd
        : ((($$try1)[0] === "Nothing")
          ? (Object.assign)({}, $sd, ({
            modules: ((sp_cons)($sd.modules))(({
              globalTypes: $core$Core$Nil,
              globalValues: $core$Core$Nil,
              path: $name,
              visibleAs: $name,
            })),
          }))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 197:8', (sp_toHuman)($$try1))));
    });
  });
  return (($core$List$for)($fileNames))($insertModuleName);
});

const $$home$nw$stuff$unstable$src$ModulesFile$insertModule = (($source) => {
  return (($mod) => {
    return (($meta) => {
      const $visibleAs = $mod.visibleAs;
      const $umr = (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($source))($mod.path);
      const $insertGlobal = (($varName) => {
        return (($core$Dict$insert)($varName))((($$home$nw$stuff$unstable$src$Types$Meta$USR)($umr))($varName));
      });
      return ({
        globalTypes: ((($core$List$for)($mod.globalTypes))($insertGlobal))($meta.globalTypes),
        globalValues: ((($core$List$for)($mod.globalValues))($insertGlobal))($meta.globalValues),
        moduleVisibleAsToUmr: ((($core$Dict$insert)($visibleAs))($umr))($meta.moduleVisibleAsToUmr),
        umrToModuleVisibleAs: ((($core$Dict$insert)($umr))($visibleAs))($meta.umrToModuleVisibleAs),
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$ModulesFile$insertLibrary = (($lib) => {
  return (($meta) => {
    const $umr = ((() => {
      const $$try1 = $lib.source;
      return (("core:prelude" === $$try1)
        ? $$home$nw$stuff$unstable$src$Types$Meta$Core
        : (("core:posix" === $$try1)
          ? $$home$nw$stuff$unstable$src$Types$Meta$Posix
          : (true
            ? (sp_todo)(("Library source `" + ($lib.source + "` is not supported.")))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/ModulesFile.sp 53:8', (sp_toHuman)($$try1)))));
    }))();
    return ((($core$List$for)($lib.modules))(($$home$nw$stuff$unstable$src$ModulesFile$insertModule)($umr)))($meta);
  });
});

const $$home$nw$stuff$unstable$src$ModulesFile$insertModules = (($sd) => {
  return (($core$List$for)($sd.modules))(($$home$nw$stuff$unstable$src$ModulesFile$insertModule)(($$home$nw$stuff$unstable$src$Types$Meta$SourceDir)($sd.path)));
});

const $$home$nw$stuff$unstable$src$Types$Meta$init = ({
  globalTypes: $core$Dict$empty,
  globalValues: $core$Dict$empty,
  moduleVisibleAsToUmr: $core$Dict$empty,
  umrToModuleVisibleAs: $core$Dict$empty,
});

const $$home$nw$stuff$unstable$src$ModulesFile$toMeta = (($mf) => {
  return ((($core$List$for)($mf.sourceDirs))($$home$nw$stuff$unstable$src$ModulesFile$insertModules))(((($core$List$for)($mf.libraries))($$home$nw$stuff$unstable$src$ModulesFile$insertLibrary))($$home$nw$stuff$unstable$src$Types$Meta$init));
});

const $core$List$map2 = (($f) => {
  const $rec = (($accum) => {
    return (($ax) => {
      return (($bx) => {
        const $$try1 = ({
          first: $ax,
          second: $bx,
        });
        return (((($$try1.first)[0] === "Cons") && (($$try1.second)[0] === "Cons"))
          ? ((() => {
            const $ahead = ($$try1.first)[1];
            const $atail = ($$try1.first)[2];
            const $bhead = ($$try1.second)[1];
            const $btail = ($$try1.second)[2];
            return ((($rec)(((sp_cons)($accum))((($f)($ahead))($bhead))))($atail))($btail);
          }))()
          : (true
            ? ($core$List$reverse)($accum)
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 130:6', (sp_toHuman)($$try1))));
      });
    });
  });
  return ($rec)($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Compile$loadMeta = (($env) => {
  return (($platform) => {
    return (($entryModuleDir) => {
      return (($projectRoot) => {
        return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($modulesFileRaw) => {
          const $resolvedDirs = (($core$List$map)((($sd) => {
            return (Object.assign)({}, $sd, ({
              path: ($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($projectRoot))((($core$Core$Cons)($sd.path))($core$Core$Nil))),
            }));
          })))($modulesFileRaw.sourceDirs);
          const $allDirs = ((($core$List$any)((($sd) => {
            return ((sp_equal)($entryModuleDir))($sd.path);
          })))($resolvedDirs)
            ? $resolvedDirs
            : ((sp_cons)($resolvedDirs))(({
              modules: $core$Core$Nil,
              path: $entryModuleDir,
            })));
          const $modulesFile = (Object.assign)({}, $modulesFileRaw, ({
            sourceDirs: $allDirs,
          }));
          const $getAllSourceDirLists = ($$home$nw$stuff$unstable$lib$posix$IO$parallel)((($core$List$map)((($sd) => {
            return (($$home$nw$stuff$unstable$src$Compile$listSourceDir)($sd.path))("");
          })))($modulesFile.sourceDirs));
          return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($allSourceDirLists) => {
            const $updatedSourceDirs = ((($core$List$map2)($$home$nw$stuff$unstable$src$Compile$updateSourceDir))($allSourceDirLists))($modulesFile.sourceDirs);
            return ($$home$nw$stuff$unstable$lib$posix$IO$succeed)(($$home$nw$stuff$unstable$src$ModulesFile$toMeta)((Object.assign)({}, $modulesFile, ({
              sourceDirs: $updatedSourceDirs,
            }))));
          })))($getAllSourceDirLists);
        })))((($$home$nw$stuff$unstable$src$Compile$loadModulesFile)($platform))($projectRoot));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$initEnv = (($ro) => {
  return ({
    defsPath: $core$Core$Nil,
    maybeShorthandTarget: $core$Maybe$Nothing,
    nonRootValues: $core$Dict$empty,
    ro: $ro,
    tyvarRenames: $core$Dict$empty,
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError = (($pos) => {
  return (($msg) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Error$res)($pos))((($errorEnv) => {
      return $msg;
    }));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$addAttributes = (($ro) => {
  return (($pos) => {
    return (($faAttrs) => {
      return (($caAttrsAccum) => {
        return ((($faAttrs)[0] === "Nil")
          ? ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($core$Maybe$Nothing))($caAttrsAccum))
          : (((($faAttrs)[0] === "Cons") && ((($faAttrs)[1].first)[0] === "At"))
            ? ((() => {
              const $p = (($faAttrs)[1].first)[1];
              const $name = (($faAttrs)[1].first)[2];
              const $maybeFaType = ($faAttrs)[1].second;
              const $faTail = ($faAttrs)[2];
              return ((($maybeFaType)[0] === "Nothing")
                ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($p))((($core$Core$Cons)(("Attribute `" + ($name + "` must have a type"))))($core$Core$Nil))
                : ((($maybeFaType)[0] === "Just")
                  ? ((() => {
                    const $faType = ($maybeFaType)[1];
                    return (($core$Result$onOk)((($caType) => {
                      return (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$addAttributes)($ro))($p))($faTail))(((($core$Dict$insert)($name))($caType))($caAttrsAccum));
                    })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($core$Maybe$Nothing))($ro))($faType));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 903:12', (sp_toHuman)($maybeFaType))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 897:4', (sp_toHuman)($faAttrs))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$maybeForeignUsr = (($getter) => {
  return (($ro) => {
    return (($maybeModule) => {
      return (($name) => {
        return ((($maybeModule)[0] === "Just")
          ? ((() => {
            const $moduleName = ($maybeModule)[1];
            const $$try1 = (($core$Dict$get)($moduleName))($ro.meta.moduleVisibleAsToUmr);
            return ((($$try1)[0] === "Just")
              ? ((() => {
                const $umr = ($$try1)[1];
                return ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($umr))($name));
              }))()
              : ((($$try1)[0] === "Nothing")
                ? (sp_todo)(("!!resolveToUsr can't find the module: " + ($moduleName + (" (for: " + ($name + ")")))))
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 105:12', (sp_toHuman)($$try1))));
          }))()
          : ((($maybeModule)[0] === "Nothing")
            ? (($core$Dict$get)($name))(($getter)($ro.meta))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 103:4', (sp_toHuman)($maybeModule))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToUsr = (($getter) => {
  return (($ro) => {
    return (($maybeModule) => {
      return (($name) => {
        return (($core$Maybe$withDefault)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($ro.currentModule))($name)))((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$maybeForeignUsr)($getter))($ro))($maybeModule))($name));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToTypeUsr = ($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToUsr)((($m) => {
  return $m.globalTypes;
}));

const $core$List$forRes = (($ls) => {
  return (($f) => {
    return (($accum) => {
      return ((($ls)[0] === "Nil")
        ? ($core$Result$Ok)($accum)
        : ((($ls)[0] === "Cons")
          ? ((() => {
            const $h = ($ls)[1];
            const $t = ($ls)[2];
            const $$try1 = (($f)($h))($accum);
            return ((($$try1)[0] === "Err")
              ? ((() => {
                const $x = ($$try1)[1];
                return ($core$Result$Err)($x);
              }))()
              : ((($$try1)[0] === "Ok")
                ? ((() => {
                  const $newAccum = ($$try1)[1];
                  return ((($core$List$forRes)($t))($f))($newAccum);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 155:12', (sp_toHuman)($$try1))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 150:4', (sp_toHuman)($ls))));
    });
  });
});

const $core$List$mapRes = (($f) => {
  return (($list) => {
    const $fun = (($a) => {
      return (($acc) => {
        return (($core$Result$map)((($b) => {
          return ((sp_cons)($acc))($b);
        })))(($f)($a));
      });
    });
    return (($core$Result$map)($core$List$reverse))(((($core$List$forRes)($list))($fun))($core$Core$Nil));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType = (($mrf) => {
  return (($ro) => {
    return (($faType) => {
      return ((($faType)[0] === "TypeVariable")
        ? ((() => {
          const $pos = ($faType)[1];
          const $name = ($faType)[2];
          return ((($mrf)[0] === "Nothing")
            ? ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($name))
            : ((($mrf)[0] === "Just")
              ? ((() => {
                const $renameFunction = ($mrf)[1];
                return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))((($renameFunction)($pos))($name)));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 920:12', (sp_toHuman)($mrf))));
        }))()
        : ((($faType)[0] === "TypeConstant")
          ? ((() => {
            const $pos = ($faType)[1];
            const $maybeModule = ($faType)[2];
            const $name = ($faType)[3];
            const $args = ($faType)[4];
            return (($core$Result$onOk)((($caArgs) => {
              return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToTypeUsr)($ro))($maybeModule))($name)))($caArgs));
            })))((($core$List$mapRes)((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro)))($args));
          }))()
          : ((($faType)[0] === "TypeFunction")
            ? ((() => {
              const $pos = ($faType)[1];
              const $fa_from = ($faType)[2];
              const $fromIsMut = ($faType)[3];
              const $fa_to = ($faType)[4];
              return (($core$Result$onOk)((($ca_from) => {
                return (($core$Result$onOk)((($ca_to) => {
                  return ($core$Result$Ok)((((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))($ca_from))($fromIsMut))($ca_to));
                })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($fa_to));
              })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($fa_from));
            }))()
            : ((($faType)[0] === "TypeTuple")
              ? ((() => {
                const $pos = ($faType)[1];
                const $types = ($faType)[2];
                return (((($types)[0] === "Cons") && (((($types)[2])[0] === "Cons") && (((($types)[2])[2])[0] === "Nil")))
                  ? ((() => {
                    const $faFirst = ($types)[1];
                    const $faSecond = (($types)[2])[1];
                    return (($core$Result$onOk)((($caFirst) => {
                      return (($core$Result$onOk)((($caSecond) => {
                        return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($core$Maybe$Nothing))(((($core$Dict$insert)("second"))($caSecond))(((($core$Dict$insert)("first"))($caFirst))($core$Dict$empty))));
                      })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($faSecond));
                    })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($faFirst));
                  }))()
                  : (((($types)[0] === "Cons") && (((($types)[2])[0] === "Cons") && ((((($types)[2])[2])[0] === "Cons") && ((((($types)[2])[2])[2])[0] === "Nil"))))
                    ? ((() => {
                      const $faFirst = ($types)[1];
                      const $faSecond = (($types)[2])[1];
                      const $faThird = ((($types)[2])[2])[1];
                      return (($core$Result$onOk)((($caFirst) => {
                        return (($core$Result$onOk)((($caSecond) => {
                          return (($core$Result$onOk)((($caThird) => {
                            return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($core$Maybe$Nothing))(((($core$Dict$insert)("third"))($caThird))(((($core$Dict$insert)("second"))($caSecond))(((($core$Dict$insert)("first"))($caFirst))($core$Dict$empty)))));
                          })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($faThird));
                        })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($faSecond));
                      })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($faFirst));
                    }))()
                    : (true
                      ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Tuples can only have size 2 or 3. Use a record."))($core$Core$Nil))
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 939:12', (sp_toHuman)($types)))));
              }))()
              : ((($faType)[0] === "TypeList")
                ? ((() => {
                  const $pos = ($faType)[1];
                  const $faItem = ($faType)[2];
                  return (($core$Result$onOk)((($caItem) => {
                    return ($core$Result$Ok)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($caItem));
                  })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($mrf))($ro))($faItem));
                }))()
                : ((($faType)[0] === "TypeRecord")
                  ? ((() => {
                    const $p = ($faType)[1];
                    const $recordArgs = ($faType)[2];
                    return (((sp_not_equal)($core$Maybe$Nothing))($recordArgs.extends)
                      ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($p))((($core$Core$Cons)("For now extensible types are disabled, I want to see if it's good to do without them"))($core$Core$Nil))
                      : (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$addAttributes)($ro))($p))($recordArgs.attrs))($core$Dict$empty));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 918:4', (sp_toHuman)($faType))))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateConstructor = (($ro) => {
  return (($unionType) => {
    return (($unionUsr) => {
      return (($$faArgs) => {
        const $pos = ($$faArgs.first)[1];
        const $name = ($$faArgs.first)[2];
        const $faArgs = $$faArgs.second;
        return (($constructors) => {
          return ((($core$Dict$member)($name))($constructors)
            ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)(("constructor " + ($name + " is duplicate"))))($core$Core$Nil))
            : (($core$Result$onOk)((($caArgs) => {
              const $c = ({
                args: $caArgs,
                pos: $pos,
                type: ((($core$List$forReversed)($caArgs))((($ar) => {
                  return (($ty) => {
                    return (((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))($ar))(false))($ty);
                  });
                })))($unionType),
                typeUsr: $unionUsr,
              });
              return ($core$Result$Ok)(((($core$Dict$insert)($name))($c))($constructors));
            })))((($core$List$mapRes)((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($core$Maybe$Nothing))($ro)))($faArgs)));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$deps_init = ({
  cons: $core$Set$empty,
  types: $core$Set$empty,
  values: $core$Set$empty,
});

const $core$Dict$for = (($dict) => {
  return (($func) => {
    return (($acc) => {
      return ((($dict)[0] === "RBEmpty_elm_builtin")
        ? $acc
        : ((($dict)[0] === "RBNode_elm_builtin")
          ? ((() => {
            const $key = ($dict)[2];
            const $value = ($dict)[3];
            const $left = ($dict)[4];
            const $right = ($dict)[5];
            return ((($core$Dict$for)($right))($func))(((($func)($key))($value))(((($core$Dict$for)($left))($func))($acc)));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 454:2', (sp_toHuman)($dict))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps = (($type) => {
  return (($acc) => {
    return ((($type)[0] === "TypeConstant")
      ? ((() => {
        const $usr = ($type)[2];
        const $args = ($type)[3];
        return ((($core$List$for)($args))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps))((($core$Set$insert)($usr))($acc));
      }))()
      : ((($type)[0] === "TypeVariable")
        ? $acc
        : ((($type)[0] === "TypeFunction")
          ? ((() => {
            const $from = ($type)[2];
            const $to = ($type)[4];
            return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps)($to))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps)($from))($acc));
          }))()
          : ((($type)[0] === "TypeRecord")
            ? ((() => {
              const $attrs = ($type)[3];
              return ((($core$Dict$for)($attrs))((($k) => {
                return $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps;
              })))($acc);
            }))()
            : ((($type)[0] === "TypeAlias")
              ? (sp_todo)("typeDeps: Should not happen")
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 158:4', (sp_toHuman)($type)))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps = (($pattern) => {
  return (($deps) => {
    return ((($pattern)[0] === "PatternConstructor")
      ? ((() => {
        const $usr = ($pattern)[2];
        const $ps = ($pattern)[3];
        return ((($core$List$for)($ps))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps))((Object.assign)({}, $deps, ({
          cons: (($core$Set$insert)($usr))($deps.cons),
        })));
      }))()
      : ((($pattern)[0] === "PatternRecord")
        ? ((() => {
          const $ps = ($pattern)[2];
          return ((($core$Dict$for)($ps))((($k) => {
            return $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps;
          })))($deps);
        }))()
        : (((($pattern)[0] === "PatternAny") && ((($pattern)[3])[0] === "Just"))
          ? ((() => {
            const $type = (($pattern)[3])[1];
            return (Object.assign)({}, $deps, ({
              types: (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps)($type))($deps.types),
            }));
          }))()
          : (((($pattern)[0] === "PatternAny") && ((($pattern)[3])[0] === "Nothing"))
            ? $deps
            : ((($pattern)[0] === "PatternLiteralNumber")
              ? $deps
              : ((($pattern)[0] === "PatternLiteralText")
                ? $deps
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 182:4', (sp_toHuman)($pattern))))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps = (($expr) => {
  return (($deps) => {
    return ((($expr)[0] === "LiteralNumber")
      ? $deps
      : ((($expr)[0] === "LiteralText")
        ? $deps
        : (((($expr)[0] === "Variable") && ((($expr)[2].ref)[0] === "RefRoot"))
          ? ((() => {
            const $attrPath = ($expr)[2].attrPath;
            const $usr = (($expr)[2].ref)[1];
            return (Object.assign)({}, $deps, ({
              values: (($core$Set$insert)($usr))($deps.values),
            }));
          }))()
          : ((($expr)[0] === "Variable")
            ? $deps
            : ((($expr)[0] === "Constructor")
              ? ((() => {
                const $usr = ($expr)[2];
                return (Object.assign)({}, $deps, ({
                  cons: (($core$Set$insert)($usr))($deps.cons),
                }));
              }))()
              : (((($expr)[0] === "Lambda") && ((($expr)[2])[0] === "ParameterPattern"))
                ? ((() => {
                  const $pa = (($expr)[2])[1];
                  const $body = ($expr)[3];
                  return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($body))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps)($pa))($deps));
                }))()
                : (((($expr)[0] === "Lambda") && ((($expr)[2])[0] === "ParameterMutable"))
                  ? ((() => {
                    const $body = ($expr)[3];
                    return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($body))($deps);
                  }))()
                  : (((($expr)[0] === "Record") && ((($expr)[2])[0] === "Nothing"))
                    ? ((() => {
                      const $exprByName = ($expr)[3];
                      return ((($core$Dict$for)($exprByName))((($name) => {
                        return $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps;
                      })))($deps);
                    }))()
                    : (((($expr)[0] === "Record") && (((($expr)[2])[0] === "Just") && (((($expr)[2])[1].ref)[0] === "RefRoot")))
                      ? ((() => {
                        const $attrPath = (($expr)[2])[1].attrPath;
                        const $usr = ((($expr)[2])[1].ref)[1];
                        const $exprByName = ($expr)[3];
                        return ((($core$Dict$for)($exprByName))((($name) => {
                          return $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps;
                        })))((Object.assign)({}, $deps, ({
                          values: (($core$Set$insert)($usr))($deps.values),
                        })));
                      }))()
                      : ((($expr)[0] === "Record")
                        ? ((() => {
                          const $exprByName = ($expr)[3];
                          return ((($core$Dict$for)($exprByName))((($name) => {
                            return $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps;
                          })))($deps);
                        }))()
                        : (((($expr)[0] === "Call") && ((($expr)[3])[0] === "ArgumentExpression"))
                          ? ((() => {
                            const $e0 = ($expr)[2];
                            const $e1 = (($expr)[3])[1];
                            return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($e1))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($e0))($deps));
                          }))()
                          : (((($expr)[0] === "Call") && ((($expr)[3])[0] === "ArgumentMutable"))
                            ? ((() => {
                              const $e0 = ($expr)[2];
                              return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($e0))($deps);
                            }))()
                            : ((($expr)[0] === "If")
                              ? ((() => {
                                const $args = ($expr)[2];
                                return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($args.false))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($args.true))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($args.condition))($deps)));
                              }))()
                              : ((($expr)[0] === "Try")
                                ? ((() => {
                                  const $e = ($expr)[2];
                                  const $patternsAndBodies = ($expr)[3];
                                  return ((($core$List$for)($patternsAndBodies))((($$b) => {
                                    const $p = $$b.first;
                                    const $b = $$b.second;
                                    return (($d) => {
                                      return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($b))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps)($p))($d));
                                    });
                                  })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($e))($deps));
                                }))()
                                : ((($expr)[0] === "LetIn")
                                  ? ((() => {
                                    const $valueDef = ($expr)[1];
                                    const $e = ($expr)[2];
                                    return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($e))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($valueDef.body))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps)($valueDef.pattern))($deps)));
                                  }))()
                                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 193:4', (sp_toHuman)($expr)))))))))))))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$cons = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("Cons");

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$nil = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("Nil");

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToConstructorUsr = ($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToUsr)((($m) => {
  return $m.globalValues;
}));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateNumber = (($constructor) => {
  return (($pos) => {
    return (($numberAsText) => {
      const $$try1 = (text_toNumber)($numberAsText);
      return ((($$try1)[0] === "Nothing")
        ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)(("invalid number: `" + ($numberAsText + "`"))))((($core$Core$Cons)("TODO link to documentation on valid number formats"))($core$Core$Nil)))
        : ((($$try1)[0] === "Just")
          ? ((() => {
            const $n = ($$try1)[1];
            return ($core$Result$Ok)((($constructor)($pos))($n));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 633:4', (sp_toHuman)($$try1))));
    });
  });
});

const $core$Maybe$mapRes = (($f) => {
  return (($m) => {
    return ((($m)[0] === "Nothing")
      ? ($core$Result$Ok)($core$Maybe$Nothing)
      : ((($m)[0] === "Just")
        ? ((() => {
          const $a = ($m)[1];
          return (($core$Result$map)($core$Maybe$Just))(($f)($a));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Maybe.sp 38:4', (sp_toHuman)($m))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern = (($ann) => {
  return (($env) => {
    return (($fa) => {
      return (((($fa)[0] === "PatternAny") && ($fa)[2])
        ? ((() => {
          const $pos = ($fa)[1];
          const $s = ($fa)[3];
          return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("This is the wrong place to use `@`"))($core$Core$Nil));
        }))()
        : (((($fa)[0] === "PatternAny") && !(($fa)[2]))
          ? ((() => {
            const $pos = ($fa)[1];
            const $name = ($fa)[3];
            const $maybeFaType = ($fa)[4];
            return ((((sp_equal)($core$Maybe$Nothing))($ann) && ((sp_not_equal)($core$Maybe$Nothing))($maybeFaType))
              ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Can't use annotations here"))($core$Core$Nil))
              : (($core$Result$onOk)((($maybeCaType) => {
                const $n = (((sp_equal)("_"))($name)
                  ? $core$Maybe$Nothing
                  : ($core$Maybe$Just)($name));
                return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternAny)($pos))($n))($maybeCaType));
              })))((($core$Maybe$mapRes)((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($ann))($env.ro)))($maybeFaType)));
          }))()
          : ((($fa)[0] === "PatternLiteralNumber")
            ? ((() => {
              const $pos = ($fa)[1];
              const $l = ($fa)[2];
              return ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateNumber)($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternLiteralNumber))($pos))($l);
            }))()
            : ((($fa)[0] === "PatternLiteralText")
              ? ((() => {
                const $pos = ($fa)[1];
                const $l = ($fa)[2];
                return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternLiteralText)($pos))($l));
              }))()
              : ((($fa)[0] === "PatternConstructor")
                ? ((() => {
                  const $pos = ($fa)[1];
                  const $maybeModule = ($fa)[2];
                  const $name = ($fa)[3];
                  const $faArgs = ($fa)[4];
                  return (($core$Result$onOk)((($caArgs) => {
                    return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternConstructor)($pos))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToConstructorUsr)($env.ro))($maybeModule))($name)))($caArgs));
                  })))((($core$List$mapRes)((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($ann))($env)))($faArgs));
                }))()
                : ((($fa)[0] === "PatternList")
                  ? ((() => {
                    const $pos = ($fa)[1];
                    const $fas = ($fa)[2];
                    const $fold = (($pattern) => {
                      return (($last) => {
                        return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternConstructor)($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$cons))((($core$Core$Cons)($pattern))((($core$Core$Cons)($last))($core$Core$Nil)));
                      });
                    });
                    return (($core$Result$onOk)((($cas) => {
                      return ($core$Result$Ok)(((($core$List$forReversed)($cas))($fold))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternConstructor)($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$nil))($core$Core$Nil)));
                    })))((($core$List$mapRes)((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($ann))($env)))($fas));
                  }))()
                  : ((($fa)[0] === "PatternRecord")
                    ? ((() => {
                      const $pos = ($fa)[1];
                      const $recordArgs = ($fa)[2];
                      return (((sp_not_equal)($core$Maybe$Nothing))($recordArgs.extends)
                        ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("can't use `with` inside patterns"))($core$Core$Nil))
                        : ((() => {
                          const $fold = (($$maybePattern) => {
                            const $p = ($$maybePattern.first)[1];
                            const $name = ($$maybePattern.first)[2];
                            const $maybePattern = $$maybePattern.second;
                            return (($dict) => {
                              return ((($core$Dict$member)($name))($dict)
                                ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($p))((($core$Core$Cons)(("duplicate attribute name in pattern: " + $name)))($core$Core$Nil))
                                : ((($maybePattern)[0] === "Nothing")
                                  ? ($core$Result$Ok)(((($core$Dict$insert)($name))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternAny)($p))(($core$Maybe$Just)($name)))($core$Maybe$Nothing)))($dict))
                                  : ((($maybePattern)[0] === "Just")
                                    ? ((() => {
                                      const $faPattern = ($maybePattern)[1];
                                      return (($core$Result$map)((($caPattern) => {
                                        return ((($core$Dict$insert)($name))($caPattern))($dict);
                                      })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($ann))($env))($faPattern));
                                    }))()
                                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 397:24', (sp_toHuman)($maybePattern)))));
                            });
                          });
                          return (($core$Result$map)((($x) => {
                            return (($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternRecord)($pos))($x);
                          })))(((($core$List$forRes)($recordArgs.attrs))($fold))($core$Dict$empty));
                        }))());
                    }))()
                    : ((($fa)[0] === "PatternListCons")
                      ? ((() => {
                        const $pos = ($fa)[1];
                        const $pas = ($fa)[2];
                        return (($core$Result$onOk)((($caPas) => {
                          const $$try1 = ($core$List$reverse)($caPas);
                          return ((($$try1)[0] === "Cons")
                            ? ((() => {
                              const $last = ($$try1)[1];
                              const $rest = ($$try1)[2];
                              return ($core$Result$Ok)(((($core$List$for)($rest))((($item) => {
                                return (($list) => {
                                  return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternConstructor)($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$cons))((($core$Core$Cons)($item))((($core$Core$Cons)($list))($core$Core$Nil)));
                                });
                              })))($last));
                            }))()
                            : ((($$try1)[0] === "Nil")
                              ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("should not happen: empty cons pattern"))($core$Core$Nil))
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 412:12', (sp_toHuman)($$try1))));
                        })))((($core$List$mapRes)((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($ann))($env)))($pas));
                      }))()
                      : ((($fa)[0] === "PatternTuple")
                        ? ((() => {
                          const $pos = ($fa)[1];
                          const $fas = ($fa)[2];
                          return (((($fas)[0] === "Cons") && (((($fas)[2])[0] === "Cons") && (((($fas)[2])[2])[0] === "Nil")))
                            ? ((() => {
                              const $fa1 = ($fas)[1];
                              const $fa2 = (($fas)[2])[1];
                              return ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($ann))($env))((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternRecord)($pos))(({
                                attrs: (($core$Core$Cons)(({
                                  first: (($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos)($fa1)))("first"),
                                  second: ($core$Maybe$Just)($fa1),
                                })))((($core$Core$Cons)(({
                                  first: (($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos)($fa2)))("second"),
                                  second: ($core$Maybe$Just)($fa2),
                                })))($core$Core$Nil)),
                                extends: $core$Maybe$Nothing,
                              })));
                            }))()
                            : (((($fas)[0] === "Cons") && (((($fas)[2])[0] === "Cons") && ((((($fas)[2])[2])[0] === "Cons") && ((((($fas)[2])[2])[2])[0] === "Nil"))))
                              ? ((() => {
                                const $fa1 = ($fas)[1];
                                const $fa2 = (($fas)[2])[1];
                                const $fa3 = ((($fas)[2])[2])[1];
                                return ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($ann))($env))((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternRecord)($pos))(({
                                  attrs: (($core$Core$Cons)(({
                                    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos)($fa1)))("first"),
                                    second: ($core$Maybe$Just)($fa1),
                                  })))((($core$Core$Cons)(({
                                    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos)($fa2)))("second"),
                                    second: ($core$Maybe$Just)($fa2),
                                  })))((($core$Core$Cons)(({
                                    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternPos)($fa3)))("third"),
                                    second: ($core$Maybe$Just)($fa3),
                                  })))($core$Core$Nil))),
                                  extends: $core$Maybe$Nothing,
                                })));
                              }))()
                              : (true
                                ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("tuples can be only of size 2 or 3"))($core$Core$Nil))
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 422:12', (sp_toHuman)($fas)))));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 355:4', (sp_toHuman)($fa)))))))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneValue = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneName);

const $core$Dict$join = (($a) => {
  return (($core$Dict$for)($a))($core$Dict$insert);
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames = (($p) => {
  return (((($p)[0] === "PatternAny") && ((($p)[2])[0] === "Nothing"))
    ? ((() => {
      const $pos = ($p)[1];
      return $core$Dict$empty;
    }))()
    : (((($p)[0] === "PatternAny") && ((($p)[2])[0] === "Just"))
      ? ((() => {
        const $pos = ($p)[1];
        const $n = (($p)[2])[1];
        return (($core$Dict$singleton)($n))($pos);
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
              return ((($core$List$for)($ps))((($x) => {
                return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($x));
              })))($core$Dict$empty);
            }))()
            : ((($p)[0] === "PatternRecord")
              ? ((() => {
                const $pos = ($p)[1];
                const $ps = ($p)[2];
                return ((($core$Dict$for)($ps))((($k) => {
                  return (($v) => {
                    return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($v));
                  });
                })))($core$Dict$empty);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 212:4', (sp_toHuman)($p))))))));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$insertParamNames = (($param) => {
  return ((($param)[0] === "ParameterMutable")
    ? ((() => {
      const $pos = ($param)[1];
      const $n = ($param)[2];
      return (($core$Dict$insert)($n))($pos);
    }))()
    : ((($param)[0] === "ParameterPattern")
      ? ((() => {
        const $pa = ($param)[1];
        return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($pa));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 264:4', (sp_toHuman)($param))));
});

const $core$Basics$identity = (($a) => {
  return $a;
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeUpdateTarget = (($pos) => {
  return (($env) => {
    return (($maybeShorthandTarget) => {
      const $$try1 = (($core$Maybe$map)(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)((Object.assign)({}, $env, ({
        maybeShorthandTarget: $core$Maybe$Nothing,
      })))))($maybeShorthandTarget);
      return ((($$try1)[0] === "Nothing")
        ? ($core$Result$Ok)(({
          maybeName: $core$Maybe$Nothing,
          wrapper: $core$Basics$identity,
        }))
        : (((($$try1)[0] === "Just") && ((($$try1)[1])[0] === "Err"))
          ? ((() => {
            const $e = (($$try1)[1])[1];
            return ($core$Result$Err)($e);
          }))()
          : (((($$try1)[0] === "Just") && (((($$try1)[1])[0] === "Ok") && (((($$try1)[1])[1])[0] === "Variable")))
            ? ((() => {
              const $args = ((($$try1)[1])[1])[2];
              return ($core$Result$Ok)(({
                maybeName: ($core$Maybe$Just)($args),
                wrapper: $core$Basics$identity,
              }));
            }))()
            : (((($$try1)[0] === "Just") && ((($$try1)[1])[0] === "Ok"))
              ? ((() => {
                const $expr = (($$try1)[1])[1];
                return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("NI { (expr) with ...} not yet implemented =("))($core$Core$Nil));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 646:4', (sp_toHuman)($$try1))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToValueRef = (($ro) => {
  return (($declaredInsideFunction) => {
    return (($maybeModule) => {
      return (($name) => {
        const $$try1 = (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$maybeForeignUsr)((($m) => {
          return $m.globalValues;
        })))($ro))($maybeModule))($name);
        return ((($$try1)[0] === "Just")
          ? ((() => {
            const $usr = ($$try1)[1];
            return ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($usr);
          }))()
          : ((($$try1)[0] === "Nothing")
            ? ($declaredInsideFunction
              ? ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefBlock)($name)
              : ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($ro.currentModule))($name)))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 131:4', (sp_toHuman)($$try1))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateArgument = (($env) => {
  return (($faExpr) => {
    return ((($faExpr)[0] === "Mutable")
      ? ((() => {
        const $pos = ($faExpr)[1];
        const $name = ($faExpr)[2];
        const $attrPath = ($faExpr)[3];
        return ((($core$Dict$member)($name))($env.nonRootValues)
          ? ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentMutable)($pos))(({
            attrPath: $attrPath,
            ref: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefBlock)($name),
          })))
          : (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("only values declared inside a function scope can be mutated!"))($core$Core$Nil)));
      }))()
      : (true
        ? (($core$Result$map)($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($faExpr))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 690:4', (sp_toHuman)($faExpr))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateAttrsRec = (($env) => {
  return (($faAttrs) => {
    return (($caAttrsAccum) => {
      return ((($faAttrs)[0] === "Nil")
        ? ($core$Result$Ok)($caAttrsAccum)
        : (((($faAttrs)[0] === "Cons") && ((($faAttrs)[1].first)[0] === "At"))
          ? ((() => {
            const $pos = (($faAttrs)[1].first)[1];
            const $attrName = (($faAttrs)[1].first)[2];
            const $maybeAttrExpression = ($faAttrs)[1].second;
            const $faTail = ($faAttrs)[2];
            const $exprRes = ((($maybeAttrExpression)[0] === "Just")
              ? ((() => {
                const $faExpr = ($maybeAttrExpression)[1];
                return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($faExpr);
              }))()
              : ((($maybeAttrExpression)[0] === "Nothing")
                ? ((() => {
                  const $declaredInsideFunction = (($core$Dict$member)($attrName))($env.nonRootValues);
                  return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))(({
                    attrPath: $core$Core$Nil,
                    ref: (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToValueRef)($env.ro))($declaredInsideFunction))($core$Maybe$Nothing))($attrName),
                  })));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 670:16', (sp_toHuman)($maybeAttrExpression))));
            return (($core$Result$onOk)((($expr) => {
              return ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateAttrsRec)($env))($faTail))(((($core$Dict$insert)($attrName))($expr))($caAttrsAccum));
            })))($exprRes);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 664:4', (sp_toHuman)($faAttrs))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$notAllSeparators = (($f) => {
  return (($ls) => {
    return ((($ls)[0] === "Nil")
      ? false
      : ((($ls)[0] === "Cons")
        ? ((() => {
          const $sep = ($ls)[1].first;
          const $item = ($ls)[1].second;
          const $tail = ($ls)[2];
          return (($f)($sep)
            ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$notAllSeparators)($f))($tail)
            : true);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 788:4', (sp_toHuman)($ls))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$sameDirectionAs = (($a) => {
  return (($b) => {
    return (((sp_equal)($b.symbol))($a.symbol)
      ? true
      : ((() => {
        const $$try1 = $a.symbol;
        return ((">" === $$try1)
          ? ((sp_equal)(">="))($b.symbol)
          : ((">=" === $$try1)
            ? ((sp_equal)(">"))($b.symbol)
            : (("<" === $$try1)
              ? ((sp_equal)("<="))($b.symbol)
              : (("<=" === $$try1)
                ? ((sp_equal)("<"))($b.symbol)
                : (true
                  ? false
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 806:8', (sp_toHuman)($$try1)))))));
      }))());
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeBinop = (($pos) => {
  return (($left) => {
    return (($op) => {
      return (($right) => {
        const $$try1 = ({
          first: $left,
          second: $op.symbol,
          third: $right,
        });
        return (((">>" === $$try1.second) && (($$try1.third)[0] === "ArgumentExpression"))
          ? ((() => {
            const $rightExpr = ($$try1.third)[1];
            return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))($rightExpr))($left);
          }))()
          : (((($$try1.first)[0] === "ArgumentExpression") && ("<<" === $$try1.second))
            ? ((() => {
              const $leftExpr = ($$try1.first)[1];
              return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))($leftExpr))($right);
            }))()
            : (true
              ? ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))(({
                attrPath: $core$Core$Nil,
                ref: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($op.usr),
              }))))($right)))($left)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 860:4', (sp_toHuman)($$try1)))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepListRec = (($env) => {
  return (($pos) => {
    return (($leftAccum) => {
      return (($opsAndRight) => {
        return ((($opsAndRight)[0] === "Nil")
          ? ($core$Result$Ok)($leftAccum)
          : ((($opsAndRight)[0] === "Cons")
            ? ((() => {
              const $op = ($opsAndRight)[1].first;
              const $faRight = ($opsAndRight)[1].second;
              const $tail = ($opsAndRight)[2];
              return (($core$Result$onOk)((($caRight) => {
                return (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepListRec)($env))($pos))((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeBinop)($pos))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)($leftAccum)))($op))($caRight)))($tail);
              })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateArgument)($env))($faRight));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 844:4', (sp_toHuman)($opsAndRight))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_leftAssociative = (($env) => {
  return (($pos) => {
    return (($leftAccum) => {
      return (($opsAndRight) => {
        return (($core$Result$onOk)((($caLeftAccum) => {
          return (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepListRec)($env))($pos))($caLeftAccum))($opsAndRight);
        })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($leftAccum));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_rightAssociative = (($env) => {
  return (($pos) => {
    return (($left) => {
      return (($opsAndRight) => {
        return (($core$Result$onOk)((($caLeft) => {
          return ((($opsAndRight)[0] === "Nil")
            ? ($core$Result$Ok)($caLeft)
            : ((($opsAndRight)[0] === "Cons")
              ? ((() => {
                const $op = ($opsAndRight)[1].first;
                const $right = ($opsAndRight)[1].second;
                const $tail = ($opsAndRight)[2];
                return (($core$Result$onOk)((($caRight) => {
                  return ($core$Result$Ok)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeBinop)($pos))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)($caLeft)))($op))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)($caRight)));
                })))((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env))($pos))($right))($tail));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 826:4', (sp_toHuman)($opsAndRight))));
        })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($left));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateSimpleBinop = (($env) => {
  return (($pos) => {
    return (($left) => {
      return (($op) => {
        return (($right) => {
          return (($core$Result$onOk)((($l) => {
            return (($core$Result$onOk)((($r) => {
              return ($core$Result$Ok)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeBinop)($pos))($l))($op))($r));
            })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateArgument)($env))($right));
          })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateArgument)($env))($left));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinops = (($env) => {
  return (($pos) => {
    return (($group) => {
      return (($$firstItem) => {
        const $firstItem = $$firstItem.first;
        const $firstTail = $$firstItem.second;
        return ((($firstTail)[0] === "Nil")
          ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($firstItem)
          : (((($firstTail)[0] === "Cons") && ((($firstTail)[2])[0] === "Nil"))
            ? ((() => {
              const $firstSep = ($firstTail)[1].first;
              const $secondItem = ($firstTail)[1].second;
              return ((($group)[0] === "Tuple")
                ? (($core$Result$onOk)((($first) => {
                  return (($core$Result$onOk)((($second) => {
                    return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($pos))($core$Maybe$Nothing))(((($core$Dict$insert)("second"))($second))(((($core$Dict$insert)("first"))($first))($core$Dict$empty))));
                  })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($secondItem));
                })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($firstItem))
                : (true
                  ? ((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateSimpleBinop)($env))($pos))($firstItem))($firstSep))($secondItem)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 719:12', (sp_toHuman)($group))));
            }))()
            : (((($firstTail)[0] === "Cons") && ((($firstTail)[2])[0] === "Cons"))
              ? ((() => {
                const $firstSep = ($firstTail)[1].first;
                const $secondItem = ($firstTail)[1].second;
                const $secondSep = (($firstTail)[2])[1].first;
                const $thirdItem = (($firstTail)[2])[1].second;
                const $thirdTail = (($firstTail)[2])[2];
                const $secondTail = ((sp_cons)($thirdTail))(({
                  first: $secondSep,
                  second: $thirdItem,
                }));
                return ((($group)[0] === "Comparison")
                  ? ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$notAllSeparators)(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$sameDirectionAs)($firstSep)))($secondTail)
                    ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("can't mix comparison ops with different direction"))($core$Core$Nil))
                    : (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("NI compops expansion"))($core$Core$Nil)))
                  : ((($group)[0] === "Logical")
                    ? ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$notAllSeparators)((($x) => {
                      return ((sp_equal)($firstSep))($x);
                    })))($secondTail)
                      ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Mixing `and` and `or` is ambiguous. Use parens!"))($core$Core$Nil))
                      : (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env))($pos))($firstItem))($firstTail))
                    : ((($group)[0] === "Tuple")
                      ? (((sp_not_equal)($core$Core$Nil))($thirdTail)
                        ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Tuples can't have more than 3 items, use a record instead."))($core$Core$Nil))
                        : (($core$Result$onOk)((($first) => {
                          return (($core$Result$onOk)((($second) => {
                            return (($core$Result$onOk)((($third) => {
                              return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($pos))($core$Maybe$Nothing))(((($core$Dict$insert)("third"))($third))(((($core$Dict$insert)("second"))($second))(((($core$Dict$insert)("first"))($first))($core$Dict$empty)))));
                            })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($thirdItem));
                          })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($secondItem));
                        })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($firstItem)))
                      : ((($group)[0] === "Pipe")
                        ? ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$notAllSeparators)((($x) => {
                          return ((sp_equal)($firstSep))($x);
                        })))($secondTail)
                          ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Mixing pipes is ambigous. Use parens."))($core$Core$Nil))
                          : (((sp_equal)($$home$nw$stuff$unstable$src$Types$Op$Right))($firstSep.associativity)
                            ? (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env))($pos))($firstItem))($firstTail)
                            : (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_leftAssociative)($env))($pos))($firstItem))($firstTail)))
                        : ((($group)[0] === "Mutop")
                          ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("mutops can't be chained"))($core$Core$Nil))
                          : (true
                            ? (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinopSepList_rightAssociative)($env))($pos))($firstItem))($firstTail)
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 737:12', (sp_toHuman)($group))))))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 714:4', (sp_toHuman)($firstTail)))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateParameter = (($env) => {
  return (($mutable) => {
    return (($faParam) => {
      const $$try1 = ({
        first: $faParam,
        second: $mutable,
      });
      return (((($$try1.first)[0] === "PatternAny") && (!(($$try1.first)[2]) && (((($$try1.first)[4])[0] === "Nothing") && $$try1.second)))
        ? ((() => {
          const $pos = ($$try1.first)[1];
          const $name = ($$try1.first)[3];
          return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$ParameterMutable)($pos))($name));
        }))()
        : (((($$try1.first)[0] === "PatternAny") && ($$try1.first)[2])
          ? ((() => {
            const $pos = ($$try1.first)[1];
            const $name = ($$try1.first)[3];
            return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Can't annotate this. =("))((($core$Core$Cons)("TODO link to rationale for forbidding annotations"))($core$Core$Nil)));
          }))()
          : (true
            ? (($core$Result$onOk)((($caPattern) => {
              return ($core$Result$Ok)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ParameterPattern)($caPattern));
            })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($core$Maybe$Nothing))($env))($faParam))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 450:4', (sp_toHuman)($$try1)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression = (($env) => {
  return (($faExpr) => {
    return ((($faExpr)[0] === "LiteralNumber")
      ? ((() => {
        const $pos = ($faExpr)[1];
        const $str = ($faExpr)[2];
        return ((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateNumber)($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber))($pos))($str);
      }))()
      : ((($faExpr)[0] === "LiteralText")
        ? ((() => {
          const $pos = ($faExpr)[1];
          const $v = ($faExpr)[2];
          return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralText)($pos))($v));
        }))()
        : ((($faExpr)[0] === "PrefixBinop")
          ? ((() => {
            const $pos = ($faExpr)[1];
            const $symbol = ($faExpr)[2];
            return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))(({
              attrPath: $core$Core$Nil,
              ref: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)($symbol)),
            })));
          }))()
          : ((($faExpr)[0] === "Variable")
            ? ((() => {
              const $pos = ($faExpr)[1];
              const $maybeModule = ($faExpr)[2];
              const $name = ($faExpr)[3];
              const $attrs = ($faExpr)[4];
              const $declaredInsideFunction = (($core$Dict$member)($name))($env.nonRootValues);
              return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))(({
                attrPath: $attrs,
                ref: (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToValueRef)($env.ro))($declaredInsideFunction))($maybeModule))($name),
              })));
            }))()
            : ((($faExpr)[0] === "Constructor")
              ? ((() => {
                const $pos = ($faExpr)[1];
                const $maybeModule = ($faExpr)[2];
                const $name = ($faExpr)[3];
                return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Constructor)($pos))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$resolveToConstructorUsr)($env.ro))($maybeModule))($name)));
              }))()
              : ((($faExpr)[0] === "Mutable")
                ? ((() => {
                  const $pos = ($faExpr)[1];
                  const $name = ($faExpr)[2];
                  return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)(($name + ": mutable values can be used only as arguments for function or mutation operators")))($core$Core$Nil));
                }))()
                : ((($faExpr)[0] === "RecordShorthand")
                  ? ((() => {
                    const $pos = ($faExpr)[1];
                    const $attrPath = ($faExpr)[2];
                    const $$try1 = $env.maybeShorthandTarget;
                    return ((($$try1)[0] === "Nothing")
                      ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Record update shorthands must be used inside a record update such as"))((($core$Core$Cons)(("    { aRecord with anAttribute = doSomethingWith ." + ((($core$Text$join)("."))($attrPath) + " }"))))((($core$Core$Cons)("but we are not inside a record update!"))($core$Core$Nil))))
                      : ((($$try1)[0] === "Just")
                        ? ((() => {
                          const $shorthandTarget = ($$try1)[1];
                          return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))((Object.assign)({}, $shorthandTarget, ({
                            attrPath: ($core$List$concat)((($core$Core$Cons)($shorthandTarget.attrPath))((($core$Core$Cons)($attrPath))($core$Core$Nil))),
                          }))));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 544:12', (sp_toHuman)($$try1))));
                  }))()
                  : ((($faExpr)[0] === "Lambda")
                    ? ((() => {
                      const $pos = ($faExpr)[1];
                      const $faParam = ($faExpr)[2];
                      const $mutable = ($faExpr)[3];
                      const $faBody = ($faExpr)[4];
                      return (($core$Result$onOk)((($caParam) => {
                        const $localEnv = (Object.assign)({}, $env, ({
                          nonRootValues: (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$insertParamNames)($caParam))($env.nonRootValues),
                        }));
                        return (($core$Result$onOk)((($caBody) => {
                          return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Lambda)($pos))($caParam))($caBody));
                        })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)($localEnv))($faBody));
                      })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateParameter)($env))($mutable))($faParam));
                    }))()
                    : ((($faExpr)[0] === "FunctionCall")
                      ? ((() => {
                        const $pos = ($faExpr)[1];
                        const $reference = ($faExpr)[2];
                        const $arguments = ($faExpr)[3];
                        const $fold = (($argument) => {
                          return (($refAccum) => {
                            return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))($refAccum))($argument);
                          });
                        });
                        return (($core$Result$onOk)((($ref) => {
                          return (($core$Result$onOk)((($args) => {
                            return ($core$Result$Ok)(((($core$List$for)($args))($fold))($ref));
                          })))((($core$List$mapRes)(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateArgument)($env)))($arguments));
                        })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($reference));
                      }))()
                      : ((($faExpr)[0] === "If")
                        ? ((() => {
                          const $pos = ($faExpr)[1];
                          const $condition = ($faExpr)[2].condition;
                          const $false = ($faExpr)[2].false;
                          const $isCompact = ($faExpr)[2].isCompact;
                          const $true = ($faExpr)[2].true;
                          return (($core$Result$onOk)((($c) => {
                            return (($core$Result$onOk)((($t) => {
                              return (($core$Result$onOk)((($f) => {
                                return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$If)($pos))(({
                                  condition: $c,
                                  false: $f,
                                  true: $t,
                                })));
                              })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)($env))($false));
                            })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)($env))($true));
                          })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($condition));
                        }))()
                        : ((($faExpr)[0] === "Unop")
                          ? ((() => {
                            const $pos = ($faExpr)[1];
                            const $op = ($faExpr)[2];
                            const $faOperand = ($faExpr)[3];
                            return (($core$Result$onOk)((($caOperand) => {
                              return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))(({
                                attrPath: $core$Core$Nil,
                                ref: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($op.usr),
                              }))))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)($caOperand)));
                            })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($faOperand));
                          }))()
                          : ((($faExpr)[0] === "Binop")
                            ? ((() => {
                              const $pos = ($faExpr)[1];
                              const $group = ($faExpr)[2];
                              const $sepList = ($faExpr)[3];
                              return (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateBinops)($env))($pos))($group))($sepList);
                            }))()
                            : ((($faExpr)[0] === "Record")
                              ? ((() => {
                                const $pos = ($faExpr)[1];
                                const $faArgs = ($faExpr)[2];
                                return (($core$Result$onOk)((($caUpdateTarget) => {
                                  return (($core$Result$onOk)((($caAttrs) => {
                                    return ($core$Result$Ok)(($caUpdateTarget.wrapper)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($pos))($caUpdateTarget.maybeName))($caAttrs)));
                                  })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateAttrsRec)((Object.assign)({}, $env, ({
                                    maybeShorthandTarget: $caUpdateTarget.maybeName,
                                  }))))($faArgs.attrs))($core$Dict$empty));
                                })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeUpdateTarget)($pos))($env))($faArgs.extends));
                              }))()
                              : ((($faExpr)[0] === "List")
                                ? ((() => {
                                  const $pos = ($faExpr)[1];
                                  const $faItems = ($faExpr)[2];
                                  const $cons = (($item) => {
                                    return (($list) => {
                                      return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($pos))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Constructor)($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$cons)))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)($item))))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)($list));
                                    });
                                  });
                                  return (($core$Result$onOk)((($es) => {
                                    return ($core$Result$Ok)(((($core$List$forReversed)($es))($cons))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Constructor)($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$nil)));
                                  })))((($core$List$mapRes)(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env)))($faItems));
                                }))()
                                : ((($faExpr)[0] === "Try")
                                  ? ((() => {
                                    const $pos = ($faExpr)[1];
                                    const $fa = ($faExpr)[2];
                                    const $translatePatternAndStatements = (($$faPattern) => {
                                      const $faPattern = $$faPattern.first;
                                      const $faStatements = $$faPattern.second;
                                      return (($core$Result$onOk)((($caPattern) => {
                                        return (($core$Result$onOk)((($block) => {
                                          return ($core$Result$Ok)(({
                                            first: $caPattern,
                                            second: $block,
                                          }));
                                        })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)((Object.assign)({}, $env, ({
                                          nonRootValues: (($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($caPattern)))($env.nonRootValues),
                                        }))))($faStatements));
                                      })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)($core$Maybe$Nothing))($env))($faPattern));
                                    });
                                    return (($core$Result$onOk)((($caValue) => {
                                      return (($core$Result$onOk)((($caPatternsAndStatements) => {
                                        return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Try)($pos))($caValue))($caPatternsAndStatements));
                                      })))((($core$List$mapRes)($translatePatternAndStatements))($fa.patterns));
                                    })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($fa.value));
                                  }))()
                                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 511:4', (sp_toHuman)($faExpr)))))))))))))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock = (($env) => {
  return (($stats) => {
    return (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "TypeAlias"))
      ? ((() => {
        const $fa = (($stats)[1])[1];
        const $$pos = $fa.name;
        const $pos = ($$pos)[1];
        return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Aliases can be declared only in the root scope"))($core$Core$Nil));
      }))()
      : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "UnionDef"))
        ? ((() => {
          const $pos = (($stats)[1])[1];
          const $fa = (($stats)[1])[2];
          return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)("Types can be declared only in the root scope"))($core$Core$Nil));
        }))()
        : ((($stats)[0] === "Nil")
          ? ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Constructor)($$home$nw$stuff$unstable$src$Types$Pos$G))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneValue))
          : (((($stats)[0] === "Cons") && (((($stats)[1])[0] === "Evaluation") && ((($stats)[2])[0] === "Nil")))
            ? ((() => {
              const $pos = (($stats)[1])[1];
              const $faExpr = (($stats)[1])[2];
              return (($core$Result$onOk)((($e) => {
                return ($core$Result$Ok)($e);
              })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateExpression)($env))($faExpr));
            }))()
            : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "Evaluation"))
              ? ((() => {
                const $pos = (($stats)[1])[1];
                const $faExpr = (($stats)[1])[2];
                const $tail = ($stats)[2];
                const $valueDef = ({
                  body: (($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($pos))($faExpr)))($core$Core$Nil),
                  modifier: $$home$nw$stuff$unstable$src$Types$Token$DefNormal,
                  nonFn: $core$Core$Nil,
                  pattern: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Types$Pos$G))(false))("_"))($core$Maybe$Nothing),
                });
                return (($core$Result$onOk)((($d) => {
                  return (($core$Result$onOk)((($tailBlockExpression) => {
                    return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$LetIn)($d))($tailBlockExpression));
                  })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)($env))($tail));
                })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateDefinition)(false))($env))($valueDef));
              }))()
              : (((($stats)[0] === "Cons") && ((($stats)[1])[0] === "Definition"))
                ? ((() => {
                  const $pos = (($stats)[1])[1];
                  const $fa = (($stats)[1])[2];
                  const $tail = ($stats)[2];
                  return (($core$Result$onOk)((($d) => {
                    return (($core$Result$onOk)((($tailBlockExpression) => {
                      return ($core$Result$Ok)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$LetIn)($d))($tailBlockExpression));
                    })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)((Object.assign)({}, $env, ({
                      nonRootValues: (($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($d.pattern)))($env.nonRootValues),
                    }))))($tail));
                  })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateDefinition)(false))($env))($fa));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 469:4', (sp_toHuman)($stats))))))));
  });
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos = (($pa) => {
  return ((($pa)[0] === "PatternAny")
    ? ((() => {
      const $p = ($pa)[1];
      const $n = ($pa)[2];
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
            const $path = ($pa)[2];
            const $ps = ($pa)[3];
            return $p;
          }))()
          : ((($pa)[0] === "PatternRecord")
            ? ((() => {
              const $p = ($pa)[1];
              const $ps = ($pa)[2];
              return $p;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 202:4', (sp_toHuman)($pa)))))));
});

const $$home$nw$stuff$unstable$src$Types$Pos$start = (($pos) => {
  return ((($pos)[0] === "P")
    ? ((() => {
      const $m = ($pos)[1];
      const $s = ($pos)[2];
      const $e = ($pos)[3];
      return $s;
    }))()
    : (true
      ? 0
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/Pos.sp 29:4', (sp_toHuman)($pos))));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateDefinition = (($isRoot) => {
  return (($env) => {
    return (($fa) => {
      const $dict = ({
        attr: "$",
        obj: ({
          $: (sp_clone)($env.tyvarRenames),
        }),
      });
      const $renameTyvar = (($pos) => {
        return (($faName) => {
          const $$try2 = (($core$Dict$get)($faName))((sp_clone)(($dict.obj)[$dict.attr]));
          return ((($$try2)[0] === "Just")
            ? ((() => {
              const $n = ($$try2)[1];
              return $n;
            }))()
            : ((($$try2)[0] === "Nothing")
              ? ((() => {
                const $n = ((text_fromNumber)(($$home$nw$stuff$unstable$src$Types$Pos$start)($pos)) + $faName);
                (($dict.obj)[$dict.attr] = ((($core$Dict$insert)($faName))($n))((sp_clone)(($dict.obj)[$dict.attr])));
                return $n;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 287:8', (sp_toHuman)($$try2))));
        });
      });
      return (($core$Result$onOk)((($pattern) => {
        const $nonRootValues1 = ($isRoot
          ? $env.nonRootValues
          : (($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($pattern)))($env.nonRootValues));
        const $localEnv0 = (Object.assign)({}, $env, ({
          defsPath: ((sp_cons)($env.defsPath))($pattern),
          nonRootValues: $nonRootValues1,
          tyvarRenames: (sp_clone)(($dict.obj)[$dict.attr]),
        }));
        const $updNonFn = (($tyvarName) => {
          return (($nonFn) => {
            const $$try1 = (($core$Dict$get)($tyvarName))($localEnv0.tyvarRenames);
            return ((($$try1)[0] === "Nothing")
              ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($pattern)))((($core$Core$Cons)("non fn on variable that's not in the annotation"))($core$Core$Nil))
              : ((($$try1)[0] === "Just")
                ? ((() => {
                  const $tr = ($$try1)[1];
                  return ($core$Result$Ok)(((($core$Dict$insert)($tr))(null))($nonFn));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 311:8', (sp_toHuman)($$try1))));
          });
        });
        return (($core$Result$onOk)((($nonFn) => {
          return (($core$Result$onOk)((($body) => {
            const $deps = ($isRoot
              ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$expressionDeps)($body))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$patternDeps)($pattern))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$deps_init))
              : $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$deps_init);
            return ($core$Result$Ok)(({
              body: $body,
              directConsDeps: $deps.cons,
              directTypeDeps: $deps.types,
              directValueDeps: $deps.values,
              mutable: ((sp_equal)($$home$nw$stuff$unstable$src$Types$Token$DefMutable))($fa.modifier),
              native: false,
              nonFn: $nonFn,
              parentDefinitions: $env.defsPath,
              pattern: $pattern,
            }));
          })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateStatementBlock)($localEnv0))($fa.body));
        })))(((($core$List$forRes)($fa.nonFn))($updNonFn))($core$Dict$empty));
      })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translatePattern)(($core$Maybe$Just)($renameTyvar)))($env))($fa.pattern));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$insertRootStatement = (($ro) => {
  return (($faStatement) => {
    return (($caModule) => {
      return ((($faStatement)[0] === "Evaluation")
        ? ((() => {
          const $pos = ($faStatement)[1];
          const $expr = ($faStatement)[2];
          return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)(($$home$nw$stuff$unstable$src$Types$FormattableAst$expressionPos)($expr)))((($core$Core$Cons)("Root Evaluations don't really do much =|"))($core$Core$Nil));
        }))()
        : ((($faStatement)[0] === "Definition")
          ? ((() => {
            const $pos = ($faStatement)[1];
            const $fa = ($faStatement)[2];
            return (($core$Result$onOk)((($def) => {
              return ($def.mutable
                ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($def.pattern)))((($core$Core$Cons)("Mutable values can be declared only inside functions."))($core$Core$Nil))
                : ($core$Result$Ok)((Object.assign)({}, $caModule, ({
                  valueDefs: ((($core$Dict$insert)($def.pattern))($def))($caModule.valueDefs),
                }))));
            })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateDefinition)(true))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$initEnv)($ro)))($fa));
          }))()
          : ((($faStatement)[0] === "TypeAlias")
            ? ((() => {
              const $fa = ($faStatement)[1];
              const $$name = $fa.name;
              const $name = ($$name)[2];
              const $pos = ($$name)[1];
              return (((($core$Dict$member)($name))($caModule.aliasDefs) || (($core$Dict$member)($name))($caModule.unionDefs))
                ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)(($name + " declared twice!")))($core$Core$Nil))
                : (($core$Result$onOk)((($type) => {
                  const $aliasDef = ({
                    args: $fa.args,
                    directTypeDeps: (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps)($type))($core$Set$empty),
                    type: $type,
                    usr: (($$home$nw$stuff$unstable$src$Types$Meta$USR)($ro.currentModule))(($$home$nw$stuff$unstable$src$Types$Pos$drop)($fa.name)),
                  });
                  return ($core$Result$Ok)((Object.assign)({}, $caModule, ({
                    aliasDefs: ((($core$Dict$insert)($name))($aliasDef))($caModule.aliasDefs),
                  })));
                })))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateType)($core$Maybe$Nothing))($ro))($fa.ty)));
            }))()
            : ((($faStatement)[0] === "UnionDef")
              ? ((() => {
                const $pos = ($faStatement)[1];
                const $fa = ($faStatement)[2];
                return (((($core$Dict$member)($fa.name))($caModule.aliasDefs) || (($core$Dict$member)($fa.name))($caModule.unionDefs))
                  ? (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$makeError)($pos))((($core$Core$Cons)(($fa.name + " declared twice!")))($core$Core$Nil))
                  : ((() => {
                    const $usr = (($$home$nw$stuff$unstable$src$Types$Meta$USR)($ro.currentModule))($fa.name);
                    const $type = ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))($usr))((($core$List$map)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos)))($fa.args));
                    return (($core$Result$onOk)((($constructors) => {
                      const $unionDef = ({
                        args: $fa.args,
                        constructors: $constructors,
                        directTypeDeps: ((($core$Dict$for)($constructors))((($k) => {
                          return (($c) => {
                            return (($core$List$for)($c.args))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$typeDeps);
                          });
                        })))($core$Set$empty),
                        usr: $usr,
                      });
                      return ($core$Result$Ok)((Object.assign)({}, $caModule, ({
                        unionDefs: ((($core$Dict$insert)($fa.name))($unionDef))($caModule.unionDefs),
                      })));
                    })))(((($core$List$forRes)($fa.constructors))(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateConstructor)($ro))($type))($usr)))($core$Dict$empty));
                  }))());
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical.sp 1007:4', (sp_toHuman)($faStatement))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$initModule = (($asText) => {
  return (($umr) => {
    return ({
      aliasDefs: $core$Dict$empty,
      asText: $asText,
      umr: $umr,
      unionDefs: $core$Dict$empty,
      valueDefs: $core$Dict$empty,
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateModule = (($ro) => {
  return (($asText) => {
    return (($umr) => {
      return (($faModule) => {
        (sp_benchStart)(null);
        const $module = (($$home$nw$stuff$unstable$src$Types$CanonicalAst$initModule)($asText))($umr);
        return ((($core$Basics$btw)(sp_benchStop))("translateModule"))(((($core$List$forRes)($faModule))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$insertRootStatement)($ro)))($module));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$textToFormattableModule = (($pars) => {
  return (($code) => {
    const $tokensResult = (($$home$nw$stuff$unstable$src$Compiler$Lexer$lexer)($pars.name))($code);
    const $tokensToStatsResult = (($tokens) => {
      (sp_benchStart)(null);
      return ((($core$Basics$btw)(sp_benchStop))("parse"))(((($$home$nw$stuff$unstable$src$Compiler$Parser$parse)($pars.stripLocations))($pars.name))($tokens));
    });
    return (($core$Result$onOk)($tokensToStatsResult))($tokensResult);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical$textToCanonicalModule = (($pars) => {
  return (($code) => {
    const $ro = ({
      currentModule: (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($pars.source))($pars.name),
      meta: $pars.meta,
    });
    const $umr = (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($pars.source))($pars.name);
    return (($core$Result$onOk)(((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$translateModule)($ro))($code))($umr)))((($$home$nw$stuff$unstable$src$Compiler$Parser$textToFormattableModule)(({
      name: $pars.name,
      stripLocations: $pars.stripLocations,
    })))($code));
  });
});

const $$home$nw$stuff$unstable$src$Compile$loadModule = (($meta) => {
  return (($umr) => {
    return (($fileName) => {
      const $$moduleName = $umr;
      const $moduleName = ($$moduleName)[2];
      const $source = ($$moduleName)[1];
      return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($moduleAsText) => {
        const $params = ({
          meta: $meta,
          name: $moduleName,
          source: $source,
          stripLocations: false,
        });
        const $eenv = ({
          moduleByName: (($core$Dict$singleton)($moduleName))(({
            content: $moduleAsText,
            fsPath: $fileName,
          })),
        });
        return (($$home$nw$stuff$unstable$src$Compile$resToIo)($eenv))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$textToCanonicalModule)($params))($moduleAsText));
      })))(($$home$nw$stuff$unstable$lib$posix$IO$readFile)($fileName));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compile$mergeWithCore = (($coreModule) => {
  return (($userModule) => {
    return (Object.assign)({}, $userModule, ({
      aliasDefs: (($core$Dict$join)($coreModule.aliasDefs))($userModule.aliasDefs),
      unionDefs: (($core$Dict$join)($coreModule.unionDefs))($userModule.unionDefs),
      valueDefs: (($core$Dict$join)($coreModule.valueDefs))($userModule.valueDefs),
    }));
  });
});

const $$home$nw$stuff$unstable$src$Compile$onResSuccess = (($errorEnv) => {
  return (($f) => {
    return (($res) => {
      return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)($f))((($$home$nw$stuff$unstable$src$Compile$resToIo)($errorEnv))($res));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compile$searchAncestorDirectories = (($isWantedFile) => {
  return (($searchDir) => {
    return (($$home$nw$stuff$unstable$lib$posix$IO$onResult)((($result) => {
      return ((($result)[0] === "Err")
        ? ($$home$nw$stuff$unstable$lib$posix$IO$succeed)($core$Maybe$Nothing)
        : ((($result)[0] === "Ok")
          ? ((() => {
            const $dirContents = ($result)[1];
            return ((($core$List$any)($isWantedFile))($dirContents)
              ? ($$home$nw$stuff$unstable$lib$posix$IO$succeed)(($core$Maybe$Just)($searchDir))
              : ((() => {
                const $parent = ($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($searchDir))((($core$Core$Cons)(".."))($core$Core$Nil)));
                return (((sp_equal)($searchDir))($parent)
                  ? ($$home$nw$stuff$unstable$lib$posix$IO$succeed)($core$Maybe$Nothing)
                  : (($$home$nw$stuff$unstable$src$Compile$searchAncestorDirectories)($isWantedFile))($parent));
              }))());
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 267:4', (sp_toHuman)($result))));
    })))(($$home$nw$stuff$unstable$lib$posix$IO$readDir)($searchDir));
  });
});

const $$home$nw$stuff$unstable$src$StateMonad$andThen = (($f) => {
  return (($m) => {
    return (($state0) => {
      const $$a = ($m)($state0);
      const $state1 = $$a.second;
      const $a = $$a.first;
      return (($f)($a))($state1);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen = $$home$nw$stuff$unstable$src$StateMonad$andThen;

const $$home$nw$stuff$unstable$src$StateMonad$get = (($getter) => {
  return (($state) => {
    return ({
      first: ($getter)($state),
      second: $state,
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$get = $$home$nw$stuff$unstable$src$StateMonad$get;

const $$home$nw$stuff$unstable$src$StateMonad$return = (($a) => {
  return (($state) => {
    return ({
      first: $a,
      second: $state,
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$return = $$home$nw$stuff$unstable$src$StateMonad$return;

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars = (($ty) => {
  return ((($ty)[0] === "TypeVariable")
    ? ((() => {
      const $pos = ($ty)[1];
      const $name = ($ty)[2];
      return (($core$Dict$singleton)($name))($pos);
    }))()
    : ((($ty)[0] === "TypeFunction")
      ? ((() => {
        const $from = ($ty)[2];
        const $fromIsMutable = ($ty)[3];
        const $to = ($ty)[4];
        return (($core$Dict$join)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($from)))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($to));
      }))()
      : ((($ty)[0] === "TypeConstant")
        ? ((() => {
          const $pos = ($ty)[1];
          const $ref = ($ty)[2];
          const $args = ($ty)[3];
          return ((($core$List$for)($args))((($a) => {
            return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($a));
          })))($core$Dict$empty);
        }))()
        : ((($ty)[0] === "TypeAlias")
          ? ((() => {
            const $path = ($ty)[2];
            const $t = ($ty)[3];
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($t);
          }))()
          : ((($ty)[0] === "TypeRecord")
            ? ((() => {
              const $pos = ($ty)[1];
              const $extensible = ($ty)[2];
              const $attrs = ($ty)[3];
              const $init = ((($extensible)[0] === "Nothing")
                ? $core$Dict$empty
                : ((($extensible)[0] === "Just")
                  ? ((() => {
                    const $name = ($extensible)[1];
                    return (($core$Dict$singleton)($name))($pos);
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 351:16', (sp_toHuman)($extensible))));
              return ((($core$Dict$for)($attrs))((($n) => {
                return (($t) => {
                  return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($t));
                });
              })))($init);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 335:4', (sp_toHuman)($ty)))))));
});

const $core$Dict$forReversed = (($t) => {
  return (($func) => {
    return (($acc) => {
      return ((($t)[0] === "RBEmpty_elm_builtin")
        ? $acc
        : ((($t)[0] === "RBNode_elm_builtin")
          ? ((() => {
            const $key = ($t)[2];
            const $value = ($t)[3];
            const $left = ($t)[4];
            const $right = ($t)[5];
            return ((($core$Dict$forReversed)($left))($func))(((($func)($key))($value))(((($core$Dict$forReversed)($right))($func))($acc)));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 476:2', (sp_toHuman)($t))));
    });
  });
});

const $core$Dict$keys = (($dict) => {
  return ((($core$Dict$forReversed)($dict))((($key) => {
    return (($value) => {
      return (($keyList) => {
        return ((sp_cons)($keyList))($key);
      });
    });
  })))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToNonFreeTyvars = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
    const $meh = (($typeVarName) => {
      return (($constrainedVars) => {
        const $$try1 = (($core$Dict$get)($typeVarName))($subs);
        return ((($$try1)[0] === "Nothing")
          ? $constrainedVars
          : ((($$try1)[0] === "Just")
            ? ((() => {
              const $ty = ($$try1)[1];
              return ((($core$Dict$for)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($ty)))((($n) => {
                return (($p) => {
                  return (($core$Dict$insert)($n))($p);
                });
              })))($constrainedVars);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2179:8', (sp_toHuman)($$try1))));
      });
    });
    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((Object.assign)({}, $env, ({
      nonFreeTyvars: ((($core$List$for)(($core$Dict$keys)($env.nonFreeTyvars)))($meh))($env.nonFreeTyvars),
    })));
  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
    return $x.substitutions;
  })));
});

const $core$Dict$map = (($func) => {
  return (($dict) => {
    return ((($dict)[0] === "RBEmpty_elm_builtin")
      ? $core$Dict$RBEmpty_elm_builtin
      : ((($dict)[0] === "RBNode_elm_builtin")
        ? ((() => {
          const $color = ($dict)[1];
          const $key = ($dict)[2];
          const $value = ($dict)[3];
          const $left = ($dict)[4];
          const $right = ($dict)[5];
          return ((((($core$Dict$RBNode_elm_builtin)($color))($key))((($func)($key))($value)))((($core$Dict$map)($func))($left)))((($core$Dict$map)($func))($right));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 426:2', (sp_toHuman)($dict))));
  });
});

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

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables = (($subs) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeConstant")
      ? ((() => {
        const $pos = ($ty)[1];
        const $ref = ($ty)[2];
        const $args = ($ty)[3];
        return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))($ref))((($core$List$map)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs)))($args));
      }))()
      : ((($ty)[0] === "TypeVariable")
        ? ((() => {
          const $name = ($ty)[2];
          const $$try2 = (($core$Dict$get)($name))($subs);
          return ((($$try2)[0] === "Just")
            ? ((() => {
              const $substitutionType = ($$try2)[1];
              return $substitutionType;
            }))()
            : ((($$try2)[0] === "Nothing")
              ? $ty
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2224:12', (sp_toHuman)($$try2))));
        }))()
        : ((($ty)[0] === "TypeFunction")
          ? ((() => {
            const $pos = ($ty)[1];
            const $from = ($ty)[2];
            const $fromIsMutable = ($ty)[3];
            const $to = ($ty)[4];
            return (((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs))($from)))($fromIsMutable))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs))($to));
          }))()
          : ((($ty)[0] === "TypeAlias")
            ? ((() => {
              const $pos = ($ty)[1];
              const $path = ($ty)[2];
              const $t = ($ty)[3];
              return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeAlias)($pos))($path))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs))($t));
            }))()
            : ((($ty)[0] === "TypeRecord")
              ? ((() => {
                const $pos = ($ty)[1];
                const $extensible = ($ty)[2];
                const $attrs = ($ty)[3];
                const $$try1 = (($core$Maybe$andThen)((($name) => {
                  return (($core$Dict$get)($name))($subs);
                })))($extensible);
                return ((($$try1)[0] === "Nothing")
                  ? ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($extensible))((($core$Dict$map)((($name) => {
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs);
                  })))($attrs))
                  : (((($$try1)[0] === "Just") && ((($$try1)[1])[0] === "TypeVariable"))
                    ? ((() => {
                      const $p = (($$try1)[1])[1];
                      const $n = (($$try1)[1])[2];
                      return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))(($core$Maybe$Just)($n)))((($core$Dict$map)((($name) => {
                        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs);
                      })))($attrs));
                    }))()
                    : (((($$try1)[0] === "Just") && ((($$try1)[1])[0] === "TypeRecord"))
                      ? ((() => {
                        const $ext2 = (($$try1)[1])[2];
                        const $attrs2 = (($$try1)[1])[3];
                        return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($ext2))((($core$Dict$map)((($name) => {
                          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs);
                        })))((($core$Dict$join)($attrs2))($attrs)));
                      }))()
                      : ((($$try1)[0] === "Just")
                        ? ((() => {
                          const $what = ($$try1)[1];
                          ((sp_log)("what"))((sp_toHuman)($what));
                          return (sp_todo)("replacing record extension with non-var");
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2243:12', (sp_toHuman)($$try1))))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2219:4', (sp_toHuman)($ty)))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType = (($ty) => {
  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs))($ty));
  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
    return $x.substitutions;
  })));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertError = (($e) => {
  return (($state) => {
    return ({
      first: null,
      second: (Object.assign)({}, $state, ({
        errors: ((sp_cons)($state.errors))($e),
      })),
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError = (($pos) => {
  return (($message) => {
    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertError)((($$home$nw$stuff$unstable$src$Compiler$Error$Simple)($pos))((() => {
      return $message;
    })));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName = (($f) => {
  return (($state) => {
    return ({
      first: ($f)((text_fromNumber)($state.nextName)),
      second: (Object.assign)({}, $state, ({
        nextName: ($state.nextName + 1),
      })),
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType = (($pos) => {
  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$addErrorWithEEnv = (($pos) => {
  return (($messageConstructor) => {
    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos);
    })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertError)((($$home$nw$stuff$unstable$src$Compiler$Error$Simple)($pos))($messageConstructor)));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError = (($pos) => {
  return (($message) => {
    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addErrorWithEEnv)($pos))((() => {
      return $message;
    }));
  });
});

const $$home$nw$stuff$unstable$src$StateMonad$list_foldl = (($f) => {
  return (($items) => {
    return (($accum) => {
      return ((($items)[0] === "Nil")
        ? ($$home$nw$stuff$unstable$src$StateMonad$return)($accum)
        : ((($items)[0] === "Cons")
          ? ((() => {
            const $head = ($items)[1];
            const $tail = ($items)[2];
            return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($$home$nw$stuff$unstable$src$StateMonad$list_foldl)($f))($tail)))((($f)($head))($accum));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/StateMonad.sp 81:4', (sp_toHuman)($items))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for = (($l) => {
  return (($f) => {
    return (($$home$nw$stuff$unstable$src$StateMonad$list_foldl)($f))($l);
  });
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$parensIf = (($test) => {
  return (($s) => {
    return ($test
      ? ("(" + ($s + ")"))
      : $s);
  });
});

const $core$Dict$toList = (($dict) => {
  const $f = (($key) => {
    return (($value) => {
      return (($list) => {
        return ((sp_cons)($list))(({
          first: $key,
          second: $value,
        }));
      });
    });
  });
  return ((($core$Dict$forReversed)($dict))($f))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$usrToText = (($currentUmr) => {
  return (($meta) => {
    return (($usr) => {
      const $$moduleUmr = $usr;
      const $name = ($$moduleUmr)[2];
      const $moduleUmr = ($$moduleUmr)[1];
      return (((sp_equal)($moduleUmr))($currentUmr)
        ? $name
        : ((() => {
          const $maybeGlobal = (($core$List$find)((($$k) => {
            const $k = $$k.first;
            const $v = $$k.second;
            return ((sp_equal)($usr))($v);
          })))(($core$Dict$toList)($meta.globalTypes));
          return ((($maybeGlobal)[0] === "Just")
            ? ((() => {
              const $k = ($maybeGlobal)[1].first;
              const $v = ($maybeGlobal)[1].second;
              return $k;
            }))()
            : ((($maybeGlobal)[0] === "Nothing")
              ? ((() => {
                const $$try1 = (($core$Dict$get)($moduleUmr))($meta.umrToModuleVisibleAs);
                return ((($$try1)[0] === "Just")
                  ? ((() => {
                    const $moduleAlias = ($$try1)[1];
                    return ($moduleAlias + ("." + $name));
                  }))()
                  : ((($$try1)[0] === "Nothing")
                    ? ((() => {
                      const $$modulePath = $moduleUmr;
                      const $modulePath = ($$modulePath)[2];
                      const $souece = ($$modulePath)[1];
                      return ($modulePath + ("." + $name));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 52:16', (sp_toHuman)($$try1))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 47:8', (sp_toHuman)($maybeGlobal))));
        }))());
    });
  });
});

const $core$Tuple$first = (($t) => {
  return $t.first;
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToPriorityAndText = (($currentUmr) => {
  return (($meta) => {
    return (($type) => {
      const $parensIfGreaterThan = (($threshold) => {
        return (($ty) => {
          const $$pri = ((($$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToPriorityAndText)($currentUmr))($meta))($ty);
          const $str = $$pri.second;
          const $pri = $$pri.first;
          return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$parensIf)(($pri > $threshold)))($str);
        });
      });
      return ((($type)[0] === "TypeConstant")
        ? ((() => {
          const $pos = ($type)[1];
          const $usr = ($type)[2];
          const $args = ($type)[3];
          return ({
            first: (((sp_equal)($core$Core$Nil))($args)
              ? 0
              : 1),
            second: (($core$Text$join)(" "))(((sp_cons)((($core$List$map)(($parensIfGreaterThan)(0)))($args)))(((($$home$nw$stuff$unstable$src$Human$CanonicalAst$usrToText)($currentUmr))($meta))($usr))),
          });
        }))()
        : ((($type)[0] === "TypeVariable")
          ? ((() => {
            const $pos = ($type)[1];
            const $name = ($type)[2];
            return ({
              first: 0,
              second: $name,
            });
          }))()
          : ((($type)[0] === "TypeFunction")
            ? ((() => {
              const $pos = ($type)[1];
              const $from = ($type)[2];
              const $fromIsMut = ($type)[3];
              const $to = ($type)[4];
              const $arrow = ($fromIsMut
                ? " @: "
                : ": ");
              return ({
                first: 2,
                second: (($core$Text$join)(""))((($core$Core$Cons)((($parensIfGreaterThan)(1))($from)))((($core$Core$Cons)($arrow))((($core$Core$Cons)((($parensIfGreaterThan)(2))($to)))($core$Core$Nil)))),
              });
            }))()
            : ((($type)[0] === "TypeRecord")
              ? ((() => {
                const $pos = ($type)[1];
                const $extend = ($type)[2];
                const $attrs = ($type)[3];
                const $attrsString = (($core$Text$join)(", "))((($core$List$map)((($$name) => {
                  const $name = $$name.first;
                  const $ty = $$name.second;
                  return ($name + (" as " + ((($$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToText)($currentUmr))($meta))($ty)));
                })))(((list_sortBy)($core$Tuple$first))(($core$Dict$toList)($attrs))));
                const $l = (($core$Core$Cons)("{"))((($core$Core$Cons)(((($extend)[0] === "Nothing")
                  ? ""
                  : ((($extend)[0] === "Just")
                    ? ((() => {
                      const $n = ($extend)[1];
                      return ($n + " with");
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 109:16', (sp_toHuman)($extend))))))((($core$Core$Cons)($attrsString))((($core$Core$Cons)("}"))($core$Core$Nil))));
                return ({
                  first: 0,
                  second: (($core$Text$join)(" "))($l),
                });
              }))()
              : ((($type)[0] === "TypeAlias")
                ? ((() => {
                  const $pos = ($type)[1];
                  const $usr = ($type)[2];
                  const $ty2 = ($type)[3];
                  return ({
                    first: 0,
                    second: ((($$home$nw$stuff$unstable$src$Human$CanonicalAst$usrToText)($currentUmr))($meta))($usr),
                  });
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 83:4', (sp_toHuman)($type)))))));
    });
  });
});

const $core$Tuple$second = (($t) => {
  return $t.second;
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToText = (($currentUmr) => {
  return (($meta) => {
    return (($t) => {
      return ($core$Tuple$second)(((($$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToPriorityAndText)($currentUmr))($meta))($t));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText = (($env) => {
  return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToText)($env.currentModule))($env.meta);
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyErrorToText = (($ue) => {
  return ((($ue)[0] === "IncompatibleTypes")
    ? "The two types are incompatible."
    : ((($ue)[0] === "IncompatibleMutability")
      ? "The mutability does not match."
      : ((($ue)[0] === "IncompatibleRecords")
        ? ((() => {
          const $args = ($ue)[1];
          return ("The record types are not compatible" + (sp_toHuman)($args));
        }))()
        : ((($ue)[0] === "Cycle")
          ? ((() => {
            const $name = ($ue)[1];
            return ("There is a cyclic dependency on " + $name);
          }))()
          : ((($ue)[0] === "NonFunctionContainsFunction")
            ? ((() => {
              const $rejectFunctions = ($ue)[1];
              return ("NonFunction can't contain functions: " + (sp_toHuman)($rejectFunctions));
            }))()
            : ((($ue)[0] === "OkThisIsActuallyPossible")
              ? "OkThisIsActuallyPossible?"
              : ((($ue)[0] === "NI")
                ? ((() => {
                  const $str = ($ue)[1];
                  return ("Not Implemented: " + $str);
                }))()
                : ((($ue)[0] === "SubstitutingAnnotation")
                  ? ((() => {
                    const $name = ($ue)[1];
                    return ("SubstitutingAnnotation: " + $name);
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1641:4', (sp_toHuman)($ue))))))))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$clashToTexts = (($env) => {
  return (($params) => {
    const $$try1 = $params.unifiedType;
    return ((($$try1)[0] === "TypeVariable")
      ? ((() => {
        const $p = ($$try1)[1];
        const $unifiedTypeName = ($$try1)[2];
        return (($core$Text$join)("\n"))((($core$List$concatMap)((($$clash) => {
          const $clashPlaceholderName = $$clash.first;
          const $clash = $$clash.second;
          return (($core$Core$Cons)($params.type1_is))((($core$Core$Cons)(""))((($core$Core$Cons)(("  " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($clash.t1))))((($core$Core$Cons)(""))((($core$Core$Cons)($params.type2_is))((($core$Core$Cons)(""))((($core$Core$Cons)(("  " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($clash.t2))))((($core$Core$Cons)(""))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyErrorToText)($clash.err)))($core$Core$Nil)))))))));
        })))(($core$Dict$toList)($params.clashes)));
      }))()
      : (true
        ? ((() => {
          const $info = (($core$Core$Cons)($params.typeSeemsToBe))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($params.unifiedType)))((($core$Core$Cons)(""))((($core$Core$Cons)("However I can't reconcile the following:"))($core$Core$Nil))));
          const $clashToError = (($$clash) => {
            const $name = $$clash.first;
            const $clash = $$clash.second;
            return (($core$Core$Cons)(""))((($core$Core$Cons)(("* `" + ($name + "`"))))((($core$Core$Cons)(""))((($core$Core$Cons)(("  " + $params.type1_is)))((($core$Core$Cons)(("  " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($clash.t1))))((($core$Core$Cons)(""))((($core$Core$Cons)(("  " + $params.type2_is)))((($core$Core$Cons)(("  " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($clash.t2))))((($core$Core$Cons)(""))((($core$Core$Cons)(("  " + ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyErrorToText)($clash.err))))($core$Core$Nil))))))))));
          });
          const $clashErrors = (($core$List$concatMap)($clashToError))(($core$Dict$toList)($params.clashes));
          return (($core$Text$join)("\n"))(($core$List$concat)((($core$Core$Cons)($info))((($core$Core$Cons)($clashErrors))($core$Core$Nil))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2487:4', (sp_toHuman)($$try1))));
  });
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos = (($e) => {
  return ((($e)[0] === "LiteralText")
    ? ((() => {
      const $pos = ($e)[1];
      return $pos;
    }))()
    : ((($e)[0] === "LiteralNumber")
      ? ((() => {
        const $pos = ($e)[1];
        return $pos;
      }))()
      : ((($e)[0] === "Variable")
        ? ((() => {
          const $pos = ($e)[1];
          return $pos;
        }))()
        : ((($e)[0] === "Constructor")
          ? ((() => {
            const $pos = ($e)[1];
            return $pos;
          }))()
          : ((($e)[0] === "Lambda")
            ? ((() => {
              const $pos = ($e)[1];
              return $pos;
            }))()
            : ((($e)[0] === "Record")
              ? ((() => {
                const $pos = ($e)[1];
                return $pos;
              }))()
              : ((($e)[0] === "Call")
                ? ((() => {
                  const $pos = ($e)[1];
                  return $pos;
                }))()
                : ((($e)[0] === "If")
                  ? ((() => {
                    const $pos = ($e)[1];
                    return $pos;
                  }))()
                  : ((($e)[0] === "Try")
                    ? ((() => {
                      const $pos = ($e)[1];
                      return $pos;
                    }))()
                    : ((($e)[0] === "LetIn")
                      ? ((() => {
                        const $valueDef = ($e)[1];
                        return ($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($valueDef.pattern);
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 241:4', (sp_toHuman)($e))))))))))));
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$skipLetIns = (($expr) => {
  return ((($expr)[0] === "LetIn")
    ? ((() => {
      const $def = ($expr)[1];
      const $e = ($expr)[2];
      return ($$home$nw$stuff$unstable$src$Types$CanonicalAst$skipLetIns)($e);
    }))()
    : (true
      ? $expr
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 175:4', (sp_toHuman)($expr))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorIncompatibleTypes = (($env) => {
  return (($reason) => {
    return (($pos_whatever) => {
      return (($unifiedType) => {
        return (($clashes) => {
          return ((($reason)[0] === "UnifyReason_CallArgument")
            ? ((() => {
              const $pos = ($reason)[1];
              const $makeError = (($eenv) => {
                const $$block = (($$home$nw$stuff$unstable$src$Compiler$Error$posToHuman)($eenv))($pos.reference);
                const $location = $$block.location;
                const $block = $$block.block;
                return (($core$Core$Cons)("This expression cannot be used as argument to this function:"))((($core$Core$Cons)(""))((($core$Core$Cons)($block))((($core$Core$Cons)(""))((($core$Core$Cons)("the argument type seems to be: "))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$clashToTexts)($env))(({
                  clashes: $clashes,
                  type1_is: "The functon expects:",
                  type2_is: "But the actual argument is:",
                  typeSeemsToBe: "The argument type seems to be",
                  unifiedType: $unifiedType,
                }))))($core$Core$Nil))))));
              });
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addErrorWithEEnv)($pos.argument))($makeError);
            }))()
            : ((($reason)[0] === "UnifyReason_TryBlock")
              ? ((() => {
                const $block = ($reason)[1];
                const $makeError = (($eenv) => {
                  return (($core$Core$Cons)("This try..as block produces a different type than the blocks preceding it."))((($core$Core$Cons)(""))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$clashToTexts)($env))(({
                    clashes: $clashes,
                    type1_is: "The previous block(s) produce:",
                    type2_is: "But this block produces:",
                    typeSeemsToBe: "The block type seems to be",
                    unifiedType: $unifiedType,
                  }))))($core$Core$Nil)));
                });
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addErrorWithEEnv)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos)($block)))($makeError);
              }))()
              : ((($reason)[0] === "UnifyReason_AnnotationVsBlock")
                ? ((() => {
                  const $pattern = ($reason)[1];
                  const $annotation = ($reason)[2];
                  const $body = ($reason)[3];
                  const $headerPos = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($pattern);
                  const $lastStatementPos = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$skipLetIns)($body));
                  const $name = (($core$Text$join)(", "))(($core$Dict$keys)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($pattern)));
                  const $makeError = (($eenv) => {
                    const $$block = (($$home$nw$stuff$unstable$src$Compiler$Error$posToHuman)($eenv))($headerPos);
                    const $location = $$block.location;
                    const $block = $$block.block;
                    return (($core$Core$Cons)(("The definition of " + ($name + " does not match the annotation:"))))((($core$Core$Cons)(""))((($core$Core$Cons)($block))((($core$Core$Cons)(""))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$clashToTexts)($env))(({
                      clashes: $clashes,
                      type1_is: "The annotation says:",
                      type2_is: "But this definition produces:",
                      typeSeemsToBe: "The produced type seems to be",
                      unifiedType: $unifiedType,
                    }))))($core$Core$Nil)))));
                  });
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addErrorWithEEnv)($lastStatementPos))($makeError);
                }))()
                : ((($reason)[0] === "UnifyReason_IsBeingCalledAsAFunction")
                  ? ((() => {
                    const $pos = ($reason)[1];
                    const $referenceType = ($reason)[2];
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)("This expression is being called as if it was a function, but its type is:"))((($core$Core$Cons)(""))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($referenceType)))($core$Core$Nil))));
                  }))()
                  : (true
                    ? ((() => {
                      const $pos = $pos_whatever;
                      const $title = ((($reason)[0] === "UnifyReason_AnnotationSimple")
                        ? "The type is not compatible with the annotation"
                        : ((($reason)[0] === "UnifyReason_DefBlockVsPattern")
                          ? "The definition block cannot be unpacked into the pattern"
                          : ((($reason)[0] === "UnifyReason_IfCondition")
                            ? "The expression inside `if ... :` should always be a Bool"
                            : ((($reason)[0] === "UnifyReason_IfBranches")
                              ? "The branches of an `if` should produce the same type of value"
                              : ((($reason)[0] === "UnifyReason_TryPattern")
                                ? "try..as patterns should have the same type"
                                : ((($reason)[0] === "UnifyReason_ConstructorArgument")
                                  ? ((() => {
                                    const $p = ($reason)[1];
                                    return ("Argument " + ((text_fromNumber)($p.argIndex) + (" to type constructor " + ((sp_toHuman)($p.usr) + " does not match the constructor definition"))));
                                  }))()
                                  : ((($reason)[0] === "UnifyReason_AttributeAccess")
                                    ? ((() => {
                                      const $attrName = ($reason)[1];
                                      return ("You are trying to access the ." + ($attrName + " attribute"));
                                    }))()
                                    : ((($reason)[0] === "UnifyReason_AttributeUpdate")
                                      ? ((() => {
                                        const $attrNames = ($reason)[1];
                                        return ("You are trying to update the " + ((($core$Text$join)(", "))($attrNames) + " attributes"));
                                      }))()
                                      : ((($reason)[0] === "UnifyReason_Override")
                                        ? "this is addSubstitution running a UnifyReason_Override, I don't know what I'm doing"
                                        : ((($reason)[0] === "UnifyReason_IsLambda")
                                          ? "this is a function, and its type should reflect that"
                                          : (true
                                            ? (sp_todo)(((sp_toHuman)($reason) + " should not even get here"))
                                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2428:16', (sp_toHuman)($reason)))))))))))));
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos_whatever))((($core$Core$Cons)($title))((($core$Core$Cons)(""))((($core$Core$Cons)("The type seems to be something like"))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$clashToTexts)($env))(({
                        clashes: $clashes,
                        type1_is: "t1 is:",
                        type2_is: "but t2 is:",
                        typeSeemsToBe: "type seems to be",
                        unifiedType: $unifiedType,
                      }))))($core$Core$Nil)))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2341:4', (sp_toHuman)($reason)))))));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$StateMonad$update = (($f) => {
  return (($state) => {
    const $s = ($f)($state);
    return ({
      first: $s,
      second: $s,
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$m_update = $$home$nw$stuff$unstable$src$StateMonad$update;

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$popClashingtypes = (($state) => {
  const $$try1 = $state.typeClashesByPlaceholderId;
  return ((($$try1)[0] === "Nothing")
    ? (sp_todo)("popping a nothing!")
    : ((($$try1)[0] === "Just")
      ? ((() => {
        const $dict = ($$try1)[1];
        return ({
          first: $dict,
          second: (Object.assign)({}, $state, ({
            typeClashesByPlaceholderId: $core$Maybe$Nothing,
          })),
        });
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 313:4', (sp_toHuman)($$try1))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorTodo = (($pos) => {
  return (($message) => {
    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)($message))($core$Core$Nil));
  });
});

const $core$Dict$values = (($dict) => {
  return ((($core$Dict$forReversed)($dict))((($key) => {
    return (($value) => {
      return (($valueList) => {
        return ((sp_cons)($valueList))($value);
      });
    });
  })))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions = (($ty) => {
  return ((($ty)[0] === "TypeConstant")
    ? ((() => {
      const $args = ($ty)[3];
      return (($core$List$any)($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions))($args);
    }))()
    : ((($ty)[0] === "TypeVariable")
      ? false
      : ((($ty)[0] === "TypeFunction")
        ? ((() => {
          const $from = ($ty)[2];
          const $fromIsMutable = ($ty)[3];
          const $to = ($ty)[4];
          return true;
        }))()
        : ((($ty)[0] === "TypeAlias")
          ? ((() => {
            const $path = ($ty)[2];
            const $t = ($ty)[3];
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($t);
          }))()
          : ((($ty)[0] === "TypeRecord")
            ? ((() => {
              const $extensible = ($ty)[2];
              const $attrs = ($ty)[3];
              return (($core$List$any)($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions))(($core$Dict$values)($attrs));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1997:4', (sp_toHuman)($ty)))))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkNonFunction = (($env) => {
  return (($name) => {
    return (($ty) => {
      const $nope = ({
        freeVarsToFlag: $core$Core$Nil,
      });
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($nonFnTyvars) => {
        const $$try1 = (($core$Dict$get)($name))($nonFnTyvars);
        return ((($$try1)[0] === "Nothing")
          ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($nope)
          : ((($$try1)[0] === "Just")
            ? ((() => {
              const $rejectReasons = ($$try1)[1];
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($ty)
                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($nope);
                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorTodo)(($$home$nw$stuff$unstable$src$Types$Pos$I)(26)))(("type `" + ($name + ("` should not contain functions, but is " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($ty))))))
                : ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($nope));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1961:4', (sp_toHuman)($$try1))));
      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
        return $x.nonFnTyvars;
      })));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$flagFreeVars = (($names) => {
  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$isAnnotation = (($n) => {
  return ((sp_equal)($core$Maybe$Nothing))((text_toNumber)($n));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar = (($n) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeVariable")
      ? ((() => {
        const $pos = ($ty)[1];
        const $name = ($ty)[2];
        return ((sp_equal)($name))($n);
      }))()
      : ((($ty)[0] === "TypeFunction")
        ? ((() => {
          const $from = ($ty)[2];
          const $fromIsMutable = ($ty)[3];
          const $to = ($ty)[4];
          return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar)($n))($from) || (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar)($n))($to));
        }))()
        : ((($ty)[0] === "TypeConstant")
          ? ((() => {
            const $pos = ($ty)[1];
            const $ref = ($ty)[2];
            const $args = ($ty)[3];
            return (($core$List$any)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar)($n)))($args);
          }))()
          : ((($ty)[0] === "TypeAlias")
            ? ((() => {
              const $path = ($ty)[2];
              const $t = ($ty)[3];
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar)($n))($t);
            }))()
            : ((($ty)[0] === "TypeRecord")
              ? ((() => {
                const $pos = ($ty)[1];
                const $extensible = ($ty)[2];
                const $attrs = ($ty)[3];
                return (((sp_equal)($extensible))(($core$Maybe$Just)($n)) || (($core$List$any)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar)($n)))(($core$Dict$values)($attrs)));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2018:4', (sp_toHuman)($ty)))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeIsTyvar = (($name) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeVariable")
      ? ((() => {
        const $n = ($ty)[2];
        return ((sp_equal)($name))($n);
      }))()
      : (true
        ? false
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1946:4', (sp_toHuman)($ty))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertTypeClash = (($id) => {
  return (($t1) => {
    return (($t2) => {
      return (($err) => {
        return (($state) => {
          const $$try1 = $state.typeClashesByPlaceholderId;
          return ((($$try1)[0] === "Nothing")
            ? ((($x) => {
              return (sp_todo)(("Inserting type clash outside of unify" + $x));
            }))((sp_toHuman)(({
              err: $err,
              id: $id,
              t1: $t1,
              t2: $t2,
            })))
            : ((($$try1)[0] === "Just")
              ? ((() => {
                const $dict = ($$try1)[1];
                const $x = ($core$Maybe$Just)(((($core$Dict$insert)($id))(({
                  err: $err,
                  t1: $t1,
                  t2: $t2,
                })))($dict));
                return ({
                  first: null,
                  second: (Object.assign)({}, $state, ({
                    typeClashesByPlaceholderId: $x,
                  })),
                });
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 291:4', (sp_toHuman)($$try1))));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError = (($pos) => {
  return (($error) => {
    return (($t1) => {
      return (($t2) => {
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($name) => {
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($name));
          })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertTypeClash)($name))($t1))($t2))($error));
        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)($core$Basics$identity));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution = (($env) => {
  return (($debugCode) => {
    return (($pos) => {
      return (($reason) => {
        return (($name) => {
          return (($rawTy) => {
            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isAnnotation)($name)
                ? ((($ty)[0] === "TypeVariable")
                  ? ((() => {
                    const $subName = ($ty)[2];
                    return (((sp_equal)($name))($subName)
                      ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($ty)
                      : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isAnnotation)($subName)
                        ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$SubstitutingAnnotation)($name)))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($name)))($ty)
                        : (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))(($debugCode + " SWITCH")))($pos))($reason))($subName))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($name))));
                  }))()
                  : (true
                    ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$SubstitutingAnnotation)($name)))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($name)))($ty)
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1892:8', (sp_toHuman)($ty))))
                : ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeHasTyvar)($name))($ty)
                  ? ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeIsTyvar)($name))($ty)
                    ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($ty)
                    : (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$Cycle)($name)))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($name)))($ty))
                  : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($nonFunction) => {
                    const $$freeVarsToFlag = $nonFunction;
                    const $freeVarsToFlag = $$freeVarsToFlag.freeVarsToFlag;
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
                        const $$try1 = (($core$Dict$get)($name))($subs);
                        return ((($$try1)[0] === "Just")
                          ? ((() => {
                            const $sub = ($$try1)[1];
                            return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))($ty))($sub);
                          }))()
                          : ((($$try1)[0] === "Nothing")
                            ? (($state) => {
                              return ({
                                first: $ty,
                                second: (Object.assign)({}, $state, ({
                                  substitutions: ((($core$Dict$insert)($name))($ty))((($core$Dict$map)((($k) => {
                                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)((($core$Dict$singleton)($name))($ty));
                                  })))($state.substitutions)),
                                })),
                              });
                            })
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1924:8', (sp_toHuman)($$try1))));
                      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                        return $x.substitutions;
                      })));
                    })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$flagFreeVars)($freeVarsToFlag));
                  })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkNonFunction)($env))($name))($ty))));
            })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($rawTy));
          });
        });
      });
    });
  });
});

const $core$Tuple$pair = (($a) => {
  return (($b) => {
    return ({
      first: $a,
      second: $b,
    });
  });
});

const $$home$nw$stuff$unstable$src$StateMonad$list_map2 = (($f) => {
  return (($la) => {
    return (($lb) => {
      const $apply = (($$a) => {
        const $a = $$a.first;
        const $b = $$a.second;
        return (($accum) => {
          return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($c) => {
            return ($$home$nw$stuff$unstable$src$StateMonad$return)(((sp_cons)($accum))($c));
          })))((($f)($a))($b));
        });
      });
      return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($x) => {
        return ($$home$nw$stuff$unstable$src$StateMonad$return)(($core$List$reverse)($x));
      })))(((($$home$nw$stuff$unstable$src$StateMonad$list_foldl)($apply))(((($core$List$map2)($core$Tuple$pair))($la))($lb)))($core$Core$Nil));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_map2 = $$home$nw$stuff$unstable$src$StateMonad$list_map2;

const $$home$nw$stuff$unstable$src$StateMonad$dict_foldl = (($f) => {
  return (($x) => {
    return (($$home$nw$stuff$unstable$src$StateMonad$list_foldl)((($$k) => {
      const $k = $$k.first;
      const $v = $$k.second;
      return (($f)($k))($v);
    })))(($core$Dict$toList)($x));
  });
});

const $$home$nw$stuff$unstable$src$StateMonad$dict_map = (($f) => {
  return (($dict) => {
    const $insert = (($c) => {
      return (($a) => {
        return (($d) => {
          return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($b) => {
            return ($$home$nw$stuff$unstable$src$StateMonad$return)(((($core$Dict$insert)($c))($b))($d));
          })))((($f)($c))($a));
        });
      });
    });
    return ((($$home$nw$stuff$unstable$src$StateMonad$dict_foldl)($insert))($dict))($core$Dict$empty);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_map = $$home$nw$stuff$unstable$src$StateMonad$dict_map;

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyToNonExtensibleRecord = (($env) => {
  return (($pos) => {
    return (($reason) => {
      return (($aName) => {
        return (($aOnly) => {
          return (($bOnly) => {
            return (($bothUnified) => {
              return (((sp_not_equal)($core$Dict$empty))($aOnly)
                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)(("record is missing attrs: " + (($core$Text$join)(", "))(($core$Dict$keys)($aOnly)))))((($core$Core$Cons)((sp_toHuman)($reason)))($core$Core$Nil)))
                : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ext) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($core$Maybe$Nothing))((($core$Dict$join)($bothUnified))($bOnly)));
                  })))((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("ne"))($pos))($reason))($aName))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(5)))($ext))($bOnly)));
                })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)($core$Maybe$Just)));
            });
          });
        });
      });
    });
  });
});

const $core$Dict$merge = (($leftStep) => {
  return (($bothStep) => {
    return (($rightStep) => {
      return (($leftDict) => {
        return (($rightDict) => {
          return (($initialResult) => {
            const $stepState = (($rKey) => {
              return (($rValue) => {
                return (($q) => {
                  const $$list = $q;
                  const $res = $$list.second;
                  const $list = $$list.first;
                  return ((($list)[0] === "Nil")
                    ? ({
                      first: $list,
                      second: ((($rightStep)($rKey))($rValue))($res),
                    })
                    : ((($list)[0] === "Cons")
                      ? ((() => {
                        const $lKey = ($list)[1].first;
                        const $lValue = ($list)[1].second;
                        const $rest = ($list)[2];
                        const $$try1 = ((basics_compare)($lKey))($rKey);
                        return ((1 === $$try1)
                          ? ({
                            first: $list,
                            second: ((($rightStep)($rKey))($rValue))($res),
                          })
                          : ((0 === $$try1)
                            ? ({
                              first: $rest,
                              second: (((($bothStep)($lKey))($lValue))($rValue))($res),
                            })
                            : (true
                              ? ((($stepState)($rKey))($rValue))(({
                                first: $rest,
                                second: ((($leftStep)($lKey))($lValue))($res),
                              }))
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 395:10', (sp_toHuman)($$try1)))));
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 390:4', (sp_toHuman)($list))));
                });
              });
            });
            const $$intermediateResult = ((($core$Dict$for)($rightDict))($stepState))(({
              first: ($core$Dict$toList)($leftDict),
              second: $initialResult,
            }));
            const $intermediateResult = $$intermediateResult.second;
            const $leftovers = $$intermediateResult.first;
            const $liftLeftStep = (($t) => {
              return (($res) => {
                const $$k = $t;
                const $v = $$k.second;
                const $k = $$k.first;
                return ((($leftStep)($k))($v))($res);
              });
            });
            return ((($core$List$for)($leftovers))($liftLeftStep))($intermediateResult);
          });
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyRecords = (($env) => {
  return (($reason) => {
    return (($pos) => {
      return (($$a_attrs) => {
        const $a_ext = $$a_attrs.first;
        const $a_attrs = $$a_attrs.second;
        return (($$b_attrs) => {
          const $b_ext = $$b_attrs.first;
          const $b_attrs = $$b_attrs.second;
          const $init = ({
            aOnly: $core$Dict$empty,
            bOnly: $core$Dict$empty,
            both: $core$Dict$empty,
          });
          const $onA = (($name) => {
            return (($type_) => {
              return (($state) => {
                return (Object.assign)({}, $state, ({
                  aOnly: ((($core$Dict$insert)($name))($type_))($state.aOnly),
                }));
              });
            });
          });
          const $onB = (($name) => {
            return (($type_) => {
              return (($state) => {
                return (Object.assign)({}, $state, ({
                  bOnly: ((($core$Dict$insert)($name))($type_))($state.bOnly),
                }));
              });
            });
          });
          const $onBoth = (($name) => {
            return (($aType) => {
              return (($bType) => {
                return (($state) => {
                  return (Object.assign)({}, $state, ({
                    both: ((($core$Dict$insert)($name))(({
                      first: $aType,
                      second: $bType,
                    })))($state.both),
                  }));
                });
              });
            });
          });
          const $$aOnly = (((((($core$Dict$merge)($onA))($onBoth))($onB))($a_attrs))($b_attrs))($init);
          const $both = $$aOnly.both;
          const $bOnly = $$aOnly.bOnly;
          const $aOnly = $$aOnly.aOnly;
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($bothUnified) => {
            const $$try1 = ({
              first: $a_ext,
              second: $b_ext,
            });
            return (((($$try1.first)[0] === "Just") && (($$try1.second)[0] === "Nothing"))
              ? ((() => {
                const $aName = ($$try1.first)[1];
                return ((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyToNonExtensibleRecord)($env))($pos))($reason))($aName))($aOnly))($bOnly))($bothUnified);
              }))()
              : (((($$try1.first)[0] === "Nothing") && (($$try1.second)[0] === "Just"))
                ? ((() => {
                  const $bName = ($$try1.second)[1];
                  return ((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyToNonExtensibleRecord)($env))($pos))($reason))($bName))($bOnly))($aOnly))($bothUnified);
                }))()
                : (((($$try1.first)[0] === "Nothing") && (($$try1.second)[0] === "Nothing"))
                  ? ((((sp_equal)($core$Dict$empty))($bOnly) && ((sp_equal)($core$Dict$empty))($aOnly))
                    ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(4)))($core$Maybe$Nothing))($bothUnified))
                    : ((() => {
                      const $e = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleRecords)(({
                        aOnly: ($core$Dict$keys)($aOnly),
                        bOnly: ($core$Dict$keys)($bOnly),
                        bothUnified: ($core$Dict$keys)($bothUnified),
                      }));
                      return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos))($e))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($a_ext))($a_attrs)))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($b_ext))($b_attrs));
                    }))())
                  : (((($$try1.first)[0] === "Just") && (($$try1.second)[0] === "Just"))
                    ? ((() => {
                      const $aName = ($$try1.first)[1];
                      const $bName = ($$try1.second)[1];
                      return ((((sp_equal)($bName))($aName) && (((sp_equal)($core$Dict$empty))($aOnly) && ((sp_equal)($core$Dict$empty))($bOnly)))
                        ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))(($core$Maybe$Just)($aName)))($bothUnified))
                        : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($new) => {
                          const $sub = ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))(($core$Maybe$Just)($new)))((($core$Dict$join)($bOnly))($a_attrs));
                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                              return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($sub);
                            })))((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("jj2"))($pos))($reason))($bName))($sub));
                          })))((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("jj1"))($pos))($reason))($aName))($sub));
                        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)($core$Basics$identity)));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1812:4', (sp_toHuman)($$try1))))));
          })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_map)((($k) => {
            return (($$a) => {
              const $a = $$a.first;
              const $b = $$a.second;
              return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))($a))($b);
            });
          })))($both));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_ = (($env) => {
  return (($reason) => {
    return (($pos1) => {
      return (($t1) => {
        return (($t2) => {
          const $$try1 = ({
            first: $t1,
            second: $t2,
          });
          return ((($$try1.first)[0] === "TypeAlias")
            ? ((() => {
              const $pos = ($$try1.first)[1];
              const $aliased = ($$try1.first)[3];
              return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))($aliased))($t2);
            }))()
            : ((($$try1.second)[0] === "TypeAlias")
              ? ((() => {
                const $aliased = ($$try1.second)[3];
                return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos1))($t1))($aliased);
              }))()
              : (((($$try1.first)[0] === "TypeConstant") && (($$try1.second)[0] === "TypeConstant"))
                ? ((() => {
                  const $pos = ($$try1.first)[1];
                  const $ref1 = ($$try1.first)[2];
                  const $args1 = ($$try1.first)[3];
                  const $ref2 = ($$try1.second)[2];
                  const $args2 = ($$try1.second)[3];
                  return (((sp_not_equal)($ref2))($ref1)
                    ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos1))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleTypes))($t1))($t2)
                    : ((() => {
                      const $fold = (($arg1) => {
                        return (($arg2) => {
                          return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))($arg1))($arg2);
                        });
                      });
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($argTypes) => {
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
                          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))($ref1))((($core$List$map)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs)))($argTypes)));
                        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                          return $x.substitutions;
                        })));
                      })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_map2)(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos)))($args1))($args2));
                    }))());
                }))()
                : (((($$try1.first)[0] === "TypeVariable") && (($$try1.second)[0] === "TypeVariable"))
                  ? ((() => {
                    const $pos = ($$try1.first)[1];
                    const $v1_name = ($$try1.first)[2];
                    const $v2_name = ($$try1.second)[2];
                    return (((sp_equal)($v2_name))($v1_name)
                      ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($t1)
                      : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
                        const $$try2 = ({
                          first: (($core$Dict$get)($v1_name))($subs),
                          second: (($core$Dict$get)($v2_name))($subs),
                        });
                        return (((($$try2.first)[0] === "Just") && (($$try2.second)[0] === "Just"))
                          ? ((() => {
                            const $sub1 = ($$try2.first)[1];
                            const $sub2 = ($$try2.second)[1];
                            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($v) => {
                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subbedTy) => {
                                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($subbedTy);
                                })))((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("vv2"))($pos))($reason))($v2_name))($v));
                              })))((((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("vv1"))($pos))($reason))($v1_name))($v));
                            })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos1))($sub1))($sub2));
                          }))()
                          : (((($$try2.first)[0] === "Nothing") && (($$try2.second)[0] === "Just"))
                            ? ((() => {
                              const $sub2 = ($$try2.second)[1];
                              return (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("vv3"))($pos))($reason))($v1_name))($t2);
                            }))()
                            : (true
                              ? (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("vv4"))($pos))($reason))($v2_name))($t1)
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1734:16', (sp_toHuman)($$try2)))));
                      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                        return $x.substitutions;
                      }))));
                  }))()
                  : ((($$try1.first)[0] === "TypeVariable")
                    ? ((() => {
                      const $pos = ($$try1.first)[1];
                      const $name1 = ($$try1.first)[2];
                      return (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("vl"))($pos))($reason))($name1))($t2);
                    }))()
                    : ((($$try1.second)[0] === "TypeVariable")
                      ? ((() => {
                        const $pos = ($$try1.second)[1];
                        const $name2 = ($$try1.second)[2];
                        return (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addSubstitution)($env))("vr"))($pos))($reason))($name2))($t1);
                      }))()
                      : (((($$try1.first)[0] === "TypeFunction") && (($$try1.second)[0] === "TypeFunction"))
                        ? ((() => {
                          const $pos = ($$try1.first)[1];
                          const $a_from = ($$try1.first)[2];
                          const $a_fromIsMutable = ($$try1.first)[3];
                          const $a_to = ($$try1.first)[4];
                          const $b_from = ($$try1.second)[2];
                          const $b_fromIsMutable = ($$try1.second)[3];
                          const $b_to = ($$try1.second)[4];
                          return (((sp_not_equal)($b_fromIsMutable))($a_fromIsMutable)
                            ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleMutability))($t1))($t2)
                            : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unified_from) => {
                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs_) => {
                                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unified_to) => {
                                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
                                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs))($unified_from)))($a_fromIsMutable))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs))($unified_to)));
                                  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                                    return $x.substitutions;
                                  })));
                                })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs_))($a_to)))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($subs_))($b_to)));
                              })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                                return $x.substitutions;
                              })));
                            })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))($a_from))($b_from)));
                        }))()
                        : (((($$try1.first)[0] === "TypeRecord") && (($$try1.second)[0] === "TypeRecord"))
                          ? ((() => {
                            const $a_ext = ($$try1.first)[2];
                            const $a_attrs = ($$try1.first)[3];
                            const $b_ext = ($$try1.second)[2];
                            const $b_attrs = ($$try1.second)[3];
                            return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyRecords)($env))($reason))($pos1))(({
                              first: $a_ext,
                              second: $a_attrs,
                            })))(({
                              first: $b_ext,
                              second: $b_attrs,
                            }));
                          }))()
                          : (true
                            ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyError)($pos1))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$IncompatibleTypes))($t1))($t2)
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1703:4', (sp_toHuman)($$try1)))))))))));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify = (($env) => {
  return (($pos) => {
    return (($reason) => {
      return (($a) => {
        return (($b) => {
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($tc) => {
            return (((sp_not_equal)($core$Maybe$Nothing))($tc)
              ? (sp_todo)("typeClashesByPlaceholderId NOT EMPTY!")
              : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedType) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($typeClashes) => {
                    return (((sp_equal)($core$Dict$empty))($typeClashes)
                      ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($unifiedType)
                      : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($unifiedType);
                      })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorIncompatibleTypes)($env))($reason))($pos))($unifiedType))($typeClashes)));
                  })))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$popClashingtypes);
                })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify_)($env))($reason))($pos))($a))($b));
              })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$m_update)((($s) => {
                return (Object.assign)({}, $s, ({
                  typeClashesByPlaceholderId: ($core$Maybe$Just)($core$Dict$empty),
                }));
              }))));
          })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
            return $x.typeClashesByPlaceholderId;
          })));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$applyAttributePath = (($env) => {
  return (($pos) => {
    return (($attrPath) => {
      const $wrap = (($attributeName) => {
        return (($ty) => {
          const $maybeAttrType = ((($ty)[0] === "TypeRecord")
            ? ((() => {
              const $e = ($ty)[2];
              const $attrs = ($ty)[3];
              return (($core$Dict$get)($attributeName))($attrs);
            }))()
            : (true
              ? $core$Maybe$Nothing
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2042:12', (sp_toHuman)($ty))));
          return ((($maybeAttrType)[0] === "Just")
            ? ((() => {
              const $attrType = ($maybeAttrType)[1];
              return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($attrType);
            }))()
            : ((($maybeAttrType)[0] === "Nothing")
              ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($extName) => {
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($attrType) => {
                  const $re = ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(2)))(($core$Maybe$Just)($extName)))((($core$Dict$singleton)($attributeName))($attrType));
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($attrType);
                  })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AttributeAccess)($attributeName)))($ty))($re));
                })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos));
              })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)($core$Basics$identity))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2048:8', (sp_toHuman)($maybeAttrType))));
        });
      });
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for)($attrPath))($wrap);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckConstructorError = (($pos) => {
  return (($env) => {
    return (($remainingArgs) => {
      return (($message) => {
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
          return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for)($remainingArgs))((($argPattern) => {
            return (($envX) => {
              return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($envX);
            });
          })))($env);
        })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))($message));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkConstructorWithItsArgs = (($env) => {
  return (($pos) => {
    return (($index) => {
      return (($actualArgs) => {
        return (($expectedArgs) => {
          const $$try1 = ({
            first: $actualArgs,
            second: $expectedArgs,
          });
          return (((($$try1.first)[0] === "Cons") && (($$try1.second)[0] === "Cons"))
            ? ((() => {
              const $actualHead = ($$try1.first)[1];
              const $actualTail = ($$try1.first)[2];
              const $expectedHead = ($$try1.second)[1];
              const $expectedTail = ($$try1.second)[2];
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($updatedEnv) => {
                return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkConstructorWithItsArgs)($updatedEnv))($pos))(($index + 1)))($actualTail))($expectedTail);
              })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkAndInsertPattern)($env))($expectedHead))($actualHead));
            }))()
            : (((($$try1.first)[0] === "Nil") && (($$try1.second)[0] === "Cons"))
              ? ((() => {
                const $expectedHead = ($$try1.second)[1];
                const $expectedTail = ($$try1.second)[2];
                const $given = $index;
                const $needed = ($index + ($core$List$length)($expectedArgs));
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env);
                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("Constructor needs " + ((text_fromNumber)($needed) + (" arguments but was given only " + (text_fromNumber)($given))))))($core$Core$Nil)));
              }))()
              : (((($$try1.first)[0] === "Cons") && (($$try1.second)[0] === "Nil"))
                ? ((() => {
                  const $actualHead = ($$try1.first)[1];
                  const $actualTail = ($$try1.first)[2];
                  return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckConstructorError)($pos))($env))($actualArgs))((($core$Core$Cons)("more arguments than needed"))($core$Core$Nil));
                }))()
                : (((($$try1.first)[0] === "Nil") && (($$try1.second)[0] === "Nil"))
                  ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 793:4', (sp_toHuman)($$try1))))));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for = (($d) => {
  return (($f) => {
    return (($$home$nw$stuff$unstable$src$StateMonad$dict_foldl)($f))($d);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$expandAlias = (($type) => {
  return ((($type)[0] === "TypeAlias")
    ? ((() => {
      const $t = ($type)[3];
      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$expandAlias)($t);
    }))()
    : (true
      ? $type
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 261:4', (sp_toHuman)($type))));
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
      return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($rlK))($rlV))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))($lLeft))($lRight)))($rlL)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($rK))($rV))($rlR))($rRight));
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
          ? ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))($lLeft))($lRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($rK))($rV))($rLeft))($rRight))
          : ((($clr)[0] === "Red")
            ? ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))($lLeft))($lRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($rK))($rV))($rLeft))($rRight))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 285:6', (sp_toHuman)($clr))));
      }))()
      : (true
        ? $dict
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 275:2', (sp_toHuman)($dict)))));
});

const $core$Dict$getMin = (($dict) => {
  return ((($dict)[0] === "RBNode_elm_builtin")
    ? ((() => {
      const $left = ($dict)[4];
      return ((($left)[0] === "RBNode_elm_builtin")
        ? ($core$Dict$getMin)($left)
        : (true
          ? $dict
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 234:6', (sp_toHuman)($left))));
    }))()
    : (true
      ? $dict
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 232:2', (sp_toHuman)($dict))));
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
              ? ((((($core$Dict$RBNode_elm_builtin)($color))($key))($value))(($core$Dict$removeMin)($left)))($right)
              : (true
                ? ((() => {
                  const $$try1 = ($core$Dict$moveRedLeft)($dict);
                  return ((($$try1)[0] === "RBNode_elm_builtin")
                    ? ((() => {
                      const $nColor = ($$try1)[1];
                      const $nKey = ($$try1)[2];
                      const $nValue = ($$try1)[3];
                      const $nLeft = ($$try1)[4];
                      const $nRight = ($$try1)[5];
                      return ((((($core$Dict$balance)($nColor))($nKey))($nValue))(($core$Dict$removeMin)($nLeft)))($nRight);
                    }))()
                    : ((($$try1)[0] === "RBEmpty_elm_builtin")
                      ? $core$Dict$RBEmpty_elm_builtin
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 257:18', (sp_toHuman)($$try1))));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 252:14', (sp_toHuman)($lLeft))))
            : (true
              ? ((((($core$Dict$RBNode_elm_builtin)($color))($key))($value))(($core$Dict$removeMin)($left)))($right)
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 250:10', (sp_toHuman)($lColor))));
        }))()
        : (true
          ? $core$Dict$RBEmpty_elm_builtin
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 248:6', (sp_toHuman)($left))));
    }))()
    : (true
      ? $core$Dict$RBEmpty_elm_builtin
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 246:2', (sp_toHuman)($dict))));
});

const $core$Dict$removeHelpEQGT = (($targetKey) => {
  return (($dict) => {
    return ((($dict)[0] === "RBNode_elm_builtin")
      ? ((() => {
        const $color = ($dict)[1];
        const $key = ($dict)[2];
        const $value = ($dict)[3];
        const $left = ($dict)[4];
        const $right = ($dict)[5];
        return (((sp_equal)($key))($targetKey)
          ? ((() => {
            const $$try1 = ($core$Dict$getMin)($right);
            return ((($$try1)[0] === "RBNode_elm_builtin")
              ? ((() => {
                const $minKey = ($$try1)[2];
                const $minValue = ($$try1)[3];
                return ((((($core$Dict$balance)($color))($minKey))($minValue))($left))(($core$Dict$removeMin)($right));
              }))()
              : ((($$try1)[0] === "RBEmpty_elm_builtin")
                ? $core$Dict$RBEmpty_elm_builtin
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 217:8', (sp_toHuman)($$try1))));
          }))()
          : ((((($core$Dict$balance)($color))($key))($value))($left))((($core$Dict$removeHelp)($targetKey))($right)));
      }))()
      : ((($dict)[0] === "RBEmpty_elm_builtin")
        ? $core$Dict$RBEmpty_elm_builtin
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 214:2', (sp_toHuman)($dict))));
  });
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
      return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($llK))($llV))($llLeft))($llRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))($lRight))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($rK))($rV))($rLeft))($rRight)));
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
          ? ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))($lLeft))($lRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($rK))($rV))($rLeft))($rRight))
          : ((($clr)[0] === "Red")
            ? ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($lK))($lV))($lLeft))($lRight)))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($rK))($rV))($rLeft))($rRight))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 318:6', (sp_toHuman)($clr))));
      }))()
      : (true
        ? $dict
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 308:2', (sp_toHuman)($dict)))));
});

const $core$Dict$removeHelpPrepEQGT = (($targetKey) => {
  return (($dict) => {
    return (($color) => {
      return (($key) => {
        return (($value) => {
          return (($left) => {
            return (($right) => {
              return (((($left)[0] === "RBNode_elm_builtin") && ((($left)[1])[0] === "Red"))
                ? ((() => {
                  const $lK = ($left)[2];
                  const $lV = ($left)[3];
                  const $lLeft = ($left)[4];
                  const $lRight = ($left)[5];
                  return ((((($core$Dict$RBNode_elm_builtin)($color))($lK))($lV))($lLeft))(((((($core$Dict$RBNode_elm_builtin)($core$Dict$Red))($key))($value))($lRight))($right));
                }))()
                : (true
                  ? (((($right)[0] === "RBNode_elm_builtin") && (((($right)[1])[0] === "Black") && (((($right)[4])[0] === "RBNode_elm_builtin") && (((($right)[4])[1])[0] === "Black"))))
                    ? ($core$Dict$moveRedRight)($dict)
                    : (((($right)[0] === "RBNode_elm_builtin") && (((($right)[1])[0] === "Black") && ((($right)[4])[0] === "RBEmpty_elm_builtin")))
                      ? ($core$Dict$moveRedRight)($dict)
                      : (true
                        ? $dict
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 199:6', (sp_toHuman)($right)))))
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 189:2', (sp_toHuman)($left))));
            });
          });
        });
      });
    });
  });
});

const $core$Dict$removeHelp = (($targetKey) => {
  return (($dict) => {
    return ((($dict)[0] === "RBEmpty_elm_builtin")
      ? $core$Dict$RBEmpty_elm_builtin
      : ((($dict)[0] === "RBNode_elm_builtin")
        ? ((() => {
          const $color = ($dict)[1];
          const $key = ($dict)[2];
          const $value = ($dict)[3];
          const $left = ($dict)[4];
          const $right = ($dict)[5];
          return (((sp_equal)((0 - 1)))(((basics_compare)($targetKey))($key))
            ? (((($left)[0] === "RBNode_elm_builtin") && ((($left)[1])[0] === "Black"))
              ? ((() => {
                const $lLeft = ($left)[4];
                return (((($lLeft)[0] === "RBNode_elm_builtin") && ((($lLeft)[1])[0] === "Red"))
                  ? ((((($core$Dict$RBNode_elm_builtin)($color))($key))($value))((($core$Dict$removeHelp)($targetKey))($left)))($right)
                  : (true
                    ? ((() => {
                      const $$try1 = ($core$Dict$moveRedLeft)($dict);
                      return ((($$try1)[0] === "RBNode_elm_builtin")
                        ? ((() => {
                          const $nColor = ($$try1)[1];
                          const $nKey = ($$try1)[2];
                          const $nValue = ($$try1)[3];
                          const $nLeft = ($$try1)[4];
                          const $nRight = ($$try1)[5];
                          return ((((($core$Dict$balance)($nColor))($nKey))($nValue))((($core$Dict$removeHelp)($targetKey))($nLeft)))($nRight);
                        }))()
                        : ((($$try1)[0] === "RBEmpty_elm_builtin")
                          ? $core$Dict$RBEmpty_elm_builtin
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 173:16', (sp_toHuman)($$try1))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 168:12', (sp_toHuman)($lLeft))));
              }))()
              : (true
                ? ((((($core$Dict$RBNode_elm_builtin)($color))($key))($value))((($core$Dict$removeHelp)($targetKey))($left)))($right)
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 166:8', (sp_toHuman)($left))))
            : (($core$Dict$removeHelpEQGT)($targetKey))(((((((($core$Dict$removeHelpPrepEQGT)($targetKey))($dict))($color))($key))($value))($left))($right)));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 160:2', (sp_toHuman)($dict))));
  });
});

const $core$Dict$remove = (($key) => {
  return (($dict) => {
    const $$try1 = (($core$Dict$removeHelp)($key))($dict);
    return (((($$try1)[0] === "RBNode_elm_builtin") && ((($$try1)[1])[0] === "Red"))
      ? ((() => {
        const $k = ($$try1)[2];
        const $v = ($$try1)[3];
        const $l = ($$try1)[4];
        const $r = ($$try1)[5];
        return ((((($core$Dict$RBNode_elm_builtin)($core$Dict$Black))($k))($v))($l))($r);
      }))()
      : (true
        ? ((() => {
          const $x = $$try1;
          return $x;
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 148:2', (sp_toHuman)($$try1))));
  });
});

const $core$Dict$diff = (($t1) => {
  return (($t2) => {
    return ((($core$Dict$for)($t2))((($k) => {
      return (($v) => {
        return (($t) => {
          return (($core$Dict$remove)($k))($t);
        });
      });
    })))($t1);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars = (($nonFreeTyvars) => {
  return (($nonFn) => {
    return (($ty) => {
      const $posToTyvar = (($name) => {
        return (($pos) => {
          return ({
            nonFn: (($core$Dict$member)($name))($nonFn),
          });
        });
      });
      return (($core$Dict$map)($posToTyvar))((($core$Dict$diff)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($ty)))($nonFreeTyvars));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$onlyBothOnly = (($da) => {
  return (($db) => {
    const $onAOnly = (($key) => {
      return (($a) => {
        return (($$aOnly) => {
          const $aOnly = $$aOnly.first;
          const $both = $$aOnly.second;
          const $bOnly = $$aOnly.third;
          return ({
            first: ((($core$Dict$insert)($key))($a))($aOnly),
            second: $both,
            third: $bOnly,
          });
        });
      });
    });
    const $onBOnly = (($key) => {
      return (($b) => {
        return (($$aOnly) => {
          const $aOnly = $$aOnly.first;
          const $both = $$aOnly.second;
          const $bOnly = $$aOnly.third;
          return ({
            first: $aOnly,
            second: $both,
            third: ((($core$Dict$insert)($key))($b))($bOnly),
          });
        });
      });
    });
    const $onBoth = (($key) => {
      return (($a) => {
        return (($b) => {
          return (($$aOnly) => {
            const $aOnly = $$aOnly.first;
            const $both = $$aOnly.second;
            const $bOnly = $$aOnly.third;
            return ({
              first: $aOnly,
              second: ((($core$Dict$insert)($key))(({
                first: $a,
                second: $b,
              })))($both),
              third: $bOnly,
            });
          });
        });
      });
    });
    return (((((($core$Dict$merge)($onAOnly))($onBoth))($onBOnly))($da))($db))(({
      first: $core$Dict$empty,
      second: $core$Dict$empty,
      third: $core$Dict$empty,
    }));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith = (($env) => {
  return (($expectedType) => {
    return (($pos) => {
      return (($actualType) => {
        const $$try1 = ({
          first: $expectedType,
          second: $actualType,
        });
        return (((($$try1.first)[0] === "TypeConstant") && (($$try1.second)[0] === "TypeConstant"))
          ? ((() => {
            const $expectedUsr = ($$try1.first)[2];
            const $expectedArgs = ($$try1.first)[3];
            const $actualUsr = ($$try1.second)[2];
            const $actualArgs = ($$try1.second)[3];
            return (((sp_not_equal)($actualUsr))($expectedUsr)
              ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("This expression should be of type"))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType)))((($core$Core$Cons)("but instead is"))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($actualType)))($core$Core$Nil)))))
              : ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for)(((($core$List$map2)($core$Tuple$pair))($expectedArgs))($actualArgs)))((($$a) => {
                const $e = $$a.first;
                const $a = $$a.second;
                return (() => {
                  return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($e))($pos))($a);
                });
              })))(null));
          }))()
          : (((($$try1.first)[0] === "TypeFunction") && (($$try1.second)[0] === "TypeFunction"))
            ? ((() => {
              const $eFrom = ($$try1.first)[2];
              const $eIsMut = ($$try1.first)[3];
              const $eTo = ($$try1.first)[4];
              const $aFrom = ($$try1.second)[2];
              const $aIsMut = ($$try1.second)[3];
              const $aTo = ($$try1.second)[4];
              return (((sp_not_equal)($aIsMut))($eIsMut)
                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("mutability clash"))($core$Core$Nil))
                : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                  return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($eTo))($pos))($aTo);
                })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($eFrom))($pos))($aFrom)));
            }))()
            : (((($$try1.first)[0] === "TypeRecord") && ((($$try1.first)[2])[0] === "Just"))
              ? ((() => {
                const $e = (($$try1.first)[2])[1];
                const $eAttrs = ($$try1.first)[3];
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("Extensible record annotation is experimentally disabled [TODO link to why]"))((($core$Core$Cons)(""))((($core$Core$Cons)(("extension: " + (sp_toHuman)($e))))((($core$Core$Cons)(""))((($core$Core$Cons)(("attrs: " + (($core$Text$join)(", "))(($core$Dict$keys)($eAttrs)))))($core$Core$Nil))))));
              }))()
              : (((($$try1.first)[0] === "TypeRecord") && (((($$try1.first)[2])[0] === "Nothing") && (($$try1.second)[0] === "TypeRecord")))
                ? ((() => {
                  const $eAttrs = ($$try1.first)[3];
                  const $aExtension = ($$try1.second)[2];
                  const $aAttrs = ($$try1.second)[3];
                  const $$aOnly = (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$onlyBothOnly)($eAttrs))($aAttrs);
                  const $aOnly = $$aOnly.third;
                  const $both = $$aOnly.second;
                  const $eOnly = $$aOnly.first;
                  return ((((sp_not_equal)($core$Dict$empty))($eOnly) && ((sp_equal)($core$Maybe$Nothing))($aExtension))
                    ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("missing attributes: " + (sp_toHuman)(($core$Dict$keys)($eOnly)))))($core$Core$Nil))
                    : (((sp_not_equal)($core$Dict$empty))($aOnly)
                      ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("extra attributes: " + (sp_toHuman)(($core$Dict$keys)($aOnly)))))($core$Core$Nil))
                      : ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($both))((($attrName) => {
                        return (($$aType) => {
                          const $eType = $$aType.first;
                          const $aType = $$aType.second;
                          return (() => {
                            return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($eType))($pos))($aType);
                          });
                        });
                      })))(null)));
                }))()
                : ((($$try1.first)[0] === "TypeAlias")
                  ? ((() => {
                    const $ty = ($$try1.first)[3];
                    return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($ty))($pos))($actualType);
                  }))()
                  : ((($$try1.second)[0] === "TypeAlias")
                    ? ((() => {
                      const $ty = ($$try1.second)[3];
                      return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($ty);
                    }))()
                    : ((($$try1.second)[0] === "TypeVariable")
                      ? ((() => {
                        const $actualName = ($$try1.second)[2];
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedArgumentType) => {
                          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                        })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AnnotationSimple))($expectedType))($actualType));
                      }))()
                      : (true
                        ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("I was expecting"))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType)))((($core$Core$Cons)("but the actual type is: "))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($actualType)))((($core$Core$Cons)("The two types are not compatible!"))($core$Core$Nil))))))
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 584:4', (sp_toHuman)($$try1))))))))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkAndInsertPattern = (($env) => {
  return (($expectedType_) => {
    return (($pattern) => {
      const $expectedType = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$expandAlias)($expectedType_);
      return ((($pattern)[0] === "PatternAny")
        ? ((() => {
          const $pos = ($pattern)[1];
          const $maybeName = ($pattern)[2];
          const $maybeAnnotation = ($pattern)[3];
          const $envWith = (($name) => {
            return (($type) => {
              return (Object.assign)({}, $env, ({
                instanceVariables: ((($core$Dict$insert)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefBlock)($name)))(({
                  definedAt: $pos,
                  freeTypeVariables: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars)($env.nonFreeTyvars))($core$Dict$empty))($type),
                  isMutable: false,
                  ty: $type,
                })))($env.instanceVariables),
              }));
            });
          });
          const $$try3 = ({
            first: $maybeName,
            second: $maybeAnnotation,
          });
          return (((($$try3.first)[0] === "Just") && (($$try3.second)[0] === "Just"))
            ? ((() => {
              const $name = ($$try3.first)[1];
              const $annotation = ($$try3.second)[1];
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((($envWith)($name))($annotation));
              })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($annotation));
            }))()
            : (((($$try3.first)[0] === "Just") && (($$try3.second)[0] === "Nothing"))
              ? ((() => {
                const $name = ($$try3.first)[1];
                return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((($envWith)($name))($expectedType));
              }))()
              : (((($$try3.first)[0] === "Nothing") && (($$try3.second)[0] === "Just"))
                ? ((() => {
                  const $annotation = ($$try3.second)[1];
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env);
                  })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($annotation));
                }))()
                : (((($$try3.first)[0] === "Nothing") && (($$try3.second)[0] === "Nothing"))
                  ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env)
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 719:12', (sp_toHuman)($$try3))))));
        }))()
        : ((($pattern)[0] === "PatternLiteralNumber")
          ? ((() => {
            const $pos = ($pattern)[1];
            const $literal = ($pattern)[2];
            (sp_todo)("TODO needs proper type comparison without `pos`");
            return (((sp_equal)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number))($expectedType)
              ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env)
              : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env);
              })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This pattern is a Number, but the annotation says it should be " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType))))($core$Core$Nil))));
          }))()
          : ((($pattern)[0] === "PatternLiteralText")
            ? ((() => {
              const $pos = ($pattern)[1];
              const $literal = ($pattern)[2];
              (sp_todo)("TODO needs proper type comparison without `pos`");
              return (((sp_equal)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text))($expectedType)
                ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env)
                : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env);
                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This pattern is a Text, but the annotation says it should be " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType))))($core$Core$Nil))));
            }))()
            : ((($pattern)[0] === "PatternConstructor")
              ? ((() => {
                const $pos = ($pattern)[1];
                const $usr = ($pattern)[2];
                const $args = ($pattern)[3];
                const $$try2 = (($core$Dict$get)($usr))($env.constructors);
                return ((($$try2)[0] === "Nothing")
                  ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckConstructorError)($pos))($env))($args))((($core$Core$Cons)(("Unknown constructor: " + (sp_toHuman)($usr))))($core$Core$Nil))
                  : ((($$try2)[0] === "Just")
                    ? ((() => {
                      const $constructor = ($$try2)[1];
                      return ((($expectedType)[0] === "TypeConstant")
                        ? ((() => {
                          const $expectedUsr = ($expectedType)[2];
                          const $args_ = ($expectedType)[3];
                          return (((sp_not_equal)($constructor.typeUsr))($usr)
                            ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckConstructorError)($pos))($env))($args))((($core$Core$Cons)(("Constructor produces " + ((sp_toHuman)($constructor.typeUsr) + (" but annotation requires " + (sp_toHuman)($expectedUsr))))))($core$Core$Nil))
                            : ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkConstructorWithItsArgs)($env))($pos))(0))($args))($constructor.args));
                        }))()
                        : (true
                          ? (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckConstructorError)($pos))($env))($args))((($core$Core$Cons)(("This pattern is an union type, but the annotation expects a " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType))))($core$Core$Nil))
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 759:20', (sp_toHuman)($expectedType))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 754:12', (sp_toHuman)($$try2))));
              }))()
              : ((($pattern)[0] === "PatternRecord")
                ? ((() => {
                  const $pos = ($pattern)[1];
                  const $patternAttrs = ($pattern)[2];
                  return ((($expectedType)[0] === "TypeRecord")
                    ? ((() => {
                      const $expectedTypeAttrs = ($expectedType)[3];
                      return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($patternAttrs))((($attrName) => {
                        return (($attrPattern) => {
                          return (($envX) => {
                            const $$try1 = (($core$Dict$get)($attrName))($expectedTypeAttrs);
                            return ((($$try1)[0] === "Nothing")
                              ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($envX);
                              })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This record pattern has an attribute `" + ($attrName + "` but it is not avaiable in the annotation"))))($core$Core$Nil)))
                              : ((($$try1)[0] === "Just")
                                ? ((() => {
                                  const $expectedAttrType = ($$try1)[1];
                                  return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkAndInsertPattern)($envX))($expectedAttrType))($attrPattern);
                                }))()
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 779:24', (sp_toHuman)($$try1))));
                          });
                        });
                      })))($env);
                    }))()
                    : (true
                      ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env);
                      })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This pattern is a record, but the annotation says it should be " + (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType))))($core$Core$Nil)))
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 776:12', (sp_toHuman)($expectedType))));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 702:4', (sp_toHuman)($pattern)))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable = (($env) => {
  return (($pos) => {
    return (($ref) => {
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addErrorWithEEnv)($pos))((($errorEnv) => {
        const $onLocal = (($name) => {
          const $$try2 = (($core$Dict$get)($name))($env.nonAnnotatedRecursives);
          return ((($$try2)[0] === "Just")
            ? ((() => {
              const $defPos = ($$try2)[1];
              return (($core$Core$Cons)(("To use function `" + ($name + "` recursively, you need to add a type annotation to its definition."))))((($core$Core$Cons)(""))((($core$Core$Cons)("This is a limit of the compiler, not sure when I'll have the time to fix it."))($core$Core$Nil)));
            }))()
            : ((($$try2)[0] === "Nothing")
              ? (($core$Core$Cons)(("Undefined value: " + $name)))((($core$Core$Cons)(""))((($core$Core$Cons)(("I can't see a definition for `" + ($name + "` anywhere, so I don't know what it is."))))($core$Core$Nil)))
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2300:8', (sp_toHuman)($$try2))));
        });
        return ((($ref)[0] === "RefBlock")
          ? ((() => {
            const $name = ($ref)[1];
            return ($onLocal)($name);
          }))()
          : (((($ref)[0] === "RefRoot") && ((($ref)[1])[0] === "USR"))
            ? ((() => {
              const $umr = (($ref)[1])[1];
              const $name = (($ref)[1])[2];
              return (((sp_equal)($env.currentModule))($umr)
                ? ($onLocal)($name)
                : ((() => {
                  const $$path = $umr;
                  const $path = ($$path)[2];
                  const $source = ($$path)[1];
                  const $$try1 = (($core$Dict$get)($path))($errorEnv.moduleByName);
                  return ((($$try1)[0] === "Just")
                    ? ((() => {
                      const $mod = ($$try1)[1];
                      return (($core$Core$Cons)(("Module `" + ($path + ("` from source `" + ((sp_toHuman)($source) + ("` does not seem to expose a variable called `" + ($name + "`."))))))))($core$Core$Nil);
                    }))()
                    : ((($$try1)[0] === "Nothing")
                      ? (($core$Core$Cons)(("The code references a `" + ($path + ("." + ($name + ("` with source `" + (sp_toHuman)($source))))))))((($core$Core$Cons)("However, I can't find any module with that path and source."))($core$Core$Nil))
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2327:16', (sp_toHuman)($$try1))));
                }))());
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2313:4', (sp_toHuman)($ref))));
      }));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromArgument = (($env) => {
  return (($argument) => {
    return ((($argument)[0] === "ArgumentExpression")
      ? ((() => {
        const $expr = ($argument)[1];
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
            first: false,
            second: $ty,
          }));
        })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($expr));
      }))()
      : ((($argument)[0] === "ArgumentMutable")
        ? ((() => {
          const $pos = ($argument)[1];
          const $attrPath = ($argument)[2].attrPath;
          const $ref = ($argument)[2].ref;
          const $$try1 = (($core$Dict$get)($ref))($env.instanceVariables);
          return ((($$try1)[0] === "Nothing")
            ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
              return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                first: true,
                second: $ty,
              }));
            })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))($ref))
            : ((($$try1)[0] === "Just")
              ? ((() => {
                const $var = ($$try1)[1];
                return (($core$Basics$not)($var.isMutable)
                  ? ((() => {
                    const $ae = (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)(("You are trying to mutate variable `" + ((sp_toHuman)($ref) + "` but it was declared as not mutable!"))))((($core$Core$Cons)(""))((($core$Core$Cons)("TODO [link to wiki page that explains how to declare variables]"))($core$Core$Nil))));
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                        first: true,
                        second: $ty,
                      }));
                    })))($ae);
                  }))()
                  : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($var.ty)
                    ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                        first: true,
                        second: $ty,
                      }));
                    })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)("mutable arguments can't allow functions"))($core$Core$Nil)))
                    : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                        first: true,
                        second: $ty,
                      }));
                    })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applyAttributePath)($env))($pos))($attrPath))($var.ty))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1342:12', (sp_toHuman)($$try1))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1336:4', (sp_toHuman)($argument))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error = (($pos) => {
  return (($description) => {
    return (($$home$nw$stuff$unstable$src$Compiler$Error$res)($pos))((() => {
      return $description;
    }));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables = (($typeByArgName) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeVariable")
      ? ((() => {
        const $pos = ($ty)[1];
        const $name = ($ty)[2];
        const $$try1 = (($core$Dict$get)($name))($typeByArgName);
        return ((($$try1)[0] === "Nothing")
          ? $ty
          : ((($$try1)[0] === "Just")
            ? ((() => {
              const $t = ($$try1)[1];
              return $t;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 185:12', (sp_toHuman)($$try1))));
      }))()
      : ((($ty)[0] === "TypeFunction")
        ? ((() => {
          const $pos = ($ty)[1];
          const $from = ($ty)[2];
          const $fromIsMutable = ($ty)[3];
          const $to = ($ty)[4];
          return (((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables)($typeByArgName))($from)))($fromIsMutable))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables)($typeByArgName))($to));
        }))()
        : ((($ty)[0] === "TypeRecord")
          ? ((() => {
            const $pos = ($ty)[1];
            const $extensible = ($ty)[2];
            const $attrs = ($ty)[3];
            return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($extensible))((($core$Dict$map)((($k) => {
              return ($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables)($typeByArgName);
            })))($attrs));
          }))()
          : ((($ty)[0] === "TypeConstant")
            ? ((() => {
              const $pos = ($ty)[1];
              const $usr = ($ty)[2];
              const $args = ($ty)[3];
              return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))($usr))((($core$List$map)(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables)($typeByArgName)))($args));
            }))()
            : ((($ty)[0] === "TypeAlias")
              ? ((() => {
                const $pos = ($ty)[1];
                const $usr = ($ty)[2];
                const $t = ($ty)[3];
                return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeAlias)($pos))($usr))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables)($typeByArgName))($t));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 183:4', (sp_toHuman)($ty)))))));
  });
});

const $core$Dict$fromList = (($assocs) => {
  return ((($core$List$for)($assocs))((($keyAndValue) => {
    return (($dict) => {
      return ((($core$Dict$insert)($keyAndValue.first))($keyAndValue.second))($dict);
    });
  })))($core$Dict$empty);
});

const $core$Dict$mapRes = (($func) => {
  return (($dict) => {
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
                return ($core$Result$Ok)(((((($core$Dict$RBNode_elm_builtin)($color))($key))($one))($two))($three));
              })))((($core$Dict$mapRes)($func))($right));
            })))((($core$Dict$mapRes)($func))($left));
          })))((($func)($key))($value));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 436:2', (sp_toHuman)($dict))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandInType = (($ga) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeVariable")
      ? ((() => {
        const $pos = ($ty)[1];
        const $name = ($ty)[2];
        return ($core$Result$Ok)($ty);
      }))()
      : ((($ty)[0] === "TypeFunction")
        ? ((() => {
          const $pos = ($ty)[1];
          const $from = ($ty)[2];
          const $fromIsMutable = ($ty)[3];
          const $to = ($ty)[4];
          return (($core$Result$onOk)((($f) => {
            return (($core$Result$onOk)((($t) => {
              return ($core$Result$Ok)((((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))($f))($fromIsMutable))($t));
            })))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandInType)($ga))($to));
          })))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandInType)($ga))($from));
        }))()
        : ((($ty)[0] === "TypeRecord")
          ? ((() => {
            const $pos = ($ty)[1];
            const $extensible = ($ty)[2];
            const $attrs = ($ty)[3];
            return (($core$Result$map)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($extensible)))((($core$Dict$mapRes)((($k) => {
              return ($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandInType)($ga);
            })))($attrs));
          }))()
          : ((($ty)[0] === "TypeAlias")
            ? ((() => {
              const $pos = ($ty)[1];
              const $path = ($ty)[2];
              const $t = ($ty)[3];
              return (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)($pos))((($core$Core$Cons)("Did we apply aliases twice?"))($core$Core$Nil));
            }))()
            : ((($ty)[0] === "TypeConstant")
              ? ((() => {
                const $pos = ($ty)[1];
                const $usr = ($ty)[2];
                const $args = ($ty)[3];
                return (($core$Result$onOk)((($replacedArgs) => {
                  const $$try1 = (($ga)($pos))($usr);
                  return ((($$try1)[0] === "Err")
                    ? ((() => {
                      const $e = ($$try1)[1];
                      return ($core$Result$Err)($e);
                    }))()
                    : (((($$try1)[0] === "Ok") && ((($$try1)[1])[0] === "TypeDefUnion"))
                      ? ((() => {
                        const $un = (($$try1)[1])[1];
                        return (((sp_not_equal)(($core$List$length)($un.args)))(($core$List$length)($replacedArgs))
                          ? (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)($pos))((($core$Core$Cons)(("union " + ((sp_toHuman)($un.usr) + (" needs " + ((text_fromNumber)(($core$List$length)($un.args)) + " args,"))))))((($core$Core$Cons)(("but was used with " + (text_fromNumber)(($core$List$length)($replacedArgs)))))($core$Core$Nil)))
                          : ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))($usr))($replacedArgs)));
                      }))()
                      : (((($$try1)[0] === "Ok") && ((($$try1)[1])[0] === "TypeDefAlias"))
                        ? ((() => {
                          const $al = (($$try1)[1])[1];
                          return (((sp_not_equal)(($core$List$length)($replacedArgs)))(($core$List$length)($al.args))
                            ? (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)($pos))((($core$Core$Cons)(("alias " + ((sp_toHuman)($al.usr) + (" needs " + ((text_fromNumber)(($core$List$length)($al.args)) + (" args, but was used with " + (text_fromNumber)(($core$List$length)($replacedArgs)))))))))($core$Core$Nil))
                            : ((() => {
                              const $typeByArgName = ($core$Dict$fromList)(((($core$List$map2)((($$name) => {
                                const $name = ($$name)[2];
                                return (($r) => {
                                  return ({
                                    first: $name,
                                    second: $r,
                                  });
                                });
                              })))($al.args))($replacedArgs));
                              return ($core$Result$Ok)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeAlias)($pos))($usr))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAliasVariables)($typeByArgName))($al.type)));
                            }))());
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 39:12', (sp_toHuman)($$try1)))));
                })))((($core$List$mapRes)(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandInType)($ga)))($args));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 19:4', (sp_toHuman)($ty)))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$findMutableArgsThatContainFunctions = (($nonFunctionPos) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeConstant")
      ? $core$Core$Nil
      : ((($ty)[0] === "TypeVariable")
        ? ((() => {
          const $name = ($ty)[2];
          return $core$Core$Nil;
        }))()
        : ((($ty)[0] === "TypeAlias")
          ? ((() => {
            const $path = ($ty)[2];
            const $t = ($ty)[3];
            return (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$findMutableArgsThatContainFunctions)($nonFunctionPos))($t);
          }))()
          : ((($ty)[0] === "TypeFunction")
            ? ((() => {
              const $functionPos = ($ty)[1];
              const $from = ($ty)[2];
              const $fromIsMutable = ($ty)[3];
              const $to = ($ty)[4];
              return ($core$List$concat)((($core$Core$Cons)(((($nonFunctionPos)[0] === "Just")
                ? ((() => {
                  const $constraintPos = ($nonFunctionPos)[1];
                  return (($core$Core$Cons)(({
                    first: $constraintPos,
                    second: $functionPos,
                  })))($core$Core$Nil);
                }))()
                : ((($nonFunctionPos)[0] === "Nothing")
                  ? $core$Core$Nil
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 104:14', (sp_toHuman)($nonFunctionPos))))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$findMutableArgsThatContainFunctions)(($fromIsMutable
                ? ($core$Maybe$Just)($functionPos)
                : $core$Maybe$Nothing)))($from)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$findMutableArgsThatContainFunctions)($nonFunctionPos))($to)))($core$Core$Nil))));
            }))()
            : ((($ty)[0] === "TypeRecord")
              ? ((() => {
                const $ext = ($ty)[2];
                const $attrs = ($ty)[3];
                return (($core$List$concatMap)(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$findMutableArgsThatContainFunctions)($nonFunctionPos)))(($core$Dict$values)($attrs));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 85:4', (sp_toHuman)($ty)))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndValidateType = (($ga) => {
  return (($rawTy) => {
    return (($core$Result$onOk)((($expandedTy) => {
      const $$try1 = (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$findMutableArgsThatContainFunctions)($core$Maybe$Nothing))($expandedTy);
      return ((($$try1)[0] === "Nil")
        ? ($core$Result$Ok)($expandedTy)
        : (true
          ? ((() => {
            const $errors = $$try1;
            return (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)(($$home$nw$stuff$unstable$src$Types$Pos$I)(567)))((($core$Core$Cons)("Mutable arguments can't be or contain functions!"))((($core$Core$Cons)((($core$Text$join)("\n"))((($core$List$map)(sp_toHuman))($errors))))($core$Core$Nil)));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 71:4', (sp_toHuman)($$try1))));
    })))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandInType)($ga))($rawTy));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAnnotation = (($allExpandedTypes) => {
  return (($type) => {
    const $gt = (($pos) => {
      return (($usr) => {
        const $$try1 = (($core$Dict$get)($usr))($allExpandedTypes);
        return ((($$try1)[0] === "Just")
          ? ((() => {
            const $t = ($$try1)[1];
            return ($core$Result$Ok)($t);
          }))()
          : ((($$try1)[0] === "Nothing")
            ? (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)($pos))((($core$Core$Cons)(("Undefined type usr: `" + ((sp_toHuman)($usr) + "`"))))($core$Core$Nil))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 324:8', (sp_toHuman)($$try1))));
      });
    });
    return (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndValidateType)($gt))($type);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$setNonFn = (($name) => {
  return (($state) => {
    return ({
      first: null,
      second: (Object.assign)({}, $state, ({
        nonFnTyvars: ((($core$Dict$insert)($name))($core$Core$Nil))($state.nonFnTyvars),
      })),
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$generateNewTypeVariables = (($tyvarByName) => {
  const $apply = (($name0) => {
    return (($arg) => {
      return (($subs) => {
        const $$nonFn = $arg;
        const $nonFn = $$nonFn.nonFn;
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($name1) => {
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(((($core$Dict$insert)($name0))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)(($$home$nw$stuff$unstable$src$Types$Pos$I)(11)))($name1)))($subs));
          })))(($nonFn
            ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$setNonFn)($name1)
            : ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null)));
        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)($core$Basics$identity));
      });
    });
  });
  return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($tyvarByName))($apply))($core$Dict$empty);
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariablesWithNew = (($freeTypeVariables) => {
  return (($type) => {
    return (((sp_equal)($core$Dict$empty))($freeTypeVariables)
      ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($type)
      : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($newTypeByOldType) => {
        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($newTypeByOldType))($type));
      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$generateNewTypeVariables)($freeTypeVariables)));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyConstructorWithItsArgs = (($p) => {
  const $$try1 = ({
    first: $p.ty,
    second: $p.args,
  });
  return (((($$try1.first)[0] === "TypeFunction") && (($$try1.second)[0] === "Cons"))
    ? ((() => {
      const $from = ($$try1.first)[2];
      const $to = ($$try1.first)[4];
      const $head = ($$try1.second)[1];
      const $tail = ($$try1.second)[2];
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($pa) => {
        const $$isFullyAnnotated = $pa;
        const $vars = $$isFullyAnnotated.vars;
        const $ty = $$isFullyAnnotated.ty;
        const $pos = $$isFullyAnnotated.pos;
        const $isFullyAnnotated = $$isFullyAnnotated.isFullyAnnotated;
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedFrom) => {
          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyConstructorWithItsArgs)((Object.assign)({}, $p, ({
            argIndex: ($p.argIndex + 1),
            args: $tail,
            isFullyAnnotated: ($isFullyAnnotated && $p.isFullyAnnotated),
            ty: $to,
            vars: $vars,
          })));
        })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($p.env))($pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_ConstructorArgument)($p)))($from))($ty));
      })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern)($p.env))($head))($p.vars));
    }))()
    : (((($$try1.first)[0] === "TypeFunction") && (($$try1.second)[0] === "Nil"))
      ? ((() => {
        const $from = ($$try1.first)[2];
        const $to = ($$try1.first)[4];
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ety) => {
          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
            first: $p.vars,
            second: $ety,
            third: $p.isFullyAnnotated,
          }));
        })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($p.pos))((($core$Core$Cons)(("Type constructor " + ((sp_toHuman)($p.usr) + (" is missing argument #" + (text_fromNumber)($p.argIndex))))))($core$Core$Nil)));
      }))()
      : ((($$try1.second)[0] === "Nil")
        ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
            first: $p.vars,
            second: $p.ty,
            third: $p.isFullyAnnotated,
          }));
        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
          return $x.substitutions;
        })))
        : ((($$try1.second)[0] === "Cons")
          ? ((() => {
            const $head = ($$try1.second)[1];
            const $tail = ($$try1.second)[2];
            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ety) => {
              return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                first: $p.vars,
                second: $ety,
                third: $p.isFullyAnnotated,
              }));
            })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($p.pos))((($core$Core$Cons)(("Type constructor " + ((sp_toHuman)($p.usr) + " has too many args"))))($core$Core$Nil)));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1570:4', (sp_toHuman)($$try1))))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern = (($env) => {
  return (($pattern) => {
    return (($vars_) => {
      const $vars = $vars_;
      return ((($pattern)[0] === "PatternAny")
        ? ((() => {
          const $pos = ($pattern)[1];
          const $maybeName = ($pattern)[2];
          const $maybeAnnotation = ($pattern)[3];
          const $isAnnotated = ((sp_not_equal)($core$Maybe$Nothing))($maybeAnnotation);
          const $makeType = ((($maybeAnnotation)[0] === "Nothing")
            ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos)
            : ((($maybeAnnotation)[0] === "Just")
              ? ((() => {
                const $type = ($maybeAnnotation)[1];
                const $$try2 = (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAnnotation)($env.types))($type);
                return ((($$try2)[0] === "Err")
                  ? ((() => {
                    const $e = ($$try2)[1];
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos);
                    })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertError)($e));
                  }))()
                  : ((($$try2)[0] === "Ok")
                    ? ((() => {
                      const $t = ($$try2)[1];
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($t);
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1487:24', (sp_toHuman)($$try2))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1482:16', (sp_toHuman)($maybeAnnotation))));
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($type) => {
            const $newVars = ((($maybeName)[0] === "Nothing")
              ? $vars
              : ((($maybeName)[0] === "Just")
                ? ((() => {
                  const $name = ($maybeName)[1];
                  return ((($core$Dict$insert)($name))(({
                    isAnnotated: $isAnnotated,
                    pos: $pos,
                    type: $type,
                  })))($vars);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1498:16', (sp_toHuman)($maybeName))));
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
              isFullyAnnotated: $isAnnotated,
              pos: $pos,
              ty: $type,
              vars: $newVars,
            }));
          })))($makeType);
        }))()
        : ((($pattern)[0] === "PatternLiteralNumber")
          ? ((() => {
            const $pos = ($pattern)[1];
            const $literal = ($pattern)[2];
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
              isFullyAnnotated: true,
              pos: $pos,
              ty: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$number,
              vars: $vars,
            }));
          }))()
          : ((($pattern)[0] === "PatternLiteralText")
            ? ((() => {
              const $pos = ($pattern)[1];
              const $literal = ($pattern)[2];
              return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                isFullyAnnotated: true,
                pos: $pos,
                ty: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$text,
                vars: $vars,
              }));
            }))()
            : ((($pattern)[0] === "PatternConstructor")
              ? ((() => {
                const $pos = ($pattern)[1];
                const $usr = ($pattern)[2];
                const $args = ($pattern)[3];
                const $constructorTyM = ((() => {
                  const $$try1 = (($core$Dict$get)($usr))($env.constructors);
                  return ((($$try1)[0] === "Nothing")
                    ? ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($usr))
                    : ((($$try1)[0] === "Just")
                      ? ((() => {
                        const $c = ($$try1)[1];
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariablesWithNew)(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars)($core$Dict$empty))($core$Dict$empty))($c.type)))($c.type);
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1512:16', (sp_toHuman)($$try1))));
                }))();
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($constructorTy) => {
                  const $p = ({
                    argIndex: 0,
                    args: $args,
                    env: $env,
                    isFullyAnnotated: true,
                    pos: $pos,
                    ty: $constructorTy,
                    usr: $usr,
                    vars: $vars,
                  });
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($$isFullyAnnotated) => {
                    const $patternVars = $$isFullyAnnotated.first;
                    const $patternTy = $$isFullyAnnotated.second;
                    const $isFullyAnnotated = $$isFullyAnnotated.third;
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                      isFullyAnnotated: $isFullyAnnotated,
                      pos: $pos,
                      ty: $patternTy,
                      vars: $patternVars,
                    }));
                  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyConstructorWithItsArgs)($p));
                })))($constructorTyM);
              }))()
              : ((($pattern)[0] === "PatternRecord")
                ? ((() => {
                  const $pos = ($pattern)[1];
                  const $attrs = ($pattern)[2];
                  const $blah = (($name) => {
                    return (($pa) => {
                      return (($$annotatedSoFar) => {
                        const $varsX = $$annotatedSoFar.first;
                        const $attrTypes = $$annotatedSoFar.second;
                        const $annotatedSoFar = $$annotatedSoFar.third;
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($paOut) => {
                          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                            first: $paOut.vars,
                            second: ((($core$Dict$insert)($name))($paOut.ty))($attrTypes),
                            third: ($paOut.isFullyAnnotated && $annotatedSoFar),
                          }));
                        })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern)($env))($pa))($varsX));
                      });
                    });
                  });
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($$attrTypes) => {
                    const $vars1 = $$attrTypes.first;
                    const $attrTypes = $$attrTypes.second;
                    const $isFullyAnnotated = $$attrTypes.third;
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                      isFullyAnnotated: $isFullyAnnotated,
                      pos: $pos,
                      ty: ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($core$Maybe$Nothing))($attrTypes),
                      vars: $vars1,
                    }));
                  })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($attrs))($blah))(({
                    first: $vars,
                    second: $core$Dict$empty,
                    third: true,
                  })));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1475:4', (sp_toHuman)($pattern)))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromParameter = (($env) => {
  return (($param) => {
    return ((($param)[0] === "ParameterPattern")
      ? ((() => {
        const $pattern = ($param)[1];
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($patternOut) => {
          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
            first: false,
            second: $patternOut,
          }));
        })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern)($env))($pattern))($core$Dict$empty));
      }))()
      : ((($param)[0] === "ParameterMutable")
        ? ((() => {
          const $pos = ($param)[1];
          const $paramName = ($param)[2];
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
            const $vars = (($core$Dict$singleton)($paramName))(({
              isAnnotated: false,
              pos: $pos,
              type: $ty,
            }));
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
              first: true,
              second: ({
                isFullyAnnotated: false,
                pos: $pos,
                ty: $ty,
                vars: $vars,
              }),
            }));
          })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1370:4', (sp_toHuman)($param))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVar = (($pars) => {
  return (($name) => {
    return (($patternVar) => {
      return (($env) => {
        const $refinedTy = ($patternVar.isAnnotated
          ? $patternVar.type
          : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariables)($pars.subs))($patternVar.type));
        const $ref = ($pars.isRoot
          ? ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($env.currentModule))($name))
          : ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefBlock)($name));
        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
          constructors: $env.constructors,
          currentModule: $env.currentModule,
          instanceVariables: ((($core$Dict$insert)($ref))(({
            definedAt: $patternVar.pos,
            freeTypeVariables: (($pars.isMutable || $pars.isParameter)
              ? $core$Dict$empty
              : ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars)($env.nonFreeTyvars))($core$Dict$empty))($refinedTy)),
            isMutable: $pars.isMutable,
            ty: $refinedTy,
          })))($env.instanceVariables),
          meta: $env.meta,
          nonAnnotatedRecursives: ($patternVar.isAnnotated
            ? $env.nonAnnotatedRecursives
            : ((($core$Dict$insert)($name))($patternVar.pos))($env.nonAnnotatedRecursives)),
          nonFreeTyvars: ($pars.isParameter
            ? ((($core$Dict$for)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($refinedTy)))($core$Dict$insert))($env.nonFreeTyvars)
            : $env.nonFreeTyvars),
          types: $env.types,
        }));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars = (($pars) => {
  return (($vars) => {
    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($vars))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVar)($pars));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPatternAndBlock = (($env) => {
  return (($$block) => {
    const $pattern = $$block.first;
    const $block = $$block.second;
    return (($$blockTypeSoFar) => {
      const $patternTypeSoFar = $$blockTypeSoFar.first;
      const $blockTypeSoFar = $$blockTypeSoFar.second;
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($patternOut) => {
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedPatternType) => {
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($env1) => {
            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
              const $ip = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars)(({
                isMutable: false,
                isParameter: false,
                isRoot: false,
                subs: $subs,
              })))($patternOut.vars))($env1);
              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($patternEnv) => {
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($blockType) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedBlockType) => {
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(({
                      first: $unifiedPatternType,
                      second: $unifiedBlockType,
                    }));
                  })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($patternOut.pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_TryBlock)($block)))($blockTypeSoFar))($blockType));
                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($patternEnv))($block));
              })))($ip);
            })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
              return $x.substitutions;
            })));
          })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToNonFreeTyvars)($env));
        })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($patternOut.pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_TryPattern))($patternOut.ty))($patternTypeSoFar));
      })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern)($env))($pattern))($core$Dict$empty));
    });
  });
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$argumentPos = (($arg) => {
  return ((($arg)[0] === "ArgumentExpression")
    ? ((() => {
      const $e = ($arg)[1];
      return ($$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos)($e);
    }))()
    : ((($arg)[0] === "ArgumentMutable")
      ? ((() => {
        const $pos = ($arg)[1];
        return $pos;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 234:4', (sp_toHuman)($arg))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyFunctionOnCallAndYieldReturnType = (($env) => {
  return (($reference) => {
    return (($referenceType) => {
      return (($callIsMutable) => {
        return (($argument) => {
          return (($callArgumentType) => {
            return ((($referenceType)[0] === "TypeFunction")
              ? ((() => {
                const $refArgumentType = ($referenceType)[2];
                const $refIsMutable = ($referenceType)[3];
                const $refReturnType = ($referenceType)[4];
                return (((sp_not_equal)($refIsMutable))($callIsMutable)
                  ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos)($reference)))((($core$Core$Cons)("mutability clash 2"))($core$Core$Nil))
                  : ((() => {
                    const $pos = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos)($reference);
                    const $reason = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_CallArgument)(({
                      argument: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$argumentPos)($argument),
                      reference: $pos,
                    }));
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedArgumentType) => {
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($refReturnType);
                    })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))($reason))($refArgumentType))($callArgumentType));
                  }))());
              }))()
              : ((($referenceType)[0] === "TypeVariable")
                ? ((() => {
                  const $pos = ($referenceType)[1];
                  const $name = ($referenceType)[2];
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($returnType) => {
                    const $ty = (((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))($callArgumentType))($callIsMutable))($returnType);
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                      return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($returnType);
                    })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IsBeingCalledAsAFunction)($pos))($referenceType)))($referenceType))($ty));
                  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos));
                }))()
                : ((($referenceType)[0] === "TypeAlias")
                  ? ((() => {
                    const $pos = ($referenceType)[1];
                    const $ty = ($referenceType)[3];
                    return (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyFunctionOnCallAndYieldReturnType)($env))($reference))($ty))($callIsMutable))($argument))($callArgumentType);
                  }))()
                  : (true
                    ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$expressionPos)($reference)))((($core$Core$Cons)("This is being called like a function, but its type is"))((($core$Core$Cons)(""))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($referenceType)))($core$Core$Nil))))
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1270:4', (sp_toHuman)($referenceType))))));
          });
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression = (($env) => {
  return (($expression) => {
    return ((($expression)[0] === "LiteralText")
      ? ((() => {
        const $pos = ($expression)[1];
        const $l = ($expression)[2];
        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text);
      }))()
      : ((($expression)[0] === "LiteralNumber")
        ? ((() => {
          const $pos = ($expression)[1];
          const $l = ($expression)[2];
          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number);
        }))()
        : ((($expression)[0] === "Variable")
          ? ((() => {
            const $pos = ($expression)[1];
            const $attrPath = ($expression)[2].attrPath;
            const $ref = ($expression)[2].ref;
            const $$try2 = (($core$Dict$get)($ref))($env.instanceVariables);
            return ((($$try2)[0] === "Nothing")
              ? ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))($ref)
              : ((($$try2)[0] === "Just")
                ? ((() => {
                  const $var = ($$try2)[1];
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($varType) => {
                    return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applyAttributePath)($env))($pos))($attrPath))($varType);
                  })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariablesWithNew)($var.freeTypeVariables))($var.ty));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1114:12', (sp_toHuman)($$try2))));
          }))()
          : ((($expression)[0] === "Constructor")
            ? ((() => {
              const $pos = ($expression)[1];
              const $usr = ($expression)[2];
              const $$try1 = (($core$Dict$get)($usr))($env.constructors);
              return ((($$try1)[0] === "Nothing")
                ? ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($usr))
                : ((($$try1)[0] === "Just")
                  ? ((() => {
                    const $c = ($$try1)[1];
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariablesWithNew)(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars)($core$Dict$empty))($core$Dict$empty))($c.type)))($c.type);
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1188:12', (sp_toHuman)($$try1))));
            }))()
            : ((($expression)[0] === "Lambda")
              ? ((() => {
                const $pos = ($expression)[1];
                const $param = ($expression)[2];
                const $body = ($expression)[3];
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($$isMutable) => {
                  const $isMutable = $$isMutable.first;
                  const $patternOut = $$isMutable.second;
                  const $ip = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars)(({
                    isMutable: $isMutable,
                    isParameter: true,
                    isRoot: false,
                    subs: $core$Dict$empty,
                  })))($patternOut.vars))($env);
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($bodyEnv) => {
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($bodyType) => {
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($refinedPatternOutTy) => {
                        return (($isMutable && ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($refinedPatternOutTy))
                          ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorTodo)($pos))("mutable args cannot be functions")
                          : ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)((((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))($refinedPatternOutTy))($isMutable))($bodyType)));
                      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($patternOut.ty));
                    })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($bodyEnv))($body));
                  })))($ip);
                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromParameter)($env))($param));
              }))()
              : ((($expression)[0] === "Call")
                ? ((() => {
                  const $pos = ($expression)[1];
                  const $reference = ($expression)[2];
                  const $argument = ($expression)[3];
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($referenceType) => {
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($$argumentType) => {
                      const $fromIsMutable = $$argumentType.first;
                      const $argumentType = $$argumentType.second;
                      return (((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unifyFunctionOnCallAndYieldReturnType)($env))($reference))($referenceType))($fromIsMutable))($argument))($argumentType);
                    })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromArgument)($env))($argument));
                  })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($reference));
                }))()
                : ((($expression)[0] === "If")
                  ? ((() => {
                    const $pos = ($expression)[1];
                    const $ar = ($expression)[2];
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($s) => {
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($trueType) => {
                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($falseType) => {
                            return ((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IfBranches))($trueType))($falseType);
                          })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($ar.false));
                        })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($ar.true));
                      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                        return $x.substitutions;
                      })));
                    })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool))($ar.condition));
                  }))()
                  : ((($expression)[0] === "Try")
                    ? ((() => {
                      const $pos = ($expression)[1];
                      const $value = ($expression)[2];
                      const $patternsAndBlocks = ($expression)[3];
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($tryType) => {
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($newBlockType) => {
                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($$inferredBlockType) => {
                            const $patternType = $$inferredBlockType.first;
                            const $inferredBlockType = $$inferredBlockType.second;
                            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($inferredBlockType);
                          })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for)($patternsAndBlocks))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPatternAndBlock)($env)))(({
                            first: $tryType,
                            second: $newBlockType,
                          })));
                        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newType)($pos));
                      })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($value));
                    }))()
                    : ((($expression)[0] === "Record")
                      ? ((() => {
                        const $pos = ($expression)[1];
                        const $maybeExt = ($expression)[2];
                        const $attrValues = ($expression)[3];
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($attrTypes) => {
                          return ((($maybeExt)[0] === "Nothing")
                            ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($core$Maybe$Nothing))($attrTypes))
                            : ((($maybeExt)[0] === "Just")
                              ? ((() => {
                                const $variableArgs = ($maybeExt)[1];
                                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty_) => {
                                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($name) => {
                                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedType) => {
                                        return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($unifiedType);
                                      })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_AttributeUpdate)(($core$Dict$keys)($attrTypes))))($ty))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))(($core$Maybe$Just)($name)))($attrTypes)));
                                    })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$newName)($core$Basics$identity));
                                  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($ty_));
                                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))($variableArgs)));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1244:12', (sp_toHuman)($maybeExt))));
                        })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_map)((($k) => {
                          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env);
                        })))($attrValues));
                      }))()
                      : ((($expression)[0] === "LetIn")
                        ? ((() => {
                          const $valueDef = ($expression)[1];
                          const $e = ($expression)[2];
                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($env1) => {
                            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                              return (($valueDef.mutable && ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($ty))
                                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($ty);
                                })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($valueDef.pattern)))((($core$Core$Cons)("blocks that define mutables can't return functions"))($core$Core$Nil)))
                                : ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($ty));
                            })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env1))($e));
                          })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromDefinition)(false))($valueDef))($env));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1106:4', (sp_toHuman)($expression))))))))))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression = (($env) => {
  return (($expectedType_) => {
    return (($expression) => {
      const $expectedType = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$expandAlias)($expectedType_);
      return ((($expression)[0] === "LiteralText")
        ? ((() => {
          const $pos = ($expression)[1];
          const $l = ($expression)[2];
          return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text);
        }))()
        : ((($expression)[0] === "LiteralNumber")
          ? ((() => {
            const $pos = ($expression)[1];
            const $l = ($expression)[2];
            return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number);
          }))()
          : ((($expression)[0] === "Variable")
            ? ((() => {
              const $pos = ($expression)[1];
              const $attrPath = ($expression)[2].attrPath;
              const $ref = ($expression)[2].ref;
              const $$try7 = (($core$Dict$get)($ref))($env.instanceVariables);
              return ((($$try7)[0] === "Nothing")
                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))($ref))
                : ((($$try7)[0] === "Just")
                  ? ((() => {
                    const $var = ($$try7)[1];
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($instantiatedType) => {
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                        return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($ty);
                      })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applyAttributePath)($env))($pos))($attrPath))($instantiatedType));
                    })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariablesWithNew)($var.freeTypeVariables))($var.ty));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 833:12', (sp_toHuman)($$try7))));
            }))()
            : ((($expression)[0] === "Constructor")
              ? ((() => {
                const $pos = ($expression)[1];
                const $usr = ($expression)[2];
                const $$try6 = (($core$Dict$get)($usr))($env.constructors);
                return ((($$try6)[0] === "Nothing")
                  ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                  })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($usr)))
                  : ((($$try6)[0] === "Just")
                    ? ((() => {
                      const $c = ($$try6)[1];
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($instantiatedType) => {
                        return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($instantiatedType);
                      })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$replaceTypeVariablesWithNew)(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars)($core$Dict$empty))($core$Dict$empty))($c.type)))($c.type));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 852:12', (sp_toHuman)($$try6))));
              }))()
              : ((($expression)[0] === "Lambda")
                ? ((() => {
                  const $pos = ($expression)[1];
                  const $param = ($expression)[2];
                  const $body = ($expression)[3];
                  const $$try5 = ({
                    first: $expectedType,
                    second: $param,
                  });
                  return (((($$try5.first)[0] === "TypeFunction") && (($$try5.first)[3] && (($$try5.second)[0] === "ParameterMutable")))
                    ? ((() => {
                      const $parameterType = ($$try5.first)[2];
                      const $returnType = ($$try5.first)[4];
                      const $parameterPos = ($$try5.second)[1];
                      const $parameterName = ($$try5.second)[2];
                      const $ip = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars)(({
                        isMutable: true,
                        isParameter: true,
                        isRoot: false,
                        subs: $core$Dict$empty,
                      })))((($core$Dict$singleton)($parameterName))(({
                        isAnnotated: true,
                        pos: $parameterPos,
                        type: $parameterType,
                      }))))($env);
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($localEnv) => {
                        return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($localEnv))($returnType))($body);
                      })))($ip);
                    }))()
                    : (((($$try5.first)[0] === "TypeFunction") && (!(($$try5.first)[3]) && (($$try5.second)[0] === "ParameterPattern")))
                      ? ((() => {
                        const $parameterType = ($$try5.first)[2];
                        const $returnType = ($$try5.first)[4];
                        const $pattern = ($$try5.second)[1];
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($localEnv) => {
                          return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($localEnv))($returnType))($body);
                        })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkAndInsertPattern)($env))($parameterType))($pattern));
                      }))()
                      : ((($$try5.first)[0] === "TypeFunction")
                        ? ((() => {
                          const $isMutable = ($$try5.first)[3];
                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("the function and the annotation have different mutability"))($core$Core$Nil));
                        }))()
                        : ((($$try5.first)[0] === "TypeVariable")
                          ? ((() => {
                            const $pos = ($$try5.first)[1];
                            const $name = ($$try5.first)[2];
                            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isAnnotation)($name)
                              ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This is a function, but the annotation says it should be of type variable `" + ($name + "` which implies that it could be of any type!"))))($core$Core$Nil))
                              : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($actualType) => {
                                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                  return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                                })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_IsLambda))($expectedType))($actualType));
                              })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($expression)));
                          }))()
                          : (true
                            ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("This is a function, but the annotation says it should be a: "))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType)))($core$Core$Nil)))
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 866:12', (sp_toHuman)($$try5)))))));
                }))()
                : ((($expression)[0] === "Call")
                  ? ((() => {
                    const $pos = ($expression)[1];
                    const $reference = ($expression)[2];
                    const $argument = ($expression)[3];
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($referenceType_) => {
                      const $referenceType = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$expandAlias)($referenceType_);
                      return ((($referenceType)[0] === "TypeFunction")
                        ? ((() => {
                          const $parameterType = ($referenceType)[2];
                          const $isMutable = ($referenceType)[3];
                          const $returnType = ($referenceType)[4];
                          return ((($argument)[0] === "ArgumentExpression")
                            ? ((() => {
                              const $argumentExpression = ($argument)[1];
                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($argumentType) => {
                                const $reason = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_CallArgument)(({
                                  argument: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$argumentPos)($argument),
                                  reference: $pos,
                                }));
                                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedArgumentType) => {
                                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($actualReturnType) => {
                                    return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($actualReturnType);
                                  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($returnType));
                                })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))($reason))($argumentType))($parameterType));
                              })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($argumentExpression));
                            }))()
                            : ((($argument)[0] === "ArgumentMutable")
                              ? ((() => {
                                const $pos = ($argument)[1];
                                const $attrPath = ($argument)[2].attrPath;
                                const $ref = ($argument)[2].ref;
                                const $$try4 = (($core$Dict$get)($ref))($env.instanceVariables);
                                return ((($$try4)[0] === "Nothing")
                                  ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                                    return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                                  })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$errorUndefinedVariable)($env))($pos))($ref))
                                  : ((($$try4)[0] === "Just")
                                    ? ((() => {
                                      const $var = ($$try4)[1];
                                      return (($core$Basics$not)($var.isMutable)
                                        ? ((() => {
                                          const $ae = (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)(("You are trying to mutate variable `" + ((sp_toHuman)($ref) + "` but it was declared as not mutable!"))))((($core$Core$Cons)(""))((($core$Core$Cons)("TODO [link to wiki page that explains how to declare variables]"))($core$Core$Nil))));
                                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                                            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                                          })))($ae);
                                        }))()
                                        : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($var.ty)
                                          ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                                            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                                          })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)("mutable arguments can't allow functions"))($core$Core$Nil)))
                                          : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($ty) => {
                                            const $reason = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_CallArgument)(({
                                              argument: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$argumentPos)($argument),
                                              reference: $pos,
                                            }));
                                            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedArgumentType) => {
                                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($actualReturnType) => {
                                                return (((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isCompatibleWith)($env))($expectedType))($pos))($actualReturnType);
                                              })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($returnType));
                                            })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($pos))($reason))($ty))($parameterType));
                                          })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applyAttributePath)($env))($pos))($attrPath))($var.ty))));
                                    }))()
                                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 946:28', (sp_toHuman)($$try4))));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 916:20', (sp_toHuman)($argument))));
                        }))()
                        : (true
                          ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("The code is trying to call this as if it was a function, but its type is: "))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($referenceType)))($core$Core$Nil)))
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 913:12', (sp_toHuman)($referenceType))));
                    })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($reference));
                  }))()
                  : ((($expression)[0] === "If")
                    ? ((() => {
                      const $pos = ($expression)[1];
                      const $condition = ($expression)[2].condition;
                      const $false = ($expression)[2].false;
                      const $true = ($expression)[2].true;
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                          return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($expectedType))($false);
                        })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($expectedType))($true));
                      })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool))($condition));
                    }))()
                    : ((($expression)[0] === "Try")
                      ? ((() => {
                        const $pos = ($expression)[1];
                        const $value = ($expression)[2];
                        const $patternsAndBlocks = ($expression)[3];
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($inferredValueType) => {
                          const $xxx = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for)($patternsAndBlocks))((($$block) => {
                            const $pattern = $$block.first;
                            const $block = $$block.second;
                            return (($patternTypeSoFar) => {
                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($patternOut) => {
                                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedPatternType) => {
                                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($env1) => {
                                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
                                      const $ip = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars)(({
                                        isMutable: false,
                                        isParameter: false,
                                        isRoot: false,
                                        subs: $subs,
                                      })))($patternOut.vars))($env1);
                                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($patternEnv) => {
                                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                          return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($unifiedPatternType);
                                        })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($patternEnv))($expectedType))($block));
                                      })))($ip);
                                    })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                                      return $x.substitutions;
                                    })));
                                  })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToNonFreeTyvars)($env));
                                })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env))($patternOut.pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_TryPattern))($patternOut.ty))($patternTypeSoFar));
                              })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern)($env))($pattern))($core$Dict$empty));
                            });
                          })))($inferredValueType);
                          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
                          })))($xxx);
                        })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env))($value));
                      }))()
                      : ((($expression)[0] === "Record")
                        ? ((() => {
                          const $pos = ($expression)[1];
                          const $maybeExtending = ($expression)[2];
                          const $attrValueByName = ($expression)[3];
                          return (((($expectedType)[0] === "TypeRecord") && ((($expectedType)[2])[0] === "Just"))
                            ? ((() => {
                              const $attrTypeByName = ($expectedType)[3];
                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("Extensible record annotation is experimentally disabled [TODO link to why]"))($core$Core$Nil));
                            }))()
                            : (((($expectedType)[0] === "TypeRecord") && ((($expectedType)[2])[0] === "Nothing"))
                              ? ((() => {
                                const $attrTypeByName = ($expectedType)[3];
                                return ((($maybeExtending)[0] === "Nothing")
                                  ? ((() => {
                                    const $xxx = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($attrValueByName))((($attrName) => {
                                      return (($attrValue) => {
                                        return (() => {
                                          const $$try3 = (($core$Dict$get)($attrName))($attrTypeByName);
                                          return ((($$try3)[0] === "Nothing")
                                            ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This record has an attribute `" + ($attrName + "` which is not in the annotation."))))($core$Core$Nil))
                                            : ((($$try3)[0] === "Just")
                                              ? ((() => {
                                                const $expectedAttrType = ($$try3)[1];
                                                return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($expectedAttrType))($attrValue);
                                              }))()
                                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1039:36', (sp_toHuman)($$try3))));
                                        });
                                      });
                                    })))(null);
                                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                      return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($attrTypeByName))((($attrName) => {
                                        return (($attrType) => {
                                          return (() => {
                                            const $$try2 = (($core$Dict$get)($attrName))($attrTypeByName);
                                            return ((($$try2)[0] === "Nothing")
                                              ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This record is missing the attribute `" + ($attrName + "`"))))($core$Core$Nil))
                                              : ((($$try2)[0] === "Just")
                                                ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null)
                                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1051:36', (sp_toHuman)($$try2))));
                                          });
                                        });
                                      })))(null);
                                    })))($xxx);
                                  }))()
                                  : ((($maybeExtending)[0] === "Just")
                                    ? ((() => {
                                      const $extending = ($maybeExtending)[1];
                                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                        return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$dict_for)($attrValueByName))((($attrName) => {
                                          return (($attrValue) => {
                                            return (() => {
                                              const $$try1 = (($core$Dict$get)($attrName))($attrTypeByName);
                                              return ((($$try1)[0] === "Nothing")
                                                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)(("This record has an attribute `" + ($attrName + "` which is not in the annotation"))))($core$Core$Nil))
                                                : ((($$try1)[0] === "Just")
                                                  ? ((() => {
                                                    const $expectedAttrType = ($$try1)[1];
                                                    return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($expectedAttrType))($attrValue);
                                                  }))()
                                                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1067:32', (sp_toHuman)($$try1))));
                                            });
                                          });
                                        })))(null);
                                      })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env))($expectedType))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($pos))($extending)));
                                    }))()
                                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1035:20', (sp_toHuman)($maybeExtending))));
                              }))()
                              : (true
                                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)($pos))((($core$Core$Cons)("This is a record, but the annotation says that this should be a"))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($expectedType)))($core$Core$Nil)))
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 1028:12', (sp_toHuman)($expectedType)))));
                        }))()
                        : ((($expression)[0] === "LetIn")
                          ? ((() => {
                            const $valueDef = ($expression)[1];
                            const $e = ($expression)[2];
                            return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($env1) => {
                              const $xxx = (($valueDef.mutable && ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeContainsFunctions)($expectedType))
                                ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addCheckError)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($valueDef.pattern)))((($core$Core$Cons)("blocks that define mutables can't return functions"))($core$Core$Nil))
                                : ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null));
                              return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                                return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env1))($expectedType))($e);
                              })))($xxx);
                            })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromDefinition)(false))($valueDef))($env));
                          }))()
                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 825:4', (sp_toHuman)($expression))))))))))));
    });
  });
});

const $core$Dict$filter = (($isGood) => {
  return (($dict) => {
    return ((($core$Dict$for)($dict))((($k) => {
      return (($v) => {
        return (($d) => {
          return ((($isGood)($k))($v)
            ? ((($core$Dict$insert)($k))($v))($d)
            : $d);
        });
      });
    })))($core$Dict$empty);
  });
});

const $core$Dict$size = ((() => {
  const $sizeHelp = (($n) => {
    return (($dict) => {
      return ((($dict)[0] === "RBEmpty_elm_builtin")
        ? $n
        : ((($dict)[0] === "RBNode_elm_builtin")
          ? ((() => {
            const $left = ($dict)[4];
            const $right = ($dict)[5];
            return (($sizeHelp)((($sizeHelp)(($n + 1)))($right)))($left);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 57:4', (sp_toHuman)($dict))));
    });
  });
  return ($sizeHelp)(0);
}))();

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkFreeVariables = (($env) => {
  return (($pos) => {
    return (($patternType) => {
      return (($blockType) => {
        const $annotatedFreeVars = (($core$Dict$filter)((($name) => {
          return (() => {
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$isAnnotation)($name);
          });
        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($patternType));
        const $actualFreeVars = ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeTyvars)($blockType);
        return ((($core$Dict$size)($annotatedFreeVars) > ($core$Dict$size)($actualFreeVars))
          ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
            return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null);
          })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$addError)($pos))((($core$Core$Cons)("The annotation is too general"))((($core$Core$Cons)(""))((($core$Core$Cons)(("The annotation uses: " + (($core$Text$join)(", "))(($core$Dict$keys)($annotatedFreeVars)))))((($core$Core$Cons)(""))((($core$Core$Cons)(("But the actual type uses only: " + (($core$Text$join)(", "))(($core$Dict$keys)($actualFreeVars)))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$typeToText)($env))($blockType)))((($core$Core$Cons)(""))((($core$Core$Cons)(("The annotation has " + ((text_fromNumber)((($core$Dict$size)($annotatedFreeVars) - ($core$Dict$size)($actualFreeVars))) + " type variables too many"))))($core$Core$Nil))))))))))
          : ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)(null));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromDefinition = (($isRoot) => {
  return (($def) => {
    return (($env) => {
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($patternOut) => {
        const $ip = ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars)(({
          isMutable: $def.mutable,
          isParameter: false,
          isRoot: $isRoot,
          subs: $core$Dict$empty,
        })))($patternOut.vars))($env);
        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($env1) => {
          return ($def.native
            ? ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env1)
            : ($patternOut.isFullyAnnotated
              ? (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                return ($$home$nw$stuff$unstable$src$Compiler$TypeCheck$return)($env1);
              })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkExpression)($env1))($patternOut.ty))($def.body))
              : (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($bodyType_) => {
                return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($bodyType) => {
                  return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($unifiedType) => {
                    return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((() => {
                      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($env2) => {
                        return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$andThen)((($subs) => {
                          return ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$insertPatternVars)(({
                            isMutable: $def.mutable,
                            isParameter: false,
                            isRoot: $isRoot,
                            subs: $subs,
                          })))($patternOut.vars))($env2);
                        })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$get)((($x) => {
                          return $x.substitutions;
                        })));
                      })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToNonFreeTyvars)($env1));
                    })))((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$checkFreeVariables)($env1))($patternOut.pos))($patternOut.ty))($bodyType));
                  })))(((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$unify)($env1))($patternOut.pos))($$home$nw$stuff$unstable$src$Compiler$TypeCheck$UnifyReason_DefBlockVsPattern))($bodyType))($patternOut.ty));
                })))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$applySubsToType)($bodyType_));
              })))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromExpression)($env1))($def.body))));
        })))($ip);
      })))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromPattern)($env))($def.pattern))($core$Dict$empty));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$initState = ({
  errors: $core$Core$Nil,
  nextName: 0,
  nonFnTyvars: $core$Dict$empty,
  substitutions: $core$Dict$empty,
  typeClashesByPlaceholderId: $core$Maybe$Nothing,
});

const $$home$nw$stuff$unstable$src$StateMonad$run = (($state) => {
  return (($m) => {
    return ($m)($state);
  });
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNamedTypes = (($p) => {
  return (((($p)[0] === "PatternAny") && ((($p)[2])[0] === "Nothing"))
    ? ((() => {
      const $pos = ($p)[1];
      return $core$Dict$empty;
    }))()
    : (((($p)[0] === "PatternAny") && ((($p)[2])[0] === "Just"))
      ? ((() => {
        const $pos = ($p)[1];
        const $n = (($p)[2])[1];
        const $maybeType = ($p)[3];
        return (($core$Dict$singleton)($n))(({
          first: $pos,
          second: $maybeType,
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
              return ((($core$List$for)($ps))((($x) => {
                return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNamedTypes)($x));
              })))($core$Dict$empty);
            }))()
            : ((($p)[0] === "PatternRecord")
              ? ((() => {
                const $pos = ($p)[1];
                const $ps = ($p)[2];
                return ((($core$Dict$for)($ps))((($k) => {
                  return (($v) => {
                    return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNamedTypes)($v));
                  });
                })))($core$Dict$empty);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 223:4', (sp_toHuman)($p))))))));
});

const $core$List$all = (($fun) => {
  return (($list) => {
    return ((($list)[0] === "Nil")
      ? true
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $h = ($list)[1];
          const $t = ($list)[2];
          return (($fun)($h)
            ? (($core$List$all)($fun))($t)
            : false);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 13:4', (sp_toHuman)($list))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromModule = (($env) => {
  return (($module) => {
    (sp_benchStart)(null);
    const $insert = (($pa) => {
      return (($def) => {
        return (($$ann) => {
          const $ann = $$ann.first;
          const $nonAnn = $$ann.second;
          const $allAnnotated = (($core$List$all)((($$maybeType) => {
            const $pos = $$maybeType.first;
            const $maybeType = $$maybeType.second;
            return ((sp_not_equal)($core$Maybe$Nothing))($maybeType);
          })))(($core$Dict$values)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNamedTypes)($pa)));
          return ($allAnnotated
            ? ({
              first: ((sp_cons)($ann))($def),
              second: $nonAnn,
            })
            : ({
              first: $ann,
              second: ((sp_cons)($nonAnn))($def),
            }));
        });
      });
    });
    const $$annotated = ((($core$Dict$for)($module.valueDefs))($insert))(({
      first: $core$Core$Nil,
      second: $core$Core$Nil,
    }));
    const $nonAnnotated = $$annotated.second;
    const $annotated = $$annotated.first;
    return (((($nonAnnotated)[0] === "Cons") && ((($nonAnnotated)[2])[0] === "Cons"))
      ? ((() => {
        const $first = ($nonAnnotated)[1];
        const $second = (($nonAnnotated)[2])[1];
        const $tail = (($nonAnnotated)[2])[2];
        const $pos = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($first.pattern);
        const $names = (($core$List$concatMap)((($d) => {
          return ($core$Dict$keys)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNamedTypes)($d.pattern));
        })))($nonAnnotated);
        return (($$home$nw$stuff$unstable$src$Compiler$Error$res)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternPos)($first.pattern)))((($eenv) => {
          return ((($core$Basics$btw)(sp_benchStop))("type check"))((($core$Core$Cons)("Support for non-annotated root definitions is not yet implemented. =*("))((($core$Core$Cons)(("These definitions need an annotation: " + (($core$Text$join)(", "))($names))))($core$Core$Nil)));
        }));
      }))()
      : (true
        ? ((() => {
          const $orderedNonAnnotated = $nonAnnotated;
          const $allOrdered = ($core$List$concat)((($core$Core$Cons)($orderedNonAnnotated))((($core$Core$Cons)($annotated))($core$Core$Nil)));
          const $$envF = (($$home$nw$stuff$unstable$src$StateMonad$run)($$home$nw$stuff$unstable$src$Compiler$TypeCheck$initState))(((($$home$nw$stuff$unstable$src$Compiler$TypeCheck$list_for)($allOrdered))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromDefinition)(true)))($env));
          const $stateF = $$envF.second;
          const $envF = $$envF.first;
          return (((sp_equal)($core$Core$Nil))($stateF.errors)
            ? ((($core$Basics$btw)(sp_benchStop))("type check"))(($core$Result$Ok)($envF))
            : ((($core$Basics$btw)(sp_benchStop))("type check"))(($core$Result$Err)(($$home$nw$stuff$unstable$src$Compiler$Error$Nested)($stateF.errors))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 386:4', (sp_toHuman)($nonAnnotated))));
  });
});

const $core$Dict$mapKeys = (($func) => {
  return (($dict) => {
    return ((($core$Dict$for)($dict))((($k) => {
      return ($core$Dict$insert)(($func)($k));
    })))($core$Dict$empty);
  });
});

const $$home$nw$stuff$unstable$src$Compile$typeCheckModule = (($meta) => {
  return (($globals) => {
    return (($module) => {
      const $env = ({
        constructors: $globals.constructors,
        currentModule: $module.umr,
        instanceVariables: (($core$Dict$mapKeys)($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot))($globals.instanceVariables),
        meta: $meta,
        nonAnnotatedRecursives: $core$Dict$empty,
        nonFreeTyvars: $core$Dict$empty,
        types: $globals.types,
      });
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromModule)($env))($module);
    });
  });
});

const $$home$nw$stuff$unstable$src$Compile$umrToFileName = (($corePath) => {
  return (($umr) => {
    const $$name = $umr;
    const $name = ($$name)[2];
    const $source = ($$name)[1];
    return ((($source)[0] === "SourceDir")
      ? ((() => {
        const $d = ($source)[1];
        return ($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($d))((($core$Core$Cons)(($name + ".sp")))($core$Core$Nil)));
      }))()
      : ((($source)[0] === "Core")
        ? ($$home$nw$stuff$unstable$lib$posix$Path$resolve)(((sp_cons)(((sp_cons)(((text_split)("/"))(($name + ".sp"))))("core")))($corePath))
        : ((($source)[0] === "Posix")
          ? ($$home$nw$stuff$unstable$lib$posix$Path$resolve)(((sp_cons)(((sp_cons)(((text_split)("/"))(($name + ".sp"))))("posix")))($corePath))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 142:4', (sp_toHuman)($source)))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$circularIsError = (($globalDefsByName) => {
  return (($names) => {
    return (($core$List$any)((($name) => {
      const $$try1 = (($core$Dict$get)($name))($globalDefsByName);
      return ((($$try1)[0] === "Nothing")
        ? false
        : ((($$try1)[0] === "Just")
          ? ((() => {
            const $globalDef = ($$try1)[1];
            const $$try2 = $globalDef.expr;
            return ((($$try2)[0] === "Lambda")
              ? false
              : (true
                ? true
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 391:14', (sp_toHuman)($$try2))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 385:6', (sp_toHuman)($$try1))));
    })))($names);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateSource = (($src) => {
  return ((($src)[0] === "Core")
    ? "core"
    : ((($src)[0] === "Posix")
      ? "posix"
      : ((($src)[0] === "SourceDir")
        ? ((() => {
          const $path = ($src)[1];
          return (((sp_equal)(""))(((text_startsWithRegex)("[a-zA-Z0-9_./]*$"))($path))
            ? (sp_todo)(("Invalid chars in source dir name: " + $path))
            : ((($core$Text$replace)("/"))("$"))(((($core$Text$replace)("."))("_"))($path)));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 45:4', (sp_toHuman)($src)))));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$makeTextUsr = (($umr) => {
  return (($$name) => {
    const $name = ($$name)[1];
    const $$modulePath = $umr;
    const $modulePath = ($$modulePath)[2];
    const $source = ($$modulePath)[1];
    return ("$" + (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateSource)($source) + ("$" + (((($core$Text$replace)("/"))("$"))($modulePath) + $name))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$generatedName = (($base) => {
  return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$DollarName)(("$$" + $base));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$userSpecifiedName = (($name) => {
  return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$DollarName)(("$" + $name));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$pickMainName = (($pattern) => {
  return (((($pattern)[0] === "PatternAny") && ((($pattern)[2])[0] === "Just"))
    ? ((() => {
      const $name = (($pattern)[2])[1];
      return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$TrivialPattern)(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$userSpecifiedName)($name));
    }))()
    : (true
      ? ((() => {
        const $$try1 = ($core$Dict$keys)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNames)($pattern));
        return ((($$try1)[0] === "Cons")
          ? ((() => {
            const $head = ($$try1)[1];
            const $tail = ($$try1)[2];
            return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$SafeMainName)(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$generatedName)($head));
          }))()
          : ((($$try1)[0] === "Nil")
            ? $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$NoNamedVariables
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 94:12', (sp_toHuman)($$try1))));
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 89:4', (sp_toHuman)($pattern))));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$generateTryName = (($counter) => {
  (($counter.obj)[$counter.attr] += 1);
  return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$generatedName)(("try" + (text_fromNumber)((sp_clone)(($counter.obj)[$counter.attr]))));
});

const $core$List$indexedFor = (($aList) => {
  return (($function) => {
    return (($init) => {
      return ($core$Tuple$second)(((($core$List$for)($aList))((($item) => {
        return (($$accum) => {
          const $index = $$accum.first;
          const $accum = $$accum.second;
          return ({
            first: ($index + 1),
            second: ((($function)($index))($item))($accum),
          });
        });
      })))(({
        first: 0,
        second: $init,
      })));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$testPattern = (($pattern) => {
  return (($valueToTest) => {
    return (($accum) => {
      return ((($pattern)[0] === "PatternAny")
        ? $accum
        : ((($pattern)[0] === "PatternLiteralText")
          ? ((() => {
            const $text = ($pattern)[2];
            return ((sp_cons)($accum))((($$home$nw$stuff$unstable$src$Types$EmittableAst$ShallowEqual)(($$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralText)($text)))($valueToTest));
          }))()
          : ((($pattern)[0] === "PatternLiteralNumber")
            ? ((() => {
              const $num = ($pattern)[2];
              return ((sp_cons)($accum))((($$home$nw$stuff$unstable$src$Types$EmittableAst$ShallowEqual)(($$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralNumber)($num)))($valueToTest));
            }))()
            : (((($pattern)[0] === "PatternConstructor") && ((($pattern)[2])[0] === "USR"))
              ? ((() => {
                const $umr = (($pattern)[2])[1];
                const $name = (($pattern)[2])[2];
                const $pas = ($pattern)[3];
                return ((($core$List$indexedFor)($pas))((($index) => {
                  return (($argPattern) => {
                    return (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$testPattern)($argPattern))((($$home$nw$stuff$unstable$src$Types$EmittableAst$ConstructorAccess)($index))($valueToTest));
                  });
                })))(((sp_cons)($accum))((($$home$nw$stuff$unstable$src$Types$EmittableAst$IsConstructor)($name))($valueToTest)));
              }))()
              : ((($pattern)[0] === "PatternRecord")
                ? ((() => {
                  const $attrs = ($pattern)[2];
                  return ((($core$Dict$for)($attrs))((($name) => {
                    return (($pa) => {
                      return (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$testPattern)($pa))((($$home$nw$stuff$unstable$src$Types$EmittableAst$RecordAccess)($name))($valueToTest));
                    });
                  })))($accum);
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 156:4', (sp_toHuman)($pattern)))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePatternRec = (($pattern) => {
  return (($accessExpr) => {
    return (($accum) => {
      return (((($pattern)[0] === "PatternAny") && ((($pattern)[2])[0] === "Nothing"))
        ? $accum
        : (((($pattern)[0] === "PatternAny") && ((($pattern)[2])[0] === "Just"))
          ? ((() => {
            const $name = (($pattern)[2])[1];
            return ((sp_cons)($accum))(({
              first: ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$userSpecifiedName)($name),
              second: $accessExpr,
            }));
          }))()
          : ((($pattern)[0] === "PatternLiteralNumber")
            ? $accum
            : ((($pattern)[0] === "PatternLiteralText")
              ? $accum
              : ((($pattern)[0] === "PatternConstructor")
                ? ((() => {
                  const $path = ($pattern)[2];
                  const $pas = ($pattern)[3];
                  return ((($core$List$indexedFor)($pas))((($index) => {
                    return (($pa) => {
                      return (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePatternRec)($pa))((($$home$nw$stuff$unstable$src$Types$EmittableAst$ConstructorAccess)($index))($accessExpr));
                    });
                  })))($accum);
                }))()
                : ((($pattern)[0] === "PatternRecord")
                  ? ((() => {
                    const $attrs = ($pattern)[2];
                    return ((($core$Dict$for)($attrs))((($name) => {
                      return (($pa) => {
                        return (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePatternRec)($pa))((($$home$nw$stuff$unstable$src$Types$EmittableAst$RecordAccess)($name))($accessExpr));
                      });
                    })))($accum);
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 114:4', (sp_toHuman)($pattern))))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePattern = (($pattern) => {
  return (($accessExpr) => {
    return ((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePatternRec)($pattern))($accessExpr))($core$Core$Nil);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr = (($usr) => {
  const $$name = $usr;
  const $name = ($$name)[2];
  const $umr = ($$name)[1];
  return (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$makeTextUsr)($umr))(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$userSpecifiedName)($name));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateVariableArgs = (($$attrPath) => {
  const $attrPath = $$attrPath.attrPath;
  const $ref = $$attrPath.ref;
  const $variableName = ((($ref)[0] === "RefBlock")
    ? ((() => {
      const $name = ($ref)[1];
      const $$n = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$userSpecifiedName)($name);
      const $n = ($$n)[1];
      return $n;
    }))()
    : ((($ref)[0] === "RefRoot")
      ? ((() => {
        const $usr = ($ref)[1];
        return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr)($usr);
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 140:8', (sp_toHuman)($ref))));
  return (($$home$nw$stuff$unstable$src$Types$EmittableAst$Variable)($variableName))($attrPath);
});

const $core$Tuple$mapSecond = (($f) => {
  return (($t) => {
    return ({
      first: $t.first,
      second: ($f)($t.second),
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression = (($counter) => {
  return (($expression) => {
    return ((($expression)[0] === "LiteralNumber")
      ? ((() => {
        const $num = ($expression)[2];
        return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralNumber)($num);
      }))()
      : ((($expression)[0] === "LiteralText")
        ? ((() => {
          const $text = ($expression)[2];
          return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralText)($text);
        }))()
        : ((($expression)[0] === "Variable")
          ? ((() => {
            const $var = ($expression)[2];
            return ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateVariableArgs)($var);
          }))()
          : ((($expression)[0] === "Constructor")
            ? ((() => {
              const $usr = ($expression)[2];
              return ($$home$nw$stuff$unstable$src$Types$EmittableAst$Constructor)(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr)($usr));
            }))()
            : (((($expression)[0] === "Lambda") && ((($expression)[2])[0] === "ParameterMutable"))
              ? ((() => {
                const $pos = ($expression)[1];
                const $name = (($expression)[2])[2];
                const $body = ($expression)[3];
                const $$n = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$userSpecifiedName)($name);
                const $n = ($$n)[1];
                return (($$home$nw$stuff$unstable$src$Types$EmittableAst$Lambda)(({
                  first: ($core$Maybe$Just)($n),
                  second: $$home$nw$stuff$unstable$src$Types$EmittableAst$Mutable,
                })))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($body));
              }))()
              : (((($expression)[0] === "Lambda") && ((($expression)[2])[0] === "ParameterPattern"))
                ? ((() => {
                  const $pos = ($expression)[1];
                  const $pattern = (($expression)[2])[1];
                  const $body = ($expression)[3];
                  const $$try2 = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$pickMainName)($pattern);
                  return ((($$try2)[0] === "NoNamedVariables")
                    ? (($$home$nw$stuff$unstable$src$Types$EmittableAst$Lambda)(({
                      first: $core$Maybe$Nothing,
                      second: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                    })))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($body))
                    : (((($$try2)[0] === "TrivialPattern") && ((($$try2)[1])[0] === "DollarName"))
                      ? ((() => {
                        const $argName = (($$try2)[1])[1];
                        return (($$home$nw$stuff$unstable$src$Types$EmittableAst$Lambda)(({
                          first: ($core$Maybe$Just)($argName),
                          second: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                        })))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($body));
                      }))()
                      : (((($$try2)[0] === "SafeMainName") && ((($$try2)[1])[0] === "DollarName"))
                        ? ((() => {
                          const $mainName = (($$try2)[1])[1];
                          const $namesAndExpressions = (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePattern)($pattern))((($$home$nw$stuff$unstable$src$Types$EmittableAst$Variable)($mainName))($core$Core$Nil));
                          const $wrapWithArgumentLetIn = (($$letExpression) => {
                            const $varName = ($$letExpression.first)[1];
                            const $letExpression = $$letExpression.second;
                            return (($inExpression) => {
                              return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                inExpression: $inExpression,
                                letExpression: $letExpression,
                                maybeName: ($core$Maybe$Just)($varName),
                                mutability: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                              }));
                            });
                          });
                          return (($$home$nw$stuff$unstable$src$Types$EmittableAst$Lambda)(({
                            first: ($core$Maybe$Just)($mainName),
                            second: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                          })))(((($core$List$for)($namesAndExpressions))($wrapWithArgumentLetIn))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($body)));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 197:12', (sp_toHuman)($$try2)))));
                }))()
                : ((($expression)[0] === "Record")
                  ? ((() => {
                    const $extends = ($expression)[2];
                    const $attrs = ($expression)[3];
                    return (($$home$nw$stuff$unstable$src$Types$EmittableAst$LiteralRecord)((($core$Maybe$map)($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateVariableArgs))($extends)))((($core$List$map)(($core$Tuple$mapSecond)(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))))(((list_sortBy)($core$Tuple$first))(($core$Dict$toList)($attrs))));
                  }))()
                  : (((($expression)[0] === "Call") && ((($expression)[3])[0] === "ArgumentMutable"))
                    ? ((() => {
                      const $ref = ($expression)[2];
                      const $var = (($expression)[3])[2];
                      return (($$home$nw$stuff$unstable$src$Types$EmittableAst$Call)((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($ref)))(({
                        first: ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateVariableArgs)($var),
                        second: $$home$nw$stuff$unstable$src$Types$EmittableAst$Mutable,
                      }));
                    }))()
                    : (((($expression)[0] === "Call") && ((($expression)[3])[0] === "ArgumentExpression"))
                      ? ((() => {
                        const $ref = ($expression)[2];
                        const $expr = (($expression)[3])[1];
                        return (($$home$nw$stuff$unstable$src$Types$EmittableAst$Call)((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($ref)))(({
                          first: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($expr),
                          second: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                        }));
                      }))()
                      : ((($expression)[0] === "If")
                        ? ((() => {
                          const $ar = ($expression)[2];
                          return ((($$home$nw$stuff$unstable$src$Types$EmittableAst$Conditional)((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($ar.condition)))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($ar.true)))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($ar.false));
                        }))()
                        : ((($expression)[0] === "Try")
                          ? ((() => {
                            const $pos = ($expression)[1];
                            const $value = ($expression)[2];
                            const $tries = ($expression)[3];
                            const $$valueExpression = (((($value)[0] === "Variable") && ((($value)[2].attrPath)[0] === "Nil"))
                              ? ((() => {
                                const $ref = ($value)[2].ref;
                                return ({
                                  first: ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateVariableArgs)(({
                                    attrPath: $core$Core$Nil,
                                    ref: $ref,
                                  })),
                                  second: $core$Basics$identity,
                                });
                              }))()
                              : (true
                                ? ((() => {
                                  const $$tryName = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$generateTryName)($counter);
                                  const $tryName = ($$tryName)[1];
                                  const $wrap = (($tryExpression) => {
                                    return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                      inExpression: $tryExpression,
                                      letExpression: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($value),
                                      maybeName: ($core$Maybe$Just)($tryName),
                                      mutability: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                                    }));
                                  });
                                  return ({
                                    first: (($$home$nw$stuff$unstable$src$Types$EmittableAst$Variable)($tryName))($core$Core$Nil),
                                    second: $wrap,
                                  });
                                }))()
                                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 245:16', (sp_toHuman)($value))));
                            const $wrapWithLetIn = $$valueExpression.second;
                            const $valueExpression = $$valueExpression.first;
                            const $addTryPatternAndBlock = (($$block) => {
                              const $pattern = $$block.first;
                              const $block = $$block.second;
                              return (($nextTryExpression) => {
                                const $testIfPatternMatches = ($$home$nw$stuff$unstable$src$Types$EmittableAst$And)(($core$List$reverse)(((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$testPattern)($pattern))($valueExpression))($core$Core$Nil)));
                                const $namesAndExpressions = (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePattern)($pattern))($valueExpression);
                                const $whenConditionMatches = ((($core$List$for)($namesAndExpressions))((($$letExpression) => {
                                  const $name = ($$letExpression.first)[1];
                                  const $letExpression = $$letExpression.second;
                                  return (($inExpression) => {
                                    return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                      inExpression: $inExpression,
                                      letExpression: $letExpression,
                                      maybeName: ($core$Maybe$Just)($name),
                                      mutability: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                                    }));
                                  });
                                })))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($block));
                                return ((($$home$nw$stuff$unstable$src$Types$EmittableAst$Conditional)($testIfPatternMatches))($whenConditionMatches))($nextTryExpression);
                              });
                            });
                            const $default = (($$home$nw$stuff$unstable$src$Types$EmittableAst$MissingPattern)($pos))($valueExpression);
                            return ($wrapWithLetIn)(((($core$List$forReversed)($tries))($addTryPatternAndBlock))($default));
                          }))()
                          : ((($expression)[0] === "LetIn")
                            ? ((() => {
                              const $valueDef = ($expression)[1];
                              const $e = ($expression)[2];
                              const $$try1 = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$pickMainName)($valueDef.pattern);
                              return ((($$try1)[0] === "NoNamedVariables")
                                ? ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                  inExpression: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($e),
                                  letExpression: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($valueDef.body),
                                  maybeName: $core$Maybe$Nothing,
                                  mutability: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                                }))
                                : (((($$try1)[0] === "TrivialPattern") && ((($$try1)[1])[0] === "DollarName"))
                                  ? ((() => {
                                    const $defName = (($$try1)[1])[1];
                                    return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                      inExpression: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($e),
                                      letExpression: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($valueDef.body),
                                      maybeName: ($core$Maybe$Just)($defName),
                                      mutability: ($valueDef.mutable
                                        ? $$home$nw$stuff$unstable$src$Types$EmittableAst$Mutable
                                        : $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable),
                                    }));
                                  }))()
                                  : (((($$try1)[0] === "SafeMainName") && ((($$try1)[1])[0] === "DollarName"))
                                    ? ((() => {
                                      const $mainName = (($$try1)[1])[1];
                                      const $namesAndExpressions = (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePattern)($valueDef.pattern))((($$home$nw$stuff$unstable$src$Types$EmittableAst$Variable)($mainName))($core$Core$Nil));
                                      const $wrapWithUnpackedPatternVar = (($$letExpression) => {
                                        const $name = ($$letExpression.first)[1];
                                        const $letExpression = $$letExpression.second;
                                        return (($inExpression) => {
                                          return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                            inExpression: $inExpression,
                                            letExpression: $letExpression,
                                            maybeName: ($core$Maybe$Just)($name),
                                            mutability: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                                          }));
                                        });
                                      });
                                      const $wrapWithActualLetIn = (($inExpression) => {
                                        return ($$home$nw$stuff$unstable$src$Types$EmittableAst$LetIn)(({
                                          inExpression: $inExpression,
                                          letExpression: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($valueDef.body),
                                          maybeName: ($core$Maybe$Just)($mainName),
                                          mutability: $$home$nw$stuff$unstable$src$Types$EmittableAst$Immutable,
                                        }));
                                      });
                                      return ($wrapWithActualLetIn)(((($core$List$forReversed)($namesAndExpressions))($wrapWithUnpackedPatternVar))((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($e)));
                                    }))()
                                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 292:12', (sp_toHuman)($$try1)))));
                            }))()
                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 179:4', (sp_toHuman)($expression))))))))))))));
  });
});

const $core$Set$map = (($f) => {
  return (($set) => {
    return ((($core$Dict$for)($set))((($k) => {
      return (() => {
        return (($core$Dict$insert)(($f)($k)))(null);
      });
    })))($core$Dict$empty);
  });
});

const $core$Set$singleton = (($a) => {
  return (($core$Dict$singleton)($a))(null);
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateRootValueDef = (($umr) => {
  return (($def) => {
    return (($accum) => {
      const $counter = ({
        attr: "$",
        obj: ({
          $: (sp_clone)(0),
        }),
      });
      const $deps = (($core$Set$map)($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr))($def.directValueDeps);
      const $$try1 = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$pickMainName)($def.pattern);
      return ((($$try1)[0] === "NoNamedVariables")
        ? $accum
        : ((($$try1)[0] === "TrivialPattern")
          ? ((() => {
            const $name = ($$try1)[1];
            const $usrAsText = (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$makeTextUsr)($umr))($name);
            return ((($core$Dict$insert)($usrAsText))(({
              deps: $deps,
              expr: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($def.body),
              name: $usrAsText,
            })))($accum);
          }))()
          : ((($$try1)[0] === "SafeMainName")
            ? ((() => {
              const $mainName = ($$try1)[1];
              const $mainUsrAsText = (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$makeTextUsr)($umr))($mainName);
              const $mainDef = ({
                deps: $deps,
                expr: (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateExpression)($counter))($def.body),
                name: $mainUsrAsText,
              });
              return ((($core$List$for)((($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translatePattern)($def.pattern))((($$home$nw$stuff$unstable$src$Types$EmittableAst$Variable)($mainUsrAsText))($core$Core$Nil))))((($$expr) => {
                const $name = $$expr.first;
                const $expr = $$expr.second;
                const $textUsr = (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$makeTextUsr)($umr))($name);
                return (($core$Dict$insert)($textUsr))(({
                  deps: ($core$Set$singleton)($mainUsrAsText),
                  expr: $expr,
                  name: $textUsr,
                }));
              })))(((($core$Dict$insert)($mainUsrAsText))($mainDef))($accum));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeEmittable.sp 344:4', (sp_toHuman)($$try1)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Prelude$compare = ({
  nonFn: (($core$Core$Cons)("a"))($core$Core$Nil),
  type: ((($$home$nw$stuff$unstable$src$Prelude$tyFun)(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(false))(((($$home$nw$stuff$unstable$src$Prelude$tyFun)(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number)),
  usr: ($$home$nw$stuff$unstable$src$Prelude$coreUsr)("compare"),
});

const $$home$nw$stuff$unstable$src$Prelude$debugUsr = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))("Debug"));

const $$home$nw$stuff$unstable$src$Prelude$debugBenchStart = ({
  nonFn: $core$Core$Nil,
  type: ((($$home$nw$stuff$unstable$src$Prelude$tyFun)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none),
  usr: ($$home$nw$stuff$unstable$src$Prelude$debugUsr)("benchStart"),
});

const $$home$nw$stuff$unstable$src$Prelude$debugBenchStop = ({
  nonFn: $core$Core$Nil,
  type: ((($$home$nw$stuff$unstable$src$Prelude$tyFun)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none),
  usr: ($$home$nw$stuff$unstable$src$Prelude$debugUsr)("benchStop"),
});

const $$home$nw$stuff$unstable$src$Prelude$debugLog = ({
  nonFn: $core$Core$Nil,
  type: ((($$home$nw$stuff$unstable$src$Prelude$tyFun)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text))(false))(((($$home$nw$stuff$unstable$src$Prelude$tyFun)(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a"))),
  usr: ($$home$nw$stuff$unstable$src$Prelude$debugUsr)("log"),
});

const $$home$nw$stuff$unstable$src$Prelude$debugToHuman = ({
  nonFn: $core$Core$Nil,
  type: ((($$home$nw$stuff$unstable$src$Prelude$tyFun)(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text),
  usr: ($$home$nw$stuff$unstable$src$Prelude$debugUsr)("toHuman"),
});

const $$home$nw$stuff$unstable$src$Prelude$debugTodo = ({
  nonFn: $core$Core$Nil,
  type: ((($$home$nw$stuff$unstable$src$Prelude$tyFun)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$text))(false))(($$home$nw$stuff$unstable$src$Prelude$tyVar)("a")),
  usr: ($$home$nw$stuff$unstable$src$Prelude$debugUsr)("todo"),
});

const $$home$nw$stuff$unstable$src$Prelude$functions = (($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$compare))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$debugTodo))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$debugLog))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$debugToHuman))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$debugBenchStart))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$debugBenchStop))($core$Core$Nil))))));

const $core$Dict$update = (($targetKey) => {
  return (($alter) => {
    return (($dictionary) => {
      const $$try1 = ($alter)((($core$Dict$get)($targetKey))($dictionary));
      return ((($$try1)[0] === "Just")
        ? ((() => {
          const $value = ($$try1)[1];
          return ((($core$Dict$insert)($targetKey))($value))($dictionary);
        }))()
        : ((($$try1)[0] === "Nothing")
          ? (($core$Dict$remove)($targetKey))($dictionary)
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 343:2', (sp_toHuman)($$try1))));
    });
  });
});

const $core$Set$fromList = (($list) => {
  return ((($core$List$for)($list))($core$Set$insert))($core$Set$empty);
});

const $$home$nw$stuff$unstable$src$Prelude$insertInModule = (($usr) => {
  return (($type) => {
    return (($nonFn) => {
      const $$name = $usr;
      const $name = ($$name)[2];
      const $umr = ($$name)[1];
      const $def = ({
        body: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralText)($$home$nw$stuff$unstable$src$Types$Pos$N))($name),
        directConsDeps: $core$Dict$empty,
        directTypeDeps: $core$Dict$empty,
        directValueDeps: $core$Dict$empty,
        mutable: false,
        native: true,
        nonFn: ($core$Set$fromList)($nonFn),
        parentDefinitions: $core$Core$Nil,
        pattern: ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternAny)($$home$nw$stuff$unstable$src$Types$Pos$N))(($core$Maybe$Just)($name)))(($core$Maybe$Just)($type)),
      });
      const $update = (($maybeModule) => {
        return ((($module) => {
          return ($core$Maybe$Just)((Object.assign)({}, $module, ({
            valueDefs: ((($core$Dict$insert)($def.pattern))($def))($module.valueDefs),
          })));
        }))((($core$Maybe$withDefault)(({
          aliasDefs: $core$Dict$empty,
          asText: "",
          umr: $umr,
          unionDefs: $core$Dict$empty,
          valueDefs: $core$Dict$empty,
        })))($maybeModule));
      });
      return (($core$Dict$update)($umr))($update);
    });
  });
});

const $$home$nw$stuff$unstable$src$Prelude$insertBinop = (($binop) => {
  return ((($$home$nw$stuff$unstable$src$Prelude$insertInModule)($binop.usr))($binop.type))($binop.nonFn);
});

const $$home$nw$stuff$unstable$src$Prelude$insertFunction = (($function) => {
  return ((($$home$nw$stuff$unstable$src$Prelude$insertInModule)($function.usr))($function.type))($function.nonFn);
});

const $$home$nw$stuff$unstable$src$Prelude$insertUnop = (($unop) => {
  return ((($$home$nw$stuff$unstable$src$Prelude$insertInModule)($unop.usr))($unop.type))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Prelude$coreModulesByUmr = ((($core$List$for)($$home$nw$stuff$unstable$src$Prelude$functions))($$home$nw$stuff$unstable$src$Prelude$insertFunction))(((($core$List$for)($$home$nw$stuff$unstable$src$Prelude$binops))($$home$nw$stuff$unstable$src$Prelude$insertBinop))((($$home$nw$stuff$unstable$src$Prelude$insertUnop)($$home$nw$stuff$unstable$src$Prelude$unaryMinus))((($$home$nw$stuff$unstable$src$Prelude$insertUnop)($$home$nw$stuff$unstable$src$Prelude$unaryPlus))($core$Dict$empty))));

const $$home$nw$stuff$unstable$src$Prelude$coreModules = ($core$Dict$values)($$home$nw$stuff$unstable$src$Prelude$coreModulesByUmr);

const $core$List$member = (($a) => {
  return (($list) => {
    return ((($list)[0] === "Nil")
      ? false
      : ((($list)[0] === "Cons")
        ? ((() => {
          const $h = ($list)[1];
          const $t = ($list)[2];
          return (((sp_equal)($h))($a)
            ? true
            : (($core$List$member)($a))($t));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 35:4', (sp_toHuman)($list))));
  });
});

const $core$List$takeWhile = (($test) => {
  const $rec = (($accum) => {
    return (($list) => {
      return ((($list)[0] === "Nil")
        ? ($core$List$reverse)($accum)
        : ((($list)[0] === "Cons")
          ? ((() => {
            const $head = ($list)[1];
            const $tail = ($list)[2];
            return (($test)($head)
              ? (($rec)(((sp_cons)($accum))($head)))($tail)
              : ($core$List$reverse)($accum));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 286:6', (sp_toHuman)($list))));
    });
  });
  return ($rec)($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$RefHierarchy$resolve = (($getEdges) => {
  return (($target) => {
    return (($path) => {
      return (($state0) => {
        return ((($core$List$member)($target))($state0.resolved)
          ? $state0
          : ((($core$List$member)($target))($path)
            ? ((() => {
              const $circ = ((sp_cons)((($core$List$takeWhile)((($key) => {
                return ((sp_not_equal)($target))($key);
              })))($path)))($target);
              return (Object.assign)({}, $state0, ({
                circular: ((($core$Dict$insert)(($core$Set$fromList)($circ)))($circ))($state0.circular),
              }));
            }))()
            : ((() => {
              const $s = ((($core$Dict$for)(($getEdges)($target)))((($a) => {
                return (() => {
                  return ((($$home$nw$stuff$unstable$src$RefHierarchy$resolve)($getEdges))($a))(((sp_cons)($path))($target));
                });
              })))($state0);
              return (Object.assign)({}, $s, ({
                resolved: ((sp_cons)($s.resolved))($target),
              }));
            }))()));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$RefHierarchy$reorder = (($nodeToEdges) => {
  return (($nodesById) => {
    const $keyToEdges = (($id) => {
      const $$try1 = (($core$Dict$get)($id))($nodesById);
      return ((($$try1)[0] === "Nothing")
        ? $core$Set$empty
        : ((($$try1)[0] === "Just")
          ? ((() => {
            const $node = ($$try1)[1];
            return ($nodeToEdges)($node);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/RefHierarchy.sp 44:8', (sp_toHuman)($$try1))));
    });
    const $state0 = ({
      circular: $core$Dict$empty,
      resolved: $core$Core$Nil,
    });
    const $stateF = ((($core$Dict$for)($nodesById))((($k) => {
      return (($v) => {
        return ((($$home$nw$stuff$unstable$src$RefHierarchy$resolve)($keyToEdges))($k))($core$Core$Nil);
      });
    })))($state0);
    return ({
      first: ($core$Dict$values)($stateF.circular),
      second: ($core$List$reverse)($stateF.resolved),
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateAll = (($userModules) => {
  (sp_benchStart)(null);
  const $modules = ($core$List$concat)((($core$Core$Cons)($userModules))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Prelude$coreModules))($core$Core$Nil)));
  const $globalDefsByName = ((($core$List$for)($modules))((($module) => {
    return (($core$Dict$for)($module.valueDefs))((() => {
      return (($def) => {
        return (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateRootValueDef)($module.umr))($def);
      });
    }));
  })))($core$Dict$empty);
  const $$circulars = (($$home$nw$stuff$unstable$src$RefHierarchy$reorder)((($globalDef) => {
    return $globalDef.deps;
  })))($globalDefsByName);
  const $reorderedNames = $$circulars.second;
  const $circulars = $$circulars.first;
  (sp_benchStop)("makeEmittable");
  const $errors = (($core$List$filter)(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$circularIsError)($globalDefsByName)))($circulars);
  return (((sp_not_equal)($core$Core$Nil))($errors)
    ? ($core$Result$Err)($errors)
    : ($core$Result$Ok)((($core$List$filterMap)((($name) => {
      return (($core$Dict$get)($name))($globalDefsByName);
    })))($reorderedNames)));
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndInsertAlias = (($allTypes) => {
  return (($al) => {
    return (($expandedTypes) => {
      const $getAlias = (($pos) => {
        return (($usr) => {
          const $$try1 = (($core$Dict$get)($usr))($expandedTypes);
          return ((($$try1)[0] === "Just")
            ? ((() => {
              const $type = ($$try1)[1];
              return ($core$Result$Ok)($type);
            }))()
            : ((($$try1)[0] === "Nothing")
              ? ((() => {
                const $$try2 = (($core$Dict$get)($usr))($allTypes);
                return ((($$try2)[0] === "Nothing")
                  ? (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)($pos))((($core$Core$Cons)(("Undefined type: `" + ((sp_toHuman)($usr) + "`"))))($core$Core$Nil))
                  : (((($$try2)[0] === "Just") && ((($$try2)[1])[0] === "TypeDefAlias"))
                    ? ((() => {
                      const $a = (($$try2)[1])[1];
                      return (sp_todo)(("expandAndInsertAlias should-not-happen: " + (sp_toHuman)($usr)));
                    }))()
                    : (((($$try2)[0] === "Just") && ((($$try2)[1])[0] === "TypeDefUnion"))
                      ? ((() => {
                        const $u = (($$try2)[1])[1];
                        return ($core$Result$Ok)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefUnion)($u));
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 166:16', (sp_toHuman)($$try2)))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 161:8', (sp_toHuman)($$try1))));
        });
      });
      return (($core$Result$onOk)((($type) => {
        return ($core$Result$Ok)(((($core$Dict$insert)($al.usr))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefAlias)((Object.assign)({}, $al, ({
          type: $type,
        })))))($expandedTypes));
      })))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndValidateType)($getAlias))($al.type));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$getTypeForUnion = (($allTypes) => {
  return (($expandedTypes) => {
    return (($pos) => {
      return (($usr) => {
        const $$try1 = (($core$Dict$get)($usr))($expandedTypes);
        return ((($$try1)[0] === "Just")
          ? ((() => {
            const $t = ($$try1)[1];
            return ($core$Result$Ok)($t);
          }))()
          : ((($$try1)[0] === "Nothing")
            ? ((() => {
              const $$try2 = (($core$Dict$get)($usr))($allTypes);
              return ((($$try2)[0] === "Just")
                ? ((() => {
                  const $t = ($$try2)[1];
                  return ($core$Result$Ok)($t);
                }))()
                : ((($$try2)[0] === "Nothing")
                  ? (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$error)($pos))((($core$Core$Cons)(("Undefined type usr: `" + ((sp_toHuman)($usr) + "`"))))($core$Core$Nil))
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 230:12', (sp_toHuman)($$try2))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 223:4', (sp_toHuman)($$try1))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndInsertUnion = (($allTypes) => {
  return (($usr) => {
    return (($typeDef) => {
      return (($expandedTypes) => {
        return ((($typeDef)[0] === "TypeDefAlias")
          ? ($core$Result$Ok)($expandedTypes)
          : ((($typeDef)[0] === "TypeDefUnion")
            ? ((() => {
              const $u = ($typeDef)[1];
              const $gt = (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$getTypeForUnion)($allTypes))($expandedTypes);
              const $mapConstructor = (($name) => {
                return (($c) => {
                  return (($core$Result$onOk)((($type) => {
                    return (($core$Result$onOk)((($args) => {
                      return ($core$Result$Ok)((Object.assign)({}, $c, ({
                        args: $args,
                        type: $type,
                      })));
                    })))((($core$List$mapRes)(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndValidateType)($gt)))($c.args));
                  })))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndValidateType)($gt))($c.type));
                });
              });
              return (($core$Result$onOk)((($cs) => {
                return ($core$Result$Ok)(((($core$Dict$insert)($usr))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefUnion)((Object.assign)({}, $u, ({
                  constructors: $cs,
                })))))($expandedTypes));
              })))((($core$Dict$mapRes)($mapConstructor))($u.constructors));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 242:4', (sp_toHuman)($typeDef))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases = (($allAliases) => {
  return (($ty) => {
    return ((($ty)[0] === "TypeConstant")
      ? ((() => {
        const $pos = ($ty)[1];
        const $usr = ($ty)[2];
        const $args = ($ty)[3];
        const $init = ((($core$Dict$member)($usr))($allAliases)
          ? ($core$Set$singleton)($usr)
          : $core$Set$empty);
        return ((($core$List$for)($args))((($ar) => {
          return ($core$Dict$join)((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases)($allAliases))($ar));
        })))(($core$Set$singleton)($usr));
      }))()
      : ((($ty)[0] === "TypeVariable")
        ? ((() => {
          const $pos = ($ty)[1];
          const $name = ($ty)[2];
          return $core$Dict$empty;
        }))()
        : ((($ty)[0] === "TypeFunction")
          ? ((() => {
            const $pos = ($ty)[1];
            const $from = ($ty)[2];
            const $maybeMut = ($ty)[3];
            const $to = ($ty)[4];
            return (($core$Dict$join)((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases)($allAliases))($from)))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases)($allAliases))($to));
          }))()
          : ((($ty)[0] === "TypeRecord")
            ? ((() => {
              const $pos = ($ty)[1];
              const $extensible = ($ty)[2];
              const $attrs = ($ty)[3];
              return ((($core$Dict$for)($attrs))((($name) => {
                return (($t) => {
                  return ($core$Dict$join)((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases)($allAliases))($t));
                });
              })))($core$Dict$empty);
            }))()
            : ((($ty)[0] === "TypeAlias")
              ? ((() => {
                const $pos = ($ty)[1];
                const $path = ($ty)[2];
                const $t = ($ty)[3];
                return (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases)($allAliases))($t);
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 131:4', (sp_toHuman)($ty)))))));
  });
});

const $core$Dict$forRes = (($dict) => {
  return (($func) => {
    return (($acc) => {
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
                return ((($core$Dict$forRes)($right))($func))($f);
              })))(((($func)($key))($value))($l));
            })))(((($core$Dict$forRes)($left))($func))($acc));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 464:4', (sp_toHuman)($dict))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAllTypes = (($allTypes) => {
  const $allAliases = ((() => {
    const $insertAlias = (($usr) => {
      return (($typeDef) => {
        return (($acc) => {
          return ((($typeDef)[0] === "TypeDefAlias")
            ? ((() => {
              const $a = ($typeDef)[1];
              return ((($core$Dict$insert)($usr))($a))($acc);
            }))()
            : (true
              ? $acc
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/ExpandTypes.sp 280:12', (sp_toHuman)($typeDef))));
        });
      });
    });
    return ((($core$Dict$for)($allTypes))($insertAlias))($core$Dict$empty);
  }))();
  const $$circulars = (($$home$nw$stuff$unstable$src$RefHierarchy$reorder)((($al) => {
    return (($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$referencedAliases)($allAliases))($al.type);
  })))($allAliases);
  const $orderedAliasRefs = $$circulars.second;
  const $circulars = $$circulars.first;
  return (((sp_not_equal)($core$Core$Nil))($circulars)
    ? ((() => {
      const $circularToError = (($circular) => {
        return (($$home$nw$stuff$unstable$src$Compiler$Error$Simple)(($$home$nw$stuff$unstable$src$Types$Pos$I)(121)))((() => {
          return (($core$Core$Cons)("circular alias: "))((($core$Core$Cons)((($core$Text$join)(" <- "))((($core$List$map)(sp_toHuman))($circular))))($core$Core$Nil));
        }));
      });
      return ($core$Result$Err)(($$home$nw$stuff$unstable$src$Compiler$Error$Nested)((($core$List$map)($circularToError))($circulars)));
    }))()
    : ((() => {
      const $oa = (($core$List$filterMap)((($ref) => {
        return (($core$Dict$get)($ref))($allAliases);
      })))($orderedAliasRefs);
      return (($core$Result$onOk)((($core$Dict$forRes)($allTypes))(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndInsertUnion)($allTypes))))(((($core$List$forRes)($oa))(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAndInsertAlias)($allTypes)))($core$Dict$empty));
    }))());
});

const $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$insertModuleTypes = (($module) => {
  return (($allTypes) => {
    return ((($core$Dict$for)($module.unionDefs))((($name) => {
      return (($def) => {
        return (($core$Dict$insert)($def.usr))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefUnion)($def));
      });
    })))(((($core$Dict$for)($module.aliasDefs))((($name) => {
      return (($def) => {
        return (($core$Dict$insert)($def.usr))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefAlias)($def));
      });
    })))($allTypes));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$boolDef = ((() => {
  const $usr = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("Bool");
  return ({
    args: $core$Core$Nil,
    constructors: ((($core$Dict$insert)("False"))(({
      args: $core$Core$Nil,
      pos: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$p,
      type: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool,
      typeUsr: $usr,
    })))(((($core$Dict$insert)("True"))(({
      args: $core$Core$Nil,
      pos: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$p,
      type: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool,
      typeUsr: $usr,
    })))($core$Dict$empty)),
    directTypeDeps: $core$Set$empty,
    usr: $usr,
  });
}))();

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$listDef = ((() => {
  const $usr = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("List");
  const $item = (($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$p))("item");
  const $consDef = ({
    args: (($core$Core$Cons)($item))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($item)))($core$Core$Nil)),
    pos: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$p,
    type: ((($core$List$forReversed)((($core$Core$Cons)($item))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($item)))($core$Core$Nil))))((($ar) => {
      return (($ty) => {
        return (((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$p))($ar))(false))($ty);
      });
    })))(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($item)),
    typeUsr: $usr,
  });
  return ({
    args: (($core$Core$Cons)("item"))($core$Core$Nil),
    constructors: ((($core$Dict$insert)("Cons"))($consDef))(((($core$Dict$insert)("Nil"))(({
      args: $core$Core$Nil,
      pos: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$p,
      type: ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($item),
      typeUsr: $usr,
    })))($core$Dict$empty)),
    directTypeDeps: $core$Set$empty,
    usr: $usr,
  });
}))();

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneDef = ((() => {
  const $usr = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneName);
  return ({
    args: $core$Core$Nil,
    constructors: (($core$Dict$singleton)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneName))(({
      args: $core$Core$Nil,
      pos: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$p,
      type: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$none,
      typeUsr: $usr,
    })),
    directTypeDeps: $core$Set$empty,
    usr: $usr,
  });
}))();

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$allDefs = (($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneDef))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$boolDef))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$listDef))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$textDef))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$numberDef))($core$Core$Nil)))));

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$insertUnionConstructors = (($typeDef) => {
  return (($constructors) => {
    return ((($typeDef)[0] === "TypeDefAlias")
      ? $constructors
      : ((($typeDef)[0] === "TypeDefUnion")
        ? ((() => {
          const $def = ($typeDef)[1];
          const $$umr = $def.usr;
          const $umr = ($$umr)[1];
          return ((($core$Dict$for)($def.constructors))((($name) => {
            return ($core$Dict$insert)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($umr))($name));
          })))($constructors);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Pipeline.sp 35:4', (sp_toHuman)($typeDef))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$coreConstructors = ((($core$List$for)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$allDefs))((($u) => {
  return ($$home$nw$stuff$unstable$src$Compiler$Pipeline$insertUnionConstructors)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefUnion)($u));
})))($core$Dict$empty);

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$coreTypes = ((($core$List$for)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$allDefs))((($def) => {
  return (($core$Dict$insert)($def.usr))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeDefUnion)($def));
})))($core$Dict$empty);

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$getFreeTypeVars = $$home$nw$stuff$unstable$src$Compiler$TypeCheck$getFreeTypeVars;

const $$home$nw$stuff$unstable$src$Types$Meta$spCorePath = "Core";

const $$home$nw$stuff$unstable$src$Types$Meta$spCoreUmr = (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))($$home$nw$stuff$unstable$src$Types$Meta$spCorePath);

const $$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR = ($$home$nw$stuff$unstable$src$Types$Meta$USR)($$home$nw$stuff$unstable$src$Types$Meta$spCoreUmr);

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$coreVariables = ((() => {
  const $insertUnop = (($unop) => {
    const $usr = ($$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR)($unop.symbol);
    const $iv = ({
      definedAt: $$home$nw$stuff$unstable$src$Types$Pos$N,
      freeTypeVariables: $core$Dict$empty,
      isMutable: false,
      ty: $unop.type,
    });
    return (($core$Dict$insert)($usr))($iv);
  });
  const $insertBinop = (($symbol) => {
    return (($binop) => {
      const $usr = ($$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR)($symbol);
      const $iv = ({
        definedAt: $$home$nw$stuff$unstable$src$Types$Pos$N,
        freeTypeVariables: ((($$home$nw$stuff$unstable$src$Compiler$Pipeline$getFreeTypeVars)($core$Dict$empty))(($core$Set$fromList)($binop.nonFn)))($binop.type),
        isMutable: false,
        ty: $binop.type,
      });
      return (($core$Dict$insert)($usr))($iv);
    });
  });
  const $insertCoreFunction = (($coreFn) => {
    const $iv = ({
      definedAt: $$home$nw$stuff$unstable$src$Types$Pos$N,
      freeTypeVariables: ((($$home$nw$stuff$unstable$src$Compiler$Pipeline$getFreeTypeVars)($core$Dict$empty))(($core$Set$fromList)($coreFn.nonFn)))($coreFn.type),
      isMutable: false,
      ty: $coreFn.type,
    });
    return (($core$Dict$insert)($coreFn.usr))($iv);
  });
  return ((($core$List$for)($$home$nw$stuff$unstable$src$Prelude$functions))($insertCoreFunction))(((($core$Dict$for)($$home$nw$stuff$unstable$src$Prelude$binopsBySymbol))($insertBinop))((($insertUnop)($$home$nw$stuff$unstable$src$Prelude$unaryMinus))((($insertUnop)($$home$nw$stuff$unstable$src$Prelude$unaryPlus))($core$Dict$empty))));
}))();

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$expandAndInsertModuleAnnotations = (($types) => {
  return (($module) => {
    const $insertName = (($def) => {
      return (($name) => {
        return (($$maybeType) => {
          const $pos = $$maybeType.first;
          const $maybeType = $$maybeType.second;
          return (($d) => {
            return ((($maybeType)[0] === "Nothing")
              ? ($core$Result$Ok)($d)
              : ((($maybeType)[0] === "Just")
                ? ((() => {
                  const $rawType = ($maybeType)[1];
                  return (($core$Result$onOk)((($type) => {
                    const $usr = (($$home$nw$stuff$unstable$src$Types$Meta$USR)($module.umr))($name);
                    const $iv = ({
                      definedAt: $pos,
                      freeTypeVariables: ((($$home$nw$stuff$unstable$src$Compiler$Pipeline$getFreeTypeVars)($core$Dict$empty))($def.nonFn))($type),
                      isMutable: false,
                      ty: $type,
                    });
                    return ($core$Result$Ok)(((($core$Dict$insert)($usr))($iv))($d));
                  })))((($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAnnotation)($types))($rawType));
                }))()
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Pipeline.sp 58:8', (sp_toHuman)($maybeType))));
          });
        });
      });
    });
    const $insertValueDef = (($def) => {
      return (($core$Dict$forRes)(($$home$nw$stuff$unstable$src$Types$CanonicalAst$patternNamedTypes)($def.pattern)))(($insertName)($def));
    });
    return (($core$Dict$forRes)($module.valueDefs))((() => {
      return $insertValueDef;
    }));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Pipeline$globalExpandedTypes = (($allModules) => {
  return (($core$Result$onOk)((($types) => {
    const $constructors = ((($core$Dict$for)($types))((() => {
      return $$home$nw$stuff$unstable$src$Compiler$Pipeline$insertUnionConstructors;
    })))($$home$nw$stuff$unstable$src$Compiler$Pipeline$coreConstructors);
    return (($core$Result$onOk)((($instanceVariables) => {
      return ($core$Result$Ok)(({
        constructors: $constructors,
        instanceVariables: $instanceVariables,
        types: $types,
      }));
    })))(((($core$Dict$forRes)($allModules))((() => {
      return ($$home$nw$stuff$unstable$src$Compiler$Pipeline$expandAndInsertModuleAnnotations)($types);
    })))($$home$nw$stuff$unstable$src$Compiler$Pipeline$coreVariables));
  })))(($$home$nw$stuff$unstable$src$Compiler$ExpandTypes$expandAllTypes)(((($core$Dict$for)($allModules))((() => {
    return $$home$nw$stuff$unstable$src$Compiler$ExpandTypes$insertModuleTypes;
  })))($$home$nw$stuff$unstable$src$Compiler$Pipeline$coreTypes)));
});

const $core$Result$mapError = (($f) => {
  return (($result) => {
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
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 26:4', (sp_toHuman)($result))));
  });
});

const $$home$nw$stuff$unstable$src$Compile$compileMain = (($pars) => {
  const $entryModulePath = ($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($pars.entryModulePath))($core$Core$Nil));
  const $entryModuleDir = ($$home$nw$stuff$unstable$lib$posix$Path$dirname)($entryModulePath);
  return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($maybeProjectRoot) => {
    const $projectRoot = (($core$Maybe$withDefault)($entryModuleDir))($maybeProjectRoot);
    return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($meta) => {
      const $maybeEntryUmr = (($core$List$find)((($umr) => {
        return ((sp_equal)($entryModulePath))((($$home$nw$stuff$unstable$src$Compile$umrToFileName)(""))($umr));
      })))(($core$Dict$values)($meta.moduleVisibleAsToUmr));
      const $entryUsr = ((($maybeEntryUmr)[0] === "Nothing")
        ? (sp_todo)(("Error: you are asking me to compile module " + ($entryModulePath + " but I can't find it anywhere.")))
        : ((($maybeEntryUmr)[0] === "Just")
          ? ((() => {
            const $umr = ($maybeEntryUmr)[1];
            return (($$home$nw$stuff$unstable$src$Types$Meta$USR)($umr))("main");
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 350:8', (sp_toHuman)($maybeEntryUmr))));
      return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($maybeCorelibParent) => {
        const $corePath = ((($maybeCorelibParent)[0] === "Nothing")
          ? (sp_todo)(("Error: I expect to find the " + ($$home$nw$stuff$unstable$src$Compile$libDirectoryName + (" directory next to the spcc executable " + ($pars.selfPath + " but I can't find it.")))))
          : ((($maybeCorelibParent)[0] === "Just")
            ? ((() => {
              const $p = ($maybeCorelibParent)[1];
              return ($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($p))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compile$libDirectoryName))($core$Core$Nil)));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 368:8', (sp_toHuman)($maybeCorelibParent))));
        const $outputFile = (($core$Maybe$withDefault)($pars.platform.defaultOutputPath))($pars.maybeOutputPath);
        ((sp_log)("Loading modules..."))("");
        const $loadAllModules = ($$home$nw$stuff$unstable$lib$posix$IO$parallel)((($core$List$map)((($umr) => {
          return ((($$home$nw$stuff$unstable$src$Compile$loadModule)($meta))($umr))((($$home$nw$stuff$unstable$src$Compile$umrToFileName)($corePath))($umr));
        })))(($core$Dict$values)($meta.moduleVisibleAsToUmr)));
        return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($userModules) => {
          const $modules = ((($core$List$for)($userModules))((($module) => {
            return (($core$Dict$update)($module.umr))((($maybeCore) => {
              return ((($maybeCore)[0] === "Nothing")
                ? ($core$Maybe$Just)($module)
                : ((($maybeCore)[0] === "Just")
                  ? ((() => {
                    const $core = ($maybeCore)[1];
                    return ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Compile$mergeWithCore)($core))($module));
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compile.sp 395:16', (sp_toHuman)($maybeCore))));
            }));
          })))($$home$nw$stuff$unstable$src$Prelude$coreModulesByUmr);
          const $eenv = ((() => {
            const $getName = (($n) => {
              const $$name = $n.umr;
              const $name = ($$name)[2];
              const $source = ($$name)[1];
              return $name;
            });
            return ({
              moduleByName: ((($core$List$for)(($core$Dict$values)($modules)))((($m) => {
                return (($core$Dict$insert)(($getName)($m)))(({
                  content: $m.asText,
                  fsPath: (($$home$nw$stuff$unstable$src$Compile$umrToFileName)($corePath))($m.umr),
                }));
              })))($core$Dict$empty),
            });
          }))();
          ((sp_log)("Solving globals..."))("");
          const $x = ($$home$nw$stuff$unstable$src$Compiler$Pipeline$globalExpandedTypes)($modules);
          return ((($$home$nw$stuff$unstable$src$Compile$onResSuccess)($eenv))((($globals) => {
            ((sp_log)("Type checking..."))("");
            const $typeCheckModules = ($$home$nw$stuff$unstable$lib$posix$IO$parallel)((($core$List$map)((($m) => {
              return (($$home$nw$stuff$unstable$src$Compile$resToIo)($eenv))(((($$home$nw$stuff$unstable$src$Compile$typeCheckModule)($meta))($globals))($m));
            })))(($core$Dict$values)($modules)));
            return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((($typeCheckEnvs) => {
              ((sp_log)("Emittable AST..."))("");
              return ((($$home$nw$stuff$unstable$src$Compile$onResSuccess)($eenv))((($emittableStatements) => {
                (sp_log)("= Platform specific stuff =");
                const $js = ((($pars.platform.compile)(({
                  constructors: ($core$Dict$toList)($globals.constructors),
                  errorEnv: $eenv,
                })))($entryUsr))($emittableStatements);
                return (($$home$nw$stuff$unstable$lib$posix$IO$onSuccess)((() => {
                  return ($$home$nw$stuff$unstable$lib$posix$IO$writeStdout)(("---> " + ($outputFile + " written. =)")));
                })))((($$home$nw$stuff$unstable$lib$posix$IO$writeFile)($outputFile))($js));
              })))((($core$Result$mapError)((($e) => {
                return (sp_todo)("MakeEmittable.translateAll returned Err");
              })))(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateAll)(($core$Dict$values)($modules))));
            })))($typeCheckModules);
          })))($x);
        })))($loadAllModules);
      })))((($$home$nw$stuff$unstable$src$Compile$searchAncestorDirectories)((($$fileName) => {
        const $isDirectory = $$fileName.first;
        const $fileName = $$fileName.second;
        return ($isDirectory && ((sp_equal)($$home$nw$stuff$unstable$src$Compile$libDirectoryName))($fileName));
      })))(($$home$nw$stuff$unstable$lib$posix$Path$dirname)(($$home$nw$stuff$unstable$lib$posix$Path$resolve)((($core$Core$Cons)($pars.selfPath))($core$Core$Nil)))));
    })))((((($$home$nw$stuff$unstable$src$Compile$loadMeta)($pars.env))($pars.platform))($entryModuleDir))($projectRoot));
  })))((($$home$nw$stuff$unstable$src$Compile$searchAncestorDirectories)((($$fileName) => {
    const $isDirectory = $$fileName.first;
    const $fileName = $$fileName.second;
    return (($core$Basics$not)($isDirectory) && ((sp_equal)($$home$nw$stuff$unstable$src$Compile$modulesFileName))($fileName));
  })))($entryModuleDir));
});

const $$home$nw$stuff$unstable$src$Compile$coreDirName = "core";

const $$home$nw$stuff$unstable$src$Compile$sdItemToUMR = (($source) => {
  return (($fileName) => {
    return (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($source))(((($core$Text$replace)(".sp"))(""))($fileName));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$false = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("False");

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$true = ($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("True");

const $$home$nw$stuff$unstable$src$Compiler$CoreTypes$usrToVariable = (($u) => {
  return (($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$p))(({
    attrPath: $core$Core$Nil,
    ref: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)($u),
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$Error$emph = ($$home$nw$stuff$unstable$src$Compiler$Error$formatWrap)("emphasys");

const $$home$nw$stuff$unstable$src$Compiler$Lexer$addOneIndentToken = (($kind) => {
  return (($state) => {
    const $pos = ($$home$nw$stuff$unstable$src$Compiler$Lexer$getPos)($state);
    return ((array_push)(({
      attr: "tokens",
      obj: ($state.obj)[$state.attr],
    })))(((($$home$nw$stuff$unstable$src$Types$Token$Token)($pos))($pos))($kind));
  });
});

const $$home$nw$stuff$unstable$src$Test$maybeToOutcome = (($m) => {
  return ((($m)[0] === "Just")
    ? ((() => {
      const $e = ($m)[1];
      return ($$home$nw$stuff$unstable$src$Test$Error)($e);
    }))()
    : ((($m)[0] === "Nothing")
      ? $$home$nw$stuff$unstable$src$Test$Success
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 15:4', (sp_toHuman)($m))));
});

const $$home$nw$stuff$unstable$src$Test$codeTest = (($toText) => {
  return (($title) => {
    return (($code) => {
      return (($functionToTest) => {
        return (($ce) => {
          const $$toMaybeError = $ce;
          const $toMaybeError = ($$toMaybeError)[1];
          return ((($$home$nw$stuff$unstable$src$Test$Single)($title))($code))((() => {
            return ($$home$nw$stuff$unstable$src$Test$maybeToOutcome)((($toMaybeError)($toText))(($functionToTest)($code)));
          }));
        });
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest = ($$home$nw$stuff$unstable$src$Test$codeTest)(sp_toHuman);

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleName = "(test)";

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$dummyErrorEnv = (($code) => {
  return ({
    moduleByName: (($core$Dict$singleton)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleName))(({
      content: $code,
      fsPath: "<TestPath>",
    })),
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$formattedToStrippedText = (($formatted) => {
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
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TestHelpers.sp 62:8', (sp_toHuman)($fmt))))));
  });
  return (($core$Text$join)(""))((($core$List$map)($strip))($formatted));
});

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$resErrorToStrippedText = (($code) => {
  return ($core$Result$mapError)((($e) => {
    return ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$formattedToStrippedText)((($$home$nw$stuff$unstable$src$Compiler$Error$toFormattedText)(($$home$nw$stuff$unstable$src$Compiler$TestHelpers$dummyErrorEnv)($code)))($e));
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens = (($s) => {
  return (($$home$nw$stuff$unstable$src$Compiler$TestHelpers$resErrorToStrippedText)($s))((($$home$nw$stuff$unstable$src$Compiler$Lexer$lexer)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleName))($s));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name = (($n) => {
  return (((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))($n))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Test$isOkAndEqualTo = (($expectedOk) => {
  return ($$home$nw$stuff$unstable$src$Test$CodeExpectation)((($toText) => {
    return (($result) => {
      return ((($result)[0] === "Err")
        ? ((() => {
          const $e = ($result)[1];
          return ($core$Maybe$Just)($e);
        }))()
        : ((($result)[0] === "Ok")
          ? ((() => {
            const $actualOk = ($result)[1];
            return (((sp_equal)($expectedOk))($actualOk)
              ? $core$Maybe$Nothing
              : ($core$Maybe$Just)((($core$Text$join)("\n"))((($core$Core$Cons)("expected = "))((($core$Core$Cons)(($toText)($expectedOk)))((($core$Core$Cons)(""))((($core$Core$Cons)("actual = "))((($core$Core$Cons)(($toText)($actualOk)))($core$Core$Nil))))))));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 81:6', (sp_toHuman)($result))));
    });
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$comments = (($$home$nw$stuff$unstable$src$Test$Group)("Comments"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] statement after comment"))("\n#\na = 1\n"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(2))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(3))(3))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(3))(4))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(5))(6))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(7))(8))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)("1"))))($core$Core$Nil)))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] nested comments allow a spurious newline?"))("\n[#[##]#]\na = 1\n"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(8))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(10))(10))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(10))(11))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(12))(13))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(14))(15))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)("1"))))($core$Core$Nil)))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("Single line"))("# hello"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(7))($$home$nw$stuff$unstable$src$Types$Token$Comment)))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("Multi line"))("[# single line #]\n\na [# inline #] = 1\n\n[#\n    multi line\n#]\n\n[# [# nested #] #]"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(16))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(19))(19))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(19))(20))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(21))(32))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(34))(35))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(36))(37))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)("1"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(39))(58))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(61))(78))($$home$nw$stuff$unstable$src$Types$Token$Comment)))($core$Core$Nil))))))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("brackets"))("[]"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(1))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(2))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Closed))))($core$Core$Nil)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] Inline comments should not break a block"))("allTests = [\n    , a\n#\n    ]"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(8))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("allTests"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(9))(10))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(11))(12))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(17))(18))($$home$nw$stuff$unstable$src$Types$Token$Comma)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(19))(20))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("a"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(21))(22))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(27))(28))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Closed))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(28))(28))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))($core$Core$Nil)))))))))))))($core$Core$Nil)))))));

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$indentation = (($$home$nw$stuff$unstable$src$Test$Group)("Blocks, sibling lines, indentation"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("1"))("\na =\n 1\nb = 1"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(1))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(2))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(3))(4))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(6))(6))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(6))(7))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)("1"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(8))(8))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(8))(8))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(8))(9))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("b"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(10))(11))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(12))(13))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)("1"))))($core$Core$Nil))))))))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] spurious spaces in front of field name"))("module =\n   importAs =\n      SPCore\n   globalTypes =\n      None"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(6))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("module"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(7))(8))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(12))(12))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(12))(20))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("importAs"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(21))(22))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(29))(29))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(29))(35))((($$home$nw$stuff$unstable$src$Types$Token$UpperName)($core$Maybe$Nothing))("SPCore"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(39))(39))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(39))(39))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(39))(50))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("globalTypes"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(51))(52))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(59))(59))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(59))(63))((($$home$nw$stuff$unstable$src$Types$Token$UpperName)($core$Maybe$Nothing))("None"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(63))(63))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(63))(63))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))($core$Core$Nil))))))))))))))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("Blocks and not"))((($core$Text$join)("\n"))((($core$Core$Cons)("module ="))((($core$Core$Cons)("   i ="))((($core$Core$Cons)("        j"))((($core$Core$Cons)("            >> k"))((($core$Core$Cons)("            >> s"))((($core$Core$Cons)(""))((($core$Core$Cons)("   importAs ="))((($core$Core$Cons)("      SPCore"))((($core$Core$Cons)(""))((($core$Core$Cons)("   globalTypes ="))((($core$Core$Cons)("      None"))((($core$Core$Cons)(""))((($core$Core$Cons)("   a +     # no block start!"))((($core$Core$Cons)("        b   # no sibling"))((($core$Core$Cons)("        c"))((($core$Core$Cons)(""))((($core$Core$Cons)("   d =     # block start"))((($core$Core$Cons)("        e   # sibling!"))((($core$Core$Cons)("        f"))((($core$Core$Cons)(""))((($core$Core$Cons)("   g = h"))($core$Core$Nil))))))))))))))))))))))))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(6))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("module"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(7))(8))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(12))(12))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(12))(13))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("i"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(14))(15))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(24))(24))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(24))(25))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("j"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(38))(40))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$sendRight))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(41))(42))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("k"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(55))(57))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$sendRight))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(58))(59))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("s"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(64))(64))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(64))(64))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(64))(72))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("importAs"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(73))(74))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(81))(81))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(81))(87))((($$home$nw$stuff$unstable$src$Types$Token$UpperName)($core$Maybe$Nothing))("SPCore"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(92))(92))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(92))(92))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(92))(103))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("globalTypes"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(104))(105))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(112))(112))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(112))(116))((($$home$nw$stuff$unstable$src$Types$Token$UpperName)($core$Maybe$Nothing))("None"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(121))(121))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(121))(121))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(121))(122))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("a"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(123))(124))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$add))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(129))(146))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(155))(156))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("b"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(159))(171))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(180))(181))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("c"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(186))(186))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(186))(187))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("d"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(188))(189))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(194))(207))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(216))(216))($$home$nw$stuff$unstable$src$Types$Token$BlockStart)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(216))(217))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("e"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(220))(230))($$home$nw$stuff$unstable$src$Types$Token$Comment)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(239))(239))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(239))(240))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("f"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(245))(245))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(245))(245))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(245))(246))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("g"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(247))(248))(($$home$nw$stuff$unstable$src$Types$Token$Defop)($$home$nw$stuff$unstable$src$Types$Token$DefNormal))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(249))(250))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("h"))($core$Core$Nil))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(250))(250))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))($core$Core$Nil)))))))))))))))))))))))))))))))))))))))))))))))))))($core$Core$Nil))));

const $core$Text$contains = (($sub) => {
  return (($str) => {
    const $$try1 = ((text_split)($sub))($str);
    return (((($$try1)[0] === "Cons") && ((($$try1)[2])[0] === "Nil"))
      ? false
      : (true
        ? true
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Text.sp 115:4', (sp_toHuman)($$try1))));
  });
});

const $$home$nw$stuff$unstable$src$Test$errorContains = (($snippets) => {
  return ($$home$nw$stuff$unstable$src$Test$CodeExpectation)((($toText) => {
    return (($result) => {
      return ((($result)[0] === "Ok")
        ? ((() => {
          const $ok = ($result)[1];
          return ($core$Maybe$Just)(("I was expecting an error, but got: Ok " + ($toText)($ok)));
        }))()
        : ((($result)[0] === "Err")
          ? ((() => {
            const $e = ($result)[1];
            const $missing = (($core$List$filter)((($sn) => {
              return ($core$Basics$not)((($core$Text$contains)($sn))($e));
            })))($snippets);
            return (((sp_equal)($core$Core$Nil))($missing)
              ? $core$Maybe$Nothing
              : ((() => {
                const $indentedError = (($core$Text$join)("\n"))((($core$List$map)((($l) => {
                  return ("    " + $l);
                })))(((text_split)("\n"))($e)));
                return ($core$Maybe$Just)(("Error message:\n\n" + ($indentedError + ("\n\nis missing snippets: " + (($core$Text$join)(", "))($missing)))));
              }))());
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 102:6', (sp_toHuman)($result))));
    });
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$keywords = (($$home$nw$stuff$unstable$src$Test$Group)("keywords"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] can't @ keywords"))("@with"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("keyword"))($core$Core$Nil)))))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokensAndDrop = (($n) => {
  return (($s) => {
    return (($core$Result$map)(($core$List$drop)($n)))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens)($s));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$ops = (($$home$nw$stuff$unstable$src$Test$Group)("Operators"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] .. set Default"))(".. []"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(2))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$textConcat))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(3))(4))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Open))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(4))(5))(($$home$nw$stuff$unstable$src$Types$Token$SquareBracket)($$home$nw$stuff$unstable$src$Types$Token$Closed))))($core$Core$Nil))))))))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$position = (($$home$nw$stuff$unstable$src$Test$Group)("Position"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] ops position"))("blah <>"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("blah <>"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] ops position, with newline"))("blah <>\n"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("blah <>"))($core$Core$Nil)))))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Test$valueTest = (($toText) => {
  return (($title) => {
    return (($generateValue) => {
      return (($ce) => {
        const $$toMaybeError = $ce;
        const $toMaybeError = ($$toMaybeError)[1];
        return ((($$home$nw$stuff$unstable$src$Test$Single)($title))(""))((() => {
          return ($$home$nw$stuff$unstable$src$Test$maybeToOutcome)((($toMaybeError)($toText))(($core$Result$Ok)(($generateValue)(null))));
        }));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$valueTest = ($$home$nw$stuff$unstable$src$Test$valueTest)(sp_toHuman);

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$textLiterals = (($$home$nw$stuff$unstable$src$Test$Group)("Text literals"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("Empty Text"))("\"\""))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(2))(($$home$nw$stuff$unstable$src$Types$Token$TextLiteral)(""))))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("Followed by colon"))("\"n\":\n"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(3))(($$home$nw$stuff$unstable$src$Types$Token$TextLiteral)("n"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(3))(4))($$home$nw$stuff$unstable$src$Types$Token$Colon)))($core$Core$Nil)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("[reg] should not add the indent!"))("try char as\n    \"\":\n        None\n\n    \"@\""))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokensAndDrop)(11)))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(38))(41))(($$home$nw$stuff$unstable$src$Types$Token$TextLiteral)("@"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(41))(41))($$home$nw$stuff$unstable$src$Types$Token$BlockEnd)))($core$Core$Nil))))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$valueTest)("Unindent function"))((() => {
  return ($$home$nw$stuff$unstable$src$Compiler$Lexer$unindent)((($core$Text$join)(""))((($core$Core$Cons)("\n"))((($core$Core$Cons)("  a\n"))((($core$Core$Cons)("      \n"))((($core$Core$Cons)("\n"))((($core$Core$Cons)("  b\n"))((($core$Core$Cons)("  "))($core$Core$Nil))))))));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Text$join)(""))((($core$Core$Cons)("a\n"))((($core$Core$Cons)("    \n"))((($core$Core$Cons)("\n"))((($core$Core$Cons)("b"))($core$Core$Nil)))))))))($core$Core$Nil)))));

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$unaryAddittiveOps = (($$home$nw$stuff$unstable$src$Test$Group)("Unary addittive ops"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("-a"))("-a"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(1))(($$home$nw$stuff$unstable$src$Types$Token$Unop)($$home$nw$stuff$unstable$src$Prelude$unaryMinus))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(2))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))($core$Core$Nil)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("a - -a"))("a - -a"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(1))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(2))(3))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$subtract))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(4))(5))(($$home$nw$stuff$unstable$src$Types$Token$Unop)($$home$nw$stuff$unstable$src$Prelude$unaryMinus))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(5))(6))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))($core$Core$Nil)))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("a-a"))("a-a"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(1))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(1))(2))(($$home$nw$stuff$unstable$src$Types$Token$Unop)($$home$nw$stuff$unstable$src$Prelude$unaryMinus))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(2))(3))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$non_mut_name)("a"))))($core$Core$Nil))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("Mutable colon:"))("@:"))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(2))($$home$nw$stuff$unstable$src$Types$Token$MutableColon)))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("-="))("-="))($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokens))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(0))($$home$nw$stuff$unstable$src$Types$Token$NewSiblingLine)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(2))(($$home$nw$stuff$unstable$src$Types$Token$Binop)($$home$nw$stuff$unstable$src$Prelude$mutableSubtract))))($core$Core$Nil))))))($core$Core$Nil))))));

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$underscores = (($$home$nw$stuff$unstable$src$Test$Group)("Underscores"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("'_' as a Name"))("_"))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokensAndDrop)(1)))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(1))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("_"))($core$Core$Nil))))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("'_10_20' as a Name"))("_10_20"))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokensAndDrop)(1)))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(6))((((($$home$nw$stuff$unstable$src$Types$Token$LowerName)($$home$nw$stuff$unstable$src$Types$Token$NameNoModifier))($core$Maybe$Nothing))("_10_20"))($core$Core$Nil))))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$codeTest)("'10_20' as a Number"))("10_20"))(($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$lexTokensAndDrop)(1)))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Types$Token$Token)(0))(5))(($$home$nw$stuff$unstable$src$Types$Token$NumberLiteral)("10_20"))))($core$Core$Nil)))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$Lexer_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("Lexer"))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$keywords))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$ops))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$unaryAddittiveOps))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$indentation))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$comments))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$underscores))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$position))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$textLiterals))($core$Core$Nil)))))))));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest = ($$home$nw$stuff$unstable$src$Test$codeTest)(sp_toHuman);

const $$home$nw$stuff$unstable$src$DefaultModules$asText = "library =\n    source = \"core:prelude\"\n\n    module =\n        path = Core\n        importAs = Core\n        globalTypes =\n            None\n            Bool\n            Text\n            Number\n        globalValues =\n            None\n            True\n            False\n\n    module =\n        path = List\n        importAs = List\n\n    module =\n        path = Maybe\n        importAs = Maybe\n        globalTypes =\n            Maybe\n        globalValues =\n           Just\n           Nothing\n\n    module =\n        path = Text\n        importAs = Text\n\n    module =\n        path = Tuple\n        importAs = Tuple\n\n    module =\n        path = Debug\n        importAs = Debug\n        globalValues =\n            log\n            todo\n\n    module =\n        path = Basics\n        globalTypes =\n            Int\n        globalValues =\n            assert\n            clamp\n            identity\n            modBy\n            min\n            max\n\n    module =\n        path = Dict\n        importAs = Dict\n        globalTypes = Dict\n\n    module =\n        path = Set\n        importAs = Set\n        globalTypes = Set\n\n    module =\n        path = Result\n        importAs = Result\n        globalTypes = Result\n        globalValues =\n            Ok\n            Err";

const $$home$nw$stuff$unstable$src$ModulesFile$textToMeta = (($sponName) => {
  return (($sponContent) => {
    return (($core$Result$map)($$home$nw$stuff$unstable$src$ModulesFile$toMeta))((($$home$nw$stuff$unstable$src$ModulesFile$textToModulesFile)($sponName))($sponContent));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$meta = ((() => {
  const $eenv = ({
    moduleByName: (($core$Dict$singleton)("DefaultModules"))(({
      content: $$home$nw$stuff$unstable$src$DefaultModules$asText,
      fsPath: "<DefaultModules>",
    })),
  });
  const $metaResult = (($core$Result$mapError)((($e) => {
    return ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$formattedToStrippedText)((($$home$nw$stuff$unstable$src$Compiler$Error$toFormattedText)($eenv))($e));
  })))((($$home$nw$stuff$unstable$src$ModulesFile$textToMeta)("DefaultModules"))($$home$nw$stuff$unstable$src$DefaultModules$asText));
  return ((($metaResult)[0] === "Err")
    ? ((() => {
      const $e = ($metaResult)[1];
      ((sp_log)(("Error in DefaultModules.sp: " + $e)))(null);
      return (sp_todo)("error loading DefaultModules.sp");
    }))()
    : ((($metaResult)[0] === "Ok")
      ? ((() => {
        const $m = ($metaResult)[1];
        return $m;
      }))()
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TestHelpers.sp 46:4', (sp_toHuman)($metaResult))));
}))();

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$source = ($$home$nw$stuff$unstable$src$Types$Meta$SourceDir)("<Test>");

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$params = ({
  meta: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$meta,
  name: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleName,
  source: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$source,
  stripLocations: true,
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$textToModule = (($code) => {
  return (($$home$nw$stuff$unstable$src$Compiler$TestHelpers$resErrorToStrippedText)($code))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$textToCanonicalModule)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$params))($code));
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
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 216:4', (sp_toHuman)($list))));
});

const $core$Result$fromMaybe = (($err) => {
  return (($maybe) => {
    return ((($maybe)[0] === "Nothing")
      ? ($core$Result$Err)($err)
      : ((($maybe)[0] === "Just")
        ? ((() => {
          const $a = ($maybe)[1];
          return ($core$Result$Ok)($a);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 34:4', (sp_toHuman)($maybe))));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition = (($code) => {
  return (($core$Result$onOk)((($mod) => {
    return (($core$Result$fromMaybe)("firstDefinition fail"))(($core$List$head)(($core$Dict$values)($mod.valueDefs)));
  })))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$textToModule)($code));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation = (($name) => {
  return (($code) => {
    return (($core$Result$onOk)((($def) => {
      return ($core$Result$Ok)($def.body);
    })))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition)($code));
  });
});

const $$home$nw$stuff$unstable$src$Test$isOk = ($$home$nw$stuff$unstable$src$Test$CodeExpectation)((($toText) => {
  return (($result) => {
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
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 69:8', (sp_toHuman)($result))));
  });
}));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$annotations = (($$home$nw$stuff$unstable$src$Test$Group)("Annotations"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("annotation on mutable value"))("x =\n  a as Number @=\n    3\n  a"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("annotation on immutable value"))("b as Number =\n  3"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("b")))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p = $$home$nw$stuff$unstable$src$Types$Pos$T;

const $$home$nw$stuff$unstable$src$Test$freeform = (($test) => {
  return ($$home$nw$stuff$unstable$src$Test$CodeExpectation)((($toText) => {
    return (($result) => {
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
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 62:4', (sp_toHuman)($result))));
    });
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$shouldHaveSameAB = (($getter) => {
  return ($$home$nw$stuff$unstable$src$Test$freeform)((($$a) => {
    const $a = $$a.first;
    const $b = $$a.second;
    return (((sp_equal)(($getter)($b)))(($getter)($a))
      ? $core$Maybe$Nothing
      : ($core$Maybe$Just)((($core$Text$join)("\n"))((($core$Core$Cons)("The two don't match:"))((($core$Core$Cons)((sp_toHuman)(($getter)($a))))((($core$Core$Cons)((sp_toHuman)(($getter)($b))))($core$Core$Nil))))));
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$transformAB = (($code) => {
  const $findAB = (($mod) => {
    const $$try1 = ((list_sortBy)((($def) => {
      return $def.pattern;
    })))(($core$Dict$values)($mod.valueDefs));
    return (((($$try1)[0] === "Cons") && (((($$try1)[2])[0] === "Cons") && (((($$try1)[2])[2])[0] === "Nil")))
      ? ((() => {
        const $a = ($$try1)[1];
        const $b = (($$try1)[2])[1];
        return ($core$Maybe$Just)(({
          first: $a,
          second: $b,
        }));
      }))()
      : (true
        ? $core$Maybe$Nothing
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/MakeCanonical_Test.sp 48:8', (sp_toHuman)($$try1))));
  });
  return (($core$Result$onOk)((($x) => {
    return (($core$Result$fromMaybe)("findAB fail"))(($findAB)($x));
  })))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$textToModule)($code));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$binops = (($$home$nw$stuff$unstable$src$Test$Group)("Binops"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("left associativity"))("a = v >> f >> g\nb = (v >> f) >> g"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$transformAB))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$shouldHaveSameAB)((($x) => {
  return $x.body;
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("right associativity"))("a = v :: f :: g\nb = v :: (f :: g)"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$transformAB))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$shouldHaveSameAB)((($x) => {
  return $x.body;
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("precedence"))("a = 1 + 2 * 3 + 4\nb = 1 + (2 * 3) + 4"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$transformAB))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$shouldHaveSameAB)((($x) => {
  return $x.body;
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("SKIP functional notation"))("a = (-)"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
  attrPath: $core$Core$Nil,
  ref: ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$makeUsr)("-")),
}))))))($core$Core$Nil)))));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinitionStripDeps = (($code) => {
  return (($core$Result$map)((($v) => {
    return (Object.assign)({}, $v, ({
      directConsDeps: $core$Dict$empty,
      directTypeDeps: $core$Dict$empty,
      directValueDeps: $core$Dict$empty,
    }));
  })))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition)($code));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$functions = (($$home$nw$stuff$unstable$src$Test$Group)("Functions"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("[rec] lambda with two arguments"))("f =\n  a: b: 1"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("f")))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$boolType = ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR)("Bool")))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr = (($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$source))($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleName);

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal = (($name) => {
  return ($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr))($name));
});

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$lists = (($$home$nw$stuff$unstable$src$Test$Group)("Lists"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("list type sugar"))("l as [ Bool ] =\n  l"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinitionStripDeps))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  body: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
    attrPath: $core$Core$Nil,
    ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("l"),
  })),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  mutable: false,
  native: false,
  nonFn: $core$Dict$empty,
  parentDefinitions: $core$Core$Nil,
  pattern: ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(($core$Maybe$Just)("l")))(($core$Maybe$Just)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$boolType))),
})))))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$moduleAndAttributePaths = ((() => {
  const $accept = (($s) => {
    return (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)($s))(("a = " + $s)))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk);
  });
  const $reject = (($s) => {
    return (($m) => {
      return (((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)($s))(("a = " + $s)))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)($m))($core$Core$Nil)));
    });
  });
  return (($$home$nw$stuff$unstable$src$Test$Group)("Module and Attribute Paths"))((($core$Core$Cons)(($accept)("blah.blah.blah")))((($core$Core$Cons)((($reject)("Blah.Blah.blah"))("constructor")))((($core$Core$Cons)((($reject)("blah.Blah.blah"))("case")))((($core$Core$Cons)((($reject)("List.blah.Blah"))("lower")))((($core$Core$Cons)((($reject)("List..blah"))("space")))((($core$Core$Cons)((($reject)(".Blah"))("upper")))((($core$Core$Cons)((($reject)(".blah.blah"))("shorthand")))((($core$Core$Cons)((($reject)(".blah"))("shorthand")))((($core$Core$Cons)((($reject)("..."))("")))((($core$Core$Cons)(($accept)("x .. y")))($core$Core$Nil)))))))))));
}))();

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$patterns = (($$home$nw$stuff$unstable$src$Test$Group)("Patterns"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("[reg] record patterns are NOT extensible"))("a =\n  { b with c } = d"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("with"))($core$Core$Nil)))))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$pipes = (($$home$nw$stuff$unstable$src$Test$Group)("Pipes"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("sendLeft is inlined"))("a = thing >> function"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
  attrPath: $core$Core$Nil,
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("function"),
}))))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
  attrPath: $core$Core$Nil,
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("thing"),
}))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("sendRight is inlined"))("a = function << thing"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Call)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
  attrPath: $core$Core$Nil,
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("function"),
}))))(($$home$nw$stuff$unstable$src$Types$CanonicalAst$ArgumentExpression)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
  attrPath: $core$Core$Nil,
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("thing"),
}))))))))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$records = (($$home$nw$stuff$unstable$src$Test$Group)("Records"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("functional update"))("a = { m with b, c = 1 }"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(($core$Maybe$Just)(({
  attrPath: $core$Core$Nil,
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("m"),
}))))(($core$Dict$fromList)((($core$Core$Cons)(({
  first: "c",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(1),
})))((($core$Core$Cons)(({
  first: "b",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
    attrPath: $core$Core$Nil,
    ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("b"),
  })),
})))($core$Core$Nil))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("update shorthand"))("b = { a.k with y = .x }"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("b")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(($core$Maybe$Just)(({
  attrPath: (($core$Core$Cons)("k"))($core$Core$Nil),
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("a"),
}))))((($core$Dict$singleton)("y"))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
  attrPath: (($core$Core$Cons)("k"))((($core$Core$Cons)("x"))($core$Core$Nil)),
  ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("a"),
}))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("annotation, extensible"))("a as { b with x as Bool } =\n  a"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("disabled"))($core$Core$Nil)))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$numberType = ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR)("Number")))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$tuples = (($$home$nw$stuff$unstable$src$Test$Group)("Tuples"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("tuple2"))("a = 1 & 2"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))($core$Maybe$Nothing))(($core$Dict$fromList)((($core$Core$Cons)(({
  first: "first",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(1),
})))((($core$Core$Cons)(({
  first: "second",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(2),
})))($core$Core$Nil))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("tuple3"))("a = 1 & 2 & 3"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$Record)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))($core$Maybe$Nothing))(($core$Dict$fromList)((($core$Core$Cons)(({
  first: "first",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(1),
})))((($core$Core$Cons)(({
  first: "second",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(2),
})))((($core$Core$Cons)(({
  first: "third",
  second: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(3),
})))($core$Core$Nil)))))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("tuple4"))("a = 1 & 2 & 3 & 4"))(($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstEvaluation)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("use a record"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("tuple2 type"))("a as Number & Number =\n  a"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinitionStripDeps))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  body: (($$home$nw$stuff$unstable$src$Types$CanonicalAst$Variable)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(({
    attrPath: $core$Core$Nil,
    ref: ($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)("a"),
  })),
  directConsDeps: $core$Dict$empty,
  directTypeDeps: $core$Dict$empty,
  directValueDeps: $core$Dict$empty,
  mutable: false,
  native: false,
  nonFn: $core$Dict$empty,
  parentDefinitions: $core$Core$Nil,
  pattern: ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))(($core$Maybe$Just)("a")))(($core$Maybe$Just)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$p))($core$Maybe$Nothing))(((($core$Dict$insert)("second"))($$home$nw$stuff$unstable$src$Compiler$TestHelpers$numberType))(((($core$Dict$insert)("first"))($$home$nw$stuff$unstable$src$Compiler$TestHelpers$numberType))($core$Dict$empty))))),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("tuple4, type"))("a as Blah & Blah & Blah & Blah =\n  a"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$firstDefinition))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("Use a record"))($core$Core$Nil)))))($core$Core$Nil))))));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$unionTypes = (($$home$nw$stuff$unstable$src$Test$Group)("Union types"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("SKIP tuples op precedence"))("union A = X Bool & Bool"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$textToModule))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("operators"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("Tuples op precedence works with parens"))("union A = X (Bool & Bool)"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$textToModule))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$codeTest)("SKIP [reg] Should reject uppercase arg name"))("union Outcome Token output = A"))($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$textToModule))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("Token"))($core$Core$Nil)))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("MakeCanonical"))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$unionTypes))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$binops))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$tuples))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$lists))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$moduleAndAttributePaths))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$records))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$patterns))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$annotations))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$pipes))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$functions))($core$Core$Nil)))))))))));

const $$home$nw$stuff$unstable$src$Compiler$Parser$binopInsideParens = (($env) => {
  return (($$home$nw$stuff$unstable$src$Compiler$Parser$andThen)((($$end) => {
    const $start = ($$end)[1];
    const $end = ($$end)[2];
    const $k = ($$end)[3];
    return ((($k)[0] === "Binop")
      ? ((() => {
        const $binop = ($k)[1];
        return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)((($$home$nw$stuff$unstable$src$Types$FormattableAst$PrefixBinop)(((($$home$nw$stuff$unstable$src$Compiler$Parser$pos)($env))($start))($end)))($binop.symbol));
      }))()
      : (true
        ? $$home$nw$stuff$unstable$src$SPLib$Parser$reject
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser.sp 1204:4', (sp_toHuman)($k))));
  })))($$home$nw$stuff$unstable$src$Compiler$Parser$oneToken);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser$inlineOrIndented = (($p) => {
  return ($$home$nw$stuff$unstable$src$SPLib$Parser$oneOf)((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser$block)($p)))((($core$Core$Cons)($p))($core$Core$Nil)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest = ($$home$nw$stuff$unstable$src$Test$codeTest)(sp_toHuman);

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$asDefinition = (($s) => {
  return ((($s)[0] === "Definition")
    ? ((() => {
      const $a = ($s)[2];
      return ($core$Result$Ok)($a);
    }))()
    : (true
      ? ($core$Result$Err)("Test says: no def")
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser_Test.sp 29:4', (sp_toHuman)($s))));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstStatement = (($code) => {
  const $grabFirst = (($stats) => {
    return ((($stats)[0] === "Nil")
      ? ($core$Result$Err)("Test says: no statements")
      : ((($stats)[0] === "Cons")
        ? ((() => {
          const $head = ($stats)[1];
          const $tail = ($stats)[2];
          return ($core$Result$Ok)($head);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser_Test.sp 52:8', (sp_toHuman)($stats))));
  });
  return (($core$Result$onOk)($grabFirst))((($$home$nw$stuff$unstable$src$Compiler$TestHelpers$resErrorToStrippedText)($code))((($$home$nw$stuff$unstable$src$Compiler$Parser$textToFormattableModule)(({
    name: "Test",
    stripLocations: true,
  })))($code)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation = (($code) => {
  const $grabAnnotation = (($def) => {
    const $$try1 = $def.pattern;
    return (((($$try1)[0] === "PatternAny") && ((($$try1)[4])[0] === "Just"))
      ? ((() => {
        const $pos = ($$try1)[1];
        const $name = ($$try1)[2];
        const $mutable = ($$try1)[3];
        const $ty = (($$try1)[4])[1];
        return ($core$Result$Ok)($ty);
      }))()
      : (true
        ? ($core$Result$Err)("no annotation")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser_Test.sp 94:8', (sp_toHuman)($$try1))));
  });
  return (($core$Result$onOk)($grabAnnotation))((($core$Result$onOk)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$asDefinition))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstStatement)($code)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$p = $$home$nw$stuff$unstable$src$Types$Pos$T;

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant = (($name) => {
  return (((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeConstant)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($core$Maybe$Nothing))($name))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$annotations = (($$home$nw$stuff$unstable$src$Test$Group)("Annotations"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Mutability 1"))("a as Number @: Int: None =\n  1"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Number")))(true))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Int")))(false))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("None")))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Mutability 2"))("a as Number: Int @: None =\n  1"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Number")))(false))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Int")))(true))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("None")))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Tuple precedence"))("a as Int & Int: Bool =\n  1"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeFunction)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeTuple)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Int")))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Int")))($core$Core$Nil)))))(false))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Bool"))))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$asEvaluation = (($s) => {
  return ((($s)[0] === "Evaluation")
    ? ((() => {
      const $a = ($s)[2];
      return ($core$Result$Ok)($a);
    }))()
    : (true
      ? ($core$Result$Err)("Test says: no eval")
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser_Test.sp 39:4', (sp_toHuman)($s))));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition = (($code) => {
  return (($core$Result$onOk)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$asDefinition))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstStatement)($code));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition = (($code) => {
  const $grabFirst = (($def) => {
    const $$try1 = $def.body;
    return ((($$try1)[0] === "Nil")
      ? ($core$Result$Err)("Test says: empty def body")
      : ((($$try1)[0] === "Cons")
        ? ((() => {
          const $head = ($$try1)[1];
          const $tail = ($$try1)[2];
          return ($core$Result$Ok)($head);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser_Test.sp 79:8', (sp_toHuman)($$try1))));
  });
  return (($core$Result$onOk)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$asEvaluation))((($core$Result$onOk)($grabFirst))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition)($code)));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$binops = ((() => {
  const $sendBtoC = (($b) => {
    return (($c) => {
      return ((($$home$nw$stuff$unstable$src$Types$FormattableAst$Binop)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($$home$nw$stuff$unstable$src$Types$Op$Pipe))(({
        first: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$Variable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($core$Maybe$Nothing))("b"))($core$Core$Nil),
        second: (($core$Core$Cons)(({
          first: $$home$nw$stuff$unstable$src$Prelude$sendRight,
          second: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$Variable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($core$Maybe$Nothing))("c"))($core$Core$Nil),
        })))($core$Core$Nil),
      }));
    });
  });
  const $sendBtoCtoD = (($b) => {
    return (($c) => {
      return (($d) => {
        return ((($$home$nw$stuff$unstable$src$Types$FormattableAst$Binop)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($$home$nw$stuff$unstable$src$Types$Op$Pipe))(({
          first: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$Variable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($core$Maybe$Nothing))("b"))($core$Core$Nil),
          second: (($core$Core$Cons)(({
            first: $$home$nw$stuff$unstable$src$Prelude$sendRight,
            second: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$Variable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($core$Maybe$Nothing))("c"))($core$Core$Nil),
          })))((($core$Core$Cons)(({
            first: $$home$nw$stuff$unstable$src$Prelude$sendRight,
            second: (((($$home$nw$stuff$unstable$src$Types$FormattableAst$Variable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))($core$Maybe$Nothing))("d"))($core$Core$Nil),
          })))($core$Core$Nil)),
        }));
      });
    });
  });
  return (($$home$nw$stuff$unstable$src$Test$Group)("Binops"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("no indent"))("a = b >> c"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($sendBtoC)(5))(10)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("assignment indent"))("a =\n    b >> c"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($sendBtoC)(9))(14)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("pipe indent"))("a =\n    b\n      >> c"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($sendBtoC)(9))(20)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("pipe indent"))("a =\n    b\n      >> c\n      >> d"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($sendBtoCtoD)(9))(20))(31)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("pyramid indent"))("a =\n    b\n      >> c\n        >> d"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($sendBtoCtoD)(9))(20))(33)))))($core$Core$Nil))))));
}))();

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluation = (($code) => {
  return (($core$Result$onOk)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$asEvaluation))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstStatement)($code));
});

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$ifs = (($$home$nw$stuff$unstable$src$Test$Group)("Ifs"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("inline"))("a = if a then b else c"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("SKIP multiline, formatted"))("x =\n    if a then\n        b\n    else\n        c"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("multiline, compact"))("x =\n  if a then b\n  else c"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$lambdas = (($$home$nw$stuff$unstable$src$Test$Group)("lambdas"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Inline nesting"))("a: b: 3"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("a"))($core$Maybe$Nothing)))(false))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("b"))($core$Maybe$Nothing)))(false))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("3"))))($core$Core$Nil)))))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Block nesting"))("a:\n  b:\n    3"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("a"))($core$Maybe$Nothing)))(false))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("b"))($core$Maybe$Nothing)))(false))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("3"))))($core$Core$Nil)))))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Sibling nesting"))("a:\nb:\n3"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("a"))($core$Maybe$Nothing)))(false))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$Lambda)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("b"))($core$Maybe$Nothing)))(false))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Evaluation)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("3"))))($core$Core$Nil)))))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("SKIP Tuple has precedence over lambda"))("x =\n  a & b: a"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] pass to function without parens"))("i =\n  x @= 1\n  xxx y: y"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil))))));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$lists = (($$home$nw$stuff$unstable$src$Test$Group)("Lists"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Inline"))("[1, 2]"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$List)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("1")))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("2")))($core$Core$Nil)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Multiline canonical"))("a =\n  [\n  , 1\n  , 2\n  ]"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$List)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("1")))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("2")))($core$Core$Nil)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Multiline compact"))("a = [\n  , 1\n  , 2\n  ]"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$List)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("1")))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("2")))($core$Core$Nil)))))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$parens = (($$home$nw$stuff$unstable$src$Test$Group)("Parens"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("SKIP Can exist on multiple lines even when useless"))("tests =\n    (Ok\n    )"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Can exist on multiple lines"))("tests =\n    blah\n        (Ok\n        )"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$patterns = (($$home$nw$stuff$unstable$src$Test$Group)("Patterns"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("list unpacking"))("[a, b] = x"))((($x) => {
  return (($core$Result$map)((($y) => {
    return $y.pattern;
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition)($x));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternList)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("a"))($core$Maybe$Nothing)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternAny)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(false))("b"))($core$Maybe$Nothing)))($core$Core$Nil)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("list unpacking, inner block"))("x =\n   [ a, b ] = c"))((($x) => {
  return (($core$Result$map)((($y) => {
    return $y.pattern;
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition)($x));
})))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("record unpacking"))("{ a, b } = x"))((($x) => {
  return (($core$Result$map)((($y) => {
    return $y.pattern;
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition)($x));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$PatternRecord)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("a"),
    second: $core$Maybe$Nothing,
  })))((($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("b"),
    second: $core$Maybe$Nothing,
  })))($core$Core$Nil)),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("record unpacking, inner block"))("x =\n  { a, b } = c"))((($x) => {
  return (($core$Result$map)((($y) => {
    return $y.pattern;
  })))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition)($x));
})))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)))));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$records = (($$home$nw$stuff$unstable$src$Test$Group)("Records"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("inline"))("a = { x = 1 }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Record)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("x"),
    second: ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("1")),
  })))($core$Core$Nil),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("multiline"))("a =\n  {\n  , x = 1\n  , y = 2\n  }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Record)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("x"),
    second: ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("1")),
  })))((($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("y"),
    second: ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("2")),
  })))($core$Core$Nil)),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("multiline compact"))("a = {\n  , x = 1\n  , y = 2\n  }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$Record)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("x"),
    second: ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("1")),
  })))((($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("y"),
    second: ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$FormattableAst$LiteralNumber)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("2")),
  })))($core$Core$Nil)),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Annotation, inline"))("a as { x as Bool } =\n  a"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeRecord)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("x"),
    second: ($core$Maybe$Just)(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Bool")),
  })))($core$Core$Nil),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("SKIP Annotation, own line"))("a as\n   { x as Bool }\n   =\n   1"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeRecord)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("x"),
    second: ($core$Maybe$Just)(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Bool")),
  })))($core$Core$Nil),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("SKIP Annotation, multiline"))("a as {\n   , x as Bool\n   }\n      =\n      a"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstAnnotation))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeRecord)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(({
  attrs: (($core$Core$Cons)(({
    first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("x"),
    second: ($core$Maybe$Just)(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Bool")),
  })))($core$Core$Nil),
  extends: $core$Maybe$Nothing,
}))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] simple assignment, inline"))("a = { b with c }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] simple assignment, as block"))("a =\n  { b with c }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] simple assignment, as block"))("a =\n  { b with c = 1 }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] real-world use"))("a =\n  { state with\n      , pos = endPos\n      , code = rest\n      , accum =\n          { kind = Token.Comment\n          , start = startPos\n          , end = endPos\n          }\n              :: state.accum\n  }"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)))))))))));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$tries = (($$home$nw$stuff$unstable$src$Test$Group)("Try"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("multiline, formatted"))("x =\n  try a as\n    b:\n      c\n    d:\n      e"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("multiline, compact"))("x =\n  try a as\n    b: c\n    d: e"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstEvaluationOfDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$unionDefs = ((() => {
  const $asTypeDef = (($s) => {
    return ((($s)[0] === "UnionDef")
      ? ((() => {
        const $a = ($s)[2];
        return ($core$Result$Ok)($a);
      }))()
      : (true
        ? ($core$Result$Err)("no type def")
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/Parser_Test.sp 322:8', (sp_toHuman)($s))));
  });
  const $firstTypeDef = (($x) => {
    return (($core$Result$onOk)($asTypeDef))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstStatement)($x));
  });
  return (($$home$nw$stuff$unstable$src$Test$Group)("Type Definitions"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Parse inline def"))("union A b c = V1 b, V2 c, V3, V4 b c"))($firstTypeDef))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
    args: (($core$Core$Cons)("b"))((($core$Core$Cons)("c"))($core$Core$Nil)),
    constructors: (($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("V1"),
      second: (($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeVariable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("b")))($core$Core$Nil),
    })))((($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("V2"),
      second: (($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeVariable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("c")))($core$Core$Nil),
    })))((($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("V3"),
      second: $core$Core$Nil,
    })))((($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("V4"),
      second: (($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeVariable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("b")))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeVariable)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("c")))($core$Core$Nil)),
    })))($core$Core$Nil)))),
    name: "A",
  })))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("Parse multiline def"))("union A =\n   , V1\n   , V2"))($firstTypeDef))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
    args: $core$Core$Nil,
    constructors: (($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("V1"),
      second: $core$Core$Nil,
    })))((($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("V2"),
      second: $core$Core$Nil,
    })))($core$Core$Nil)),
    name: "A",
  })))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("list argument"))("union A = A [Int]"))($firstTypeDef))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
    args: $core$Core$Nil,
    constructors: (($core$Core$Cons)(({
      first: (($$home$nw$stuff$unstable$src$Types$Pos$At)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))("A"),
      second: (($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$FormattableAst$TypeList)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$p))(($$home$nw$stuff$unstable$src$Compiler$Parser_Test$typeConstant)("Int"))))($core$Core$Nil),
    })))($core$Core$Nil),
    name: "A",
  })))))($core$Core$Nil))));
}))();

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$values = (($$home$nw$stuff$unstable$src$Test$Group)("Values"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] Unop"))("a = f -n"))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$Parser_Test$codeTest)("[reg] deal with spurious NewSiblingLine introduced by inline comments"))("library =\n    # \"spcore\" is a special value for the core library\n    source = \"spcore\""))($$home$nw$stuff$unstable$src$Compiler$Parser_Test$firstDefinition))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Compiler$Parser_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("Parser"))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$values))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$parens))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$lambdas))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$annotations))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$unionDefs))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$lists))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$records))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$ifs))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$tries))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$patterns))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$binops))($core$Core$Nil))))))))))));

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$listType = (($itemType) => {
  return ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR)("List")))((($core$Core$Cons)($itemType))($core$Core$Nil));
});

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$localType = (($name) => {
  return (($$home$nw$stuff$unstable$src$Types$Meta$USR)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr))($name);
});

const $$home$nw$stuff$unstable$src$Compiler$TestHelpers$noneType = ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Types$Meta$spCoreUSR)("None")))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck$splitName = (($s) => {
  const $$try1 = ((text_split)("."))($s);
  return (((($$try1)[0] === "Cons") && (((($$try1)[2])[0] === "Cons") && (((($$try1)[2])[2])[0] === "Nil")))
    ? ((() => {
      const $moduleName = ($$try1)[1];
      const $valueName = (($$try1)[2])[1];
      return ({
        first: ($core$Maybe$Just)($moduleName),
        second: $valueName,
      });
    }))()
    : (true
      ? ({
        first: $core$Maybe$Nothing,
        second: $s,
      })
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck.sp 2286:4', (sp_toHuman)($$try1))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$outToHuman = (($out) => {
  const $freeVars = $out.freeTypeVariables;
  const $nf = (($core$Text$join)(", "))(($core$Dict$keys)((($core$Dict$filter)((($k) => {
    return (($v) => {
      return $v.nonFn;
    });
  })))($freeVars)));
  return (($core$Text$join)("\n"))((($core$Core$Cons)(("  freeTypeVariables = [ " + ((($core$Text$join)(", "))(($core$Dict$keys)($freeVars)) + " ]"))))((($core$Core$Cons)(("  (NonFunction = [" + ($nf + "])"))))((($core$Core$Cons)(("  isMutable = " + (sp_toHuman)($out.isMutable))))((($core$Core$Cons)(("  ty = " + ((($$home$nw$stuff$unstable$src$Human$CanonicalAst$typeToText)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr))($$home$nw$stuff$unstable$src$Compiler$TestHelpers$meta))($out.ty))))((($core$Core$Cons)(("  pos = " + (sp_toHuman)($out.ty))))($core$Core$Nil))))));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest = ($$home$nw$stuff$unstable$src$Test$codeTest)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$outToHuman);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall = (($vars) => {
  return ((($core$List$for)($vars))((($n) => {
    return (($core$Dict$insert)($n))(({
      nonFn: false,
    }));
  })))($core$Dict$empty);
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$ftv = (($n) => {
  return (($core$Dict$singleton)($n))(({
    nonFn: false,
  }));
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$function = (($from) => {
  return (($to) => {
    return (((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($$home$nw$stuff$unstable$src$Types$Pos$T))($from))(false))($to);
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNone = $$home$nw$stuff$unstable$src$Compiler$TestHelpers$noneType;

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber = $$home$nw$stuff$unstable$src$Compiler$TestHelpers$numberType;

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($$home$nw$stuff$unstable$src$Types$Pos$T);

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$initNstate = ({
  next: 0,
  replacements: $core$Dict$empty,
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen = $$home$nw$stuff$unstable$src$StateMonad$andThen;

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$get = $$home$nw$stuff$unstable$src$StateMonad$get;

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$intToName = (($n) => {
  return (($acc) => {
    return ((0 === $n)
      ? "a"
      : ((1 === $n)
        ? "b"
        : ((2 === $n)
          ? "c"
          : ((3 === $n)
            ? "d"
            : ((4 === $n)
              ? "e"
              : ((5 === $n)
                ? "f"
                : (true
                  ? (sp_todo)("intToName")
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 163:4', (sp_toHuman)($n)))))))));
  });
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$newName = (($state) => {
  return ({
    first: (($$home$nw$stuff$unstable$src$Human$CanonicalAst$intToName)($state.next))($core$Core$Nil),
    second: (Object.assign)({}, $state, ({
      next: ($state.next + 1),
    })),
  });
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$return = $$home$nw$stuff$unstable$src$StateMonad$return;

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$normName = (($name) => {
  return ((((text_length)($name) > 1) && ((sp_equal)($core$Maybe$Nothing))((text_toNumber)($name)))
    ? ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)($name)
    : (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($n2l) => {
      const $$try1 = (($core$Dict$get)($name))($n2l);
      return ((($$try1)[0] === "Just")
        ? ((() => {
          const $replacement = ($$try1)[1];
          return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)($replacement);
        }))()
        : ((($$try1)[0] === "Nothing")
          ? (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($n) => {
            const $addReplacement = (($s) => {
              return (Object.assign)({}, $s, ({
                replacements: ((($core$Dict$insert)($name))($n))($s.replacements),
              }));
            });
            return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((() => {
              return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)($n);
            })))(($$home$nw$stuff$unstable$src$StateMonad$update)($addReplacement));
          })))($$home$nw$stuff$unstable$src$Human$CanonicalAst$newName)
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 274:8', (sp_toHuman)($$try1))));
    })))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$get)((($x) => {
      return $x.replacements;
    }))));
});

const $$home$nw$stuff$unstable$src$StateMonad$list_map = (($f) => {
  return (($la) => {
    const $apply = (($a) => {
      return (($accum) => {
        return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($b) => {
          return ($$home$nw$stuff$unstable$src$StateMonad$return)(((sp_cons)($accum))($b));
        })))(($f)($a));
      });
    });
    return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($x) => {
      return ($$home$nw$stuff$unstable$src$StateMonad$return)(($core$List$reverse)($x));
    })))(((($$home$nw$stuff$unstable$src$StateMonad$list_foldl)($apply))($la))($core$Core$Nil));
  });
});

const $$home$nw$stuff$unstable$src$StateMonad$maybe_map = (($f) => {
  return (($ma) => {
    return ((($ma)[0] === "Nothing")
      ? ($$home$nw$stuff$unstable$src$StateMonad$return)($core$Maybe$Nothing)
      : ((($ma)[0] === "Just")
        ? ((() => {
          const $a = ($ma)[1];
          return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($b) => {
            return ($$home$nw$stuff$unstable$src$StateMonad$return)(($core$Maybe$Just)($b));
          })))(($f)($a));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/StateMonad.sp 66:4', (sp_toHuman)($ma))));
  });
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$normType = (($ty) => {
  return ((($ty)[0] === "TypeConstant")
    ? ((() => {
      const $pos = ($ty)[1];
      const $name = ($ty)[2];
      const $args = ($ty)[3];
      return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($args_n) => {
        return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($pos))($name))($args_n));
      })))((($$home$nw$stuff$unstable$src$StateMonad$list_map)($$home$nw$stuff$unstable$src$Human$CanonicalAst$normType))($args));
    }))()
    : ((($ty)[0] === "TypeVariable")
      ? ((() => {
        const $pos = ($ty)[1];
        const $name = ($ty)[2];
        return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($n) => {
          return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($pos))($n));
        })))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$normName)($name));
      }))()
      : ((($ty)[0] === "TypeFunction")
        ? ((() => {
          const $pos = ($ty)[1];
          const $from0 = ($ty)[2];
          const $fromIsMut = ($ty)[3];
          const $to0 = ($ty)[4];
          return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($from1) => {
            return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($to1) => {
              return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)((((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeFunction)($pos))($from1))($fromIsMut))($to1));
            })))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$normType)($to0));
          })))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$normType)($from0));
        }))()
        : ((($ty)[0] === "TypeRecord")
          ? ((() => {
            const $pos = ($ty)[1];
            const $ext0 = ($ty)[2];
            const $attrs0 = ($ty)[3];
            return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($ext1) => {
              return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($attrs1) => {
                return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($pos))($ext1))($attrs1));
              })))((($$home$nw$stuff$unstable$src$StateMonad$dict_map)((($k) => {
                return $$home$nw$stuff$unstable$src$Human$CanonicalAst$normType;
              })))($attrs0));
            })))((($$home$nw$stuff$unstable$src$StateMonad$maybe_map)($$home$nw$stuff$unstable$src$Human$CanonicalAst$normName))($ext0));
          }))()
          : ((($ty)[0] === "TypeAlias")
            ? ((() => {
              const $pos = ($ty)[1];
              const $path = ($ty)[2];
              const $t = ($ty)[3];
              return (($$home$nw$stuff$unstable$src$Human$CanonicalAst$andThen)((($t1) => {
                return ($$home$nw$stuff$unstable$src$Human$CanonicalAst$return)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeAlias)($pos))($path))($t1));
              })))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$normType)($t));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Human/CanonicalAst.sp 242:4', (sp_toHuman)($ty)))))));
});

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$normalizeTypeAndTyvars = (($tyOld) => {
  return (($tyvarsOld) => {
    const $$state = (($$home$nw$stuff$unstable$src$StateMonad$run)($$home$nw$stuff$unstable$src$Human$CanonicalAst$initNstate))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$normType)($tyOld));
    const $state = $$state.second;
    const $tyNew = $$state.first;
    const $replace = (($name) => {
      return ($core$Dict$insert)((($core$Maybe$withDefault)($name))((($core$Dict$get)($name))($state.replacements)));
    });
    const $tyvarsNew = ((($core$Dict$for)($tyvarsOld))($replace))($core$Dict$empty);
    return ({
      first: $tyNew,
      second: $tyvarsNew,
    });
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer = (($name) => {
  return (($code) => {
    const $tcEnvResult = ((() => {
      const $params = ({
        meta: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$meta,
        name: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleName,
        source: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$source,
        stripLocations: true,
      });
      return (($core$Result$onOk)((($module) => {
        const $modules = ((($core$Dict$insert)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr))($module))($$home$nw$stuff$unstable$src$Prelude$coreModulesByUmr);
        return (($core$Result$onOk)((($expandedTypes) => {
          const $$constructors = $expandedTypes;
          const $types = $$constructors.types;
          const $instanceVariables = $$constructors.instanceVariables;
          const $constructors = $$constructors.constructors;
          const $env = ({
            constructors: $constructors,
            currentModule: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr,
            instanceVariables: (($core$Dict$mapKeys)($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot))(((($core$Dict$insert)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr))("reset")))(({
              definedAt: $$home$nw$stuff$unstable$src$Types$Pos$T,
              freeTypeVariables: $core$Dict$empty,
              isMutable: false,
              ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber))(true))($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNone),
            })))(((($core$Dict$insert)((($$home$nw$stuff$unstable$src$Types$Meta$USR)($$home$nw$stuff$unstable$src$Compiler$TestHelpers$moduleUmr))("add")))(({
              definedAt: $$home$nw$stuff$unstable$src$Types$Pos$T,
              freeTypeVariables: $core$Dict$empty,
              isMutable: false,
              ty: (($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$function)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber))((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$function)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber))($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber)),
            })))($instanceVariables))),
            meta: $$home$nw$stuff$unstable$src$Compiler$TestHelpers$meta,
            nonAnnotatedRecursives: $core$Dict$empty,
            nonFreeTyvars: $core$Dict$empty,
            types: $types,
          });
          return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromModule)($env))($module);
        })))(($$home$nw$stuff$unstable$src$Compiler$Pipeline$globalExpandedTypes)($modules));
      })))((($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$textToCanonicalModule)($params))($code));
    }))();
    return (($core$Result$onOk)((($tcEnv) => {
      const $$try1 = (($core$Dict$get)(($$home$nw$stuff$unstable$src$Compiler$TestHelpers$rootLocal)($name)))($tcEnv.instanceVariables);
      return ((($$try1)[0] === "Nothing")
        ? ($core$Result$Err)("dict fail")
        : ((($$try1)[0] === "Just")
          ? ((() => {
            const $var = ($$try1)[1];
            const $$ty = (($$home$nw$stuff$unstable$src$Human$CanonicalAst$normalizeTypeAndTyvars)($var.ty))($var.freeTypeVariables);
            const $tyvars = $$ty.second;
            const $ty = $$ty.first;
            return ($core$Result$Ok)(({
              freeTypeVariables: $var.freeTypeVariables,
              isMutable: $var.isMutable,
              ty: $ty,
            }));
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Compiler/TypeCheck_Test.sp 151:4', (sp_toHuman)($$try1))));
    })))((($$home$nw$stuff$unstable$src$Compiler$TestHelpers$resErrorToStrippedText)($code))($tcEnvResult));
  });
});

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable = ($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)($$home$nw$stuff$unstable$src$Types$Pos$T);

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$functions = (($$home$nw$stuff$unstable$src$Test$Group)("functions"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Known function with correct params"))("a = add 3 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber,
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Known function with wrong params"))("a = add False"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("Bool"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Function inference 1"))("a = x: add x 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: (($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$function)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber))($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Function inference 2: same as 1, but with swapped args"))("a = x: add 1 x"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: (($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$function)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber))($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] fn has type None"))("a = x: 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$ftv)("1"),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("a")))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Multiple arguments are correctly inferred"))("a = x: y: z: x + y + z"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Annotation should be consistent with mutability"))("f as Number @: Number = a:\n  a"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("f")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("mutability"))($core$Core$Nil)))))($core$Core$Nil))))))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$higherOrderTypes = (($$home$nw$stuff$unstable$src$Test$Group)("higher order types"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Parse precedence"))("union T a = T a\n\na as T a: T a =\n    l: l"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$ftv)("0a"),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Compiler$TestHelpers$localType)("T")))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("0a")))($core$Core$Nil))))(false))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Compiler$TestHelpers$localType)("T")))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("0a")))($core$Core$Nil))),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Union type constructors"))("union X a = L\nl = L"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("l")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$ftv)("1"),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeConstant)($$home$nw$stuff$unstable$src$Types$Pos$T))(($$home$nw$stuff$unstable$src$Compiler$TestHelpers$localType)("X")))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)(($$home$nw$stuff$unstable$src$Types$Pos$I)(11)))("a")))($core$Core$Nil)),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP [reg] type check mistakes a union type with free tyvars for a free tyvar?"))("union O r e o = O r e o\n\nrun as (r: O r e o): r: O r e o = rToOreo: r:\n    rToOreo r"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("run")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Wrong should be Text"))("union O o = O Text o\n\nfun as Number: Text: O wrong = _: a:\n    O a a"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("fun")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("wrong"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP [reg] Should complain about undefined type argument"))("union O a = O Text output\nx = 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("undefined"))($core$Core$Nil)))))($core$Core$Nil))))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$if_else = (($$home$nw$stuff$unstable$src$Test$Group)("if..else"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("basic functionality"))("x = q:\n  if q then 1\n  else 2"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("rejects non-bool conditions"))("x = q:\n  if 1 then 1\n  else 2"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("Bool"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("rejects non-matching blocks"))("x = q:\n  if q then 2\n  else False"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("Number"))($core$Core$Nil)))))($core$Core$Nil))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$mu = (($$home$nw$stuff$unstable$src$Test$Group)("mutability"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Statement blocks that define mutables can't return functions"))("a =\n  x @= 1\n  y: y"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("can't return functions"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Statement blocks that define mutables can't return functions (with annotation)"))("a as y: y =\n  x @= 1\n  y: y"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("can't return functions"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Immutable variables can't be used as mutable"))("a = x:\n  @x := 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("mutable"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Detect mismatching annotations"))("a as Number: None =\n  reset"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("utability"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Correctly unify annotation's mutability"))("a as Number @: None =\n  reset"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNumber))(true))($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tyNone),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Functions can't be mutable 1"))("a @= x: x"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("utable"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Functions can't be mutable 2"))("a = f@:\n    @f := (x: x)"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("mutable args cannot be functions"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Functions can't be mutable 3"))("a = f@:\n  f 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("mutable args cannot be functions"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Lambda argument mutability is correctly inferred"))("a = x: reset x"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("mutability clash"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("*Nested* lambda argument mutability is correctly inferred"))("a = x: (y: reset y) x"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("mutability clash"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Functions can't be mutable (annotation)"))("a as Number: Number @=\n  add 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("utable"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Mutables can contain functions via free tyvars"))("a = x:\n  s @= x\n  s\n\nz as x: x =\n  a (x: x)"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Mutable assignment as last stament yields None"))("a as None =\n    x @= 1\n    @x := 2"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil))))))))))))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$nonFunction = (($$home$nw$stuff$unstable$src$Test$Group)("NonFunction"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP Basic functionality"))("blah as List a: List a =\n  with a NonFunction\n  a:\n  a\n\nmeh =\n  blah [x: x]"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("meh")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("should not contain functions"))($core$Core$Nil)))))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$patterns = (($$home$nw$stuff$unstable$src$Test$Group)("Patterns"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("List unpacking"))("x = q:\n   [ first, second ] = q\n   first"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$ftv)("2"),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(($$home$nw$stuff$unstable$src$Compiler$CoreTypes$list)((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)(($$home$nw$stuff$unstable$src$Types$Pos$I)(11)))("a"))))(false))((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeVariable)(($$home$nw$stuff$unstable$src$Types$Pos$I)(11)))("a")),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Records are correctly unpacked"))("x = q:\n    { first } = q\n    first"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall)((($core$Core$Cons)("2"))($core$Core$Nil)),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($$home$nw$stuff$unstable$src$Types$Pos$T))($core$Maybe$Nothing))(($core$Dict$fromList)((($core$Core$Cons)(({
    first: "first",
    second: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("a"),
  })))($core$Core$Nil)))))(false))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("a")),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Constructors should instantiate their variable types"))("each as [a]: (a: b): None =\n    ls: f:\n    try ls as\n        Core.Nil:\n            None\n\nresult =\n      1 :: Core.Nil = Core.Nil"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("result")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Trying to check against an inferred value?"))("tuple as Text & Number =\n    \"\" & 1\n\nx =\n    (a as Text) & (b as Number) =\n        tuple"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$records = (($$home$nw$stuff$unstable$src$Test$Group)("Records"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Attribute access"))("a = b: b.meh.blah"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall)((($core$Core$Cons)("2"))((($core$Core$Cons)("4"))((($core$Core$Cons)("5"))($core$Core$Nil)))),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(2)))(($core$Maybe$Just)("a")))((($core$Dict$singleton)("meh"))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(2)))(($core$Maybe$Just)("b")))((($core$Dict$singleton)("blah"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("c")))))))(false))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("c")),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Attribute mutation"))("a = b@: @b.meh.blah += 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall)((($core$Core$Cons)("2"))((($core$Core$Cons)("4"))($core$Core$Nil))),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(2)))(($core$Maybe$Just)("a")))((($core$Dict$singleton)("meh"))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(2)))(($core$Maybe$Just)("b")))((($core$Dict$singleton)("blah"))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number))))))(true))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$none),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Tuple3 direct item mutability"))("x =\n    a @= 3 & False & 2\n\n    @a.third += 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Tuple2 direct item mutability, annotated"))("x = y:\n   a as Number & Number @=\n     1 & 2\n\n   @a.first += 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("functional update"))("a = b: { b with x = 1 }"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($re) => {
  return ({
    freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall)((($core$Core$Cons)("2"))($core$Core$Nil)),
    isMutable: false,
    ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)($re))(false))($re),
  });
}))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($$home$nw$stuff$unstable$src$Types$Pos$T))(($core$Maybe$Just)("a")))((($core$Dict$singleton)("x"))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP instantiate and refine inferred records"))("a = t: { t with x = 1 }\nc = a"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("c")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(((($re) => {
  return ({
    freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall)((($core$Core$Cons)("a"))($core$Core$Nil)),
    isMutable: false,
    ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)($re))(false))($re),
  });
}))(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)($$home$nw$stuff$unstable$src$Types$Pos$T))(($core$Maybe$Just)("a")))((($core$Dict$singleton)("x"))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number)))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] excessive forallness in records"))("x = q:\n a = q.first\n a"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$forall)((($core$Core$Cons)("3"))((($core$Core$Cons)("4"))($core$Core$Nil))),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(((($$home$nw$stuff$unstable$src$Types$CanonicalAst$TypeRecord)(($$home$nw$stuff$unstable$src$Types$Pos$I)(2)))(($core$Maybe$Just)("a")))(($core$Dict$fromList)((($core$Core$Cons)(({
    first: "first",
    second: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("b"),
  })))($core$Core$Nil)))))(false))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("b")),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] refineType when the record has a non-extensible alias"))("alias A = { c as Number, d as Number }\n\nupd as A: A = a:\n  { a with c = .c + 1 }"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("upd")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] infinite recursion on addSubstitution/unify_"))("alias B = { l as [Text] }\n\nreadOne as B: (Text & B) = b:\n    try b.l as\n        []: \"\" & b\n        h :: t: h & { b with l = t }"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("readOne")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] unifyToNonExtensibleRecord correctly substitutes the record extension"))("alias R = { x as Number, y as Number }\n\nrec as R: R =\n    s:\n        if True then\n            { s with y = .y }\n        else\n            rec { s with y = .y }"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("rec")))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)))))))))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$statements = (($$home$nw$stuff$unstable$src$Test$Group)("statements"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Statement blocks should return the last statement's type"))("a =\n  3\n  False"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool,
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Definition statement return type None"))("a =\n  f = x: 3"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$none,
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Definition statement with annotation return type None"))("a as None =\n  f = 3"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("a")))($$home$nw$stuff$unstable$src$Test$isOk)))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP Local values can't shadow root values"))("a = 1\nb as Number =\n    a = 1\n    a"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("b")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("already"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP Prevent local redeclarations"))("b =\n  a = 1\n  a = 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("b")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("declar"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("SKIP Prevent root redeclarations"))("a = 1\na = 1"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("b")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("declar"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] Annotated declarations are actually typechecked"))("x as None =\n    q = 1 + \"\""))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)($core$Core$Nil))))($core$Core$Nil))))))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$try_as = (($$home$nw$stuff$unstable$src$Test$Group)("try..as"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("basic functionality"))("x = q:\n    try q as\n        True: 2\n        _: 3"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: $core$Dict$empty,
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)($$home$nw$stuff$unstable$src$Compiler$CoreTypes$bool))(false))($$home$nw$stuff$unstable$src$Compiler$CoreTypes$number),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("rejects non-matching patterns"))("x = q:\n    try q as\n        True: 2\n        []: 3"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("List"))((($core$Core$Cons)("Bool"))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("rejects non-matching blocks"))("x = q:\n try q as\n   True: 2\n   False: False"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("Number"))((($core$Core$Cons)("Bool"))($core$Core$Nil))))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("[reg] actually infers blocks"))("x as Number =\n  try \"\" as\n    \"\": y"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("x")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("y"))($core$Core$Nil)))))($core$Core$Nil)))));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$variableTypes = (($$home$nw$stuff$unstable$src$Test$Group)("Variable types"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Identity"))("id as a: a =\n  a: a"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("id")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  freeTypeVariables: ($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$ftv)("0a"),
  isMutable: false,
  ty: ((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeFunction)(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("0a")))(false))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$typeVariable)("0a")),
})))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$codeTest)("Annotated vars are instantiated when referenced"))("q as [item] =\n  Core.Nil\n\nr as [Text] =\n      q"))(($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$infer)("r")))($$home$nw$stuff$unstable$src$Test$isOk)))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("TypeCheck"))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$functions))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$statements))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$variableTypes))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$mu))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$higherOrderTypes))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$records))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$patterns))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$try_as))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$if_else))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$nonFunction))($core$Core$Nil)))))))))));

const $$home$nw$stuff$unstable$src$Human$CanonicalAst$normalizeType = (($t) => {
  return ($core$Tuple$first)((($$home$nw$stuff$unstable$src$StateMonad$run)($$home$nw$stuff$unstable$src$Human$CanonicalAst$initNstate))(($$home$nw$stuff$unstable$src$Human$CanonicalAst$normType)($t)));
});

const $$home$nw$stuff$unstable$src$RefHierarchy_Test$valueTest = ($$home$nw$stuff$unstable$src$Test$valueTest)(sp_toHuman);

const $$home$nw$stuff$unstable$src$RefHierarchy_Test$canonicalJsTest = ((($$home$nw$stuff$unstable$src$RefHierarchy_Test$valueTest)("[reg] THIS SHOULD BE IN CANONICALTOJS"))((() => {
  return ((basics_compare)(null))(null);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(0));

const $$home$nw$stuff$unstable$src$RefHierarchy_Test$graph1 = ((() => {
  const $x = (($k) => {
    return (($l) => {
      return ({
        first: $k,
        second: ({
          first: $k,
          second: ($core$Set$fromList)($l),
        }),
      });
    });
  });
  return ($core$Dict$fromList)((($core$Core$Cons)((($x)("a"))((($core$Core$Cons)("b"))((($core$Core$Cons)("d"))($core$Core$Nil)))))((($core$Core$Cons)((($x)("b"))((($core$Core$Cons)("c"))((($core$Core$Cons)("e"))($core$Core$Nil)))))((($core$Core$Cons)((($x)("c"))((($core$Core$Cons)("e"))((($core$Core$Cons)("d"))($core$Core$Nil)))))((($core$Core$Cons)((($x)("d"))($core$Core$Nil)))((($core$Core$Cons)((($x)("e"))($core$Core$Nil)))($core$Core$Nil))))));
}))();

const $$home$nw$stuff$unstable$src$RefHierarchy_Test$graph2 = ((() => {
  const $x = (($k) => {
    return (($l) => {
      return ({
        first: $k,
        second: ({
          first: $k,
          second: ($core$Set$fromList)($l),
        }),
      });
    });
  });
  return ($core$Dict$fromList)((($core$Core$Cons)((($x)("a"))((($core$Core$Cons)("b"))((($core$Core$Cons)("d"))($core$Core$Nil)))))((($core$Core$Cons)((($x)("b"))((($core$Core$Cons)("c"))((($core$Core$Cons)("e"))($core$Core$Nil)))))((($core$Core$Cons)((($x)("c"))((($core$Core$Cons)("e"))((($core$Core$Cons)("d"))($core$Core$Nil)))))((($core$Core$Cons)((($x)("d"))((($core$Core$Cons)("b"))($core$Core$Nil))))((($core$Core$Cons)((($x)("e"))($core$Core$Nil)))($core$Core$Nil))))));
}))();

const $$home$nw$stuff$unstable$src$RefHierarchy_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("RefHierarchy"))((($core$Core$Cons)($$home$nw$stuff$unstable$src$RefHierarchy_Test$canonicalJsTest))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$RefHierarchy_Test$valueTest)("Basic"))((() => {
  return (($$home$nw$stuff$unstable$src$RefHierarchy$reorder)($core$Tuple$second))($$home$nw$stuff$unstable$src$RefHierarchy_Test$graph1);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  first: $core$Core$Nil,
  second: (($core$Core$Cons)("d"))((($core$Core$Cons)("e"))((($core$Core$Cons)("c"))((($core$Core$Cons)("b"))((($core$Core$Cons)("a"))($core$Core$Nil))))),
})))))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$RefHierarchy_Test$valueTest)("Circular"))((() => {
  return (($$home$nw$stuff$unstable$src$RefHierarchy$reorder)($core$Tuple$second))($$home$nw$stuff$unstable$src$RefHierarchy_Test$graph2);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  first: (($core$Core$Cons)((($core$Core$Cons)("b"))((($core$Core$Cons)("d"))((($core$Core$Cons)("c"))($core$Core$Nil)))))($core$Core$Nil),
  second: (($core$Core$Cons)("d"))((($core$Core$Cons)("e"))((($core$Core$Cons)("c"))((($core$Core$Cons)("b"))((($core$Core$Cons)("a"))($core$Core$Nil))))),
})))))($core$Core$Nil))));

const $core$Array_Test$valueTest = ($$home$nw$stuff$unstable$src$Test$valueTest)(sp_toHuman);

const $core$Array_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("Array"))((($core$Core$Cons)(((($core$Array_Test$valueTest)("push"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)("a"))($core$Core$Nil))),
    }),
  });
  ((array_push)($a))("b");
  ((array_push)($a))("c");
  return (array_toList)((sp_clone)(($a.obj)[$a.attr]));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)("a"))((($core$Core$Cons)("b"))((($core$Core$Cons)("c"))($core$Core$Nil)))))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("pop 1"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)("x"))((($core$Core$Cons)("y"))((($core$Core$Cons)("z"))($core$Core$Nil))))),
    }),
  });
  const $b = (array_pop)($a);
  const $c = (array_pop)($a);
  const $l = (array_toList)((sp_clone)(($a.obj)[$a.attr]));
  return ({
    b: $b,
    c: $c,
    l: $l,
  });
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  b: ($core$Maybe$Just)("z"),
  c: ($core$Maybe$Just)("y"),
  l: (($core$Core$Cons)("x"))($core$Core$Nil),
})))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("pop empty"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)($core$Core$Nil)),
    }),
  });
  const $b = (array_pop)($a);
  const $l = (array_toList)((sp_clone)(($a.obj)[$a.attr]));
  return ({
    b: $b,
    l: $l,
  });
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  b: $core$Maybe$Nothing,
  l: $core$Core$Nil,
})))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("get Just"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)("p"))((($core$Core$Cons)("q"))($core$Core$Nil)))),
    }),
  });
  return ((array_get)((sp_clone)(($a.obj)[$a.attr])))(1);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(($core$Maybe$Just)("q")))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("get Nothing"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)("p"))((($core$Core$Cons)("q"))($core$Core$Nil)))),
    }),
  });
  return ((array_get)((sp_clone)(($a.obj)[$a.attr])))(3);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)($core$Maybe$Nothing))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("set success"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)(8))((($core$Core$Cons)(9))($core$Core$Nil)))),
    }),
  });
  const $r = (((array_set)($a))(0))(10);
  const $l = (array_toList)((sp_clone)(($a.obj)[$a.attr]));
  return ({
    l: $l,
    r: $r,
  });
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  l: (($core$Core$Cons)(10))((($core$Core$Cons)(9))($core$Core$Nil)),
  r: true,
})))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("set fail"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)(8))((($core$Core$Cons)(9))($core$Core$Nil)))),
    }),
  });
  const $r = (((array_set)($a))(3))(10);
  const $l = (array_toList)((sp_clone)(($a.obj)[$a.attr]));
  return ({
    l: $l,
    r: $r,
  });
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(({
  l: (($core$Core$Cons)(8))((($core$Core$Cons)(9))($core$Core$Nil)),
  r: false,
})))))((($core$Core$Cons)(((($core$Array_Test$valueTest)("sortBy"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)((($core$Core$Cons)(55))((($core$Core$Cons)(99))((($core$Core$Cons)(22))($core$Core$Nil))))),
    }),
  });
  ((array_sortBy)($a))((($x) => {
    return -($x);
  }));
  return (array_toList)((sp_clone)(($a.obj)[$a.attr]));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(99))((($core$Core$Cons)(55))((($core$Core$Cons)(22))($core$Core$Nil)))))))($core$Core$Nil)))))))));

const $core$Dict_Test$valueTest = ($$home$nw$stuff$unstable$src$Test$valueTest)(sp_toHuman);

const $core$Dict_Test$insertAndGet = (($$home$nw$stuff$unstable$src$Test$Group)("insertAndGet"))((($core$Core$Cons)(((($core$Dict_Test$valueTest)("get, success"))((() => {
  return (($core$Dict$get)(($core$Maybe$Just)("a")))(((($core$Dict$insert)(($core$Maybe$Just)("b")))(2))(((($core$Dict$insert)(($core$Maybe$Just)("a")))(1))($core$Dict$empty)));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(($core$Maybe$Just)(1)))))((($core$Core$Cons)(((($core$Dict_Test$valueTest)("get, fail"))((() => {
  return (($core$Dict$get)(($core$Maybe$Just)("c")))(((($core$Dict$insert)(($core$Maybe$Just)("b")))(2))(((($core$Dict$insert)(($core$Maybe$Just)("a")))(1))($core$Dict$empty)));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)($core$Maybe$Nothing))))($core$Core$Nil)));

const $core$Dict_Test$lists = (($$home$nw$stuff$unstable$src$Test$Group)("lists"))((($core$Core$Cons)(((($core$Dict_Test$valueTest)("keys"))((() => {
  return ((list_sortBy)($core$Basics$identity))(($core$Dict$keys)(((($core$Dict$insert)($core$Maybe$Nothing))(2))(((($core$Dict$insert)(($core$Maybe$Just)("b")))(2))(((($core$Dict$insert)(($core$Maybe$Just)("a")))(1))($core$Dict$empty)))));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(($core$Maybe$Just)("a")))((($core$Core$Cons)(($core$Maybe$Just)("b")))((($core$Core$Cons)($core$Maybe$Nothing))($core$Core$Nil)))))))((($core$Core$Cons)(((($core$Dict_Test$valueTest)("values"))((() => {
  return ((list_sortBy)($core$Basics$identity))(($core$Dict$values)(((($core$Dict$insert)(($core$Maybe$Just)("b")))(({
    a: 3,
  })))(((($core$Dict$insert)(($core$Maybe$Just)("a")))(({
    a: 1,
  })))($core$Dict$empty))));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(({
  a: 1,
})))((($core$Core$Cons)(({
  a: 3,
})))($core$Core$Nil))))))($core$Core$Nil)));

const $core$Dict_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("Dict"))((($core$Core$Cons)($core$Dict_Test$insertAndGet))((($core$Core$Cons)($core$Dict_Test$lists))($core$Core$Nil)));

const $core$Hash$fromList = (($l) => {
  const $h = ({
    attr: "$",
    obj: ({
      $: (sp_clone)(hash_empty),
    }),
  });
  (($core$List$each)($l))((($$k) => {
    const $k = $$k.first;
    const $v = $$k.second;
    return (((hash_insert)($h))($k))($v);
  }));
  return (sp_clone)(($h.obj)[$h.attr]);
});

const $core$Hash_Test$valueTest = ($$home$nw$stuff$unstable$src$Test$valueTest)(sp_toHuman);

const $core$Hash_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("Hash"))((($core$Core$Cons)(((($core$Hash_Test$valueTest)("insert"))((() => {
  const $h = ({
    attr: "$",
    obj: ({
      $: (sp_clone)(($core$Hash$fromList)((($core$Core$Cons)(({
        first: 1,
        second: 2,
      })))($core$Core$Nil))),
    }),
  });
  (((hash_insert)($h))(2))(3);
  return (sp_clone)(($h.obj)[$h.attr]);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(($core$Hash$fromList)((($core$Core$Cons)(({
  first: 1,
  second: 2,
})))((($core$Core$Cons)(({
  first: 2,
  second: 3,
})))($core$Core$Nil)))))))((($core$Core$Cons)(((($core$Hash_Test$valueTest)("remove"))((() => {
  const $h = ({
    attr: "$",
    obj: ({
      $: (sp_clone)(($core$Hash$fromList)((($core$Core$Cons)(({
        first: 1,
        second: 2,
      })))((($core$Core$Cons)(({
        first: 3,
        second: 4,
      })))($core$Core$Nil)))),
    }),
  });
  ((hash_remove)($h))(1);
  return (sp_clone)(($h.obj)[$h.attr]);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(($core$Hash$fromList)((($core$Core$Cons)(({
  first: 3,
  second: 4,
})))($core$Core$Nil))))))((($core$Core$Cons)(((($core$Hash_Test$valueTest)("get Just"))((() => {
  const $h = ($core$Hash$fromList)((($core$Core$Cons)(({
    first: 1,
    second: 2,
  })))((($core$Core$Cons)(({
    first: 3,
    second: 4,
  })))($core$Core$Nil)));
  return ((hash_get)($h))(1);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)(($core$Maybe$Just)(2)))))((($core$Core$Cons)(((($core$Hash_Test$valueTest)("get Nothing"))((() => {
  const $h = ($core$Hash$fromList)((($core$Core$Cons)(({
    first: 1,
    second: 2,
  })))((($core$Core$Cons)(({
    first: 3,
    second: 4,
  })))($core$Core$Nil)));
  return ((hash_get)($h))(66);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)($core$Maybe$Nothing))))((($core$Core$Cons)(((($core$Hash_Test$valueTest)("for"))((() => {
  return (((hash_for)(($core$Hash$fromList)((($core$Core$Cons)(({
    first: ($core$Maybe$Just)(true),
    second: 2,
  })))((($core$Core$Cons)(({
    first: $core$Maybe$Nothing,
    second: 4,
  })))($core$Core$Nil)))))((($k) => {
    return (($v) => {
      return (($a) => {
        return ((list_sortBy)($core$Tuple$first))(((sp_cons)($a))(({
          first: $v,
          second: $k,
        })));
      });
    });
  })))($core$Core$Nil);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(({
  first: 2,
  second: ($core$Maybe$Just)(true),
})))((($core$Core$Cons)(({
  first: 4,
  second: $core$Maybe$Nothing,
})))($core$Core$Nil))))))((($core$Core$Cons)(((($core$Hash_Test$valueTest)("each"))((() => {
  const $a = ({
    attr: "$",
    obj: ({
      $: (sp_clone)((array_fromList)($core$Core$Nil)),
    }),
  });
  ((hash_each)(($core$Hash$fromList)((($core$Core$Cons)(({
    first: ($core$Maybe$Just)(true),
    second: 2,
  })))((($core$Core$Cons)(({
    first: $core$Maybe$Nothing,
    second: 1,
  })))($core$Core$Nil)))))((($k) => {
    return (($v) => {
      return (($core$List$each)((($core$List$range)(1))($v)))((() => {
        return ((array_push)($a))($k);
      }));
    });
  }));
  ((array_sortBy)($a))($core$Basics$identity);
  return (sp_clone)(($a.obj)[$a.attr]);
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((array_fromList)((($core$Core$Cons)(($core$Maybe$Just)(true)))((($core$Core$Cons)(($core$Maybe$Just)(true)))((($core$Core$Cons)($core$Maybe$Nothing))($core$Core$Nil))))))))($core$Core$Nil)))))));

const $core$List_Test$valueTest = ($$home$nw$stuff$unstable$src$Test$valueTest)(sp_toHuman);

const $core$List_Test$concat = (($$home$nw$stuff$unstable$src$Test$Group)("concat"))((($core$Core$Cons)(((($core$List_Test$valueTest)("concats two lists"))((() => {
  return ($core$List$concat)((($core$Core$Cons)((($core$Core$Cons)(1))((($core$Core$Cons)(2))($core$Core$Nil))))((($core$Core$Cons)((($core$Core$Cons)(3))((($core$Core$Cons)(4))($core$Core$Nil))))($core$Core$Nil)));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(1))((($core$Core$Cons)(2))((($core$Core$Cons)(3))((($core$Core$Cons)(4))($core$Core$Nil))))))))($core$Core$Nil));

const $core$List_Test$sortBy = (($$home$nw$stuff$unstable$src$Test$Group)("sortBy"))((($core$Core$Cons)(((($core$List_Test$valueTest)("Can actually sort stuff"))((() => {
  return ((list_sortBy)($core$Basics$identity))((($core$Core$Cons)(($core$Maybe$Just)(23)))((($core$Core$Cons)($core$Maybe$Nothing))((($core$Core$Cons)(($core$Maybe$Just)(11)))($core$Core$Nil))));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(($core$Maybe$Just)(11)))((($core$Core$Cons)(($core$Maybe$Just)(23)))((($core$Core$Cons)($core$Maybe$Nothing))($core$Core$Nil)))))))((($core$Core$Cons)(((($core$List_Test$valueTest)("Correctly orders tuple-2"))((() => {
  return ((list_sortBy)($core$Basics$identity))((($core$Core$Cons)(({
    first: 23,
    second: 1,
  })))((($core$Core$Cons)(({
    first: 1,
    second: 2,
  })))((($core$Core$Cons)(({
    first: 11,
    second: 3,
  })))($core$Core$Nil))));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(({
  first: 1,
  second: 2,
})))((($core$Core$Cons)(({
  first: 11,
  second: 3,
})))((($core$Core$Cons)(({
  first: 23,
  second: 1,
})))($core$Core$Nil)))))))((($core$Core$Cons)(((($core$List_Test$valueTest)("Correctly orders tuple-3"))((() => {
  return ((list_sortBy)($core$Basics$identity))((($core$Core$Cons)(({
    first: "z",
    second: "a",
    third: "2",
  })))((($core$Core$Cons)(({
    first: "a",
    second: "b",
    third: "33",
  })))((($core$Core$Cons)(({
    first: "z",
    second: "a",
    third: "1",
  })))((($core$Core$Cons)(({
    first: "z",
    second: "b",
    third: "3",
  })))($core$Core$Nil)))));
})))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)((($core$Core$Cons)(({
  first: "a",
  second: "b",
  third: "33",
})))((($core$Core$Cons)(({
  first: "z",
  second: "a",
  third: "1",
})))((($core$Core$Cons)(({
  first: "z",
  second: "a",
  third: "2",
})))((($core$Core$Cons)(({
  first: "z",
  second: "b",
  third: "3",
})))($core$Core$Nil))))))))($core$Core$Nil))));

const $core$List_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("List"))((($core$Core$Cons)($core$List_Test$sortBy))((($core$Core$Cons)($core$List_Test$concat))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Main$allTests = (($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Lexer_Test$tests))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$Parser_Test$tests))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$MakeCanonical_Test$tests))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Compiler$TypeCheck_Test$tests))((($core$Core$Cons)($core$Hash_Test$tests))((($core$Core$Cons)($core$Array_Test$tests))((($core$Core$Cons)($core$List_Test$tests))((($core$Core$Cons)($core$Dict_Test$tests))((($core$Core$Cons)($$home$nw$stuff$unstable$src$RefHierarchy_Test$tests))($core$Core$Nil)))))))));

const $$home$nw$stuff$unstable$src$Platforms$Posix$header = "#!/usr/bin/env -S node --stack-size=65500\n\n//Error.stackTraceLimit = 100;\n\nconst { performance } = require('perf_hooks');\n\n";

const $$home$nw$stuff$unstable$src$Platforms$Posix$posixRuntime = "\n//\n// Platform: IO\n//\nconst fs = require('fs');\nconst path = require('path');\n\nconst io_wrap = (f) => [ \"IO.IO\", f ];\n\nconst io_parallel = (iosAsList) => io_wrap((never) => {\n    // as [IO a]: IO [a]\n\n    const ios = array_fromList(iosAsList);\n\n    // TODO actually run them in parallel!\n\n    let arr = [];\n    for (let io of ios) {\n        const r = io[1](never);\n        if (r[0] === \"Ok\")\n            arr.push(r[1]);\n        else\n            return $core$Result$Err(r[1]);\n    }\n\n    return $core$Result$Ok(array_toList(arr));\n});\n\n\nconst io_readDir = (dirPath) => io_wrap((never) => {\n    // as Text: IO [Bool & Text]\n\n    var entries;\n    try {\n        entries = fs.readdirSync(dirPath, { withFileTypes: true });\n    } catch (e) {\n        return $core$Result$Err(e.message);\n    }\n\n    return $core$Result$Ok(array_toList(entries.map((dirent) => ({\n        first: dirent.isDirectory(),\n        second: dirent.name,\n    }))));\n});\n\n\nconst io_readFile = (path) => io_wrap((never) => {\n    // as Text: IO Text\n\n    var content;\n    try {\n        content = fs.readFileSync(path, 'utf8');\n    } catch (e) {\n        return $core$Result$Err(e.message);\n    }\n\n    return $core$Result$Ok(content);\n});\n\n\nconst io_writeFile = (path) => (content) => io_wrap((never) => {\n    // as Text: Text: IO None\n\n    try {\n        fs.writeFileSync(path, content);\n    } catch (e) {\n        return $core$Result$Err(e.message);\n    }\n\n    return $core$Result$Ok(null);\n});\n\n\nconst io_writeStdout = (content) => io_wrap((never) => {\n    // as Text: IO None\n\n    console.info(content);\n    return $core$Result$Ok(null);\n});\n\n\nconst path_resolve = (p) => path.resolve(...array_fromList(p));\n\n\nconst path_dirname = path.dirname;\n";

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$constructorArgumentName = (($i) => {
  return ("$" + (text_fromNumber)($i));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$literalString = (($str) => {
  const $escaped = ((($core$Text$replace)("\""))("\\\""))(((($core$Text$replace)("\n"))("\\n"))($str));
  return ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)(("\"" + ($escaped + "\"")));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateConstructor = (($$caCons) => {
  const $usr = $$caCons.first;
  const $caCons = $$caCons.second;
  const $$slug = $usr;
  const $slug = ($$slug)[2];
  const $umr = ($$slug)[1];
  const $usrAsText = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr)($usr);
  const $argNames = (($core$List$indexedMap)((($index) => {
    return (($name) => {
      return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$constructorArgumentName)(($index + 1));
    });
  })))($caCons.args);
  const $arrayHead = ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$literalString)($slug);
  const $arrayTail = (($core$List$map)($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var))($argNames);
  const $array = ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Array)(((sp_cons)($arrayTail))($arrayHead));
  return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Define)($usrAsText))(((($core$List$forReversed)($argNames))((($argName) => {
    return (($expr) => {
      return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda)((($core$Core$Cons)($argName))($core$Core$Nil)))($expr);
    });
  })))($array));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrs = (($attrPath) => {
  return (($e) => {
    return ((($core$List$for)($attrPath))($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithDot))($e);
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrsButTheLast = (($attrHead) => {
  return (($attrTail) => {
    return (($e) => {
      const $fold = (($attr) => {
        return (($$expr) => {
          const $expr = $$expr.first;
          const $last = $$expr.second;
          return ({
            first: (($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithDot)($last))($expr),
            second: $attr,
          });
        });
      });
      return ((($core$List$for)($attrTail))($fold))(({
        first: $e,
        second: $attrHead,
      }));
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessArrayIndex = (($index) => {
  return ($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithBrackets)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)((text_fromNumber)($index)));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$clone = (($expr) => {
  return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("sp_clone")))((($core$Core$Cons)($expr))($core$Core$Nil));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$maybeCloneMutable = (($mutability) => {
  return (($expr) => {
    return ((($mutability)[0] === "Immutable")
      ? $expr
      : ((($mutability)[0] === "Mutable")
        ? ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$clone)($expr)
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 339:4', (sp_toHuman)($mutability))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unwrapMutable = (($ja_x) => {
  return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithBrackets)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithDot)("attr"))($ja_x)))((($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithDot)("obj"))($ja_x));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$wrapCalls = (($env) => {
  return (($exprAndMutability) => {
    return (($baseExpr) => {
      return ((($core$List$for)($exprAndMutability))((($arg) => {
        return (($accum) => {
          return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)($accum))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
            nativeBinop: false,
          })))($env))($arg)))($core$Core$Nil));
        });
      })))($baseExpr);
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateVariable = (($env) => {
  return (($valueName) => {
    return (($attrPath) => {
      return (($eaArgs) => {
        const $$try1 = (($core$Dict$get)($valueName))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$overrides)(null));
        return ((($$try1)[0] === "Just")
          ? ((() => {
            const $override = ($$try1)[1];
            return (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrs)($attrPath))((($override)($env))($eaArgs));
          }))()
          : ((($$try1)[0] === "Nothing")
            ? ((($core$Set$member)($valueName))($env.mutables)
              ? ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$clone)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrs)($attrPath))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unwrapMutable)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($valueName))))
              : ((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$wrapCalls)($env))($eaArgs))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrs)($attrPath))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($valueName))))
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 190:4', (sp_toHuman)($$try1))));
      });
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$wrapMutable = (($mutability) => {
  return (($expr) => {
    return ((($mutability)[0] === "Mutable")
      ? ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Record)(((($core$Dict$insert)("obj"))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Record)((($core$Dict$singleton)("$"))($expr))))(((($core$Dict$insert)("attr"))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$literalString)("$")))($core$Dict$empty)))
      : ((($mutability)[0] === "Immutable")
        ? $expr
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 325:4', (sp_toHuman)($mutability))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpression = (($env) => {
  return (($eaExpression) => {
    return (((($eaExpression)[0] === "Call") && (((($eaExpression)[1])[0] === "Call") && (((($eaExpression)[1])[1])[0] === "Variable")))
      ? ((() => {
        const $name = ((($eaExpression)[1])[1])[1];
        const $attrPath = ((($eaExpression)[1])[1])[2];
        const $argAndMut1 = (($eaExpression)[1])[2];
        const $argAndMut2 = ($eaExpression)[2];
        return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateVariable)($env))($name))($attrPath))((($core$Core$Cons)($argAndMut1))((($core$Core$Cons)($argAndMut2))($core$Core$Nil))));
      }))()
      : (((($eaExpression)[0] === "Call") && ((($eaExpression)[1])[0] === "Variable"))
        ? ((() => {
          const $name = (($eaExpression)[1])[1];
          const $attrPath = (($eaExpression)[1])[2];
          const $argAndMut = ($eaExpression)[2];
          return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateVariable)($env))($name))($attrPath))((($core$Core$Cons)($argAndMut))($core$Core$Nil)));
        }))()
        : ((($eaExpression)[0] === "Variable")
          ? ((() => {
            const $name = ($eaExpression)[1];
            const $attrPath = ($eaExpression)[2];
            return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateVariable)($env))($name))($attrPath))($core$Core$Nil));
          }))()
          : ((($eaExpression)[0] === "Call")
            ? ((() => {
              const $ref = ($eaExpression)[1];
              const $arg = ($eaExpression)[2];
              return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($ref)))((($core$Core$Cons)(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
                nativeBinop: false,
              })))($env))($arg)))($core$Core$Nil)));
            }))()
            : ((($eaExpression)[0] === "LetIn")
              ? ((() => {
                const $inExpression = ($eaExpression)[1].inExpression;
                const $letExpression = ($eaExpression)[1].letExpression;
                const $maybeName = ($eaExpression)[1].maybeName;
                const $mutability = ($eaExpression)[1].mutability;
                const $localEnv = ((() => {
                  const $$try6 = ({
                    first: $maybeName,
                    second: $mutability,
                  });
                  return (((($$try6.first)[0] === "Just") && (($$try6.second)[0] === "Mutable"))
                    ? ((() => {
                      const $name = ($$try6.first)[1];
                      return (Object.assign)({}, $env, ({
                        mutables: (($core$Set$insert)($name))($env.mutables),
                      }));
                    }))()
                    : (true
                      ? $env
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 468:16', (sp_toHuman)($$try6))));
                }))();
                const $inStatements = ((() => {
                  const $$try5 = (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpression)($localEnv))($inExpression);
                  return ((($$try5)[0] === "Block")
                    ? ((() => {
                      const $stats = ($$try5)[1];
                      return $stats;
                    }))()
                    : ((($$try5)[0] === "Inline")
                      ? ((() => {
                        const $jaExpression = ($$try5)[1];
                        return (($core$Core$Cons)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Return)($jaExpression)))($core$Core$Nil);
                      }))()
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 475:16', (sp_toHuman)($$try5))));
                }))();
                return ((($maybeName)[0] === "Nothing")
                  ? ((() => {
                    const $$try4 = (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpression)($env))($letExpression);
                    return ((($$try4)[0] === "Inline")
                      ? ((() => {
                        const $expr = ($$try4)[1];
                        return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Block)(((sp_cons)($inStatements))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Eval)($expr)));
                      }))()
                      : ((($$try4)[0] === "Block")
                        ? ((() => {
                          const $stats = ($$try4)[1];
                          return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Block)(($core$List$concat)((($core$Core$Cons)($stats))((($core$Core$Cons)($inStatements))($core$Core$Nil))));
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 481:20', (sp_toHuman)($$try4))));
                  }))()
                  : ((($maybeName)[0] === "Just")
                    ? ((() => {
                      const $name = ($maybeName)[1];
                      const $letStatement = (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Define)($name))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$wrapMutable)($mutability))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$maybeCloneMutable)($mutability))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($letExpression))));
                      return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Block)(((sp_cons)($inStatements))($letStatement));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 479:12', (sp_toHuman)($maybeName))));
              }))()
              : ((($eaExpression)[0] === "Lambda")
                ? ((() => {
                  const $maybeName = ($eaExpression)[1].first;
                  const $mutability = ($eaExpression)[1].second;
                  const $body = ($eaExpression)[2];
                  const $args = ((($maybeName)[0] === "Just")
                    ? ((() => {
                      const $name = ($maybeName)[1];
                      return (($core$Core$Cons)($name))($core$Core$Nil);
                    }))()
                    : ((($maybeName)[0] === "Nothing")
                      ? $core$Core$Nil
                      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 502:16', (sp_toHuman)($maybeName))));
                  const $localEnv = ((() => {
                    const $$try3 = ({
                      first: $maybeName,
                      second: $mutability,
                    });
                    return (((($$try3.first)[0] === "Just") && (($$try3.second)[0] === "Mutable"))
                      ? ((() => {
                        const $name = ($$try3.first)[1];
                        return (Object.assign)({}, $env, ({
                          mutables: (($core$Set$insert)($name))($env.mutables),
                        }));
                      }))()
                      : (true
                        ? $env
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 508:16', (sp_toHuman)($$try3))));
                  }))();
                  const $statements = ((() => {
                    const $$try2 = (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpression)($localEnv))($body);
                    return ((($$try2)[0] === "Inline")
                      ? ((() => {
                        const $expr = ($$try2)[1];
                        return (($core$Core$Cons)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Return)($expr)))($core$Core$Nil);
                      }))()
                      : ((($$try2)[0] === "Block")
                        ? ((() => {
                          const $block = ($$try2)[1];
                          return $block;
                        }))()
                        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 515:16', (sp_toHuman)($$try2))));
                  }))();
                  return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$BlockLambda)($args))($statements));
                }))()
                : ((($eaExpression)[0] === "LiteralText")
                  ? ((() => {
                    const $string = ($eaExpression)[1];
                    return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$literalString)($string));
                  }))()
                  : ((($eaExpression)[0] === "LiteralNumber")
                    ? ((() => {
                      const $num = ($eaExpression)[1];
                      return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)((text_fromNumber)($num)));
                    }))()
                    : ((($eaExpression)[0] === "Conditional")
                      ? ((() => {
                        const $test = ($eaExpression)[1];
                        const $true = ($eaExpression)[2];
                        const $false = ($eaExpression)[3];
                        return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Conditional)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($test)))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($true)))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($false)));
                      }))()
                      : ((($eaExpression)[0] === "And")
                        ? ((() => {
                          const $eaTests = ($eaExpression)[1];
                          const $jaTests = (($core$List$map)(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env)))($eaTests);
                          const $$try1 = ($core$List$reverse)($jaTests);
                          return ((($$try1)[0] === "Nil")
                            ? ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)("true"))
                            : ((($$try1)[0] === "Cons")
                              ? ((() => {
                                const $head = ($$try1)[1];
                                const $tail = ($$try1)[2];
                                return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(((($core$List$for)($tail))((($test) => {
                                  return (($expr) => {
                                    return ((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop)("&&"))($test))($expr);
                                  });
                                })))($head));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 542:12', (sp_toHuman)($$try1))));
                        }))()
                        : ((($eaExpression)[0] === "ShallowEqual")
                          ? ((() => {
                            const $a = ($eaExpression)[1];
                            const $b = ($eaExpression)[2];
                            return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop)("==="))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($a)))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($b)));
                          }))()
                          : ((($eaExpression)[0] === "LiteralArray")
                            ? ((() => {
                              const $items = ($eaExpression)[1];
                              return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Array)((($core$List$map)(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env)))($items)));
                            }))()
                            : ((($eaExpression)[0] === "ArrayAccess")
                              ? ((() => {
                                const $index = ($eaExpression)[1];
                                const $array = ($eaExpression)[2];
                                return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessArrayIndex)($index))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($array)));
                              }))()
                              : ((($eaExpression)[0] === "Constructor")
                                ? ((() => {
                                  const $name = ($eaExpression)[1];
                                  return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateVariable)($env))($name))($core$Core$Nil))($core$Core$Nil));
                                }))()
                                : ((($eaExpression)[0] === "ConstructorAccess")
                                  ? ((() => {
                                    const $argIndex = ($eaExpression)[1];
                                    const $value = ($eaExpression)[2];
                                    return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessArrayIndex)(($argIndex + 1)))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($value)));
                                  }))()
                                  : ((($eaExpression)[0] === "IsConstructor")
                                    ? ((() => {
                                      const $name = ($eaExpression)[1];
                                      const $eaValue = ($eaExpression)[2];
                                      const $jaValue = (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($eaValue);
                                      const $jaExpr = (("True" === $name)
                                        ? $jaValue
                                        : (("False" === $name)
                                          ? (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Unop)("!"))($jaValue)
                                          : (true
                                            ? ((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop)("==="))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessArrayIndex)(0))($jaValue)))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$literalString)($name))
                                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 584:16', (sp_toHuman)($name)))));
                                      return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)($jaExpr);
                                    }))()
                                    : ((($eaExpression)[0] === "LiteralRecord")
                                      ? ((() => {
                                        const $maybeExtend = ($eaExpression)[1];
                                        const $attrNamesAndValues = ($eaExpression)[2];
                                        const $obj = ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Record)(((($core$List$for)($attrNamesAndValues))((($$name) => {
                                          const $name = $$name.first;
                                          const $value = $$name.second;
                                          return (($core$Dict$insert)($name))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($value));
                                        })))($core$Dict$empty));
                                        return ((($maybeExtend)[0] === "Nothing")
                                          ? ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)($obj)
                                          : ((($maybeExtend)[0] === "Just")
                                            ? ((() => {
                                              const $extend = ($maybeExtend)[1];
                                              return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("Object.assign")))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Record)($core$Dict$empty)))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($extend)))((($core$Core$Cons)($obj))($core$Core$Nil)))));
                                            }))()
                                            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 604:12', (sp_toHuman)($maybeExtend))));
                                      }))()
                                      : ((($eaExpression)[0] === "RecordAccess")
                                        ? ((() => {
                                          const $attrName = ($eaExpression)[1];
                                          const $value = ($eaExpression)[2];
                                          return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$AccessWithDot)($attrName))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($value)));
                                        }))()
                                        : ((($eaExpression)[0] === "MissingPattern")
                                          ? ((() => {
                                            const $pos = ($eaExpression)[1];
                                            const $value = ($eaExpression)[2];
                                            const $human = (($$home$nw$stuff$unstable$src$Compiler$Error$posToHuman)($env.errorEnv))($pos);
                                            return ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$Inline)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)("sp_throw")))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)("'Missing pattern in try..as'")))((($core$Core$Cons)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)(("'" + ($human.location + "'")))))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Literal)("sp_toHuman")))((($core$Core$Cons)((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($value)))($core$Core$Nil))))($core$Core$Nil)))));
                                          }))()
                                          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 447:4', (sp_toHuman)($eaExpression)))))))))))))))))))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression = (($env) => {
  return (($expr) => {
    const $$try1 = (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpression)($env))($expr);
    return ((($$try1)[0] === "Inline")
      ? ((() => {
        const $e = ($$try1)[1];
        return $e;
      }))()
      : ((($$try1)[0] === "Block")
        ? ((() => {
          const $block = ($$try1)[1];
          return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Call)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$BlockLambda)($core$Core$Nil))($block)))($core$Core$Nil);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 436:4', (sp_toHuman)($$try1))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg = (($stuff) => {
  return (($env) => {
    return (($$eaExpression) => {
      const $eaExpression = $$eaExpression.first;
      const $mutability = $$eaExpression.second;
      return ((($mutability)[0] === "Immutable")
        ? (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($eaExpression)
        : ((($mutability)[0] === "Mutable")
          ? ((($eaExpression)[0] === "Variable")
            ? ((() => {
              const $name = ($eaExpression)[1];
              const $attrPath = ($eaExpression)[2];
              return ($stuff.nativeBinop
                ? (($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrs)($attrPath))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unwrapMutable)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($name)))
                : ((($attrPath)[0] === "Nil")
                  ? ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($name)
                  : ((($attrPath)[0] === "Cons")
                    ? ((() => {
                      const $head = ($attrPath)[1];
                      const $tail = ($attrPath)[2];
                      return ((($$lastAttrName) => {
                        const $wrappedExpr = $$lastAttrName.first;
                        const $lastAttrName = $$lastAttrName.second;
                        return ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Record)(((($core$Dict$insert)("attr"))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$literalString)($lastAttrName)))(((($core$Dict$insert)("obj"))($wrappedExpr))($core$Dict$empty)));
                      }))(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$accessAttrsButTheLast)($head))($tail))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unwrapMutable)(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($name))));
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 255:24', (sp_toHuman)($attrPath)))));
            }))()
            : (true
              ? (sp_todo)("translateArg: this should not happen!")
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 245:12', (sp_toHuman)($eaExpression))))
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 240:4', (sp_toHuman)($mutability))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop = (($jsOp) => {
  return (($env) => {
    return (($arguments) => {
      return (((($arguments)[0] === "Cons") && (((($arguments)[2])[0] === "Cons") && (((($arguments)[2])[2])[0] === "Nil")))
        ? ((() => {
          const $right = ($arguments)[1];
          const $left = (($arguments)[2])[1];
          return ((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop)($jsOp))(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
            nativeBinop: true,
          })))($env))($left)))(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
            nativeBinop: true,
          })))($env))($right));
        }))()
        : (((($arguments)[0] === "Cons") && ((($arguments)[2])[0] === "Nil"))
          ? ((() => {
            const $right = ($arguments)[1];
            return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda)((($core$Core$Cons)("a"))($core$Core$Nil)))(((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop)($jsOp))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("a")))(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
              nativeBinop: true,
            })))($env))($right)));
          }))()
          : ((($arguments)[0] === "Nil")
            ? (($$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda)((($core$Core$Cons)("b"))($core$Core$Nil)))((($$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda)((($core$Core$Cons)("a"))($core$Core$Nil)))(((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Binop)($jsOp))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("a")))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("b"))))
            : (true
              ? (sp_todo)("compiler bug: wrong number of arguments for binop")
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 150:4', (sp_toHuman)($arguments))))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$constructor = (($jsValue) => {
  return (($env) => {
    return (($arguments) => {
      return ($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($jsValue);
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function = (($jaName) => {
  return (($env) => {
    return (($arguments) => {
      return ((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$wrapCalls)($env))($arguments))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)($jaName));
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unaryMinus = (($env) => {
  return (($arguments) => {
    return (((($arguments)[0] === "Cons") && ((($arguments)[2])[0] === "Nil"))
      ? ((() => {
        const $arg = ($arguments)[1];
        return (($$home$nw$stuff$unstable$src$Types$JavascriptAst$Unop)("-"))(((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
          nativeBinop: false,
        })))($env))($arg));
      }))()
      : ((($arguments)[0] === "Nil")
        ? (($$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda)((($core$Core$Cons)("v"))($core$Core$Nil)))((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Unop)("-"))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("v")))
        : (true
          ? (sp_todo)("compiler bug: wrong number of arguments for binop")
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 132:4', (sp_toHuman)($arguments)))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unaryPlus = (($env) => {
  return (($arguments) => {
    return (((($arguments)[0] === "Cons") && ((($arguments)[2])[0] === "Nil"))
      ? ((() => {
        const $arg = ($arguments)[1];
        return ((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateArg)(({
          nativeBinop: false,
        })))($env))($arg);
      }))()
      : ((($arguments)[0] === "Nil")
        ? (($$home$nw$stuff$unstable$src$Types$JavascriptAst$SimpleLambda)((($core$Core$Cons)("a"))($core$Core$Nil)))(($$home$nw$stuff$unstable$src$Types$JavascriptAst$Var)("a"))
        : (true
          ? (sp_todo)("compiler bug: wrong number of arguments for binop")
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 116:4', (sp_toHuman)($arguments)))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$overrides = (() => {
  const $corelib = (($m) => {
    return (($n) => {
      return (($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Core))($m)))($n);
    });
  });
  const $ioModule = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Posix))("IO"));
  const $pathModule = ($$home$nw$stuff$unstable$src$Types$Meta$USR)((($$home$nw$stuff$unstable$src$Types$Meta$UMR)($$home$nw$stuff$unstable$src$Types$Meta$Posix))("Path"));
  return (($core$Dict$mapKeys)($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr))(($core$Dict$fromList)((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$unaryPlus.usr,
    second: $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unaryPlus,
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$unaryMinus.usr,
    second: $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$unaryMinus,
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$add.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("+"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$multiply.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("*"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$subtract.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("-"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$mutableAssign.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("="),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$mutableAdd.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("+="),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$mutableSubtract.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("-="),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$textConcat.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("+"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$greaterThan.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)(">"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$lesserThan.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("<"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$greaterOrEqualThan.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)(">="),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$lesserOrEqualThan.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("<="),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$or_.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("||"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$and_.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$binop)("&&"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$true,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$constructor)("true"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$false,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$constructor)("false"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Compiler$CoreTypes$noneValue,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$constructor)("null"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$divide.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_divide"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$listCons.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_cons"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$equal.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_equal"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$notEqual.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_not_equal"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Basics"))("modBy"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("basics_modBy"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$debugLog.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_log"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$debugTodo.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_todo"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$debugToHuman.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_toHuman"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$debugBenchStart.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_benchStart"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$debugBenchStop.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("sp_benchStop"),
  })))((($core$Core$Cons)(({
    first: $$home$nw$stuff$unstable$src$Prelude$compare.usr,
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("basics_compare"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("fromNumber"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_fromNumber"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("toNumber"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_toNumber"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("split"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_split"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("length"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_length"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("slice"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_slice"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("startsWith"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_startsWith"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("startsWithRegex"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_startsWithRegex"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("replaceRegex"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_replaceRegex"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("trimLeft"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_trimLeft"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("dropLeft"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_dropLeft"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Text"))("forEach"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("text_forEach"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Hash"))("empty"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("hash_empty"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Hash"))("insert"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("hash_insert"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Hash"))("remove"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("hash_remove"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Hash"))("get"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("hash_get"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Hash"))("for"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("hash_for"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Hash"))("each"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("hash_each"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("push"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_push"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("pop"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_pop"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("get"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_get"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("set"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_set"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("sortBy"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_sortBy"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("fromList"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_fromList"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("Array"))("toList"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("array_toList"),
  })))((($core$Core$Cons)(({
    first: (($corelib)("List"))("sortBy"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("list_sortBy"),
  })))((($core$Core$Cons)(({
    first: ($ioModule)("parallel"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("io_parallel"),
  })))((($core$Core$Cons)(({
    first: ($ioModule)("readDir"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("io_readDir"),
  })))((($core$Core$Cons)(({
    first: ($ioModule)("readFile"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("io_readFile"),
  })))((($core$Core$Cons)(({
    first: ($ioModule)("writeFile"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("io_writeFile"),
  })))((($core$Core$Cons)(({
    first: ($ioModule)("writeStdout"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("io_writeStdout"),
  })))((($core$Core$Cons)(({
    first: ($pathModule)("dirname"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("path_dirname"),
  })))((($core$Core$Cons)(({
    first: ($pathModule)("resolve"),
    second: ($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$function)("path_resolve"),
  })))($core$Core$Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateDef = (($env) => {
  return (($def) => {
    const $$try1 = (($core$Dict$get)($def.name))(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$overrides)(null));
    return ((($$try1)[0] === "Just")
      ? $core$Maybe$Nothing
      : ((($$try1)[0] === "Nothing")
        ? ($core$Maybe$Just)((($$home$nw$stuff$unstable$src$Types$JavascriptAst$Define)($def.name))((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateExpressionToExpression)($env))($def.expr)))
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/EmittableToJs.sp 665:4', (sp_toHuman)($$try1))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateAll = (($errorEnv) => {
  return (($caConstructors) => {
    return (($eaDefs) => {
      const $jaConstructors = (($core$List$map)($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateConstructor))($caConstructors);
      const $env = ({
        errorEnv: $errorEnv,
        mutables: $core$Set$empty,
        tryCounter: 0,
      });
      const $jaStatements = (($core$List$filterMap)(($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateDef)($env)))($eaDefs);
      return ($core$List$concat)((($core$Core$Cons)($jaConstructors))((($core$Core$Cons)($jaStatements))($core$Core$Nil)));
    });
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id = (($level) => {
  return (($core$Text$repeat)($level))("  ");
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitBlock = (($l) => {
  return (($block) => {
    const $lines = (($core$Text$join)("\n"))((($core$List$map)(($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitStatement)(($l + 1))))($block));
    return ("{\n" + ($lines + ("\n" + (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)($l) + "}"))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr = (($l) => {
  return (($expression) => {
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
            return ("(" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($ref) + (")(" + ((($core$Text$join)(", "))((($core$List$map)(($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l)))($args)) + ")"))));
          }))()
          : ((($expression)[0] === "Unop")
            ? ((() => {
              const $op = ($expression)[1];
              const $left = ($expression)[2];
              return ($op + ("(" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($left) + ")")));
            }))()
            : ((($expression)[0] === "Binop")
              ? ((() => {
                const $op = ($expression)[1];
                const $left = ($expression)[2];
                const $right = ($expression)[3];
                return ("(" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($left) + (" " + ($op + (" " + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($right) + ")"))))));
              }))()
              : ((($expression)[0] === "Mutop")
                ? ((() => {
                  const $op = ($expression)[1];
                  const $yield = ($expression)[2];
                  const $left = ($expression)[3];
                  const $right = ($expression)[4];
                  return ("(" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($left) + (" " + ($op + (" " + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($right) + (", " + ($yield + ")"))))))));
                }))()
                : ((($expression)[0] === "SimpleLambda")
                  ? ((() => {
                    const $params = ($expression)[1];
                    const $expr = ($expression)[2];
                    return ("((" + ((($core$Text$join)(", "))($params) + (") => " + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($expr) + ")"))));
                  }))()
                  : ((($expression)[0] === "BlockLambda")
                    ? ((() => {
                      const $params = ($expression)[1];
                      const $stats = ($expression)[2];
                      return ("((" + ((($core$Text$join)(", "))($params) + (") => " + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitBlock)($l))($stats) + ")"))));
                    }))()
                    : ((($expression)[0] === "Record")
                      ? ((() => {
                        const $attrs = ($expression)[1];
                        return (((sp_equal)($core$Dict$empty))($attrs)
                          ? "{}"
                          : ((($a) => {
                            return ("({\n" + ((($core$Text$join)("\n"))($a) + ("\n" + (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)($l) + "})"))));
                          }))((($core$List$map)((($$key) => {
                            const $key = $$key.first;
                            const $value = $$key.second;
                            return (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)(($l + 1)) + ($key + (": " + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)(($l + 1)))($value) + ","))));
                          })))(((list_sortBy)($core$Tuple$first))(($core$Dict$toList)($attrs)))));
                      }))()
                      : ((($expression)[0] === "AccessWithDot")
                        ? ((() => {
                          const $name = ($expression)[1];
                          const $e = ($expression)[2];
                          return ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($e) + ("." + $name));
                        }))()
                        : ((($expression)[0] === "AccessWithBrackets")
                          ? ((() => {
                            const $i = ($expression)[1];
                            const $expr = ($expression)[2];
                            return ("(" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($expr) + (")[" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($i) + "]"))));
                          }))()
                          : ((($expression)[0] === "Conditional")
                            ? ((() => {
                              const $p = ($expression)[1];
                              const $true = ($expression)[2];
                              const $false = ($expression)[3];
                              return (("(" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($p) + "\n")) + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)(($l + 1)) + ("? " + (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)(($l + 1)))($true))) + ("\n" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)(($l + 1)) + (": " + (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)(($l + 1)))($false))) + ")"))));
                            }))()
                            : ((($expression)[0] === "Array")
                              ? ((() => {
                                const $items = ($expression)[1];
                                return (((sp_equal)($core$Core$Nil))($items)
                                  ? "[]"
                                  : ((($a) => {
                                    return ("([\n" + ((($core$Text$join)("\n"))($a) + ("\n" + (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)($l) + "])"))));
                                  }))((($core$List$map)((($i) => {
                                    return (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)(($l + 1)) + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)(($l + 1)))($i) + ","));
                                  })))($items)));
                              }))()
                              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/JsToText.sp 41:4', (sp_toHuman)($expression)))))))))))))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitStatement = (($l) => {
  return (($stat) => {
    const $std = (($mid) => {
      return (($expr) => {
        return (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)($l) + ($mid + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($expr) + ";")));
      });
    });
    return ((($stat)[0] === "Eval")
      ? ((() => {
        const $e = ($stat)[1];
        return (($std)(""))($e);
      }))()
      : ((($stat)[0] === "Return")
        ? ((() => {
          const $e = ($stat)[1];
          return (($std)("return "))($e);
        }))()
        : ((($stat)[0] === "Define")
          ? ((() => {
            const $name = ($stat)[1];
            const $e = ($stat)[2];
            return (($std)(("const " + ($name + " = "))))($e);
          }))()
          : ((($stat)[0] === "If")
            ? ((() => {
              const $condition = ($stat)[1];
              const $block = ($stat)[2];
              return (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$id)($l) + ("if (" + ((($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitExpr)($l))($condition) + (") " + (($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitBlock)($l))($block)))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Targets/Javascript/JsToText.sp 14:4', (sp_toHuman)($stat))))));
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listCons = "Cons";

const $$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listNil = "Nil";

const $$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$nativeDefinitions = ("const sp_clone = (src) => {\n if (Array.isArray(src))\n   return src.map(sp_clone);\n\n if (typeof(src) === 'object') {\n   const dest = {};\n   for (let k in src) { dest[k] = sp_clone(src[k]); }\n   return dest;\n }\n\n return src;\n}\n\n\n/*  HACK\n\n    TODO this is super brittle\n    once we have a proper Platform system in place, the platform can probably\n    use its internal Meta to figure out the proper constructor\n\n*/\nconst maybe_nothing = [ \"Nothing\" ];\nconst maybe_just = (a) => [ \"Just\", a ];\n\n\n\n//\n// Basic ops\n//\n\n\nconst sp_equal = (a) => (b) => {\n  if (a === b)\n    return true\n\n  if (Array.isArray(a)) {\n    if (!Array.isArray(b)) return false;\n\n    const l = a.length;\n    if (l !== b.length) return false;\n\n    let i = 0;\n    while (i < l) {\n      if (!sp_equal(a[i])(b[i])) return false;\n      ++i;\n    }\n\n    return true;\n  }\n\n  if (typeof(a) === 'object') {\n    if (typeof(b) !== 'object') return false;\n\n    const keys = Object.keys(a);\n    const l = keys.length;\n    if (l !== Object.keys(b).length) return false;\n\n    let i = 0;\n    while (i < l) {\n      let k = keys[i];\n      if (!sp_equal(a[k])(b[k])) return false;\n      ++i;\n    }\n\n    return true;\n  }\n\n  return false;\n}\n\n\nconst sp_not_equal = (a) => (b) => {\n  return !sp_equal(a)(b);\n}\n\n\nconst sp_compare = (a, b) => {\n\n  // union type\n  if (Array.isArray(a)) {\n    // compare constructor names\n    if (a[0] > b[0]) return 1;\n    if (b[0] > a[0]) return -1;\n    for (let i = 1; i < a.length; i++) {\n        const cmp = sp_compare(a[i], b[i]);\n        if (cmp) return cmp;\n    }\n    return 0;\n  }\n\n  // None is represented as null\n  if (a === null)\n      return 0;\n\n  if (typeof a === 'object') {\n    const keys = Object.keys(a).sort();\n    for (let k of keys) {\n        const cmp = sp_compare(a[k], b[k]);\n        if (cmp) return cmp;\n    }\n    return 0;\n  }\n\n  if (a > b) return 1;\n  if (a < b) return -1;\n  return 0;\n}\n\nconst sp_divide = (right) => (left) => {\n  if (right === 0) return 0;\n  return left / right;\n}\n\n\nconst basics_modBy = (a) => (b) => b % a;\n\nconst basics_compare = (a) => (b) => sp_compare(a, b);\n\n\n//\n// Debug\n//\n\n\nconst sp_todo = (message) => {\n  throw new Error(\"TODO: \" + message);\n}\n\n\nconst sp_log = (message) => (thing) => {\n  console.log(message, sp_toHuman(thing));\n  return thing;\n}\n\n\nconst sp_throw = function (errorName) {\n    console.error(...arguments);\n    throw new Error(errorName);\n}\n\n\n//\n// Benchmarking\n//\n\n\nvar debug_benchStartTime = null;\nvar debug_benchStartStack = null;\nvar debug_benchEntries = {};\n\n\nconst pad = (l, s) => ' '.repeat(Math.max(0, l - s.length)) + s;\n\n\nconst fmt = (n) => {\n    const s = Math.floor(n) + '';\n    return s.slice(0, -3) + '.' + pad(3, s.slice(-3));\n}\n\n\n// TODO how should benchmark work in a browser?\ntypeof process !== 'undefined' && process.on('beforeExit', (code) => {\n    if (debug_benchStartStack !== null)\n        console.error(`ERROR: a benchmark has been started but not stopped!\nStart was at:${debug_benchStartStack}`);\n\n    const ks = Object.keys(debug_benchEntries);\n    if (ks.length) {\n        console.info(\"\");\n        console.info(\"Benchmark results:\");\n        ks.sort().forEach(k => {\n            const entry = debug_benchEntries[k];\n            console.info(\n                    'TotalTime:', pad(10, fmt(entry.dt )) + 's',\n                    '   ',\n                    'Runs:', pad(6, '' + entry.n),\n                    '   ',\n                    'Key:', k,\n            );\n        });\n    }\n});\n\n\nconst sp_benchStart = (none) => {\n    if (debug_benchStartStack !== null)\n        throw new Error(`\nbenchStart called when a benchmark is already ongoing!\nPrevious benchStart call was ${debug_benchStartStack}\n`);\n\n    debug_benchStartStack = new Error().stack;\n    debug_benchStartTime = performance.now();\n}\n\n\nconst sp_benchStop = (name) => {\n    const now = performance.now();\n\n    if (debug_benchStartStack === null)\n        throw new Error(\"benchStop called while no benchmark is ongoing!\");\n\n    debug_benchStartStack = null;\n\n    const dt = now - debug_benchStartTime;\n\n    const entry = debug_benchEntries[name] || { dt: 0, n: 0 };\n    entry.dt += dt;\n    entry.n += 1;\n    debug_benchEntries[name] = entry;\n}\n\n\n\n\n//\n// To Human\n//\n\n\nconst sp_toHuman = (a) => {\n\n  if (Array.isArray(a))\n    return sp_toHumanAsList([], a) || sp_toHumanAsUnion(a);\n\n  if (typeof a === 'function') {\n    return '<function>';\n  }\n\n  if (typeof a === 'object') {\n    let x = [];\n    for (let i in a) x.push(i + ' = ' + sp_toHuman(a[i]));\n    return '{' + x.join(', ') + '}';\n  }\n\n  return JSON.stringify(a, null, 0);\n}\n\n\nconst sp_toHumanAsUnion = (a) => {\n  return a[0] + ' ' + a.slice(1).map(arg => '(' + sp_toHuman(arg) + ')').join(' ');\n}\n\n\nconst sp_toHumanAsList = (arrayAccum, list) => {\n  if (list[0] === '" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listCons + ("') {\n    arrayAccum.push(sp_toHuman(list[1]));\n    return sp_toHumanAsList(arrayAccum, list[2]);\n  }\n\n  if (list[0] === '" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listNil + ("')\n    return '[' + arrayAccum.join(', ') + ']';\n\n  return false;\n}\n\n\n//\n// Text\n//\n\n\nconst text_fromNumber = (n) => '' + n;\n\nconst text_toNumber = (t) => {\n    const n = +t;\n\n    return isNaN(n) ? maybe_nothing : maybe_just(n);\n}\n\nconst text_split = (separator) => (target) => array_toList(target.split(separator));\n\nconst text_length = (s) => s.length;\n\nconst text_slice = (start) => (end) => (s) => s.slice(start, end);\n\nconst text_startsWith = (sub) => (s) => s.startsWith(sub);\n\nconst text_startsWithRegex = (regex) => {\n  let re;\n  try {\n    re = new RegExp('^' + regex);\n  } catch (e) {\n    return () => \"\"\n  }\n\n  return (s) => {\n    let m = s.match(re);\n    return m ? m[0] : \"\";\n  }\n}\n\nconst text_replaceRegex = (regex) => {\n  let re;\n  try {\n    re = new RegExp(regex, 'g');\n  } catch (e) {\n    return () => () => \"\"\n  }\n\n  return (replacer) => (s) => s.replace(re, replacer);\n}\n\nconst text_trimLeft = (s) => {\n  return s.trimLeft();\n}\n\nconst text_dropLeft = (n) => (s) => {\n  return s.slice(n);\n}\n\nconst text_forEach = (s) => (f) => {\n  for (let i of s) f(i);\n  return null;\n}\n\n\n//\n// Hashes\n//\n\nconst hash_empty = {};\n\n\nconst hash_insert = (hash) => (key) => (value) => {\n    const h = hash.obj[hash.attr];\n    h[JSON.stringify(key)] = [key, value];\n    return null;\n}\n\n\nconst hash_remove = (hash) => (key) => {\n    const h = hash.obj[hash.attr];\n    delete h[JSON.stringify(key)];\n    return null;\n}\n\n\nconst hash_get = (hash) => (key) => {\n    const r = hash[JSON.stringify(key)];\n    return r === undefined ? maybe_nothing : maybe_just(r[1]);\n}\n\n\nconst hash_for = (hash) => (f) => (acc) => {\n    for (let k in hash) {\n        const kv = hash[k];\n        acc = f(kv[0])(kv[1])(acc);\n    }\n    return acc;\n}\n\n\nconst hash_each = (hash) => (f) => {\n    for (let k in hash) {\n        const kv = hash[k];\n        f(kv[0])(kv[1]);\n    }\n    return null;\n}\n\n\n//\n// Arrays\n//\n\nconst array_push = (array) => (item) => {\n    array.obj[array.attr].push(item);\n    return null;\n}\n\nconst array_pop = (array) => {\n    const a = array.obj[array.attr];\n    return a.length ? maybe_just(a.pop()) : maybe_nothing;\n}\n\nconst array_get = (array) => (index) => {\n    const r = array[index];\n    return r === undefined ? maybe_nothing : maybe_just(r);\n}\n\nconst array_set = (array) => (index) => (item) => {\n    if (index < 0) return false;\n    const a = array.obj[array.attr];\n    if (index >= a.length) return false;\n    a[index] = item;\n    return true;\n}\n\nconst array_sortBy = (array) => (f) => {\n    const arr = array.obj[array.attr];\n    arr.sort((a, b) => sp_compare(f(a), f(b)));\n    return null;\n}\n\nconst array_toList = (array) => {\n  let length = array.length;\n  let list = [ '" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listNil + ("' ];\n  for (let i = length - 1; i >= 0; i--) {\n      list = [ '" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listCons + ("', array[i], list ];\n  }\n  return list;\n}\n\nconst array_fromList = (list) => {\n  const array = [];\n  const rec = (ls) => {\n    if (ls[0] === '" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listNil + ("')\n      return array;\n\n    array.push(ls[1]);\n    return rec(ls[2]);\n  };\n\n  return rec(list);\n}\n\n\n\n//\n// Lists\n//\n\n\nconst sp_cons = (list) => (item) => {\n  return [ '" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$listCons + "', item, list];\n}\n\nconst list_sortBy = (f) => (list) => array_toList(array_fromList(list).sort((a, b) => sp_compare(f(a), f(b))));\n    "))))))))))));

const $$home$nw$stuff$unstable$src$Platforms$Posix$compile = (($getRidOfMe) => {
  return (($targetUsr) => {
    return (($emittableStatements) => {
      const $$constructors = $getRidOfMe;
      const $eenv = $$constructors.errorEnv;
      const $constructors = $$constructors.constructors;
      ((sp_log)("Creating JS AST..."))("");
      const $jaStatements = ((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateAll)($eenv))($constructors))($emittableStatements);
      ((sp_log)("Emitting JS..."))("");
      const $callMain = ("\nconst out = " + (($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr)($targetUsr) + "({})(array_toList(process.argv.slice(1)))[1]('never');\n        if (out[1]) console.error(out[1]);\n        "));
      const $statements = (($core$Text$join)("\n\n"))((($core$List$map)(($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitStatement)(0)))($jaStatements));
      return ($$home$nw$stuff$unstable$src$Platforms$Posix$header + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$nativeDefinitions + ($$home$nw$stuff$unstable$src$Platforms$Posix$posixRuntime + ($statements + $callMain))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Platforms$Posix$posixModules = "\nlibrary =\n    source = \"core:posix\"\n\n    module =\n        path = IO\n        globalTypes = IO\n\n    module =\n        path = Path";

const $$home$nw$stuff$unstable$src$Platforms$Posix$platform = ({
  compile: $$home$nw$stuff$unstable$src$Platforms$Posix$compile,
  defaultModules: ($$home$nw$stuff$unstable$src$DefaultModules$asText + $$home$nw$stuff$unstable$src$Platforms$Posix$posixModules),
  defaultOutputPath: "nodeExecutable.js",
  name: "posix",
  quickstart: "TODO",
});

const $$home$nw$stuff$unstable$src$Platforms$RawJavaScript$compile = (($getRidOfMe) => {
  return (($targetUsr) => {
    return (($emittableStatements) => {
      const $$constructors = $getRidOfMe;
      const $eenv = $$constructors.errorEnv;
      const $constructors = $$constructors.constructors;
      ((sp_log)("Creating JS AST..."))("");
      const $jaStatements = ((($$home$nw$stuff$unstable$src$Targets$Javascript$EmittableToJs$translateAll)($eenv))($constructors))($emittableStatements);
      ((sp_log)("Emitting JS..."))("");
      const $statements = (($core$Text$join)("\n\n"))((($core$List$map)(($$home$nw$stuff$unstable$src$Targets$Javascript$JsToText$emitStatement)(0)))($jaStatements));
      const $main = ($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateUsr)($targetUsr);
      return ("squarepantsMain = (function() {\n" + ($$home$nw$stuff$unstable$src$Targets$Javascript$Runtime$nativeDefinitions + ($statements + ("\n return " + ($main + ";\n })();")))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Platforms$RawJavaScript$platform = ({
  compile: $$home$nw$stuff$unstable$src$Platforms$RawJavaScript$compile,
  defaultModules: $$home$nw$stuff$unstable$src$DefaultModules$asText,
  defaultOutputPath: "squarepants.mjs",
  name: "rawjs",
  quickstart: "TODO",
});

const $$home$nw$stuff$unstable$src$Main$availablePlatforms = (($core$Core$Cons)($$home$nw$stuff$unstable$src$Platforms$Posix$platform))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Platforms$RawJavaScript$platform))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Main$cliDefaults = ({
  platform: $$home$nw$stuff$unstable$src$Platforms$Posix$platform,
});

const $$home$nw$stuff$unstable$src$Main$parsePlatformName = (($maybeValue) => {
  return (($cliState) => {
    return ((($maybeValue)[0] === "Nothing")
      ? ($core$Result$Err)("Please specify a platform name, for example: `--platform=posix`")
      : ((($maybeValue)[0] === "Just")
        ? ((() => {
          const $value = ($maybeValue)[1];
          const $$try1 = (($core$List$find)((($p) => {
            return ((sp_equal)($value))($p.name);
          })))($$home$nw$stuff$unstable$src$Main$availablePlatforms);
          return ((($$try1)[0] === "Nothing")
            ? ($core$Result$Err)(("I don't know this platform name: `" + ($value + ("`\n\n  Valid platform names are:\n\n                    " + (($core$Text$join)("\n"))((($core$List$map)((($p) => {
              return ("    " + $p.name);
            })))($$home$nw$stuff$unstable$src$Main$availablePlatforms))))))
            : ((($$try1)[0] === "Just")
              ? ((() => {
                const $platform = ($$try1)[1];
                return ($core$Result$Ok)((Object.assign)({}, $cliState, ({
                  platform: $platform,
                })));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 155:12', (sp_toHuman)($$try1))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 150:4', (sp_toHuman)($maybeValue))));
  });
});

const $$home$nw$stuff$unstable$src$Main$cliOptions = (($core$Core$Cons)(({
  info: "select build platform",
  name: "--platform",
  parser: $$home$nw$stuff$unstable$src$Main$parsePlatformName,
})))($core$Core$Nil);

const $$home$nw$stuff$unstable$src$Main$indent = (($s) => {
  return (($core$Text$join)("\n"))((($core$List$map)((($l) => {
    return ("  " + $l);
  })))(((text_split)("\n"))($s)));
});

const $core$List$partition = (($f) => {
  return (($ls) => {
    return ((($core$List$forReversed)($ls))((($item) => {
      return (($$false) => {
        const $true = $$false.first;
        const $false = $$false.second;
        return (($f)($item)
          ? ({
            first: ((sp_cons)($true))($item),
            second: $false,
          })
          : ({
            first: $true,
            second: ((sp_cons)($false))($item),
          }));
      });
    })))(({
      first: $core$Core$Nil,
      second: $core$Core$Nil,
    }));
  });
});

const $$home$nw$stuff$unstable$src$Main$parseArguments = (($options) => {
  return (($args) => {
    return (($initState) => {
      const $$optionTexts = (($core$List$partition)((text_startsWith)("--")))($args);
      const $others = $$optionTexts.second;
      const $optionTexts = $$optionTexts.first;
      const $findOption = (($optionText) => {
        return (($state) => {
          const $$try1 = ((text_split)("="))($optionText);
          return ((($$try1)[0] === "Nil")
            ? ($core$Result$Ok)($state)
            : ((($$try1)[0] === "Cons")
              ? ((() => {
                const $optionName = ($$try1)[1];
                const $rest = ($$try1)[2];
                const $$try2 = (($core$List$find)((($o) => {
                  return ((sp_equal)($optionName))($o.name);
                })))($options);
                return ((($$try2)[0] === "Nothing")
                  ? ($core$Result$Err)(("Unknown option " + $optionName))
                  : ((($$try2)[0] === "Just")
                    ? ((() => {
                      const $option = ($$try2)[1];
                      const $value = (((sp_equal)($core$Core$Nil))($rest)
                        ? $core$Maybe$Nothing
                        : ($core$Maybe$Just)((($core$Text$join)("="))($rest)));
                      return (($option.parser)($value))($state);
                    }))()
                    : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 43:16', (sp_toHuman)($$try2))));
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 38:8', (sp_toHuman)($$try1))));
        });
      });
      return (($core$Result$map)(($core$Tuple$pair)($others)))(((($core$List$forRes)($optionTexts))($findOption))($initState));
    });
  });
});

const $$home$nw$stuff$unstable$src$Main$order = (($outcome) => {
  return ((($outcome)[0] === "Success")
    ? 0
    : ((($outcome)[0] === "Skipped")
      ? 1
      : ((($outcome)[0] === "Error")
        ? 2
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 86:4', (sp_toHuman)($outcome)))));
});

const $$home$nw$stuff$unstable$src$Term$green = ($$home$nw$stuff$unstable$src$Term$color)("\x1b[32m");

const $$home$nw$stuff$unstable$src$Main$testOutcomeToText = (($name) => {
  return (($code) => {
    return (($outcome) => {
      return ((($outcome)[0] === "Success")
        ? ($$home$nw$stuff$unstable$src$Term$green)(("* PASS: " + $name))
        : ((($outcome)[0] === "Skipped")
          ? ($$home$nw$stuff$unstable$src$Term$yellow)(("* skip: " + $name))
          : ((($outcome)[0] === "Error")
            ? ((() => {
              const $error = ($outcome)[1];
              return (($$home$nw$stuff$unstable$src$Term$red)(("FAIL ! " + $name)) + ("\n" + (($$home$nw$stuff$unstable$src$Main$indent)($code) + ("\n" + ($$home$nw$stuff$unstable$src$Main$indent)($error)))));
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 73:4', (sp_toHuman)($outcome)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Test$getName = (($test) => {
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
          return ($$home$nw$stuff$unstable$src$Test$getName)($t);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 145:4', (sp_toHuman)($test)))));
});

const $$home$nw$stuff$unstable$src$Test$outcomesRec = (($path) => {
  return (($test) => {
    return (($accum) => {
      return ((($test)[0] === "Single")
        ? ((() => {
          const $name = ($test)[1];
          const $code = ($test)[2];
          const $f = ($test)[3];
          return (((text_startsWith)("SKIP"))($name)
            ? ((sp_cons)($accum))(({
              code: "",
              name: ($path + $name),
              outcome: $$home$nw$stuff$unstable$src$Test$Skipped,
            }))
            : ((sp_cons)($accum))(({
              code: $code,
              name: ($path + $name),
              outcome: ($f)(null),
            })));
        }))()
        : ((($test)[0] === "NotNow")
          ? ((() => {
            const $t = ($test)[1];
            return ((sp_cons)($accum))(({
              code: "",
              name: ($path + ($$home$nw$stuff$unstable$src$Test$getName)($t)),
              outcome: $$home$nw$stuff$unstable$src$Test$Skipped,
            }));
          }))()
          : ((($test)[0] === "Group")
            ? ((() => {
              const $pathSegment = ($test)[1];
              const $ts = ($test)[2];
              return ((($core$List$for)($ts))(($$home$nw$stuff$unstable$src$Test$outcomesRec)(($path + ($pathSegment + " / ")))))($accum);
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 126:4', (sp_toHuman)($test)))));
    });
  });
});

const $$home$nw$stuff$unstable$src$Test$flatten = (($tests) => {
  return ((($core$List$for)($tests))(($$home$nw$stuff$unstable$src$Test$outcomesRec)("")))($core$Core$Nil);
});

const $$home$nw$stuff$unstable$src$Main$selftestMain = (() => {
  return ($$home$nw$stuff$unstable$lib$posix$IO$writeStdout)((($core$Text$join)("\n"))((($core$List$map)((($x) => {
    return ((($$home$nw$stuff$unstable$src$Main$testOutcomeToText)($x.name))($x.code))($x.outcome);
  })))(((list_sortBy)((($x) => {
    return ({
      first: ($$home$nw$stuff$unstable$src$Main$order)($x.outcome),
      second: $x.name,
    });
  })))(($$home$nw$stuff$unstable$src$Test$flatten)($$home$nw$stuff$unstable$src$Main$allTests)))));
});

const $$home$nw$stuff$unstable$src$Main$main = (($env) => {
  return (($args) => {
    const $$try1 = ((($$home$nw$stuff$unstable$src$Main$parseArguments)($$home$nw$stuff$unstable$src$Main$cliOptions))($args))($$home$nw$stuff$unstable$src$Main$cliDefaults);
    return ((($$try1)[0] === "Err")
      ? ((() => {
        const $message = ($$try1)[1];
        return ($$home$nw$stuff$unstable$lib$posix$IO$writeStdout)($message);
      }))()
      : ((($$try1)[0] === "Ok")
        ? ((() => {
          const $args = ($$try1)[1].first;
          const $cliState = ($$try1)[1].second;
          return (((($args)[0] === "Cons") && (((($args)[2])[0] === "Cons") && ("selftest" === (($args)[2])[1])))
            ? ((() => {
              const $self = ($args)[1];
              const $tail = (($args)[2])[2];
              return ($$home$nw$stuff$unstable$src$Main$selftestMain)(null);
            }))()
            : (((($args)[0] === "Cons") && ((($args)[2])[0] === "Cons"))
              ? ((() => {
                const $self = ($args)[1];
                const $head = (($args)[2])[1];
                const $tail = (($args)[2])[2];
                const $mainModulePath = $head;
                const $maybeOutputPath = ($core$List$head)($tail);
                return ($$home$nw$stuff$unstable$src$Compile$compileMain)(({
                  entryModulePath: $mainModulePath,
                  env: $env,
                  maybeOutputPath: $maybeOutputPath,
                  platform: $cliState.platform,
                  selfPath: $self,
                }));
              }))()
              : (true
                ? ($$home$nw$stuff$unstable$lib$posix$IO$writeStdout)("\nHi! This is the Squarepants compiler!\n\nTo compile something, write:\n\n    squarepants pathToMainModule.sp\n")
                : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 216:12', (sp_toHuman)($args)))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 211:4', (sp_toHuman)($$try1))));
  });
});

const $$home$nw$stuff$unstable$src$Main$parseCli = (($args) => {
  return (((($args)[0] === "Cons") && (((($args)[2])[0] === "Cons") && ("selftest" === (($args)[2])[1])))
    ? ((() => {
      const $self = ($args)[1];
      const $tail = (($args)[2])[2];
      return $$home$nw$stuff$unstable$src$Main$Selftest;
    }))()
    : (((($args)[0] === "Cons") && ((($args)[2])[0] === "Cons"))
      ? ((() => {
        const $self = ($args)[1];
        const $head = (($args)[2])[1];
        const $tail = (($args)[2])[2];
        return ($$home$nw$stuff$unstable$src$Main$Compile)(({
          mainModulePath: $head,
          maybeOutputPath: ($core$List$head)($tail),
          self: $self,
        }));
      }))()
      : (true
        ? $$home$nw$stuff$unstable$src$Main$Help
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Main.sp 187:4', (sp_toHuman)($args)))));
});

const $$home$nw$stuff$unstable$src$SPLib$Buffer$readOne = (($b) => {
  return (($b.nextPos < $b.fullSize)
    ? ({
      first: (((text_slice)($b.nextPos))(($b.nextPos + 1)))($b.fullText),
      second: (Object.assign)({}, $b, ({
        nextPos: ($b.nextPos + 1),
      })),
    })
    : ({
      first: "",
      second: $b,
    }));
});

const $$home$nw$stuff$unstable$src$SPLib$Parser$tuple3 = (($pa) => {
  return (($pb) => {
    return (($pc) => {
      return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($a) => {
        return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($b) => {
          return (($$home$nw$stuff$unstable$src$SPLib$Parser$andThen)((($c) => {
            return ($$home$nw$stuff$unstable$src$SPLib$Parser$accept)(({
              first: $a,
              second: $b,
              third: $c,
            }));
          })))($pc);
        })))($pb);
      })))($pa);
    });
  });
});

const $$home$nw$stuff$unstable$src$SPON$logHead = (($statements) => {
  ((($statements)[0] === "Cons")
    ? ((() => {
      const $head = ($statements)[1];
      const $tail = ($statements)[2];
      ((sp_log)("LOG"))($head);
      return null;
    }))()
    : ((($statements)[0] === "Nil")
      ? ((sp_log)("LOG"))(null)
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/SPON.sp 68:4', (sp_toHuman)($statements))));
  return (($$home$nw$stuff$unstable$src$SPON$Accepted)($statements))(null);
});

const $$home$nw$stuff$unstable$src$StateMonad$map = (($f) => {
  return (($m) => {
    return (($$home$nw$stuff$unstable$src$StateMonad$andThen)((($x) => {
      return ($$home$nw$stuff$unstable$src$StateMonad$return)(($f)($x));
    })))($m);
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest = ($$home$nw$stuff$unstable$src$Test$codeTest)(sp_toHuman);

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval = (($value) => {
  return (($code) => {
    return ($core$Result$Ok)("CanonicalToJS.eval not implemented");
  });
});

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$ifs = (($$home$nw$stuff$unstable$src$Test$Group)("ifs"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("basic sanity"))("a =\n  if True:\n    1\n  else\n    2"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("1"))))($core$Core$Nil));

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$misc = (($$home$nw$stuff$unstable$src$Test$Group)("misc"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("definitions and mutations return None"))("x =\n  m @= 0\n\ny =\n  m @= 0\n  @m += 1\n\na =\n  { x, y }"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"x\":null,\"y\":null}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("Cons"))("a = 1 :: []"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("[\"Core.Cons\",1,[\"Core.Nil\"]]"))))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$mutation = (($$home$nw$stuff$unstable$src$Test$Group)("mutation"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("basic sanity"))("a =\n  m @= 0\n  @m += 1\n  x = m\n  @m := 10\n  y = m\n  @m += 1\n  z = m\n  { x, y, z, m }"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.a")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"m\":11,\"x\":1,\"y\":10,\"z\":11}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("nested record"))("record = { x = { y = { z = 4 } } }\n\nresult =\n   m @= record\n   @m.x.y :=  { z = 1 }\n   @m.x.y.z += 1\n   m"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"x\":{\"y\":{\"z\":2}}}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("pass mutable to function"))("fun @m =\n  @m += 55\n\nresult =\n   m @= 2\n   fun @m\n   m"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("57"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("pass nested mutable value to function"))("fun @m =\n  @m += 55\n\nrecord = { x = { y = { z = 4 } } }\n\nresult =\n   m @= record\n   fun @m.x.y.z\n   m"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"x\":{\"y\":{\"z\":59}}}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("[reg] mut args should be dereferenced and cloned"))("result =\n    l @= 3\n    f @l\n\nf @a =\n    as Number @: Number\n    a"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("3"))))($core$Core$Nil))))));

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$natives = (($$home$nw$stuff$unstable$src$Test$Group)("natives"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("Debug.log"))("result = log \"this is produced by a test\" True"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("true"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("Debug.log, partially applied"))("result = log \"if this gets actually logged, we have a problem\""))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("undefined"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("Debug.todo"))("a = todo \"blah\"\nresult = 1"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$errorContains)((($core$Core$Cons)("blah"))($core$Core$Nil)))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("Text concat (..)"))("result = \"a\" .. \"b\" .. \"c\""))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("\"abc\""))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("add"))("result = 1 + 2"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("3"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("subtract"))("result = 5 - 3"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("2"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("multiply"))("result = 3 * 2"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("6"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("divide"))("result = 3 / 2"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("1.5"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("divide by zero"))("result = 3 / 0"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("0"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("lesser than (<)"))("result = 3 < 2 & 2 < 3"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"first\":false,\"second\":true}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("greater than (>)"))("result = 3 > 2 & 2 > 3"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"first\":true,\"second\":false}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("partial application"))("result = (-) 2"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("undefined"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("mutable partial application"))("f = (+=) 3\n\nresult =\n   m @= 1\n   f @m\n   m"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("4"))))($core$Core$Nil))))))))))))));

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$try_ = (($$home$nw$stuff$unstable$src$Test$Group)("try"))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("basic sanity"))("union A = A Number, B, C Bool\n\na x =\n  try x as\n    A 1: 11\n    A n: n\n    B: 3\n    C False: 5\n    C _: 6\n\nresult =\n { x = a (A 2)\n , y = a (A 1)\n , z = a B\n , w = a (C False)\n , k = a (C True)\n }"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("{\"k\":6,\"w\":6,\"x\":2,\"y\":11,\"z\":3}"))))((($core$Core$Cons)((((($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$codeTest)("[reg]: pattern any"))("result =\n   try 2 as\n     x: x"))(($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$eval)("Test.result")))(($$home$nw$stuff$unstable$src$Test$isOkAndEqualTo)("2"))))($core$Core$Nil)));

const $$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$tests = (($$home$nw$stuff$unstable$src$Test$Group)("CanonicalToJS"))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$misc))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$mutation))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$ifs))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$try_))((($core$Core$Cons)($$home$nw$stuff$unstable$src$Targets$Javascript$CanonicalToJs_Test$natives))($core$Core$Nil))))));

const $$home$nw$stuff$unstable$src$Test$errorsFirst = (($outcome) => {
  return ((($outcome)[0] === "Error")
    ? ((() => {
      const $e = ($outcome)[1];
      return (0 - 1);
    }))()
    : ((($outcome)[0] === "Skipped")
      ? 0
      : ((($outcome)[0] === "Success")
        ? 1
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Test.sp 165:4', (sp_toHuman)($outcome)))));
});

const $$home$nw$stuff$unstable$src$Types$CanonicalAst$typePos = (($ty) => {
  return ((($ty)[0] === "TypeConstant")
    ? ((() => {
      const $p = ($ty)[1];
      return $p;
    }))()
    : ((($ty)[0] === "TypeVariable")
      ? ((() => {
        const $p = ($ty)[1];
        return $p;
      }))()
      : ((($ty)[0] === "TypeFunction")
        ? ((() => {
          const $p = ($ty)[1];
          return $p;
        }))()
        : ((($ty)[0] === "TypeRecord")
          ? ((() => {
            const $p = ($ty)[1];
            return $p;
          }))()
          : ((($ty)[0] === "TypeAlias")
            ? ((() => {
              const $p = ($ty)[1];
              return $p;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/CanonicalAst.sp 190:4', (sp_toHuman)($ty)))))));
});

const $$home$nw$stuff$unstable$src$Types$FormattableAst$patternNames = (($pattern) => {
  const $foldOver = (($pas) => {
    return ((($core$List$for)($pas))((($p) => {
      return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternNames)($p));
    })))($core$Dict$empty);
  });
  const $insertAttr = (($a) => {
    const $$maybePa = $a;
    const $maybePa = $$maybePa.second;
    const $name = ($$maybePa.first)[2];
    const $pos = ($$maybePa.first)[1];
    return ((($maybePa)[0] === "Nothing")
      ? (($core$Dict$insert)($name))($pos)
      : ((($maybePa)[0] === "Just")
        ? ((() => {
          const $pat = ($maybePa)[1];
          return ($core$Dict$join)(($$home$nw$stuff$unstable$src$Types$FormattableAst$patternNames)($pat));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 177:8', (sp_toHuman)($maybePa))));
  });
  return ((($pattern)[0] === "PatternAny")
    ? ((() => {
      const $pos = ($pattern)[1];
      const $n = ($pattern)[3];
      return (($core$Dict$singleton)($n))($pos);
    }))()
    : ((($pattern)[0] === "PatternLiteralNumber")
      ? $core$Dict$empty
      : ((($pattern)[0] === "PatternLiteralText")
        ? $core$Dict$empty
        : ((($pattern)[0] === "PatternConstructor")
          ? ((() => {
            const $pas = ($pattern)[4];
            return ($foldOver)($pas);
          }))()
          : ((($pattern)[0] === "PatternList")
            ? ((() => {
              const $pas = ($pattern)[2];
              return ($foldOver)($pas);
            }))()
            : ((($pattern)[0] === "PatternListCons")
              ? ((() => {
                const $pas = ($pattern)[2];
                return ($foldOver)($pas);
              }))()
              : ((($pattern)[0] === "PatternRecord")
                ? ((() => {
                  const $pos = ($pattern)[1];
                  const $ars = ($pattern)[2];
                  return ((($core$List$for)($ars.attrs))($insertAttr))($core$Dict$empty);
                }))()
                : ((($pattern)[0] === "PatternTuple")
                  ? ((() => {
                    const $pas = ($pattern)[2];
                    return ($foldOver)($pas);
                  }))()
                  : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 181:4', (sp_toHuman)($pattern))))))))));
});

const $$home$nw$stuff$unstable$src$Types$FormattableAst$sepList_mapItem = (($f) => {
  return (($aAndLa) => {
    const $$a = $aAndLa;
    const $la = $$a.second;
    const $a = $$a.first;
    return ({
      first: ($f)($a),
      second: (($core$List$map)(($core$Tuple$mapSecond)($f)))($la),
    });
  });
});

const $$home$nw$stuff$unstable$src$Types$FormattableAst$typePos = (($type) => {
  return ((($type)[0] === "TypeVariable")
    ? ((() => {
      const $p = ($type)[1];
      return $p;
    }))()
    : ((($type)[0] === "TypeConstant")
      ? ((() => {
        const $p = ($type)[1];
        return $p;
      }))()
      : ((($type)[0] === "TypeFunction")
        ? ((() => {
          const $p = ($type)[1];
          return $p;
        }))()
        : ((($type)[0] === "TypeTuple")
          ? ((() => {
            const $p = ($type)[1];
            return $p;
          }))()
          : ((($type)[0] === "TypeList")
            ? ((() => {
              const $p = ($type)[1];
              return $p;
            }))()
            : ((($type)[0] === "TypeRecord")
              ? ((() => {
                const $p = ($type)[1];
                return $p;
              }))()
              : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/FormattableAst.sp 125:4', (sp_toHuman)($type))))))));
});

const $$home$nw$stuff$unstable$src$Types$Pos$end = (($pos) => {
  return ((($pos)[0] === "P")
    ? ((() => {
      const $m = ($pos)[1];
      const $s = ($pos)[2];
      const $e = ($pos)[3];
      return $e;
    }))()
    : (true
      ? 0
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/Pos.sp 36:4', (sp_toHuman)($pos))));
});

const $$home$nw$stuff$unstable$src$Types$Pos$range = (($a) => {
  return (($b) => {
    const $$try1 = ({
      first: $a,
      second: $b,
    });
    return (((($$try1.first)[0] === "P") && (($$try1.second)[0] === "P"))
      ? ((() => {
        const $ma = ($$try1.first)[1];
        const $sa = ($$try1.first)[2];
        const $ea = ($$try1.first)[3];
        const $mb = ($$try1.second)[1];
        const $sb = ($$try1.second)[2];
        const $eb = ($$try1.second)[3];
        return (((sp_not_equal)($mb))($ma)
          ? (sp_todo)("trying to range across two different modules")
          : ((($$home$nw$stuff$unstable$src$Types$Pos$P)($ma))((($core$Basics$min)($sa))($sb)))((($core$Basics$max)($ea))($eb)));
      }))()
      : ((($$try1.first)[0] === "P")
        ? $a
        : (true
          ? $b
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/src/Types/Pos.sp 43:4', (sp_toHuman)($$try1)))));
  });
});

const $$home$nw$stuff$unstable$tutorial$CompileText$blue = (($t) => {
  return ("<span class=blue>" + ($t + "</span>"));
});

const $$home$nw$stuff$unstable$tutorial$CompileText$red = (($t) => {
  return ("<span class=red>" + ($t + "</span>"));
});

const $$home$nw$stuff$unstable$tutorial$CompileText$yellow = (($t) => {
  return ("<span class=yellow>" + ($t + "</span>"));
});

const $$home$nw$stuff$unstable$tutorial$CompileText$formattedToConsoleColoredText = (($formattedText) => {
  return ((($formattedText)[0] === "FormattedText_Default")
    ? ((() => {
      const $t = ($formattedText)[1];
      return $t;
    }))()
    : ((($formattedText)[0] === "FormattedText_Emphasys")
      ? ((() => {
        const $t = ($formattedText)[1];
        return ($$home$nw$stuff$unstable$tutorial$CompileText$yellow)($t);
      }))()
      : ((($formattedText)[0] === "FormattedText_Warning")
        ? ((() => {
          const $t = ($formattedText)[1];
          return ($$home$nw$stuff$unstable$tutorial$CompileText$red)($t);
        }))()
        : ((($formattedText)[0] === "FormattedText_Decoration")
          ? ((() => {
            const $t = ($formattedText)[1];
            return ($$home$nw$stuff$unstable$tutorial$CompileText$blue)($t);
          }))()
          : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 17:4', (sp_toHuman)($formattedText))))));
});

const $$home$nw$stuff$unstable$tutorial$CompileText$loadModule = (($meta) => {
  return (($umr) => {
    return (($moduleAsText) => {
      const $$moduleName = $umr;
      const $moduleName = ($$moduleName)[2];
      const $source = ($$moduleName)[1];
      const $params = ({
        meta: $meta,
        name: $moduleName,
        source: $source,
        stripLocations: false,
      });
      const $eenv = ({
        moduleByName: (($core$Dict$singleton)($moduleName))(({
          content: $moduleAsText,
          fsPath: "<user input>",
        })),
      });
      return (($$home$nw$stuff$unstable$src$Compiler$MakeCanonical$textToCanonicalModule)($params))($moduleAsText);
    });
  });
});

const $$home$nw$stuff$unstable$tutorial$CompileText$resToConsoleText = (($errorEnv) => {
  return (($res) => {
    return ((($res)[0] === "Ok")
      ? ((() => {
        const $a = ($res)[1];
        return ($core$Result$Ok)($a);
      }))()
      : ((($res)[0] === "Err")
        ? ((() => {
          const $e = ($res)[1];
          return ($core$Result$Err)((($core$Text$join)(""))((($core$List$map)($$home$nw$stuff$unstable$tutorial$CompileText$formattedToConsoleColoredText))((($$home$nw$stuff$unstable$src$Compiler$Error$toFormattedText)($errorEnv))($e))));
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 26:4', (sp_toHuman)($res))));
  });
});

const $$home$nw$stuff$unstable$tutorial$CompileText$onResSuccess = (($errorEnv) => {
  return (($f) => {
    return (($res) => {
      return (($core$Result$onOk)($f))((($$home$nw$stuff$unstable$tutorial$CompileText$resToConsoleText)($errorEnv))($res));
    });
  });
});

const $$home$nw$stuff$unstable$tutorial$CompileText$typeCheckModule = (($meta) => {
  return (($globals) => {
    return (($module) => {
      const $env = ({
        constructors: $globals.constructors,
        currentModule: $module.umr,
        instanceVariables: (($core$Dict$mapKeys)($$home$nw$stuff$unstable$src$Types$CanonicalAst$RefRoot))($globals.instanceVariables),
        meta: $meta,
        nonAnnotatedRecursives: $core$Dict$empty,
        nonFreeTyvars: $core$Dict$empty,
        types: $globals.types,
      });
      return (($$home$nw$stuff$unstable$src$Compiler$TypeCheck$fromModule)($env))($module);
    });
  });
});

const $$home$nw$stuff$unstable$tutorial$CompileText$main = ((() => {
  const $platform = $$home$nw$stuff$unstable$src$Platforms$RawJavaScript$platform;
  const $modulesFileName = "modules.sp";
  const $inputFileName = "user_input";
  const $meta = ((() => {
    const $$try2 = (($$home$nw$stuff$unstable$src$ModulesFile$textToModulesFile)($modulesFileName))($platform.defaultModules);
    return ((($$try2)[0] === "Ok")
      ? ((() => {
        const $m = ($$try2)[1];
        return ($$home$nw$stuff$unstable$src$ModulesFile$toMeta)($m);
      }))()
      : ((($$try2)[0] === "Err")
        ? ((() => {
          const $err = ($$try2)[1];
          const $eenv = ({
            moduleByName: (($core$Dict$singleton)($modulesFileName))(({
              content: $platform.defaultModules,
              fsPath: $modulesFileName,
            })),
          });
          const $errAsText = (($core$Text$join)(""))((($core$List$map)($$home$nw$stuff$unstable$tutorial$CompileText$formattedToConsoleColoredText))((($$home$nw$stuff$unstable$src$Compiler$Error$toFormattedText)($eenv))($err)));
          ((sp_log)($errAsText))("--");
          return (sp_todo)("This is a compiler bug, not your fault.");
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 118:6', (sp_toHuman)($$try2))));
  }))();
  const $umr = (($$home$nw$stuff$unstable$src$Types$Meta$UMR)(($$home$nw$stuff$unstable$src$Types$Meta$SourceDir)($inputFileName)))($inputFileName);
  const $entryUsr = (($$home$nw$stuff$unstable$src$Types$Meta$USR)($umr))("pixelColor");
  return (($code) => {
    const $eenv = ({
      moduleByName: (($core$Dict$singleton)($inputFileName))(({
        content: $code,
        fsPath: "",
      })),
    });
    return (($core$Result$onOk)((($module) => {
      const $modules = (($core$Dict$singleton)($umr))($module);
      const $globals = ((() => {
        const $$try1 = ($$home$nw$stuff$unstable$src$Compiler$Pipeline$globalExpandedTypes)($modules);
        return ((($$try1)[0] === "Err")
          ? ((() => {
            const $e = ($$try1)[1];
            return (sp_todo)((sp_toHuman)($e));
          }))()
          : ((($$try1)[0] === "Ok")
            ? ((() => {
              const $g = ($$try1)[1];
              return $g;
            }))()
            : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/tutorial/CompileText.sp 168:8', (sp_toHuman)($$try1))));
      }))();
      const $typeCheckModules = (($core$List$mapRes)((($m) => {
        return ((($$home$nw$stuff$unstable$tutorial$CompileText$typeCheckModule)($meta))($globals))($m);
      })))(($core$Dict$values)($modules));
      return ((($$home$nw$stuff$unstable$tutorial$CompileText$onResSuccess)($eenv))((($emittableStatements) => {
        return ($core$Result$Ok)(((($platform.compile)(({
          constructors: ($core$Dict$toList)($globals.constructors),
          errorEnv: $eenv,
        })))($entryUsr))($emittableStatements));
      })))((($core$Result$mapError)((($e) => {
        return (sp_todo)("MakeEmittable.translateAll returned Err");
      })))(($$home$nw$stuff$unstable$src$Compiler$MakeEmittable$translateAll)(($core$Dict$values)($modules))));
    })))((($$home$nw$stuff$unstable$tutorial$CompileText$resToConsoleText)($eenv))(((($$home$nw$stuff$unstable$tutorial$CompileText$loadModule)($meta))($umr))($code)));
  });
}))();

const $core$Core$sendLeft = "sendLeft";

const $core$Core$sendRight = "sendRight";

const $core$Dict$intersect = (($t1) => {
  return (($t2) => {
    return (($core$Dict$filter)((($k) => {
      return (() => {
        return (($core$Dict$member)($k))($t2);
      });
    })))($t1);
  });
});

const $core$Dict$isEmpty = (($dict) => {
  return ((($dict)[0] === "RBEmpty_elm_builtin")
    ? true
    : ((($dict)[0] === "RBNode_elm_builtin")
      ? false
      : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Dict.sp 70:2', (sp_toHuman)($dict))));
});

const $core$Dict$partition = (($isGood) => {
  return (($dict) => {
    const $add = (($key) => {
      return (($value) => {
        return (($t) => {
          const $$t1 = $t;
          const $t2 = $$t1.second;
          const $t1 = $$t1.first;
          return ((($isGood)($key))($value)
            ? ({
              first: ((($core$Dict$insert)($key))($value))($t1),
              second: $t2,
            })
            : ({
              first: $t1,
              second: ((($core$Dict$insert)($key))($value))($t2),
            }));
        });
      });
    });
    return ((($core$Dict$for)($dict))($add))(({
      first: $core$Dict$empty,
      second: $core$Dict$empty,
    }));
  });
});

const $core$Hash$toList = (($h) => {
  return (((hash_for)($h))((($k) => {
    return (($v) => {
      return (($l) => {
        return ((sp_cons)($l))(({
          first: $k,
          second: $v,
        }));
      });
    });
  })))($core$Core$Nil);
});

const $core$List$indexBy = (($getIndex) => {
  return (($list) => {
    return ((($core$List$for)($list))((($i) => {
      return (($core$Dict$insert)(($getIndex)($i)))($i);
    })))($core$Dict$empty);
  });
});

const $core$List$mapFirst = (($f) => {
  return (($ls) => {
    return ((($ls)[0] === "Nil")
      ? $core$Maybe$Nothing
      : ((($ls)[0] === "Cons")
        ? ((() => {
          const $h = ($ls)[1];
          const $tail = ($ls)[2];
          const $r = ($f)($h);
          return (((sp_equal)($core$Maybe$Nothing))($r)
            ? (($core$List$mapFirst)($f))($tail)
            : $r);
        }))()
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/List.sp 318:4', (sp_toHuman)($ls))));
  });
});

const $core$List$sort = (list_sortBy)($core$Basics$identity);

const $core$Maybe$map2 = (($f) => {
  return (($ma) => {
    return (($mb) => {
      return (($core$Maybe$andThen)((($a) => {
        return (($core$Maybe$andThen)((($b) => {
          return ($core$Maybe$Just)((($f)($a))($b));
        })))($mb);
      })))($ma);
    });
  });
});

const $core$Maybe$map3 = (($f) => {
  return (($ma) => {
    return (($mb) => {
      return (($mc) => {
        return (($core$Maybe$andThen)((($a) => {
          return (($core$Maybe$andThen)((($b) => {
            return (($core$Maybe$andThen)((($c) => {
              return ($core$Maybe$Just)(((($f)($a))($b))($c));
            })))($mc);
          })))($mb);
        })))($ma);
      });
    });
  });
});

const $core$Result$withDefault = (($default) => {
  return (($result) => {
    return ((($result)[0] === "Ok")
      ? ((() => {
        const $a = ($result)[1];
        return $a;
      }))()
      : ((($result)[0] === "Err")
        ? $default
        : (sp_throw)('Missing pattern in try..as', '/home/nw/stuff/unstable/lib/core/Result.sp 42:4', (sp_toHuman)($result))));
  });
});

const $core$Set$diff = $core$Dict$diff;

const $core$Set$intersect = $core$Dict$intersect;

const $core$Set$isEmpty = $core$Dict$isEmpty;

const $core$Set$join = $core$Dict$join;

const $core$Set$remove = $core$Dict$remove;

const $core$Set$size = $core$Dict$size;

const $core$Set$toList = $core$Dict$keys;

const $core$Text$dropRight = (($n) => {
  return (($s) => {
    return (($n > 0)
      ? (((text_slice)(0))((0 - $n)))($s)
      : $s);
  });
});

const $core$Tuple$mapBoth = (($fa) => {
  return (($fb) => {
    return (($t) => {
      return ({
        first: ($fa)($t.first),
        second: ($fb)($t.second),
      });
    });
  });
});

const $core$Tuple$mapFirst = (($f) => {
  return (($t) => {
    return ({
      first: ($f)($t.first),
      second: $t.second,
    });
  });
});

const $core$Tuple$pair_$$$$ = "pair_$$$$";
 return $$home$nw$stuff$unstable$tutorial$CompileText$main;
 })();