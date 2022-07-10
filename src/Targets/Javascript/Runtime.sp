

# TODO get these from CoreTypes, then translate it via MakeEmittable.whateverItsName
# TODO add Just and Nothing
listCons as Text =
    "Cons"


listNil as Text =
    "Nil"



nativeDefinitions as Text =
    """
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
        console.error(`ERROR: a benchmark has been started but not stopped!\nStart was at:${debug_benchStartStack}`);

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
        throw new Error(`\nbenchStart called when a benchmark is already ongoing!\nPrevious benchStart call was ${debug_benchStartStack}\n`);

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
  if (list[0] === '""" .. listCons .. """') {
    arrayAccum.push(sp_toHuman(list[1]));
    return sp_toHumanAsList(arrayAccum, list[2]);
  }

  if (list[0] === '""" .. listNil .. """')
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
  let list = [ '""" .. listNil .. """' ];
  for (let i = length - 1; i >= 0; i--) {
      list = [ '""" .. listCons .. """', array[i], list ];
  }
  return list;
}

const array_fromList = (list) => {
  const array = [];
  const rec = (ls) => {
    if (ls[0] === '""" .. listNil .. """')
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
  return [ '""" .. listCons .. """', item, list];
}

const list_sortBy = (f) => (list) => array_toList(array_fromList(list).sort((a, b) => sp_compare(f(a), f(b))));
    """
