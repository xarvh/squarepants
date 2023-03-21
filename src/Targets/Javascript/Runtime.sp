

# TODO get these from CoreTypes, then translate it via MakeEmittable.whateverItsName
# TODO add Just and Nothing
listCons as Text =
    "Cons"


listNil as Text =
    "Nil"



nativeDefinitions as Text =
    """
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


const id = (n) => '    '.repeat(n);


const sp_toHuman = (a, l = 0) => {

  if (Array.isArray(a))
    return sp_toHumanAsList([], a, l) || sp_toHumanAsUnion(a, l);

  if (typeof a === 'function') {
    return '<fn ' + a.length + '>';
  }

  if (typeof a === 'object') {
    let acc = '{\\n';
    for (let key in a)
        acc += id(l + 1) + key + ' = ' + sp_toHuman(a[key], l + 1) + '\\n';

    return acc + id(l) + '}';
  }

  return JSON.stringify(a, null, 0);
}


const sp_toHumanAsUnion = (a, l) => {

  if (a.length === 1) {
      return a[0];
  }

  let acc = a[0] + '\\n';

  a.slice(1).forEach(arg => {

      const sub = sp_toHuman(arg, l + 1);
      if (!sub.startsWith('{') && sub.indexOf('\\n') > -1)
          acc += id(l + 1) + '(' + sub + id(l + 1) + ')\\n';
      else
          acc += id(l + 1) + sub + '\\n';

  })

  return acc;
}


const sp_toHumanAsList = (arrayAccum, list, l) => {
  if (list[0] === '""" .. listCons .. """' && list.length === 3) {
    arrayAccum.push(sp_toHuman(list[1], l));
    return sp_toHumanAsList(arrayAccum, list[2], l);
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
    if (ls[0] === '""" .. listNil .. """')
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
  let list = [ '""" .. listNil .. """' ];
  for (let i = length - 1; i >= 0; i--) {
      list = [ '""" .. listCons .. """', arr[i], list ];
  }
  return list;
}

const array_toList = (arr) => [arrayToListLow(arr), arr];


const arrayFromListLow = (list) => {
  const array = [];
  const rec = (ls) => {
    if (ls[0] === '""" .. listNil .. """')
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
  return [ '""" .. listCons .. """', item, list];
}

const list_sortBy = (f, list) => arrayToListLow(arrayFromListLow(list).sort((a, b) => basics_compare(f(a), f(b))));
    """
