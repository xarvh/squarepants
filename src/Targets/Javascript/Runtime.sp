listConsName as Text =
    Targets/Javascript/EmittableToJs.translateName CoreDefs.consName


listNilName as Text =
    Targets/Javascript/EmittableToJs.translateName CoreDefs.nilName


nativeDefinitions as Text =

    okRef as Text =
        Targets/Javascript/EmittableToJs.constructorUsrToText  ('USR (CoreDefs.makeUmr "Result") "'ok")
        >> __ .. ".usr"

    errRef as Text =
        Targets/Javascript/EmittableToJs.constructorUsrToText ('USR (CoreDefs.makeUmr "Result") "'err")
        >> __ .. ".usr"


    nothingRef as Text =
        Targets/Javascript/EmittableToJs.constructorUsrToText ('USR (CoreDefs.makeUmr "Maybe") "'nothing")

    justRef as Text =
        Targets/Javascript/EmittableToJs.constructorUsrToText ('USR (CoreDefs.makeUmr "Maybe") "'just")
        >> __ .. ".usr"

#    translateUsrSelf =
#        sp_introspect_value Targets/Javascript/EmittableToJs.translateUsr
#
#    translateUsrRef as Text =
#        Targets/Javascript/EmittableToJs.translateUsr translateUsrSelf.usr

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


    const basics_cloneUni = sp_clone;


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
            console.error("");
            console.error("Benchmark results:");
            ks.sort().forEach(k => {
                const entry = debug_benchEntries[k];
                console.error(
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
        return sp_toHumanAsList([], a, l) || sp_toHumanAsDict(a, l) || sp_toHumanAsUnion(a, l);

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
      if (list[0] === '
    """
    .. listConsName
    .. """
    ' && list.length === 3) {
        arrayAccum.push(sp_toHuman(list[1], l));
        return sp_toHumanAsList(arrayAccum, list[2], l);
      }

      if (list[0] === '
    """
    .. listNilName
    .. """
    ')
        return '[' + arrayAccum.join(', ') + ']';

      return false;
    }

    const sp_toHumanAsDict = (dict, l) => {
      if (dict[0] === 'RBNode_elm_builtin') {
          return 'DICT' + sp_toHumanAsList([], $core$Dict$toList(dict), l);
      }

      return false;
    }


    //
    // Text
    //


    const text_fromNumber = (n) => '' + n;

    const text_toNumber = (t) => {
        const n = +t;

        return isNaN(n) ?
    """
    .. nothingRef
    .. """
     :
    """
    .. justRef
    .. """
    (n);
        }

        const text_toLower = (s) => s.toLowerCase()

        const text_toUpper = (s) => s.toUpperCase()

        const text_split = (separator, target) => arrayToListLow(target.split(separator));

        const text_length = (s) => s.length;

        const text_slice = (start, end, s) => s.slice(start, end);

        const text_startsWith = (sub, s) => s.startsWith(sub);

        const text_startsWithRegex = (regex) => {
          let re, f;
          try {
            re = new RegExp('^' + regex, 's');
          } catch (e) {
            f = () => "";
          }

          f = (s) => {
            let m = s.match(re);
            return m ? m[0] : "";
          };

          return { ctx: [], usr: f };
        }

        const text_replaceRegex = (regex) => {
          let re, f;
          try {
            re = new RegExp(regex, 'g');
          } catch (e) {
            f = () => ""
          }

          f = (replacer, s) => s.replace(re, replacer);

          return { ctx: [], usr: f };
        }

        const text_trimLeft = (s) => s.trimLeft();

        const text_dropLeft = (n, s) => s.slice(n);

        const text_forEach = (s, f) => {
          for (let i of s) f.usr(...f.ctx, i);
          return null;
        }


        //
        // Hashes
        //

        const hash_pop = (hash) => {
            for (let key in hash) {
                const [actualKey, value] = hash[key];
                delete hash[key];
                return 
    """
    .. justRef
    .. """
    ({ first: actualKey, second: value });
            }

            return 
    """
    .. nothingRef
    .. """;
        }


        const hash_fromList = (list) => {
          const hash = {};

          // TODO iteration instead of recursion
          const rec = (ls) => {
            if (ls[0] === '"""
    .. listNilName
    .. """')
              return hash;

            const { first, second } = ls[1];

            hash[JSON.stringify(first)] = [first, second];

            return rec(ls[2]);
          };

          return rec(list);
        }


        const hash_insert = (hash, key, value) => {
            hash[JSON.stringify(key)] = [key, value];
            return null;
        }


        const hash_remove = (hash, key) => {
            delete hash[JSON.stringify(key)];
            return null;
        }


        const hash_get = (hash, key) => {
            const r = hash[JSON.stringify(key)];
            return r === undefined ?
    """
    .. nothingRef
    .. """
     :
    """
    .. justRef
    .. """
    (r[1]);
        }


        const hash_for = (hash, f, acc) => {
            for (let k in hash) {
                const kv = hash[k];
                acc = f.usr(...f.ctx, kv[0], kv[1], acc);
            }
            return acc;
        }


        const hash_each = (hash, f) => {
            for (let k in hash) {
                const kv = hash[k];
                f.usr(...f.ctx, kv[0], kv[1]);
            }
            return null;
        }


        //
        // Arrays
        //

        const array_each = (array, f) => {
            array.forEach((e) => f.usr(...f.ctx, e));
            return null;
        }

        const array_push = (array, item) => {
            array.push(item);
            return null;
        }

        const array_pop = (a) => {
            return a.length ?
    """
    .. justRef
    .. """
    (a.pop()) :
    """
    .. nothingRef
    .. """
    ;
        }

        const array_get = (array, index) => {
            const r = array[index];
            return r === undefined ?
    """
    .. nothingRef
    .. """
     :
    """
    .. justRef
    .. """
    (r);
        }

        const array_set = (a, index, item) => {
            if (index < 0) return false;
            if (index >= a.length) return [false, a];
            a[index] = item;
            return true;
        }

        const array_sortBy = (arr, f) => {
            arr.sort((a, b) => basics_compare(f.usr(...f.ctx, a), f.usr(...f.ctx, b)));
            return null;
        }

        const arrayToListLow = (arr) => {
          const length = arr.length;
          let list = [ '
    """
    .. listNilName
    .. """
    ' ];
          for (let i = length - 1; i >= 0; i--) {
              list = [ '
    """
    .. listConsName
    .. """
    ', arr[i], list ];
          }
          return list;
        }

        const array_toList = arrayToListLow;


        const arrayFromListLow = (list) => {
          const array = [];
          const rec = (ls) => {
            if (ls[0] === '
    """
    .. listNilName
    .. """
    ')
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
          return [ '
    """
    .. listConsName
    .. """
    ', item, list];
        }

        const list_sortBy = (f, list) => arrayToListLow(arrayFromListLow(list).sort((a, b) => basics_compare(f.usr(...f.ctx, a), f.usr(...f.ctx, b))));


        //
        // Dynamic loading
        //
        const self_load = (requestedTypeHumanized, pars, variantConstructor) => {

            const re = (s) => s.replace(/[" \\n]/g, '');

            const actualTypeHumanized = sp_toHuman(pars.type);
            if (re(actualTypeHumanized) !== re(requestedTypeHumanized)) {
                return """ .. errRef .. """(pars.type);
            }

            const tUsrToString = (tUsr) => array_fromList(tUsr).join('$');

            const js = c0$BuildInfo$compile(arrayToListLow([]), pars);

            //   { name1, name2, name3, ... } = externals;
            const unpackExterns = ''; //'const { ' + pars.externalValues.map((e) => tUsrToString(e.usr)).join(', ') + ' } = externs;';

            const body = `{ ${unpackExterns}\n${js}; return ${tUsrToString(pars.entryUsr)}; }`;

            const arg = {};
            //pars.externalValues.forEach((e) => arg[tUsrToString(e.usr)] = e.self.value);

            return """ .. okRef .. """ (variantConstructor(Function('externs', body)(arg)));
        };


    """
