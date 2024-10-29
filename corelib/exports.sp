
module =
   path = Core
   exposes =
      None
      Bool
      Text
      List
      Number
      'none
      'true
      'false


module =
   path = Debug
   exposes =
      log
      todo
      toHuman
      benchStart
      benchStop


module =
    path = Array_Test
    exposes =
        tests


module =
   path = Array
   exposes =
      Array
      push
      pop
      get
      set
      sortBy
      fromList
      toList
      each


module =
    path = List_Test
    exposes =
        tests


module =
   path = List
   exposes =
      any
      all
      find
      findMap
      contains
      sort
      sortBy
      indexBy
      for
      for2
      forWithIndex
      for2WithIndex
      forReversed
      length
      map
      map2
      mapRes
      forRes
      range
      mapWithIndex
      map2WithIndex
      append
      concat
      mapConcat
      partition
      head
      last
      take
      takeFast
      takeTailRec
      takeReverse
      takeWhile
      filter
      filterMap
      mapFirst
      each
      each2WithIndex
      reverse
      repeat
      drop
      minimum
      maximum
      circularPairs
      intersperse
      partitionWhile

module =
    path = Maybe
    exposes =
        Maybe
        'just
        'nothing
        onJust
        map
        toResult
        mapRes
        withDefault


module =
    path = Text
    exposes =
        forEach
        length
        slice
        fromNumber
        toNumber
        startsWith
        trimLeft
        trimRight
        toLower
        toUpper
        dropLeft
        dropRight
        padLeft
        padRight
        repeat
        replace
        startsWithRegex
        replaceRegex
        split
        contains
        join

module =
    path = Tuple
    exposes =
        first
        second
        mapFirst
        mapSecond
        mapBoth
        pair



module =
    path = Basics
    exposes =
        Int
        compare
        identity
        not
        applyIf
        btw
        cloneUni
        cloneImm
        max
        min
        clamp
        round
        modBy


module =
    path = Hash_Test
    exposes =
        tests


module =
    path = Hash
    exposes =
        Hash
        insert
        remove
        get
        for_
        for
        each
        fromList
        toList
        pop


module =
    path = Dict_Test
    exposes =
        tests


module =
    path = Dict
    exposes =
        Dict
        empty
        get
        has
        length
        insert
        remove
        update
        ofOne

        join
        intersect
        diff
        merge
        onlyBothOnly
        map
        mapWithKey
        mapRes
        mapKeys
        each
        for
        forRes
        forReversed
        filter
        filterWithKey
        partition
        any
        keys
        values
        toList
        fromList

module =
    path = Set
    exposes =
      Set
      empty
      member
      size
      isEmpty
      insert
      remove
      ofOne
      join
      intersect
      diff
      map
      for
      toList
      fromList

module =
    path = Result
    exposes =
      Result
      'ok
      'err
      onOk
      onErr
      resolveErrorsWith
      map
      mapError
      fromMaybe
      withDefault


module =
    path = Self_Test
    exposes =
        tests


module =
    path = Self
    exposes =
        Value
        Self
        Def
        'value
        'openVarType
        'openAliasType
        'opaqueType
        internalRepresentation
        LoadPars
        load
        toCaModules


module =
   path = Compiler/EmittableAst
   exposes =
        Name
        UnivarId
        Uniqueness
        'uni
        'imm
        'depends
        toImm
        toUni

        TranslatedUsr

        RawType
        'typeExact
        'typeFn
        'typeVar
        'typeRecord
        ParType
        'parRe
        'parSp
        FullType

        Expression
        'literalText
        'literalNumber
        'localVariable
        'globalVariable
        'placeholderVariable
        'call
        'fn
        'conditional
        'and
        'isLiteralText
        'isLiteralNumber
        'letIn
        'constructor
        'constructorAccess
        'isConstructor
        'literalRecord
        'recordAccess
        'missingPattern
        'introspect
        Argument
        'argumentRecycle
        'argumentSpend
        GlobalDefinition

