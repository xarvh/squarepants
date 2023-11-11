
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
   path = List
   exposes =
      any
      all
      find
      findMap
      member
      sort
      sortBy
      indexBy
      for
      for2
      indexedFor
      indexedFor2
      forReversed
      forReversed2
      length
      map
      map2
      mapRes
      forRes
      range
      indexedMap
      indexedMap2
      append
      concat
      concatMap
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
      indexedEach2
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
    path = Dict
    exposes =
        Dict
        empty
        get
        member
        size
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
        mapRes
        mapKeys
        each
        for
        forRes
        forReversed
        filter
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
      map
      mapError
      fromMaybe
      withDefault

module =
    path = Self
    exposes =
        Value
        Self
        introspect
        internalRepresentation
        LoadPars
        load

module =
   path = Compiler/Ast
   exposes =
      Name
      Ref
      UnivarId
      Uniqueness
      'refLocal
      'refGlobal
      'refPlaceholder
      'uni
      'imm
      'depends
      toImm
      toUni

module =
   path = Compiler/TypedAst

module =
   path = Compiler/CanonicalAst

module =
   path = Compiler/EmittableAst

module =
   path = Compiler/FormattableAst

module =
    path = Compiler/Meta
    exposes =
        Imports
        Exports
        ByUsr
        USR
        UMR
        Source
        DependencyType
        'USR
        'UMR
        'valueDependency
        'constructorDependency
        'typeDependency

module =
    path = Compiler/Op

module =
   path = Compiler/Pos
   exposes =
        Pos
        At
        'at

module =
    path = Compiler/Token
    exposes =
        Token
        'token

