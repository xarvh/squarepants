lazy loading
------------
I need a way to map any UMR to a valid filename?
  Can I just use a base64 representation of the UMR?

  I need an umrToValidFilename as Text: Text





init as LoadAllModulesState:
    definitions_required = compile target
    definitions_used = []
    loaded_modules = []


load_all_modules as LoadAllModulesState: LoadAllModulesState & <LoadModules [ModuleName], Errors [Error], Done>
    state:

    required_modules = for any def in definitions_required insert module name

    modules_to_load = required_modules - loaded_modules

    if modules_to_load /= []
      LoadModules modules_to_load

    else
        try definitions_required as
          []: Ok

          [first, ...rest]:
            if first is in module then
              { state with
              , definitions_required = rest
              , definitions_used = def
              }
            else
              Error "module X does not contain definition Y"


1) starting from the compile target

    load canonical modules that contain the required type, cons and value dependencies

        try to load the module's cached typecheck if available

        for each ValueDef, calculate a SHA:
          - based on its annotation, present or not, to tell whether its TYPE has changed
          - based on its body, to tell whether its IMPLEMENTATION has changed

        also resolve root pattern definitions
          union RootName = Simple Text, Composite (Set Text)
          patterns with zero names are not allowed
          PatternAny is transformed directly without splitting


    add the required defs in a Dict
    stop once all required defs are loaded


2) order all required ValueDefs by dependency

3) go through all defs in order, if any of a def's dependencies is marked as changed, mark the def as changed

4) typecheck all changed defs

      ? Stuff that is not annotated can be checked as if it was declared in a let..in?
        -> only annotated stuff can be seen outside of a module, which means that if main is required to be annotated, then everything actually used will be typechecked?

5) for each loaded module
      store the checked status of each ValueDef, together with the valuedef (TYPE or IMPLEMENTATION?) SHA
        - Passed
        - Failed
        - NotChecked


