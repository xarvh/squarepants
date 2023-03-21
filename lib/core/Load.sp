#
# Dynamic loading
#

#union ExposedValue =
#    ExposedValue
#    #Never ExposedValue
#
#
#expose as fn a: ExposedValue =
#    fn a:
#    todo "native"


#
# TODO this makes core dependent on an external lib
# Might need to move all of Compiler into core
#
dynamicLoad as fn Compiler/Compiler.CompileModulesOut, (fn specific: general): Result Text general =
    todo "platform does not override dynamicLoad"

