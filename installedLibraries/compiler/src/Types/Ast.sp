#
# The name of a variable, type, attribute or "visibleAs" module name
#
Name =
    Text


#
# A reference to a defined variable
#
var Ref =
    , # This is for stuff defined inside the current function/block
      'refLocal Name
    , # This is for stuff defined at root level
      'refGlobal USR
    , # This is for generated function argument placeholders
      'refPlaceholder Int
