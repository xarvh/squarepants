

id as Int: Text =
    level:
    Text.repeat level "  "


emitStatement as Int: JA.Statement: Text =
    l: stat:

    std = mid: expr:
        id l .. mid .. emitExpr l expr .. ";"

    try stat as
        JA.Eval e:
            std "" e

        JA.Return e:
            std "return " e

        JA.Define isReassignable name e:
            modifier = if isReassignable then "let" else "const"
            std (modifier .. " " .. name .. " = ") e

        JA.If condition block:
            id l .. "if (" .. emitExpr l condition .. ") " .. emitBlock l block


emitBlock as Int: List JA.Statement: Text =
    l: block:
    lines =
        block
            >> List.map (emitStatement (l + 1))
            >> Text.join "\n"

    "{\n" .. lines .. "\n" .. id l .. "}"


# TODO reduce the number of parens?
emitExpr as Int: JA.Expr: Text =
    l: expression:
    try expression as
        JA.Literal s:
            s

        JA.Var n:
            n

        JA.Call ref args:
            "(" .. emitExpr l ref .. ")(" .. Text.join ", " (List.map (emitExpr l) args) .. ")"

        JA.Unop op left:
            op .. "(" .. emitExpr l left .. ")"

        JA.Binop op left right:
            "(" .. emitExpr l left .. " " .. op .. " " .. emitExpr l right .. ")"

        JA.Mutop op yield left right:
            "(" .. emitExpr l left .. " " .. op .. " " .. emitExpr l right .. ", " .. yield .. ")"

        JA.SimpleLambda params expr:
            "((" .. Text.join ", " params .. ") => " .. emitExpr l expr .. ")"

        JA.BlockLambda params stats:
            "((" .. Text.join ", " params .. ") => " .. emitBlock l stats .. ")"

        JA.Record attrs:
            if attrs == Dict.empty then
                "{}"

            else
                attrs
                    >> Dict.toList
                    >> List.sortBy Tuple.first
                    >> List.map (( key & value ): id (l + 1) .. key .. ": " .. emitExpr (l + 1) value .. ",")
                    >> (a: "({\n" .. Text.join "\n" a .. "\n" .. id l .. "})")

        JA.AccessWithDot name e:
            emitExpr l e .. "." .. name

        JA.AccessWithBrackets i expr:
            "(" .. emitExpr l expr .. ")[" .. emitExpr l i .. "]"

        JA.Conditional p true false:
            ("(" .. emitExpr l p .. "\n")
                .. (id (l + 1) .. "? " .. emitExpr (l + 1) true)
                .. "\n"
                .. (id (l + 1) .. ": " .. emitExpr (l + 1) false)
                .. ")"

        JA.Array items:
            if items == [] then
                "[]"

            else
                items
                    >> List.map (i: id (l + 1) .. emitExpr (l + 1) i .. ",")
                    >> (a: "([\n" .. Text.join "\n" a .. "\n" .. id l .. "])")

        JA.Comma expr:
            "(" .. Text.join ", " (List.map (emitExpr l) expr) .. ")"

