

id level =
    as Int: Text
    Text.repeat level "  "


emitStatement l stat =
    as Int: JA.Statement: Text

    std mid expr =
        id l .. mid .. emitExpr l expr .. ";"

    try stat as
        JA.Eval e:
            std "" e

        JA.Return e:
            std "return " e

        JA.Define name e:
            std ("const " .. name .. " = ") e

        JA.If condition block:
            id l .. "if (" .. emitExpr l condition .. ") " .. emitBlock l block


emitBlock l block =
    as Int: List JA.Statement: Text
    lines =
        block
            >> List.map (emitStatement (l + 1))
            >> Text.join "\n"

    "{\n" .. lines .. "\n" .. id l .. "}"


# TODO reduce the number of parens?
emitExpr l expression =
    as Int: JA.Expr: Text
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
            if attrs == Dict.empty:
                "{}"

            else
                attrs
                    >> Dict.toList
                    >> List.sortBy Tuple.first
                    >> List.map (fn ( key & value ): id (l + 1) .. key .. ": " .. emitExpr (l + 1) value .. ",")
                    >> (fn a: "({\n" .. Text.join "\n" a .. "\n" .. id l .. "})")

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
            if items == []:
                "[]"

            else
                items
                    >> List.map (fn i: id (l + 1) .. emitExpr (l + 1) i .. ",")
                    >> (fn a: "([\n" .. Text.join "\n" a .. "\n" .. id l .. "])")

