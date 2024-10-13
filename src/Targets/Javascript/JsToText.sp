id as fn Int: Text =
    fn level:
    Text.repeat level "  "


emitStatement as fn Int, JA.Statement: Text =
    fn l, stat:
    std =
        fn mid, expr:
        id l .. mid .. emitExpr l expr .. ";"

    try stat as

        JA.'eval e:
            std "" e

        JA.'return e:
            std "return " e

        JA.'define isReassignable name e:
            modifier =
                if isReassignable then "let" else "const"

            std (modifier .. " " .. name .. " = ") e

        JA.'if condition block:
            id l .. "if (" .. emitExpr l condition .. ") " .. emitBlock l block


emitBlock as fn Int, List JA.Statement: Text =
    fn l, block:
    lines =
        block
        >> List.map (emitStatement (l + 1) __) __
        >> Text.join "\n" __

    "{\n" .. lines .. "\n" .. id l .. "}"


# TODO reduce the number of parens?
emitExpr as fn Int, JA.Expr: Text =
    fn l, expression:
    try expression as

        JA.'literal s:
            s

        JA.'var n:
            n

        JA.'call ref args:
            "(" .. emitExpr l ref .. ")(" .. Text.join ", " (List.map (emitExpr l __) args) .. ")"

        JA.'unop op left:
            op .. "(" .. emitExpr l left .. ")"

        JA.'binop op left right:
            "(" .. emitExpr l left .. " " .. op .. " " .. emitExpr l right .. ")"

        JA.'mutop op yield left right:
            "(" .. emitExpr l left .. " " .. op .. " " .. emitExpr l right .. ", " .. yield .. ")"

        JA.'simpleLambda params expr:
            "((" .. Text.join ", " params .. ") => " .. emitExpr l expr .. ")"

        JA.'blockLambda params stats:
            "((" .. Text.join ", " params .. ") => " .. emitBlock l stats .. ")"

        JA.'record attrs:
            if attrs == Dict.empty then
                "{}"
            else
                attrs
                >> Dict.toList
                >> List.sortBy Tuple.first __
                >> List.map (fn key & value: id (l + 1) .. key .. ": " .. emitExpr (l + 1) value .. ",") __
                >> (fn a: "({\n" .. Text.join "\n" a .. "\n" .. id l .. "})")

        JA.'accessWithDot name e:
            emitExpr l e .. "." .. name

        JA.'accessWithBrackets i expr:
            "(" .. emitExpr l expr .. ")[" .. emitExpr l i .. "]"

        JA.'conditional p true false:
            ("(" .. emitExpr l p .. "\n")
            .. (id (l + 1) .. "? " .. emitExpr (l + 1) true)
            .. "\n"
            .. (id (l + 1) .. ": " .. emitExpr (l + 1) false)
            .. ")"

        JA.'array items:
            if items == [] then
                "[]"
            else
                items
                >> List.map (fn i: id (l + 1) .. emitExpr (l + 1) i .. ",") __
                >> (fn a: "([\n" .. Text.join "\n" a .. "\n" .. id l .. "])") __

        JA.'comma expr:
            "(" .. Text.join ", " (List.map (emitExpr l __) expr) .. ")"

        JA.'threeDots expr:
            "...(" .. emitExpr l expr .. ")"
