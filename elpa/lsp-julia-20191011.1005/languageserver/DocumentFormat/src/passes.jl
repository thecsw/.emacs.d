function operator_pass(x, state)
    if x.typ === CSTParser.BinaryOpCall
        if CSTParser.precedence(x.args[2]) in (8, 13, 14, 16) || x.args[2].fullspan == 0
            ensure_no_space_after(x.args[1], state, state.offset)
            ensure_no_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        else
            ensure_single_space_after(x.args[1], state, state.offset)
            ensure_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        end
    elseif x.typ === CSTParser.WhereOpCall
        ensure_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        n = length(x.args)
        offset = state.offset + x.args[1].fullspan + x.args[2].fullspan
        for i = 3:n
            a = x.args[i]
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif x.typ === CSTParser.ColonOpCall
        offset = state.offset
        n = length(x.args)
        for (i, a) in enumerate(x.args)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif x.typ === CSTParser.ChainOpCall || x.typ == CSTParser.Comparison
        offset = state.offset
        n = length(x.args)
        for (i, a) in enumerate(x.args)
            if i != n
                ensure_single_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function tuple_pass(x, state)
    if x.typ === CSTParser.TupleH
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            i == n && continue
            if a.typ === CSTParser.PUNCTUATION && a.kind === Tokens.COMMA && !(x.args[i + 1].typ === CSTParser.PUNCTUATION)
                ensure_single_space_after(a, state, offset)
            elseif !(x.args[i + 1].typ === CSTParser.Parameters)
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function curly_pass(x, state)
    if x.typ === CSTParser.Curly
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function call_pass(x, state)
    if x.typ === CSTParser.Call
        offset = state.offset + x.args[1].fullspan
        n = length(x)
        for (i, a) in enumerate(x)
            i == 1 && continue
            if a.typ === CSTParser.PUNCTUATION && a.kind === Tokens.COMMA
                ensure_single_space_after(a, state, offset)
            elseif i != n && !(x.args[i + 1].typ === CSTParser.Parameters)
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif x.typ === CSTParser.Kw
        ensure_single_space_after(x.args[1], state, state.offset)
        ensure_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
    end
end

function forloop_pass(x, state)
    if x.typ === CSTParser.For
        offset = state.offset + x.args[1].fullspan
        for a in x.args[2]
            # convert iter = I into iter in I
            if a.typ === CSTParser.BinaryOpCall && CSTParser.is_eq(a.args[2])
                offset += a.args[1].fullspan
                push!(state.edits, Edit(offset + 1:offset + 2, "in "))
                offset += a.args[2].fullspan
                offset += a.args[3].fullspan
            else
                offset += a.fullspan
            end
        end
    end
end

# TODO: move this to CSTParser?
function str_value(x)
    if x.typ === CSTParser.PUNCTUATION
        x.kind == Tokens.LPAREN && return "("
        x.kind == Tokens.LBRACE && return "{"
        x.kind == Tokens.LSQUARE && return "["
        x.kind == Tokens.RPAREN && return ")"
        x.kind == Tokens.RBRACE && return "}"
        x.kind == Tokens.RSQUARE && return "]"
        x.kind == Tokens.COMMA && return ","
        x.kind == Tokens.SEMICOLON && return ";"
        x.kind == Tokens.AT_SIGN && return "@"
        x.kind == Tokens.DOT && return "."
        return ""
    elseif x.typ === CSTParser.IDENTIFIER || x.typ === CSTParser.LITERAL || x.typ === CSTParser.OPERATOR || x.typ === CSTParser.KEYWORD
        return CSTParser.str_value(x)
    else
        s = ""
        for a in x
            s *= str_value(a)
        end
        return s
    end
end


function doc_pass(x, state)
    return
    if x.typ === CSTParser.MacroCall && x.args[1].typ === CSTParser.GlobalRefDoc
        # Align global docstring to:
        #
        # """
        # doc
        # """
        #
        # If the doc is single quoted i.e. "doc", they will be replaced with triple quotes.
        offset = state.offset + x.args[1].fullspan
        doc = x.args[2]

        val = str_value(doc)

        # s = escape_string(strip(val, ['\n']), "\$")
        s = strip(val, ['\n'])
        ds = string("\"\"\"\n", s, "\n", "\"\"\"\n")

        # Check if docstring needs to be edited
        if length(ds) != doc.fullspan || s != val
            # Remove previous docstring
            push!(state.edits, Edit(offset + 1:offset + doc.fullspan, ""))
            # Append newly formatted docstring
            push!(state.edits, Edit(offset, ds))
        end
    end
end
