using DocumentFormat: format
using Test

@testset "All" begin
@testset "basic" begin
    @test format("a") == "a"
end
@testset "tuples" begin
    @test format("a,b") == "a, b"
    @test format("a ,b") == "a, b"
    @test format("a ,b") == "a, b"
    @test format("a ,b") == "a, b"
    @test format("a , b") == "a, b"
    @test format("(a,b)") == "(a, b)"
    @test format("(a ,b)") == "(a, b)"
    @test format("( a, b)") == "(a, b)"
    @test format("(a, b )") == "(a, b)"
    @test format("(a, b ,)") == "(a, b,)"
    @test format("π,汉") == "π, 汉"
    @test format("π ,汉") == "π, 汉"
    @test format("π ,汉") == "π, 汉"
    @test format("π ,汉") == "π, 汉"
    @test format("π , 汉") == "π, 汉"
    @test format("(π,汉)") == "(π, 汉)"
    @test format("(π ,汉)") == "(π, 汉)"
    @test format("( π, 汉)") == "(π, 汉)"
    @test format("(π, 汉 )") == "(π, 汉)"
    @test format("(π, 汉 )") == "(π, 汉)"
end
@testset "curly" begin
    @test format("X{a,b}") == "X{a,b}"
    @test format("X{ a,b}") == "X{a,b}"
    @test format("X{a ,b}") == "X{a,b}"
    @test format("X{a, b}") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
    @test format("X{π,汉}") == "X{π,汉}"
    @test format("X{ π,汉}") == "X{π,汉}"
    @test format("X{π ,汉}") == "X{π,汉}"
    @test format("X{π, 汉}") == "X{π,汉}"
    @test format("X{π,汉 }") == "X{π,汉}"
    @test format("X{π,汉 }") == "X{π,汉}"
end
# @testset "unary ops" begin
#     @test format("! x") == "!x"
# end
@testset "binary ops" begin
    @test format("a+b*c") == "a + b * c"
    @test format("a +b*c") == "a + b * c"
    @test format("a+ b*c") == "a + b * c"
    @test format("a+b *c") == "a + b * c"
    @test format("a+b* c") == "a + b * c"
    @test format("a+b*c ") == "a + b * c "
    @test format("a:b") == "a:b"
    @test format("a : b") == "a:b"
    @test format("a: b") == "a:b"
    @test format("a :b") == "a:b"
    @test format("a:b:c") == "a:b:c"
    @test format("a :b:c") == "a:b:c"
    @test format("a: b:c") == "a:b:c"
    @test format("a:b :c") == "a:b:c"
    @test format("a:b: c") == "a:b:c"
    @test format("a:b:c ") == "a:b:c "
    @test format("a::b:: c") == "a::b::c"
    @test format("a :: b::c") == "a::b::c"
    @test format("π+σ*汉") == "π + σ * 汉"
    @test format("π +σ*汉") == "π + σ * 汉"
    @test format("π+ σ*汉") == "π + σ * 汉"
    @test format("π+σ *汉") == "π + σ * 汉"
    @test format("π+σ* 汉") == "π + σ * 汉"
    @test format("π+σ*汉 ") == "π + σ * 汉 "
    @test format("π:汉") == "π:汉"
    @test format("π : 汉") == "π:汉"
    @test format("π: 汉") == "π:汉"
    @test format("π :汉") == "π:汉"
    @test format("π:σ:汉") == "π:σ:汉"
    @test format("π :σ:汉") == "π:σ:汉"
    @test format("π: σ:汉") == "π:σ:汉"
    @test format("π:σ :汉") == "π:σ:汉"
    @test format("π:σ: 汉") == "π:σ:汉"
    @test format("π:σ:汉 ") == "π:σ:汉 "
    @test format("π::σ:: 汉") == "π::σ::汉"
    @test format("π :: σ::汉") == "π::σ::汉"
    @test format("-π/ 2") == "-π / 2"
end

@testset "op chain" begin
    @test format("a+b+c+d") == "a + b + c + d"
    @test format("π+σ+汉+a汉π") == "π + σ + 汉 + a汉π"
end

@testset "comparison chain" begin
    @test format("a<b==c≥d") == "a < b == c ≥ d"
    @test format("π<σ==汉≥a汉π") == "π < σ == 汉 ≥ a汉π"
end

@testset "colon op" begin
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("π:σ:汉") == "π:σ:汉"
end

# @testset "func call" begin
#     @test format("func(a, b, c)") == "func(a, b, c)"
#     @test format("func(a,b,c)") == "func(a, b, c)"
#     @test format("func(a,b,c,)") == "func(a, b, c,)"
#     @test format("func(a,b,c, )") == "func(a, b, c,)"
#     @test format("func( a,b,c    )") == "func(a, b, c)"
#     @test format("func(a, b, c) ") == "func(a, b, c) "
#     @test format("func(a, b; c)") == "func(a, b; c)"
#     @test format("func(  a, b; c)") == "func(a, b; c)"
#     @test format("func(a  ,b; c)") == "func(a, b; c)"
#     @test format("func(a=1,b; c=1)") == "func(a = 1, b; c = 1)"
# end

@testset "indents" begin
@testset "begin" begin
str = """
begin
    arg
end"""
    @test format("""
                begin
                arg
                end""") == str
    @test format("""
                begin
                    arg
                end""") == str
    @test format("""
                begin
                    arg
                end""") == str
    @test format("""
                begin
                        arg
                end""") == str
    str = """
    begin
        begin
            arg
        end
    end"""
    @test format("""
                begin
                begin
                arg
                end
                end""") == str
    @test format("""
                begin
                            begin
                arg
                end
                end""") == str
    @test format("""
                begin
                            begin
                arg
                        end
                end""") == str
end
end

@testset "quote" begin
    str = """
    quote
        arg
    end"""
    @test format("""
    quote
        arg
    end""") == str
    @test format("""
    quote
    arg
    end""") == str
    @test format("""
    quote
            arg
        end""") == str
end

@testset "do" begin
    str = """
    map(args) do x
        y = 20
        return x * y
    end"""

    @test format("""
    map(args) do x
      y = 20
                        return x * y
        end""") == str

end

@testset "for" begin
    str = """
    for iter in I
        arg
    end"""
    @test format("""
    for iter in I
        arg
    end""") == str
    @test format("""
    for iter in I
    arg
    end""") == str
    @test format("""
    for iter in I
      arg
    end""") == str

    str = """
    for iter in I, iter2 in I2
        arg
    end"""
    @test format("""
    for iter in I, iter2 in I2
        arg
    end""") == str
    @test format("""
    for iter in I, iter2 in I2
    arg
    end""") == str
    @test format("""
    for iter in I, iter2 in I2
            arg
        end""") == str

    str = """
    for iter in I, iter2 in I2
        arg
    end"""
    @test format("""
    for iter=I, iter2 in I2
        arg
    end""", convert_iterator_ops=true) == str
    @test format("""
    for iter =I, iter2 in I2
        arg
    end""", convert_iterator_ops=true) == str
    @test format("""
    for iter =I, iter2 in I2
        arg
    end""", convert_iterator_ops=true) == str
    @test format("""
    for iter = I, iter2 = I2
        arg
    end""", convert_iterator_ops=true) == str
end

@testset "while" begin
    str = """
    while cond
        arg
    end"""
    @test format("""
    while cond
        arg
    end""") == str
    @test format("""
    while cond
    arg
    end""") == str
    @test format("""
    while cond
            arg
        end""") == str
end

@testset "let" begin
    str = """
    let x = X
        arg
    end"""
    @test format("""
    let x=X
        arg
    end""") == str
    @test format("""
    let x=X
    arg
    end""") == str
    @test format("""
    let x=X
        arg
    end""") == str
    str = """
    let x = X, y = Y
        arg
    end"""
    @test format("""
    let x = X, y = Y
        arg
    end""") == str
    @test format("""
    let x = X, y = Y
    arg
    end""") == str

    str = """
    y, back = let
        body
    end"""
    @test format("""
    y,back = let
      body
    end""") == str
end

@testset "struct" begin
    str = """
    struct name
        arg
    end"""
    @test format("""
    struct name
        arg
    end""") == str
    @test format("""
    struct name
    arg
    end""") == str
    @test format("""
    struct name
            arg
        end""") == str
end

@testset "mutable struct" begin
    str = """
    mutable struct name
        arg
    end"""
    @test format("""
    mutable struct name
        arg
    end""") == str
    @test format("""
    mutable struct name
    arg
    end""") == str
    @test format("""
    mutable struct name
            arg
        end""") == str
end

@testset "try-catch" begin
    str = """
    try
        arg
    catch
        arg
    end"""
    @test format("""
    try
        arg
    catch
        arg
    end""") == str
    @test format("""
    try
    arg
    catch
    arg
    end""") == str
    @test format("""
    try
            arg
        catch
            arg
        end""") == str
    str = """
    try
        arg
    catch
        arg
    end"""
    @test format("""
    try
        arg
    catch
        arg
    end""") == str
    @test format("""
    try
    arg
    catch
    arg
    end""") == str
    @test format("""
    try
            arg
        catch
            arg
        end""") == str

    str = """
    try
        arg
    catch err
        arg
    end"""
    @test format("""
    try
        arg
    catch err
        arg
    end""") == str
    @test format("""
    try
    arg
    catch err
    arg
    end""") == str
    @test format("""
    try
            arg
        catch err
            arg
        end""") == str
end

# @testset "docs" begin
#     str = """
#     \"""
#     doc
#     \"""
#     function f()
#         20
#     end"""

#     @test format("""
#     \"""doc
#     \"""
#     function f()
#         20
#     end""") == str
#     @test format("""
#     \"""
#     doc\"""
#     function f()
#         20
#     end""") == str
#     @test format("""
#     \"""doc\"""
#     function f()
#         20
#     end""") == str

#     @test format("""
#     "doc
#     "
#     function f()
#         20
#     end""") == str
#     @test format("""
#     "
#     doc"
#     function f()
#         20
#     end""") == str
#     @test format("""
#     "doc"
#     function f()
#         20
#     end""") == str

#     # tests indentation and correctly formatting a docstring with escapes
#     str = """
#        begin
#            \"""
#                f

#            docstring for f
#            :(function \$(dict[:name]){\$(all_params...)}(\$(dict[:args]...);
#                                                 \$(dict[:kwargs]...))::\$rtype
#            \$(dict[:body])
#            \"""
#            function f()
#                100
#            end
#        end"""
#     @test format("""
#        begin
#        \"""

#            f

#        docstring for f
#        :(function \$(dict[:name]){\$(all_params...)}(\$(dict[:args]...);
#                                             \$(dict[:kwargs]...))::\$rtype
#        \$(dict[:body])

#        \"""
#        function f()
#            100
#        end
#        end""") == str
# end

end
