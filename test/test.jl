using Base.Test
using StrPack

@struct type A
    a::Uint8
end

@struct type B
    a::Array{Uint8,2}(2,2)
end

@struct type C
    a::ASCIIString(5)
end

@struct type D
    a::C
end

@struct type E
    a::Array{C,1}(2)
end

import Base.isequal

for typname in {:A, :B, :C, :D, :E}
    @eval function isequal(a::$typname, b::$typname)
        for name in $typname.names
            if !isequal(getfield(a, name), getfield(b, name))
                return false
            end
        end
        true
    end
end

function roundtrip(a)
    ios = IOString()
    pack(ios, a)
    seek(ios, 0)
    isequal(a, unpack(ios, typeof(a)))
end

@test roundtrip(A(0xbc))

@test !roundtrip(B(zeros(Uint8, 0, 0)))
@test !roundtrip(B([0xab 0xcd]))
@test roundtrip(B([0x01 0x02; 0x03 0x04]))
@test !roundtrip(B([0x01 0x02 0xab; 0x03 0x04 0xcd; 0xac 0xbd 0xef]))

@test roundtrip(C(""))
@test roundtrip(C("a"))
@test roundtrip(C("asdfg"))
@test !roundtrip(C("asdfgh"))

@test roundtrip(D(C("")))

@test !roundtrip(E([C("a")]))
@test roundtrip(E([C("a"); C("b")]))
