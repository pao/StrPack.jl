__precompile__(false)
module StrPack

export @struct
export pack, unpack, sizeof
export DataAlign
export align_default, align_packed, align_packmax, align_structpack, align_table
export align_x86_pc_linux_gnu, align_native
export show_struct_layout

using Base.Meta
using Printf

import Base.read, Base.write
import Base.isequal

bswap(c::Char) = identity(c) # white lie which won't work for multibyte characters in UTF-16 or UTF-32

# Represents a particular way of adding bytes to maintain certain alignments
struct DataAlign
    ttable::Dict
    # default::(Type -> Integer); used for bits types not in ttable
    default::Function
    # aggregate::(Vector{Type} -> Integer); used for composite types not in ttable
    aggregate::Function
end
DataAlign(def::Function, agg::Function) = DataAlign((Dict{Type,Integer}()), def, agg)

struct Str
    asize::Dict
    strategy::DataAlign
    endianness::Symbol
end

@eval macro $(Symbol("struct"))(xpr...)
    (typname, typ, asize) = extract_annotations(xpr[1])
    if length(xpr) > 3
        error("too many arguments supplied to @str")
    end
    if length(xpr) > 2
        if isexpr(xpr[3], :quote) && haskey(endianness_converters, eval(xpr[3]))
            endianness = xpr[3]
        else
            error("$(string(xpr[3])) is not a valid endianness")
        end
        alignment = xpr[2]
    elseif length(xpr) > 1
        if isexpr(xpr[2], :quote) && haskey(endianness_converters, eval(xpr[2]))
            endianness = xpr[2]
            alignment = :(align_default)
        else
            alignment = xpr[2]
            endianness = :(:NativeEndian)
        end
    else
        alignment = :(align_default)
        endianness = :(:NativeEndian)
    end
    new_struct = :(Str($asize, $alignment, $endianness))
    quote
        $(esc(typ))
        $(esc(:(isdefined(:STRUCT_REGISTRY) || const STRUCT_REGISTRY = ObjectIdDict())))
        $(esc(:(STRUCT_REGISTRY[$typname]))) = $new_struct
        const fisequal = isequal
        fisequal(a::$(esc(typname)), b::$(esc(typname))) = begin
            for name in fieldnames($(esc(typname)))
                if !isequal(getfield(a, name), getfield(b, name))
                    return false
                end
            end
            true
        end
    end
end

headof(xpr, sym) = false
headof(xpr::Expr, sym) = (xpr.head == sym)

# hmm...AST selectors could be useful
function extract_annotations(exprIn)
    fieldnames = Expr[]
    asizes = Expr[]
    typname = nothing
    if isexpr(exprIn, :type)
        if isa(exprIn.args[2],Symbol)
            typname = exprIn.args[2]
        elseif isexpr(exprIn.args[2],:<:)
            typname = exprIn.args[2].args[1]
        else
            error("Unable to extract type name!")
        end
        for field_xpr in exprIn.args[3].args
            if isexpr(field_xpr, :(::)) && isexpr(field_xpr.args[2], :call)
                push!(fieldnames, quot(field_xpr.args[1]))
                push!(asizes, quot(Integer[field_xpr.args[2].args[2:end]...]))
                # turn this back into a legal type expression
                field_xpr.args[2] = field_xpr.args[2].args[1]
            end
        end
    else
        error("only type definitions can be supplied to @str")
    end
    asize = :(Dict(zip([$(fieldnames...)], Array{Integer,1}[$(asizes...)])))
    (typname, exprIn, asize)
end

endianness_converters =  Dict(
    :BigEndian => (hton, ntoh),
    :LittleEndian => (htol, ltoh),
    :NativeEndian => (identity, identity),
    :SwappedEndian => (Base.bswap, Base.bswap))

# A byte of padding
primitive type PadByte 8 end
write(s::IO, x::PadByte) = write(s, 0x00)
read(s::IO, ::Type{PadByte}) = read(s, UInt8)

function isbitsequivalent(::Type{T}) where T
    if isbits(T) || T <: AbstractString && !T.abstract
        return true
    elseif isempty(fieldnames(T))
        return false
    end
    # TODO AbstractArray inspect element instead
    for S in T.types
        if S <: AbstractArray
            S = eltype(S)
        end
        if !isbitsequivalent(S)
            return false
        end
    end
    true
end

function chktype(::Type{T}) where T
    if isempty(fieldnames(T))
        error("Type $T is not an aggregate type.")
    end
    if !isbitsequivalent(T)
        error("Type $T is not bits-equivalent.")
    end
end

function unpack(in::IO, ::Type{T}, asize::Dict, strategy::DataAlign, endianness::Symbol) where T
    chktype(T)
    tgtendianness = endianness_converters[endianness][2]
    offset = 0
    rvar = Any[]
    for (typ, name) in zip(T.types, fieldnames(T))
        dims = get(asize, name, 1)
        intyp = if typ <: AbstractArray
            eltype(typ)
        else
            typ
        end

        # Skip padding before next field
        pad = pad_next(offset,intyp,strategy)
        skip(in,pad)
        offset += pad
        offset += if intyp <: AbstractString
            push!(rvar, rstrip(convert(typ, read(in, UInt8, dims...)), ['\0']))
            prod(dims)
        elseif !isempty(fieldnames(intyp))
            if typ <: AbstractArray
                item = Array{intyp}(dims...)
                for i in 1:prod(dims)
                    item[i] = unpack(in, intyp)
                end
                push!(rvar, item)
            else
                push!(rvar, unpack(in, intyp))
            end
            calcsize(intyp)*prod(dims)
        else
            if typ <: AbstractArray
                push!(rvar, map(tgtendianness, read(in, intyp, dims...)))
            else
                push!(rvar, tgtendianness(read(in, intyp)))
            end
            sizeof(intyp)*prod(dims)
        end
    end
    skip(in, pad_next(offset, T, strategy))
    T(rvar...)
end
function unpack(in::IO, ::Type{T}, endianness::Symbol) where T
    chktype(T)
    reg = T.name.module.STRUCT_REGISTRY[T]::Str
    unpack(in, T, reg.asize, reg.strategy, endianness)
end
function unpack(in::IO, ::Type{T}) where T
    chktype(T)
    reg = T.name.module.STRUCT_REGISTRY[T]::Str
    unpack(in, T, reg.asize, reg.strategy, reg.endianness)
end

function pack(out::IO, str::T, asize::Dict, strategy::DataAlign, endianness::Symbol) where T
    chktype(T)
    tgtendianness = endianness_converters[endianness][1]
    offset = 0
    for (typ, name) in zip(T.types, fieldnames(T))
        if typ <: AbstractArray
            typ = eltype(typ)
        end
        data = if typ <: AbstractString
            typ = UInt8
            convert(Array{UInt8}, getfield(str, name))
        else
            getfield(str, name)
        end

        offset += write(out, zeros(UInt8, pad_next(offset, typ, strategy)))

        numel = prod(get(asize, name, 1))
        idx_end = numel > 1 ? min(numel, length(data)) : 1
        if !isempty(fieldnames(typ))
            if typeof(data) <: AbstractArray
                for i in 1:idx_end
                    offset += pack(out, data[i])
                end
                offset += write(out, zeros(UInt8, calcsize(typ)*(numel-idx_end)))
            else
                offset += pack(out, data)
            end
        else
            offset += if typeof(data) <: AbstractArray
                isempty(data) ? 0 : write(out, map(tgtendianness, data[1:idx_end]))
            else
                write(out, tgtendianness(data))
            end
            offset += write(out, zeros(typ, max(numel-idx_end, 0)))
        end
    end
    offset += write(out, zeros(UInt8, pad_next(offset, T, strategy)))
end

zeros(x, n) = Base.zeros(x, n)
zeros(x::Type{Ptr{T}}, n) where T = [x(C_NULL) for i in 1:n]

function pack(out::IO, str::T, endianness::Symbol) where T
    chktype(T)
    reg = T.name.module.STRUCT_REGISTRY[T]
    pack(out, str, reg.asize, reg.strategy, endianness)
end
function pack(out::IO, str::T) where T
    chktype(T)
    reg = T.name.module.STRUCT_REGISTRY[T]
    pack(out, str, reg.asize, reg.strategy, reg.endianness)
end

# Convenience methods when you just want to use strings
macro withIOBuffer(iostr, ex)
    quote
        $iostr = IOBuffer()
        $ex
        $iostr
    end
end

pack(str::T, a::Dict, s::DataAlign, n::Symbol) where T = @withIOBuffer iostr pack(iostr, a, s, n)
pack(str::T) where T = @withIOBuffer iostr pack(iostr, str)

unpack(str::Union{AbstractString, Array{UInt8,1}}, ::Type{T}) where T = unpack(IOBuffer(str), T)

## Alignment strategies and utility functions ##

# default alignment for bitstype T is nextpow2(sizeof(::Type{T}))
type_alignment_default(::Type{T}) where T<:AbstractArray = type_alignment_default(eltype(T))
type_alignment_default(::Type{T}) where T<:AbstractString = 1
type_alignment_default(::Type{T}) where T = nextpow2(sizeof(T))

# default strategy
align_default = DataAlign(type_alignment_default, x -> maximum(map(type_alignment_default, x)))

# equivalent to __attribute__ (( __packed__ ))
align_packed = DataAlign(x -> 1, x -> 1)

# equivalent to #pragma pack(n)
align_packmax(da::DataAlign, n::Integer) = DataAlign(
    da.ttable,
    x -> min(type_alignment_default(x), n),
    da.aggregate,
    )

# equivalent to __attribute__ (( align(n) ))
align_structpack(da::DataAlign, n::Integer) = DataAlign(
    da.ttable,
    da.default,
    x -> n,
    )

# provide an alignment table
align_table(da::DataAlign, ttable::Dict) = DataAlign(
    merge(da.ttable, ttable),
    da.default,
    da.aggregate,
    )

# convenience forms using a default alignment
for fun in (:align_packmax, :align_structpack, :align_table)
    @eval ($fun)(arg) = ($fun)(align_default, arg)
end

# Specific architectures
align_x86_pc_linux_gnu = align_table(align_default,
    Dict(
    Int64 => 4,
    UInt64 => 4,
    Float64 => 4,
    ))

# Get alignment for a given type
function alignment_for(strategy::DataAlign, T::Type)
    if haskey(strategy.ttable, T)
        strategy.ttable[T]
    elseif !isempty(fieldnames(T))
        strategy.aggregate(T.types)
    else
        strategy.default(T)
    end
end

function pad_next(offset, typ, strategy::DataAlign)
    align_to = alignment_for(strategy, typ)
    (align_to - offset % align_to) % align_to
end

function calcsize(::Type{T}, asize::Dict, strategy::DataAlign) where T
    chktype(T)
    size = 0
    for (typ, name) in zip(T.types, fieldnames(T))
        dims = get(asize, name, 1)
        typ = if typ <: Array
            eltype(typ)
        elseif typ <: AbstractString
            UInt8
        else
            typ
        end
        size += pad_next(size, typ, strategy)
        size += if isbits(typ)
            prod(dims)*sizeof(typ)
        elseif !isempty(fieldnames(typ))
            prod(dims)*sizeof(Str(typ))
        else
            error("Improper type $typ in str.")
        end
    end
    size += pad_next(size, T, strategy)
    size
end
calcsize(::Type{T}) where T = calcsize(T, T.name.module.STRUCT_REGISTRY[T].asize, T.name.module.STRUCT_REGISTRY[T].strategy)

function show_struct_layout(::Type{T}, asize::Dict, strategy::DataAlign, width, bytesize) where T
    chktype(T)
    offset = 0
    for (typ, name) in zip(T.types, fieldnames(T))
        dims = get(asize, name, 1)
        intyp = if typ <: Array
            eltype(typ)
        elseif typ <: AbstractString
            UInt8
        else
            typ
        end
        padsize = pad_next(offset, intyp, strategy)
        offset = show_layout_format(PadByte, sizeof(PadByte), padsize, width, bytesize, offset)
        offset = show_layout_format(typ, sizeof(intyp), dims, width, bytesize, offset)
    end
    padsize = pad_next(offset, T, strategy)
    offset = show_layout_format(PadByte, sizeof(PadByte), padsize, width, bytesize, offset)
    if offset % width != 0
        println()
    end
end
show_struct_layout(::Type{T}) where T = show_struct_layout(T, T.name.module.STRUCT_REGISTRY[T].asize, T.name.module.STRUCT_REGISTRY[T].strategy, 8, 10)
show_struct_layout(::Type{T}, width::Integer, bytesize::Integer) where T = show_struct_layout(T, T.name.module.STRUCT_REGISTRY[T].asize, T.name.module.STRUCT_REGISTRY[T].strategy, width, bytesize)
# show_struct_layout(T::Type, asize::Dict, strategy::DataAlign, width) = show_struct_layout(T, strategy, width, 10)

function show_layout_format(typ, typsize, dims, width, bytesize, offset)
    for i in 1:prod(dims)
        tstr = string(typ)[1:min(typsize*bytesize-2, end)]
        str = "[" * tstr * "-"^(bytesize*typsize-2-length(tstr)) * "]"
        typsize_i = typsize
        while !isempty(str)
            if offset % width == 0
                @printf("0x%04X ", offset)
            end
            len_prn = min(width - (offset % width), typsize_i)
            nprint = bytesize*len_prn
            print(str[1:nprint])
            str = str[nprint+1:end]
            typsize_i -= len_prn
            offset += len_prn
            if offset % width == 0
                println()
            end
        end
    end
    offset
end

## Native layout ##
align_native = align_table(align_default, let
    i8a, i16a, i32a, i64a, f32a, f64a = Array{UInt}(undef, 1), Array{UInt}(undef, 1), Array{UInt}(undef, 1), Array{UInt}(undef, 1), Array{UInt}(undef, 1), Array{UInt}(undef, 1)

    ccall("jl_native_alignment", Nothing,
          (Ptr{UInt}, Ptr{UInt}, Ptr{UInt}, Ptr{UInt}, Ptr{UInt}, Ptr{UInt}),
          i8a, i16a, i32a, i64a, f32a, f64a)

     Dict(
     Int8 => i8a[1],
     UInt8 => i8a[1],
     Int16 => i16a[1],
     UInt16 => i16a[1],
     Int32 => i32a[1],
     UInt32 => i32a[1],
     Int64 => i64a[1],
     UInt64 => i64a[1],
     Float32 => f32a[1],
     Float64 => f64a[1],
    )
end)

end
