module StrPack

export @struct
export pack, unpack, sizeof
export DataAlign
export align_default, align_packed, align_packmax, align_structpack, align_table
export align_x86_pc_linux_gnu, align_native
export show_struct_layout

using Meta

import Base.read, Base.write
import Base.isequal

bswap(c::Char) = identity(c) # white lie which won't work for multibyte characters in UTF-16 or UTF-32

# Represents a particular way of adding bytes to maintain certain alignments
immutable DataAlign
    ttable::Dict
    # default::(Type -> Integer); used for bits types not in ttable
    default::Function
    # aggregate::(Vector{Type} -> Integer); used for composite types not in ttable
    aggregate::Function
end
DataAlign(def::Function, agg::Function) = DataAlign((Type=>Integer)[], def, agg)

immutable Struct
    asize::Dict
    strategy::DataAlign
    endianness::Symbol
end

const STRUCT_REGISTRY = Dict{Type, Struct}()

macro struct(xpr...)
    (typname, typ, asize) = extract_annotations(xpr[1])
    if length(xpr) > 3
        error("too many arguments supplied to @struct")
    end
    if length(xpr) > 2
        if isexpr(xpr[3], :quote) && any(xpr[3] .== keys(endianness_converters))
            endianness = xpr[3]
        else
            error("$(string(xpr[3])) is not a valid endianness")
        end
        alignment = xpr[2]
    elseif length(xpr) > 1
        if isexpr(xpr[2], :quote) && any(xpr[2] .== keys(endianness_converters))
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
    new_struct = :(Struct($asize, $alignment, $endianness))
    quote
        $(esc(typ))
        STRUCT_REGISTRY[$(esc(typname))] = $new_struct
        const fisequal = isequal
        fisequal(a::$(esc(typname)), b::$(esc(typname))) = begin
            for name in $(esc(typname)).names
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
        error("only type definitions can be supplied to @struct")
    end
    asize = :(Dict([$(fieldnames...)], Array{Integer,1}[$(asizes...)]))
    (typname, exprIn, asize)
end

endianness_converters = {
    :BigEndian => (hton, ntoh),
    :LittleEndian => (htol, ltoh),
    :NativeEndian => (identity, identity),
    }

# A byte of padding
bitstype 8 PadByte
write(s::IO, x::PadByte) = write(s, 0x00)
read(s::IO, ::Type{PadByte}) = read(s, Uint8)

function isbitsequivalent{T}(::Type{T})
    if isbits(T) || T <: String && !T.abstract
        return true
    elseif isempty(T.names)
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

function chktype{T}(::Type{T})
    if isempty(T.names)
        error("Type $T is not an aggregate type.")
    end
    if !isbitsequivalent(T)
        error("Type $T is not bits-equivalent.")
    end
end

function unpack{T}(in::IO, ::Type{T}, asize::Dict, strategy::DataAlign, endianness::Symbol)
    chktype(T)
    tgtendianness = endianness_converters[endianness][2]
    offset = 0
    rvar = Any[]
    for (typ, name) in zip(T.types, T.names)
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
        offset += if intyp <: String
            push!(rvar, rstrip(convert(typ, read(in, Uint8, dims...)), "\0"))
            prod(dims)
        elseif !isempty(intyp.names)
            if typ <: AbstractArray
                item = Array(intyp, dims...)
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
function unpack{T}(in::IO, ::Type{T}, endianness::Symbol)
    reg = STRUCT_REGISTRY[T]
    unpack(in, T, reg.asize, reg.strategy, endianness)
end
function unpack{T}(in::IO, ::Type{T})
    reg = STRUCT_REGISTRY[T]
    unpack(in, T, reg.asize, reg.strategy, reg.endianness)
end

function pack{T}(out::IO, struct::T, asize::Dict, strategy::DataAlign, endianness::Symbol)
    chktype(T)
    tgtendianness = endianness_converters[endianness][1]
    offset = 0
    for (typ, name) in zip(T.types, T.names)
        if typ <: AbstractArray
            typ = eltype(typ)
        end
        data = if typ <: String
            typ = Uint8
            convert(Array{Uint8}, getfield(struct, name))
        else
            getfield(struct, name)
        end

        offset += write(out, zeros(Uint8, pad_next(offset, typ, strategy)))

        numel = prod(get(asize, name, 1))
        idx_end = numel > 1 ? min(numel, length(data)) : 1
        if !isempty(typ.names)
            if typeof(data) <: AbstractArray
                for i in 1:idx_end
                    offset += pack(out, data[i])
                end
                offset += write(out, zeros(Uint8, calcsize(typ)*(numel-idx_end)))
            else
                offset += pack(out, data)
            end
        else
            offset += if typeof(data) <: AbstractArray
                write(out, map(tgtendianness, data[1:idx_end]))
            else
                write(out, tgtendianness(data))
            end
            offset += write(out, zeros(typ, max(numel-idx_end, 0)))
        end
    end
    offset += write(out, zeros(Uint8, pad_next(offset, T, strategy)))
end
function pack{T}(out::IO, struct::T, endianness::Symbol)
    reg = STRUCT_REGISTRY[T]
    pack(out, struct, reg.asize, reg.strategy, endianness)
end
function pack{T}(out::IO, struct::T)
    reg = STRUCT_REGISTRY[T]
    pack(out, struct, reg.asize, reg.strategy, reg.endianness)
end

# Convenience methods when you just want to use strings
macro withIOString(iostr, ex)
    quote
        $iostr = IOString()
        $ex
        $iostr
    end
end

pack{T}(struct::T, a::Dict, s::DataAlign, n::Symbol) = @withIOString iostr pack(iostr, a, s, n)
pack{T}(struct::T) = @withIOString iostr pack(iostr, struct)

unpack{T}(str::Union(String, Array{Uint8,1}), ::Type{T}) = unpack(IOString(str), T)

## Alignment strategies and utility functions ##

# default alignment for bitstype T is nextpow2(sizeof(::Type{T}))
type_alignment_default{T<:AbstractArray}(::Type{T}) = type_alignment_default(eltype(T))
type_alignment_default{T<:String}(::Type{T}) = 1
type_alignment_default{T}(::Type{T}) = nextpow2(sizeof(T))

# default strategy
align_default = DataAlign(type_alignment_default, x -> max(map(type_alignment_default, x)))

# equivalent to __attribute__ (( __packed__ ))
align_packed = DataAlign(_ -> 1, _ -> 1)

# equivalent to #pragma pack(n)
align_packmax(da::DataAlign, n::Integer) = DataAlign(
    da.ttable,
    _ -> min(type_alignment_default(_), n),
    da.aggregate,
    )

# equivalent to __attribute__ (( align(n) ))
align_structpack(da::DataAlign, n::Integer) = DataAlign(
    da.ttable,
    da.default,
    _ -> n,
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
    [
    Int64 => 4,
    Uint64 => 4,
    Float64 => 4,
    ])

# Get alignment for a given type
function alignment_for(strategy::DataAlign, T::Type)
    if has(strategy.ttable, T)
        strategy.ttable[T]
    elseif !isempty(T.names)
        strategy.aggregate(T.types)
    else
        strategy.default(T)
    end
end

function pad_next(offset, typ, strategy::DataAlign)
    align_to = alignment_for(strategy, typ)
    (align_to - offset % align_to) % align_to
end

function calcsize{T}(::Type{T}, asize::Dict, strategy::DataAlign)
    chktype(T)
    size = 0
    for (typ, name) in zip(T.types, T.names)
        dims = get(asize, name, 1)
        typ = if typ <: Array
            eltype(typ)
        elseif typ <: String
            Uint8
        else
            typ
        end
        size += pad_next(size, typ, strategy)
        size += if isbits(typ)
            prod(dims)*sizeof(typ)
        elseif !isempty(typ.names)
            prod(dims)*sizeof(Struct(typ))
        else
            error("Improper type $typ in struct.")
        end
    end
    size += pad_next(size, T, strategy)
    size
end
calcsize{T}(::Type{T}) = calcsize(T, STRUCT_REGISTRY[T].asize, STRUCT_REGISTRY[T].strategy)

function show_struct_layout{T}(::Type{T}, asize::Dict, strategy::DataAlign, width, bytesize)
    chktype(T)
    offset = 0
    for (typ, name) in zip(T.types, T.names)
        dims = get(asize, name, 1)
        intyp = if typ <: Array
            eltype(typ)
        elseif typ <: String
            Uint8
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
show_struct_layout{T}(::Type{T}) = show_struct_layout(T, STRUCT_REGISTRY[T].asize, STRUCT_REGISTRY[T].strategy, 8, 10)
show_struct_layout{T}(::Type{T}, width::Integer, bytesize::Integer) = show_struct_layout(T, STRUCT_REGISTRY[T].asize, STRUCT_REGISTRY[T].strategy, width, bytesize)
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
    i8a, i16a, i32a, i64a, f32a, f64a = Array(Uint, 1), Array(Uint, 1), Array(Uint, 1), Array(Uint, 1), Array(Uint, 1), Array(Uint, 1)

    ccall("jl_native_alignment", Void,
          (Ptr{Uint}, Ptr{Uint}, Ptr{Uint}, Ptr{Uint}, Ptr{Uint}, Ptr{Uint}),
          i8a, i16a, i32a, i64a, f32a, f64a)

    [
     Int8 => i8a[1],
     Uint8 => i8a[1],
     Int16 => i16a[1],
     Uint16 => i16a[1],
     Int32 => i32a[1],
     Uint32 => i32a[1],
     Int64 => i64a[1],
     Uint64 => i64a[1],
     Float32 => f32a[1],
     Float64 => f64a[1],
     ]
end)

end
