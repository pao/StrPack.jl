StrPack: Structured Binary Stream Tools
=======================================

One of the things I find annoying about MATLAB is dealing with binary data. There's a lot of boilerplate, a lot of `fread()`, and some weird thing involving a `=>`.

Enter StrPack. StrPack decodes binary streams to Julia composite types, handling stream endianness and padding bytes for the source ABI along the way. StrPack also encodes instances of Julia composite types to binary streams, setting endianness and adding padding if required to meet an ABI. Along with Julia's `IOString` type, StrPack can also be used to convert between Julia composite types and buffers suitable for some C function arguments.

StrPack is not a serializer/deserializer. StrPack only handles Julia's [bits types](http://docs.julialang.org/en/latest/manual/types/#id1) or user types for which `read(io, ::UserType)` and `write(io, data::UserType)` have been defined. However, you could use StrPack to build those things.
