StrPack.jl --- Structured Stream I/O
====================================

.. .. module:: StrPack.jl
   :synopsis: Structured Stream I/O

This package performs serialization of structures to and deserialization from binary data streams and string objects.

-------
Example
-------

Let's create a C library as follows:

.. code-block:: c

    struct teststruct {
      int      int1;
      float    float1;
    };
    
    void getvalues(struct teststruct* ts)
    {
      ts->int1 = 7;
      ts->float1 = 3.7;
    }

The ``getvalues`` function just fills the two fields with specified values. Compile this as a shared library,
which on Linux is achieved with ``gcc -fPIC teststruct.c -shared -o libteststruct.so``.

Let's also create the Julia analog of this structure::

    using StrPack

    @struct type TestStruct
        int1::Int32
        float1::Float32
    end
    TestStruct(i, f) = TestStruct(convert(Int32, i), convert(Float32, f))

Note that C's ``int`` corresponds to ``Int32``. Let's initialize an object of this type::

    s = TestStruct(-1, 1.2)
    
We can pack ``s`` into a form suitable to pass as the input to our C function ``getvalues``, which we do in the
following way::

    iostr = IOString()
    pack(iostr, s)

It's worth seeing what has happened here::

    julia> iostr
    IOString([0xff, 0xff, 0xff, 0xff, 0x9a, 0x99, 0x99, 0x3f],9)

The first 4 bytes correspond to the ``Int32`` representation of -1, and the last 4 to the ``Float32``
representation of 1.2. In other words, this is just a packed memory buffer encoding ``s``. (There are
subtleties such as data alignment, endian status, etc. strpack knows about this stuff, and users who need
to control its behavior manually can do so.)

Now we load our library and make a ``ccall``::

    const libtest = dlopen("libteststruct")
    ccall(dlsym(libtest, :getvalues), Void, (Ptr{Void},), iostr.data)

The C function ``getvalues`` stores its output in the buffer we provided as input. We unpack this buffer back
into a Julia type::

    seek(iostr, 0)   # "rewind" to the beginning of the buffer
    s2 = unpack(iostr, TestStruct)

Voila! You have the result back.

.. macro:: @struct(type, strategy, endianness)

    Create and register a structural Julia type with StrPack. The type argument uses an extended form of the standard Julia type syntax to define the size of arrays and strings. Each element must declare its type, and each type must be reducible to a bits type or array or composite of bits types.

        @struct type StructuralType
            a::Float64 # a bits type
            b::Array{Int32,2}(4, 4) # an array of bits types
            c::ASCIIString(8) # a string with a fixed number of bytes
        end

.. function:: pack(io, composite[, asize, strategy, endianness])

    Create a packed buffer representation of ``composite`` in stream ``io``, using array and string sizes fixed by ``asize`` and data alignment coded by ``strategy`` with endianness ``endianness``. If the optional arguments are not provided, then ``composite`` is expected to have been created with the ``@struct`` macro.
    
.. function:: unpack(io, T[, asize, strategy, endianness])

    Extract an instance of the Julia composite type ``T`` from the packed representation in the stream ``io``.
    If the optional arguments are not provided, then ``composite`` is expected to have been created with the ``@struct`` macro.
