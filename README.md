
[![Build](https://img.shields.io/travis/pinselimo/Hasky-Types.svg)](https://travis-ci.org/pinselimo/Hasky-Types)

# Hasky-Types

This repository is part of the development of [```Hasky```](https://github.com/pinselimo/Hasky/). It defines types for 

## Dependencies

You can use these types as a classic ```hackage``` package. It has no other dependencies than some of the ```Foreign.*``` modules contained in the standard library of ```GHC``` and the [C-structs](https://github.com/pinselimo/cstructs-in-haskell) package.

### CArray

~~~haskell
λ> import Foreign.Hasky.Array (CArray, newArray, peekArray, freeArray)
λ> arr <- newArray $ map (*2) [1..5]
λ> peekArray arr
[2.0,4.0,6.0,8.0,10.0]
λ> freeArray arr
~~~

CArray is a type synonym for a pointer to the Struct containing the data. The Haskell code above can be interpreted as quasi equivalent to:

~~~C
struct CArray {
    int length;
    double* array;
};

struct CArray *arr;
arr = malloc (sizeOf (struct CArray));
arr->length = 5;
arr->array = {2.0, 4.0, 6.0, 8.0, 10.0};
~~~

or with Python's ```ctypes```:

~~~python
>>> from ctypes import Structure, c_int. c_double, POINTER, pointer
>>> class CArray( Structure ):
...     _fields_ = [("length", c_int), ("array", POINTER(c_double))]
...
>>> arr = pointer( CArray(5, (c_double * 5)(*map(lambda x:x*2,range(1,6)))) )
~~~

On memory all of these examples should have the exact same representation. A pointer to either ```arr``` can then be exchanged with the other and used in a ```foreign``` call.
For a more elaborated usage example checkout [```hasky.types```](https://github.com/pinselimo/Hasky/hasky/types.py).

### CList

~~~haskell
λ> import Foreign.Hasky.List (CList, newList, peekList, freeList)
λ> list <- newList $ map (*2) [1..5]
λ> peekList list
[2.0,4.0,6.0,8.0,10.0]
λ> freeList list
~~~

It builds a linked list and returns a pointer to its first element like:

~~~C
struct CList {
    double data;
    struct CList *next;
};

struct CList *list;
// The code to build the list is intentionally omitted.
~~~

For a more elaborated usage example checkout [```hasky.types```](https://github.com/pinselimo/Hasky/hasky/types.py).

### Tuples

~~~haskell
λ> import Foreign.Hasky.Tuples (CTuple4, newTuple4, peekTuple4, free)
λ> tuple <- newTuple4 (63 :: Int, 'a', 42.0, 1 :: Word)
λ> peekTuple4 tuple
(63, 'a', 42.0, 1)
λ> free tuple
~~~

There exist also ```CTuple2``` and ```CTuple3``` for tuples with less fields. The ```CTuple*``` names are type synonyms for pointers to structs of [```C-structs```](https://github.com/pinselimo/cstructs-in-haskell). The above code is de facto equivalent to the following C code:

~~~C
struct CTuple4 {
    int a;
    char b;
    double c;
    unsigned int d;
};

struct CTuple4 *tuple;
tuple = malloc (sizeOf (CTuple4));
tuple->a = 63;
tuple->b = 'a';
tuple->c = 42.0;
tuple->d = 1;
~~~

For a more elaborated usage example checkout [```hasky.types```](https://github.com/pinselimo/Hasky/hasky/types.py).

### Strings

The ```Hasky``` ```CWString``` is actually a pointer to a ```CWString``` of ```Foreign.C.String```.

~~~haskell
λ> import Foreign.Hasky.String (CWString, newCWString, peekCWString, freeCWString)
λ> string <- newCWString "Why the hell would Hasky need its own string type?"
λ> peekCWString string
"Why the hell would Hasky need its own string type?"
λ> freeCWString string
~~~

The reason for the ```CWString``` type to exist is found in its heritage from a the Python module [```Hasky```](https://github.com/pinselimo/Hasky).
Unfortunately Python's own ```ctypes``` makes some strange choices when it comes to wrapping foreign data.
One of the is, that a foreign pointer to a ```c_wchar``` is immediately converted to a proper Python ```str```. This makes it impossible to free the pointer because one cannot access it.
A pointer to a pointer is however left alone and just introduces minor wrapping. It can be used in Python as follows:

~~~python
>>> from ctypes import POINTER, pointer, c_wchar_p
>>> CWString = POINTER( c_wchar_p )
>>> string = pointer( c_wchar_p("This is why Hasky needs this boiler plate") )
~~~

## Testing

Identity properties are tested with QuickCheck to ensure that peek and poke are reversible. Imports from C are tested in ```test/CTest.hs``` and form together with the identity tests the guarantee that also exports to C are consistent.
The QuickCheck tests are performed for all latest minor revisions of GHC versions >= 8.0 through [haskell-ci](https://github.com/haskell-CI/haskell-ci). More GHC versions will be added once the C-structs package is available through Hackage.

Further testing is done in the [```Hasky```](https://github.com/pinselimo/Hasky) packages where correct interfacing with Python is ensured.

## License

This part of Hasky is licensed under the ```MIT``` License. Please be aware that the full ```Hasky``` package is under ```LGPLv3```. Refer to the accompanying LICENSE or COPYING files for details.

> (c) 2020 Simon Plakolb

