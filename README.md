# FTL

FTL (Fortran Template Library) is an object oriented implementation of generic containers similar to the C++ STL (Standard Template Library). Not all elements present in STL are implemented in FTL, yet the documentation in http://www.cplusplus.com/reference/stl/ can be used as generic conceptual reference. Specific documentation for FTL generated with Doxgen can be found in the [`documentation`](https://github.com/ffadrique/FTL/tree/master/documentation/doxygen) directory.

Some of the descriptions in the readme file have been taken from the above reference and adapted to the specific implementation of FTL.


## Standard Containers
A container is a holder object that stores a collection of other objects (its elements). They are implemented as class templates, which allows a great flexibility in the types supported as elements.

The container manages the storage space for its elements and provides member functions to access them, either directly or through iterators (reference objects with similar properties to pointers).

Containers replicate structures very commonly used in programming: dynamic arrays (vector), queues (queue), stacks (stack), heaps (priority_queue), linked lists (list), trees (set), associative arrays (map)...

Many containers have several member functions in common, and share functionalities. The decision of which type of container to use for a specific need does not generally depend only on the functionality offered by the container, but also on the efficiency of some of its members (complexity). This is especially true for sequence containers, which offer different trade-offs in complexity between inserting/removing elements and accessing them.

## Container class templates
| FTL  | STL Equivalent | Description
| ------------ | ------------ | ------------ |
| vector  | vector  | Vector
| list | list | Doubly linked list |
| slist  | forward_list | Singly linked list |
| queue | queue | FIFO queue |
| stack | stack | LIFO stack |
| tree | - | Generic tree |

### *vector*

Vectors are sequence containers representing arrays that can change in size.

Vectors use contiguous storage locations for their elements. Their size can change dynamically, with their storage being handled automatically by the container.

Internally, vectors use a dynamically allocated array to store their elements. This array may need to be reallocated in order to grow in size when new elements are inserted, which implies allocating a new array and moving all elements to it. This is a relatively expensive task in terms of processing time, and thus, vectors do not reallocate each time an element is added to the container.

Reference STL documentation: http://www.cplusplus.com/reference/vector/vector/

Limitations with respect to STL C++
- No reverse iteration.
- No constant iterators.
- Max size is dummy (dummy value not computed from architecture).
- No emplace functions.
- No operator [].
- Splice can append lists with an extension on the STL C++ interface that emulates the past-last-element with null iterator

Container file name: [`vector.f90`](https://github.com/ffadrique/FTL/blob/master/src/vector.f90 "vector.f90")

### *list*

Lists are sequence containers that allow constant time insert and erase operations anywhere within the sequence, and iteration in both directions.

List containers are implemented as doubly-linked lists; Doubly linked lists can store each of the elements they contain in different and unrelated storage locations. The ordering is kept internally by the association to each element of a link to the element preceding it and a link to the element following it.

Reference STL documentation: http://www.cplusplus.com/reference/list/list/

Limitations with respect to STL C++
- No reverse iteration.
- No constant iterators.
- Max size is dummy (dummy value not computed from architecture).
- No emplace functions.
- Splice can append lists with an extension on the STL C++ interface that emulates the past-last-element with null iterator

Container file name: [`list.f90`](https://github.com/ffadrique/FTL/blob/master/src/list.f90 "list.f90")

### *slist*

Single lists are sequence containers that allow constant time insert and erase operations anywhere within the sequence.

Single lists are implemented as singly-linked lists; Singly linked lists can store each of the elements they contain in different and unrelated storage locations. The ordering is kept by the association to each element of a link to the next element in the sequence.

The interface of *slist* is common to *list*, therefore the applicable STL C++ documentation is that of *list*

Reference STL documentation: http://www.cplusplus.com/reference/list/list/

Limitations with respect to STL C++
- No reverse iteration.
- No constant iterators.
- Max size is dummy (dummy value not computed from architecture).
- No emplace functions.
- Splice can append lists with an extension on the STL C++ interface that emulates the past-last-element with null iterator

Container file name: [`slist.f90`](https://github.com/ffadrique/FTL/blob/master/src/slist.f90 "slist.f90")
### queue

Queues are a type of container, specifically designed to operate in a FIFO context (first-in first-out), where elements are inserted into one end of the container and extracted from the other. Elements are pushed into the *back* of the specific container and popped from its *front*.

Reference STL documentation: http://www.cplusplus.com/reference/queue/queue/

Limitations with respect to STL C++
- No emplace functions.
- No swap functions.

Container file name: [`queue.f90`](https://github.com/ffadrique/FTL/blob/master/src/queue.f90 "queue.f90")

### stack

Stacks are a type of container, specifically designed to operate in a LIFO context (last-in first-out), where elements are inserted and extracted only from one end of the container. Elements are pushed/popped from the *back* of the specific container, which is known as the top of the stack.

Reference STL documentation: http://www.cplusplus.com/reference/stack/stack/

Limitations with respect to STL C++
- No emplace functions.
- No swap functions.

Container file name: [`stack.f90`](https://github.com/ffadrique/FTL/blob/master/src/stack.f90 "stack.f90")

### tree

Tree is a container designed to store a hierarchical collection of elements. Tree implements the hierarchy as doubly-linked nodes pointing to parents, siblings and children.

Reference STL documentation: N/A

Container file name: [`tree.f90`](https://github.com/ffadrique/FTL/blob/master/src/tree.f90 "tree.f90")

## Container implementation

FTL containers are implemented as generic classes to be instantiated replacing the template keywords by the actual contained element type information. Each generated container class is therefore a unique class that can generate any number of objects, also independently of other container classes. This provides high flexibility as each independent container class does not have any common or static properties in the module implementing the container.

The keywords in the container templates:
- `xxuse__`: Name of the module to reference in the `use` statement, like in `use xxuse__`. The used module contains the declaration of the contained element class.
- `xxmodulebase__`: Name of the module to be used in the declaration of the container, like in `module xxmodulebase___list_ftl`. Usually this is the same as `xxuse__`.
- `xxtypebase__`: Name of the type of the contained element, like in `class(xxtypebase__), intent(in) :: element`
- `xxconstructor__`: Name to be used in the name of the container constructor, like in `mylist = xxconstructor___list_ftl()`

## Using the container templates

The FTL templates can be used directly creating dedicated instances for each type of contained object replacing the keyword identified above manually. However, it is recommended to use the Python scripts that can be found in *[fxx](https://github.com/ffadrique/FTL/tree/master/fxx,"fxx")*. Two different generation options are available.

### Single container direct generation

To generate a single container from the contained module name and type name, use the following command. Additional options can be furnished to steer the template generation.

```sh
usage: fxx-container-only.py [-h] [--verbose] --templates-dir TEMPLATES_DIR
                             --name {list,slist,vector,queue,stack,tree}
                             --module MODULE --type TYPE
                             [--constructor CONSTRUCTOR]
                             [--output-dir OUTPUT_DIR] [--pure]
                             [--polymorphic]

options:
  -h, --help            show this help message and exit
  --verbose             Trace detailed information of the process steps
  --templates-dir TEMPLATES_DIR
                        Location of the container templates (default: .)
  --name {list,slist,vector,queue,stack,tree}
                        Container name (default: list)
  --module MODULE       Module name
  --type TYPE           Type name
  --constructor CONSTRUCTOR
                        Constructor name (optional if type has the form
                        "t_typename", then constructor = "typename")
  --output-dir OUTPUT_DIR
                        Output directory (default: . or the directory of the
                        input file
  --pure                Configure the container to have pure accessors
                        (default: True)
  --polymorphic         Configure the container to store polymorphic objects
                        (default: False)
```

The generated container source file is named `<MODULE>_<name>_ftl.f90`. The container type name is `<TYPE>_<name>_ftl`.

The host module shall the use the module using the generated name above and shall declare the container instances using `type(<TYPE>_<name>_ftl) :: x`

### Multiple ontatiner generation from host template

It is possible to generate one or more container modules from a host source file that contains embedded meta commands. In this case the module and type names necessary to build the containers are taken from the host source. The host source is also expanded to the final syntax consistently with the generated containers.

```sh
usage: fxx.py [-h] [--verbose] [--templates-dir TEMPLATES_DIR] source

positional arguments:
  source                Source file with meta code for template instantiation

options:
  -h, --help            show this help message and exit
  --verbose             Trace detailed information of the process steps
  --templates-dir TEMPLATES_DIR
                        Location of the container templates (default: .)
```

The following structures are supported

| Construct type | Metacode example | Expanded code (module=`m_foo`, type=`t_foo`) | Remarks |
| ------------ | ------------ | ------------ | ------------ |
| Use statement | `use <list>`| `use m_foo_list_ftl` | One entry per identified container |
| Object declaration | `list<m_foo,t_foo> :: x` | `type(t_foo_list_ftl) :: x` | `use <list>` generates `use m_foo_list_ftl` |
| Constructor invocation | `x = list<m_foo,t_foo>()` | `x = foo_list_ftl()` | `use <list>` generates `use m_foo_list_ftl` |
| Inherit container | `type, extends(list<m_foo,t_foo> :: t_bar` | `type, extends(t_foo_list_ftl)` :: t_bar | `use <list>` generates `use m_foo_list_ftl` |
| Reference to parent | `x%list<m_foo,t_foo>%begin()` | `x%t_foo_list_ftl%begin()` | `use <list>` generates `use m_foo_list_ftl` |
| Case in `select type` construct | `class is( list<m_foo,t_foo> )` | `class is( t_foo_list_ftl )` | `use <list>` generates `use m_foo_list_ftl` |

If the contained element permits, it is possible to generate the container with `pure` accessors for `front`, `back`, `top`, `bottom` and `get_elememt`. The `~` is used for this purpose in the use declaration.
If the container has to store polymorphic data the container can be configured using the `*` in the meta code.

| Construct type | Metacode example | Expanded code (module=m_foo, type=t_foo) | Remarks |
| ------------ | ------------ | ------------ | ------------ |
| Use statement with pure accessor | `use <~list>`| `use m_foo_list_ftl` | One entry per identified container |
| Object declaration with polymorphic contained element | `list<m_foo,*t_foo> :: x` | `type(t_foo_list_ftl)` :: x | `use <list>` generates `use m_foo`; `type(t_foo)`converted to `class(t_foo)` in container module |

The host module source template must have one of the extensions `.t90`, `.t95`, `.t03`, `.t08`, `.t15`, `.t18`. The expanded host module source is generated in the same folder as the host source template, with the same name and with the same extension changing the `t` ba an `f` (e.g. `m_foo.t03` generates `m_foo.t03`). For every requested container in the host source template a container module is generated.

### Contained element type requirements

To support some of the functions provided by the containers, the contained element type must furnish some type bound procedures. In particular these are needed for searching and sorting functions. These are the comparison operators `operator(<)` and `operator(>)`, and the equality operator `operator(==)`.
Additionally, if the container is to store polymorphic objects, the contained element type must implement the assignment `assignment(=)` as implicit assignment is not permitted for polymorphic objects.

### Test example

The following file can be used to test and understand the multiple container generation.

#### Host module (not to compile)
Host module with file name `m_database.t18`

```Fortran
module m_database

! Use statements
  use <~vector>
  use <list>

! Container object declarations
  vector<m_foo,t_foo> :: foo_object
  list<m_bar,*t_bar> :: bar_object

! Contained object declarations wit the intent attribute
  vector<m_foo,t_foo>, pointer, intent(in) :: foo_object
  list<m_bar,*t_bar>, allocatable, dimension(:), intent(out) :: bar_object

! Iterator declarations
  vector_iterator<m_foo,t_foo> :: foo_object
  list_iterator<m_bar,*t_bar> :: bar_object

! Inheritance of a container
  type, extends(vector<m_foo,t_foo>) :: t_foo_list
  type, extends(list<m_bar,*t_bar>), abstract :: t_bar_list

! Type selection (class or type)
  select type( x )
    type is(vector<m_foo,t_foo>)
    class is(list<m_bar,*t_bar>)
  end select

! Reference to the parent in a type derived by inheritance
  x = object%vector<m_foo,t_foo>%get_param()
  y = object%list<m_bar,t_bar>%get_param()

! Constructor
  x = vector<m_foo,t_foo>()
  y = list<m_bar,t_bar>( a, b )

end module m_database
```

#### Expanded host module (not to compile)
Generated expanded host module with file name `m_database.f18`

```Fortran
module m_database

! Use statements
  use m_foo_vector_ftl
  use m_bar_list_ftl

! Container object declarations
  type(t_foo_vector_ftl) :: foo_object
  type(t_bar_list_ftl) :: bar_object

! Contained object declarations wit the intent attribute
  class(t_foo_vector_ftl), pointer, intent(in) :: foo_object
  class(t_bar_list_ftl), allocatable, dimension(:), intent(out) :: bar_object

! Iterator declarations
  type(t_foo_vector_ftl_iterator) :: foo_object
  type(t_bar_list_ftl_iterator) :: bar_object

! Inheritance of a container
  type, extends(foo_vector_ftl) :: t_foo_list
  type, extends(bar_list_ftl), abstract :: t_bar_list

! Type selection (class or type)
  select type( x )
    type is(t_foo_vector_ftl)
    class is(t_bar_list_ftl)
  end select

! Reference to the parent in a type derived by inheritance
  x = object%t_foo_vector_ftl%get_param()
  y = object%t_bar_list_ftl%get_param()

! Constructor
  x = foo_vector_ftl()
  y = bar_list_ftl( a, b )

end module m_database
```

### Generated containers
From the above transformation of the host module, the following containers are generated
- `m_foo_vector.f18` (from `vector.f90`)
- `m_bar_list.f18` (from `list.f90`)

## Building and testing FTL
FTL has been tested with Intel Fortran 19 (or higher) and gfortran 9.4 (or higher).

The test provided along with the FTL libraries are written using *[XFunit](https://github.com/ffadrique/XFunit)*. Testing also requires that the dependencies [Fommons](https://github.com/ffadrique/Fommons) and [XFunit](https://github.com/ffadrique/XFunit) are available and compiled in the target sytem.

Although the provided templates need to be instantiated for the specific contained element type, it is possible to compile and test the raw templates using the test contained module `Use.f90`. This allows testing FTL in the target platform (OS and compiler) before using it in the final application.

In both Windows and Linux deploy the Fommons and XFunit from the same root directory
- Windows: `Projects/Fommons` and `Projects/XFunit`. The `.sln` files are prepared for this configuration.
- Linux: `Projects/fommons` and `Projects/xfunit`. The `gmake` files are prepared for this configuration (mind the lowercase for the library folder names).

### Windows
FTL is provided with a Visual Studio 2019 configured solution that allows building and testing the library. The solution provides a C# project that integrates the unit test in Fortran with the unit test detection feature of Visual Studio. This allows the execution of all unit tests from the Test Explorer menu.
To use the templates it is not necessary to download and install the Visual Studio solution, just the template files.
The purpose of this Visual Studio solution is to test the library in the destination platform (OS and compiler). 

### Linux
FTL is provided with `gmake` makefiles to build and test the library.
The purpose of this gmake and the unit test suite (utest folder) is to test the library in the destination platform (OS and compiler). 
To use the templates it is not necessary to download and install the gmake files or the unit tests, just the template files.
 
To build the FTL library execute the following command in the `src` directory
```make
gmake libs
```
To build the FTL library and unit tests execute the following command in the `src` directory
```make
gmake all
```
To execute the unit tests execute the following command in the `src` or the `utest` directory
```make
gmake units
```
The default compiler is `gfortran` but can be overridden from the command line to use Intel Fortran
```make
gmake F90COMP=ifort
```
The ifort or gfortran commands must be in the execution path.
Alternatively, the makefile can be edited to provide the full path to the compiler.
Compiler options are automatically selected for the active compiler; only Debug configuration is provided in the make files.

## Documentation
This readme page is the main user documentation. In addition, documentation generated with FORD and Doxygen can be found in the [`documentation`](https://github.com/ffadrique/FTL/tree/master/documentation/doxygen) directory. 

## Licensing
FTL is open-source software, licensed under the GNU Lesser General Public License (LGPL).
