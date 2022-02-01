# FTL

FTL (Fortran Template Library) is an object oriented implementation of generic container similar to the C++ STL (Standard Template Library). Not all eelements present in STL are implemented in FTL, yet the documentation in http://www.cplusplus.com/reference/stl/ can be used as generic conceptual reference.

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

Limitations with repsect to STL C++
- No reverse iteration.
- No constant iterators.
- Max size is dummy (dummy value not computed from architecture).
- No emplace functions.
- No oeprator [].
- Splice can append lists with an extension on the STL C++ interface that emulates the past-last-element with null iterator

Container file name: `vector.f03`

### *list*

Lists are sequence containers that allow constant time insert and erase operations anywhere within the sequence, and iteration in both directions.

List containers are implemented as doubly-linked lists; Doubly linked lists can store each of the elements they contain in different and unrelated storage locations. The ordering is kept internally by the association to each element of a link to the element preceding it and a link to the element following it.

Reference STL documentation: http://www.cplusplus.com/reference/list/list/

Limitations with repsect to STL C++
- No reverse iteration.
- No constant iterators.
- Max size is dummy (dummy value not computed from architecture).
- No emplace functions.
- Splice can append lists with an extension on the STL C++ interface that emulates the past-last-element with null iterator

Container file name: `list.f03`

### *slist*

Single lists are sequence containers that allow constant time insert and erase operations anywhere within the sequence.

Single lists are implemented as singly-linked lists; Singly linked lists can store each of the elements they contain in different and unrelated storage locations. The ordering is kept by the association to each element of a link to the next element in the sequence.

THe internace of *slist* is commont to *list*, therefore the applicable STL C++ documentation is that of *list*

Reference STL documentation: http://www.cplusplus.com/reference/list/list/

Limitations with repsect to STL C++
- No reverse iteration.
- No constant iterators.
- Max size is dummy (dummy value not computed from architecture).
- No emplace functions.
- Splice can append lists with an extension on the STL C++ interface that emulates the past-last-element with null iterator

Container file name: `slist.f03`

### queue

Queues are a type of containers, specifically designed to operate in a FIFO context (first-in first-out), where elements are inserted into one end of the container and extracted from the other. Elements are pushed into the *back* of the specific container and popped from its *front*.

Reference STL documentation: http://www.cplusplus.com/reference/queue/queue/

Limitations with repsect to STL C++
- No emplace functions.
- No swap functions.

Container file name: `queue.f03`

### stack

Stacks are a type of containers, specifically designed to operate in a LIFO context (last-in first-out), where elements are inserted and extracted only from one end of the container. Elements are pushed/popped from the *back* of the specific container, which is known as the top of the stack.

Reference STL documentation: http://www.cplusplus.com/reference/stack/stack/

Limitations with repsect to STL C++
- No emplace functions.
- No swap functions.

Container file name: `stack.f03`

### tree

Tree is a container designed to store a hierarchical collection of elements. Tree implements the hierachy as doubly-linked nodes pointing to parents, siblings and children.

Reference STL documentation: N/A

Container file name: `tree.f03`

## Container implementation

FTL containers are implemented as generic clasess to be instantiated replacing the template keywords by the actual contained element type information. Each generated container class is therefore a unique class that can generate any number of ojects, also independenly of other container classes. This provides high flexibility as each idependent container class does not have any common or static properties in the module implementing the container.

The keywords in the container templates:
- `xxuse__`: Name of the module to reference in the `use` statement, like in `use xxuse__`. The used module contains the declaration of the contained element class.
- `xxmodulebase__`: Name of the module to be use in the delaration of the container, like in `module xxmodulebase___list_ftl`. Usually this is the same as `xxuse__`.
- `xxtypebase__`: Name of the type of the contained element, like in `class(xxtypebase__), intent(in) :: element`
- `xxconstructor__`: Name to be used in the name of the container consturctor, like in `mylist = xxconstructor___list_ftl()`

