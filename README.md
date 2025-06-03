currydoc: A Documentation Generator for Curry Programs
======================================================

This package contains a tool to generate
the documentation for a Curry program (i.e., the main module
and all its imported modules) in HTML (or LaTeX) format.
The generated HTML pages contain information about
all data types and functions exported by a module as well
as links between the different entities.
Furthermore, some information about the definitional status
of functions (like rigid, flexible, external, complete, or
overlapping definitions) are provided and combined with
documentation comments provided by the programmer.

Short Description
-----------------

A **documentation comment** starts at the beginning of a line
with three dashes (also in literate programs!).
All documentation comments immediately before a
definition of a datatype or (top-level) function are kept together.
The documentation comments for the complete module occur before
the first `module` or `import` line in the module.
The comments can also contain several special tags. These tags
must be the first thing on its line (in the documentation comment)
and continues until the next tag is encountered or until the
end of the comment. The following tags are recognized:

    @author comment

Specifies the author of a module (only reasonable in module comments).

    @version comment

Specifies the version of a module (only reasonable in module comments).

    @cons id comment

A comment for the constructor `id` of a datatype
(only reasonable in datatype comments).

    @param id comment

A comment for function parameter `id` (only reasonable in function
comments). Due to pattern matching, this need not be the name of a
parameter given in the declaration of the function but all parameters
for this functions must be commented in left-to-right order.

    @return comment

A comment for the return value of a function (only reasonable in function comments).

The comment of a documented entity can be any string in
[Markdown](http://en.wikipedia.org/wiki/Markdown) syntax.
The currently supported set of elements is described in
[Curry package markdown](https://github.com/curry-packages/markdown/blob/master/docs/markdown-syntax.md).
The comments can also contain markups in HTML format
so that special characters like `<` must be quoted.
In addition to Markdown or HTML markups,
one can also mark **references to names** of operations or data types
in Curry programs. These are translated into links inside
the generated HTML documentation (if they are unqualified) or into links
in other module documentations if they are qualified with a module name.
Such references have to be enclosed in single quotes.

The following example shows a Curry program with some
documentation comments:

    --- This is an
    --- example module.
    --- @author Michael Hanus
    --- @version 0.1
    
    module Example where
    
    --- The function `conc` concatenates two lists.
    --- It is also predefined as 'Prelude.++'.
    --- @param xs - the first list
    --- @param ys - the second list
    --- @return a list containing all elements of `xs` and `ys`
    conc []     ys = ys
    conc (x:xs) ys = x : conc xs ys
    -- this comment will not be included in the documentation
    
    --- The function `last` computes the last element of a given list.
    --- It is based on the operation 'conc' to concatenate two lists.
    --- @param xs - the given input list
    --- @return last element of the input list
    last xs | conc ys [x] =:= xs  = x   where x,ys free
    
    --- This data type defines _polymorphic_ trees.
    --- @cons Leaf - a leaf of the tree
    --- @cons Node - an inner node of the tree
    data Tree a = Leaf a | Node [Tree a]
  
If this program is contained in the file `Example.curry`,
one can generate the documentation by executing the command

    currydoc Example

This command creates the directory `DOC_Example` (if it does not exist)
and puts all HTML documentation files for the main program module
`Example` and all its imported modules in this directory together with
a main index file `index.html`. Additionally, a file containing the
CDoc representation of the module is created.
If one prefers another directory for the documentation files,
one can also execute the command

    currydoc docdir Example

where `docdir` is the directory for the documentation files.


Further Information
-------------------

More details on CurryDoc are described in the user manuals of the Curry systems
[PAKCS](https://www.curry-lang.org/pakcs/) and
[KiCS2](https://www.curry-lang.org/kics2/).
There is also a paper describing the basic ideas of CurryDoc:

M. Hanus:
CurryDoc : A Documentation Tool for Declarative Programs.
Proc. of the 11th International Workshop on Functional and (Constraint)
Logic Programming (WFLP 2002),
Research Report UDMI/18/2002/RR, Università degli Studi di Udine,
pp. 225-228, 2002 
