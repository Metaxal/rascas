#lang scribble/manual
@require[racket/require]
@require[@for-label[rascas
                    (subtract-in racket/base
                                 rascas)]
         racket/base
         racket/runtime-path]

@title{Rascas}
@author{Laurent Orseau}

@defmodule[rascas]

A Computer Algebra System for Racket.

Initially a port of @hyperlink["https://github.com/dharmatech/mpl"]{dharmatech/mpl},
but now incorporates lots of modifications.

Rascas is still unstable and some behaviours may change without warning,
although normally only to make them more consistent.

@section{Numeric over symbolic}

Rascas implements the @emph{numeric over symbolic} principle:
@nested[#:style 'inset]{
If @racket[f] is a symbolic expression with @emph{n} free variables,
then if for all valid numeric values of these @emph{n} variables
(that do not lead to @racket[+nan.0] or to an exception being raised)
the expression reduces to a constant numeric value @emph{c},
then Rascas @emph{may} choose to eagerly reduce the expression @racket[f]
containing free variables to @emph{c}.}

For example, Rascas reduces @racket[(* 0 'x)] to @racket[0], because
this is correct for all valid values for @racket['x], and
if @racket[x] is infinite the result would be @racket[+nan.0]
(note that Racket actually considers @racket[(* 0 +inf.0)] to be @racket[0],
but Rascas reduces this to @racket[+nan.0]).

By contrast, Rascas does @emph{not} reduce @racket[(* 0.0 'x)] because if @racket['x] is negative
the result would be @racket[-0.0], which may lead to different values since for example
@racket[(/ 1 -0.0)] is @racket[-inf.0].


This principle of numeric over symbolic also
means that the user must be careful about the order in which variables are substituted,
and it is preferable to use @racket[concurrent-substitute] so that the expression is reduced only
after all variables are substituted with their value.


