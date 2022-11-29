# `%>%`

Pipe operator


## Description

See `magrittr::` for details.


## Usage

```r
lhs %>% rhs
```


## Arguments

Argument      |Description
------------- |----------------
`lhs`     |     A value or the magrittr placeholder.
`rhs`     |     A function call using the magrittr semantics.


## Value

The result of calling `rhs(lhs)`.


