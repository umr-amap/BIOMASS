# Procrust analysis

Do a procrust analysis. X is the target matrix, Y is the matrix we want
to fit to the target. This function returns a translation vector and a
rotation matrix After the procrust problem you **must** do the rotation
before the translation. **Warning : The order of the value on both
matrix is important**

## Usage

``` r
procrust(X, Y)
```

## Arguments

- X:

  the target matrix

- Y:

  the matrix we want to fit to the target

## Value

A list with the translation vector and the matrix of rotation

## Author

Arthur PERE
