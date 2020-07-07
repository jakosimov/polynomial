# polynomial

## Description

A library for dealing with polynomials with rational coefficents. This is without a doubt not the most efficient implementation of this functionality, but I wanted to create a library with an easy API for dealing with polynomials.

## Global Installation

To be able to use the package with `ghci`, outside of any projects, first build the project with
```
stack build
```
from the project directory.
You may have to run
```
stack setup
```
first.

After that, you can simply run
```
cabal v1-install
```
and you should be good to go.

Import the library with
```
import Numeric.Polynomial
```
in `ghci` or a `.hs` file.

## Usage

The function you will probably use most often is the `term :: Double -> Int -> Polynomial` function. With it, you create a polynomial with a single term. To create a polynomial with several terms, you simply add or subtract several terms together.
```
> term 3 2 + term 1 0
3x² + 1
> term 1 5 - term 2 3
x⁵ - 2x³
```
`Polynomial` is an instance of `Num`, so you can use all the operations you would expect from `Num`.
```
> term 5 2 * term 3 3
15x⁵
> term 3 1 * (term 1 5 - term 2 3)
3x⁶ - 6x⁴
> (term 2 1 - term 3 0) * (term 2 1 + term 3 0)
4x² - 9
> (term 1 1 - term 1 0)^2
x² - 2x + 1
```
Since all instances of `Num` implements `fromInteger`, the following to expressions would be identical:
```
> term 5 0 * (term 1 1 + term 1 0)
5x + 5
> 5 * (term 1 1 + 1)
5x + 5
```
There is also the `PolynomialRatio` type, that implements `Num` as well, and that will simplify rational expressions for you.
```
> (term 3 2 + 7) `over` (term 1 1 - 9)
(3x² + 7) / (x - 9)
> (term 1 1 - 3) `over` (term 1 2 - 9)
1 / (x + 3)
> (term 1 2 - 9) `over` (term 1 1 - 3)
x + 3                                  -- This will still be of type PolynomialRatio.
```

A few other useful functions are:
* `eval` which evaluates the polynomial at a given 'x'.
* `</>` which returns the quotient between two polynomials.
* `<%>` which returns the remainder after division of two polynomials.
* `derive` which returns the derivative of the polynomial.
* `greatestCommondivisor`, which finds the greatest common divisor of two polynomials.
* `normalize` which returns the same polynomial, but scaled such that the leading coefficient is 1.
