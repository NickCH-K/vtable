
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vtable

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/vtable)](https://CRAN.R-project.org/package=vtable)
<!-- badges: end -->

The **vtable** package is designed to help you quickly and efficiently
look at and document your data.

There are three main functions in **vtable**:

1.  `vtable`, or `vt` for short, shows you information about the
    variables in your data set, including variable labels, in a way that
    is easy to use “find in page” to search through. It was designed to
    be similar to Stata’s “Variables” panel.
2.  `sumtable` or `st` for short, provides a table of summary
    statistics. It is very similar in spirit to the summary statistics
    function of `stargazer::stargazer()` except that it accepts
    `tibble`s, handles factor variables, and makes by-group statistics
    and group tests easy.
3.  `labeltable` provides a table of value labels, either for variables
    labelled with **sjlabelled** or **haven** or similar, or for when
    you want to see how the values of one column line up with the values
    of another.

All three of these functions are built with the intent of being *fast*.
Not so much fast to *run*, but fast to *use*. The defaults are intended
to be good defaults, and the output by default prints to the Viewer tab
(in RStudio) or the browser (outside RStudio) so you can see it
immediately, and continue to look at it as you work on your data.

You could almost certainly build your own highly-customized version of
`vtable`, But why do that when you can just do `vt(df)` and see the
information you need to see? And there are eight million packages that
make summary statistics tables to your exact specifications if you tweak
them. But there’s a good chance that `st(df)` does what you want. If you
want something real out there, that’s when you can break out the big
guns.

All three main **vtable** functions can produce HTML, LaTeX,
`data.frame`, or `knitr::kable()` output.

## Installation

You can install pmdplyr from CRAN:

``` r
install.packages("pmdplyr")
```

### Development version

The development version can be installed from
[GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("NickCH-K/pmdplyr")
```

## vtable Example

I’ll just do a brief example here, using the `iris` we all know and
love. Output will be to `kable` since this is an RMarkdown document.

``` r
data(iris)

# Basic vtable
vt(iris, out = 'kable')
```

| Name         | Class   | Values                            |
| :----------- | :------ | :-------------------------------- |
| Sepal.Length | numeric | Num: 4.3 to 7.9                   |
| Sepal.Width  | numeric | Num: 2 to 4.4                     |
| Petal.Length | numeric | Num: 1 to 6.9                     |
| Petal.Width  | numeric | Num: 0.1 to 2.5                   |
| Species      | factor  | ‘setosa’ ‘versicolor’ ‘virginica’ |

There are plenty of options if we want to go nuts, but let’s keep it
simple and just ask for a little more with
`lush`

``` r
vt(iris, lush = TRUE, out = 'kable')
```

| Name         | Class   | Values                            | Missing | Summary                               |
| :----------- | :------ | :-------------------------------- | :------ | :------------------------------------ |
| Sepal.Length | numeric | Num: 4.3 to 7.9                   | 0       | mean: 5.843<br>sd: 0.828<br>nuniq: 35 |
| Sepal.Width  | numeric | Num: 2 to 4.4                     | 0       | mean: 3.057<br>sd: 0.436<br>nuniq: 23 |
| Petal.Length | numeric | Num: 1 to 6.9                     | 0       | mean: 3.758<br>sd: 1.765<br>nuniq: 43 |
| Petal.Width  | numeric | Num: 0.1 to 2.5                   | 0       | mean: 1.199<br>sd: 0.762<br>nuniq: 22 |
| Species      | factor  | ‘setosa’ ‘versicolor’ ‘virginica’ | 0       | nuniq: 3                              |

## sumtable Example

Let’s stick with `iris`\!

``` r
# Basic summary stats
st(iris, out = 'kable')
```

| Variable     | N   | Mean  | Std.Dev. | Min | Pctl.25 | Pctl.75 | Max |
| :----------- | :-- | :---- | :------- | :-- | :------ | :------ | :-- |
| Sepal.Length | 150 | 5.843 | 0.828    | 4.3 | 5.1     | 6.4     | 7.9 |
| Sepal.Width  | 150 | 3.057 | 0.436    | 2   | 2.8     | 3.3     | 4.4 |
| Petal.Length | 150 | 3.758 | 1.765    | 1   | 1.6     | 5.1     | 6.9 |
| Petal.Width  | 150 | 1.199 | 0.762    | 0.1 | 0.3     | 1.8     | 2.5 |
| Species      | 150 |       |          |     |         |         |     |
| … setosa     | 50  | 33.3% |          |     |         |         |     |
| … versicolor | 50  | 33.3% |          |     |         |         |     |
| … virginica  | 50  | 33.3% |          |     |         |         |     |

Note that `sumtable` allows for much more customization than `vtable`
since there’s a heightened chance you want it for a paper or something.
But I’ll leave that to the more detailed documentation. For now just
note it does by-group stats, either in “`group.long`” format (multiple
`sumtable`s stacked on top of each other), or by default, in columns,
with an option to add a group test.

``` r
st(iris, 
   group = 'Species', 
   group.test = TRUE, 
   out = 'kable')
```

| Variable     | N      | Mean  | SD    | N          | Mean  | SD    | N         | Mean  | SD    | Test             |
| :----------- | :----- | :---- | :---- | :--------- | :---- | :---- | :-------- | :---- | :---- | :--------------- |
| Species      | setosa |       |       | versicolor |       |       | virginica |       |       |                  |
| Sepal.Length | 50     | 5.006 | 0.352 | 50         | 5.936 | 0.516 | 50        | 6.588 | 0.636 | F=119.265\*\*\*  |
| Sepal.Width  | 50     | 3.428 | 0.379 | 50         | 2.77  | 0.314 | 50        | 2.974 | 0.322 | F=49.16\*\*\*    |
| Petal.Length | 50     | 1.462 | 0.174 | 50         | 4.26  | 0.47  | 50        | 5.552 | 0.552 | F=1180.161\*\*\* |
| Petal.Width  | 50     | 0.246 | 0.105 | 50         | 1.326 | 0.198 | 50        | 2.026 | 0.275 | F=960.007\*\*\*  |

## labeltable Example

For this we’ll need labeled values.

``` r
data(efc, package = 'sjlabelled')

# Now shoot - how was gender coded?
labeltable(efc$e16sex, out = 'kable')
```

| e16sex | Label  |
| :----- | :----- |
| 1      | male   |
| 2      | female |

`labeltable` can also be used to see, for values of one variable, what
values are present of other variables. This is intended for use if one
variable is a recode, simplification, or lost-labels version of another,
but hey, go nuts.

``` r
labeltable(efc$e15relat,efc$e16sex,efc$e42dep, out = 'kable')
```

| e15relat | e16sex   | e42dep         |
| :------- | :------- | :------------- |
| 1        | 2, 1     | 3, 4, 1, 2, NA |
| 2        | 2, 1, NA | 3, 4, 2, 1     |
| 3        | 1, 2     | 3, 2, 1, 4     |
| 4        | 2, 1     | 4, 3, 2, 1     |
| 5        | 2, 1     | 3, 2, 1, 4     |
| 6        | 2, 1     | 4, 3, 1, 2     |
| 7        | 2, 1     | 4, 3, 2, 1     |
| 8        | 2, 1     | 3, 4, 2, 1     |
| NA       | 2, NA    | 3, NA          |
