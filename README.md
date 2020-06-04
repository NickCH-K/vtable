
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

You can install vtable from CRAN. Note that the documentation on this
site refers to the development version, and so may not work perfectly
for the CRAN version. But the two will usually be the same.:

``` r
install.packages("vtable")
```

### Development version

The development version can be installed from
[GitHub](https://github.com/):

``` r
# install.packages("remotes")
remotes::install_github("NickCH-K/vtable")
```

## vtable Example

I’ll just do a brief example here, using the `iris` we all know and
love. Output will be to `kable` since this is an RMarkdown document.

``` r
data(iris)

# Basic vtable
vt(iris)
```

<table>

<caption>

iris

</caption>

<thead>

<tr>

<th style="text-align:left;">

Name

</th>

<th style="text-align:left;">

Class

</th>

<th style="text-align:left;">

Values

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Sepal.Length

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 4.3 to 7.9

</td>

</tr>

<tr>

<td style="text-align:left;">

Sepal.Width

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 2 to 4.4

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Length

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 1 to 6.9

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Width

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 0.1 to 2.5

</td>

</tr>

<tr>

<td style="text-align:left;">

Species

</td>

<td style="text-align:left;">

factor

</td>

<td style="text-align:left;">

‘setosa’ ‘versicolor’ ‘virginica’

</td>

</tr>

</tbody>

</table>

There are plenty of options if we want to go nuts, but let’s keep it
simple and just ask for a little more with `lush`

``` r
vt(iris, lush = TRUE)
```

<table>

<caption>

iris

</caption>

<thead>

<tr>

<th style="text-align:left;">

Name

</th>

<th style="text-align:left;">

Class

</th>

<th style="text-align:left;">

Values

</th>

<th style="text-align:left;">

Missing

</th>

<th style="text-align:left;">

Summary

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Sepal.Length

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 4.3 to 7.9

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

mean: 5.843\<br\>sd: 0.828\<br\>nuniq: 35

</td>

</tr>

<tr>

<td style="text-align:left;">

Sepal.Width

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 2 to 4.4

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

mean: 3.057\<br\>sd: 0.436\<br\>nuniq: 23

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Length

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 1 to 6.9

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

mean: 3.758\<br\>sd: 1.765\<br\>nuniq: 43

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Width

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

Num: 0.1 to 2.5

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

mean: 1.199\<br\>sd: 0.762\<br\>nuniq: 22

</td>

</tr>

<tr>

<td style="text-align:left;">

Species

</td>

<td style="text-align:left;">

factor

</td>

<td style="text-align:left;">

‘setosa’ ‘versicolor’ ‘virginica’

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

nuniq: 3

</td>

</tr>

</tbody>

</table>

## sumtable Example

Let’s stick with `iris`\!

``` r
# Basic summary stats
st(iris)
```

<table>

<caption>

Summary Statistics

</caption>

<thead>

<tr>

<th style="text-align:left;">

Variable

</th>

<th style="text-align:left;">

N

</th>

<th style="text-align:left;">

Mean

</th>

<th style="text-align:left;">

Std. Dev.

</th>

<th style="text-align:left;">

Min

</th>

<th style="text-align:left;">

Pctl. 25

</th>

<th style="text-align:left;">

Pctl. 75

</th>

<th style="text-align:left;">

Max

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Sepal.Length

</td>

<td style="text-align:left;">

150

</td>

<td style="text-align:left;">

5.843

</td>

<td style="text-align:left;">

0.828

</td>

<td style="text-align:left;">

4.3

</td>

<td style="text-align:left;">

5.1

</td>

<td style="text-align:left;">

6.4

</td>

<td style="text-align:left;">

7.9

</td>

</tr>

<tr>

<td style="text-align:left;">

Sepal.Width

</td>

<td style="text-align:left;">

150

</td>

<td style="text-align:left;">

3.057

</td>

<td style="text-align:left;">

0.436

</td>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

2.8

</td>

<td style="text-align:left;">

3.3

</td>

<td style="text-align:left;">

4.4

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Length

</td>

<td style="text-align:left;">

150

</td>

<td style="text-align:left;">

3.758

</td>

<td style="text-align:left;">

1.765

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

1.6

</td>

<td style="text-align:left;">

5.1

</td>

<td style="text-align:left;">

6.9

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Width

</td>

<td style="text-align:left;">

150

</td>

<td style="text-align:left;">

1.199

</td>

<td style="text-align:left;">

0.762

</td>

<td style="text-align:left;">

0.1

</td>

<td style="text-align:left;">

0.3

</td>

<td style="text-align:left;">

1.8

</td>

<td style="text-align:left;">

2.5

</td>

</tr>

<tr>

<td style="text-align:left;">

Species

</td>

<td style="text-align:left;">

150

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

… setosa

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

33.3%

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

… versicolor

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

33.3%

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

… virginica

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

33.3%

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

</tbody>

</table>

Note that `sumtable` allows for much more customization than `vtable`
since there’s a heightened chance you want it for a paper or something.
But I’ll leave that to the more detailed documentation. For now just
note it does by-group stats, either in “`group.long`” format (multiple
`sumtable`s stacked on top of each other), or by default, in columns,
with an option to add a group test.

Grouped `sumtables` look a little nicer in formats that suport
multi-column cells like HTML and LaTeX.

**These tables include multi-column cells, which are not supported in
the `kable` output, but are supported by `vtable`’s `dftoHTML` and
`dftoLaTeX` functions. They look nicer in the HTML or LaTeX output.**

``` r
st(iris, 
   group = 'Species', 
   group.test = TRUE)
```

<table>

<caption>

Summary Statistics

</caption>

<thead>

<tr>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Species

</div>

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

setosa

</div>

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

versicolor

</div>

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

virginica

</div>

</th>

<th style="border-bottom:hidden" colspan="1">

</th>

</tr>

<tr>

<th style="text-align:left;">

Variable

</th>

<th style="text-align:left;">

N

</th>

<th style="text-align:left;">

Mean

</th>

<th style="text-align:left;">

SD

</th>

<th style="text-align:left;">

N

</th>

<th style="text-align:left;">

Mean

</th>

<th style="text-align:left;">

SD

</th>

<th style="text-align:left;">

N

</th>

<th style="text-align:left;">

Mean

</th>

<th style="text-align:left;">

SD

</th>

<th style="text-align:left;">

Test

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Sepal.Length

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

5.006

</td>

<td style="text-align:left;">

0.352

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

5.936

</td>

<td style="text-align:left;">

0.516

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

6.588

</td>

<td style="text-align:left;">

0.636

</td>

<td style="text-align:left;">

F=119.265<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left;">

Sepal.Width

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

3.428

</td>

<td style="text-align:left;">

0.379

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

2.77

</td>

<td style="text-align:left;">

0.314

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

2.974

</td>

<td style="text-align:left;">

0.322

</td>

<td style="text-align:left;">

F=49.16<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Length

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

1.462

</td>

<td style="text-align:left;">

0.174

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

4.26

</td>

<td style="text-align:left;">

0.47

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

5.552

</td>

<td style="text-align:left;">

0.552

</td>

<td style="text-align:left;">

F=1180.161<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left;">

Petal.Width

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

0.246

</td>

<td style="text-align:left;">

0.105

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

1.326

</td>

<td style="text-align:left;">

0.198

</td>

<td style="text-align:left;">

50

</td>

<td style="text-align:left;">

2.026

</td>

<td style="text-align:left;">

0.275

</td>

<td style="text-align:left;">

F=960.007<sup>\*\*\*</sup>

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border:0;" colspan="100%">

<sup></sup> Statistical significance markers: \* p\<0.1; \*\* p\<0.05;
\*\*\* p\<0.01

</td>

</tr>

</tfoot>

</table>

## labeltable Example

For this we’ll need labeled values.

``` r
data(efc, package = 'sjlabelled')

# Now shoot - how was gender coded?
labeltable(efc$e16sex)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

e16sex

</th>

<th style="text-align:left;">

Label

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

male

</td>

</tr>

<tr>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

female

</td>

</tr>

</tbody>

</table>

`labeltable` can also be used to see, for values of one variable, what
values are present of other variables. This is intended for use if one
variable is a recode, simplification, or lost-labels version of another,
but hey, go nuts.

``` r
labeltable(efc$e15relat,efc$e16sex,efc$e42dep)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

e15relat

</th>

<th style="text-align:left;">

e16sex

</th>

<th style="text-align:left;">

e42dep

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

2, 1

</td>

<td style="text-align:left;">

3, 4, 1, 2, NA

</td>

</tr>

<tr>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

2, 1, NA

</td>

<td style="text-align:left;">

3, 4, 2, 1

</td>

</tr>

<tr>

<td style="text-align:left;">

3

</td>

<td style="text-align:left;">

1, 2

</td>

<td style="text-align:left;">

3, 2, 1, 4

</td>

</tr>

<tr>

<td style="text-align:left;">

4

</td>

<td style="text-align:left;">

2, 1

</td>

<td style="text-align:left;">

4, 3, 2, 1

</td>

</tr>

<tr>

<td style="text-align:left;">

5

</td>

<td style="text-align:left;">

2, 1

</td>

<td style="text-align:left;">

3, 2, 1, 4

</td>

</tr>

<tr>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

2, 1

</td>

<td style="text-align:left;">

4, 3, 1, 2

</td>

</tr>

<tr>

<td style="text-align:left;">

7

</td>

<td style="text-align:left;">

2, 1

</td>

<td style="text-align:left;">

4, 3, 2, 1

</td>

</tr>

<tr>

<td style="text-align:left;">

8

</td>

<td style="text-align:left;">

2, 1

</td>

<td style="text-align:left;">

3, 4, 2, 1

</td>

</tr>

<tr>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

2, NA

</td>

<td style="text-align:left;">

3, NA

</td>

</tr>

</tbody>

</table>
