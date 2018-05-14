
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gestalt

Every non-primitive function in R chains together other functions. The
*gestalt* package enables you to naturally express this using the
`%>>>%` operator. It adopts the semantics of the
[magrittr](https://magrittr.tidyverse.org) `%>%`, in addition to
supporting tidyverse-style
[quasiquotation](https://rlang.r-lib.org/reference/quasiquotation.html).

Use it to improve the modularity, reusability and comprehensibility of
your functions.

``` r
library(gestalt)

model <- mpg ~ wt

r2_cyl <- 
  group: split(.$cyl) %>>>%
  lapply(function(data) lm(!!model, data)) %>>>% 
  summary: (
    lapply(summary) %>>>%
    stat: sapply(`[[`, "r.squared")
  )

r2_cyl(mtcars)
#>         4         6         8 
#> 0.5086326 0.4645102 0.4229655

(split(.$gear) %>>>% r2_cyl[-1])(mtcars)
#>         3         4         5 
#> 0.6082956 0.6771827 0.9791962

r2 <- r2_cyl
r2$group <- function(x, grp) split(x, x[[grp]])
r2(mtcars, "gear")
#>         3         4         5 
#> 0.6082956 0.6771827 0.9791962

residuals <- r2
residuals$summary$stat <- function(s) lapply(s, `[[`, "residuals")
residuals(mtcars, "cyl")[["6"]]
#>      Mazda RX4  Mazda RX4 Wag Hornet 4 Drive        Valiant       Merc 280 
#>     -0.1249670      0.5839601      1.9291961     -0.6896780      0.3547199 
#>      Merc 280C   Ferrari Dino 
#>     -1.0452801     -1.0079511
```

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("egnha/gestalt")
```

## License

MIT Copyright © 2018 [Eugene Ha](https://github.com/egnha)
