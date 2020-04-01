Decline in Variability on Svalbard
================

*Contributors: Sondre Hølleland<sup>†</sup>, Hans Arnfinn Karlsen.
University of Bergen, Norway.*

*<sup>†</sup> Responsible for the code.*

*Correspondance to: <sondre.holleland@uib.no>*

*The full paper can be found here (link will come).*

This repository contains the necessary code for reproducing results from
the paper *Decline in Variability on Svalbard*. The experiments are
coded in C++ for effeciency, but administered from R. Thus, necessary
packages to run the code is:

``` r
list.of.packages <- c("Rcpp", 
                      "RcppArmadillo",
                      "doParallel", 
                      "ggplot2", 
                      "reshape2")
# The following code will install the missing packages: 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
```

The Rcpp packages require a C++ compilator. This is easiest set up by
installing [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

The bootstrap with seven parallel workers takes about an hour to run.

## Author

**Sondre Hølleland** - [holleland](https://github.com/holleland)

## License

This project is licensed under the GNU GPLv3 License - see
[LICENSE.md](LICENSE.md) for details.
