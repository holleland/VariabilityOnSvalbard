Decline in Variability on Svalbard
================

*Contributors: Sondre Hølleland<sup>†</sup>, Hans Arnfinn Karlsen. University of Bergen, Norway.*

*<sup>†</sup> Responsible for the code.*

*Correspondance to: <sondre.holleland@uib.no>*

*The full paper can be found here (link will come).*

This repository contains the necessary code for reproducing results from the paper *Decline in Variability on Svalbard*. To be able run all the code, install the necessary packages by the following code:

``` r
list.of.packages <- c("TMB", "ggplot2", "zoo", "lubridate", "reshape2",
                      "forecast", "xtable", "doParallel", "mapsdata",
                      "plyr", "dplyr", "ggpubr")
# The following code will install the missing packages: 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
```

The [TMB package](https://github.com/kaskr/adcomp/wiki) requires a C++ compilator. Follow the instructions for [download and installing](https://github.com/kaskr/adcomp/wiki/Download).

**The bootstrap** takes about an hour to run with seven parallel workers. If not adjusted, the script will use one less than the number of available cores.

Data
----

The data is downloaded from [eklima.no](http://eklima.no), published here in accordance with the [Creative Commons 3.0 licence](https://creativecommons.org/licenses/by/3.0/no/) and thus with permission of [Norwegian Meteorological Institute](https://www.met.no/en) .

[<img src="https://www.met.no/en/About-us/logo/_/image/943fbdc6-eba8-4e19-aff1-75f453ba9c7f:4bbfe4ae9e1826b3e159a3fff6e5d3893a93b072/full/Met_RGB_Horisontal_ENG.jpg" alt="drawing" width="339" height = "154"/>](https://www.met.no/en)

Author
------

**Sondre Hølleland** - [holleland](https://github.com/holleland)

License
-------

This project is licensed under the GNU GPLv3 License - see [LICENSE.md](LICENSE.md) for details. The data is licenced under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/no/).
