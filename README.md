
This project is obsolete. As of November 10th, 2014, Betfair drooped support for
the old version of their API. This package doesn't support the
[new generation API](https://api.developer.betfair.com/services/webapps/docs/display/1smk3cen4v3lu3yomq5qye0ni/API-NG+Overview).


# Access Betfair API from R 


The `betfairly` package allows to access most of the Betfair
[API](https://docs.developer.betfair.com/betfair/) directly from R. The free API
is fully implemented. For the description of payed and free access types check
this [page](http://bdp.betfair.com/index.php?option=com_content&task=view&id=36&Itemid=64).


## Details

For the list of all implemented functions and the details of the current
development status please see the [todo](inst/extra/todo.org).


For the basic usage please see [examples.R](inst/extra/examples.R). 

Make sure that you have read the _*`betfairly-package`*_ section from the
[manual](http://betfairly.googlecode.com/files/betfairly_manual.pdf). before you
start using it.

## Instalation

You can install the most recent version of the spackage directly from github
with `devtools` package:

```R
library(devtools)
install_github("betfairly", "vitoshka")
```

<!-- This package is also available from CRAN: -->

<!-- ```R -->
<!-- install.packages("betfairly") -->
<!-- ``` -->


## Reporting Bugs

Before reporting bugs please see the relevant section in the official Betfair <a
href="https://docs.developer.betfair.com/betfair/">documentation</a> and ensure
it's not a betfair service issue. Known issues are documented for each Betfair
API action. See also the [FAQ](http://code.google.com/p/betfairly/wiki/FAQ)
page.


## Disclaimer

The betfairly package is provided with absolutely no warranty. The documentation
of the functional API is an adapted and abbreviated version of official
[Betfair documentation](https://docs.developer.betfair.com/betfair/). Please
refer to it for the complete reference.
