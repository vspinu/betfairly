# Access Betfair API from R 


The `betfairly` package allows to access most of the Betfair <a
href="https://docs.developer.betfair.com/betfair/">API</a> directly from R. The
free API is fully implemented. For the description of payed and free access
types check this <ahref="http://bdp.betfair.com/index.php?option=com_content&task=view&id=36&Itemid=64">
page</a>. </p>


## Details

For the list of all implemented functions and the details of the current
development status please see the [todo](https://github.com/vitoshka/betfairly/blob/inst/extra/todo.org) list.


For the basic usage please see [examples.R](https://github.com/vitoshka/betfairly/blob/inst/extra/examples.R). 

Make sure that you have read the _*`betfairly-package`*_ section from the <a
href="http://betfairly.googlecode.com/files/betfairly_manual.pdf">manual</a>
before you start using it.

## Instalation

You can install the most recent version of the spackage directly from github
with `devtools` package:

```R
library(devtools)
install_github("betfairly", username = "vitoshka")
```

This package is also available from CRAN:

```R
install.packages("betfairly")
```


## Reporting Bugs

Before reporting bugs please see the relevant section in the official Betfair <a
href="https://docs.developer.betfair.com/betfair/">documentation</a> and ensure
it's not a betfair service issue. Known issues are documented for each Betfair
API action. See also the [http://code.google.com/p/betfairly/wiki/FAQ FAQ]
page.


## Disclaimer

The betfairly package is provided with absolutely no warranty. The documentation
of the functional API is an adapted and abbreviated version of official
[Betfair documentation](https://docs.developer.betfair.com/betfair/). Please
refer to it for the complete reference.
