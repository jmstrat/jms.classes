# jms.classes

Provides functionality to other R packages, not intended to be used directly.


## Installation

This pakage is not designed to be used directly by users, and will usually be installed automatically during
installation of another package. If you wish to install it manually, follow the steps below:

1. Install the devtools package if you do not already have it:

   `install.packages("devtools")`

2. Install this package

   `devtools::install_github("jmstrat/jms.classes")`


## Details

This package serves three main purposes:

* Defines a data object for the storage of experimental data
* Defines a database object for the storage of small amounts of metadata relating to experimental data
* Provides utility functions for use in other packages

### Data objects

A `jms.data.object` is effectively just a `data.frame` that keeps track of which column represents the x-axis
and which columns represent the y-axes. This means that it can be easily plotted, and can behave slightly
more inteligently in certain circumstances:

* **Arithmetic Operations**
  i.e. `+`, `-`, `*`, `/` only apply to y columns by default

* **Range Operations**
  i.e. `range`, `min`, `max` only apply to y columns by default

* **diff**
  Calculates the difference between 2 different `jms.data.objects` using their respective x and y columns.

* **Plotting**
  Data objects produce a sensible default plot when using `plot(myData)`

* **Interactive Plotting**
  Data objects can produce an interactive plot using `iplot`

* **Exporting**
  Data objects can be exported as a csv file using `export`

You can convert a `jms.data.object` into a `data.frame` using `as.data.frame`.

### Database objects

A database object holds database tables, which are used to store metadata relating to experiments.
See [Echem.Database](https://github.com/jmstrat/echem.database) for an example.
Database objects automatically sync to disk and update across different sessions, they can be made
[shiny](https://shiny.rstudio.com) compatible using the `as.jms.reactive.database` function.

## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details

