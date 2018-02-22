## interlineaR : utility functions for turning into data frame XML documents containing interlinearized corpora and associated dictionaries.

**Author:** Sylvain Loiseau<br/>
**License:** [BSD_3_clause](https://opensource.org/licenses/BSD-3-Clause)


# Installation

```{r}
devtools::install_github("sylvainloiseau/interlineaR")
```

# Usage

Import an interlinearised corpus in the EMELD XML format (as exported from SIL FieldWorks for instance) :

```{r}
corpus <- read.emeld("path")
```

Import a dictionary in the LIFT XML format (as exported from SIL FieldWorks for instance) :

```{r}
dictionary <- read.lift("path", language.code="tww")
```
