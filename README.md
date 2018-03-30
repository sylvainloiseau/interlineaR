## interlineaR : utility functions for turning into data frame XML documents containing interlinearized corpora and associated dictionaries.

**Author:** Sylvain Loiseau<br/>
**License:** [BSD_3_clause](https://opensource.org/licenses/BSD-3-Clause)


# Installation

```{r}
devtools::install_github("sylvainloiseau/interlineaR")
```

# Usage

Import an interlinearised corpus in the EMELD XML format (as exported from SIL FieldWorks for instance):

```{r}
path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
corpus <- read.emeld(path, vernacular.languages="tww")
```

Import an interlinearised corpus in Toolbox (SIL) format:

```{r}
path <- system.file("exampleData", "tuwariToolbox.txt", package="interlineaR")
corpus <- read.toolbox(path)
```

Import a dictionary in the LIFT XML format (as exported from SIL FieldWorks for instance):

```{r}
dicpath <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
dictionary <- read.lift(dicpath, language.code="tww")
```

# Documentation

See the vignette interlineaR for an overview of the data model and the functions of this package.