# cv.models: Model selection and hyper-parameter tuning by cross validation

This package provides simple interface for cross validation and hyper-parameter selection for several models.

To install the package, please copy&paste following code into your R terminal.

```{r}
install.packages(
    c("model.adapter", "cv.models"), type = "source",
    repos = c(
        "http://florivory.net/R/repos", options()$repos
    )
)
```

To build the package use `source("build.r")`.
To install the package, use `source("install.r")`.

For the details see Quick start guide ([English](http://florivory.net/R/cv.models/cv.models.html), [Source Code](https://github.com/Marchen/cv.models/blob/master/vignettes/cv.models.rmd)) or ([Japanese](http://florivory.net/R/cv.models/cv.models.j.html), [Source Code](https://github.com/Marchen/cv.models/blob/master/vignettes/cv.models.j.rmd)).