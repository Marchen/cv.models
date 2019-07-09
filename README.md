# cv.models: Model selection and hyper-parameter tuning by cross validation

[![Build Status](https://travis-ci.com/Marchen/cv.models.svg?branch=master)](https://travis-ci.com/Marchen/cv.models)

This package provides simple interface for cross validation and hyper-parameter selection for several models.

To install the package, please copy & paste following code into your R terminal if you have `devtools` package.

```{R}
devtools::install_github("marchen/model.adapter@*release")
devtools::install_github("marchen/cv.models@*release")
```

If not, use following code.

```{R}
install.packages(
    c("model.adapter", "cv.models"), type = "source",
    repos = c(
        "http://florivory.net/R/repos", options()$repos
    )
)
```

For the details see Quick start guide ([English](http://florivory.net/R/cv.models/cv.models.html), [Source Code](https://github.com/Marchen/cv.models/blob/master/vignettes/cv.models.rmd)) or ([Japanese](http://florivory.net/R/cv.models/cv.models.j.html), [Source Code](https://github.com/Marchen/cv.models/blob/master/vignettes/cv.models.j.rmd)).