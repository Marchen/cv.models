# 2019-07-09: 0.1.4

- Add Spearman's rho and Kendall's tau as performance measures for regression models.
- Start CI with Travis-CI.
- Bug fixes.

# 2018-11-10: 0.1.3

- Add `group` argument which controls user defined grouping for cross validation.

# 2018-09-14: 0.1.2

- Support model adapter > 0.1.0.
- Bug fix: occasionally fail to detect number of CPU cores.
- Add English vignette.

# 2018-01-31: 0.1.1

- Bug fix: cross validation is not conducted when model object or call produced by call() function is specified.

# 2018-01-28: 0.1.0

- Now find.best.models() uses the seed of random numbers in cv.models object if seed in the object is not NULL.
- Replaced dependency on pROC with OptimalCutpoints.
- Add calculation of Cohen's Kappa for metrics of classification models.
- Bug fix: specifying 'seed' returns same result even with cluster.
- Minor bug fix of print.cv.best.models().

# 2017-09-10: 0.0.4

- Experimental: implemented log-likelihood and likelihood based R squared values for classification models.
- Bug fix: wrong assignment of CPU cores.

# 2017-07-07: 0.0.3

- Bug fix: occasionally evaluation of model fails.

# 2017-07-07: 0.0.2

- Bug fix: correspondence between the response variable and the predicted values was broken.

# 2017-07-05: 0.0.1

- Started versioning.
