#==============================================================================
#	Unit test of cv.models.object with user defined group
#==============================================================================

context("Unit test for cv.models.object with user defined group.")

require(randomForest)

test_that(
	"Test folds with user defined group with different variable types", {
		create.test.object <- function(group) {
			call <- substitute(randomForest(Petal.Length ~ ., data = iris))
			object <- cv.models:::cv.models.object(
				call, folds = 10, stratify = TRUE, n.cores = 1, seed = 1,
				positive.class = NULL, package.name = NULL,
				envir = environment(), aggregate.method = "join",
				grid = NULL, grid.predict = NULL, group = group,
				cutpoint.options = list()
			)
			return(object)
		}
		obj <- create.test.object(rep(c("a","b"), each = 75))
		expect_equal(obj$folds, 2)
		obj <- create.test.object(rep(1:3, each = 50))
		expect_equal(obj$folds, 3)
		obj <- create.test.object(rep(c(TRUE, FALSE), each = 75))
		expect_equal(obj$folds, 2)
		obj <- create.test.object(iris$Species)
		expect_equal(obj$folds, 3)
	}
)
