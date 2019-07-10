#==============================================================================
#	Unit test of MESS calculation
#==============================================================================
library(testthat)
#library(cv.models)


#------------------------------------------------------------------------------
context("Unit test for distance using the Vincenty's formulae.")

deg2rad <- cv.models:::deg2rad
vincenty.distance <- cv.models:::vincenty.distance


#------------------------------------------------------------------------------
test_that(
	"Test correctness of distance calculation using the Vincenty's formulae.",
	{
		# Test data were calculated by:
		# http://geographiclib.sourceforge.net/cgi-bin/GeodSolve
		data <- list(
			# Correct cases.
			list(x1 = 130, y1 = 45, x2 = 135, y2 = 50, d = 671179.867),
			list(x1 = 135, y1 = 50, x2 = 130, y2 = 45, d = 671179.867),
			list(x1 = 0, y1 = 0, x2 = 135, y2 = 50, d = 13021637.948),
			# Vectorized version.
			list(
				x1 = rep(130, 2), y1 = rep(45, 2),
				x2 = rep(135, 2), y2 = rep(50, 2), d = rep(671179.867, 2)
			),
			# Negative coordinates
			list(x1 = -130, y1 = 0, x2 = 135, y2 = 50, d = 10367107.083856503),
			list(x1 = -130, y1 = 0, x2 = -135, y2 = -50, d = 5561303.74228109),
			# Extreme values.
			list(x1 = -180, y1 = 0, x2 = 180, y2 = 90, d = 10001965.729312723),
			list(x1 = 180, y1 = 90, x2 = 180, y2 = 89.999999, d = 0.111693980)
		)
		for (i in data) {
			expect_equal(
				vincenty.distance(
					6378137, 6356752.3142451793,
					deg2rad(i$x1), deg2rad(i$y1), deg2rad(i$x2), deg2rad(i$y2)
				),
				i$d
			)
		}
	}
)
