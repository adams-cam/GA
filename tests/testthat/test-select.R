context('select')

# test data
y <- mtcars$mpg
x <- as.matrix(mtcars[, c(-1)])

# test GA::select
test_that('GA algorithm works ',
            {test <- GA::select(y, x, family = "gaussian",
                                objective_function = stats::AIC, verbose = F)
              expect_type(test, "list") #list
              expect_s3_class(test, "GA") # of class GA
              expect_type(GA::select(y, x,
                    family = "gaussian",
                    objective_function = stats::AIC, verbose = F)$Best_model,
                        "character")
})

test_that('GA algorithm does not converge',
            {expect_equal(GA::select(y, x, family = "gaussian", iter = 2,
                                mutation_rate = 0.8, verbose = F)$converged, "No")
            expect_equal(GA::select(y, x, family = "gaussian", iter = 2,
                                        mutation_rate = 0.8, verbose = F)$iter, 2)
})

test_that('test for input errors',
            {expect_error(GA::select(y, "foo", family = "gaussian", verbose = F))
            expect_error(GA::select(x, family = "gaussian", verbose = F))
            expect_error(GA::select(y[-1], x, , verbose = F))
            expect_error(GA::select(y, x, family = "binomial", verbose = F))
            expect_error(GA::select(y, x, nCores = 1000L, verbose = F))
            expect_error(GA::select(cbind(y, y, y), x, verbose = F))
            expect_error(GA::select(y, x, objective_function = "AIC", verbose = F))
            expect_error(GA::select(y, x, minimize = "True", verbose = F))
            expect_error(GA::select(y, x, family = "gessian", verbose = F))
            expect_error(GA::select(y, x, converge = "Yes please do", verbose = F))
})

