context("MatrixUtil tests")

test_that("subsetFirstDimension", {
	
	x = c(1,2,3,4)
	logiset4 = c(T,F,T,F)
			
	expect_that(subsetFirstDimension(x, logiset4), equals( c(1,3) ))
	
	mx <- matrix(c(1:6), nrow=2)
	logiset2 <- c(T,F)
	
	mx_subset <- subsetFirstDimension(mx, logiset2)
	expect_that(mx_subset, equals( matrix(c(1,3,5), nrow=1) ))

	arr <- array(c(1:9), dim=c(2,3,1))
	
	arr_subset <- subsetFirstDimension(arr, logiset2)
	expect_that(arr_subset, equals( array(c(1,3,5), dim=c(1,3,1))  ))
	
	
})

