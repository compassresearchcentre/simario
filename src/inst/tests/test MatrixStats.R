context("MatrixStats tests")

test_that("mean_mx_cols_BCASO without grouping - output includes rownames", {
	
	mx <- matrix (c(1:10), ncol = 2)
	colnames(mx) <- c("A", "B")
	logiset = NULL; grpby = NULL ; grpby.tag = NULL
	wgts = rep(1, nrow(mx))
	dict = NULL
	 
	actual <- mean_mx_cols_BCASO(mx, grpby=grpby, grpby.tag=grpby.tag, logiset=logiset, wgts=wgts)
	expected <- matrix(c(3,8), ncol=1, dimnames=list(colnames(mx)))
	
	expect_that(actual, equals( expected ) )
})

test_that("mean_mx_cols_BCASO with grouping - output includes rownames", {

		
		mx <- matrix (c(1:10), ncol = 2)
		colnames(mx) <- c("Col1", "Col2")
		logiset = NULL; 
		grpby <- c("A","A","A","B","B") ; grpby.tag = "AB"
		wgts = rep(1, nrow(mx))
		dict = dict.MELC
		
		actual <- mean_mx_cols_BCASO(mx, grpby=grpby, grpby.tag=grpby.tag, logiset=logiset, wgts=wgts, dict=dict)
		expected <- structure(matrix(c(2,7,4.5,9.5), ncol=2, dimnames=list(colnames(mx), c("Not in subgroup", "In subgroup"))), meta=c(grpby.tag="AB"))
		
		expect_that(actual, equals( expected ) )
})


