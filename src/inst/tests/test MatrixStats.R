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


test_that("table_mx_cols_BCASO without grouping or logiset - output includes rownames", {

	mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(rownames=NULL, colnames=(LETTERS[1:5])))
	dict = dict.MELC
	table_mx_cols_BCASO(mx, dict=dict)
	expect_that(actual, equals( expected ) )
})




