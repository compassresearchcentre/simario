context("Collate tests")

test_that("collator_freqs_remove_zero_cat2 output has row names", {

	run_tables <- 
		structure(list(run1 = structure(list(`1` = structure(c(899, 118
								), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), `2` = structure(c(897, 
										120), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`3` = structure(c(920, 97), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `4` = structure(c(928, 89), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `5` = structure(c(939, 
										78), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`6` = structure(c(951, 66), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `7` = structure(c(959, 58), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `8` = structure(c(973, 
										44), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`9` = structure(c(977, 40), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `10` = structure(c(974, 43), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `11` = structure(c(969, 
										48), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`12` = structure(c(980, 37), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `13` = structure(c(982, 35), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL))), .Names = c("1", 
						"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"
				), meta = structure("z1accomLvl1", .Names = "varname")), run2 = structure(list(
						`1` = structure(c(899, 118), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `2` = structure(c(914, 103), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `3` = structure(c(929, 
										88), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`4` = structure(c(943, 74), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `5` = structure(c(942, 75), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `6` = structure(c(951, 
										66), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`7` = structure(c(952, 65), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `8` = structure(c(954, 63), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `9` = structure(c(964, 
										53), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`10` = structure(c(968, 49), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL)), `11` = structure(c(970, 47), .Dim = c(2L, 
										1L), .Dimnames = list(c("0", "1"), NULL)), `12` = structure(c(978, 
										39), .Dim = c(2L, 1L), .Dimnames = list(c("0", "1"), NULL)), 
						`13` = structure(c(975, 42), .Dim = c(2L, 1L), .Dimnames = list(
										c("0", "1"), NULL))), .Names = c("1", "2", "3", "4", 
						"5", "6", "7", "8", "9", "10", "11", "12", "13"), meta = structure("z1accomLvl1", .Names = "varname"))), .Names = c("run1", 
		"run2"))

	expected_rownames <- names(run_tables[[1]])
	
	dict <- dict.MELC
	CI <- TRUE
	binbreaks <- NULL
	runs <- run_tables
	cat.adjustments <- NULL
	
	actual <- collator_freqs_remove_zero_cat2(runs, dict=dict, CI=CI, cat.adjustments=cat.adjustments, binbreaks=binbreaks)
	
	expect_that (rownames(actual), equals(expected_rownames))

})

test_that("mean_array_z_pctile_CIs2 output has row names", {
	runs_array <- structure(c(899, 897, 920, 928, 939, 951, 959, 973, 977, 974, 
					969, 980, 982, 118, 120, 97, 89, 78, 66, 58, 44, 40, 43, 48, 
					37, 35, 899, 914, 929, 943, 942, 951, 952, 954, 964, 968, 970, 
					978, 975, 118, 103, 88, 74, 75, 66, 65, 63, 53, 49, 47, 39, 42
			), .Dim = c(13L, 2L, 2L), .Dimnames = list(c("1", "2", "3", "4", 
							"5", "6", "7", "8", "9", "10", "11", "12", "13"), c("0", "1"), 
					c("run1", "run2")), meta = structure("z1accomLvl1", .Names = "varname"))
	dict <- dict.MELC
	CI <- TRUE
	binbreaks <- NULL
	cat.adjustments <- NULL
	xa <- runs_array
	NA.as.zero=T
	
	actual <- mean_array_z_pctile_CIs2(xa, CI=CI, NA.as.zero=NA.as.zero, cat.adjustments=cat.adjustments, dict=dict, binbreaks=binbreaks)
	expect_that (rownames(actual), equals(rownames(runs_array)))		
})

