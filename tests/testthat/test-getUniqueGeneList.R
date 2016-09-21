library(ungeneanno)

context("Test getUniqueGeneList")

test_that("getUniqueGenelist produces unique list of genes", {
  f <- matrix(c(1,"BRAF",1,"BRAF",1,"PYN1",1,"SMURF2",1,"BRAF"),ncol=2,byrow=TRUE)
  d <- c("BRAF","PYN1","SMURF2")
  expect_identical(getUniqueGeneList(geneanno(),f)@genelist, d)
})

test_that("getUniqueGenelist groups genes", {
  f <- matrix(c(1,"BRAF",2,"BRAF",2,"PYN1",3,"SMURF2",3,"BRAF"),ncol=2,byrow=TRUE)
  d <- c("1","2","3")
  expect_identical(getUniqueGeneList(geneanno(),f)@groupnos, d)
})
