library(ungeneanno)

context("Test getGroupGeneList")

test_that("getGroupGenelist groups genes correctly", {
  f <- matrix(c(1,"BRAF",2,"BRAF",2,"PYN1",3,"SMURF2",3,"BRAF"),ncol=2,byrow=TRUE)
  g <- getUniqueGeneList(geneanno(),f)
  expect_is(getGroupGeneList(g,f),"list")
  d <- list("1" = list("BRAF"),"2" = list("BRAF","PYN1"),"3" = list("SMURF2","BRAF"))
  expect_identical(getGroupGeneList(g,f), d)
})
