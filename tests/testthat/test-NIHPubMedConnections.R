library(ungeneanno)

context("Test NIH PubMed connections")

test_that("Create string for PubMed searches",{
  #  testthat::skip("Skip test creating strings")
  f <- matrix(c("Axitinib","BRAF","Imatinib","BRAF","Axitinib","PTEN","Imatinib","SMURF2"),ncol=2,byrow=TRUE)
  d <- c("Axitinib%20BRAF","Imatinib%20BRAF","Axitinib%20PTEN","Imatinib%20SMURF2")
  e <- c("Axitinib BRAF","Imatinib BRAF","Axitinib PTEN","Imatinib SMURF2")
  expect_equal(createSearchString(f[1,])[[2]],d[1])
  expect_equal(createSearchString(f[1,])[[1]],e[1])
  expect_equal(createSearchString(f)[[2]],d)
  expect_equal(createSearchString(f)[[1]],e)
})

test_that("Gets from NCBI PubMed using PMID",{
#  testthat::skip("Pubmed ID skip")
  testthat::skip_on_cran()
  y <- getNihQuery(query(),"pubmed","19486507")
  query <- sprintf("%sesummary.fcgi?db=%s&query_key=%s&WebEnv=%s",y@nihbase,y@db,y@querykey,y@webenv)
  a <- xmlParse(GET(query))
  expect_equal_to_reference(a,"ncbi_test_pubmed.rds")
  expect_equal(unlist(xpathApply(xmlClone(a),"//Item[@Name='Source']",xmlValue)),"AIDS Res Ther")
})

test_that("Gets from NCBI PubMed using Text Search",{
#  testthat::skip("Pubmed Test Search skip")
  testthat::skip_on_cran()
  y <- getNihQuery(query(),"pubmed",gsub(" ","+","Thompson IR[Author] HIV"))
  query <- sprintf("%sesummary.fcgi?db=%s&query_key=%s&WebEnv=%s",y@nihbase,y@db,y@querykey,y@webenv)
  a <- xmlParse(GET(query))
  expect_equal_to_reference(a,"ncbi_test_pubmed2.rds")
  expect_equal(unlist(xpathApply(xmlClone(a),"//Item[@Name='Source']",xmlValue)),"AIDS Res Ther")
})


