library(ungeneanno)

context("Test Uniprot connections")

test_that("Gets from Uniprot database",{
  #  testthat::skip("Uniprot gene skip")
  testthat::skip_on_cran()
  f <- query()
  f@gene <- "BRAF"
  g <- gene()
  g@name <- f@gene
#  query <- sprintf("Homo+sapiens[organism]+%s[gene+name]+alive[prop]",f@gene)
  p <- getUniprotSummary(g,f)
  expect_equal_to_reference(p,"uniprot_test_gene.rds")
  expect_equal(p@uniprot_name,"BRAF BRAF1 RAFB1")
})
