#' Collate Gene Annotation Data from Uniprot and NIH Gene Databases
#'
#' Taking groups of genes, the package collates together the summary information about those genes from the publicly available resources at Uniprot and NCBI.
#' This is achieved through the \code{\link{getUniqueGeneList}}, \code{\link{getGeneSummary}}, \code{\link{getGroupGeneList}} and \code{\link{produceOutputFiles}} functions.
#' Additionally, the package is able to collate publication information from a search of the NCBI Pubmed database via the \code{\link{getPublicationList}} function.
#'
#' A 2 column matrix, containing a column of 'group' identifers and a column of gene names, is required
#' as input. Unique lists of both group identifiers and gene names are created and the data from the NCBI gene and Uniprot databases
#' is downloaded. A vector of vectors is used to recreate the relationships between the group identifers and gene names, which is then used to
#' create output files detailing the downloaded information for the genes in each group.
#'
#' @author Richard Thompson <ithompson@qf.org.qa>
# @seealso \code{\link{getUniqueGeneList}}
#' @seealso \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}
#' @seealso \url{http://www.uniprot.org/help/uniprotkb_column_names}
#' @examples
#' \dontrun{
#' ## Create geneanno object and set save directory
#' ga <- geneanno()
#' ga@fileroot <- "~/Desktop"
#'
#' ## Parse input gene names and group identifiers into unique lists
#' data("genematrix")
#' ga <- getUniqueGeneList(ga,genematrix)
#'
#' ## Query databases and Parse responses
#' gs <- getGeneSummary(ga)
#'
#' ## Create vector of vectors containing gene names for each group identifier
#' dgl <- getGroupGeneList(ga,genematrix)
#'
#' ## Collate data into out files
#' produceOutputFiles(ga, dgl, gs)
#'
#' ## Query PubMed database
#' query <- "Thompson IR HIV"
#' ReturnedPublications <- getPublicationList(query)
#'
#' ## List PubMedIDs from PubMed query
#' for (i in 1:length(ReturnedPublications)){
#'   print(ReturnedPublications[[i]]@Id)
#'   }
#' }
"_PACKAGE"
