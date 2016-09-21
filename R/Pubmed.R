library(httr)
library(XML)
library(methods)

#' Gather NIH Pubmed Data
#'
#' \code{getPublicationList} collates Pubmed data returned from a html request for input string.
#'
#' @param query A query string to be submitted to Pubmed
#' @return A vector of \code{pubmed} objects each detailing a publication relating to the entered query combination requested.
#' @importFrom methods setGeneric setMethod
#' @importFrom XML xmlParse xpathApply xmlClone xmlValue getNodeSet
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' query <- "Thompson IR HIV"
#' ReturnedPublications <- getPublicationList(query)
#' }
#' @export
#'
setGeneric("getPublicationList", function(query) {
  standardGeneric("getPublicationList")
})

#' @describeIn getPublicationList as above.
setMethod("getPublicationList","character", function(query){
  y <- getNihQuery(query(),"pubmed",gsub(" ","+",query))
  q = sprintf("%sesummary.fcgi?db=%s&query_key=%s&WebEnv=%s",y@nihbase,y@db,y@querykey,y@webenv)
  print("Getting Results")
  r = xmlParse(GET(q))
  v = vector()
  j <- getNodeSet(r,"//DocSum")
  if (length(j) >0 ) {
    for ( i in 1:length(j) ){
      #print(i)
      z <- pubmed()
      z@Id <- unlist(xpathApply(xmlClone(j[[i]]),"//Id",xmlValue))
      z@Journal <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='Source']",xmlValue))
      z@Date <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='PubDate']",xmlValue))
      z@Authors <- ifelse(xpathApply(xmlClone(j[[i]]),"//Item[@Name='AuthorList']",xmlValue) == "","",xpathApply(xmlClone(j[[i]]),"//Item[@Name='AuthorList']/Item[@Name='Author']",xmlValue) )
      z@Title <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='Title']",xmlValue))
      z@Volume <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='Volume']",xmlValue))
      z@Issue <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='Issue']",xmlValue))
      z@Pages <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='Pages']",xmlValue))
      a <- unlist(xpathApply(xmlClone(j[[i]]),"//Item[@Name='DOI']",xmlValue))
      z@DOI <- ifelse(is.null(a),"",a)
      v <- append(v,z)
    }
  }
  return(v)
}
)


#' Carry out Pubmed search for a mtrix
#'
#' \code{searchPublications} carries out a series of Pubmed searches for each row of information in the given matrix.
#' The function uses the same matrix used for input to the \code{getUniqueGeneList} function, however
#' given the nature of the search it is best to ensure the groups have meaningful names and not arbitrary numbers.
#'
#' @param query A matrix, as input to getUniqueGenes,  string to be submitted to Pubmed
#' @return A 2 column matrix containg the query string and a list of \code{pubmed} objects each detailing a publication relating to the respective query.
#' @importFrom methods setGeneric setMethod
#' @examples
#' \dontrun{
#' query   f <- matrix(c("Axitinib","BRAF","Imatinib","BRAF"),ncol=2,byrow=TRUE)
#' ReturnedPublications <- searchPublications(query)
#' }
#' @export
#'
setGeneric("searchPublications", function(query) {
  standardGeneric("searchPublications")
})

#' @describeIn searchPublications as above.
setMethod("searchPublications","matrix",
          function(query){
            searchStrings <- createSearchString(query)
            pub <- lapply(searchStrings[[2]], function(x) getPublicationList(x))
            out <- cbind(searchStrings[[1]],pub)
            return(out)
          }
)
#http://www.ncbi.nlm.nih.gov/pubmed?term=(BRAF%5BTitle%2FAbstract%5D)%20AND%20(%222014%22%5BDate%20-%20Publication%5D%20%3A%20%223000%22%5BDate%20-%20Publication%5D)


# @describeIn createSearchString object of type geneanno; a copy of input object having the additional list of group numbers and a unique list of genes from \code{s}

setGeneric("createSearchString", function(query) {
  standardGeneric("createSearchString")
})

setMethod("createSearchString","character",
          function(query){
            query <- paste(query,collapse=" ")
            query2 <- gsub(" ","%20",query,perl=TRUE)
            return(list(query,query2))
          }
)


setMethod("createSearchString","matrix",
          function(query){
            query <- apply(query,1, function(x) paste(x,collapse=" ") )
            query2 <- gsub(" ","%20",query,perl=TRUE)
            return(list(query,query2))
          }
)

