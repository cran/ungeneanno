library(httr)
library(XML)
library(methods)

#' Gather Uniprot Data
#'
#' \code{getUniprotSummary} collates data returned from a html request to the Uniport publicly available databases to populate the gene object.
#' The query currently returns id, entry name, reviewed, protein names, genes, organism, length and "comment(FUNCTION)".
#'
#' @param x \code{gene} object
#' @param y \code{query} object (see \code{getNihQuery()})
# @param col comma-seperated string of column names to be returned from the Uniport database (see http://www.uniprot.org/help/uniprotkb_column_names). Defaults to id, entry name, reviewed, protein names, genes, organism, length and "comment(FUNCTION)"
#' @return \code{gene} object - a copy of the input object 'y', having the uniprot data added.
#' @importFrom methods setGeneric setMethod
#' @seealso \url{http://www.uniprot.org/help/uniprotkb_column_names}
#' @importFrom httr content GET
#' @examples
#' \dontrun{
#' f <- query()
#' f@gene <- "BRAF"
#' gene <- gene()
#' gene@name <- f@gene
#' gene <- getUniprotSummary(gene,f)
#' }
#' @export
#'
setGeneric("getUniprotSummary", function(x,y) {
  standardGeneric("getUniprotSummary")
})
# setGeneric("getUniprotSummary", function(x,y,col="id,entry name,reviewed,protein names,genes,organism,length,comment(FUNCTION)") {

#' @describeIn getUniprotSummary \code{gene} object - a copy of the input object 'y', having the uniprot data added.
setMethod("getUniprotSummary","gene", function(x,y){
    cols = strsplit(y@col,",")
    #q = sprintf("%s?query=%s+%s&columns=%s",y@uniprotbase,y@gene,y@uniprotquery,gsub(" ","%20",gsub(", ",",",y@col)))
    q = sprintf("%s?query=%s+%s&columns=%s",y@uniprotbase,y@gene,y@uniprotquery,gsub(" ","%20",gsub(", ",",",toString(cols[[1]]))))
    #print(q)
    r = GET(q)
    s <- content(r,"text")
    for (i in strsplit(s,"\n")[[1]]){
        j <- strsplit(i,"\t")[[1]]
        #l <- strsplit(j[5]," ")[[1]]
        l <- strsplit(j[which(cols[[1]]=="genes")]," ")[[1]]
        if (y@gene %in% l || x@name %in% l){
            x@uniprot_summary = strsplit(j[which(cols[[1]]=="comment(FUNCTION)")],"FUNCTION: ")[[1]]
            x@uniprot_protein_name = j[which(cols[[1]]=="protein names")]
            x@uniprot_name = j[which(cols[[1]]=="genes")]
        }
    }
    return(x)
  }
)

