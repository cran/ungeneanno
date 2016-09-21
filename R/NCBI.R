library(httr)
library(XML)
library(methods)


#' create initial NIH Query
#'
#' \code{getNihQuery} contacts the NIH E-utilities and carries out an initial data query.
#' Accessing the NCBI NIH databases is a two stage process; the \code{getNihQuery} function
#' carries out the first stage, returning a short list of IDs and a unique query key. This Query key
#' is then used in the second stage to fetch the results of the query.
#'
#' @param x \code{query} object.
#' @param db The name of the NCBI database to be searched for the query, e.g. "gene" or "pubmed"
#' @param query The query string for the database
#' @return \code{query} object a copy of the input object 'x', having the query specifiers from NIH added.
#' @seealso \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}
#' @importFrom methods setGeneric setMethod
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' f <- query()
#' f@gene <- "BRAF"
#' db <- "gene"
#' f <- getNihQuery(f,db,f@gene)
#' gene <- getNihSummary(gene(),f)
#' }
#' @export
#'
setGeneric("getNihQuery", function(x,db,query) {
  standardGeneric("getNihQuery")
})

#' @describeIn getNihQuery \code{query} object a copy of the input object 'x', having the query specifiers from NIH added.
setMethod("getNihQuery","query", function(x,db,query){
    x@db = db
    q = sprintf("%sesearch.fcgi?db=%s&term=%s&usehistory=y",x@nihbase,x@db,query)
    #print(q)
    r = xmlParse(GET(q))
    x@webenv = xmlToList(r)$WebEnv
    x@querykey = xmlToList(r)$QueryKey
    return(x)
    }
)

#' Gather NIH Gene Data
#'
#' \code{getNihSummary} collates data returned from a html request to the NCBI publicly available gene database to populate the gene object.
#' Accessing the NCBI NIH databases is a two stage process; the \code{getNihSummary} function
#' carries out the second stage in relation to the NCBI "gene" database, populating NIH specific slots in a \code{gene} object.
#'
#' @param x \code{gene} object
#' @param y \code{query} object (see \code{getNihQuery()})
#' @return \code{gene} object a copy of the input object 'x', having the uniprot data added.
#' @importFrom methods setGeneric setMethod
#' @importFrom XML xmlParse xmlToList
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' f <- query()
#' f@gene <- "BRAF"
#' db <- "gene"
#' f <- getNihQuery(f,db,f@gene)
#' gene <- getNihSummary(gene(),f)
#' }
#' @export
#'
setGeneric("getNihSummary", function(x,y) {
  standardGeneric("getNihSummary")
})

#' @describeIn getNihSummary \code{gene} object a copy of the input object 'x', having the uniprot data added.
setMethod("getNihSummary","gene", function(x,y){
    q = sprintf("%sesummary.fcgi?db=%s&query_key=%s&WebEnv=%s",y@nihbase,y@db,y@querykey,y@webenv)
    r = xmlParse(GET(q))
    s <-xmlToList(r)
    x@name = s$DocumentSummarySet$DocumentSummary$Name
    if (is.character(s$DocumentSummarySet$DocumentSummary$Summary)) {
        x@nih_summary = s$DocumentSummarySet$DocumentSummary$Summary
    }
    x@symbol = s$DocumentSummarySet$DocumentSummary$NomenclatureSymbol
    if (!is.null(s$DocumentSummarySet$DocumentSummary$OtherAliases)){
        x@alternatives = s$DocumentSummarySet$DocumentSummary$OtherAliases
    }
    x@nih_id = as.integer(s$DocumentSummarySet$DocumentSummary$.attrs['uid'])
    x@chromosome = s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ChrLoc
    x@start = s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ChrStart
    x@stop = s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ChrStop
    x@exon_count = s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ExonCount
    x@chraccver = s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ChrAccVer
    if (!is.null(s$DocumentSummarySet$DocumentSummary$map_location)){
      x@map_location = s$DocumentSummarySet$DocumentSummary$map_location
    }
    x@direction = ifelse(s$DocumentSummarySet$DocumentSummary$ChrStart == s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ChrStart,"forward",ifelse(s$DocumentSummarySet$DocumentSummary$ChrStart == s$DocumentSummarySet$DocumentSummary$GenomicInfo$GenomicInfoType$ChrStop,"reverse","NA"))
    return(x)
    }
)

