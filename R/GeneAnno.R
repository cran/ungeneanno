library(httr)
library(XML)
library(methods)

#' An S4 class to represent the base information used by the methods.
#'
#' @slot db A character string stating the NIH/NCBI database to be queried. (Default - "gene")
#' @slot nihbase A character string stating the base URL for NIH/NCBI queries.
#' @slot ids A Vector of character strings
#' @slot uniprotbase A character string stating the base URL for Uniprot queries.
#' @slot uniprotquery A character string stating additional information to optimize the query to the Uniport database.
#' @slot uniprotcolumns A character string stating the database columns to be returned from the Uniport database.
#' @slot genelist A vector of character strings representing a unique list of the genes queried.
#' @slot fileroot A charcter string of the directory for file input and storage. (defaults to working directory)
#' @slot outputstem A character string stating the sub-directory of 'fileroot' into which output files are saved. (Default - "gene_annotations")
#' @slot genefilestem A character string stating the sub-directory of 'fileroot' into which gene objects are saved. (Default - "genes")
#' @slot groupnos A numeric vector representing the identifiers of groups in the input file/list.
#' @importFrom methods setClass new slot slotNames
#' @export geneanno
geneanno <- setClass("geneanno",
    slots= list(
        db="character",
        nihbase = "character",
        ids="vector",
        uniprotbase = "character",
        uniprotquery="character",
        uniprotcolumns="character",
        genelist="vector",
        fileroot="character",
        outputstem="character",
        genefilestem="character",
        groupnos="vector"),
    prototype=list(
                db="gene",
                nihbase = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
                uniprotquery = "AND+reviewed:yes+AND+organism:9606&format=tab",
                uniprotcolumns = "columns=id,entry%20name,reviewed,protein%20names,genes,organism,length,comment(FUNCTION)",
                uniprotbase = "http://www.uniprot.org/uniprot/",
                outputstem = "gene_annotations",
                genefilestem = "genes",
                ids  = vector("character"),
                genelist  = vector("character"),
                groupnos = vector("character")
    )

)

#' An S4 class to represent an initial query to the NCBI e-utilities, inherits from \code{geneanno} class.
#'
#' @slot gene A character vector of the gene name.
#' @slot querykey a character string identifier from the NCBI Eutils.
#' @slot webenv A character string uniquely identifying the query from the NCBI Eutils.
#' @importFrom methods setClass new slot slotNames
#' @export query
query <- setClass("query",
    contains="geneanno",
    slots=list(
        webenv="character",
        querykey="character",
        gene="character"
    ),
    prototype=list(
        gene="BRAF"
    )
)

#' An S4 class to represent a query to the Pubmed database, inherits from \code{geneanno} class.
#'
#' @slot Id The pubmed ID for the publication
#' @slot Authors List of article authors
#' @slot Date The publication date of the article
#' @slot Title Publication Title
#' @slot Journal The publication journal of the article
#' @slot Volume The journal volume of the article
#' @slot Issue The journal volume issue number of the article
#' @slot Pages The journal pages of the article
#' @slot DOI The unique article Digital Object Identifier (DOI, see www.doi.org)
#' @importFrom methods setClass new slot slotNames
#' @export pubmed
pubmed <- setClass("pubmed",
         slots=list(
           Journal ="character",
           Date ="character",
           Title ="character",
           Id ="character",
           Authors = "vector",
           Volume="character",
           Issue ="character",
           Pages ="character",
           DOI ="character"
         ),
         prototype=list(
           Authors = vector("character")
         )
)


#' An S4 class to represent a gene.
#'
#' @slot name A character string stating the name of the gene as found in NCBI.
#' @slot nih_id A numeric value stating the NCBI gene database identifier for the gene.
#' @slot nih_summary A character string describing the function and interactions of the gene as found in the NCBI.
#' @slot uniprot_name A character string stating the name of the gene as found in Uniprot.
#' @slot uniprot_protein_name A character string stating the name of the protein gene product as found in Uniprot.
#' @slot uniprot_summary A vector of character strings describing the function and interactions of the gene as found in the Uniprot dataset.
#' @slot alternatives A vector of character strings stating commonly used alternative names for the gene (from NCBI).
#' @slot symbol A character string stating the offical gene symbol (from NCBI).
#' @slot chromosome A character string stating the chromosome upon which the gene resides (from NCBI).
#' @slot start A character string stating the chromosomal position of the transcription start (from NCBI).
#' @slot stop A character string stating the chromosomal position of the transcription stop (from NCBI).
#' @slot exon_count A character stating the number of exons in the gene (from NCBI).
#' @slot chraccver A character string stating the NIH accession number and version for the chromosome (from NCBI).
#' @slot map_location A character string representing the location of the gene on the karyotype (from NCBI).
#' @slot direction A character string [forward/reverse] identifying the direction of the gene on the chromosome (from NCBI).
#' @importFrom methods setClass new slot slotNames
#' @export gene
gene <- setClass("gene",
    slots=list(
      name = "character",
      nih_id = "numeric",
      nih_summary = "character",
      uniprot_name = "vector",
      uniprot_protein_name = "character",
      uniprot_summary = "vector",
      alternatives = "vector",
      symbol = "character",
      chromosome = "character",
      start = "character",
      stop = "character",
      exon_count = "character",
      chraccver = "character",
      map_location = "character",
      direction = "character"
    ),
    prototype = list(
        alternatives = vector("character"),
        uniprotname = vector("character"),
        uniprotsummary = vector("character")
    )
)

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
    q = sprintf("%sesearch.fcgi?db=%s&term=%s&usehistory=y",x@nihbase,db,query)
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
    col="id,entry name,reviewed,protein names,genes,organism,length,comment(FUNCTION)"
    q = sprintf("%s?query=%s+%s&columns=%s",y@uniprotbase,y@gene,y@uniprotquery,gsub(" ","%20",gsub(", ",",",col)))
    #print(q)
    r = GET(q)
    s <- content(r,"text")
    for (i in strsplit(s,"\n")[[1]]){
        j <- strsplit(i,"\t")[[1]]
        l <- strsplit(j[5]," ")[[1]]
        if (y@gene %in% l || x@name %in% l){
            x@uniprot_summary = strsplit(j[8],"FUNCTION: ")[[1]]
            x@uniprot_protein_name = j[4]
            x@uniprot_name = j[5]
        }
    }
    return(x)
  }
)

#' Create gene specific objects containing data from online resources
#'
#' \code{getGeneSummary} populates and returns a vector of \code{gene} objects with information sourced from  a series of html requests to the NIH and Uniport publicly available databases.
#'
#' Information returned from a database requests is parsed into a \code{gene} object, which are saved in a 'genes' subdirectory of the working directory.
#' Each \code{gene} object is added to a vector of objects, which is then returned.
#' Where gene information has previously been downloaded and objects saved (within the last seven days), \code{gene} objects are repopulated from the saved files so as to minimise server traffic.
#' \emph{N.B.} the function includes a random wait (of up to 5s) between each gene downloaded.
#'
#' \emph{N.B.} It is possible to define an alternative directory using \code{geneanno@fileroot <- "/path/to/directory"}
#'
#' @param x object of class \code{geneanno}.
#' @return vector of \code{gene} objects, each containing the collated data from the public resources.
#' @importFrom methods setGeneric setMethod
#' @importFrom stats runif
#' @examples
#' \dontrun{
#' geneanno@fileroot <- "~/Desktop"
#' genesummaries <- getGeneSummary(geneanno)
#' }
#' @export
#'
setGeneric("getGeneSummary", function(x) {
  standardGeneric("getGeneSummary")
})

#' @describeIn getGeneSummary Produces a vector of \code{gene} objects, each containing the collated data from the public resources.
setMethod("getGeneSummary","geneanno",
    function(x){
        output = list()
        count = 0
        fileroot <- ifelse(identical(x@fileroot,character(0)),getwd(),x@fileroot)
        if (!dir.exists(file.path(fileroot,x@genefilestem))) {dir.create(file.path(fileroot,x@genefilestem))}
        for (gene in x@genelist){
            filename = file.path(fileroot,x@genefilestem,sprintf("%s.RData",gene))
            count = count + 1
            if (file.exists(filename) && Sys.time() < (file.info(filename)$mtime + 7*86400)){
                load(filename)
                output[[gene]] <- g
                cat(count, gene, "\n" )
            }
            else{
                f <- query()
                f@gene <- gene
                query <- sprintf("Homo+sapiens[organism]+%s[gene+name]+alive[prop]",gene)
                f <- getNihQuery(f,"gene",query)
                cat(count, gene, " step 1..." )
                g <- gene()
                g <- getNihSummary(g,f)
                cat(" 2..." )
                g <- getUniprotSummary(g,f)
                cat("downloaded\n" )
                output[[gene]] <- g
                save(g,file=filename)
                Sys.sleep(runif(1)*5)
            }
        }
        return(output)
    }
)

#' Create unique list of genes from input list
#'
#' \code{getUniqueGeneList} takes a 2-column matrix of group identifiers and gene names, returning the seperated list of unique group identifiers and a unique list of genes.
#'
#' @param x object of class geneanno.
#' @param inputlist vector of strings, being a mixed list of group numbers and gene names
#' @return object of type geneanno; a copy of input object having the additional list of group numbers and a list of genes from \code{uniquelist}
#' @importFrom methods setGeneric setMethod
#' @examples
#' \dontrun{
#' data("genematrix")
#' ga <- getUniqueGeneList(ga,genematrix)
#' }
#' @export
#'
setGeneric("getUniqueGeneList", function(x,inputlist) {
  standardGeneric("getUniqueGeneList")
})

#' @describeIn getUniqueGeneList object of type geneanno; a copy of input object having the additional list of group numbers and a unique list of genes from \code{s}
setMethod("getUniqueGeneList",signature(x="geneanno",inputlist="character"),
    function(x,inputlist){
        cat("inputlist is a List")
        q <- inputlist[grep("^[0-9]",inputlist)]
        x@groupnos <- unique(q)
        inputlist <- inputlist[grep("^[A-Za-z]",inputlist)]
        inputlist <- gsub("^([a-zA-Z0-9]+)\\..*","\\1",inputlist,perl=TRUE)
        x@genelist <- unique(inputlist)
        return(x)
    }
)

#' @describeIn getUniqueGeneList object of type geneanno; a copy of input object having the additional list of group numbers and a unique list of genes from \code{s}
setMethod("getUniqueGeneList",signature(x="geneanno",inputlist="matrix"),
          function(x,inputlist){
            #cat("inputlist is a Matrix")
            x@groupnos <- unique(inputlist[,1])
            s <- inputlist[,2]
            s <- gsub("^([a-zA-Z0-9]+)\\..*","\\1",s,perl=TRUE)
            x@genelist <- unique(s)
            return(x)
          }
)

#' Create unique list of group,gene combinations from input list
#'
#' \code{getGroupGeneList} takes a mixed list of group identifiers (numeric) and gene names or a 2-column matrix of group identifiers and gene names, returning the seperated list of group identifiers and the related lists of genes.
#'
#' @param x object of class geneanno.
#' @param inputlist vector of strings, being a mixed list of group numbers and gene names
#' @return object of type geneanno; a copy of input object having the additional list of group numbers and a unique list of genes from \code{s}
#' @examples
#' \dontrun{
#' data("genematrix")
#' dgl <- getGroupGeneList(geneanno(),genematrix)
#' }
#' @importFrom methods setGeneric setMethod
#' @export
#'
setGeneric("getGroupGeneList", function(x,inputlist) {
  standardGeneric("getGroupGeneList")
})

#' @describeIn getGroupGeneList uses vector of characters of group ids and Gene names for inputlist.
setMethod("getGroupGeneList",signature(x="geneanno", inputlist="character"),
    function(x,inputlist){
        output <- list()
        group = 0
        z <- 1
        for (i in x@groupnos){
            output[[i]] <- list()
        }
        for (i in inputlist){
            if (grepl("^[A-Za-z]",i)){
              output[[group]] <- append(output[[group]],i)
            } else if (grepl("^[0-9]+",i)){
                group <- i
            }
        }
        for (i in x@groupnos){
            t <- output[[i]][!is.null(output[[i]])]
            output[[i]] <- unique(t)
        }
        return(output)
    }
)

#' @describeIn getGroupGeneList uses matrix of group ids and Gene names for inputlist.
setMethod("getGroupGeneList",signature(x="geneanno",inputlist="matrix"),
          function(x,inputlist){
            output <- list()
            group = 0
            for (i in x@groupnos){
              output[[i]] <- list()
            }
            for (i in 1:nrow(inputlist)){
              group <- inputlist[i,1]
              output[[group]] <- append(output[[group]],inputlist[i,2])
            }
            for (i in x@groupnos){
              t <- output[[i]][!is.null(output[[i]])]
              output[[i]] <- unique(t)
            }
            return(output)
          }
)

#' Parse files of gene names and group identifiers to create unique lists of each
#'
#' \code{parseInputFile} takes a mixed file containing group identifiers (numeric) and gene names, returning the list of group identifiers and genes with the remaining columns removed.
#' The package was originally written to work from a file laid out thus:
#' group_id1
#' gene_name1
#' gene_name2
#' group_id2
#' gene_name1
#' gene_name3
#' The methods assume that both group identifiers and gene names are alphanumeric; the group identifiers, where present, begining with a number and gene names starting with a character.
#' please note, this populates the vector with only the alphanumric strings begining each line of the input file. Also, RNA genes (begining ENSG000) are excluded.
#'
#' @param x object of class geneanno.
#' @param file character string providing the name of the input file
#' @return vector of character strings, as exemplified by the inputlist data object.
#' @importFrom methods setGeneric setMethod
#' @importFrom utils read.table
#' @export
#'
setGeneric("parseInputFile", function(x,file) {
  standardGeneric("parseInputFile")
})

#' @describeIn parseInputFile vector of character strings
setMethod("parseInputFile","geneanno",
    function(x,file){
        fileroot <- ifelse(identical(x@fileroot,character(0)),getwd(),x@fileroot)
        file <- file.path(fileroot,file)
        s <- read.table(file,header=FALSE)[1]
        t <- gsub("^([a-zA-Z0-9]+)\\..*","\\1",s[,1],perl=TRUE)
        t <- t[grep("^ENSG000",t,invert=TRUE)]
        return(t)
    }
)


#' Save Gene Information to group Specific Output Files
#'
#' \code{produceOutputFiles} saves the downloaded gene summary information into group specific files.
#'
#' @param x object of class geneanno.
#' @param dgl a Vector of group specific vectors relating the list of genes to the groups, as in the input file
#' @param gs Vector of Gene Summary information downloaded from NIH and Uniprot databases, typically output by getGeneSummary
#' @importFrom methods setGeneric setMethod
#' @importFrom utils read.table
#' @export
#'

setGeneric("produceOutputFiles", function(x,dgl,gs) {
  standardGeneric("produceOutputFiles")
})

#' @describeIn produceOutputFiles Save object data to text files.
setMethod("produceOutputFiles","geneanno",
          function(x,dgl,gs){
            fileroot <- ifelse(identical(x@fileroot,character(0)),getwd(),x@fileroot)
            if (!dir.exists(file.path(fileroot,x@outputstem))) {dir.create(file.path(fileroot,x@outputstem))}
            for (d in x@groupnos){
              filename = file.path(fileroot,x@outputstem,sprintf("%s.txt",d))
              if (file.exists(filename)){file.remove(filename)}
              for (i in dgl[[d]]){
                if (!is.null(i)){
                  write(i,file=filename,append = TRUE)
                  for (j in slotNames(gs[[i]])){
                    for (f in slot(gs[[i]],j)){
                      if(!is.na(f) && !is.null(f) && f != ""){
                        write(sprintf("\t%s: %s",toupper(j),f), file=filename,append = TRUE)
                    }
                  }
                }
                write('\n',file=filename,append = TRUE)
              }
            }
          }
        }
)


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
  return(v)
}
)
