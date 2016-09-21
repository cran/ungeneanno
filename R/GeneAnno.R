library(httr)
library(XML)
library(methods)

source("R/GeneAnnoClasses.R")

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
#' @param pub Matrix containing journal article information from the PubMed resource, as retruned by the \code{searchPublications} function
#' @importFrom methods setGeneric setMethod
#' @importFrom utils read.table
#' @export
#'

setGeneric("produceOutputFiles", function(x,dgl,gs,pub) {
  standardGeneric("produceOutputFiles")
})

#' @describeIn produceOutputFiles Save object data to text files.
setMethod("produceOutputFiles",signature(x="geneanno",dgl="vector",gs="vector",pub="missing"),
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

#' @describeIn produceOutputFiles Save object data, including journal articles, to text files.
setMethod("produceOutputFiles",signature(x="geneanno",dgl="vector",gs="vector",pub="vector"),
          function(x,dgl,gs,pub){
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
                  for (k in pub[which(pub == paste(c(d,i),collapse=" ") ),2] ){
                    if (length(k) > 0){
                      write("\n\tJOURNAL ARTICLES:", file=filename,append = TRUE)
                      for (l in k){
                        vol <- ifelse((!is.na(l@Issue) && !is.null(l@Issue) && l@Issue != ""),sprintf("%s(%s)",l@Volume,l@Issue),l@Volume)
                        write(sprintf("\t%s\n\t%s (%s). %s. %s; %s.\n\tPMID:%s. DOI:%s\n",l@Title,l@Authors,strsplit(l@Date," ")[[1]][1],l@Journal,vol,l@Pages,l@Id,l@DOI), file=filename,append = TRUE)
                      }
                    }
                  }
                  write('\n',file=filename,append = TRUE)
                }
              }
            }
          }
)


