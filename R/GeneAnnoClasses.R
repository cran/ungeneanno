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
        gene="character",
        col="character"
    ),
    prototype=list(
      col="id,entry name,reviewed,protein names,genes,organism,length,comment(FUNCTION)"
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
