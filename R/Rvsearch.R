
## The main wrapper around vsearch
vsearch <- function(sequences, index, ..., type=c("single", "paired", "crossbow"), outfile,
                   force=FALSE, strict=TRUE)
{
    type <- match.arg(type)
    args <- list(...)
    args <- args[setdiff(names(args), c("1", "2", "12"))]
    seqIn <- !is.null(args[["c"]]) && args[["c"]]
    seqArg <- ""
    if(strict)
    {
        seqArg <- switch(type,
                         single={
                             if(!is.character(sequences) || (!seqIn && !all(file.exists(sequences))))
                                 stop("Argument 'sequences' has to be a character vector of filenames ",
                                      "to align against the vsearch index or a character of read ",
                                      "sequences if the additional argument c==TRUE.")
                             paste(shQuote(sequences), collapse=",")
                         },
                         paired={
                         if(!is.list(sequences) || length(sequences)!=2)
                             stop("Argument 'sequences' must be a list of length 2.")
                         tmp <- NULL
                         for(i in 1:2)
                         {
                             if(!is.character(sequences[[i]]) || (!seqIn && !all(file.exists(sequences[[i]]))))
                                 stop("Argument 'sequences[[", i, "]]' has to be a character vector of filenames ",
                                      "to align against the vsearch index or a character of read ",
                                      "sequences if the additional argument c==TRUE.")
                             tmp <- paste(tmp,  "-", i, " ", paste(shQuote(sequences[[i]]), collapse=","), " ", sep="")
                         }
                         tmp
                     },
                     crossbow={
                         if(!is.character(sequences) || (!seqIn && !all(file.exists(sequences))))
                                 stop("Argument 'sequences' has to be a character vector of filenames ",
                                      "to align against the vsearch index or a character of read ",
                                      "sequences if the additional argument c==TRUE.")
                         paste("-12 ", paste(shQuote(sequences), collapse=","))
           })
    
        if(!is.character(index) || !file.exists(dirname(index)))
            stop("Argument 'index' has to be a character scalar giving the path to the index directory.")
    }
    outfile <- if(!missing(outfile))
    {
        if(strict && (!is.character(outfile) || length(outfile)!=1))
            stop("Argument 'outfile' must be a character scalar giving the output ",
                 "file name to store the vsearch alignments in.")
        if(strict && (file.exists(outfile) && !force))
            stop("File '", outfile, "' exists. Use 'force=TRUE' to overwrite.")
        sprintf(" %s", shQuote(outfile))
    } else ""
   
    
    args <- sprintf("%s %s %s %s", .createFlags(args), shQuote(index), seqArg, outfile)
    return(invisible(.vsearchBin("vsearch", args)))
}

## Little helpers that return a description of the intended usage for vsearch and vsearch-build
vsearch_usage <- function()
    print(vsearch("dummy", "dummy", force=TRUE, usage=TRUE, strict=FALSE))

vsearch_version <- function(){
    print(.vsearchBin(bin="vsearch", args="--version"))
}



## A helper function to create a scalar of command line arguments from a named list.
## Logical list entries are being interpreted as flags, all other entries are being
## collapsed into the form '<entryName>=<entryValue>'. Vectors of non-logical entry
## values will be collapsed into a single comma-separated scalar.
.createFlags <- function(flagList)
{
    if(!length(flagList))
        return("")
    if(is.null(names(flagList)) || any(names(flagList)==""))
        stop("Unable to create command line arguments from input.")
    logFlags <- sapply(flagList, is.logical)
    flags <- NULL
    if(any(logFlags))
    {
        fnames <- names(flagList)[logFlags][sapply(flagList[logFlags], function(x) x[1])]
        flags <- paste(sapply(fnames, function(x) ifelse(nchar(x)==1, sprintf("-%s", x), sprintf("--%s", x))),
                       collapse=" ")
    }
    fnames <- sapply(names(flagList)[!logFlags], function(x) ifelse(nchar(x)==1, sprintf("-%s", x),
                                                                    sprintf("--%s", x)))
    flags <- paste(flags, paste(fnames, sapply(flagList[!logFlags], paste, collapse=","),
                                collapse=" ", sep=" "), collapse=" ")
    return(gsub("^ *| *$", "", flags))
}


## A helper function to call one of the two vsearch binaries with additional arguments.
.vsearchBin <- function(args="")
{
    if(is.null(args) || args=="")
        stop("The vsearch binaries need to be called with additional arguments")
    args <- gsub("^ *| *$", "", args)
    call <- paste(shQuote(file.path(system.file(package="Rvsearch"), "vsearch")), args)
    #return(call)
    output <- system(call, intern=TRUE)
    return(output)
}

## The direct binary call function
.execute <- function(callstr, ...){
  call <- file.path(shQuote(system.file(package="Rvsearch")), callstr)
  return(system(call, ...))
}

