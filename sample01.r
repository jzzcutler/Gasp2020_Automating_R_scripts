# This is a sample script that can be called normally from the RStudio or R GUIs
# and can also be called from the command line as well.  Being able to call from
# the command line allows you to schedule the runs of your R scripts.

# To add this functionality to your own script 
#   1) Make sure all your code is inside functions (so that it doesn't run when
#   your code is sourced).
#   2) Copy this setup function to your own script and place a call to it at the
#   BOTTOM of your script so that setup is run after all your code has been
#   sourced.

Setup <- function() {
  #Make sure this function is not somewhere up the call stack which would result
  #in an infinite loop
  aa <- 1
  nThisFnActive <- '.MasterSetup.'
  a <- any(sapply(
    1:(sys.nframe() + 2), #+ for any(sapply()) 
    function(x) {exists(nThisFnActive, envir = sys.frame(-x))}
  ))
  if (a) {
    return()
  }
  assign(nThisFnActive, TRUE)

  #=========================================================================
  # Start of functions stolen from the project Setup() was stolen from
  #=========================================================================
  #cat() with a \n appended
  catx <- function(...) {
    cat(...)
    cat('\n')
  }
  
  #Lazy way to print a Name-And-Value
  nav <- function(x) {
    a <- as.character(sys.call()[2])
    b <- paste(x, collapse = '^')
    paste0(a, '=', b)
  }
  
  #normalizePath with a default winslash
  normalizePathx <- function(...) {
    normalizePath(..., winslash = "/", mustWork = F)
  }
  
  #Combine any number of pieces into a folder or file path - which is more than
  #file.path() can do.  If the last piece starts with a . then assume it is an
  #extension and the next to last piece is a file name.
  PathCombine <- function(...) {
    input <- list(...)
    
    #Make sure only the last piece starts with . - indicating it is an extension
    #and we're building a file path instead of a folder path.
    a <- grepl('^\\.', input)
    if (any(a) && (which.max(a) < length(a))) {
      stop('A piece other than the last one starts with . in the input data')
    }
    
    #Put / between all the pieces
    a <- normalizePathx(paste(input, collapse = '/'))
    
    #If there was a . at the front of the last piece then paste will have
    #created file-name/.file-extension so we change the /. into a .
    if ('.' == substr(input[length(input)], 1, 1)) {
      a <- sub('/.', '.', a, fixed = T)
    }
    return(a)
  }
  
  #Build a path and create it if it doesn't exist
  PathCombineCreate <- function(...) {
    path <- PathCombine(...)
    
    if (grepl('\\.', path))
      stop('PathCombineCreate called for file: ', path)
    
    if (!file.exists(path))
      dir.create(path, recursive = T)
    
    return(path)
  }
  #=========================================================================
  # End of functions stolen from the project Setup() was stolen from
  #=========================================================================

  #eConst is an R environment where you can place values of interest to your
  #code.  You don't need to use it.  I use it to keep from cluttering up the
  #global environment and to make it easy to save and restore a lot of settings
  #in a single file on disk: saveRDS().  It is also very handy for saving
  #intermediate results during long runs so that the function can be restarted
  eConst <- new.env(parent = emptyenv())
  assign('eConst', eConst, envir = .GlobalEnv)
  eConst$value1 <- 'Hello'

  initial.options <- commandArgs(trailingOnly = FALSE)
  aa <- 1
  if (any(grepl('^--interactive$', initial.options))) {
    bRscript <<- FALSE
    a <- eval(parse(text = sys.call()[[1]]))
    eConst$sSourceFile <- getSrcFilename(a)
    eConst$sSourceFullPath <- getSrcFilename(a, full.names = TRUE)
    eConst$sWorkingDir <- normalizePathx(getSrcDirectory(a))
    eConst$sSourceRootR <- normalizePathx(paste0(getSrcDirectory(a), '/..'))
  } else {
    #Make sure we're running under RSCRIPT and then find the location info
    if (!any(grepl('^--slave$', initial.options))) {
      stop('"--slave" not found in initial.options')
    }
    bRscript <<- TRUE
    
    #initial.options[--file] contains the path to the script file relative to
    #the DOS current directory
    a <- initial.options[grepl('^--file=', initial.options)]
    a <- sub('^--file=', '', a)
    eConst$sSourceFullPath <- normalizePathx(a)
    eConst$sSourceFile <- sub('^(.*)/([^/]*)', '\\2', eConst$sSourceFullPath)
    eConst$sWorkingDir <- sub('^(.*)/([^/]*)', '\\1', eConst$sSourceFullPath)
    eConst$sSourceRootR <- normalizePathx(paste0(eConst$sWorkingDir, '/..'))
    
    setwd(eConst$sWorkingDir)
    setWindowTitle(suffix = eConst$sSourceFile)
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Put project specific stuff below this point.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  eConst$value2 <- 'Goodbye'

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # If we're running under RSCRIPT we run whatever command is specified by
  # --RscriptRun=
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (bRscript) {
    # Show some interesting values.
    catx(paste(paste(rep('+', 100), collapse = ''), ''))
    catx(nav(bRscript))
    catx(nav(getwd()))
    catx(paste(paste(rep('+', 100), collapse = ''), 'initial.options'))
    catx(initial.options)
    catx(paste(paste(rep('+', 100), collapse = ''), 'ls.str(eConst)'))
    print(ls.str(eConst))
    catx(paste(rep('+', 100), collapse = ''))

    #initial.options is a vector split on spaces in the calling Windows command
    #line so we have to find the first (in a) and last (in b) vector elements
    #that comprise the entire --RscriptRun= string and paste them together into
    #a single character string to pass to eval().
    firstElem <- min(grep('^--RscriptRun=', initial.options, value = F))
    if (length(firstElem) == 0) {
      stop('--RscriptRun= was not specified.')
    }
    if (length(initial.options) > firstElem) {
      lastElem <- initial.options[(firstElem + 1):length(initial.options)]
      lastElem <- grep('^--', lastElem, value = F) #-- begins a new flag
      if (length(lastElem) > 0) {
        lastElem <- firstElem + min(lastElem) - 1
      }
      else {
        lastElem <- length(initial.options)
      }
    }
    else {
      lastElem <- firstElem
    }
    sEval <- paste(initial.options[firstElem:lastElem], collapse = ' ')

    # To handle Windows command line oddities, we sometimes have to enclose the
    # entire --RscriptRun value in quotes.  We remove --RscriptRun from the
    # beginning of the sEval string and remove any quotes that enclose the
    # entire remaining string.
    sEval <- sub('^--RscriptRun=', '', sEval)
    sEval <- sub('^"(.*)"$', '\\1', sEval)
    sEval <- sub("^'(.*)'$", '\\1', sEval)
    if (length(sEval) == 0) {
      stop('--RscriptRun=XXX was not specified.')
    }
    setWindowTitle(suffix = paste(eConst$sSourceFile, '-', sEval))
    catx(paste('RSCRIPT running: going to evaluate:', sEval))
    catx(paste(paste(rep('+', 100), collapse = ''), ''))
    eval(parse(text = sEval))
  }
}

Hello <- function(x) {
  if (bRscript) { #This global is set by Setup().  TRUE if not in RStudio.
    print(paste(eConst$value1, 'beautiful', x))
  } else {
    print(paste(eConst$value1, 'wonderful', x))
  }
}


Goodbye <- function(x) {
  print(paste(eConst$value2, 'cruel', x))
}


Setup()
