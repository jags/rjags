print.jags <- function(x, ...)
{
  model <- x$model()
  for (i in 1:length(model)) {
    cat(model[i],"\n",sep="")
  }
}

jags.model <- function(file, data, inits, nchain = 1)
{

  if (missing(file)) {
    stop("Model file name missing")
  }
  p <- .Call("make_console", PACKAGE="rjags") 
  .Call("check_model", p, file, PACKAGE="rjags")
  .Call("compile", p, data, as.integer(nchain), TRUE, PACKAGE="rjags")


  setParameters <- function(initsi, chain) {
    if (!is.list(initsi))
      stop("Parameters must be a list")
    if (is.null(names(initsi)) || any(nchar(names(initsi)) == 0))
      stop("Parameters must be a named list")
    if (!is.null(initsi[[".RNG.name"]])) {
      .Call("set_rng_name", p, initsi[[".RNG.name"]], PACKAGE="rjags")
      inits[[".RNG.name"]] <- NULL
    }
    .Call("set_parameters", p, initsi, as.integer(chain), PACKAGE="rjags")
  }

  if (!missing(inits)) {
    if (!is.list(inits)) {
      stop("Initial values must be a list")
    }
    if (length(inits) != nchain) {
      stop("inits list must be the same length as the number of chains")
    }

    for (i in 1:nchain) {
      setParameters(inits[[i]], i)
    }
  }

  .Call("initialize", p, PACKAGE="rjags")

  model.state <- .Call("get_state", p, PACKAGE="rjags")
  model.data <- .Call("get_data", p, PACKAGE="rjags")
  model.code <- readLines(file)
  model <- list("ptr" = function() {p},
                "data" = function() {model.data},
                "model" = function() {model.code},
                "state" = function(internal=FALSE)
                {
                  if(!internal) {
                    for(i in 1:nchain) {
                      model.state[[i]][[".RNG.state"]] <- NULL
                      model.state[[i]][[".RNG.name"]] <- NULL
                      model.state[[i]][[".Iteration"]] <- NULL
                    }
                  }
                  return(model.state)
                },
                "update" = function(niter, by) {
                  .Call("update", p, niter, PACKAGE="rjags")
                  model.state <<- .Call("get_state", p, PACKAGE="rjags")
                  invisible(NULL)
                },
                "recompile" = function() {
                  ## Clear the console
                  .Call("clear_console", p, PACKAGE="rjags")
                  p <<- .Call("make_console", PACKAGE="rjags")
                  ## Write the model to a temporary file so we can re-read it
                  mf <- tempfile()
                  writeLines(model.code, mf)
                  .Call("check_model", p, mf, PACKAGE="rjags")
                  unlink(mf)
                  ## Re-compile
                  .Call("compile", p, data, nchain, FALSE, PACKAGE="rjags")
                  ## Re-initialize
                  if (!is.null(model.state)) {
                    if (length(model.state) != nchain) {
                      error("Incorrect number of chains in saved state")
                    }
                    for (i in 1:nchain) {
                      statei <- model.state[[i]]
                      rng <- statei[[".RNG.name"]]
                      if (!is.null(rng)) {
                        .Call("set_rng_name", p, rng, i, PACKAGE="rjags")
                        statei[[".RNG.name"]] <- NULL
                      }
                      .Call("set_parameters", p, statei, i, PACKAGE="rjags")
                    }
                    .Call("initialize", p, PACKAGE="rjags")
                  }
                  invisible(NULL)
                })
  class(model) <- "jags"
  return(model)
}

read.jagsdata <- function(file)
{
  e <- new.env()
  eval(parse(file), e)
  return(as.list(e))
}

model.samples <- function(model, variable.names, n.iter, thin=1)
{
  if (class(model) != "jags")
    stop("Invalid JAGS model")

  if (!is.character(variable.names))
    stop("variable.names must be a character vector")
  if (length(variable.names) == 0)
    stop("Empty variable name list")
  
  if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
    stop("n.iter must be a positive integer")

  for (i in seq(along=variable.names)) {
    .Call("set_monitor", model$ptr(), variable.names[i], thin, PACKAGE="rjags")
  }
  model$update(as.integer(n.iter))
  nchain <- length(model$state())
  if (nchain == 1) {
    ans <- .Call("get_monitored_values", model$ptr(), 1, PACKAGE="rjags")
  }
  else {
    ans <- vector("list", nchain)
    for (i in 1:length(ans)) {
      ans[[i]] <- .Call("get_monitored_values", model$ptr(), i, PACKAGE="rjags")
    }
  }
  for (i in seq(along=variable.names)) {
    .Call("clear_monitor", model$ptr(), variable.names[i], PACKAGE="rjags")
  }
  return(ans)
}
