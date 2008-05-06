ploss.samples <-
  function(model, n.iter, thin=1, type)
{
    if (nchain(model) == 1) {
        stop("Estimation of the penalty requires 2 or more parallel chains")
    }
    jags.module("dic")
    
    if (class(model) != "jags")
      stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")

    type=match.arg(type, c("pD","popt"))

    .Call("set_default_monitors", model$ptr(), as.integer(thin),
          "deviance", PACKAGE="rjags")
    .Call("set_default_monitors", model$ptr(), as.integer(thin),
          type, PACKAGE="rjags")

    model$update(as.integer(n.iter))
    dev <- .Call("get_monitored_values", model$ptr(), "deviance",
                 PACKAGE="rjags")
    for (i in seq(along=dev)) {
        class(dev[[i]]) <- "mcarray"
    }
    pen <- .Call("get_monitored_values", model$ptr(), type,
                 PACKAGE="rjags")
    for (i in seq(along=pen)) {
        class(pen[[i]]) <- "mcarray"
    }
    
    .Call("clear_default_monitors", model$ptr(), "deviance", PACKAGE="rjags")
    .Call("clear_default_monitors", model$ptr(), type, PACKAGE="rjags")

    ans <-  list(deviance = dev, penalty=pen, type=type)
    class(ans) <- "ploss"
}
