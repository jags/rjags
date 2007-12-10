print.mcarray <- function(x, ...)
{
    summary(x, mean)
}
   
summary.mcarray <- function(x, FUN, ...)
{
    dn <- names(dimnames(x))
    drop.dims <- dn %in% c("iteration","chain")
    y <- apply(x, which(!drop.dims), FUN, ...)
    cat("mcarray:\n")
    print(y)
    if (any(drop.dims)) {
        cat("\nMarginalizing over:", 
            paste(dn[drop.dims], "(", dim(x)[drop.dims],")\n" , sep=""))
    }
    invisible(y)
}
   

