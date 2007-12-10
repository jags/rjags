print.mcarray <- function(x, ...)
{
    print(summary(x, mean))
}
   
summary.mcarray <- function(x, FUN, ...)
{
    dn <- dimnames(x)
    drop.dims <- names(dn) %in% c("iteration","chain")
    y <- apply(x, which(!drop.dims), FUN)
    cat("mcarray: FUN=", deparse(FUN))
    print(y)
    if (any(drop.dims)) {
        cat("Marginalizing over:", 
            paste(dn[drop.dims], "(", dim(x)[drop.dims],")\n" , sep=""))
    }
}
   

