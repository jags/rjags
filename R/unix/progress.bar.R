updatePB <- function(start.iter, n.iter, adapting)
{
    tkProgressBar(title=ifelse(adapting, "Adapting","Updating"),
                  label="Iteration 0", min = start.iter, max=start.iter + n.iter)
}

setPB <- function(pb, iter)
{
    setTkProgressBar(pb, iter, label=paste("Iteration",iter))
}
