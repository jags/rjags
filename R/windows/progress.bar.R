updatePB <- function(start.iter, n.iter, adapting)
{
    winProgressBar(title=ifelse(adapting, "Adapting","Updating"),
                  label="Iteration 0", min = start.iter, max=start.iter + n.iter)
}

setPB <- function(pb, iter)
{
    setWinProgressBar(pb, iter, label=paste("Iteration",iter))
}
