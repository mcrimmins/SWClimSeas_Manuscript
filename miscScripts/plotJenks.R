plotJenks <- function(data, n=3, brks.cex=0.70, top.margin=10, dist=5){ 
  df <- data.frame(sorted.values=sort(data, decreasing=TRUE))
  Jclassif <- classIntervals(df$sorted.values, n, style = "jenks") #requires the 'classInt' package
  test <- jenks.tests(Jclassif) #requires the 'classInt' package
  df$class <- cut(df$sorted.values, unique(Jclassif$brks), labels=FALSE, include.lowest=TRUE) #the function unique() is used to remove non-unique breaks, should the latter be produced. This is done because the cut() function cannot break the values into classes if non-unique breaks are provided
  if(length(Jclassif$brks)!=length(unique(Jclassif$brks))){
    info <- ("The method has produced non-unique breaks, which have been removed. Please, check '...$classif$brks'")
  } else {info <- ("The method did not produce non-unique breaks.")}
  loop.res <- numeric(nrow(df))
  i <- 1
  repeat{
    i <- i+1
    loop.class <- classIntervals(df$sorted.values, i, style = "jenks")
    loop.test <- jenks.tests(loop.class)
    loop.res[i] <- loop.test[[2]]
    if(loop.res[i]>0.9999){
      break
    }
  }
  max.GoF.brks <- which.max(loop.res)
  plot(x=df$sorted.values, y=c(1:nrow(df)), type="b", main=paste0("Jenks natural breaks optimization; number of classes: ", n), sub=paste0("Goodness of Fit: ", round(test[[2]],4), ". Max GoF (", round(max(loop.res),4), ") with classes:", max.GoF.brks), ylim =c(0, nrow(df)+top.margin), cex=0.75, cex.main=0.95, cex.sub=0.7, ylab="observation index", xlab="value (increasing order)")
  abline(v=Jclassif$brks, lty=3, col="red")
  text(x=Jclassif$brks, y= max(nrow(df)) + dist, labels=sort(round(Jclassif$brks, 2)), cex=brks.cex, srt=90)
  results <- list("info"=info, "classif" = Jclassif, "breaks.max.GoF"=max.GoF.brks, "class.data" = df)
  return(results)
}