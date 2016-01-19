barplot.asterisk<-function(formula, data, set1=c(1), set2=c(2), method, eb=sd) {
  nlev<-nlevels(factor(df[[as.character(formula[3])]]))
  if(length(set1)!=length(set2)) {
    stop("Sequences 'set1' and 'set2' must have same lengths")
  }
  for(i in 1:length(set1)) {
    if(set1[i]%%1!=0) {
      stop("Only integers could be in set1")
    }
    if(set2[i]%%1!=0) {
      stop("Only integers could be in set2")
    }
    if(set1[i]==set2[i]) {
      stop("Values of set1 and set2 must differ")
    }
    if((set1[i]<1) | (set1[i]>nlev)) {
      stop("The value of set1 must be from 1 to number of levels")
    }
    if((set2[i]<1) | (set2[i]>nlev)) {
      stop("The value of set2 must be from 1 to number of levels")
    }
  }
  means <- c()
  stds <- c()
  for(i in 1:nlev) {
    isgoodrow<-df[,as.character(formula[3])]==levels(factor(df[[as.character(formula[3])]]))[i]
    means<-c(means, mean(df[isgoodrow,as.character(formula[2])]))
    stds<-c(stds, eb(df[isgoodrow,as.character(formula[2])]))
  }
  barplot(means)
}

