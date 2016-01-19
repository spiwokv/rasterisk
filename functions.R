sem<-function(x) {
  return(sd(x)/sqrt(length(x)))
}

noeb<-function(x) {
  return(rep(0,length(x)))
}

barplot.errorbars<-function(formula, data, eb=sd, lwd=1,
                           col=NULL, main=NULL, sub=NULL, ylab=NULL,
                           ylim=NULL, axes=TRUE, axisnames=TRUE, 
                           cex.axis=par("cex.axis"), cex.names=par("cex.axis"),
                           code=3, updown=1) {
  par(lwd=lwd)
  nlev<-nlevels(factor(df[[as.character(formula[3])]]))
  means <- c()
  stds <- c()
  for(i in 1:nlev) {
    isgoodrow<-df[,as.character(formula[3])]==levels(factor(df[[as.character(formula[3])]]))[i]
    means<-c(means, mean(df[isgoodrow,as.character(formula[2])]))
    stds<-c(stds, eb(df[isgoodrow,as.character(formula[2])]))
  }
  barplot(means, names.arg=levels(factor(df[[as.character(formula[3])]])),
          main=main, sub=sub, ylab=ylab, col=col, lwd=lwd,
          ylim=ylim, axes=axes, axisnames=axisnames,
          cex.axis=cex.axis, cex.names=cex.names)
  if(updown==1) {
    for(i in 1:nlev) {
      arrows(1.2*i-0.5, means[i]-stds[i], 1.2*i-0.5, means[i]+stds[i],
             angle=90, code=code, length=0.5/nlev, lwd=lwd)
    }
  }
  if(updown==2) {
    for(i in 1:nlev) {
      arrows(1.2*i-0.5, means[i], 1.2*i-0.5, means[i]+stds[i],
             angle=90, code=code, length=0.5/nlev, lwd=lwd)
    }
  }
  if(updown==3) {
    for(i in 1:nlev) {
      arrows(1.2*i-0.5, means[i]-stds[i], 1.2*i-0.5, means[i],
             angle=90, code=code, length=0.5/nlev, lwd=lwd)
    }
  }
}

barplot.asterisk2<-function(formula, data, set1=c(1), set2=c(2), method, eb=sd,
                           lwd=1, col=NULL, main=NULL, sub=NULL, ylab=NULL,
                           ylim=NULL, axes=TRUE, axisnames=TRUE, 
                           cex.axis=par("cex.axis"), cex.names=par("cex.axis"),
                           code=3, updown=1) {
  par(lwd=lwd)
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
  barplot(means, names.arg=levels(factor(df[[as.character(formula[3])]])),
          main=main, sub=sub, ylab=ylab, col=col,
          ylim=ylim, axes=axes, axisnames=axisnames,
          cex.axis=cex.axis, cex.names=cex.names, lwd=lwd)
  if(updown==1) {
    for(i in 1:nlev) {
      arrows(1.2*i-0.5, means[i]-stds[i], 1.2*i-0.5, means[i]+stds[i],
             angle=90, code=code, length=0.5/nlev, lwd=lwd)
    }
  }
  if(updown==2) {
    for(i in 1:nlev) {
      arrows(1.2*i-0.5, means[i], 1.2*i-0.5, means[i]+stds[i],
             angle=90, code=code, length=0.5/nlev, lwd=lwd)
    }
  }
  if(updown==3) {
    for(i in 1:nlev) {
      arrows(1.2*i-0.5, means[i]-stds[i], 1.2*i-0.5, means[i],
      angle=90, code=code, length=0.5/nlev, lwd=lwd)
    }
  }
}
