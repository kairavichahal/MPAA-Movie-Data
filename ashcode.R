
##########
####Code for 1-Dim Average Shifted Histogram
####2/7/13
####Original algorithm by D. Scott, Rice Univ.

####Arguments:  
####x - the 1-dim data (vector)
####n.shifts - the number of histograms you want to create; a higher m gives a final histogram with smaller bins
####n.bins - the number of bins the histograms use; a higher nbins is less smooth
####plot.hist - whether or not you want the individual histograms plotted; default set to FALSE
####x.lab - the label for the x-axis; default set to "x"

####Return Value:
####ash.hist - the vector of counts for each of the bins in the average shifted histogram

ashcode1D<-function(x, n.shifts, n.bins=2*IQR(x)*length(x)^(-1/3), plot.hist=FALSE, x.lab="x", color=4){
  #data(presidents); x=c(presidents[!is.na(presidents)]); n.shifts=10; n.bins=10; plot.hist=TRUE; x.lab="presidents"   #Test values

  n.bins <- round(n.bins)
  if (plot.hist) {gr.par<-ceiling(sqrt(n.shifts+1)); par(mfrow=c(gr.par,gr.par))} #else par(mfrow=c(1,1))
  
  
###Creating the First (Original) Histogram	
  x<-sort(x)
  h<-(max(x)-(min(x)-.001))/n.bins  #h: bin width.
  x.range<-c(min(x)-(h+0.001),max(x)+h)
  hist.orig<-hist(x, seq(x.range[1],x.range[2],by=h), col=2,xlab=x.lab,main="Original Histogram",plot=plot.hist)
  orig.counts<-hist.orig$counts

###Creating the Shifted Histograms
  count.matrix<-rep(0,length(orig.counts)*n.shifts)
  for(i in 1:(length(orig.counts)-1)) count.matrix[((i-1)*n.shifts+1):(i*n.shifts)]<-orig.counts[i]

  for(j in 1:(n.shifts-1)){
    shift<-j
    temp<-hist(x,seq(x.range[1]+shift*(h/n.shifts),x.range[2]+shift*(h/n.shifts),by=h),col=2,xlab=x.lab,main=paste("Histogram: Shift ",shift),plot=plot.hist)
    counts<-rep(0,length(orig.counts)*n.shifts)
    for(i in 1:(length(temp$counts)-1)) counts[((i-1)*n.shifts+1+shift):(i*n.shifts +shift)]<-temp$counts[i]
    count.matrix<-rbind(count.matrix,counts)
  }
  
###Creating the Final ASH
  ash.hist <- apply(count.matrix,2,mean)
  bp <- barplot(ash.hist,
                xaxt="n", ylab="Frequency",
                main=paste("Average Shifted Histogram\n n.shifts = ", n.shifts, ", Orig. Bins = ", n.bins), xlab=x.lab, col=color)
  
  lab <- round(seq(hist.orig$breaks[1],hist.orig$breaks[length(orig.counts)],length=6), 0)
  axis(1,at=seq(bp[1],bp[length(bp)],length=length(lab)),labels=lab)
  
  return(list(ash.hist=ash.hist))
}
