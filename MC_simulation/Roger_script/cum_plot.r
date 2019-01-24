cum.plot<-function(nvec, quants=c(0.025, 0.05, seq(0.1, 0.5, by=0.1), 0.75, 0.9, 0.95)){
	par(mar=c(4.5, 4.5, 0.5, 0.2))
	nvec=nvec[!is.na(nvec)]
	plot(sort(nvec), (1:length(nvec))/length(nvec), type="l", xaxs="i", yaxs="i", las=1, ylim=c(0,1),
		xlab="value", ylab="cumulative probability")
	quants=quantile(nvec, prob=quants)
	#browser()
	segments(x0=min(nvec), x1=quants, y0=as.numeric(gsub(names(quants), pattern="%", replacement="", fixed=T))/100, y1=as.numeric(gsub(names(quants), pattern="%", replacement="", fixed=T))/100, lty=3)
	segments(x0=quants, x1=quants, y0=0, y1=as.numeric(gsub(names(quants), pattern="%", replacement="", fixed=T))/100, lty=3)
	text(labels=names(quants), x=quants, y=apply(cbind(0, as.numeric(gsub(names(quants), pattern="%", replacement="", fixed=T))/100), 1, mean)*1.2, 
		srt=-90, pos=4)
	axis(side=1, at=1:max(nvec), tcl=-0.25, labels=F)
}
