#	plot() for `partynat'
#	Juraj Medzihorsky
#	2014-12-10

plot.partynat <-
	function(x, ...)
	{
		v <- rbind(x$total, x$choices)
		v$col <- c('red', rep('blue', nrow(x$choices))) 
		v$pch <- c(18, rep(20, nrow(x$choices)))
		v$lab <- c('Table', rownames(x$choices))
		v <- v[order(v[,1]), ]
		v$ycoord <- 1:nrow(v)
		imax <- max(v[,1])
		imin <- min(v[,1])
		if ( (imin>=0) & (imax<=1))
		{
			xl <- c(0, 1)
		} else {
			xl <- range(v[,1])
		}
		plot(0, 0, type='n', xlim=xl, ylim=range(v$ycoord), xlab='', yaxt='n', ylab='', ...)
		axis(2, at=1:nrow(v), v$lab, las=1)
		for (i in 1:nrow(v)) {
		  	lines(xl, rep(i, 2), col='grey', lty=3) 
			points(v[i,1], i, pch=v$pch[i], col=v$col[i])
			if (!is.na(v[i,2])) {
				lines(v[i,3:4], rep(i,2), col=v$col[i])
			}
	   	}
	}

#	END
