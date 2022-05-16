#	summary() for `partynat' objects
#	Juraj Medzihorsky
#	2022-05-16

setMethod('summary',
          'partynat',
	function(object, d=2, ...)
	{	
        x <- object
		c_n <- c('Estimate', 'Std.Error',
				 sprintf('%.2f Low.', x$confidence_level),
				 sprintf('%.2f Upp.', x$confidence_level)) 
		x1 <- x$total
		x2 <- x$choices
		colnames(x1) <- colnames(x2) <- c_n
		rownames(x1) <- 'Table'
		if (sum(is.na(x1[, 2:4]))==3) {
			x1 <- data.frame('Value'=x1[, 1])
			x2 <- data.frame('Value'=x2[, 1])
			rownames(x1) <- 'Table'
			rownames(x2) <- rownames(x$choices)
		}
		cat('Statistic:', '\n\n')
		cat(x$name, '\n\n')
		cat('Total:\n')
		print(round(x1, d))
		cat('\nChoices:\n')
		print(round(x2, d))
		cat('\n\n')
	}
)    

#	END
