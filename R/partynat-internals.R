#	partynat package internals
#	Juraj Medzihorsky
#	2015-05-23


#	This file contains internal functions of the `partynat' package.
#	Functions for indices at the beginning, auxiliary functions at the end.
#	First functions defined for individual parties, then those defined only for
#	party system.


#	==============================================
#		Indices defined for individual parties
#	==============================================


#	-------------------------------
#		PNSw from Bochsler 2010
#	-------------------------------

bochsler2010pnsw <-
	function(x, weight_choice=TRUE, weight_territory=TRUE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (!weight_territory) { stop('The index weights territories.') }
		rr <- list()
		nc <- ncol(x)
		rs <- rowSums(x)
		cd <- colSums(x)/sum(x)
		auxpnsw <-
			function(s, r)
			{
				id <- order(s/r)
				v <- r[id]
				s <- s[id]
				top <- sum(v*(cumsum(s)-s/2))
				bot <- sum(v)*sum(s)
				b <- 2*top/bot
				return(b)
			}
		choices <- sapply(1:nc, function(j) auxpnsw(s=x[,j], r=rs))
		if (weight_choice) { 
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices 
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	-------------------------------------
#		PNS10/spns from Bochsler 2010
#	-------------------------------------

bochsler2010pns10 <-
	function(x, weight_choice=TRUE, weight_territory=TRUE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (!weight_territory) { stop('The index weights territories.') }
		rr <- list()
		nc <- ncol(x)
		rs <- rowSums(x)
		cd <- colSums(x)/sum(x)
		enu <- (sum(rs)^2)/sum(rs^2)
		lge <- 1/log10(enu)
		auxpnsw <-
			function(s, r)
			{
				id <- order(s/r)
				v <- r[id]
				s <- s[id]
				top <- sum(v*(cumsum(s)-s/2))
				bot <- sum(v)*sum(s)
				b <- 2*top/bot
				return(b)
			}
		choices <- sapply(1:nc, function(j) auxpnsw(s=x[,j], r=rs))
		if (weight_choice) { 
			rr$total <- sum(cd*choices)^lge
		} else {
			rr$total <- mean(choices)^lge
		}
		rr$choices <- choices^lge 
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	--------------------------
#		Territorial Coverage Index  Caramani 2004 (61)	
#	--------------------------

caramani2004tci <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('Territory weights not available for this index.')  }
		rr <- list()
		cd <- colSums(x)/sum(x)
		no <- prod(dim(x))	#	Bochsler's d
		nc <- ncol(x)		#	Bochsler's N_OV
		PI <- x!=0			#	Bochsler's pi
		choices <- sapply(1:nc, function(j) mean(PI[,j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	----------------------------
#		Index of Party Regionalization from Golosov and Ponarin 1999	
#	----------------------------

golosovponarin1999ipr <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('The index is not territory-weighted.') }
		rr <- list()
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i)))
		auxipr <-
			function(p)
			{
				n <- length(p)
				d <- sum(abs(mean(p)-p))
				r <- sqrt((n*d)/(2*(n-1)*sum(p)))
				return(r)
			}
		choices <- sapply(1:nc, function(j) auxipr(rp[,j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}



#	---------------------------
#		Coefficient of Party Regionalization by Golosov 2014	
#	---------------------------

golosov2014cpr <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('The index is not territory-weighted.') }
		rr <- list()
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i)))
		auxcpr <-
			function(p)
			{
				n <- length(p)
				top <- n - ( (sum(p)^2) / sum(p^2) )
				bot <- n - 1
				r <- top/bot
				return(r)
			}
		choices <- sapply(1:nc, function(j) auxcpr(rp[,j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	---------------------------
#		Index of Party Nationalization by Golosov 2014	
#	---------------------------

golosov2014ipn <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('The index is not territory-weighted.') }
		rr <- list()
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i)))
		auxcpr <-
			function(p)
			{
				n <- length(p)
				top <- n - ( (sum(p)^2) / sum(p^2) )
				bot <- n - 1
				r <- 1 - top/bot
				return(r)
			}
		choices <- sapply(1:nc, function(j) auxcpr(rp[,j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	--------------------------
#		PNS as complement of gini from Jones & Mainwaring 2003
#	--------------------------

jonesmainwaring2003pns <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('The index is not territory-weighted.') }
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i)))
		rr <- list()
		#	gini for a vector
		auxgini <-
			function(x)
			{
				n <- length(x)
				p <- x[order(x)]/sum(x)	
				o <- cumsum(p)
				m <- (1:n)/n
				a <- sum(m)-sum(o) 
				return(2*a/n)
			}
		choices <- sapply(1:nc, function(j) auxgini(rp[,j])) 
		if (weight_choice) {
			rr$total <- 1-sum(cd*choices)
		} else {
			rr$total <- 1-mean(choices)
		}
		rr$choices <- 1-choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	----------------------
#		Lee index 1988	
#	----------------------

lee1988 <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('Territory weights not available for this index.')  }
		rr <- list()
		nc <- ncol(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		cd <- colSums(x)/sum(x)
		choices <- sapply(1:nc, function(j) sum(abs(rp[,j]-cd[j]))/2)
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	--------------------------
#		Mean Absolute Deviation Rose and Urwin 1975 (28)	
#	--------------------------

roseurwin1975mad <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('Territory weights not available for this index.')  }
		rr <- list()
		nc <- ncol(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		cd <- colSums(x)/sum(x)
		choices <- sapply(1:nc, function(j) mean(abs(rp[,j]-cd[j])))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	------------------------------
#		Mean Squared Deviation 	
#	------------------------------

msd <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('Territory weights not available for this index.')  }
		rr <- list()
		nc <- ncol(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		cd <- colSums(x)/sum(x)
		choices <- sapply(1:nc, function(j) mean((rp[,j]-cd[j])^2))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	----------------
#		Variance 	
#	----------------

variance <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_territory) { stop('Territory weights not available for this index.')  }
		rr <- list()
		nc <- ncol(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		cd <- colSums(x)/sum(x)
		choices <- sapply(1:nc, function(j) var(rp[,j]-cd[j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	------------------------------------
#		Variability Coefficient (Ersson et al 1985)	
#	------------------------------------

erssonetal1985vc <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x))	{ stop('The data does not meet the requirements') }
		if (weight_territory) { stop('The index does not weigh territories.') }
		rr <- list()
		nr <- nrow(x)
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		choices <- sapply(1:nc, function(j) sd(rp[,j])/mean(rp[,j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal' 
		return(rr)	
	}


#	------------------------------------
#		Standardized and Weighted Variability Coefficient (Ersson et al 1985)	
#	------------------------------------

erssonetal1985swvc <-
	function(x, weight_choice=TRUE, weight_territory=TRUE)
	{
		if (!ok.tcm(x))	{ stop('The data does not meet the requirements') }
		if (weight_territory) { stop('The index does not weigh territories.') }
		rr <- list()
		nr <- nrow(x)
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		#	Version from Bochsler 2010, 166
		choices <- sapply(1:nc, function(j) sqrt(sum((rp[,j]-cd[j])^2))/cd[j]/sqrt(nr))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal' 
		return(rr)	
	}


#	------------------------------------
#		Normalized Coefficient of Variation (Golosov 2014)		
#	------------------------------------

golosov2014ncv <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x))	{ stop('The data does not meet the requirements') }
		if (weight_territory) { stop('The index does not weigh territories.') }
		rr <- list()
		nr <- nrow(x)
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		hh <- sapply(1:nc, function(j) sum(rp[,j]^2)/(sum(rp[,j])^2))
		choices <- sapply(1:nc, function(j) sqrt( (hh[j] -(1/nr)) /(1-(1/nr))  ))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal' 
		return(rr)	
	}


#	------------------------------------
#		Index adjusted for Party size and number of Regions (Caramani 2004)		
#	------------------------------------

caramani2004ipr <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x))	{ stop('The data does not meet the requirements') }
		if (weight_territory) { stop('The index does not weigh territories.') }
		rr <- list()
		nr <- nrow(x)
		nc <- ncol(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		auxipr <-
			function(y)
			{
				top <- nr*sum(abs(y-mean(y)))
				bot <- 2*(nr-1)*sum(y)
				ipr <- sqrt(top/bot)
			}
		choices <- sapply(1:nc, function(j) auxipr(rp[,j]))
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal' 
		return(rr)	
	}


#	------------------------------------
#		Cumulative Regional Inequality (Rose and Urwin 1975)	
#	------------------------------------

roseurwin1975cri <-
	function(x, weight_choice=TRUE, weight_territory=FALSE)
	{
		if (!ok.tcm(x))	{ stop('The data does not meet the requirements') }
		if (weight_territory) { stop('The index does not weigh territories.') }
		rr <- list()
		nr <- nrow(x)
		nc <- ncol(x)
		rd <- rowSums(x)/sum(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i) ))
		cp <- apply(x, 2, function(j) j/sum(j))
		auxcri <-
			function(j)
			{
				sum(abs(rp[,j] - cp[,j]))/2e2
			}
		choices <- sapply(1:nc, auxcri)
		if (weight_choice) {
			rr$total <- sum(cd*choices)
		} else {
			rr$total <- mean(choices)
		}
		rr$choices <- choices
		class(rr) <- 'partynat_internal' 
		return(rr)	
	}


#	=========================================================
#		Indices defined only for systems (inflation/ENPP)
#	=========================================================


#	-------------------------------------
#		Indicator of Party Aggregation (Chibber Kollman 1998)	
#	-------------------------------------

chibberkollman1998ipa <-
	function(x, weight_choice=FALSE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_choice) { stop('The index does not weight choicess.') }
		if (weight_territory) { stop('The index does not weight territories.') }
		rr <- list()
		nr <- nrow(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i))) 
		et <- 1/sum(cd^2)
		ed <- apply(rp, 1, function(i) 1/sum(i^2))
		rr$total <- et - mean(ed)
		rr$choices <- as.numeric(rep(NA, ncol(x))) 
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	----------------------------------
#		Inflation Score (Cox 1999)	
#	----------------------------------

cox1999is <-
	function(x, weight_choice=FALSE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_choice) { stop('The index does not weight choicess.') }
		if (weight_territory) { stop('The index does not weight territories.') }
		rr <- list()
		nr <- nrow(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i))) 
		et <- 1/sum(cd^2)
		ed <- apply(rp, 1, function(i) 1/sum(i^2))
		rr$total <- 1e2*(et - mean(ed))/et
		rr$choices <- as.numeric(rep(NA, ncol(x))) 
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	------------------------------
#		Index of Party Aggregation	(Allik 2006)	
#	------------------------------

allik2006ipa <-
	function(x, weight_choice=FALSE, weight_territory=FALSE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_choice) { stop('The index does not weight choicess.') }
		if (weight_territory) { stop('The index does not weight territories.') }
		rr <- list()
		nr <- nrow(x)
		cd <- colSums(x)/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i))) 
		et <- 1/sum(cd^2)
		ed <- apply(rp, 1, function(i) 1/sum(i^2))
		rr$total <- 1 - (et - mean(ed))/et
		rr$choices <- as.numeric(rep(NA, ncol(x))) 
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	-----------------------------
#		Inflation Index (Moenius and Kasuya 2004)
#	-----------------------------

moeniuskasuya2004ii <-
	function(x, weight_choice=FALSE, weight_territory=TRUE)
	{
		if (!ok.tcm(x)) { stop('The supplied data does not meet the requirements.') }
		if (weight_choice) { stop('The index does not weight choicess.') }
		rr <- list()
		nr <- nrow(x)
		rs <- rowSums(x)
		cd <- colSums(x)/sum(x)
		rd <- rs/sum(x)
		rp <- t(apply(x, 1, function(i) i/sum(i))) 
		et <- 1/sum(cd^2)
		ed <- apply(rp, 1, function(i) 1/sum(i^2))
		if (!weight_territory) {
			rr$total <- 1e2*(et - mean(ed))/mean(ed)
		} else if (weight_territory) {
			#	rr$total <- 1e2*((sum(x)*et) / (sum(ed*rs)) - 1)
			rr$total <- 1e2*((et - sum(ed*rd))/sum(ed*rd) )
		}
		rr$choices <- as.numeric(rep(NA, ncol(x))) 
		class(rr) <- 'partynat_internal'
		return(rr)	
	}


#	===========================
#		Auxiliary functions	
#	===========================


#	--------------------------------------
#		Function for checking the data 	
#	--------------------------------------

ok.tcm <-
	function(x)
	{
		x <- as.matrix(x)
		###	STOPS
		#	x must have 2 dimensions
		if (length(dim(x))!=2) 
		{
			stop('TerritoryChoiceMatrix must have 2 dimensions.')
		}
		#	x must be free of NaN
		if (sum(is.nan(x))>0)
		{
			stop('NaN found in ', sum(is.nan(x)) , ' cell(s).')
		}
		#	x must be discrete
		if (sum(x[!is.na(x)]!=round(x[!is.na(x)]))>0)
		{
			stop('TerritoryChoiceMatrix must contain counts.')
		}
		###	WARNINGS
		#	x must have rownames
		if (is.null(rownames(x)))
		{
			warning('rownames missing.')
		}
		#	x must have rownames
		if (is.null(colnames(x)))
		{
			warning('colnames missing.')
		}
		#	count NAs
		if (sum(is.na(x))>0)
		{
			warning('NA found in ', sum(is.na(x)), ' cell(s).')
		}
		return(TRUE)
	}


#	-------------------------------------------------
#		Function for pooling jackknifed estimates
#	-------------------------------------------------


pool.jack <-
	function(data, 
			 ct 	= TRUE, 
			 estimate, 
			 jack_est,
			 side 	= NULL,
			 conf 	= 0.95,
			 lower 	= -Inf,
			 upper 	= Inf,
			 bias 	= FALSE)
{
	p <- 1 - (1-conf)/2
	e <- estimate
	j <- jack_est			
	
	n <- ifelse(ct, sum(data), length(data))	#	No. cases
	#	Weight
	if (ct) {
		w <- as.vector(data)	
		if ( (length(w)-length(j)) == sum(w==0) ) {
			w <- w[w!=0]						#	assumes no js for empty cells
		}
	} else {
		w <- rep(1, n)
	}
	#
	m <- ifelse(ct, sum(j*w)/n, mean(j))		#	Weighted mean jack value


	if (bias) {
		se <- sqrt( (n-1)/n * sum( w * (j-m)^2 ) )
		bias <- (n-1)*(m-e)						# 	bias
		theta_bc <- n*e - (n-1)*m				#	bias corrected stat
		theta <- theta_bc
	} else {
		se <- sqrt( (n-1)/n * sum( w * (j-e)^2 ) )
		bias <- as.numeric(NA)
		theta <- e		
	}
		
	side <- tolower(side)
			
	if (length(grep('au', side))==1) {
		side <- ifelse(theta > 0, 'lower', 'upper')
	}
		
	
	if (length(grep('lo', side))==1) {
		
		side <- 'lower'
		low <- max(theta - qnorm(p)*se, lower)
	   	upp <- upper
		
	} else if (length(grep('up', side))==1) {
		
		side <- 'upper'
		low <- lower 
		upp <- min(theta + qnorm(p)*se, upper)
		
	} else if (length(side)==0) {							#	!!!!!!!!! 
		
		side <- 'both'
		low <- max(theta - qnorm(1-(1-conf)/2)*se, lower)
		upp <- min(theta + qnorm(1-(1-conf)/2)*se, upper)
		
	} else {
		
		stop('Unsupported confidence interval type.\n')
		
	}
		
	
	out <- list(se = se, 
				low = low,
				upp = upp,				
				theta = theta,
				conf = conf,
				side = side,
				bias = bias)
	
	return(out)
}

#	END
