#	partynat function for partynat
#	Juraj Medzihorsky
#	2022-05-16


partynat <-
	function(mat,
			 statistic='PNS',
			 weight_choice=TRUE,
			 weight_territory=TRUE,
			 boot=FALSE,
			 jack=FALSE,
			 subsample=FALSE,
			 n_rep=1e1,
			 bias=TRUE,
			 size=round(sum(mat)/2),
			 confidence_level=0.95)
	{
		out <- list()
		out$call <- match.call()
		out$stat <- statistic 

		if (is.character(statistic)) {
			stat <- tolower(statistic)[1]
			if (stat=='pnsw') {
				auxfun <- bochsler2010pnsw
				out$name <- 'Weighted Party (System) Nationalization Score (Bochsler 2010)'
			} else if (stat=='pns10') {
				auxfun <- bochsler2010pns10
				out$name <- 'Standardized Party (System) Nationalization Score (Bochsler 2010)'
			} else if (stat=='tci') {
				auxfun <- caramani2004tci 
				out$name <- 'Territorial Coverage Index (Caramani 2004)'
			} else if (stat=='ipr1') {
				auxfun <- golosovponarin1999ipr
				out$name <- 'Index of Party Regionalization (Golosov and Ponarin 1999)'
			} else if (stat=='cpr') {
				auxfun <- golosov2014cpr
				out$name <- 'Coefficient of Party Regionalization (Golosov 2014)'
			} else if (stat=='ipn') {
				auxfun <- golosov2014ipn
				out$name <- 'Index of Party (System) Nationalization (Golosov 2014)'
			} else if (stat=='npns') {
				stop('Not yet supported.')
				out$name <- 'Normalized Party (System) Nationalization Score (Golosov 2014)'
			} else if (stat=='pns') {
				auxfun <- jonesmainwaring2003pns
				out$name <- 'Party Nationalization Score (Jones and Mainwaring 2003)'
			} else if (stat=='lee') {
				auxfun <- lee1988
				out$name <- 'Lee index (Lee 1988)'
			} else if (stat=='mad') {
				auxfun <- roseurwin1975mad
				out$name <- 'Index of variation/Mean Absolute Deviation (Rose and Urwin 1975)'
			} else if (stat=='msd') {
				auxfun <- msd
				out$name <- 'Mean Standard Deviation of row shares'
			} else if (stat=='var') {
				auxfun <- variance
				out$name <- 'Variance of row shares'
			} else if (stat=='vc') {
				auxfun <- erssonetal1985vc
				out$name <- 'Variability Coefficient (Ersson et al 1985)'
			} else if (stat=='swvc') {
				auxfun <- erssonetal1985swvc
				out$name <- 'Standardized and Weighted Variability Coefficient (Ersson et al 1985)'
			} else if (stat=='nvc') {
				auxfun <- golosov2014ncv
				out$name <- 'Normalized Coefficient of Variation (Glosov 2014)'
			} else if (stat=='ipr2') {
				auxfun <- caramani2004ipr
				out$name <- 'Index adjusted for Party size and number of Regions (Caramani 2004)'
			} else if (stat=='cri') {
				auxfun <- roseurwin1975cri
				out$name <- 'Cumulative Regional Inequality (Rose and Urwin 1975)'
			} else if (stat=='ipa1') {
				auxfun <- chibberkollman1998ipa
				out$name <- 'Indicator of Party Aggregation (Chibber Kollman 1998)'
			} else if (stat=='is') {
				auxfun <- cox1999is
				out$name <- 'Inflation Score (Cox 1999)'
			} else if (stat=='ipa2') {
				auxfun <- allik2006ipa
				out$name <- 'Index of Party Aggregation (Allik 2006)'
			} else if (stat=='ii') {
				auxfun <- moeniuskasuya2004ii
				out$name <- 'Inflation Index (Moenius and Kasuya 2004)'
			} else if (stat=='delta') {
				auxfun <- dissimilarity2015
				out$name <- 'Dissimilarity Index'
			} else if (stat=='mi') {
				auxfun <- mutualinfo
				out$name <- 'Mutual Information (bits)'
			} else {
				stop('Unsupported statistic requested.')
			}
		} else if (is.function(statistic)) {
			auxfun <- statistic
			out$name <- 'User supplied function.'
		} else {
			stop('Invalid statistic argument')
		}	

		l <- auxfun(x=mat,
					weight_choice=weight_choice,
					weight_territory=weight_territory)
	
		out$total <- data.frame(est=l$total, se=NA, low=NA, upp=NA)
		out$choices <- data.frame(est=l$choices, se=NA, low=NA, upp=NA)
		rownames(out$choices) <- colnames(mat)

		if ( (confidence_level<0) | (confidence_level>1) ) {
			stop('Confidence level must be from [0, 1].')
		}
		
		num_boot <- as.numeric(boot)
		num_jack <- as.numeric(jack)
		num_subs <- as.numeric(subsample)
		

		if ( (num_boot+num_jack+num_subs) > 1 )	{
		   	
			stop('Only one of {boot, jack, susbample} is allowed to be TRUE.')

	   	} else if (boot) {

			cat('Bootstrap in progress, no. rep. =', n_rep, '\n')
			p_low <- (1-confidence_level)/2
			p_upp <- 1 - p_low 
			pp <- 1 - (1-confidence_level)/2
			auxboot <-
				function(m=mat)
				{
					z <- matrix(rmultinom(1, sum(m), as.vector(m)),
							   	nrow(m), ncol(m))
					rownames(z) <- rownames(m)
					colnames(z) <- colnames(m)
					return(z)
				}
			S <- lapply(1:n_rep, function(i) auxboot())
			R <- lapply(S, auxfun,
						weight_choice=weight_choice,
						weight_territory=weight_territory)
			r <- list()
			r$total$boot <- do.call(c, lapply(R, function(i) i$total))
			r$choices$boot <- do.call(rbind, lapply(R, function(i) i$choices))
			#	
			if (bias) {
				out$total$est <- 2*l$total - mean(r$total$boot)  
				out$choices$est <- sapply(1:length(l$choices),
										  function(i) 2*l$choices[i] - mean(r$choices$boot[,i]))
			}
			out$total$se <- sd(r$total$boot)
			out$total$low <- out$total$est - qnorm(pp)*out$total$se
			out$total$upp <- out$total$est + qnorm(pp)*out$total$se
			out$choices$se <- apply(r$choices$boot, 2, sd)
			out$choices$low <- out$choices$est - qnorm(pp)*out$choices$se
			out$choices$upp <- out$choices$est + qnorm(pp)*out$choices$se

		} else if (jack) {

			auxjack <- function(i, m) {	m[i] <- m[i]-1 ; return(m) }
			rep_vec <- which(as.vector(mat)!=0)
			cat('Jackknife in progress, no. rep. =', length(rep_vec), '\n')
			S <- lapply(rep_vec, auxjack, m=mat)
			S <- lapply(S, function(i)
						{
							i <- matrix(i, nrow(mat), ncol(mat))
							rownames(i) <- rownames(mat)
							colnames(i) <- colnames(mat)
							return(i)
						})
			R <- lapply(S, auxfun,
						weight_choice=weight_choice,
						weight_territory=weight_territory)
			r <- list()
			r$total$jack <- do.call(c, lapply(R, function(i) i$total))
			r$choices$jack <- do.call(rbind, lapply(R, function(i) i$choices))
			#
			jack_total <- pool.jack(data=mat,
									ct=TRUE,
								   	estimate=out$total$est,
									jack_est=r$total$jack,
									side=NULL,
									conf=confidence_level,
									lower=-Inf,
									upper=Inf,
									bias=bias)
			#
			jack_choices <- lapply(1:ncol(r$choices$jack),
								   function(j)
								   {
									   pool.jack(data=mat,
												 ct=TRUE,
												 estimate=out$choices$est[j],
												 jack_est=r$choices$jack[,j],
												 side=NULL,
												 conf=confidence_level,
												 lower=-Inf,
												 upper=Inf,
												 bias=bias)
								   })
			#
			out$total$se <- jack_total$se
			out$total$low <- jack_total$low
			out$total$upp <- jack_total$upp
			out$choices$se <- sapply(jack_choices, function(i) i$se)
			out$choices$low <- sapply(jack_choices, function(i) i$low)
			out$choices$upp <- sapply(jack_choices, function(i) i$upp)
			out$conf <- confidence_level
			if (bias) {
				out$total$est <- jack_total$theta
				out$choices$est <- sapply(jack_choices, function(i) i$theta)
			}

		} else if (subsample) {

			pp <- 1 - (1-confidence_level)/2
			cat('Subsampling in progress, no. rep. =', n_rep, '\n')
			b <- size
			sqrt_b <- sqrt(b)
			sqrt_n <- sqrt(sum(mat))
			alpha <- 1 - confidence_level
			alpha_ps <- c(1-alpha/2, alpha/2)
			l_ch <- length(l$choices) 
			auxsub <- 
				function(m=mat, b=b)
				{
					y <- m
					y[1:length(y)] <- 0
					s <- table(sample(rep(1:length(m), m), size=b, replace=FALSE))	
					y[as.integer(names(s))] <- s
					return(y)
				}
			S <- lapply(1:n_rep, function(i) auxsub(m=mat, b=b))
			R <- lapply(S, auxfun,
						weight_choice=weight_choice,
						weight_territory=weight_territory)
			r <- list()
			r$total$sub <- do.call(c, lapply(R, function(i) i$total))
			r$choices$sub <- do.call(rbind, lapply(R, function(i) i$choices))
			r$total$est_star <- sqrt_b*(r$total$sub-l$total)
			if (bias) {
				out$total$est <- 2*l$total - mean(r$total$sub)
				out$choices$est <- 2*l$choices - colMeans(r$choices$sub)
			}
			out$total$se <- sd(r$total$sub)*sqrt(b/sum(mat))
			out$choices$se <- sapply(1:l_ch, function(j) sd(r$choices$sub[,j])/sqrt(b/sum(mat)))
			out$total$low <- out$total$est - qnorm(pp)*out$total$se 
			out$total$upp <- out$total$est + qnorm(pp)*out$total$se 
			out$choices$low <- out$choices$est - qnorm(pp)*out$choices$se
			out$choices$upp <- out$choices$est + qnorm(pp)*out$choices$se

		}
	    out$confidence_level <- confidence_level    

		class(out) <- 'partynat'
		return(out)
	}
#	END
