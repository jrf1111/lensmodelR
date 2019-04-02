# to do list -------------------------------------------------------------------
# add formula interface
# add plotting function



#' @param criterion the variable that judgments are being compared against
#' @param judgment the variable for people's decisions
#' @param cues variables/cues in the environment for the environment and judge's models.  Categorical vars must be dummy coded
#' @param j_cues variables/cues in the environment for the judge's model.  Categorical vars must be dummy coded
#' @param e_cues variables/cues in the environment for the environment's model.  Categorical vars must be dummy coded
#' @param j_ID judge ID
#' @param e_ID environment/stimulus ID
#' @param j_misc other judge/respondent-level variables to save, such as condition, age, sex, etc
#' @param e_misc other environment/stimulus-level variables to save, such as brand, etc
#' @param std How should variables be standardized? Use "both" or TRUE to standardize X and Y variables.  
#'            Use "x" for X standardization (scale predictors only). Use "y" for Y standardization (scale outcomes only).
#'            Use "none" or FALSE if no standardization is wanted.
#' @param method Function name specifying how the models should be fit. Can use any method that 
#'                has broom::tidy(), broom::glance(), and predict() methods, 
#'                and accepts formulas. Default is lm.
#' @param step Character vector indicating if the function step() be used to reduce the number of cues used in the environment's and/or judges' models?
#'             Default is step="n" for No. Use step="j" to reduce judge's models.  Use step="e" to reduce environment's model.  
#'             Use step=c("e", "j") to use in both.  
#'             Using step=FALSE will be treated as step="n".  Using step=TRUE will be treated as step=c("e", "j").
#' @param parallel Should the judges' models be run in parallel? Default is FALSE.
#' @param cores Number of cores to use if running in parallel.  Default is 3.
#' @param save Character vector indicating if the model objects for the environment (e) and/or judges (j) should be saved.
#'             Default is c("e").  Use c("e", "j") if the individual model objects should be saved for the judges and the environment.
#'             
#' @param data The data.frame, data.table, or matrix containing the data.  
#' @param ... Arguments passed to other methods. For example, if method = glm, you might pass family = binomial.  
#'            If step = T, you might pass a value for k or direction
#'
#' @return returns a list of class 'lens' containing several elements:
#' 1) ind_data: a data.frame for the individual judgements that contains the j_ID variable, j_misc variables, 
#'      regression coefs, model fit information (R, R squared, adjusted R squared, residual standard error, 
#'      omnibus test statistic (e.g., F-ratio), AIC, BIC, and deviance), and the lens model statistics 
#'      G (correlation between fitted values from model predicting criterion and fitted values from model predicting judgment), 
#'      accuracy (correlation between criterion and judgment), and C (unmodelled knowledge; correlation between residuals 
#'      from model predicting criterion and residuals from model predicting judgment)
#'      
#' 2) env_data: a data.frame for the environment that contains the criterion,
#'      predicted value of the criterion (suffix "_hat"), the prediction residual, and the e_misc variables
#'      
#' 3) env_model: a data.frame containing regression coefs and model fit information (R squared, 
#'      adjusted R squared, residual standard error, F ratio, AIC, BIC, and deviance) for the environment model
#'      
#' 4) env_fit: the environment model object
#'      
#' 5) j_cues: a character vector containing the cues used in the models for the judges
#' 
#' 6) e_cues: a character vector containing the cues used in the model for the environment
#' 



library(foreach)
library(doParallel)
library(broom)
lensModel = function(criterion, judgment, cues=NULL, j_cues=NULL, e_cues=NULL, j_ID, j_misc=NULL,
										 e_ID, e_misc=NULL, std = c("both", "x", "y", "none"),
										 method = glm, method_args = list(),
										 step = "n", step_args = list(k = 2, direction = "both", scope = .~.),
										 pred_type = "response",
										 parallel = F, cores = 3, save = c("e"), data){
	
	if(is.null(j_cues) & !is.null(cues)){
		warning("'j_cues' is null. Setting 'j_cues = cues'", immediate. = T)
		j_cues = cues
	}
	
	if(is.null(j_cues) & is.null(cues)){
		warning("'j_cues' and 'cues' are null. Judge's models are intercept only", immediate. = T)
	}
	
	if(is.null(e_cues) & !is.null(cues)){
		warning("'e_cues' is null. Setting 'e_cues = cues'", immediate. = T)
		e_cues = cues
	}
	
	if(is.null(e_cues) & is.null(cues)){
		warning("'e_cues' and 'cues' are null. Environment's model is intercept only", immediate. = T)
	}
	
	
	
	if(step==T) step=c("e", "j") else step="n"
	estep=F
	jstep=F
	if("e" %in% step) estep=T else estep=F
	if("j" %in% step) jstep=T else jstep=F
	
	
	std = match.arg(std, c("both", "x", "y", "none"))
	
	
	
	data = as.data.frame(data)
	

# environment analyses ----------------------------------------------------
	{
		env_data = data.frame(e_ID = unique(data[, e_ID]))
		
		temp = data[!duplicated(data[, e_ID]), ]
		
		
		# if UNstandardized coefs are requested
		if(std == "none"){
			
			#make formula
			form = as.formula(paste(criterion, "~", paste(e_cues, collapse = "+")))
			
			#fit environment model
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			
			if(estep){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			#get coefficients
			tidy = broom::tidy(env_fit)
			
			#put coefficients in a data.frame
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(e_cues)){
				
				term =  e_cues[a]
				
				if(term %in% tidy$term){
					env_coef[1, paste0("b_", e_cues[a]) ] = tidy[tidy$term == term, "estimate" ]
					env_coef[1, paste0("se_", e_cues[a]) ] = tidy[tidy$term == term, "std.error" ]
				} else{
					env_coef[1, paste0("b_", e_cues[a]) ] = NA
					env_coef[1, paste0("se_", e_cues[a]) ] = NA
				}
			}
			
			rownames(env_coef) = NULL
			
			#save predicted values and residuals
			criterion_hat = paste0(criterion, "_hat")
			data[, criterion_hat] = predict(env_fit, type = pred_type, newdata = data)
			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
			
			
		}
		
		# if standardized coefs are requested
		if(std == "both") {
			
			#make formula
			form = as.formula(paste("scale(", criterion, ") ~",
															"scale(", paste(e_cues, collapse = ")+scale("), ")"  ))
			
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			if(estep){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			tidy = broom::tidy(env_fit)
			
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(e_cues)){
				
				term = paste0( "scale(", e_cues[a], ")" )
				
				if(term %in% tidy$term){
					env_coef[1, paste0("B_", e_cues[a]) ] = tidy[tidy$term == term, "estimate" ]
					env_coef[1, paste0("se_", e_cues[a]) ] = tidy[tidy$term == term, "std.error" ]
				} else{
					env_coef[1, paste0("B_", e_cues[a]) ] = NA
					env_coef[1, paste0("se_", e_cues[a]) ] = NA
				}
				
			}
			
			rownames(env_coef) = NULL
			
			
			criterion_hat = paste0(criterion, "_hat")
			data[, criterion_hat] = (as.numeric(predict(env_fit, type = pred_type, newdata = data)) * sd(as.numeric(data[, criterion]))) +
				mean(as.numeric(data[, criterion]))
			#to undo the standardization/scaling
			
			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
			
			
		}
		
		
		# if X standardized coefs are requested
		if(std == "x") {
			
			#make formula
			form = as.formula(paste(criterion, "~",
															"scale(", paste(e_cues, collapse = ")+scale("), ")"  ))
			
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			if(estep){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			tidy = broom::tidy(env_fit)
			
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(e_cues)){
				
				term = paste0( "scale(", e_cues[a], ")" )
				
				if(term %in% tidy$term){
					env_coef[1, paste0("B_", e_cues[a]) ] = tidy[tidy$term == term, "estimate" ]
					env_coef[1, paste0("se_", e_cues[a]) ] = tidy[tidy$term == term, "std.error" ]
				} else{
					env_coef[1, paste0("B_", e_cues[a]) ] = NA
					env_coef[1, paste0("se_", e_cues[a]) ] = NA
				}
				
			}
			
			rownames(env_coef) = NULL
			
			
			criterion_hat = paste0(criterion, "_hat")
			data[, criterion_hat] = predict(env_fit, type = pred_type, newdata = data)
			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
			
			
		}
		
		
		# if Y standardized coefs are requested
		if(std == "y") {
			
			#make formula
			form = as.formula(paste("scale(", criterion, ") ~",
															paste(e_cues, collapse = "+")))
			
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			if(estep){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			tidy = broom::tidy(env_fit)
			
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(e_cues)){
				
				term =  e_cues[a]
				
				if(term %in% tidy$term){
					env_coef[1, paste0("b_", e_cues[a]) ] = tidy[tidy$term == term, "estimate" ]
					env_coef[1, paste0("se_", e_cues[a]) ] = tidy[tidy$term == term, "std.error" ]
				} else{
					env_coef[1, paste0("b_", e_cues[a]) ] = NA
					env_coef[1, paste0("se_", e_cues[a]) ] = NA
				}
			}
			
			rownames(env_coef) = NULL
			
			
			criterion_hat = paste0(criterion, "_hat")
			data[, criterion_hat] = (as.numeric(predict(env_fit, type = pred_type, newdata = data)) * sd(as.numeric(data[, criterion]))) +
				mean(as.numeric(data[, criterion]))
			#to undo the standardization/scaling
			
			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
			
		}
		
		
		
		
		env_model_fit = as.data.frame(broom::glance(env_fit))
		env_model_fit$r_squared = cor( as.numeric(data[, criterion]), as.numeric(data[, criterion_hat]) )^2
		
		cue_terms = as.character(form)[3]
		cue_terms = unlist(strsplit(cue_terms, " + ", fixed = T))
		
		p = sum(cue_terms %in% tidy$term)+1
		n = nobs(env_fit)
		
		#from summary.lm ans$adj.r.squared <-   1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
		env_model_fit$r_squared_adj = 1 - (1 - env_model_fit$r_squared) * ((n-1)/(n-p)) #adjusted R2
		
		
		
		#save e_misc vars
		for(i in 1:nrow(env_data)){
			
			temp = data[which(data[, e_ID] == env_data[i, "e_ID"] ),  ]
			
			for(a in 1:length(e_misc)){
				
				env_data[i, e_misc[a]] = temp[1, e_misc[a]]
				env_data[i, criterion_hat] = temp[1, criterion_hat]
				env_data[i, "residual"] = temp[1, "env_resid"]
			}
			
			
		}
		
		
		
		colnames(env_data)[1] = e_ID
		
	}
	
	
	
	

# judges analyses ---------------------------------------------------------
	{
		
		if(parallel){
			cores = as.integer(cores)
			if(is.null(cores) | is.na(cores)){
				doParallel::registerDoParallel()
			} else{
				doParallel::registerDoParallel(cores = cores)
			}
		} 
		
		
		my_combine = function(...){
			
			dots = list(...)
			
			info = lapply(dots, function(x) x[[1]])
			info = plyr::rbind.fill(info)
			
			mods = lapply(dots, function(x) x[[2]])
			
			list(info, mods)
		}
		
		n_judges = length(unique(data[, j_ID]))
		
		ind_res = foreach::foreach(i = 1:n_judges, 
															 .combine=my_combine, .multicombine = T, 
															 .maxcombine = n_judges) %dopar% {
			eval_judge(i = i, criterion = criterion, judgment = judgment, j_cues = j_cues, criterion_hat = criterion_hat,
								 j_ID = j_ID, j_misc = j_misc, std = std, method = method, method_args = method_args,
								 pred_type = pred_type, step = jstep, step_args = step_args, data = data, save = save)
		}
		
		doParallel::stopImplicitCluster()
		
		ind_data = ind_res[[1]]
		colnames(ind_data)[1] = j_ID
		
		ind_mods = ind_res[[2]]
		names(ind_mods) = ind_data[, j_ID]
		rm(ind_res)
		
	}
	
	
	
	
	result = list(ind_data = ind_data, env_data = env_data,
								env_model = as.data.frame(cbind(env_coef, env_model_fit)),
								cues = cues, e_cues = e_cues, j_cues = j_cues
	)
	
	if("e" %in% save){
		result[["env_fit"]] = env_fit
	}
	
	if("j" %in% save){
		result[["ind_mods"]] = ind_mods
	}
	
	class(result) = "lens"
	
	result
	
}


#eval_judge is an ancillary function to do judge level analyses with %dopar%
#'Code outline
#'for a single j_ID:
#'    subset data
#'    save j_misc vars
#'    run model
#'    step() model if needed
#'    save coefs
#'    save G, accuracy, C, model fit info
#'    return ind_data as a one row data.frame

eval_judge = function(i, criterion, judgment, j_cues, j_ID, j_misc,
											std, method, method_args, pred_type, step, step_args, 
											data, criterion_hat, save, ...){
	
	r_ID_scalar = unique( data[, j_ID] )[i]
	
	ind_data = data.frame(j_ID = r_ID_scalar, stringsAsFactors=F)
	
	temp = data[which(data[, j_ID] == r_ID_scalar),  ]
	
	
	#save j_misc vars
	for(a in 1:length(j_misc)){
		ind_data[1, j_misc[a]] = temp[1, j_misc[a]]
	}
	
	
	# if UNstandardized coefs are requested
	if(std == "none") {
		
		#make formula
		form = as.formula(paste(judgment, "~", paste(j_cues, collapse = "+")))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		
		
		for( a in 1:length(j_cues)){
			
			term =  j_cues[a]
			
			if(term %in% tidy$term){
				ind_data[1, paste0("b_", j_cues[a]) ] = tidy[tidy$term == term, "estimate" ]
				ind_data[1, paste0("se_", j_cues[a]) ] = tidy[tidy$term == term, "std.error" ]
			} else{
				ind_data[1, paste0("b_", j_cues[a]) ] = NA
				ind_data[1, paste0("se_", j_cues[a]) ] = NA
			}
		}
		
		
	}
	
	
	# if standardized coefs are requested
	if(std == "both") {
		
		#make formula
		form = as.formula(paste("scale(", judgment, ") ~",
														"scale(", paste(j_cues, collapse = ")+scale("), ")"  ))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(j_cues)){
			
			term = paste0( "scale(", j_cues[a], ")" )
			
			if(term %in% tidy$term){
				ind_data[1, paste0("B_", j_cues[a])   ] = tidy[tidy$term == term, "estimate" ]
				ind_data[1, paste0("se_", j_cues[a])   ] = tidy[tidy$term == term, "std.error" ]
			} else{
				ind_data[1, paste0("B_", j_cues[a])   ] = NA
				ind_data[1, paste0("se_", j_cues[a])   ] = NA
			}
			
		}
	}
	
	
	# if X standardized coefs are requested
	if(std == "x") {
		
		#make formula
		form = as.formula(paste(judgment, "~",
														"scale(", paste(j_cues, collapse = ")+scale("), ")"  ))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(j_cues)){
			
			term = paste0( "scale(", j_cues[a], ")" )
			
			if(term %in% tidy$term){
				ind_data[1, paste0("B_", j_cues[a])   ] = tidy[tidy$term == term, "estimate" ]
				ind_data[1, paste0("se_", j_cues[a])   ] = tidy[tidy$term == term, "std.error" ]
			} else{
				ind_data[1, paste0("B_", j_cues[a])   ] = NA
				ind_data[1, paste0("se_", j_cues[a])   ] = NA
			}
			
		}
	}
	
	
	# if Y standardized coefs are requested
	if(std == "y") {
		
		#make formula
		form = as.formula(paste("scale(", judgment, ") ~",
														paste(j_cues, collapse = "+")))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(j_cues)){
			
			term =  j_cues[a]
			
			if(term %in% tidy$term){
				ind_data[1, paste0("b_", j_cues[a]) ] = tidy[tidy$term == term, "estimate" ]
				ind_data[1, paste0("se_", j_cues[a]) ] = tidy[tidy$term == term, "std.error" ]
			} else{
				ind_data[1, paste0("b_", j_cues[a]) ] = NA
				ind_data[1, paste0("se_", j_cues[a]) ] = NA
			}
		}
	}
	
	
	ind_data$accuracy = cor(as.numeric(temp[, criterion]), as.numeric(temp[, judgment]))
	
	ind_data$G = cor(as.numeric(temp[, criterion_hat]), as.numeric(unlist(predict(fit, type = pred_type, newdata = temp))) )
	
	ind_data$C = cor(as.numeric(temp[, "env_resid"]), 
									 (as.numeric(temp[, judgment])-as.numeric(unlist(predict(fit, type = pred_type, newdata = temp))) ))
	
	
	
	
	cue_terms = as.character(form)[3]
	cue_terms = unlist(strsplit(cue_terms, " + ", fixed = T))
	
	p = sum(cue_terms %in% tidy$term)+1
	n = nobs(fit)
	
	
	ind_data$R = cor(as.numeric(temp[, judgment]), as.numeric(unlist(predict(fit, type = pred_type, newdata = temp))))
	
	ind_data$r_squared = cor(as.numeric(temp[, judgment]), as.numeric(unlist(predict(fit, type = pred_type, newdata = temp))) )^2
	ind_data$r_squared_adj = 1 - (1 - ind_data$r_squared) * ((n-1)/(n-p)) #adjusted R2
	#from summary.lm ans$adj.r.squared <-   1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
	
	
	
	if("statistic" %in% colnames(broom::glance(fit))){
		ind_data$statistic = as.numeric(glance(fit)["statistic"])
	}
	
	ind_data$aic = as.numeric(glance(fit)["AIC"])
	ind_data$bic = as.numeric(glance(fit)["BIC"])
	
	if("deviance" %in% colnames(glance(fit))){
		ind_data$deviance = as.numeric(glance(fit)["deviance"])
	}
	
	if("j" %in% save){
		ind_data = list(ind_data, fit)
	}
	
	names(ind_data)[2] = r_ID_scalar
	
	ind_data
	
}




#p-value is not calculated for judgments because the correct test will depend on the method used
summary.lens = function(lens, stat = median, by = NULL, digits = 3, conf = 0.95, ...){
	
	environment = as.data.frame(broom::tidy(lens$env_fit))
	alpha = 1-conf
	environment[, paste0("LCI_", conf)] = environment$estimate - (qnorm(1-(alpha/2))*environment$std.error)
	environment[, paste0("UCI_", conf)] = environment$estimate + (qnorm(1-(alpha/2))*environment$std.error)
	if("coefficient_type" %in% colnames(environment)) environment$coefficient_type=NULL
	environment[, -1] = round(environment[, -1], digits = digits)
	
	cues = lens$cues
	ind_data = lens$ind_data
	
	ind_coefs = data.frame(term = cues, estimate = 0, std.error = 0, statistic = 0, included = 0, stringsAsFactors = F)
	
	for(i in 1:length(cues)){
		if(paste0("B_", cues[i]) %in% colnames(ind_data)){
			ind_coefs$estimate[i] = stat(ind_data[, paste0("B_", cues[i])], na.rm=T)
			ind_coefs$std.error[i] = stat(ind_data[, paste0("se_", cues[i])], na.rm=T)
			ind_coefs$included[i] = mean(!is.na(ind_data[, paste0("se_", cues[i])]))
			ind_coefs$term[i] = paste0("scale(", cues[i], ")")
		}
		
		if(paste0("b_", cues[i]) %in% colnames(ind_data)){
			ind_coefs$estimate[i] = stat(ind_data[, paste0("b_", cues[i])], na.rm=T)
			ind_coefs$std.error[i] = stat(ind_data[, paste0("se_", cues[i])], na.rm=T)
			ind_coefs$included[i] = mean(!is.na(ind_data[, paste0("se_", cues[i])]))
			ind_coefs$term[i] = cues[i]
		}
		
		if( !(paste0("B_", cues[i]) %in% colnames(ind_data)) & !(paste0("b_", cues[i]) %in% colnames(ind_data))  ){
			ind_coefs$estimate[i] = NA
			ind_coefs$std.error[i] = NA
			ind_coefs$included[i] = NA
			ind_coefs$term[i] = cues[i]
		}
		
	}
	
	
	ind_coefs = rbind( c("(Intercept)", stat(ind_data$intercept), NA, NA, mean(!is.na(ind_data$intercept))), ind_coefs)
	
	ind_coefs[, "estimate"] = as.numeric(ind_coefs[, "estimate"])
	ind_coefs[, "std.error"] = as.numeric(ind_coefs[, "std.error"])
	ind_coefs[, "included"] = as.numeric(ind_coefs[, "included"])
	
	ind_coefs$statistic = ind_coefs$estimate/ind_coefs$std.error
	
	ind_coefs[, paste0("LCI_", conf)] = ind_coefs$estimate - (qnorm(1-(alpha/2))*ind_coefs$std.error)
	ind_coefs[, paste0("UCI_", conf)] = ind_coefs$estimate + (qnorm(1-(alpha/2))*ind_coefs$std.error)
	
	
	ind_coefs[, -1] = round(ind_coefs[, -1], digits = digits)
	
	
	res = list(environment = environment,
						 judgements = ind_coefs, 
						 G = round(summary(ind_data$G), digits = digits),
						 Accuracy = round(summary(ind_data$accuracy), digits = digits),
						 C = round(summary(ind_data$C), digits = digits)
	)
	
	
	if(!is.null(by)){
		
		groups = unique(ind_data[, by])
		
		for(b in 1:length(groups)){
			
			g = groups[b]
			
			temp = ind_data[which(ind_data[, by] == g),]
			
			g_coef = data.frame(term = cues, estimate = 0, std.error = 0, statistic = 0, included = 0, stringsAsFactors = F)
			
			for(i in 1:length(cues)){
				if(paste0("B_", cues[i]) %in% colnames(temp)){
					g_coef$estimate[i] = stat(temp[, paste0("B_", cues[i])], na.rm=T)
					g_coef$std.error[i] = stat(temp[, paste0("se_", cues[i])], na.rm=T)
					g_coef$included[i] = mean(!is.na(temp[, paste0("se_", cues[i])]))
					g_coef$term[i] = paste0("scale(", cues[i], ")")
				}
				
				if(paste0("b_", cues[i]) %in% colnames(temp)){
					g_coef$estimate[i] = stat(temp[, paste0("b_", cues[i])], na.rm=T)
					g_coef$std.error[i] = stat(temp[, paste0("se_", cues[i])], na.rm=T)
					g_coef$included[i] = mean(!is.na(temp[, paste0("se_", cues[i])]))
					g_coef$term[i] = cues[i]
				}
				
				if( !(paste0("B_", cues[i]) %in% colnames(temp)) & !(paste0("b_", cues[i]) %in% colnames(temp))  ){
					g_coef$estimate[i] = NA
					g_coef$std.error[i] = NA
					g_coef$included[i] = NA
					g_coef$term[i] = cues[i]
				}
				
			}
			
			g_coef = rbind( c("(Intercept)", stat(temp$intercept), NA, NA, mean(!is.na(temp$intercept))), g_coef)
			
			g_coef[, "estimate"] = as.numeric(g_coef[, "estimate"])
			g_coef[, "std.error"] = as.numeric(g_coef[, "std.error"])
			g_coef[, "included"] = as.numeric(g_coef[, "included"])
			
			g_coef$statistic = g_coef$estimate/g_coef$std.error
			
			g_coef[, paste0("LCI_", conf)] = g_coef$estimate - (qnorm(1-(alpha/2))*g_coef$std.error)
			g_coef[, paste0("UCI_", conf)] = g_coef$estimate + (qnorm(1-(alpha/2))*g_coef$std.error)
			
			
			g_coef[, -1] = round(g_coef[, -1], digits = digits)
			
			res[[ paste0("judgements.", g) ]] = g_coef
			
			res[[ paste0("G.", g) ]] = round(summary(temp$G), digits = digits)
			res[[ paste0("Accuracy.", g) ]]= round(summary(temp$accuracy), digits = digits)
			res[[ paste0("C.", g) ]] = round(summary(temp$C), digits = digits)
			
		}
		
	}
	
	res
	
}





