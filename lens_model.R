# to do list -------------------------------------------------------------------
# convert to use broom::tidy to get all coefs instead of using a for loop.  will need to change
#       .combine arg in the main loop to plyr::rbind.fill
# add formula interface?
# add plotting function



#' @param criterion the variable that judgments are being compared against
#' @param judgment the variable for people's decisions
#' @param cues variables in the environment.  Categorical vars must be dummy coded
#' @param r_ID respondent ID
#' @param s_ID stimulus ID
#' @param r_misc other respondent-level variables to save, such as condition, age, sex, etc
#' @param s_misc other stimulus-level variables to save, such as brand, etc
#' @param std How should variables be standardized? Use "both" or TRUE to standardize X and Y variables.  
#'            Use "x" for X standardization (scale predictors only). Use "y" for Y standardization (scale outcomes only).
#'            Use "none" or FALSE if no standardization is wanted.
#' @param method Function name specifying how the models should be fit. Can use any method that 
#'                has broom::tidy(), broom::glance(), and predict() methods, 
#'                and accepts formulas. Default is lm.
#' @param step Should the function step() be used to reduce the number of cues used in the environment's and judges' models?
#'             default is FALSE.  If TRUE, the default behavior uses step(..., direction = "both)
#' @param parallel Should the judges' models be run in parallel? Default is FALSE.
#' @param cores Number of cores to use if running in parallel.  Default is 3.
#' @param data The data.frame, data.table, or matrix containing the data.  
#' @param ... Arguments passed to other methods. For example, if method = glm, you might pass family = binomial.  
#'            If step = T, you might pass a value for k or direction
#'
#' @return returns a list of class 'lens' containing several elements:
#' 1) ind_data: a data.frame for the individual judgements that contains the r_ID variable, r_misc variables, 
#'      regression coefs, model fit information (R, R squared, adjusted R squared, residual standard error, 
#'      omnibus test statistic (e.g., F-ratio), AIC, BIC, and deviance), and the lens model statistics 
#'      G (correlation between fitted values from model predicting criterion and fitted values from model predicting judgment), 
#'      accuracy (correlation between criterion and judgment), and C (unmodelled knowledge; correlation between residuals 
#'      from model predicting criterion and residuals from model predicting judgment)
#'      
#' 2) env_data: a data.frame for the environment that contains the criterion,
#'      predicted value of the criterion (suffix "_hat"), the prediction residual, and the s_misc variables
#'      
#' 3) env_model: a data.frame containing regression coefs and model fit information (R squared, 
#'      adjusted R squared, residual standard error, F ratio, AIC, BIC, and deviance) for the environment model
#'      
#' 4) env_fit: the environment model object
#'      
#' 5) cues: a character vector containing the cues used in the analysis
#' 



library(foreach)
library(doParallel)
library(broom)
lensModel = function(criterion, judgment, cues, r_ID, r_misc,
										 s_ID, s_misc, std = c("both", "x", "y", "none"),
										 method = glm, method_args = list(),
										 step = F, step_args = list(k = 2, direction = "both", scope = .~.),
										 pred_type = "response",
										 parallel = F, cores = 3, data){
	
	
	
	std = match.arg(std, c("both", "x", "y", "none"))
	
	
	
	data = as.data.frame(data)
	
	
	#environment analyses
	{
		env_data = data.frame(s_ID = unique(data[, s_ID]))
		
		temp = data[!duplicated(data[, s_ID]), ]
		
		
		# if UNstandardized coefs are requested
		if(std == "none"){
			
			#make formula
			form = as.formula(paste(criterion, "~", paste(cues, collapse = "+")))
			
			#fit environment model
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			
			if(step){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			#get coefficients
			tidy = broom::tidy(env_fit)
			
			#put coefficients in a data.frame
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(cues)){
				env_coef[1, paste0("b_", cues[a]) ] = tidy[tidy$term == cues[a], "estimate" ]
				env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == cues[a], "std.error" ]
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
															"scale(", paste(cues, collapse = ")+scale("), ")"  ))
			
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			if(step){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			tidy = broom::tidy(env_fit)
			
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(cues)){
				
				term = paste0( "scale(", cues[a], ")" )
				
				if(term %in% tidy$term){
					env_coef[1, paste0("B_", cues[a]) ] = tidy[tidy$term == term, "estimate" ]
					env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == term, "std.error" ]
				} else{
					env_coef[1, paste0("B_", cues[a]) ] = NA
					env_coef[1, paste0("se_", cues[a]) ] = NA
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
															"scale(", paste(cues, collapse = ")+scale("), ")"  ))
			
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			if(step){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			tidy = broom::tidy(env_fit)
			
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(cues)){
				
				env_coef[1, paste0("B_", cues[a]) ] = tidy[tidy$term == paste0( "scale(", cues[a], ")" ), "estimate" ]
				env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == paste0( "scale(", cues[a], ")" ), "std.error" ]
				
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
															paste(cues, collapse = "+")))
			
			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
			
			if(step){
				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
			}
			
			tidy = broom::tidy(env_fit)
			
			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
			
			for( a in 1:length(cues)){
				
				env_coef[1, paste0("b_", cues[a]) ] = tidy[tidy$term == cues[a], "estimate" ]
				env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == cues[a], "std.error" ]
				
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
		
		
		
		#save s_misc vars
		for(i in 1:nrow(env_data)){
			
			temp = data[which(data[, s_ID] == env_data[i, "s_ID"] ),  ]
			
			for(a in 1:length(s_misc)){
				
				env_data[i, s_misc[a]] = temp[1, s_misc[a]]
				env_data[i, criterion_hat] = temp[1, criterion_hat]
				env_data[i, "residual"] = temp[1, "env_resid"]
			}
			
			
		}
		
		
		
		colnames(env_data)[1] = s_ID
		
	}
	
	
	
	
	
	#respondent level analyses
	{
		
		if(parallel){
			
			cores = as.integer(cores)
			
			if(is.null(cores) | is.na(cores)){
				doParallel::registerDoParallel()
			} else{
				doParallel::registerDoParallel(cores = cores)
			}
			
			ind_data = foreach::foreach(i = 1:length(unique(data[, r_ID])), .combine=rbind) %dopar% {
				eval_judge(i = i, criterion = criterion, judgment = judgment, cues = cues, criterion_hat = criterion_hat,
									 r_ID = r_ID, r_misc = r_misc, std = std, method = method, method_args = method_args,
									 pred_type = pred_type, step = step, step_args = step_args, data = data)
			}
			
			doParallel::stopImplicitCluster()
			
		} else{
			
			ind_data = foreach(i = 1:length(unique(data[, r_ID])), .combine=rbind) %do% {
				eval_judge(i = i, criterion = criterion, judgment = judgment, cues = cues, criterion_hat = criterion_hat,
									 r_ID = r_ID, r_misc = r_misc, std = std, method = method, method_args = method_args,
									 pred_type = pred_type, step = step, step_args = step_args, data = data)
			}
			
		}
		
		
		colnames(ind_data)[1] = r_ID
		
		
	}
	
	
	
	
	result = list(ind_data = ind_data, env_data = env_data,
								env_model = as.data.frame(cbind(env_coef, env_model_fit)),
								env_fit = env_fit,
								cues = cues
	)
	
	class(result) = "lens"
	
	result
	
}


#eval_judge is an ancillary function to do respondent level analyses with %dopar%
#'Code outline
#'for a single r_ID:
#'    subset data
#'    save r_misc vars
#'    run model
#'    step() model if needed
#'    save coefs
#'    save G, accuracy, C, model fit info
#'    return ind_data as a one row data.frame

eval_judge = function(i, criterion, judgment, cues, r_ID, r_misc,
											std, method, method_args, pred_type, step, step_args, data, criterion_hat, ...){
	
	r_ID_scalar = unique( data[, r_ID] )[i]
	
	ind_data = data.frame(r_ID = r_ID_scalar, stringsAsFactors=F)
	
	temp = data[which(data[, r_ID] == r_ID_scalar),  ]
	
	
	#save r_misc vars
	for(a in 1:length(r_misc)){
		ind_data[1, r_misc[a]] = temp[1, r_misc[a]]
	}
	
	
	# if UNstandardized coefs are requested
	if(std == "none") {
		
		#make formula
		form = as.formula(paste(judgment, "~", paste(cues, collapse = "+")))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(cues)){
			
			ind_data[1, paste0("b_", cues[a]) ] = tidy[tidy$term == cues[a], "estimate" ]
			ind_data[1, paste0("se_", cues[a]) ] = tidy[tidy$term == cues[a], "std.error" ]
			
		}
		
	}
	
	
	# if standardized coefs are requested
	if(std == "both") {
		
		#make formula
		form = as.formula(paste("scale(", judgment, ") ~",
														"scale(", paste(cues, collapse = ")+scale("), ")"  ))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(cues)){
			
			term = paste0( "scale(", cues[a], ")" )
			
			if(term %in% tidy$term){
				ind_data[1, paste0("B_", cues[a])   ] = tidy[tidy$term == term, "estimate" ]
				ind_data[1, paste0("se_", cues[a])   ] = tidy[tidy$term == term, "std.error" ]
			} else{
				ind_data[1, paste0("B_", cues[a])   ] = NA
				ind_data[1, paste0("se_", cues[a])   ] = NA
			}
			
		}
	}
	
	
	# if X standardized coefs are requested
	if(std == "x") {
		
		#make formula
		form = as.formula(paste(judgment, "~",
														"scale(", paste(cues, collapse = ")+scale("), ")"  ))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(cues)){
			
			ind_data[1, paste0("B_", cues[a])   ] = tidy[tidy$term == paste0( "scale(", cues[a], ")" ), "estimate" ]
			ind_data[1, paste0("se_", cues[a])   ] = tidy[tidy$term == paste0( "scale(", cues[a], ")" ), "std.error" ]
			
		}
	}
	
	
	# if Y standardized coefs are requested
	if(std == "y") {
		
		#make formula
		form = as.formula(paste("scale(", judgment, ") ~",
														paste(cues, collapse = "+")))
		
		fit = do.call("method", c(list(formula = form, data = temp), method_args))
		
		if(step){
			fit = do.call("step", c(list(object = fit, trace = F), step_args))
		}
		
		tidy = broom::tidy(fit)
		
		ind_data$intercept = as.numeric(unname(tidy[tidy$term == "(Intercept)", "estimate" ]))
		
		for( a in 1:length(cues)){
			
			ind_data[1, paste0("b_", cues[a]) ] = tidy[tidy$term == cues[a], "estimate" ]
			ind_data[1, paste0("se_", cues[a]) ] = tidy[tidy$term == cues[a], "std.error" ]
			
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












# test area ------------------------------------------------
# 
# 
# judgements_imp = read.csv("Cleaned Data/merged_NFP_FOP_data_long_judgements_and_cereal_imputed.csv")
# 
# 
# #healthy and nutrition are sufficiently correlated to combine them
# judgements_imp$judgment = rowMeans(cbind( judgements_imp$healthy, judgements_imp$nutritious ), na.rm = T)
# 
# 
# # remove one cereal without nuval score
# judgements_imp = judgements_imp[!is.na(judgements_imp$NuValScore), ]
# 
# 
# 
# 
# #healthy and nutrition are sufficiently correlated to combine them
# judgements_imp$judgment = rowMeans(cbind( judgements_imp$healthy, judgements_imp$nutritious ), na.rm = T)
# 
# 
# 
# 
# 
# lensModel_test = function(criterion, judgment, cues, r_ID, r_misc,
# 													s_ID, s_misc, std = c("both", "x", "y", "none"),
# 													method = glm, method_args = list(),
# 													step = F, step_args = list(k = 2, direction = "both", scope = .~.),
# 													pred_type = "response",
# 													parallel = F, cores = 3, data){
# 	
# 	
# 	
# 	std = match.arg(std, c("both", "x", "y", "none"))
# 	
# 	
# 	
# 	data = as.data.frame(data)
# 	
# 	
# 	#environment analyses
# 	{
# 		env_data = data.frame(s_ID = unique(data[, s_ID]))
# 		
# 		temp = data[!duplicated(data[, s_ID]), ]
# 		
# 		
# 		# if UNstandardized coefs are requested
# 		if(std == "none"){
# 			
# 			#make formula
# 			form = as.formula(paste(criterion, "~", paste(cues, collapse = "+")))
# 			
# 			#fit environment model
# 			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
# 			
# 			
# 			if(step){
# 				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
# 			}
# 			
# 			#get coefficients
# 			tidy = broom::tidy(env_fit)
# 			
# 			#put coefficients in a data.frame
# 			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
# 			
# 			for( a in 1:length(cues)){
# 				env_coef[1, paste0("b_", cues[a]) ] = tidy[tidy$term == cues[a], "estimate" ]
# 				env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == cues[a], "std.error" ]
# 			}
# 			
# 			rownames(env_coef) = NULL
# 			
# 			#save predicted values and residuals
# 			criterion_hat = paste0(criterion, "_hat")
# 			data[, criterion_hat] = predict(env_fit, type = pred_type, newdata = data)
# 			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
# 			
# 			
# 		}
# 		
# 		# if standardized coefs are requested
# 		if(std == "both") {
# 			
# 			#make formula
# 			form = as.formula(paste("scale(", criterion, ") ~",
# 															"scale(", paste(cues, collapse = ")+scale("), ")"  ))
# 			
# 			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
# 			
# 			if(step){
# 				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
# 			}
# 			
# 			tidy = broom::tidy(env_fit)
# 			
# 			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
# 			
# 			for( a in 1:length(cues)){
# 				
# 				term = paste0( "scale(", cues[a], ")" )
# 				
# 				if(term %in% tidy$term){
# 					env_coef[1, paste0("B_", cues[a]) ] = tidy[tidy$term == term, "estimate" ]
# 					env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == term, "std.error" ]
# 				} else{
# 					env_coef[1, paste0("B_", cues[a]) ] = NA
# 					env_coef[1, paste0("se_", cues[a]) ] = NA
# 				}
# 				
# 			}
# 			
# 			rownames(env_coef) = NULL
# 			
# 			
# 			criterion_hat = paste0(criterion, "_hat")
# 			data[, criterion_hat] = (as.numeric(predict(env_fit, type = pred_type, newdata = data)) * sd(as.numeric(data[, criterion]))) +
# 				mean(as.numeric(data[, criterion]))
# 			#to undo the standardization/scaling
# 			
# 			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
# 			
# 			
# 		}
# 		
# 		
# 		# if X standardized coefs are requested
# 		if(std == "x") {
# 			
# 			#make formula
# 			form = as.formula(paste(criterion, "~",
# 															"scale(", paste(cues, collapse = ")+scale("), ")"  ))
# 			
# 			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
# 			
# 			if(step){
# 				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
# 			}
# 			
# 			tidy = broom::tidy(env_fit)
# 			
# 			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
# 			
# 			for( a in 1:length(cues)){
# 				
# 				env_coef[1, paste0("B_", cues[a]) ] = tidy[tidy$term == paste0( "scale(", cues[a], ")" ), "estimate" ]
# 				env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == paste0( "scale(", cues[a], ")" ), "std.error" ]
# 				
# 			}
# 			
# 			rownames(env_coef) = NULL
# 			
# 			
# 			criterion_hat = paste0(criterion, "_hat")
# 			data[, criterion_hat] = predict(env_fit, type = pred_type, newdata = data)
# 			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
# 			
# 			
# 		}
# 		
# 		
# 		# if Y standardized coefs are requested
# 		if(std == "y") {
# 			
# 			#make formula
# 			form = as.formula(paste("scale(", criterion, ") ~",
# 															paste(cues, collapse = "+")))
# 			
# 			env_fit = do.call("method", c(list(formula = form, data = temp), method_args))
# 			
# 			if(step){
# 				env_fit = do.call("step", c(list(object = env_fit, trace = F), step_args))
# 			}
# 			
# 			tidy = broom::tidy(env_fit)
# 			
# 			env_coef = data.frame(intercept = unname(tidy[tidy$term == "(Intercept)", "estimate" ]) )
# 			
# 			for( a in 1:length(cues)){
# 				
# 				env_coef[1, paste0("b_", cues[a]) ] = tidy[tidy$term == cues[a], "estimate" ]
# 				env_coef[1, paste0("se_", cues[a]) ] = tidy[tidy$term == cues[a], "std.error" ]
# 				
# 			}
# 			
# 			rownames(env_coef) = NULL
# 			
# 			
# 			criterion_hat = paste0(criterion, "_hat")
# 			data[, criterion_hat] = (as.numeric(predict(env_fit, type = pred_type, newdata = data)) * sd(as.numeric(data[, criterion]))) +
# 				mean(as.numeric(data[, criterion]))
# 			#to undo the standardization/scaling
# 			
# 			data$env_resid = as.numeric(data[, criterion]) - as.numeric(data[, criterion_hat])
# 			
# 		}
# 		
# 		
# 		
# 		
# 		env_model_fit = as.data.frame(broom::glance(env_fit))
# 		env_model_fit$r_squared = cor( as.numeric(data[, criterion]), as.numeric(data[, criterion_hat]) )^2
# 		
# 		cue_terms = as.character(form)[3]
# 		cue_terms = unlist(strsplit(cue_terms, " + ", fixed = T))
# 		
# 		p = sum(cue_terms %in% tidy$term)+1
# 		n = nobs(env_fit)
# 		
# 		#from summary.lm ans$adj.r.squared <-   1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
# 		env_model_fit$r_squared_adj = 1 - (1 - env_model_fit$r_squared) * ((n-1)/(n-p)) #adjusted R2
# 		
# 		
# 		
# 		#save s_misc vars
# 		for(i in 1:nrow(env_data)){
# 			
# 			temp = data[which(data[, s_ID] == env_data[i, "s_ID"] ),  ]
# 			
# 			for(a in 1:length(s_misc)){
# 				
# 				env_data[i, s_misc[a]] = temp[1, s_misc[a]]
# 				env_data[i, criterion_hat] = temp[1, criterion_hat]
# 				env_data[i, "residual"] = temp[1, "env_resid"]
# 			}
# 			
# 			
# 		}
# 		
# 		
# 		
# 		colnames(env_data)[1] = s_ID
# 		
# 	}
# 	
# 	
# 	
# 	
# 	
# 	#respondent level analyses
# 	{
# 		
# 		if(parallel){
# 			
# 			cores = as.integer(cores)
# 			
# 			if(is.null(cores) | is.na(cores)){
# 				doParallel::registerDoParallel()
# 			} else{
# 				doParallel::registerDoParallel(cores = cores)
# 			}
# 			
# 			ind_data = foreach::foreach(i = 1:length(unique(data[, r_ID])), .combine=rbind) %dopar% {
# 				eval_judge(i = i, criterion = criterion, judgment = judgment, cues = cues, criterion_hat = criterion_hat,
# 									 r_ID = r_ID, r_misc = r_misc, std = std, method = method, method_args = method_args,
# 									 pred_type = pred_type, step = step, step_args = step_args, data = data)
# 			}
# 			
# 			doParallel::stopImplicitCluster()
# 			
# 		} else{
# 			
# 			ind_data = foreach(i = 1:length(unique(data[, r_ID])), .combine=rbind) %do% {
# 				eval_judge(i = i, criterion = criterion, judgment = judgment, cues = cues, criterion_hat = criterion_hat,
# 									 r_ID = r_ID, r_misc = r_misc, std = std, method = method, method_args = method_args,
# 									 pred_type = pred_type, step = step, step_args = step_args, data = data)
# 			}
# 			
# 		}
# 		
# 		
# 		colnames(ind_data)[1] = r_ID
# 		
# 		
# 	}
# 	
# 	
# 	
# 	
# 	result = list(ind_data = ind_data, env_data = env_data,
# 								env_model = as.data.frame(cbind(env_coef, env_model_fit)),
# 								env_fit = env_fit,
# 								cues = cues
# 	)
# 	
# 	class(result) = "lens"
# 	
# 	result
# 	
# }
# 
# 
# 
# 
# 
# library(ordinal)
# 
# judgements_imp$NuValScore_ord = factor(judgements_imp$NuValScore, 
# 																			 levels = 0:100, 
# 																			 ordered = T)
# 
# judgements_imp$judgment_ord = factor(round(judgements_imp$judgment), 
# 																		 levels = 0:100,
# 																		 ordered = T)
# 
# 
# lens_clm = lensModel_test(criterion = "NuValScore_ord", judgment = "judgment_ord", 
# 										 cues = c('Calories', 'Calories_Fat', 'Total_Fatg', 'Saturated_Fatg', 
# 										 				 'Polyunsaturated_Fatg', 'Monounsaturated_Fatg',
# 										 				 'Sodiummg', 'Potassiummg', 'Total_Carbohydratesg', 
# 										 				 'Dietary_Fiberg', 'Soluble_Fiberg', 'Insoluble_Fiberg', 'Sugars_g',
# 										 				 'Other_Carbohydratesg', 'Proteing', 'Vitamins_Minerals'),
# 										 r_ID = "ResponseId",
# 										 r_misc = 'condition', 
# 										 s_ID = "cereal_master",
# 										 s_misc = c('cereal_master', 'cereal_NFP', 'cereal_FOP', 'UPC',
# 										 					 'Brand', 'Product', 'NuValScore') , 
# 										 data = judgements_imp, parallel = F, step = F,
# 										 method = clm, pred_type = "class",
# 										 std = "x"
# )
# 
# 
# 
# criterion = "NuValScore_ord"; judgment = "judgment_ord"
# cues = c('Calories', 'Calories_Fat', 'Total_Fatg', 'Saturated_Fatg', 
# 				 'Polyunsaturated_Fatg', 'Monounsaturated_Fatg',
# 				 'Sodiummg', 'Potassiummg', 'Total_Carbohydratesg', 
# 				 'Dietary_Fiberg', 'Soluble_Fiberg', 'Insoluble_Fiberg', 'Sugars_g',
# 				 'Other_Carbohydratesg', 'Proteing', 'Vitamins_Minerals')
# r_ID = "ResponseId"
# r_misc = 'condition'
# s_ID = "cereal_master"
# s_misc = c('cereal_master', 'cereal_NFP', 'cereal_FOP', 'UPC',
# 					 'Brand', 'Product', 'NuValScore')
# data = judgements_imp; parallel = T; step = F;
# method = clm; pred_type = "class";
# std = "x"
# method_args = list()
# 
# step_args = list(k = 2, direction = "both", scope = .~.)







# 
# test = lensModel_test(criterion = "NuValScore", judgment = "judgment",
# 											cues = c('Calories', 'Calories_Fat', 'Total_Fatg', 'Saturated_Fatg',
# 															 'Polyunsaturated_Fatg', 'Monounsaturated_Fatg',
# 															 'Sodiummg', 'Potassiummg', 'Total_Carbohydratesg',
# 															 'Dietary_Fiberg', 'Soluble_Fiberg', 'Insoluble_Fiberg', 'Sugars_g',
# 															 'Other_Carbohydratesg', 'Proteing', 'Vitamins_Minerals'),
# 											r_ID = "ResponseId",
# 											r_misc = 'condition',
# 											s_ID = "cereal_master",
# 											s_misc = c('cereal_master', 'cereal_NFP', 'cereal_FOP', 'UPC',
# 																 'Brand', 'Product', 'NuValScore'),
# 											data = judgements_imp,
# 											std = "both", parallel = T, method = glm, step = T, step_args = c(k = 2)
# )









#plotting function would be really nice
{
	
}






