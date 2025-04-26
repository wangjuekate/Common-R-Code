
library(survival)
library(plm)
library(fixest)
library(xtable)

panel = read.table('/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Raghu_Platform/FinalPanel/Finalpanelnew.csv', sep=',',head=TRUE)



#Full sample analyses
cleardata <- function(input) {
  names <- rownames(input)
  n <- nrow(input)
  
  # Create standard errors in parentheses
  st_errors <- paste0("(", format(round(as.numeric(input[, 2]), digits=2), nsmall=2), ")")
  
  # Create coefficients with significance markers
  coefficients <- sapply(1:n, function(i) {
    p_val <- as.numeric(input[i, 4])
    coef <- format(round(as.numeric(input[i, 1]), digits=2), nsmall=2)
    
    if (is.na(p_val)) return("NA")
    if (p_val <= 0.001) return(paste0(coef, "***"))
    if (p_val <= 0.01) return(paste0(coef, "**"))
    if (p_val <= 0.05) return(paste0(coef, "*"))
    if (p_val <= 0.1) return(paste0(coef, "+"))
    return(coef)
  })
  
  # Create result matrix
  final <- matrix("", nrow=n*2, ncol=2)
  idx <- seq(1, 2*n, by=2)
  
  final[idx, 1] <- paste0(names, "")
  final[idx+1, 1] <- paste0(names, "2")
  final[idx, 2] <- coefficients
  final[idx+1, 2] <- st_errors
  
  return(final)
}

run_model <- function(results, model_number = NULL, printmodel = NULL) {
  finalresults <- summary(results)
  
  modelformat <- data.frame(rbind(
    cleardata(finalresults$coeftable),
    c("Log lik.", round(logLik(results), 3)),
    c("Obs.", finalresults$nobs)
  ))
  
  if(is.null(printmodel)) {
    return(modelformat)
  } else {
    return(merge(printmodel, modelformat, all = TRUE, by.x = 'X1', by.y = "X1"))
  }
}



controllist = "+priorapproval+logfirmage+logfirmsize+ num_RC+ lobbyist "



# Define model formulas
model_formulas <- list(
  as.formula(paste0("approvaldummy_adj ~",controllist)),
  as.formula(paste0("approvaldummy_adj ~", "+socialgroup_dummy",controllist)),
  as.formula(paste0("approvaldummy_adj ~", "+socialgroup_dummy*matchregulator",controllist)),
  as.formula(paste0("approvaldummy_adj ~", "+socialgroup_dummy*matchstakeholder",controllist)),
  as.formula(paste0("approvaldummy_adj ~", "+socialgroup_dummy*matchregulator+socialgroup_dummy*matchstakeholder",controllist))
)
all_model_results <- list()     # To store the raw model objects

all_model_results[[1]] <- femlm(model_formulas[[1]], panel, family =  "logit", cluster= 'year')
all_model_results[[2]] <- femlm(model_formulas[[2]], panel, family =  "logit")
all_model_results[[3]] <- femlm(model_formulas[[3]], panel, family =  "logit")
all_model_results[[4]] <- femlm(model_formulas[[4]],  panel, family =  "logit")
all_model_results[[5]] <- femlm(model_formulas[[5]],  panel, family =  "logit")



# Run all models sequentially
printmodel <- NULL
for(i in seq_along(all_model_results)) {
  printmodel <- run_model(all_model_results[[i]], i, printmodel)
  printmodel$label <- NULL
}

# Process final table
Final <- printmodel
Final[is.na(Final)] <- ""

# Set column names
colnames(Final) <- c("label", paste("Model", seq_len(ncol(Final)-1)))

outdir= '/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Raghu_Platform/FinalPanel'
# Write variable names
write.table(Final$label, file = file.path(outdir ,"variablenames_adj.csv"), 
            sep = ",", row.names = FALSE)

# Read variable names with display formatting
varname <- read.table( file.path(outdir , "maintable.csv"), sep = ",", header = TRUE)

# Merge with formatted variable names
printmodel <- merge(varname, Final, by.x = "label", by.y = "label", all.x = TRUE, sort = FALSE)
printmodel$label <- NULL
printmodel$x <- NULL

# Output final table

test <- xtable(printmodel)
print(test, type = "latex", 
      file = file.path(outdir ,  "maintable.tex"),
      include.rownames = FALSE,
      latex.environments = "center",
      sanitize.text.function = function(x) x)


#####The supplement


# Define model formulas
model_formulas <- list(
  as.formula(paste0("approvaldummy_adj ~", "+socialgroup_dummy",controllist)),
  as.formula(paste0("approvaldummy_adj ~", "+socialgroup_dummy",controllist))
)
all_model_results <- list()     # To store the raw model objects


all_model_results[[1]] <- femlm(model_formulas[[1]], panel[which(panel$online>0),], family =  "logit")
all_model_results[[2]] <- femlm(model_formulas[[2]],  panel[which(panel$online==0),], family =  "logit")




# Run all models sequentially
printmodel <- NULL
for(i in seq_along(all_model_results)) {
  printmodel <- run_model(all_model_results[[i]], i, printmodel)
  printmodel$label <- NULL
}

# Process final table
Final <- printmodel
Final[is.na(Final)] <- ""

# Set column names
colnames(Final) <- c("label", paste("Model", seq_len(ncol(Final)-1)))

outdir= '/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Raghu_Platform/FinalPanel'
# Write variable names
write.table(Final$label, file = file.path(outdir ,"variablenames_adj.csv"), 
            sep = ",", row.names = FALSE)

# Read variable names with display formatting
varname <- read.table( file.path(outdir , "maintable.csv"), sep = ",", header = TRUE)

# Merge with formatted variable names
printmodel <- merge(varname, Final, by.x = "label", by.y = "label", all.x = TRUE, sort = FALSE)
printmodel$label <- NULL
printmodel$x <- NULL

# Output final table

test <- xtable(printmodel)
print(test, type = "latex", 
      file = file.path(outdir ,  "onlineoffline.tex"),
      include.rownames = FALSE,
      latex.environments = "center",
      sanitize.text.function = function(x) x)



#####Split the exposure########


# Define model formulas
model_formulas <- list(
  as.formula(paste("approvaldummy_adj ~", "+word_count_rep",controllist)),
 as.formula(paste("approvaldummy_adj ~", "+peerapproval",controllist))
)
all_model_results <- list()     # To store the raw model objects


all_model_results[[1]] <- femlm(model_formulas[[1]], panel[which(panel$socialgroup_dummy>0),], family =  "logit")
all_model_results[[2]] <- femlm(model_formulas[[2]],  panel[which(panel$socialgroup_dummy>0),], family =  "logit")




# Run all models sequentially
printmodel <- NULL
for(i in seq_along(all_model_results)) {
  printmodel <- run_model(all_model_results[[i]], i, printmodel)
  printmodel$label <- NULL
}

# Process final table
Final <- printmodel
Final[is.na(Final)] <- ""

# Set column names
colnames(Final) <- c("label", paste("Model", seq_len(ncol(Final)-1)))

outdir= '/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Raghu_Platform/FinalPanel'
# Write variable names
write.table(Final$label, file = file.path(outdir ,"variablenames_adj.csv"), 
            sep = ",", row.names = FALSE)

# Read variable names with display formatting
varname <- read.table( file.path(outdir , "maintable.csv"), sep = ",", header = TRUE)

# Merge with formatted variable names
printmodel <- merge(varname, Final, by.x = "label", by.y = "label", all.x = TRUE, sort = FALSE)
printmodel$label <- NULL
printmodel$x <- NULL

# Output final table

test <- xtable(printmodel)
print(test, type = "latex", 
      file = file.path(outdir ,  "splitexposure.tex"),
      include.rownames = FALSE,
      latex.environments = "center",
      sanitize.text.function = function(x) x)
