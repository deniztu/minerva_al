# Hallo ich bin eine Dana Änderung!!!

# function for the standard error of the mean

std_err <- function(x){sd(x)/sqrt(length(x))} 

# supplementary function to divide n by n_replication

divide_by_n_rep <- function(x, n_rep = n_replications){x/n_rep}

# function to find trial in which a criterion expectancy is achieved (e.g Retrieval of X|A)

find_crit_trial <- function(x){
  min(which(x >= 0.95))
}

# function to put the standard error in paranthesis next to the mean 
table_fun <- function(mean_value = NA, std.error = NA){
  paste0(round(mean_value, 2)," (", round(std.error, 2), ")")
}




