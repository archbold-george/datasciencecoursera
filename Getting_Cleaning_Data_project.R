
## Section 1
# install.packages("data.table")
library("data.table")
# Reading in data
outcome <- data.table::fread('outcome-of-care-measures.csv')
outcome[, (11) := lapply(.SD, as.numeric), .SDcols = (11)]
outcome[, lapply(.SD, hist, xlab= "Deaths", main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", col="lightblue"), .SDcols = (11)]

## section 2
best <- function(state, outcome) {
  out_dt <- data.table::fread('outcome-of-care-measures.csv')
  outcome <- tolower(outcome)  
  chosen_state <- state 
  if (!chosen_state %in% unique(out_dt[["State"]])) {
    stop('invalid state')
  }  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  setnames(out_dt, tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" )))  
  out_dt <- out_dt[state == chosen_state]
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
  out_dt <- out_dt[, .SD ,.SDcols = col_indices]
  out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
  out_dt <- out_dt[complete.cases(out_dt),]
  out_dt <- out_dt[order(get(outcome), `hospital name`)]
  return(out_dt[, "hospital name"][1])
}

## section 3
rankhospital <- function(state, outcome, num = "best") {
  out_dt <- data.table::fread('outcome-of-care-measures.csv')
  outcome <- tolower(outcome)
  chosen_state <- state 
  if (!chosen_state %in% unique(out_dt[["State"]])) {stop('invalid state')}  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop('invalid outcome')}
  setnames(out_dt, tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" )))
  out_dt <- out_dt[state == chosen_state]
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
  out_dt <- out_dt[, .SD ,.SDcols = col_indices]
  out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
  out_dt <- out_dt[complete.cases(out_dt),]
  out_dt <- out_dt[order(get(outcome), `hospital name`)]
  out_dt <- out_dt[,  .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
  if (num == "best"){
    return(out_dt[1,`hospital name`])
  }
  if (num == "worst"){
    return(out_dt[.N,`hospital name`])
  }  
  return(out_dt[num,`hospital name`])
}

## section 4
rankall <- function(outcome, num = "best") {
  out_dt <- data.table::fread('outcome-of-care-measures.csv')
   outcome <- tolower(outcome)
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  setnames(out_dt, tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" )))
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
  out_dt <- out_dt[, .SD ,.SDcols = col_indices]
  out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
    if (num == "best"){
    return(out_dt[order(state, get(outcome), `hospital name`), .(hospital = head(`hospital name`, 1)), by = state])
  }
    if (num == "worst"){
    return(out_dt[order(get(outcome), `hospital name`), .(hospital = tail(`hospital name`, 1)), by = state])
  }
    return(out_dt[order(state, get(outcome), `hospital name`), head(.SD,num), by = state, .SDcols = c("hospital name") ])
                
}
