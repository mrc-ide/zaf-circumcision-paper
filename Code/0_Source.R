##########################################################################################
### This file contains all source code needed to run the models used in the  the project 'zaf-circumcision-rates' ###
##########################################################################################
### Functions:                                                                         ###
### * create.hazard.matrix.agetime - A function to create a matrix selecting the inst- ###
###        antaneous hazard rate needed for survival analysis by age and time          ###
### * create.icar.prec.matrix - A function to create the precision matrix for an ICAR  ###
###        process                                                                     ###
### * create.integration.matrix.agetime - A function to create a matrix to estimate    ###
###        the cumulative hazard rate needed for survival analysis by age and time     ###
### * create.integration.matrix.agetime.lag - A function to create a matrix to estima- ###
###        te the lagged cumulative hazard rate needed for survival analysis by age    ###
###        and time                                                                    ###
### * create.rw.prec.matrix- A function to create the precision matrix for RWp process ###
##########################################################################################

#################################
### Loading required packages ###
#################################
library(boot)
library(doParallel)
library(Matrix)
library(naomi)
library(parallel)
library(plyr)
library(readr)
library(sf)
library(spdep)
library(tidyverse)
library(TMB)

################################################################
### A function to create a matrix selecting the instantaneous###
### hazard rate needed for survival analysis by age and time ###
################################################################
### Inputs: dat - Dataset used for modelling                 ###
###         subset - Subset for dataset                      ###
###         NCluster - Number of cores to distribute work to ###
###         time1 - Variable name for time of birth          ###
###         time2 - Variable name for time circ'd/censor     ###
###         timecaps - Window to fix temporal dimension bef- ###
###                    -ore and after                        ###
###         Ntime - Number of time points (if NULL, function ###
###                 will calculate)                          ###
###         age - Variable with age of circumcision/censor   ###
###         Nage - Number of age groups (if NULL, function   ###
###                will calculate)                           ###
###         strat - Variable to stratify by is using a 3D    ###
###                 hazard function                          ###
###         Nstrat - Number of stratifiction vars (if NULL,  ###
###                  function will calculate)                ###
###         circ - Variables with circumcision matrix        ###
### Outputs: Matrix for selecting instantaneous hazard rate  ###
################################################################
create.hazard.matrix.agetime <- function(dat,
                                         NCluster = detectCores(),
                                         subset = NULL, 
                                         time1 = 'time1',
                                         time2 = 'time2',
                                         age = 'age',
                                         timecaps = c(1, Inf),
                                         Ntime = NULL,
                                         Nage = NULL,
                                         Nstrat = NULL,
                                         strat = NULL,
                                         circ = 'circ'){
  
  # Opening cluster 
  cl <- makePSOCKcluster(NCluster)
  # Exporting relevant variables to each cluster 
  clusterExport(cl = cl, 
                varlist = list("age", "time1", "time2","timecaps"), 
                envir = environment())
  # Integration matrix for cumululative hazard
  dat$time1_cap <- unlist(parApply(cl = cl,
                                   X = dat, 
                                   MARGIN = 1, 
                                   FUN = function(x){min(timecaps[2] - timecaps[1] + 1, max(1, as.numeric(x[time1]) - timecaps[1] + 1))}))
  # Integration matrix for cumululative hazard
  dat$time2_cap <- unlist(parApply(cl = cl,
                                   X = dat, 
                                   MARGIN = 1, 
                                   FUN = function(x){min(timecaps[2] - timecaps[1] + 1, max(1, as.numeric(x[time2]) - timecaps[1] + 1))}))
  # Closing cluster
  stopCluster(cl)
  # Number of dimensions in the hazard function
  if (is.null(Ntime)) {Ntime <- max(dat[,'time1_cap', drop = TRUE])}
  if (is.null(Nage)) {Nage <- max(dat[age])}
  if (is.null(strat) == FALSE & is.null(Nstrat)) {Nstrat <- max(dat[strat])}
  # Subsetting data if necessary 
  if (is.null(subset) == FALSE){
    dat <- subset(dat, eval(parse(text=subset)))
  }
  # Matrix for 2D age time hazard function if strat is NULL
  if (is.null(strat) == TRUE) {
    # Opening cluster 
    cl <- makePSOCKcluster(NCluster)
    # Exporting relevant variables to each cluster 
    clusterExport(cl = cl, 
                  varlist = list("Nage", "Ntime"), 
                  envir = environment())
    # Integration matrix for cumululative hazard
    cols <- unlist(parApply(cl = cl,
                            X = dat, 
                            MARGIN = 1, 
                            FUN = function(x){Ntime * (as.numeric(x[age]) - 1) + as.numeric(x['time2_cap'])}))
    # Closing cluster
    stopCluster(cl)
    # Matrix dimension 
    ncol <- Ntime * Nage
  }
  # Matrix for 3D hazard function if strat not NULL
  if (is.null(strat) == FALSE) {
    # Opening cluster 
    cl <- makePSOCKcluster(NCluster)
    # Exporting relevant variables to each cluster 
    clusterExport(cl = cl, 
                  varlist = list("Nage", "Nstrat", "Ntime", "strat"), 
                  envir = environment())
    # Integration matrix for cumululative hazard
    cols <- unlist(parApply(cl = cl,
                            X = dat, 
                            MARGIN = 1, 
                            FUN = function(x){Ntime * Nage * (as.numeric(x[strat]) - 1) + Ntime * (as.numeric(x[age]) - 1) + as.numeric(x['time2_cap'])}))
    # Closing cluster
    stopCluster(cl)
    # Matrix dimension 
    ncol <- Ntime * Nage * Nstrat
  }
  # Outputting sparse matrix 
  A <- sparseMatrix(i = 1:nrow(dat), 
                    j = cols, 
                    x = dat[,circ,drop=TRUE], 
                    dims = c(nrow(dat), ncol))
  # Returning matrix
  return(A)
}

################################################################
### A function to create a matrix to estimate the cumulative ###
### hazard rate needed for survival analysis by age and time ###
################################################################
### Inputs: dat - Dataset used for modelling                 ###
###         NCluster - Number of cores to distribute work to ###
###         time1 - Variable name for time of birth          ###
###         time2 - Variable name for time circ'd/censor     ###
###         timecaps - Window to fix temporal dimension bef- ###
###                    -ore and after                        ###
###         Ntime - Number of time points (if NULL, function ###
###                 will calculate)                          ###
###         age - Variable with age of circumcision/censor   ###
###         Nage - Number of age groups (if NULL, function   ###
###                will calculate)                           ###
###         strat - Variable to stratify by is using a 3D    ###
###                 hazard function                          ###
###         Nstrat - Number of stratifiction vars (if NULL,  ###
###                  function will calculate)                ###
### Outputs: Matrix for selecting instantaneous hazard rate  ###
################################################################
create.integration.matrix.agetime <- function(dat,
                                              subset = NULL,
                                              NCluster = detectCores(),
                                              time1 = 'time1',
                                              time2 = 'time2',
                                              timecaps = c(1,Inf),
                                              Ntime = NULL,
                                              Nage = NULL,
                                              Nstrat = NULL,
                                              age = 'age',
                                              strat = NULL){
  # Opening cluster 
  cl <- makePSOCKcluster(NCluster)
  # Exporting relevant variables to each cluster 
  clusterExport(cl = cl, 
                varlist = list("age", "time1", "time2","timecaps"), 
                envir = environment())
  # Integration matrix for cumululative hazard
  dat$time1_cap <- unlist(parApply(cl = cl,
                                   X = dat, 
                                   MARGIN = 1, 
                                   FUN = function(x){min(timecaps[2] - timecaps[1] + 1, max(1, as.numeric(x[time1]) - timecaps[1] + 1))}))
  # Integration matrix for cumululative hazard
  dat$time2_cap <- unlist(parApply(cl = cl,
                                   X = dat, 
                                   MARGIN = 1, 
                                   FUN = function(x){min(timecaps[2] - timecaps[1] + 1, max(1, as.numeric(x[time2]) - timecaps[1] + 1))}))
  # Closing cluster
  stopCluster(cl)
  # Shifting time points by teh time caps 
  dat$time1_cap2 <- dat[,time1, drop = TRUE] - timecaps[1] + 1
  dat$time2_cap2 <- dat[,time2, drop = TRUE] - timecaps[1] + 1
  # Number of dimensions in the hazard function
  if (is.null(Ntime)) {Ntime <- max(dat[,'time1_cap', drop = TRUE])}
  if (is.null(Nage)) {Nage <- max(dat[age])}
  if (is.null(strat) == FALSE & is.null(Nstrat)) {Nstrat <- max(dat[strat])}
  # Subsetting data if necessary 
  if (is.null(subset) == FALSE){
    dat <- subset(dat, eval(parse(text=subset)))
  }
  # Adding dummy variabel for the rows of the matrix 
  dat$row <- 1:nrow(dat)
  # Matrix for 3D hazard function if strat not NULL
  if (is.null(strat) == TRUE) {
    # Opening cluster 
    cl <- makePSOCKcluster(NCluster)
    # Exporting relevant variables to each cluster 
    clusterExport(cl = cl, 
                  varlist = list("Nage", "Ntime", "age", "time1", "time2", "timecaps"), 
                  envir = environment())
    # column entries for integration matrix
    cols <- unlist(parApply(cl = cl,
                            X = dat, 
                            MARGIN = 1, 
                            FUN = function(x){
                              # If circumcised at birth select relevant entry 
                              if (as.numeric(x['time1_cap2']) == (as.numeric(x['time2_cap2']))){
                                min(timecaps[2] - timecaps[1] + 1, 
                                    max(1, as.numeric(x['time1_cap2'])))
                              }
                              # Else just estimate the 
                              else{
                                cumsum(c(max(1, as.numeric(x['time1_cap2'])),
                                         Ntime + (as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) > 0 &
                                                    as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) <= timecaps[2] - timecaps[1])))
                              }
                            }))
    # Row entries for integration matrix 
    rows <- unlist(parApply(cl = cl,
                            X = dat, 
                            MARGIN = 1, 
                            FUN = function(x){rep(as.numeric(x['row']), as.numeric(x[time2]) - as.numeric(x[time1]) + 1)}))
    # Closing cluster
    stopCluster(cl)
    # Matrix dimension 
    ncol <- Ntime * Nage 
  }
  # Matrix for 3D hazard function if strat not NULL
  if (is.null(strat) == FALSE) {
    # Opening cluster 
    cl <- makePSOCKcluster(NCluster)
    # Exporting relevant variables to each cluster 
    clusterExport(cl = cl, 
                  varlist = list("Nage", "Ntime", "Nstrat", "age", "strat", "time1", "time2", "timecaps"), 
                  envir = environment())
    # column entries for integration matrix
    cols <- unlist(parApply(cl = cl,
                            X = dat, 
                            MARGIN = 1, 
                            FUN = function(x){
                              # If circumcised at birth select relevant entry 
                              if (as.numeric(x['time1_cap2']) == (as.numeric(x['time2_cap2']))){
                                Ntime * Nage * (as.numeric(x[strat]) - 1) +
                                  min(timecaps[2] - timecaps[1] + 1, 
                                      max(1, as.numeric(x['time1_cap2'])))
                              }
                              # Else just estimate the 
                              else{
                                cumsum(c(Ntime * Nage * (as.numeric(x[strat]) - 1) + max(1, as.numeric(x['time1_cap2'])),
                                         Ntime + (as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) > 0 &
                                                    as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) <= timecaps[2] - timecaps[1])))
                              }
                            }))
    # Row entries for integration matrix 
    rows <- unlist(parApply(cl = cl,
                            X = dat, 
                            MARGIN = 1, 
                            FUN = function(x){rep(as.numeric(x['row']), as.numeric(x[time2]) - as.numeric(x[time1]) + 1)}))
    # Closing cluster
    stopCluster(cl)
    # Matrix dimension 
    ncol <- Ntime * Nage * Nstrat
  }
  # Outputting sparse matrix
  A <- sparseMatrix(i = rows,
                    j = cols,
                    x = 1,
                    dims = c(nrow(dat), ncol))
  # Returning matrix
  return(A)
}

################################################################
### A function to create a matrix to estimate the lagged     ###
### cumulative hazard rate needed for survival analysis by   ###
### age and time                                             ###
################################################################
### Inputs: dat - Dataset used for modelling                 ###
###         NCluster - Number of cores to distribute work to ###
###         time1 - Variable name for time of birth          ###
###         time2 - Variable name for time circ'd/censor     ###
###         timecaps - Window to fix temporal dimension bef- ###
###                    -ore and after                        ###
###         Ntime - Number of time points (if NULL, function ###
###                 will calculate)                          ###
###         age - Variable with age of circumcision/censor   ###
###         Nage - Number of age groups (if NULL, function   ###
###                will calculate)                           ###
###         strat - Variable to stratify by is using a 3D    ###
###                 hazard function                          ###
###         Nstrat - Number of stratifiction vars (if NULL,  ###
###                  function will calculate)                ###
### Outputs: Matrix for selecting instantaneous hazard rate  ###
################################################################
create.integration.matrix.agetime.lag <- function(dat,
                                              subset = NULL,
                                              NCluster = detectCores(),
                                              time1 = 'time1',
                                              time2 = 'time2',
                                              timecaps = c(1,Inf),
                                              Ntime = NULL,
                                              Nage = NULL,
                                              Nstrat = NULL,
                                              age = 'age',
                                              strat = NULL){
  # Opening cluster 
  cl <- makePSOCKcluster(NCluster)
  # Exporting relevant variables to each cluster 
  clusterExport(cl = cl, 
                varlist = list("age", "time1", "time2","timecaps"), 
                envir = environment())
  # Integration matrix for cumululative hazard
  dat$time1_cap <- unlist(parApply(cl = cl,
                                   X = dat, 
                                   MARGIN = 1, 
                                   FUN = function(x){min(timecaps[2] - timecaps[1] + 1, max(1, as.numeric(x[time1]) - timecaps[1] + 1))}))
  # Integration matrix for cumululative hazard
  dat$time2_cap <- unlist(parApply(cl = cl,
                                   X = dat, 
                                   MARGIN = 1, 
                                   FUN = function(x){min(timecaps[2] - timecaps[1] + 1, max(1, as.numeric(x[time2]) - timecaps[1] + 1))}))
  # Closing cluster
  stopCluster(cl)
  # Shifting time points by teh time caps 
  dat$time1_cap2 <- dat[,time1, drop = TRUE] - timecaps[1] + 1
  dat$time2_cap2 <- dat[,time2, drop = TRUE] - timecaps[1] + 1
  # Number of dimensions in the hazard function
  if (is.null(Ntime)) {Ntime <- max(dat[,'time1_cap', drop = TRUE])}
  if (is.null(Nage)) {Nage <- max(dat[age])}
  if (is.null(strat) == FALSE & is.null(Nstrat)) {Nstrat <- max(dat[strat])}
  # Subsetting data if necessary 
  if (is.null(subset) == FALSE){
    dat <- subset(dat, eval(parse(text=subset)))
  }  
  # Nnumber of rows in the resulting matrix
  nrow <- nrow(dat)
  # Adding dummy variabel for the rows of the matrix 
  dat$row <- 1:nrow(dat)
  # Matrix for 3D hazard function if strat not NULL
  if (is.null(strat) == TRUE) {
    # Opening cluster 
    cl <- makePSOCKcluster(NCluster)
    # Exporting relevant variables to each cluster 
    clusterExport(cl = cl, 
                  varlist = list("Nage", "Ntime", "age", "time1", "time2", "timecaps"), 
                  envir = environment())
    # column entries for integration matrix
    cols <- unlist(parApply(cl = cl,
                            X = dat,
                            MARGIN = 1,
                            FUN = function(x){
                              # If circumcised at birth select relevant entry 
                              if (as.numeric(x['time1_cap2']) == (as.numeric(x['time2_cap2']))){
                                test <- min(timecaps[2] - timecaps[1] + 1, 
                                            max(1, as.numeric(x['time1_cap2'])))
                              }
                              # Else just estimate the 
                              else{
                                test <- cumsum(c(max(1, as.numeric(x['time1_cap2'])),
                                                 Ntime + (as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) > 0 &
                                                            as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) <= timecaps[2] - timecaps[1])))
                              }
                              test <- test[-length(test)]
                              return(test)
                            }))
    # Row entries for integration matrix
    rows <- unlist(parApply(cl = cl,
                     X = dat,
                     MARGIN = 1,
                     FUN = function(x){rep(as.numeric(x['row']), as.numeric(x[time2]) - as.numeric(x[time1]))}))
    # Closing cluster
    stopCluster(cl)
    # Matrix dimension 
    ncol <- Ntime * Nage
  }
  # Matrix for 3D hazard function if strat not NULL
  if (is.null(strat) == FALSE) {
    # Opening cluster
    cl <- makePSOCKcluster(NCluster)
    # Exporting relevant variables to each cluster
    clusterExport(cl = cl,
                  varlist = list("Nage", "Ntime", "Nstrat", "age", "strat", "time1", "time2", "timecaps"),
                  envir = environment())
    # column entries for integration matrix
    cols <- unlist(parApply(cl = cl,
                            X = dat,
                            MARGIN = 1,
                            FUN = function(x){
                              # If circumcised at birth select relevant entry
                              if (as.numeric(x['time1_cap2']) == (as.numeric(x['time2_cap2']))){
                                test <- Ntime * Nage * (as.numeric(x[strat]) - 1) +
                                  min(timecaps[2] - timecaps[1] + 1,
                                      max(1, as.numeric(x['time1_cap2'])))
                              }
                              # Else just estimate the
                              else{
                                test <- cumsum(c(Ntime * Nage * (as.numeric(x[strat]) - 1) + max(1, as.numeric(x['time1_cap2'])),
                                                 Ntime + (as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) > 0 &
                                                            as.numeric(x['time1_cap2']):(as.numeric(x['time2_cap2']) - 1) <= timecaps[2] - timecaps[1])))
                              }
                              test <- test[-length(test)]
                              return(test)
                            }))
    # Row entries for integration matrix
    rows <- unlist(parApply(cl = cl,
                            X = dat,
                            MARGIN = 1,
                            FUN = function(x){rep(as.numeric(x['row']), as.numeric(x[time2]) - as.numeric(x[time1]))}))
    # Closing cluster
    stopCluster(cl)
    # Matrix dimension
    ncol <- Ntime * Nage * Nstrat
  }
  # Outputting sparse matrix
  A <- sparseMatrix(i = rows,
                    j = cols,
                    x = 1,
                    dims = c(nrow, ncol))
  # Returning matrix
  return(A)
}

################################################################
### A function to create the precision matrix for RWp process###
################################################################
### Inputs: dim - Dimension of the precision matrix          ###
###         order - Order of the random walk                 ###
###         offset.diag - Option to offset diagonal by 1E-6  ###
### Outputs: RW precision matrix                             ###
################################################################
create.rw.prec.matrix <- function(dim = NULL,
                                  order = 1,
                                  offset.diag = TRUE){
  # Creating stucture matrix
  Q <- diff(diag(dim), differences = order)
  Q <- t(Q) %*% Q
  # Adding offset to diagonal if required
  if (offset.diag == TRUE){
    diag(Q) <- diag(Q) + 1E-6
    
  }
  # Converting to sparse matrix
  Q <- as(Q, "sparseMatrix")
  # Returning matrix
  return(Q)
}

################################################################
### A function to create the precision matrix for an ICAR    ###
### process                                                  ###
################################################################
### Inputs: sf_obj - Shapefiles needed for adjacency         ###
###         row.names - Unique IDs for the areas             ###
### Outputs: ICAR precision matrix                           ###
################################################################
create.icar.prec.matrix <- function(sf_obj = NULL,
                                    row.names = NULL){
  # Creating neighbourhood structure 
  Q_space <-  poly2nb(sf_obj, row.names = sf_obj[,row.names])
  # Converting to adjacency matrix
  Q_space <- nb2mat(Q_space, style = 'B')
  # Converting to sparse matrix
  Q_space <- as(Q_space, "sparseMatrix")
  # Creating precision matrix from adjacency
  Q_space <- INLA::inla.scale.model(diag(rowSums(Q_space)) - 0.99*Q_space,
                                    constr = list(A = matrix(1, 1, nrow(Q_space)), e = 0))
}