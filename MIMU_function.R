

#Escher, Godel and Bach: An eternal golden (by Hofstadter)

#Rule 1: Add U to the end of any string ending in I
#Rule 2: Double the string after the M
#Rule 3: Replace any III with a U
#Rule 4: Remove any UU

#Function with conditions

conditions_fun <- function(x){
  
  MI_vect_last <- x
  #define output vector
  
  aux <- c()
  
  for (i in 1:length(MI_vect_last)){
    
    vect <- MI_vect_last[i]
    
    #Rule 1: Add U to the end of any string ending in I
    
    tl <- nchar(vect)
    if (substr(vect, tl, tl)  == 'I') {
      aux <-c(aux, as.vector(paste0(vect, 'U')))
    }
    
    #Rule 2: Double the string after the M
    
    for (j in 1:nchar(vect)) {
      if(substr(vect, j, j) == 'M') {
        aux <- c(aux, as.vector(paste0(vect, strsplit(vect, substr(vect, j, j))[[1]][2])))
      }
    }
    
    #Rule 3: Replace any III with a U
    
    vaux <- unlist(strsplit(vect, ""))
    for(j in 1:length(vaux)) {
      if(((!is.na(vaux[j+1]) & (!is.na(vaux[j+2]))) &
          ((vaux[j] == 'I') & (vaux[j+1] == 'I') & (vaux[j+2] == 'I')))){
        vaux_s <- vaux[-((j+1):(j+2))]
        vaux_s[j] <- 'U'
        vaux2 <- c()
        for(k in 1:length(vaux_s)){
          vaux2 <- paste0(vaux2, vaux_s[k])
        }
        aux <- c(aux, vaux2)
      }
    }
    
    #Rule 4: Remove any UU
    
    aux3 <- c()
    for(j in 1:length(vaux)) {
      
      if(!is.na(vaux[j+1]) & ((vaux[j] == 'U') & (vaux[j+1] == 'U'))){
        spt_vec <- strsplit(vect, 'UU')[[1]]
        for(n in 1:length(spt_vec)){
          aux3 <- paste0(aux3, spt_vec[n])
        }
      }
      aux <- c(aux, aux3)
    }
  }
  return(aux)
}

#Validation

conditions_fun("MI")

conditions_fun(c("MIU", "MII"))

conditions_fun(c("MIUIU", "MIIU",  "MIIII"))


#Looks fine... let's integrate in a function:


MIMU_problem <- function (input, len){
  
  output <- list()
  step <- 1
  
  if(step == 1){
    output[[step]] <- conditions_fun(input)
    step <- step + 1
    }
    
  while (!('MU' %in% output[[length(output)]]) & (step <= len)) {
    output[[step]] <- conditions_fun(output[[length(output)]])
    step <- step + 1
    }
  return(output)
}


results <- MIMU_problem('MI', 4)



