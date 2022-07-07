install.packages("readxl")


tpheroin_death    <-  0.0075   # Heroin to death
heroin_continue     <-  0.8425   # Heroin Use --> Heroin Use

tpOD1_heroin1      <-   0.878      # 1st overdose to Heroin use
tpOD1_DC2           <-  0.062        # 1st overdose to discontinue use 2
tpOD1_death      <-   0.06    # 1st overdose to death

tpOD2_heroin2     <-0.864  # 1st overdose to Heroin use
tpOD2_DC3          <-0.062    # 1st overdose to Heroin use
tpOD2_death       <-0.074  # 1st overdose to Heroin use
tpOD3_heroin2 <- 0.864 #3rd Overdose to Resume Heroin Use 2
tpOD3_DC3 <- 0.062 #3rd Overdose to Discontinue Heroin Use 3

tpheroin_OD1        <-   0.09      # Heroin to 1st overdose
tpheroin1_OD2       <-0.22  #resume heroin use 1 to 2nd overdose
tpheroin2_OD3 <-0.34 #Resume Heroin Use 2 to 3rd Overdose

tpheroin_DC1         <-  0.06     # Heroin to discontinue use 1
tpheroin1_DC2     <-0.06  #Resume heroin use 1 to discontinue use 2
tpheroin2_DC3 <- 0.06 #Resume Heroin Use 2 to discontinue heroin use 3


tpDC1_DC1 <- 0.93 #Discontinue Heroin use 1 to  Discontinue Heroin use 1
tpDC2_heroin <- 0.07 #Discontinue Heroin use 2 to Resume Heroin Use 1
tpDC2_DC2 <- 0.93 #Discontinue Heroin use 2 to Discontinue Heroin use 2
tpDC3_heroin2 <- 0.07 #Discontinue Heroin use 3 to Resume Heroin Use 2
tpDC3_DC3 <- 0.93 #Discontinue Heroin use 3 --> Discontinue Heroin use 3
tpOD3_death <- 0.074 #3rd Overdose --> Death


#raw death rate
Death_rate <- read_excel("/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Naloxone Cost Utility Analysis/Markov/markov.xlsx")

Death_rate <- as.matrix(Death_rate)

HeroinUse <- 1-tpheroin_OD1-Death_rate-tpheroin_DC1

ResumeHeroinUse1_ContinueHeroinUse <- 1-tpheroin1_OD2-tpheroin1_DC2-Death_rate

ResumeHeroinUse2_ContinueHeroinUse <- 1-tpheroin2_OD3-tpheroin2_DC3-Death_rate

n_t <- 66

n_c <- 1000

v_state_names <- c("Heroin Use", 
                   "Overdose", "Resume Heroin Use1", "2nd Overdose", 
                   "Resume Heroin Use 2", "3rd Overdose", "Discontinue Heroin Use 1", 
                   "Discontinue Heroin Use 2", "Discontinue Heroin Use 3" , "Death")

v_cycles <- 0:65
  
main_matrix <-matrix(c(n_c, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                       nrow= n_t, ncol= 10, byrow=TRUE,
                       dimnames = list( v_cycles,
                                       to = v_state_names))


for (i in 2:n_t) {
  main_matrix [i,1] <- main_matrix [i-1,1]*heroin_continue + tpheroin_DC1*main_matrix [i-1,7] #Heroin Use 
  
  main_matrix [i,2] <- tpheroin_OD1*main_matrix [i-1,1] #Overdose
  
  main_matrix [i,3] <- tpOD1_heroin1*main_matrix [i-1,2] + ResumeHeroinUse1_ContinueHeroinUse [i] *main_matrix [i-1,3]+tpDC2_heroin*main_matrix [i-1,8] #Resume Heroin Use 1
  
  main_matrix [i,4] <- tpheroin1_OD2*main_matrix [i-1,3] #2nd Overdose
  
  main_matrix [i,5] <- tpOD2_heroin2*main_matrix [i-1,4]+main_matrix [i-1,9]*main_matrix [i-1,4]+tpOD3_heroin2*main_matrix [i-1,6]+tpDC3_heroin2*main_matrix [i-1,9] #Resume Heroin Use 2
  
  main_matrix [i,6] <- tpheroin2_OD3*main_matrix [i-1,5] #3rd Overdose
  
  main_matrix [i,7] <- tpheroin_DC1*main_matrix [i-1,1]+tpDC1_DC1*main_matrix [i-1,7] #Discontinue Heroin Use 1
  
  main_matrix [i,8] <- tpOD1_DC2*main_matrix [i-1,2]+tpheroin1_DC2*main_matrix [i-1,3]+tpDC2_DC2*main_matrix [i-1,8] #Discontinue Heroin Use 2
  
  main_matrix [i,9] <- tpOD2_DC3*main_matrix [i-1,4]+tpheroin2_DC3*main_matrix [i-1,5]+tpOD3_DC3*main_matrix [i-1,6]+tpDC3_DC3*main_matrix [i-1,9] #Discontinue Heroin Use 3
  
 main_matrix [i,10] <- Death_rate[i]*main_matrix [i-1,1]
 +tpOD1_death*main_matrix [i-1,2]
 +Death_rate[i]*main_matrix [i-1,3]
 +tpOD2_death*main_matrix [i-1,4]
 +Death_rate[i]*main_matrix [i-1,5]
 +tpOD3_death*main_matrix [i-1,6]+main_matrix [i-1,10]
  }




