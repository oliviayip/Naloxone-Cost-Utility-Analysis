#################################################################
## Title:       Markov model for ADS and naloxone
## Programmer:  Mark Bounthavong
## Date:        28 September 2022
## Program:     R - 4.2.1
## Version:     1.1
## Updated:     29 September 2022 (in progress)
## Updated by:  Mark Bounthavong, Olivia Yip
#################################################################




#### Notes ######################################################
# This R code includes both the DA tree and the Markov mode.
# Use the output from the DA tree to feed into the Markov model
#################################################################

#### Clear environment -- Don't execute if using initial values from DA tree
rm(list = ls())


#### Load libraries
library("openxlsx")
library("tidyverse")



##################################################
#### Decision Tree ####
##################################################



##################################################
#### Vector of parameter inputs ####
##################################################
input <- data.frame(
  p.ad_nal_yes          =   0.80   ,     # Probability of naloxone prescription after AD education
  p.od_yes              =   0.850  ,     # Probability of witnessing and overdose
  p.used_yes            =   0.80   ,     # Probability of naloxone being used
  p.amb_yes             =   0.60   ,     # Probability ambulance being called
  p.live_yes            =   0.979  ,     # Probability of surviving 
  p.nal_ems_live_yes    =   0.899  ,     # Probability of surviving when naloxone is not administered
  p.nal_yes             =   0.20   ,     # Probability of naloxone prescription after no AD education
  
  c.ad                  =   80.00  ,     # Cost of academic detailing
  c.nal                 =   25.00  ,     # Cost of naloxone
  c.ems                 =   2092   ,     # Cost of EMS
  c.trans               =   352    ,     # Cost transportation to ER
  c.ed                  =   1034   ,     # Cost of emergency department services
  
  u.live                =   0.70   ,     # Utility of survival
  u.dead                =   0.00         # Utility of death
  
)



##################################################
#### Wrap decision tree into a function ####
##################################################

dec_tree <- function(params){
  with(
    as.list(params), 
    {
      
      # Expected probabilities for each pathway
      
      ### Pathways for AD (+)
      epA.1  <-  p.ad_nal_yes * p.od_yes * p.used_yes * p.amb_yes * p.live_yes                     # Expected probability for Pathway 1
      epA.2  <-  p.ad_nal_yes * p.od_yes * p.used_yes * p.amb_yes * (1 - p.live_yes)               # Expected probability for Pathway 2
      
      epA.3  <-  p.ad_nal_yes * p.od_yes * p.used_yes * (1 - p.amb_yes) * p.live_yes               # Expected probability for Pathway 3
      epA.4  <-  p.ad_nal_yes * p.od_yes * p.used_yes * (1 - p.amb_yes) * (1 - p.live_yes)         # Expected probability for Pathway 4
      
      epA.5  <-  p.ad_nal_yes * p.od_yes * (1 - p.used_yes) * p.amb_yes * p.live_yes               # Expected probability for Pathway 5
      epA.6  <-  p.ad_nal_yes * p.od_yes * (1 - p.used_yes) * p.amb_yes * (1 - p.live_yes)         # Expected probability for Pathway 6
      
      epA.7  <-  p.ad_nal_yes * p.od_yes * (1 - p.used_yes) * (1 - p.amb_yes) * p.nal_ems_live_yes            # Expected probability for Pathway 7
      epA.8  <-  p.ad_nal_yes * p.od_yes * (1 - p.used_yes) * (1 - p.amb_yes) * (1 - p.nal_ems_live_yes)      # Expected probability for Pathway 8
      
      epA.9  <-  p.ad_nal_yes * (1 - p.od_yes) * p.live_yes                                        # Expected probability for Pathway 9
      epA.10 <-  p.ad_nal_yes * (1 - p.od_yes) * (1 - p.live_yes)                                  # Expected probability for Pathway 10
      
      epA.11 <-  (1 - p.ad_nal_yes) * p.od_yes * p.amb_yes * p.live_yes                            # Expected probability for Pathway 11
      epA.12 <-  (1 - p.ad_nal_yes) * p.od_yes * p.amb_yes * (1 - p.live_yes)                      # Expected probability for Pathway 22
      
      epA.13 <-  (1 - p.ad_nal_yes) * p.od_yes * (1 - p.amb_yes) * p.nal_ems_live_yes              # Expected probability for Pathway 13
      epA.14 <-  (1 - p.ad_nal_yes) * p.od_yes * (1 - p.amb_yes) * (1 - p.nal_ems_live_yes)        # Expected probability for Pathway 14
      
      epA.15 <-  (1 - p.ad_nal_yes) * (1 - p.od_yes) * p.nal_ems_live_yes                          # Expected probability for Pathway 15
      epA.16 <-  (1 - p.ad_nal_yes) * (1 - p.od_yes) * (1 - p.nal_ems_live_yes)                    # Expected probability for Pathway 16
      
      
      ### Pathways for AD (-)
      epB.1  <-  p.nal_yes * p.od_yes * p.used_yes * p.amb_yes * p.live_yes                        # Expected probability for Pathway 1
      epB.2  <-  p.nal_yes * p.od_yes * p.used_yes * p.amb_yes * (1 - p.live_yes)                  # Expected probability for Pathway 2
      
      epB.3  <-  p.nal_yes * p.od_yes * p.used_yes * (1 - p.amb_yes) * p.live_yes                  # Expected probability for Pathway 3
      epB.4  <-  p.nal_yes * p.od_yes * p.used_yes * (1 - p.amb_yes) * (1 - p.live_yes)            # Expected probability for Pathway 4
      
      epB.5  <-  p.nal_yes * p.od_yes * (1 - p.used_yes) * p.amb_yes * p.live_yes                  # Expected probability for Pathway 5
      epB.6  <-  p.nal_yes * p.od_yes * (1 - p.used_yes) * p.amb_yes * (1 - p.live_yes)            # Expected probability for Pathway 6
      
      epB.7  <-  p.nal_yes * p.od_yes * (1 - p.used_yes) * (1 - p.amb_yes) * p.nal_ems_live_yes            # Expected probability for Pathway 7
      epB.8  <-  p.nal_yes * p.od_yes * (1 - p.used_yes) * (1 - p.amb_yes) * (1 - p.nal_ems_live_yes)      # Expected probability for Pathway 8
      
      epB.9  <-  p.nal_yes * (1 - p.od_yes) * p.live_yes                                           # Expected probability for Pathway 9
      epB.10 <-  p.nal_yes * (1 - p.od_yes) * (1 - p.live_yes)                                     # Expected probability for Pathway 10
      
      epB.11 <-  (1 - p.nal_yes) * p.od_yes * p.amb_yes * p.live_yes                               # Expected probability for Pathway 11
      epB.12 <-  (1 - p.nal_yes) * p.od_yes * p.amb_yes * (1 - p.live_yes)                         # Expected probability for Pathway 22
      
      epB.13 <-  (1 - p.nal_yes) * p.od_yes * (1 - p.amb_yes) * p.nal_ems_live_yes                 # Expected probability for Pathway 13
      epB.14 <-  (1 - p.nal_yes) * p.od_yes * (1 - p.amb_yes) * (1 - p.nal_ems_live_yes)           # Expected probability for Pathway 14
      
      epB.15 <-  (1 - p.nal_yes) * (1 - p.od_yes) * p.nal_ems_live_yes                             # Expected probability for Pathway 15
      epB.16 <-  (1 - p.nal_yes) * (1 - p.od_yes) * (1 - p.nal_ems_live_yes)                       # Expected probability for Pathway 16
      
      
      
      
      # Total costs for each pathway (unweighted)
      
      ### Total costs for Treatment A
      tcA.1   <-  c.ad + c.nal + c.ems + c.trans + c.ed
      tcA.2   <-  c.ad + c.nal + c.ems + c.trans + c.ed
      
      tcA.3   <-  c.ad + c.nal
      tcA.4   <-  c.ad + c.nal
      
      tcA.5   <-  c.ad + c.nal + c.ems + c.trans + c.ed
      tcA.6   <-  c.ad + c.nal + c.ems + c.trans + c.ed
      
      tcA.7   <-  c.ad + c.nal
      tcA.8   <-  c.ad + c.nal      
      
      tcA.9   <-  c.ad + c.nal
      tcA.10  <-  c.ad + c.nal      
      
      tcA.11  <-  c.ad + c.ems + c.trans + c.ed
      tcA.12  <-  c.ad + c.ems + c.trans + c.ed
      
      tcA.13  <-  c.ad + 0
      tcA.14  <-  c.ad + 0     
      
      tcA.15  <-  c.ad + 0
      tcA.16  <-  c.ad + 0     
      
      ### Total costs for Treatment B
      tcB.1   <-  c.nal + c.ems + c.trans + c.ed
      tcB.2   <-  c.nal + c.ems + c.trans + c.ed
      
      tcB.3   <-  c.nal
      tcB.4   <-  c.nal
      
      tcB.5   <-  c.nal + c.ems + c.trans + c.ed
      tcB.6   <-  c.nal + c.ems + c.trans + c.ed
      
      tcB.7   <-  c.nal
      tcB.8   <-  c.nal      
      
      tcB.9   <-  c.nal
      tcB.10  <-  c.nal      
      
      tcB.11  <-  c.ems + c.trans + c.ed
      tcB.12  <-  c.ems + c.trans + c.ed
      
      tcB.13  <-  0
      tcB.14  <-  0     
      
      tcB.15  <-  0
      tcB.16  <-  0        
      
      TxA.n <- epA.1 + epA.3 + epA.5 + epA.7 + epA.9 + epA.11 + epA.13 + epA.15  ### Total prob in TxA who are alive
      
      TxB.n <- epB.1 + epB.3 + epB.5 + epB.7 + epB.9 + epB.11 + epB.13 + epB.15  ### Total prob in TxB who are alive
      
      
      # Expected Total Costs for each Treatment strategy is the sum of the weighted values
      # (probabilities of each pathway multiplied by the total costs of each pathway)
      
      ### Expected Total Costs for Treatment A
      exp.tcA <- (epA.1 * tcA.1) + 
        (epA.2 * tcA.2) + 
        (epA.3 * tcA.3) + 
        (epA.4 * tcA.4) + 
        (epA.5 * tcA.5) + 
        (epA.6 * tcA.6) + 
        (epA.7 * tcA.7) + 
        (epA.8 * tcA.8) + 
        (epA.9 * tcA.9) + 
        (epA.10 * tcA.10) + 
        (epA.11 * tcA.11) + 
        (epA.12 * tcA.12) + 
        (epA.13 * tcA.13) + 
        (epA.14 * tcA.14) + 
        (epA.15 * tcA.15) + 
        (epA.16 * tcA.16) 
      
      ### Expected Total Costs for Treatment B
      exp.tcB <- (epB.1 * tcB.1) + 
        (epB.2 * tcB.2) + 
        (epB.3 * tcB.3) + 
        (epB.4 * tcB.4) + 
        (epB.5 * tcB.5) + 
        (epB.6 * tcB.6) + 
        (epB.7 * tcB.7) + 
        (epB.8 * tcB.8) + 
        (epB.9 * tcB.9) + 
        (epB.10 * tcB.10) + 
        (epB.11 * tcB.11) + 
        (epB.12 * tcB.12) + 
        (epB.13 * tcB.13) + 
        (epB.14 * tcB.14) + 
        (epB.15 * tcB.15) + 
        (epB.16 * tcB.16) 
      
      
      
      # Expected utilities for each Treatment strategy is the sum of the weighted values
      # (probabilities of each pathway multiplied by the utility of each pathway)
      
      ### Expected utility for Treatment A
      utility.tcA <- (epA.1 * u.live) + 
        (epA.2 * u.dead) + 
        (epA.3 * u.live) + 
        (epA.4 * u.dead) + 
        (epA.5 * u.live) + 
        (epA.6 * u.dead) + 
        (epA.7 * u.live) + 
        (epA.8 * u.dead) + 
        (epA.9 * u.live) + 
        (epA.10 * u.dead) + 
        (epA.11 * u.live) + 
        (epA.12 * u.dead) + 
        (epA.13 * u.live) + 
        (epA.14 * u.dead) + 
        (epA.15 * u.live) + 
        (epA.16 * u.dead) 
      
      
      ### Expected utility for Treatment B
      utility.tcB <- (epB.1 * u.live) + 
        (epB.2 * u.dead) + 
        (epB.3 * u.live) + 
        (epB.4 * u.dead) + 
        (epB.5 * u.live) + 
        (epB.6 * u.dead) + 
        (epB.7 * u.live) + 
        (epB.8 * u.dead) + 
        (epB.9 * u.live) + 
        (epB.10 * u.dead) + 
        (epB.11 * u.live) + 
        (epB.12 * u.dead) + 
        (epB.13 * u.live) + 
        (epB.14 * u.dead) + 
        (epB.15 * u.live) + 
        (epB.16 * u.dead)       
      
      # Expected total costs, expected life years, incremental costs, incremental life years , and ICER lists
      E      <- c(TxA.n, TxB.n)
      C      <- c(exp.tcA, exp.tcB)
      QALY   <- c(utility.tcA, utility.tcB)
      IC     <- exp.tcA - exp.tcB
      IE     <- utility.tcA - utility.tcB
      ICER   <- (exp.tcA - exp.tcB) / (utility.tcA - utility.tcB)
      
      
      names(E)      <- paste("E", c("TxA", "TxB"), sep = "_")
      names(C)      <- paste("C", c("TxA", "TxB"), sep = "_")
      names(QALY)   <- paste("QALY", c("TxA", "TxB"), sep = "_")
      names(IC)     <- paste("Incr Costs")
      names(IE)     <- paste("Incr QALYs")
      names(ICER)   <- paste("ICER")
      
      # Generate the output
      return(rbind(c(E, C, QALY, IC, IE, ICER)))
      #return(c(E, C, QALY, IC, IE, ICER))
    }
  )
}

#### Now, we can use the function "dec_tree" with the inputs to estimate the ICER and its corresponding values
output <- dec_tree(input)
output

### Format the output
sprintf("%.3f", output)

TxA.n <- output[1, 1]
TxB.n <- output[1, 2]

TxA.c <- output[1, 3]
TxB.c <- output[1, 4]

TxA.e <- output[1, 5]
TxB.e <- output[1, 6]





###################################################################
## Markov model
###################################################################
# start the clock and run the model
p <- Sys.time()

#### Cost and utility parameters
u.heroin1         <-  0.80     # Utility for the heroin1 state
u.discontinue1    <-  0.856    # Utility for the discontinue1 state
u.overdose1       <-  0.00     # Utility for the overdose1 state

u.heroin2         <-  0.80     # Utility for the heroin2 state
u.discontinue2    <-  0.856    # Utility for the discontinue2 state
u.overdose2       <-  0.00     # Utility for the overdose2 state

u.heroin3         <-  0.80     # Utility for the heroin3 state
u.discontinue3    <-  0.856    # Utility for the discontinue3 state
u.overdose3       <-  0.00     # Utility for the overdose3 state

u.death           <-  0.00     # utility of death


c.heroin1         <-  1486    # Costs for the heroin1 state
c.discontinue1    <-  0       # Costs for the discontinue1 state
c.overdose1       <-  17000   # Costs for the overdose1 state

c.heroin2         <-  988     # Costs for the heroin2 state
c.discontinue2    <-  0       # Costs for the discontinue2 state
c.overdose2       <-  17000   # Costs for the overdose2 state

c.heroin3         <-  988     # Costs for the heroin3 state
c.discontinue3    <-  0       # Costs for the discontinue3 state
c.overdose3       <-  17000   # Costs for the overdose3 state

c.death           <-  0.00    # Costs for the death state


#### Transition probabilities
tpheroin_death    <-  0.095   # Heroin to death

tpOD1_DC2         <-  0.062    # 1st overdose to discontinue use 2
tpOD2_DC3         <-  0.062    # 2nd Overdose to Discontinue Heroin Use 3
tpOD3_DC3         <-  0.062    # 3rd Overdose to Discontinue Heroin Use 3

tpOD1_death       <-  0.06     # 1st overdose to Death
tpOD2_death       <-  0.074    # 2nd Overdose to Death
tpOD3_death       <-  0.074    # 3rd Overdose to Death

tpOD1_heroin1     <-  1 - tpOD1_DC2 - tpOD1_death    # 1st overdose to Heroin use
tpOD2_heroin2     <-  1 - tpOD2_DC3 - tpOD2_death    # 2st overdose to Heroin use 2
tpOD3_heroin2     <-  1 - tpOD3_DC3 - tpOD3_death    # 3rd Overdose to Resume Heroin Use 2

tpheroin_OD1      <-  0.09     # Heroin to 1st overdose
tpheroin1_OD2     <-  0.22     # Resume heroin use 1 to 2nd overdose
tpheroin2_OD3     <-  0.34     # Resume Heroin Use 2 to 3rd Overdose

tpheroin_DC1      <-  0.06     # Heroin to discontinue use 1
tpheroin1_DC2     <-  0.06     # Resume heroin use 1 to discontinue use 2
tpheroin2_DC3     <-  0.06     # Resume Heroin Use 2 to discontinue heroin use 3

tpDC1_DC1         <-  0.93     # Discontinue Heroin use 1 to Discontinue Heroin use 1
tpDC2_DC2         <-  0.93     # Discontinue Heroin use 2 to Discontinue Heroin use 2
tpDC3_DC3         <-  0.93     # Discontinue Heroin use 3 to Discontinue Heroin use 3

tpDC1_heroin      <-  0.07     # Discontinue Heroin use 2 to Resume Heroin Use 1
tpDC2_heroin      <-  0.07     # Discontinue Heroin use 2 to Resume Heroin Use 1
tpDC3_heroin2     <-  0.07     # Discontinue Heroin use 3 to Resume Heroin Use 2

heroin_continue   <-  1 - tpheroin_death - tpheroin_OD1 - tpheroin_DC1 # Heroin Use to Heroin Use


# #### Load survival data and convert to matrix
# death_rate <- openxlsx::read.xlsx("C:\\Users\\mbounthavong\\Dropbox\\Projects\\CUA Academic Detailing and Naloxone\\Decision Tree\\R Code\\Marks model\\markov deathrate.xlsx")
# 
# death_rate <- as.matrix(death_rate)    # Convert to matrix
# dim(death_rate)                        # Assess the dimensions (65 x 1)


#### State names
v_state_names <- c("Heroin Use", 
                   "DC1",
                   "Overdose", 
                   "Resume Heroin Use1", 
                   "DC2",
                   "2nd Overdose", 
                   "Resume Heroin Use 2", 
                   "DC3",
                   "3rd Overdose", 
                   "Death")


#### Number of cycles
n.t <- 100                      # number of cycles


#### Discount weight
d.c  <- d.e <- 0.03                            # equal discount of costs and QALYs by 3%
v.dwe <- v.dwc <- 1 / ((1 + d.e) ^ (0:(n.t-1)))  # discount weight (equal discounting is assumed for costs and effects)


#### Number of states
n.s <- length(v_state_names)   # number of states



#### MODEL
#### Construct the matrix -- No Simulation
m.P <- matrix(c(1 - tpheroin_DC1 - tpheroin_OD1 - tpheroin_death,tpheroin_DC1,tpheroin_OD1,0,0,0,0,0,0,tpheroin_death,
                1 - tpDC1_DC1,tpDC1_DC1,0,0,0,0,0,0,0,0,
                0,tpOD1_DC2,0,1 - tpOD1_DC2 - tpOD1_death,0,0,0,0,0,tpOD1_death,
                0,0,0,1 - tpheroin1_DC2 - tpheroin1_OD2 - tpheroin_death,tpheroin1_DC2,tpheroin1_OD2,0,0,0,tpheroin_death,
                0,0,0,1 - tpDC2_DC2,tpDC2_DC2,0,0,0,0,0,
                0,0,0,0,0,0,1 - tpOD2_DC3 - tpOD2_death,tpOD2_DC3,0,tpOD2_death,
                0,0,0,0,0,0,1 - tpheroin2_DC3 - tpheroin2_OD3 - tpheroin_death,tpheroin2_DC3,tpheroin2_OD3,tpheroin_death,
                0,0,0,0,0,0,1 - tpDC3_DC3,tpDC3_DC3,0,0,
                0,0,0,0,0,0,1 - tpOD3_DC3 - tpOD3_death,tpOD3_DC3,0,tpOD3_death,
                0,0,0,0,0,0,0,0,0,1),
              ncol = length(v_state_names),
              nrow = length(v_state_names),
              byrow = TRUE,
              dimnames = list(v_state_names, v_state_names))


#### Check sum (should be 1 for each state)
rowSums(m.P)        


#### Markov trace
m.TR.a <- matrix(NA, 
               nrow = n.t, 
               ncol = n.s, 
               dimnames = list(1:n.t, v_state_names)) 

m.TR.b <- matrix(NA, 
                 nrow = n.t, 
                 ncol = n.s, 
                 dimnames = list(1:n.t, v_state_names)) 


#### Initialize Markov trace (There are 10 states) | Everyone begins at "heroin Use" state at t = 1
# Note: The initial starting points for the trt and ctrl are generated from a DA tree.
# Initial values for TxA.n = 0.9668080
# Initial values for TxB.n = 0.9465520
a <- TxA.n     ## Prop alive post DA tree - TxA
b <- TxB.n     ## Prop alive post DA tree - TxB

m.TR.a[1,] <- c(a,0,                          # TxA Model
                0,0,
                0,0,
                0,0,
                0,0) 

m.TR.b[1,] <- c(b,0,                          # TxB model
                0,0,
                0,0,
                0,0,
                0,0) 

#### Estimate the Markov trace for cycle t
# TxA matrix
for (t in 2:n.t)                              # Start at t = 2
    {
     m.TR.a[t,] <- m.TR.a[t - 1, ] %*% m.P
    }

# TxB matrix
for (t in 2:n.t)                              # Start at t = 2
{
  m.TR.b[t,] <- m.TR.b[t - 1, ] %*% m.P
}


#### Markov trace -- Visualize
m.TR.a
m.TR.b


#### Check rows (These should all equal to 1) 
# since the model is initialized by the DA tree, this may not be equal to 1
rowSums(m.TR.a)
rowSums(m.TR.b)


#### Plot Markov trace for TXA - Method 1
plot(1:n.t, m.TR.a[,"Heroin Use"],type = "l", ylim = c(0, 1), ylab = "Proportion of cohort", xlab = "Cycles" )
lines(m.TR.a[,"DC1"],col = 2)
lines(m.TR.a[,"Overdose"],col = 3)
lines(m.TR.a[,"Resume Heroin Use1"],col = 4)
lines(m.TR.a[,"DC2"],col = 5)
lines(m.TR.a[,"2nd Overdose"],col = 6)
lines(m.TR.a[,"Resume Heroin Use 2"],col = 7)
lines(m.TR.a[,"DC3"],col = 8)
lines(m.TR.a[,"3rd Overdose"],col = 9)
lines(m.TR.a[,"Death"],col = 10)
legend("right", v_state_names, col = 1:10, lty = rep(1, 10), bty = "n")

#### Plot Markov trace for TXA- Method 2
matplot(m.TR.a)


#### Visualize the survival curve for TXA
v.S <- rowSums(m.TR.a[, 1:9])
plot(v.S, type = "l", ylab = "Proportion alive", xlab = "Cycles")


#### Output
# Utility scores in each state
v.u.trt <- c(u.heroin1, u.discontinue1, u.overdose1, 
             u.heroin2, u.discontinue2, u.overdose2, 
             u.heroin3, u.discontinue3, u.overdose3, 
             u.death)
v.u.ctl <- c(u.heroin1, u.discontinue1, u.overdose1, 
             u.heroin2, u.discontinue2, u.overdose2, 
             u.heroin3, u.discontinue3, u.overdose3, 
             u.death)

# Costs in each state
v.c.trt <- c(c.heroin1, c.discontinue1, c.overdose1, 
             c.heroin2, c.discontinue2, c.overdose2, 
             c.heroin3, c.discontinue3, c.overdose3, 
             c.death)
v.c.ctl <- c(c.heroin1, c.discontinue1, c.overdose1, 
             c.heroin2, c.discontinue2, c.overdose2, 
             c.heroin3, c.discontinue3, c.overdose3, 
             c.death)

v.E_trt <- m.TR.a %*% v.u.trt  # calculate total effect per cycle for treatment
v.C_trt <- m.TR.a %*% v.c.trt  # calculate total cost per cycle for treatment

v.E_ctl <- m.TR.b %*% v.u.ctl  # calculate total effect per cycle for control
v.C_ctl <- m.TR.b %*% v.c.ctl  # calculate total cost per cycle for control  

dim(t(v.E_trt))

te_trt <- t(v.E_trt) %*% v.dwe    # Apply discounting (Transpose the matrix using t(Matrix)
tc_trt <- t(v.C_trt) %*% v.dwc    # Apply discounting (Transpose the matrix using t(Matrix)

te_ctl <- t(v.E_ctl) %*% v.dwe    # Apply discounting (Transpose the matrix using t(Matrix)
tc_ctl <- t(v.C_ctl) %*% v.dwc    # Apply discounting (Transpose the matrix using t(Matrix)

te_trt                            # Total QALYs for TxA
tc_trt                            # Total Costs for TxA

te_ctl                            # Total QALYs for TxB
tc_ctl                            # Total Costs for TxA

DE <- te_trt - te_ctl          # Incremental QALYs
DC <- tc_trt - tc_ctl          # Incremental Costs

ICER <- DC/DE                  # ICER
ICER


#### Added the DA tree results
DE.da <- (te_trt + TxA.e) - (te_ctl + TxB.e)        # Incremental QALYs
DC.da <- (tc_trt + TxA.c) - (tc_ctl + TxB.c)        # Incremental Costs
DE.da
DC.da

ICER.da <- DC.da / DE.da          # ICER
ICER.da


#### End program (estimate the time to completion)
Sys.time() - p


