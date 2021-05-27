###Function to simulate CLNU monitoring power

## For glm.nb():
library(MASS)

# For left_join():
library(dplyr)

# For ggplot:
library(ggplot2)
library(ggthemes)

# Read in data:
CLNU_dat <- read.csv("~\\grevstad\\consulting\\yellowstone_nutcrackers2\\data_2019\\CLNU_data2019 summary.csv", 
                     header = TRUE)
CLNU_dat<-CLNU_data_summary
set.seed(1)

aggregate(Birds ~ Forest_Type, data = CLNU_dat, FUN = mean)
aggregate(Birds ~ month, data = CLNU_dat, FUN = mean)
aggregate(Birds ~ Forest_Type + month, data = CLNU_dat, FUN = mean)


my.reg <- glm.nb(Birds ~ Forest_Type + month, data = CLNU_dat)

summary(my.reg)

###Initialize objects for simulation

years15 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
years14 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
years13 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
years12 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
years11 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
years10 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
years9 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
years8 <- c(0, 1, 2, 3, 4, 5, 6, 7)
years7 <- c(0, 1, 2, 3, 4, 5, 6)
years6 <- c(0, 1, 2, 3, 4, 5)
years5 <- c(0, 1, 2, 3, 4)
list<-list(years15, years14, years13, years12, years11, years10, years9, years8, years7,
           years6,years5)

###beta7 is the simulated annual decline
b7 <- log(0.975) 
decline<-exp(b7)
###Generate an empty vector for power estimates
Power<-numeric()

###Function
pow_sim_func<-function(year_list,b7){
  Power<-numeric()
  for(i in 1:length(year_list)){
  
  n.trnscts.per.Forest_type <- 2
  n.vsts.per.month <- 1
  
  #(Intercept) 
  b0 <- my.reg$coefficients[1]
  #Forest_TypePICO 
  b1 <- my.reg$coefficients[2]
  #Forest_TypePIEN
  b2 <- my.reg$coefficients[3]
  #Forest_TypePIFL
  b3 <- my.reg$coefficients[4]
  #Forest_TypePSME
  b4 <- my.reg$coefficients[5]
  #monthOctober
  b5 <- my.reg$coefficients[6]
  #monthSeptember
  b6<- my.reg$coefficients[7]
  
  #Dispersion parameter
  theta0 <- my.reg$theta
  
  new_Forest_Type <- rep(c("PIAL", "PICO", "PIEN", "PIFL", "PSME"),
                         each = n.trnscts.per.Forest_type)
  new_month <- rep(c("July-August", "October", "September"),
                   each = n.vsts.per.month)
  
  my.data <- expand.grid(new_Forest_Type, new_month, year_list[[i]], 
                         stringsAsFactors = FALSE)
  names(my.data) <- c("Forest_Type", "month", "Year")
  my.data$Birds <- 1
  
  Forest_Type_coded <- as.data.frame(contrasts(CLNU_dat$Forest_Type))
  names(Forest_Type_coded) <- paste("Forest_Type", c("PICO", "PIEN", "PIFL", "PSME"), sep = "")
  Forest_Type_coded$Forest_Type <- row.names(Forest_Type_coded)
  
  # in left_join():
  month_coded <- as.data.frame(contrasts(CLNU_dat$month))
  names(month_coded) <- paste("month", c("October", "September"), sep = "")
  month_coded$month <- row.names(month_coded)
  
  my.data <- left_join(my.data, Forest_Type_coded)
  my.data <- left_join(my.data, month_coded)
  my.data
  
  my.reg.for.sims <- glm(Birds ~ -1 + offset(b0 + b1*Forest_TypePICO +
                                               b2* Forest_TypePIEN +
                                               b3*Forest_TypePIFL +
                                               b4*Forest_TypePSME +
                                               b5*monthOctober +
                                               b6*monthSeptember +
                                               b7*Year), 
                         data = my.data, 
                         family = negative.binomial(theta = theta0))
  
  summary(my.reg.for.sims)
  
  n.sims <- 500
  
  sim.data0 <- simulate(my.reg.for.sims, n = n.sims)
  sim.data <- cbind(sim.data0, 
                    my.data[c("Year",
                              "Forest_TypePICO",
                              "Forest_TypePIEN",
                              "Forest_TypePIFL",
                              "Forest_TypePSME",
                              "monthOctober",
                              "monthSeptember")])
  
  pvals <- rep(NA, n.sims)
  
  for(i in 1:n.sims) {
    sim_i <- paste("sim_", as.character(i), sep = "")
    formu <- formula(paste(sim_i, "~ Forest_TypePICO +
                                  Forest_TypePIFL +
                                  Forest_TypePSME +
                                  monthOctober +
                                  monthSeptember +
                                  Year"))
    my.reg <- glm.nb(formu, data = sim.data)
    my.reg.summary <- summary.glm(my.reg)
    my.reg.coeffs <- my.reg.summary$coefficients
    pvals[i] <- my.reg.coeffs[7, "Pr(>|t|)"]
  }
  #my.power<-sum(pvals < 0.05)/length(pvals)
  my.power <- mean(pvals < 0.05)
  Power<- c(Power,my.power)
  }
  return(Power)
}
b7<-log(0.975)
pow<-pow_sim_func(list,b7)

pow_975<-pow_sim_func(list,b7)
pow_975

intervals<-c(15,14,13,12,11,10,9,8,7,6,5)
power_sum<-cbind(intervals,0.95,pow)
power_sum_975<-cbind(intervals,0.975,pow_975)
power_sum
power_sum_975

pow_tot <- rbind(power_sum,power_sum_975)


colnames(pow_tot) <- c("years","Decline","Est_pow")
power_sum<-as.data.frame(pow_tot)
power_sum$Decline<-as.factor(power_sum$Decline)

power_sum %>%
  ggplot( aes(x=years, y=Est_pow, col = Decline)) +
  geom_line() +
  geom_point()+
  labs(x = "Years", y = "Estimated Power")+
  ggtitle( "Power function plot", subtitle = "N = 30 surveys per year")+
  theme_clean()


