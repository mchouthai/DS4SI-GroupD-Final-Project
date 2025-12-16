########################################################
# R Code for JEPS article:
# "The effect of biased peacekeepers on building trust"
#
#RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for macOS
#Mozilla/5.0 (Macintosh; Intel Mac OS X 12_6_0) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.10 Chrome/69.0.3497.128 Safari/537.36
#
#######################################################
remove(list=ls())

set.seed(31489)

###Packages needed for analysis##

#install.packages("pastec", "psych", "dplyr", "VGAM", "AER", "plm", "censReg", "stargazer", "ggplot2")

#load packages
library(pastecs)
library(psych)
library(dplyr)
library(VGAM)
library(AER)
library( plm )
library(censReg)
library(stargazer)
library(ggplot2)

getwd()

#setwd("~/Dropbox/Oestman Wilson/JEPS Draft/Data and Analysis/")

#Load data files

#outtrjo is data from the trust game
outtrjo <- read.csv("outtrjo.csv")

#destroy is data from the contest game
destroy <- read.csv("destroy.csv")

memchar <- read.csv("memchar.csv")

#surv_ques is data from survey questions
surv_ques <- read.csv("surv_ques.csv")

#Create data frames that dump session 99
# Session 99 was used for debugging purposes

trjo <- subset(outtrjo, outtrjo$SESSION < 99, select = c("SESSION", "TRT" ))
#rename SESSION
trjo$session <- trjo$SESSION
#create a small data frame for each session and keep the treatment
trts <- distinct(trjo, session, TRT, keep_all = FALSE)
#get rid of session 99
destroy <- subset(destroy, destroy$session < 99)

# merge the two data frames to include have treatment variables for destroy
destroy <- merge(destroy, trts, by="session", all.x=TRUE)
# now get rid of TRT == 1 this was the old baseline
destroy <- subset(destroy, destroy$TRT > 1)

#If both groups destroyed ALL tickets then a 9 was recorded for
#for group_won. The following corrects for this and sets to zero
destroy$group_won[destroy$group_won == 9] <- 0

#Get rid of temp files not needed
remove(trjo)
remove(trts)


#######################################################
#####Descriptive statistics for the contest game######
#######################################################


#First calculate mean # tickets destroyed by period
mean_dest<-aggregate(destroy$ticket, by=list(per=destroy$period), FUN=mean)
mean_dest
#Second calculate mean # tickets destroyed by period and whether group won
mean_dest<-aggregate(destroy$ticket, by=list(per=destroy$period,
               won=destroy$group_won), FUN=mean)
## rename x as mean.vec
colnames(mean_dest)[colnames(mean_dest)=="x"] <- "mean.vec"
mean_dest

#Calc standard deviation of # tickets destroyed by period, group won
sd_dest<-aggregate(destroy$ticket, by=list(per=destroy$period,
                                           won=destroy$group_won), FUN=sd)
## rename x as sd.vec
colnames(sd_dest)[colnames(sd_dest)=="x"] <- "sd.vec"
sd_dest

#Calc the total n by group and period
num_dest <- destroy %>% count(period, group_won, sort = FALSE)
num_dest

## rename column variables for merging
colnames(num_dest)[colnames(num_dest)=="period"] <- "per"
colnames(num_dest)[colnames(num_dest)=="group_won"] <- "won"

##merge the data together
des_dat <- merge(mean_dest, num_dest, by=c("per","won"))
des_dat <- merge(des_dat, sd_dest, by=c("per","won"))
des_dat
##generate the standard error vector
des_dat$se.vec <- des_dat$sd.vec/sqrt(des_dat$n)
des_dat
#build in a vector for the graph used later
period <- (c(1,2,4,5,7,8))
des_dat$period <- period
des_dat




##################################
# Figure 1 - Plot of Means, SEs of Destroyed tickets in contest Game
##################################

 p<- ggplot(des_dat) +
     annotate("rect", xmin=.3, xmax=2.75, ymin=-.1, ymax=7.5, fill="gray90", alpha=1)+
     annotate("rect", xmin=3.30, xmax=5.75, ymin=-.1, ymax=7.5, fill="gray90", alpha=1)+
     annotate("rect", xmin=6.30, xmax=8.75, ymin=-.1, ymax=7.5, fill="gray90", alpha=1)+
     geom_pointrange(aes(x=period, y=mean.vec, ymin=mean.vec-(2*se.vec), ymax=mean.vec+(2*se.vec), group=won, color=won)) +
     ylim(-.1,7.5)


#title additions
p+labs(title="Average Tickets Destroyed", x="Period and Outcome", y = "Average Tickets Destroyed")+
 theme_classic() +
 annotate("text", x = 1.1, y = 1.75, label = "Group Lost") +
 annotate("text", x = 2, y = 5.75, label = "Group Won") +
 annotate("text", x = 4, y = 2.05, label = "Group Lost") +
 annotate("text", x = 5, y = 6.20, label = "Group Won") +
 annotate("text", x = 7, y = 2.75, label = "Group Lost") +
 annotate("text", x = 7.85, y = 6.80, label = "Group Won") +
 annotate("text", x = 1.5, y = 0.25, label = "Period 1") +
 annotate("text", x = 4.5, y = 0.25, label = "Period 2") +
 annotate("text", x = 7.5, y = 0.25, label = "Period 3") +
 theme(axis.text.x=element_blank(),
       axis.ticks.x=element_blank()) +
 theme(legend.position="none")


#clean up
remove(mean_dest)
remove(sd_dest)




##############################
##############################
#Analysis for the Trust Game##
##############################
##############################

#Create data frame getting rid of Session 99 and TRT = 1 is old baseline

outtrjo <- subset(outtrjo, outtrjo$SESSION < 99 & outtrjo$TRT > 1)

## Get rid of instances where second mover could return nothing because nothing was sent

outtrjo2 <- subset(outtrjo, outtrjo$MOVE1>0)

## calculate the tripled amount sent
outtrjo2$MOVE1x3<-outtrjo2$MOVE1*3

## calculate the proportion returned
outtrjo2$P2_tw<-outtrjo2$MOVE2/(outtrjo2$MOVE1x3)


# Calculate the percentage returned

outtrjo2$P2_100<-round(outtrjo2$P2_tw*100, digits=0)


attach(outtrjo)

#Calculate descriptive statistics for first mover

describe<-cbind(MOVE1, MOVE2, TRT, PERIOD)


##Statistics reported in Panel B of Figure 2

describeBy(MOVE1, group=list(TRT, PERIOD), mat=TRUE)

detach(outtrjo)

attach(outtrjo2)


#Statistics reported in Panel B of Figure 3

describe2<-cbind(MOVE1, MOVE2, P2_tw, P2_100, TRT, PERIOD)

describeBy(P2_100, group=list(TRT, PERIOD), mat=TRUE)


detach(outtrjo2)


#create a limited data frame used for plotting Figures 2 and 3
desc_dat<-data.frame(describe)

#build two vectors
m1.vec <- c(rep(1,12))
s1.vec <- c(rep(1,12))
cnter <- 0

#write a loop building mean's and se's for periods by treatment
for (i in 1:4) {
 for (k in 2:4)
 {

  cnter <- cnter+1

 #calc mean
  m1.vec[cnter] <- mean(desc_dat$MOVE1[desc_dat$PERIOD == i & desc_dat$TRT == k])
 #calc se
  s1.vec[cnter] <- (sd(desc_dat$MOVE1[desc_dat$PERIOD == i & desc_dat$TRT == k])/
          sqrt(length(desc_dat$MOVE1[desc_dat$PERIOD == i & desc_dat$TRT == k])))

 }
}
#build some additional information
per <- (c(1,2,3,5,6,7,9,10,11,13,14,15))
type <- (c(1,2,3,1,2,3,1,2,3,1,2,3))

des_dat <- data_frame(m1.vec,s1.vec,per,type)




#####################################
#Figure 2 - Hinge Plot by Treatment##
#####################################

desc_dat %>%
 ggplot( aes(x=TRT, y=MOVE1, group=TRT)) +
 geom_boxplot( fill="snow3", notch=TRUE) +
 geom_jitter( size=0.9, color="gray3", width=0.1)+
 labs(title="Amount Sent by First Mover", x="Treatment", y = "Amount Sent")+
 theme_classic() +
 annotate("text", x = 2, y = -7.75, label = "Unbiased Monitor") +
 annotate("text", x = 3, y = -7.75, label = "Biased Monitor") +
 annotate("text", x = 4, y = -7.75, label = "No Monitor") +
 theme(axis.text.x=element_blank(),
       axis.ticks.x=element_blank()) +
 theme(legend.position="none")


## create 3 distinct dataframes - TRT1 is deliberately selecting trt==4
## trt 4 is the no monitor case
TRT1 <- subset(outtrjo, outtrjo$TRT == 4)
TRT2 <- subset(outtrjo, outtrjo$TRT == 2)
TRT3 <- subset(outtrjo, outtrjo$TRT == 3)


####################################
## Tests reported in text on p. 11##
####################################

t.test(TRT2$MOVE1, TRT3$MOVE1, alternative="greater")
kruskal.test(list(TRT2$MOVE1,TRT3$MOVE1))
ks.test(TRT2$MOVE1, TRT3$MOVE1, alternative="less", exact = FALSE)

t.test(TRT3$MOVE1, TRT1$MOVE1, alternative="greater")
kruskal.test(list(TRT3$MOVE1,TRT1$MOVE1))
ks.test(TRT3$MOVE1, TRT1$MOVE1, exact = FALSE)




##########################
#### Reciprocity
#########################

# Again create a limited data frame for plotting
desc_p2 <- data.frame(describe2)




###########################################
#Figure 3
###########################################


desc_p2 %>%
 ggplot( aes(x=TRT, y=P2_100, group=TRT)) +
 geom_boxplot( fill="snow3", notch=TRUE) +
 geom_jitter( size=0.9, color="gray3", width=0.1)+
 labs(title="Percentage Returned by Second Mover", x="Treatment", y = "Percentage Returned")+
 theme_classic() +
 annotate("text", x = 2, y = -7.75, label = "Unbiased Monitor") +
 annotate("text", x = 3, y = -7.75, label = "Biased Monitor") +
 annotate("text", x = 4, y = -7.75, label = "No Monitor") +
 theme(axis.text.x=element_blank(),
       axis.ticks.x=element_blank()) +
 theme(legend.position="none")


####################################
## Tests reported in text on p. 16##
####################################


#tests Unbiased and Biased Monitors
t.test(P2_100[TRT != 4] ~ TRT[TRT != 4],data=desc_p2,alternative="greater",var.equal=TRUE)
kruskal.test(list(desc_p2$P2_100[desc_p2$TRT ==2], desc_p2$P2_100[desc_p2$TRT == 3]))



#tests Biased and No Monitors
t.test(P2_100[TRT != 3] ~ TRT[TRT != 3],data=desc_p2,alternative="two.sided",var.equal=TRUE)
kruskal.test(list(desc_p2$P2_100[desc_p2$TRT ==2], desc_p2$P2_100[desc_p2$TRT == 4]))



###########################
###########################
## Supporting Information##
###########################
###########################


############################################
#Plot the CDFs - Panel A of Figure C1 in SI
############################################

binsize=20
df1<-tibble(x=seq(min(TRT1$MOVE1), max(TRT1$MOVE1), diff(range(TRT1$MOVE1))/binsize)) %>%
  bind_cols(Ecdf=with(.,ecdf(TRT1$MOVE1)(x))) %>%
  mutate(Ecdf_scaled=Ecdf*max(TRT1$MOVE1))

df2<-tibble(x=seq(min(TRT2$MOVE1), max(TRT2$MOVE1), diff(range(TRT2$MOVE1))/binsize)) %>%
  bind_cols(Ecdf=with(.,ecdf(TRT2$MOVE1)(x))) %>%
  mutate(Ecdf_scaled=Ecdf*max(TRT2$MOVE1))

df3<-tibble(x=seq(min(TRT3$MOVE1), max(TRT3$MOVE1), diff(range(TRT3$MOVE1))/binsize)) %>%
  bind_cols(Ecdf=with(.,ecdf(TRT3$MOVE1)(x))) %>%
  mutate(Ecdf_scaled=Ecdf*max(TRT3$MOVE1))



ecdf_123<- ggplot() +
  #ggtitle("First Mover Investments: Groups 1 & 2")+
  geom_line(data = df1, aes(x=x, y=Ecdf_scaled), color="gray50", size = .75, linetype="solid") +
  annotate(geom="text", x=5, y=51.5, label="No Monitor", color="black", size=4) +
  annotate(geom = "segment", x=5, xend=10, y=50, yend=41, color="black", arrow=arrow(length=unit(0.01,"npc"))) +
  scale_y_continuous(name = "Count",sec.axis = sec_axis(trans = ~./max(TRT1$MOVE1), name = "Ecdf")) +
  geom_line(data = df2, aes(x=x, y=Ecdf_scaled), color="gray30", size = .75, linetype="longdash") +
  annotate(geom="text", x=34, y=33.5, label="Unbiased", color="black", size=4) +
  annotate(geom = "segment", x=33, xend=26, y=35, yend=44, color="black", arrow=arrow(length=unit(0.01,"npc")), lty=5) +
  geom_line(data = df3, aes(x=x, y=Ecdf_scaled), color="gray5", size = .75, linetype="dotted") +
  annotate(geom="text", x=18, y=69, label="Biased", color="black", size=4) +
  annotate(geom = "segment", x=20, xend=25, y=67, yend=57, color="black", arrow=arrow(length=unit(0.01,"npc")), lty=3) +
  xlab("Amount Sent by First Mover") +
  ggtitle("Density of Amount Sent by Monitor Type") +
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 12))
ecdf_123


#####################
#Plot the Histograms - Panel B of Figure C.1 in SI
#####################
# Plot the three distributions
#first build some labels
outtrjo$trt_name[outtrjo$TRT == 2] <- "Unbiased Monitor"
outtrjo$trt_name[outtrjo$TRT == 3] <- "Biased Monitor"
outtrjo$trt_name[outtrjo$TRT == 4] <- "No Monitor"

trt_name2 <- c("Unbiased Monitor","Biased Monitor","No Monitor")

##assigning treatments to labels
trt_labs <- c("Unbiased Monitor","Biased Monitor","No Monitor")
names(trt_labs) <- c("2", "3", "4")

outtrjo %>%
  ggplot(aes(MOVE1, fill = "grey30"))+
  geom_histogram(binwidth=10, fill = "grey70", colour = "black") +
  facet_wrap(~TRT, labeller = labeller(TRT = trt_labs)) +
  labs(title="Distribution of Amount Sent by First Mover In Each Experimental Treatment", x="Amount Sent", y = "Frequency")+
  theme_classic() +
  theme(legend.position="none")




###########################################
#### Non-registered data analysis##########
###########################################


#################################################
#Merge trust game data with destroy data
#################################################

#First calc # times individual's group won
dest_sum<-aggregate(destroy$group_won, by=list(ID1=destroy$id), FUN=sum)
## do some convenient renaming
colnames(dest_sum)[colnames(dest_sum)=="x"] <- "num_wins"

# merge the two data frames so that I have how often individuals won in destroy game
outtrjo <- merge(outtrjo, dest_sum, by="ID1", all.x=TRUE)

# bring in the sex of the player - create dummy for female
surv_ques$female <- as.numeric(surv_ques$sq11 == 2)
surv_ques$ID1 <- surv_ques$id
sex_p <- subset(surv_ques, select=c("ID1", "female"))

# merge into outtrjo
outtrjo <- merge(outtrjo, sex_p, by="ID1", all.x=TRUE)

#now create some new variables for the estimates below
outtrjo$trt2dum <- as.numeric(outtrjo$TRT == 2)
outtrjo$trt3dum <- as.numeric(outtrjo$TRT == 3)
outtrjo$trt4dum <- as.numeric(outtrjo$TRT == 4)

outtrjo$trt2period <- outtrjo$trt2dum * outtrjo$PERIOD
outtrjo$trt3period <- outtrjo$trt3dum * outtrjo$PERIOD
outtrjo$trt4period <- outtrjo$trt4dum * outtrjo$PERIOD

outtrjo$trt2belief <- outtrjo$trt2dum * outtrjo$P1A
outtrjo$trt3belief <- outtrjo$trt3dum * outtrjo$P1A
outtrjo$trt4belief <- outtrjo$trt4dum * outtrjo$P1A

outtrjo$beliefxperiod <- outtrjo$P1A * outtrjo$PERIOD

outtrjo$t2beliefxperiod <- outtrjo$trt2belief * outtrjo$PERIOD
outtrjo$t3beliefxperiod <- outtrjo$trt3belief * outtrjo$PERIOD
outtrjo$t4beliefxperiod <- outtrjo$trt4belief * outtrjo$PERIOD

ls(outtrjo)


####################################################
#Estimate models
#First need to set the data up as panel data
#Use plm to build an appropriate panel data frame
####################################################


p1_data <- pdata.frame(outtrjo, c( "ID1") )

ls(p1_data)

attach(p1_data)

#Model 1
p1_mod1 <- censReg( MOVE1 ~ trt2dum + trt3dum + PERIOD, left=0, right=100,
             data = p1_data, method = "BHHH")
summary(p1_mod1)

#Model 2
p1_mod2 <- censReg( MOVE1 ~ trt2dum + trt3dum + PERIOD + trt2period + trt3period,
                    left=0, right=100,
                    data = p1_data, method = "BHHH")
summary(p1_mod2)



#Model 3
p1_mod3 <- censReg( MOVE1 ~ trt2dum + trt3dum + P1A + trt2belief + trt3belief,
             left=0, right=100,
             data=p1_data, method= "BHHH")
summary(p1_mod3)


##Plot marginal effects for interaction with beliefs.
coef(p1_mod3)

beta.hat <- coef(p1_mod3)
cov <- vcov(p1_mod3)

z0 <- seq(min(p1_data$P1A), max(p1_data$P1A), length.out = 200)

dy.dx2 <- (beta.hat["P1A"]*z0 + beta.hat["trt2dum"]+ beta.hat["trt2belief"]*z0)+(-4.75198222)
se.dy.dx2 <- sqrt(cov["trt2dum", "trt2dum"] + z0^2*cov["trt2belief", "trt2belief"] + 2*z0*cov["trt2dum", "trt2belief"])
upr2 <- dy.dx2 + 1.96*se.dy.dx2
lwr2 <- dy.dx2 - 1.96*se.dy.dx2

dy.dx3 <- (beta.hat["P1A"]*z0 + beta.hat["trt3dum"]+ beta.hat["trt3belief"]*z0)+(-4.75198222)
se.dy.dx3 <- sqrt(cov["trt3dum", "trt3dum"] + z0^2*cov["trt3belief", "trt3belief"] + 2*z0*cov["trt3dum", "trt3belief"])
upr3 <- dy.dx3 + 1.96*se.dy.dx3
lwr3 <- dy.dx3 - 1.96*se.dy.dx3


cols=c("Unbiased"="indianred3", "Biased"="royalblue3")



#############################################
#Figure SI C2.1
###########################################


ggplot() +
    labs(x="First Mover Beliefs",y="Amount Sent",title="Predicted Amount Sent by Beliefs and Monitor Type") +
    geom_point(aes(z0, dy.dx2, color="Unbiased"), size = 1) +
    geom_errorbar(aes(x=z0, ymin=lwr2, ymax=upr2,color="Unbiased"), width=.1) +
    geom_line(aes(z0, dy.dx2, color="Unbiased"), size=.5, linetype=1) +
    geom_point(aes(z0, dy.dx3, color="Biased"), size = 1) +
    geom_errorbar(aes(x=z0, ymin=lwr3, ymax=upr3, , color="Biased"), width=.1) +
    geom_line(aes(z0, dy.dx3, , color="Biased"), size=.5, linetype=1) +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank())




#Model 4
p1_mod4 <- censReg( MOVE1 ~ trt2dum + trt3dum + PERIOD + trt2period + trt3period +
                        P1A + trt2belief + trt3belief,
                    left=0, right=100,
                    data=p1_data, method = "BHHH")
summary(p1_mod4)



#Model 5
p1_mod5 <- censReg( MOVE1 ~ trt2dum + trt3dum + PERIOD + trt2period + trt3period +
                     P1A + trt2belief + trt3belief +
                     num_wins + female,
                    left=0, right=100,
                    data=p1_data, method = "BHHH")
summary(p1_mod5)




#################################
## Table C2.1
#Output these models to the table
##
#################################

stargazer(p1_mod1, p1_mod2, p1_mod3, p1_mod4, p1_mod5,
      covariate.labels = c("Unbiased Monitor","Biased Monitor","Period",
      "Unbiased x Period","Biased x Period","Beliefs","Unbiased x Beliefs",
      "Biased x Beliefs","Number of Group Wins","Female"),
      out="1stMover.htm")




##############################################################################
###############################################################################

#######################################
### Reciprocity Analysis
#######################################

#use outtrjo from above. But now change everything to reflect the second mover
# merge the two data frames to include how often individuals won in destroy game

p2_jo <- merge(outtrjo2, dest_sum, by.x="ID2", by.y="ID1", all.x=TRUE)
# merge sex of the second mover into outtrjo
p2_jo <- merge(p2_jo, sex_p, by.x="ID2", by.y="ID1", all.x=TRUE)

# Get rid of the instances in which nothing was sent
p2_jo <- subset(p2_jo, p2_jo$MOVE1 != 0)

#now create some new variables for the estimates below
p2_jo$trt2dum <- as.numeric(p2_jo$TRT == 2)
p2_jo$trt3dum <- as.numeric(p2_jo$TRT == 3)
p2_jo$trt4dum <- as.numeric(p2_jo$TRT == 4)

p2_jo$trt2period <- p2_jo$trt2dum * p2_jo$PERIOD
p2_jo$trt3period <- p2_jo$trt3dum * p2_jo$PERIOD
p2_jo$trt4period <- p2_jo$trt4dum * p2_jo$PERIOD

p2_jo$trt2belief <- p2_jo$trt2dum * p2_jo$P2A
p2_jo$trt3belief <- p2_jo$trt3dum * p2_jo$P2A
p2_jo$trt4belief <- p2_jo$trt4dum * p2_jo$P2A

p2_jo$beliefxperiod <- p2_jo$P2A * p2_jo$PERIOD

p2_jo$t2beliefxperiod <- p2_jo$trt2belief * p2_jo$PERIOD
p2_jo$t3beliefxperiod <- p2_jo$trt3belief * p2_jo$PERIOD
p2_jo$t4beliefxperiod <- p2_jo$trt4belief * p2_jo$PERIOD



###########################
### Estimates for SI Table C3.1
###########################

#################################################
#Estimate models
#First need to set the data up as panel data
#Use plm to build an appropriate panel data frame
#################################################

p2_jo <- pdata.frame(p2_jo, c( "ID2") )

#Model 1
p2_mod1 <- plm( P2_100 ~ trt2dum + trt3dum + PERIOD,
                    data = p2_jo, model = "random", index = c("ID2"))
summary(p2_mod1)


#Model 2
p2_mod2 <- plm( P2_100 ~ trt2dum + trt3dum + PERIOD + trt2period +
             trt3period,  data = p2_jo, model = "random", index = c("ID2"))
summary(p2_mod2)


#Model 3
p2_mod3 <- plm( P2_100 ~ trt2dum + trt3dum + P2A + trt2belief +
                trt3belief,  data = p2_jo, model = "random", index = c("ID2"))
summary(p2_mod3)

#Model 4
p2_mod4 <- plm( P2_100 ~ trt2dum + trt3dum + PERIOD + trt2period +
                trt3period + P2A + trt2belief + trt3belief,
                data = p2_jo, model = "random", index = c("ID2"))
summary(p2_mod4)

#Model 5
p2_mod5 <- plm( P2_100 ~ trt2dum + trt3dum + PERIOD + trt2period +
                trt3period + P2A + trt2belief + trt3belief + num_wins +
               female, data = p2_jo, model = "random", index = c("ID2"))
summary(p2_mod5)


####################################
## Output Table SI C.3.1
####################################

stargazer(p2_mod1, p2_mod2, p2_mod3, p2_mod4, p2_mod5, type="html",
          covariate.labels = c("Unbiased Monitor","Biased Monitor","Period",
           "Unbiased x Period","Biased x Period","Beliefs","Unbiased x Beliefs",
            "Biased x Beliefs","Number of Group Wins","Female"),
          out="2ndMover.htm")



##############################################
#Create Bubble Plots
#Figures SI 3.1a,b,c
#############################################



#create subset data for Unbiased Monitor Condition
TRT2<-desc_p2 %>%
 filter(TRT==2)%>%
 select(MOVE1, MOVE2, P2_100, P2_tw)
#Calculate the slope of the line
summary(lm(MOVE2~MOVE1, TRT2))

bubble1<- ggplot(TRT2, aes(x=MOVE1, y=MOVE2)) +
 geom_count(color="dark gray")+
 scale_size_area()+
 xlab("Amount Sent ") +
 ylab("Amount Returned")+
 ggtitle("Unbiased monitor")+
 theme_bw() +
 theme(
  panel.border = element_blank(),
  legend.position="none"
 )+
 ylim(0, 200)+
 geom_smooth(method='lm', se=FALSE)+
 annotate(geom="text", x=25, y=115, label="y=6.801+1.011*MOVE1", size=3.5, color="black")+
 geom_abline(intercept = 0, slope = 1.0, color="red", linetype="dashed")
# ggsave("bubble1.png")
#Figure in C.3.1-Panel A
bubble1

#create subset data for Biased Monitor Condition
TRT3<-desc_p2 %>%
 filter(TRT==3)%>%
 select(MOVE1, MOVE2, P2_100, P2_tw)
#Calculate the slope of the line
summary(lm(MOVE2~MOVE1, TRT3))

bubble2<-ggplot(TRT3, aes(x=MOVE1, y=MOVE2)) +
 geom_count(color="dark gray")+
 scale_size_area()+
 xlab("Amount Sent") +
 ylab("Amount Returned")+
 ggtitle("Biased Monitor")+
 theme_bw() +
 theme(
  panel.border = element_blank(),
  legend.position="none"
 )+
 ylim(0, 200)+
 geom_smooth(method='lm', se=FALSE)+
 annotate(geom="text", x=25, y=115, label="y=3.26+0.86*MOVE1", size=3.5, color="black")+
 geom_abline(intercept = 0, slope = 1.0, color="red", linetype="dashed")
# ggsave("bubble2.png")
#Figure in C.3.1-Panel B
bubble2

#create subset data for No Monitor Condition
TRT1<-desc_p2 %>%
  filter(TRT==4)%>%
  select(MOVE1, MOVE2, P2_100, P2_tw)
#Calculate the slope of the line
summary(lm(MOVE2~MOVE1, TRT1))

bubble3<- ggplot(TRT1, aes(x=MOVE1, y=MOVE2)) +
  geom_count(color="dark gray")+
  scale_size_area()+
  xlab("Amount Sent") +
  ylab("Amount Returned")+
  ggtitle("No Monitor")+
  theme_bw() +
  theme(
    panel.border = element_blank(),
    legend.position="none"
  )+
  ylim(0, 200)+
  geom_smooth(method='lm', se=FALSE)+
  annotate(geom="text", x=25, y=115, label="y=-5.37+1.02*SENT", size=3.5, color="black")+
  geom_abline(intercept = 0, slope = 1.0, color="red", linetype="dashed")
# ggsave("bubble3.png")
#Figure in C.3.1-Panel C
bubble3
