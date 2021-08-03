library(foreign)
library(memisc)
library(data.table)

rm(list=ls())
##########################################################################
#
# Functions
#
##########################################################################
# Function to code scales
CodeScales <- function(variable, reverse=FALSE, policy=FALSE){
    # Args:
    #   variable: The variable to be re-coded
    #   reverse (bool): Whether the question was re-verse coded 
    if(policy == FALSE){    
        if(reverse == FALSE){
            recoded.variable <- recode(variable,
                                       1 <- "Strongly Agree",
                                       2 <- "Somewhat Agree", 
                                       3 <- "Neither Agree nor Disagree",
                                       4 <- "Somewhat Disagree",
                                       5 <- "Strongly Disagree")
        } 
        if(reverse == TRUE){
            recoded.variable <- recode(variable,
                                       1 <- "Strongly Disagree",
                                       2 <- "Somewhat Disagree",
                                       3 <- "Neither Agree nor Disagree",
                                       4 <- "Somewhat Agree", 
                                       5 <- "Strongly Agree")
        }
    } else {
        recoded.variable <- recode(variable,
                                   1 <- "very bad idea",
                                   2 <- "pretty bad idea",
                                   3 <- "neither a good nor a bad idea",
                                   4 <- "pretty good idea", 
                                   5 <- "very good idea")
    }
    return(as.numeric(recoded.variable))
}


VariableScore<-function(vars){
    # Function to combine several seperate variables into a single variable with a numeric score.
    #
    # Args:
    #   vars: A cbind object of the different numeric variables to be combined. 
    #
    # Returns:
    #   score: a numeric vector of the seperate variables combined into a single value.
    mat <- cbind(vars)
    number.answered <- numeric(nrow(mat))
    for(i in 1:nrow(mat)){ 
        number.answered[i]<-sum(is.na(mat[i,]) == FALSE)
    }
    total <- numeric(nrow(mat))
    for(i in 1:nrow(mat)){
        total[i]<-sum(na.omit(mat[i,]))
    }
    score <- numeric(nrow(mat))
    for(i in 1:nrow(mat)){
        score[i]<-total[i]/number.answered[i]
    }
    return(score)		
}

##########################################################################
#
# Pre-Process the Data
#
##########################################################################
# Load the data
raw_data <- read.csv('data_dec_18.csv', header=TRUE)
raw_data <- droplevels(raw_data[-c(1,2),])

# Drop those that do no consent
raw_data <- subset(raw_data, (raw_data$Finished == "True"
                              & raw_data$Q41 == 'green'
                              & raw_data$Status != 'Survey Preview'))


#################################################
# Reoder factors to convert to numeric
#################################################
raw_data$ideology <- recode(raw_data$Q35,
                            1 <- "Extremely liberal",
                            2 <- "Liberal", 
                            3 <- "Slightly liberal",
                            4 <- "Moderate or middle of the road",
                            5 <- "Slightly conservative",
                            6 <- "Conservative",
                            7 <- "Extremely conservative")
raw_data$pid <- recode(raw_data$Q37.1,
                       1 <- "Strong Democrat",
                       2 <- "Weak Democrat", 
                       3 <- "Independent (Lean Democrat)",
                       4 <- "Independent (No partisan lean)",
                       5 <- "Independent (Lean Republican)",
                       6 <- "Weak Republican",
                       7 <- "Strong Republican")
raw_data$education <- recode(raw_data$Q39.1,
                             1 <- "Middle school/some high school",
                             2 <- "High school diploma", 
                             3 <- "Some college",
                             4 <- "Four year college degree",
                             5 <- "Graduate school or other advanced degree")
raw_data$income <- recode(raw_data$Q47,
                          1 <- "Less than $30,000",
                          2 <- "$30,000 - $69,999", 
                          3 <- "$70,000 - $99,999",
                          4 <- "$100,000 - $200,000" ,
                          5 <- "Greater than $200,000")
raw_data$ideology <- as.numeric(raw_data$ideology)
raw_data$pid <- as.numeric(raw_data$pid)
raw_data$education <- as.numeric(raw_data$education)
raw_data$income <- as.numeric(raw_data$income)
#################################################
# Process Respondent Demographics
#################################################
raw_data$race <- as.factor(raw_data$Q41.1)
raw_data$gender <- as.factor(raw_data$Q45)
raw_data$sexuality <- as.factor(raw_data$Q43)

raw_data$race.lim <- raw_data$race
levels(raw_data$race.lim) <- c(NA, "Other", "Asian", "Black", "Latinx", "Asian", "Other", "White")
raw_data$gender.lim <- ifelse(raw_data$gender == "Man" | raw_data$gender == "Transgender Man", 1, 0)
raw_data$lgbt <- ifelse(raw_data$sexuality == "Heterosexual" | 
                            !(raw_data$gender %in% c("Man", "Woman")), 0, 1)

raw_data$race.lim <- relevel(raw_data$race.lim, "White")
raw_data$race.bin <- ifelse(raw_data$race.lim == "White", 1, 0)

raw_data$r.other <- ifelse(raw_data$race.lim == "Other", 1, 0)
raw_data$r.asian <- ifelse(raw_data$race.lim == "Asian", 1, 0)
raw_data$r.black <- ifelse(raw_data$race.lim == "Black", 1, 0)
raw_data$r.latinx <- ifelse(raw_data$race.lim == "Latinx", 1, 0)
raw_data$r.white <- ifelse(raw_data$race.lim == "White", 1, 0)

raw_data$white_man <- ifelse(raw_data$gender.lim == 1 & raw_data$race.bin == 1, 1, 0)
raw_data$white_woman <- ifelse(raw_data$gender.lim == 0 & raw_data$race.bin == 1, 1, 0)
raw_data$man_of_color <- ifelse(raw_data$gender.lim == 1 & raw_data$race.bin == 0, 1, 0)
raw_data$woman_of_color <- ifelse(raw_data$gender.lim == 0 & raw_data$race.bin == 0, 1, 0)
#################################################
# Racial Resentment
#################################################
raw_data$resentment1 <- CodeScales(raw_data$Q27, reverse = TRUE)
raw_data$resentment2 <- CodeScales(raw_data$Q29)
raw_data$resentment3 <- CodeScales(raw_data$Q31)
raw_data$resentment4 <- CodeScales(raw_data$Q33, reverse = TRUE)

raw_data$resentment1.s <- (raw_data$resentment1 - mean(raw_data$resentment1, na.rm=T)) / sd(raw_data$resentment1, na.rm=T)
raw_data$resentment2.s <-(raw_data$resentment2 - mean(raw_data$resentment2, na.rm=T)) / sd(raw_data$resentment2, na.rm=T)
raw_data$resentment3.s <- (raw_data$resentment3 - mean(raw_data$resentment3, na.rm=T)) / sd(raw_data$resentment3, na.rm=T)
raw_data$resentment4.s <- (raw_data$resentment4 - mean(raw_data$resentment4, na.rm=T)) / sd(raw_data$resentment4, na.rm=T)


raw_data$racial_resentment <- VariableScore(
    vars = cbind(raw_data$resentment1,
                 raw_data$resentment2,
                 raw_data$resentment3,
                 raw_data$resentment4)
)
raw_data$racial_resentment.s <- VariableScore(
    vars = cbind(raw_data$resentment1.s,
                 raw_data$resentment2.s,
                 raw_data$resentment3.s,
                 raw_data$resentment4.s)
)

# Respondents that answered same for all questions
length(which(raw_data$resentment1 == 5 &
                 raw_data$resentment2 == 1 &
                 raw_data$resentment3 == 1 &
                 raw_data$resentment4 == 5))

# hist(raw_data$resentment1, density=10, angle=45, col='red', ylim=c(0, 500), breaks=0:5)
# hist(raw_data$resentment2, density=10, angle=135, col='red', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$resentment3, density=10, angle=135, col='blue', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$resentment4, density=10, angle=45, col='blue', add=T, ylim=c(0, 500), breaks=0:5)
# summary(raw_data$racial_resentment)

raw_data$racial_resentment_bin <- ifelse(raw_data$racial_resentment.s >= 0, 1, 0)
#################################################
# Modern Sexism
#################################################
raw_data$sexism1 <- CodeScales(raw_data$Q7, reverse = TRUE)
raw_data$sexism2 <- CodeScales(raw_data$Q9)
raw_data$sexism3 <- CodeScales(raw_data$Q11, reverse = TRUE)
raw_data$sexism4 <- CodeScales(raw_data$Q13, reverse = TRUE)
raw_data$sexism5 <- CodeScales(raw_data$Q15)

raw_data$sexism1.s <- (raw_data$sexism1 - mean(raw_data$sexism1, na.rm=T)) / sd(raw_data$sexism1, na.rm=T)
raw_data$sexism2.s <-(raw_data$sexism2 - mean(raw_data$sexism1, na.rm=T)) / sd(raw_data$sexism2, na.rm=T)
raw_data$sexism3.s <- (raw_data$sexism3 - mean(raw_data$sexism3, na.rm=T)) / sd(raw_data$sexism3, na.rm=T)
raw_data$sexism4.s <- (raw_data$sexism4 - mean(raw_data$sexism4, na.rm=T)) / sd(raw_data$sexism4, na.rm=T)
raw_data$sexism5.s <- (raw_data$sexism5 - mean(raw_data$sexism5, na.rm=T)) / sd(raw_data$sexism5, na.rm=T)

# Respondents that answered same for all questions
length(which(raw_data$sexism1 == 5 &
                 raw_data$sexism2 == 5 &
                 raw_data$sexism3 == 1 &
                 raw_data$sexism4 == 1 &
                 raw_data$sexism5 == 5))

# hist(raw_data$sexism1, density=10, angle=45, col='red', ylim=c(0, 500), breaks=0:5)
# hist(raw_data$sexism2, density=10, angle=135, col='red', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$sexism3, density=10, angle=135, col='blue', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$sexism4, density=10, angle=45, col='blue', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$sexism4, density=10, angle=185, col='blue', add=T, ylim=c(0, 500), breaks=0:5)

raw_data$modern_sexism <- VariableScore(
    vars = cbind(raw_data$sexism1,
                 raw_data$sexism2,
                 raw_data$sexism3,
                 raw_data$sexism4,
                 raw_data$sexism5)
)
raw_data$modern_sexism.s <- VariableScore(
    vars = cbind(raw_data$sexism1.s,
                 raw_data$sexism2.s,
                 raw_data$sexism3.s,
                 raw_data$sexism4.s,
                 raw_data$sexism5.s)
)
summary(raw_data$modern_sexism)
#################################################
# Heterosexism
#################################################
raw_data$heterosexism1 <- CodeScales(raw_data$Q17, reverse = TRUE)
raw_data$heterosexism2 <- CodeScales(raw_data$Q19, reverse = TRUE)
raw_data$heterosexism3 <- CodeScales(raw_data$Q21, reverse = TRUE)
raw_data$heterosexism4 <- CodeScales(raw_data$Q23, reverse = TRUE)
raw_data$heterosexism5 <- CodeScales(raw_data$Q25, reverse = FALSE)

raw_data$heterosexism <- VariableScore(
    vars = cbind(raw_data$heterosexism1,
                 raw_data$heterosexism2,
                 raw_data$heterosexism3,
                 raw_data$heterosexism4,
                 raw_data$heterosexism5)
) 
# hist(raw_data$heterosexism1, density=10, angle=45, col='red', ylim=c(0, 500), breaks=0:5)
# hist(raw_data$heterosexism2, density=10, angle=135, col='red', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$heterosexism3, density=10, angle=135, col='blue', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$heterosexism4, density=10, angle=45, col='blue', add=T, ylim=c(0, 500), breaks=0:5)
# hist(raw_data$heterosexism4, density=10, angle=185, col='blue', add=T, ylim=c(0, 500), breaks=0:5)

raw_data$heterosexism1.s <- (raw_data$heterosexism1 - mean(raw_data$heterosexism1, na.rm=T)) / sd(raw_data$heterosexism1, na.rm=T)
raw_data$heterosexism2.s <-(raw_data$heterosexism2 - mean(raw_data$heterosexism2, na.rm=T)) / sd(raw_data$heterosexism2, na.rm=T)
raw_data$heterosexism3.s <- (raw_data$heterosexism3 - mean(raw_data$heterosexism3, na.rm=T)) / sd(raw_data$heterosexism3, na.rm=T)
raw_data$heterosexism4.s <- (raw_data$heterosexism4 - mean(raw_data$heterosexism4, na.rm=T)) / sd(raw_data$heterosexism4, na.rm=T)
raw_data$heterosexism5.s <- (raw_data$heterosexism5 - mean(raw_data$heterosexism5, na.rm=T)) / sd(raw_data$heterosexism5, na.rm=T)

raw_data$heterosexism.s <- VariableScore(
    vars = cbind(raw_data$heterosexism1.s,
                 raw_data$heterosexism2.s,
                 raw_data$heterosexism3.s,
                 raw_data$heterosexism4.s,
                 raw_data$heterosexism5.s)
) 
#################################################
# Position Threat
#################################################
rr.stand <- (raw_data$racial_resentment - mean(raw_data$racial_resentment)) / sd(raw_data$racial_resentment)
ms.stand <- (raw_data$modern_sexism - mean(raw_data$modern_sexism)) / sd(raw_data$modern_sexism)
hs.stand <- (raw_data$heterosexism - mean(raw_data$heterosexism)) / sd(raw_data$heterosexism)

raw_data$position_threat <- VariableScore(
    vars = cbind(rr.stand,
                 ms.stand,
                 hs.stand)
) 
raw_data$position_threat.s <- VariableScore(
    vars = cbind(raw_data$racial_resentment.s,
                 raw_data$modern_sexism.s,
                 raw_data$heterosexism.s)
) 
rm(rr.stand)
rm(ms.stand)
rm(hs.stand)
summary(raw_data$heterosexism)

#################################################
# Categorize into Low and High scoring groups
#################################################
pt.sum <- quantile(raw_data$racial_resentment, probs=seq(0, 1, .1))
raw_data$racial_resentment_groups <- ifelse(
    raw_data$racial_resentment <= pt.sum[4], "Low", 
    ifelse(raw_data$racial_resentment >= pt.sum[8], "High", NA)
)
raw_data$racial_resentment_groups <- as.factor(raw_data$racial_resentment_groups)

pt.sum <- quantile(raw_data$modern_sexism, probs=seq(0, 1, .1))
raw_data$modern_sexism_groups <- ifelse(
    raw_data$modern_sexism <= pt.sum[4], "Low", 
    ifelse(raw_data$modern_sexism >= pt.sum[8], "High", NA)
)
raw_data$modern_sexism_groups <- as.factor(raw_data$modern_sexism_groups)

pt.sum <- quantile(raw_data$heterosexism, probs=seq(0, 1, .1))
raw_data$heterosexism_groups <- ifelse(
    raw_data$heterosexism <= pt.sum[4], "Low", 
    ifelse(raw_data$heterosexism >= pt.sum[8], "High", NA)
)
raw_data$heterosexism_groups <- as.factor(raw_data$heterosexism_groups)

pt.sum <- quantile(raw_data$position_threat, probs=seq(0, 1, .1))
raw_data$position_threat_groups <- ifelse(
    raw_data$position_threat <= pt.sum[4], "Low", 
    ifelse(raw_data$position_threat >= pt.sum[8], "High", NA)
)
raw_data$position_threat_groups <- as.factor(raw_data$position_threat_groups)

rm(pt.sum)
#################################################
# Treatments
#################################################
raw_data$photo_treatment <- NA
raw_data$photo_treatment[raw_data$photo_block == 0 & raw_data$obama_photo == 0] <- "Biden"
raw_data$photo_treatment[raw_data$photo_block == 0 & raw_data$obama_photo == 1] <- "Biden with Obama"
raw_data$photo_treatment[raw_data$photo_block == 1 & raw_data$obama_photo == 0] <- "Clinton"
raw_data$photo_treatment[raw_data$photo_block == 1 & raw_data$obama_photo == 1] <- "Clinton with Obama"
raw_data$photo_treatment <- as.factor(raw_data$photo_treatment)
raw_data$photo_treatment <- relevel(raw_data$photo_treatment, "Biden")
raw_data$photo_treatment2 <- relevel(raw_data$photo_treatment, 'Clinton')


raw_data$policy_treatment <- NA
raw_data$policy_treatment[raw_data$policy == 0] <- "MOC"
raw_data$policy_treatment[raw_data$policy == 1] <- "Obama"
raw_data$policy_treatment[raw_data$policy == 2] <- "Clinton"
raw_data$policy_treatment[raw_data$policy == 3] <- "Biden"
raw_data$policy_treatment <- as.factor(raw_data$policy_treatment)
raw_data$policy_treatment  <- relevel(raw_data$policy_treatment, "MOC")
#################################################
# Outcomes
#################################################
raw_data$obama_thermometer <- as.numeric(as.character(raw_data$Q34_1))
raw_data$Q31_1 <- as.numeric(as.character(raw_data$Q31_1))
raw_data$Q48_1 <- as.numeric(as.character(raw_data$Q48_1))
raw_data$Q35_1 <- as.numeric(as.character(raw_data$Q35_1))
raw_data$Q45_1 <- as.numeric(as.character(raw_data$Q45_1))

raw_data$Q36 <- CodeScales(raw_data$Q36, policy=TRUE)
raw_data$Q37 <- CodeScales(raw_data$Q37, policy=TRUE)
raw_data$Q38 <- CodeScales(raw_data$Q38, policy=TRUE)
raw_data$Q39 <- CodeScales(raw_data$Q39, policy=TRUE)

raw_data$treat_thermometer <- numeric(length=nrow(raw_data))
raw_data$treat_thermometer[raw_data$photo_treatment == "Biden"] <- raw_data[raw_data$photo_treatment == "Biden", "Q31_1"]
raw_data$treat_thermometer[raw_data$photo_treatment == "Biden with Obama"] <- raw_data[raw_data$photo_treatment == "Biden with Obama", "Q48_1"]
raw_data$treat_thermometer[raw_data$photo_treatment == "Clinton"] <- raw_data[raw_data$photo_treatment == "Clinton", "Q35_1"]
raw_data$treat_thermometer[raw_data$photo_treatment == "Clinton with Obama"] <- raw_data[raw_data$photo_treatment == "Clinton with Obama", "Q45_1"]

raw_data$them_dif <- raw_data$treat_thermometer - raw_data$obama_thermometer

raw_data$policy_support <- NA
raw_data$policy_support[raw_data$policy == 0] <- raw_data[raw_data$policy == 0, "Q36"]
raw_data$policy_support[raw_data$policy == 1] <- raw_data[raw_data$policy == 1, "Q37"]
raw_data$policy_support[raw_data$policy == 2] <- raw_data[raw_data$policy == 2, "Q38"]
raw_data$policy_support[raw_data$policy == 3] <- raw_data[raw_data$policy == 3, "Q39"]

#################################################
# Attitude Quantiles
#################################################
setDT(raw_data)[,rr.quantile := cut(racial_resentment.s, quantile(racial_resentment.s, probs=seq(0, 1, .2)),
                                    labels = FALSE, include.lowest = TRUE),]
setDT(raw_data)[,ms.quantile := cut(modern_sexism.s, quantile(modern_sexism.s, probs=seq(0, 1, .2)),
                                    labels = FALSE, include.lowest = TRUE),]
setDT(raw_data)[,rr.quantile := cut(racial_resentment.s, quantile(racial_resentment.s, probs=seq(0, 1, .2)),
                                    labels = FALSE, include.lowest = TRUE),]
setDT(raw_data)[,ms.quantile := cut(modern_sexism.s, quantile(modern_sexism.s, probs=seq(0, 1, .2)),
                                    labels = FALSE, include.lowest = TRUE),]
setDT(raw_data)[,hs.quantile := cut(heterosexism.s, quantile(heterosexism.s, probs=seq(0, 1, .2)),
                                    labels = FALSE, include.lowest = TRUE),]
raw_data <- as.data.frame(raw_data)
#################################################
# Save
#################################################
boxes_data <- raw_data
rm(raw_data)
save.image("Parsedata.RData")
