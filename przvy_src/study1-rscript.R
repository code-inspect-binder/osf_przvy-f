#######################
#Description of script#
#######################

#This script was used to analyze the analyze the data of Study 1 in the paper "Effects of grammatical gender on gender inferences: evidence from
#French hybrid nouns"
#To run this code, the following additional files are necessary: 
#   study1-data.csv            (contains the data)
#   model-study1.RData         (contains the Bayesian model of the data)

#####################
#Description of data#
#####################

#The datafile contains 13 columns: 
# * Participant: a number for each participant in the study (Participants in Group1 have correspond to numbers <100, Participants in Group 2 correspond to numbers >100)
# * ParticipantGender: participant's gender (female, male, N/A)
# * ParticipantAge: participant's age
# * InterviewTime: study duration for each participant (in second)
# * Noun: target noun (there are 28 common nouns, as well as additional proper names)
# * Age: age of the noun's referent according to participants, evaluated along a 7-point Likert scale
#           1: young ("âge jeune")
#           2, 3, 4, 5, 6: intermediate values for the Age variable
#           7: old ("âge avancé")
# * Gender: social gender of the noun's referent according to participants, evaluated along a 7-point Likert scale
#           1: male ("homme")
#           2, 3, 4, 5, 6: intermediate degrees of certainty about the referent's gender
#           7: female ("femme")
# * Education: level of education of the noun's referent according to participants, evaluated along a 7-point Likert scale
#           1: not advanced ("peu avancé")
#           2, 3, 4, 5, 6: intermediate degrees of education
#           7: advanced ("avancé")
# * Group: 
#           1: participant belonging to Group 1 (participants were exposed to 7 masculine nouns and 7 feminine nouns)
#           2: participant belonging to Group 2 (participants were exposed to the other half of nouns)
# * ItemType: 
#           experimental: experimental items (masculine/feminine hybrid nouns)
#           filler: filler items (proper names)
# * GramGender (grammatical gender): 
#           feminine: feminine grammatical gender for the noun
#           masculine: masculine grammatical gender for the noun
#           (no value): for filler items (proper names) 
# * Item: 
#           noun pair that includes the target noun (e.g. star/as, bête/phénomène, etc.). These pairs consist of a feminine noun and a masculine with similar semantics. There are 14 such pairs.
# * Sentence: 
#           sentence in which the target noun appeared

##########################################################
#Packages to be loaded for data manipulation and analysis#
##########################################################

if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")} #to reshape the dataset from wide to long format
if (!require("plyr")) {install.packages("plyr"); require("plyr")}#to rename levels of a variable
if (!require("stringi")) {install.packages("stringi"); require("stringi")}#to find first occurrence of character in a string
if (!require("stringr")) {install.packages("stringr"); require("stringr")}#for function str_match
if (!require("data.table")) {install.packages("data.table"); require("data.table")}#provides function for renaming columns of dataframe
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")} #for plots
if (!require("utils")) {install.packages("utils"); require("utils")} #provides the expand.grid function 

#For Bayesian ordinal regression using brms (interface to Stan)
if (!require("brms")) {install.packages("brms"); require("brms")}
packageVersion("brms")
if (!require("bayestestR")) {install.packages("bayestestR"); require("bayestestR")} #to calculate 95% credibility interval for Bayesian posterior samples 

###############
#Load the data#
###############

#load the data
datafile = read.csv("study1-data.csv", sep =",", header=TRUE)
head(datafile)
dim(datafile) 

###############
#Visualization#
###############

#Remove the filler items 
subset.for.plot <- subset(datafile, !(ItemType=="filler"))
subset.for.plot <- droplevels(subset.for.plot)

#Prepare data for plot
data.likert <- data.frame(xtabs(~GramGender+Gender, subset.for.plot))

#Plot
plot.likert <- ggplot(data.likert, aes_string(x="`GramGender`", y="`Freq`", fill="`Gender`")) + geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45)) + coord_flip() + 
  ggtitle("Influence of grammatical gender in the interpretation")+
  ylab("Percent")+
  xlab("Grammatical gender")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")

#Save the plot
jpeg("study1-plot.jpeg", units="in", width=16, height=8, res=300)
plot.likert
dev.off()

######################
#Statistical analysis#                                                                        
######################

#Make GramGender a factor with two levels (feminine, masculine)
subset.for.plot$GramGender <- as.factor(subset.for.plot$GramGender)

#Contrast coding for modeling
contrasts(subset.for.plot$GramGender) <- contr.treatment(2,1)  #baseline = feminine

#The fitted model can be directly loaded (the name of the model is model.study1.RData)
load("model-study1.RData")

#Or it can be run again using the following code (running the model takes a few hours)
model.study1 <- brm(
  formula = Gender ~ 1 + GramGender  + (GramGender|Participant) + (GramGender|Group) + (1|Item) + (1|Noun),
  data = subset.for.plot,
  family = cumulative("probit"), 
  iter = 4000,   #increase the number from default 2000 to 4000
  chains = 4,
  control = list(adapt_delta = 0.999999999, max_treedepth = 16), #adapt_delta increased to address the problem of divergent transitions, increase max_treedepth from 10 to 16 to address the problem with transitions after warmup that exceeded the maximum treedepth. 
  inits = 0, #to avoid the pb mentioned here: https://discourse.mc-stan.org/t/initialization-error-try-specifying-initial-values-reducing-ranges-of-constrained-values-or-reparameterizing-the-model/4401
  save_pars = save_pars(all = TRUE),
  set.seed(1702) #to be able to reproduce the analysis
  #, 
)
save(model.study1, file = "model-study1.RData")

#Summary of the model
summary(model.study1)
#Grammatical Gender does not significantly affect gender inferences (Credibility Interval includes 0)

#Plot
conditional_effects(model.study1, categorical = TRUE)

#Post-hoc analysis:
#Grammatical Gender does not significantly affect gender inferences overall (averaging across Group 1 and Group 1)
#But does it affect gender inferences within each group?

#Effect of GramGender in Group1
mean(posterior_samples(model.study1)[['b_GramGender2']] + posterior_samples(model.study1)[['r_Group[1,GramGender2]']])
#-1.05
#lower
ci(posterior_samples(model.study1)[['b_GramGender2']] + posterior_samples(model.study1)[['r_Group[1,GramGender2]']],
   method = "ETI", ci=0.95)[[2]] #=-1.63
#upper
ci(posterior_samples(model.study1)[['b_GramGender2']] + posterior_samples(model.study1)[['r_Group[1,GramGender2]']],
   method = "ETI", ci=0.95)[[3]] #=-0.49
#--> Masculine gender results in male-biased interpretations as compared to feminine gender in Group 1
#(0 is outside the 95% CI --> the effect is significant)

#Effect of GramGender in Group2
mean(posterior_samples(model.study1)[['b_GramGender2']] + posterior_samples(model.study1)[['r_Group[2,GramGender2]']])
#-0.87
#lower
ci(posterior_samples(model.study1)[['b_GramGender2']] + posterior_samples(model.study1)[['r_Group[2,GramGender2]']],
   method = "ETI", ci=0.95)[[2]] 
#=-1.45
#upper
ci(posterior_samples(model.study1)[['b_GramGender2']] + posterior_samples(model.study1)[['r_Group[2,GramGender2]']],
   method = "ETI", ci=0.95)[[3]]
#=-0.27
#--> Masculine gender results in male-biased interpretations as compared to feminine gender in Group 2
#(0 is outside the 95% CI --> the effect is significant)

#Check that overall the effect of Grammatical Gender is not significant:
mean(posterior_samples(model.study1)[['b_GramGender2']])
#-0.85
#lower
ci(posterior_samples(model.study1)[['b_GramGender2']],
   method = "ETI", ci=0.95)[[2]] 
#=-2.50
#upper
ci(posterior_samples(model.study1)[['b_GramGender2']],
   method = "ETI", ci=0.95)[[3]]
#=1.25
#--> Masculine gender does not result in male-biased interpretations as compared to feminine gender across the two groups
#(0 is included in the 95% CI --> the effect is not significant)

#Problem: there is an effect of Grammatical Gender within each group but not when the two groups are considered together
#Next step: plot the results to look for an explanation

###############
#Visualization#                                                                        
###############

#change name
datafile.for.model <- subset.for.plot

#Number of samples
length(posterior_samples(model.study1)[[1]])

#Make Group a factor
datafile.for.model$Group <- as.factor(datafile.for.model$Group)

#Construct a dataset with posterior samples and calculated posteriors for all cells in the factorial design. 
#There are 8000 samples in the regression, 2*2=4 cells in the factorial design.
#-> The dataframe must contain 8000 * 4= 32000 rows
# 8000 samples for each cell. 
# the function expand.grid is provided by the package utils
post.data <- expand.grid(GramGender = levels(datafile.for.model$GramGender), 
                         Group = levels(datafile.for.model$Group))

#number of sample
nsamples = 8000

#repeating each row nsamples times
post.data <- post.data[rep(seq_len(nrow(post.data)), each = nsamples), ]
nrow(post.data)

#Add the posterior_samples values as a column in the dataframe. 
#Repeat it as many times as there are cells in the factorial design, namely 4 times 
#factor estimates
post.data$b_GramGender2 <- rep(posterior_samples(model.study1)[['b_GramGender2']], 4)
post.data$r_Group1Intercept <- rep(posterior_samples(model.study1)[['r_Group[1,Intercept]']], 4)
post.data$r_Group2Intercept <- rep(posterior_samples(model.study1)[['r_Group[2,Intercept]']], 4)
post.data$r_Group1GramGender2 <- rep(posterior_samples(model.study1)[['r_Group[1,GramGender2]']], 4)
post.data$r_Group2GramGender2 <- rep(posterior_samples(model.study1)[['r_Group[2,GramGender2]']], 4)
#threshold estimates
post.data$b_Intercept1 <- rep(posterior_samples(model.study1)[['b_Intercept[1]']], 4)
post.data$b_Intercept2 <- rep(posterior_samples(model.study1)[['b_Intercept[2]']], 4)
post.data$b_Intercept3 <- rep(posterior_samples(model.study1)[['b_Intercept[3]']], 4)
post.data$b_Intercept4 <- rep(posterior_samples(model.study1)[['b_Intercept[4]']], 4)
post.data$b_Intercept5 <- rep(posterior_samples(model.study1)[['b_Intercept[5]']], 4)
post.data$b_Intercept6 <- rep(posterior_samples(model.study1)[['b_Intercept[6]']], 4)

#Calculate for each cell/combination of predictor values the sum of the effects weighted by the contrast coding scheme. 
#This assumes the following baseline level for GramGender: "feminine"
post.data$equation.predictors <- rep(0, nrow(post.data))
post.data$Resp1 <- rep(0, nrow(post.data))
post.data$Resp2 <- rep(0, nrow(post.data))
post.data$Resp3 <- rep(0, nrow(post.data))
post.data$Resp4 <- rep(0, nrow(post.data))
post.data$Resp5 <- rep(0, nrow(post.data))
post.data$Resp6 <- rep(0, nrow(post.data))
post.data$Resp7 <- rep(0, nrow(post.data))

#Look at the header of the dataframe
head(post.data)

#Loop
for (i in 1:nrow(post.data)){
  post.data$equation.predictors[i] <- post.data$b_GramGender2[i] * ifelse(post.data$GramGender[i]=="masculine", 1, 0) + 
    post.data$r_Group1Intercept[i] * ifelse(post.data$Group[i]=="1", 1, 0) +
    post.data$r_Group2Intercept[i] * ifelse(post.data$Group[i]=="2", 1, 0) +
    post.data$r_Group1GramGender2[i] * ifelse(post.data$Group[i]=="1" & post.data$GramGender[i]=="masculine", 1, 0) +
    post.data$r_Group2GramGender2[i] * ifelse(post.data$Group[i]=="2" & post.data$GramGender[i]=="masculine", 1, 0)

  #cf Buerkner and Vuorre p 79 Equation (5)
  post.data$Resp1[i] <- pnorm(post.data$b_Intercept1[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp2[i] <- pnorm(post.data$b_Intercept2[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept1[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp3[i] <- pnorm(post.data$b_Intercept3[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept2[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp4[i] <- pnorm(post.data$b_Intercept4[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept3[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp5[i] <- pnorm(post.data$b_Intercept5[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept4[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp6[i] <- pnorm(post.data$b_Intercept6[i] - post.data$equation.predictors[i], 0, 1) - 
    pnorm(post.data$b_Intercept5[i] - post.data$equation.predictors[i], 0, 1)
  post.data$Resp7[i] <- 1 - pnorm(post.data$b_Intercept6[i] - post.data$equation.predictors[i], 0, 1)
}

########################################################################
#Plot the posterior using the 7-point Likert scale as response variable#
########################################################################

#take subset of the data with just the relevant columns
post.data2 <- subset(post.data, select = c(GramGender, Group, Resp1, Resp2, Resp3, Resp4, Resp5, Resp6, Resp7))

#Put the data in long format to have a single Resp variable with 7 levels. 
post.data.long <- reshape2::melt(post.data2, id.vars=c("GramGender", "Group"))
head(post.data.long)
dim(post.data.long)

#Rename variable as Response and value as Probability
colnames(post.data.long)[colnames(post.data.long)=="variable"] <- "Response"
colnames(post.data.long)[colnames(post.data.long)=="value"] <- "Probability"

#Treat all variables as factors
post.data.long$GramGender <- as.factor(post.data.long$GramGender)
post.data.long$Group <- as.factor(post.data.long$Group)

#Rename Resp1, etc. as 1, 2,3...
post.data.long$Response <- revalue(post.data.long$Response, c("Resp1"="1", "Resp2"="2",
                                                              "Resp3"="3", "Resp4"="4",
                                                              "Resp5"="5", "Resp6"="6",
                                                              "Resp7"="7"
))

#Summarize with ddply (cf. http://www.cookbook-r.com/Manipulating_data/Summarizing_data/) 
#Uses the function ci() from the bayestestR package to compute the credibility interval on the posterior samples. 
#Use the ETI method for CI

#Summary by group
summary.posterior <- ddply(post.data.long, c("GramGender", "Group", "Response"), summarise,
                           N    = length(Probability),
                           mean = mean(Probability),
                           hdi_l95 = ci(Probability, method = "ETI", ci=0.95)[[2]],
                           hdi_u95 = ci(Probability, method = "ETI", ci=0.95)[[3]]
)

#Plot the posterior distribution for the 7 responses as a function of Grammatical Gender and Group
plot.ordinal.regression <- ggplot(summary.posterior, aes(x=GramGender, y=mean, color=Response)) +
  geom_point(aes(color=Response), size=3,position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin=hdi_l95, ymax=hdi_u95), width=.3, position = position_dodge(width = 0.9)) +
  geom_line() +
  theme_bw() + labs(y="Posterior probability", x="Grammatical gender") +
  scale_color_grey(name = "Response") +
  facet_wrap(~Group) +
  theme_classic(base_size = 20)


plot.ordinal.regression

#Save the plot as jpg figure
jpeg("plot-ordinal-regression-study1-by-group.jpeg", units="in", width=13, height=8, res=300)
plot.ordinal.regression
dev.off()

#Summary across groups
summary.posterior <- ddply(post.data.long, c("GramGender", "Response"), summarise,
                           N    = length(Probability),
                           mean = mean(Probability),
                           hdi_l95 = ci(Probability, method = "ETI", ci=0.95)[[2]],
                           hdi_u95 = ci(Probability, method = "ETI", ci=0.95)[[3]]
)

#Plot the posterior distribution for the 7 responses as a function of Grammatical Gender (across groups)
plot.ordinal.regression.average <- ggplot(summary.posterior, aes(x=GramGender, y=mean, color=Response)) +
  geom_point(aes(color=Response), size=3,position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin=hdi_l95, ymax=hdi_u95), width=.3, position = position_dodge(width = 0.9)) +
  geom_line() +
  theme_bw() + labs(y="Posterior probability", x="Grammatical gender") +
  scale_color_grey(name = "Inferred gender") +
  theme_classic(base_size = 20)

plot.ordinal.regression.average

#Save the plot as jpg figure
jpeg("plot-ordinal-regression-study1-average.jpeg", units="in", width=13, height=8, res=300)
plot.ordinal.regression.average
dev.off()


################################################################################
#Plot the posterior using the latent (continuous) variable as response variable#
################################################################################

#take subset of the data with just the relevant columns
post.data2 <- subset(post.data, select = c(GramGender, Group, equation.predictors))
#Put the data in long format t have a single Resp variable with 7 levels. 
post.data.long <- reshape2::melt(post.data2, id.vars=c("GramGender", "Group"))
head(post.data.long)
dim(post.data.long)

#Treat all variables as factors
post.data.long$GramGender <- as.factor(post.data.long$GramGender)
post.data.long$Group <- as.factor(post.data.long$Group)

#Summarise with ddply (cf. http://www.cookbook-r.com/Manipulating_data/Summarizing_data/) 
#Uses the function ci() from the bayestestR package to compute the credibility interval on the posterior samples. 
summary.posterior <- ddply(post.data.long, c("GramGender", "Group"), summarise,
                           N    = length(value),
                           mean = mean(value),
                           hdi_l95 = ci(value, method = "ETI", ci=0.95)[[2]],
                           hdi_u95 = ci(value, method = "ETI", ci=0.95)[[3]]
)

#Rename GramGender as "Grammatical Gender" for plotting
colnames(post.data.long)[colnames(post.data.long)=="GramGender"] <- "Grammatical gender"
colnames(summary.posterior)[colnames(summary.posterior)=="GramGender"] <- "Grammatical gender"

#Change names of Group variable to Group 1 and Group 2
post.data.long$Group <- revalue(post.data.long$Group , c("1"="Group 1", "2"="Group 2"))
summary.posterior$Group <- revalue(summary.posterior$Group , c("1"="Group 1", "2"="Group 2"))

#Find the mean of the posterior distribution for Intercept3 and Intercept4
#These two values will delimit the range of values of the underlying continuous variable that correspond
#to an unbiased response (=4 on the 7-point Likert scale)
Intercept3 <- mean(post.data$b_Intercept3)
Intercept4 <- mean(post.data$b_Intercept4)

#Plot the posterior distribution of the latent continuous variable Inferred Gender as a function of Grammatical Gender and Group
#Show the mean for each combination of the two variables (Grammatical Gender and Group)
#Show the range of values for the variable Inferred Gender that correspond to an unbiased response (=4)
study1.density.plot <- ggplot(post.data.long, aes_string(x ="value", fill="`Grammatical gender`")) + xlim(-3, 2) +
  geom_density(alpha=0.4) + facet_wrap(~Group, ncol=1)  +
  geom_vline(data=summary.posterior, aes_string(xintercept="mean", color="`Grammatical gender`"), linetype="dashed", size=0.7) +
  geom_vline(xintercept=Intercept3, linetype="solid", size=1.3) +
  geom_vline(xintercept=Intercept4, size=1.3) +
  scale_fill_grey() + scale_color_grey() + xlab("Inferred gender") + ylab("Posterior density") +
  annotate(geom="text", x=-2, y=1.65, label="Male bias", size=20/.pt, fontface=2) + 
  annotate(geom="text", x=1.5, y=1.65, label="Female bias", size=20/.pt, fontface=2) + 
  annotate(geom="text", x=0.2, y=1.65, label="No bias", size=20/.pt, fontface=2)  +
  theme_classic(base_size = 20) 

study1.density.plot 

#Save the plot
jpeg("study1-density-plot.jpeg", units="in", width=12, height=8, res=300)
study1.density.plot 
dev.off()
