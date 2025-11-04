# Set working directory
setwd("C:/Users/liudm/Desktop")

# Import data from csv format
pd <- read.csv("prototypes.csv")

str(pd)

# convert variable target_child and language to factor level
pd$target_child <- as.factor(pd$target_child)
length(unique(pd$target_child)) # 6 kids
pd$language <- as.factor(pd$language)

# I think it's much better to calculate proportions of OIR in total utterances;
# Otherwise, we're predicting e.g. the distance in utterances between 
# one OIR instance and the next in each child's speech (might be uninformative):
pd$prop.OIR.CS <- pd$OIRtcs_tf_T/pd$total_utt_T
pd$prop.OIR.CDS <- pd$OIRcds_tf_T/pd$total_utt_T
pd$prop.OIR.CCS <- pd$OIRcss_tf_T/pd$total_utt_T

# Should we use Mixed Modeling instead of LMs?

# Is OIR production nested in individual children, i.e., do children differ regarding their mean level of production? 
# In other words, is production non-independent? 
# Test with ICC(1)

library(multilevel)
iccmodel <- aov(pd$prop.OIR.CS ~ pd$target_child, data = pd) # total OIR production
summary(iccmodel) # significant 
ICC1(iccmodel) #  intraclass correlation coefficient ICC(1), i.e., proportion of the total variance explained by the grouping structure
# strong group-level relationships
ICC2(iccmodel) # reliability of group-level means
# near-excellent reliability

# General "cutoff" rules of thumb:
# LeBreton and Senter (2008) have suggested that an ICC(1)=.05 (already!) represents a small to medium effect;
# ICC(2) <0.40 are poor, those from 0.40 to 0.75 are fair to good, and those >0.75 are excellent (Fleiss, 1986)

attach(pd)

# Problem: predicting for proportion
# Read on it: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5860877/

hist(pd$prop.OIR.CS)
qqnorm(pd$prop.OIR.CS, pch = 1, frame = FALSE)
qqline(pd$prop.OIR.CS, col = "steelblue", lwd = 2)
# non-normal residuals

# trying the transformations

try<-asin(sqrt(prop.OIR.CS)) # arcsine square root transformation
qqnorm(try, pch = 1, frame = FALSE)
qqline(try, col = "steelblue", lwd = 2)

install.packages("VGAM")
library(VGAM)

try2<-clogloglink(prop.OIR.CS, bvalue = .Machine$double.eps) # complementary log-log
qqnorm(try2, pch = 1, frame = FALSE)
qqline(try2, col = "steelblue", lwd = 2)
## Numerical values of theta close to 0 or 1 or out of range result in Inf, -Inf, NA or NaN.
# Hence, "bvalue"...


# We can fit a mixed linear model as an illustration:

library(lme4)
# only random intercept with transform
m1 <- lmer(try2 ~ target_age_days + prop.OIR.CDS 
 + prop.OIR.CCS + n_part_av_T + language + (1 | target_child), 
           data = pd)
summary(m1) # no coefficient significant; singularity problem (random effects 0)

# only random intercept without transform, rescaled
# rescaling
perc.OIR.CS<-prop.OIR.CS*100
perc.OIR.CDS<-prop.OIR.CDS*100
perc.OIR.CSS<-prop.OIR.CSS*100
target_age_months<-target_age_days/30

m2 <- lmer(perc.OIR.CS ~ target_age_months*perc.OIR.CDS
           + perc.OIR.CSS + n_part_av_T + language + (1 | target_child), 
           data = pd)
summary(m2)

anova(m1, m2) # m2 is preferred



# Generalized mixed models

# binomial but with weights specified

target_age_years<-target_age_days/365 # rescale

m3 <- glmer(prop.OIR.CS ~ target_age_years + prop.OIR.CDS 
            + prop.OIR.CSS + n_part_av_T + language + (1 | target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m3)


m4 <- glmer(prop.OIR.CS ~ target_age_years + prop.OIR.CDS 
            + prop.OIR.CSS + n_part_av_T + language + (language | target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)

##########################

# If fails to converge:
# rule out singularity
diag.vals <- getME(m4,"theta")[getME(m4,"lower") == 0]
any(diag.vals < 1e-6) # FALSE = not singular, so fixing this is possible

# Restart model fit with prior values as starting values
ss <- getME(m4, c("theta","fixef"))
m4b <- update(m4, start = ss, 
              control = glmerControl(optCtrl = list(maxfun = 2e4)))
summary(m4b)

###########################

anova(m3, m4b) # no model difference


m5 <- glmer(prop.OIR.CS ~ target_age_years*prop.OIR.CDS 
            + prop.OIR.CSS + n_part_av_T + language + (language | target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)

summary(m5)

anova(m3, m5) # no model difference

m6 <- glmer(prop.OIR.CS ~ target_age_years + prop.OIR.CDS 
            + prop.OIR.CSS + n_part_av_T + language + ( target_age_years| target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)

summary(m6)

anova(m5, m6) # m6 is better, with slopes according to the age of the child

mean(prop.OIR.CCS/prop.OIR.CDS) # prop of OIR in CSS is generally greater

AIC(m5,m6)

# Predicting for different OIR types


pd$prop.OIR1.CS <- pd$OIRtcs_t1_total_T/pd$OIRall_tf_T
pd$prop.OIR2.CS <- pd$OIRtcs_t2_total_T/pd$OIRall_tf_T
pd$prop.OIR3.CS <- pd$OIRtcs_t3_total_T/pd$OIRall_tf_T

pd$prop.OIR1.CDS <- pd$OIRcds_t1_total_T/pd$OIRall_tf_T
pd$prop.OIR2.CDS <- pd$OIRcds_t2_total_T/pd$OIRall_tf_T
pd$prop.OIR3.CDS <- pd$OIRcds_t3_total_T/pd$OIRall_tf_T

pd$prop.OIR1.CSS <- pd$OIRcss_t1_total_T/pd$OIRall_tf_T
pd$prop.OIR2.CSS <- pd$OIRcss_t2_total_T/pd$OIRall_tf_T
pd$prop.OIR3.CSS <- pd$OIRcss_t3_total_T/pd$OIRall_tf_T

# type 1
m7 <- glmer(prop.OIR1.CS ~ target_age_years + prop.OIR1.CDS 
            + prop.OIR1.CSS + n_part_av_T + language + ( target_age_years| target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)

summary(m7)

# type 2
m8 <- glmer(prop.OIR2.CS ~ target_age_years + prop.OIR2.CDS 
            + prop.OIR2.CSS + n_part_av_T + language + ( target_age_years| target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)

summary(m8)

# type 3
m9 <- glmer(prop.OIR3.CS ~ target_age_years + prop.OIR3.CDS 
            + prop.OIR3.CSS + n_part_av_T + language + ( target_age_years| target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)

summary(m9)


# More experimentation

# Language 
m10 <- glmer(prop.OIR1.CS ~ language + ( language| target_child), 
            data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m10) # Russian negative significant

m11 <- glmer(prop.OIR2.CS ~ language + ( language| target_child), 
             data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m11) # Russian negative barely significant

m12 <- glmer(prop.OIR3.CS ~ language + ( language| target_child), 
             data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m12) # Russian & Indo positive significant (compared to Chintang)

# CDS
m13 <- glmer(prop.OIR1.CS ~ prop.OIR1.CDS + ( 1| target_child), 
             data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m13) # significant

m14 <- glmer(prop.OIR2.CS ~ prop.OIR2.CDS + ( 1| target_child), 
             data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m14) # significant

m15 <- glmer(prop.OIR3.CS ~ prop.OIR3.CDS + ( 1| target_child), 
             data = pd, family = binomial(link = "logit"), weights=total_utt_T)
summary(m15) # significant


# etc...

# For publication:

sjPlot::tab_model(m6)

# # Investigate model diagnostics with qq-plot

sjPlot::plot_model(m6, type = "diag")
?sjPlot

# Libraries with Beta family distributions!

library(glmmTMB)
?glmmTMB::family_glmmTMB

m16<- glmmTMB(prop.OIR.CS ~ target_age_days + prop.OIR.CDS 
                     + prop.OIR.CSS + n_part_av_T + language + ( 1| target_child),
                     data = pd,
                     ziformula=~1,
                     family=beta_family(link = "logit"))
summary(m16)

m17<- glmmTMB(prop.OIR.CS ~ target_age_days * prop.OIR.CDS 
              + prop.OIR.CSS + n_part_av_T + language + ( 1| target_child),
              data = pd,
              ziformula=~1,
              family=beta_family(link = "logit"))
summary(m17)

anova(m16, m17) # no difference


# For specific types

# type 1

# Produces NaNs (target age estimate is too close to zero?)
m18 <- glmmTMB(prop.OIR1.CS ~ target_age_days + prop.OIR1.CDS 
               + prop.OIR1.CSS + n_part_av_T + language + ( 1| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))

summary(m18)

# without target age
m18 <- glmmTMB(prop.OIR1.CS ~ prop.OIR1.CDS 
               + prop.OIR1.CSS + n_part_av_T + language + ( 1| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))
summary(m18)

m18b <- glmmTMB(prop.OIR1.CS ~ prop.OIR1.CDS 
               + prop.OIR1.CSS + n_part_av_T + language + ( target_age_days| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))

summary(m18b)

# type 2
m19 <- glmmTMB(prop.OIR2.CS ~ prop.OIR2.CDS 
               + prop.OIR2.CSS + n_part_av_T + language + ( 1| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))

summary(m19)

m19b <- glmmTMB(prop.OIR2.CS ~ prop.OIR2.CDS 
               + prop.OIR2.CSS + n_part_av_T + language + ( target_age_days| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))

summary(m19b)

# type 3
m20 <- glmmTMB(prop.OIR3.CS ~ prop.OIR3.CDS 
               + prop.OIR3.CSS + n_part_av_T + language + ( 1| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))

summary(m20)

m20b <- glmmTMB(prop.OIR3.CS ~ prop.OIR3.CDS 
               + prop.OIR3.CSS + n_part_av_T + language + ( target_age_days| target_child),
               data = pd,
               ziformula=~1,
               family=beta_family(link = "logit"))

summary(m20b)

####################
# To create LaTeX code for inclusion in a LaTeX document (or Overleaf)
install.packages("texreg")
library(texreg)
source(system.file("other_methods","extract.R",package="glmmTMB"))
texreg(m16)
?texreg
####################

# Bayesian modelling

install.packages("brms")
library(brms)
?brmsfamily

m16<-brm(prop.OIR.CS ~ target_age_days + prop.OIR.CDS 
         + prop.OIR.CSS + n_part_av_T + language + ( 1| target_child), 
         data = pd, family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(m16)
# To test whether all regression coefficients are different from zero, 
# we can look at the Credible Intervals that are listed in the summary output 
# or we can visually represent them in density plots.

install.packages("ggmcmc")
library(ggmcmc)
model1tranformed <- ggs(m16) # the ggs function transforms the brms output 
# into a longformat tibble, that we can use to make different types of plots.

ggplot(filter(model1tranformed, Parameter == "b_prop.OIR.CDS", Iteration > 1000), aes(x = value))+
  geom_density(fill = "orange", alpha = .5)+
  geom_vline(xintercept = 0, col = "red", size = 1)+
  scale_x_continuous(name = "Value", limits = c(-4, 36))+ 
  theme_light()+
  labs(title = "Posterior Density of Regression Coefficient for OIR in CDS Proportion")

# the coefficient not different from 0 = not "significant"


#type 1
m18c <- brm(prop.OIR1.CS ~ target_age_days + prop.OIR1.CDS 
           + prop.OIR1.CSS + n_part_av_T + language + ( 1| target_child), 
           data = pd, family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
  

summary(m18c)

# type 2
m19c <- brm(prop.OIR2.CS ~ target_age_days + prop.OIR2.CDS 
            + prop.OIR2.CSS + n_part_av_T + language + ( 1| target_child), 
            data = pd, family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))


summary(m19c)


# type 3
m20c <- brm(prop.OIR3.CS ~ target_age_days + prop.OIR3.CDS 
            + prop.OIR3.CSS + n_part_av_T + language + ( 1| target_child), 
            data = pd, family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))


summary(m20c)



pp_check(m16)

conditional_effects(m16)

conditional_effects(m18c) # type1

conditional_effects(m19c) # type2

conditional_effects(m20c) # type3



# Previous agreement analysis

agreement_sample <- read.csv("agreement_sample.csv")
View(agreement_sample)
install.packages("psych")
library(psych)
cohen.kappa(agreement_sample, w=NULL,n.obs=NULL,alpha=.05,levels=NULL)  
wkappa(x, w = NULL)    #deprecated

install.packages("irr")
library(irr)

agree(agreement_sample, tolerance=0)

kappa2(agreement_sample)

t<-c(210,119,37,0,26,26,21,0,11)
q<-c(190,52,23,1,38,33,39,6,17)

library(stats)
cor.test(t,q)
