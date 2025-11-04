
## Setup ----

# load libraries
library(Sleuth2) # Bumpus data
library(tidyverse)

# organize data
df <- ex2016 %>%
  mutate(Survival = ifelse(Status == "Survived", 1, 0))

## Linear model ----
mean_absolute_fitness <- mean(df$Survival)
mean_absolute_fitness

df$relative_fitness <- df$Survival/mean_absolute_fitness

# reproduce Table 1 from Janzen and Stearn 1998
lm_selection <- lm(relative_fitness ~ 
                     scale(log(TL)) + # total length
                     scale(log(AE)) + # alar extent
                     scale(I(log(WT)/3)) + # weight
                     scale(log(BH)) + # head (and beak?) length
                     scale(log(HL)) + # humerous length
                     scale(log(FL)) + # femur length
                     scale(log(TT)) + # tibiotarsus length
                     scale(log(SK)) + # skull breadth
                     scale(log(KL)), # sternum length
                   df)
summary(lm_selection) # close enough

## linear model on absolute fitness scale, then converted
lm_selection_abs <- lm(Survival ~ 
                     scale(log(TL)) + # total length
                     scale(log(AE)) + # alar extent
                     scale(I(log(WT)/3)) + # weight
                     scale(log(BH)) + # head (and beak?) length
                     scale(log(HL)) + # humerous length
                     scale(log(FL)) + # femur length
                     scale(log(TT)) + # tibiotarsus length
                     scale(log(SK)) + # skull breadth
                     scale(log(KL)), # sternum length
                   df)
summary(lm_selection_abs)
coef(lm_selection_abs)/mean_absolute_fitness - coef(lm_selection) # equivalent

## Janzen and Stearn method
glm_selection <- glm(Survival ~ 
                     scale(log(TL)) + # total length
                     scale(log(AE)) + # alar extent
                     scale(I(log(WT)/3)) + # weight
                     scale(log(BH)) + # head (and beak?) length
                     scale(log(HL)) + # humerous length
                     scale(log(FL)) + # femur length
                     scale(log(TT)) + # tibiotarsus length
                     scale(log(SK)) + # skull breadth
                     scale(log(KL)), # sternum length
                   df, 
                   family = "binomial")
summary(glm_selection)
coef(glm_selection) # alphas in Table 1 for logistic regression
W_z <- predict(glm_selection, type = "response") # expected fitness for each individual
mean_brackets <- mean(W_z*(1-W_z)) # ref. brackets and explanation after equation 4.
mean_brackets * coef(glm_selection) / mean_absolute_fitness # beta_avggrad in Table 1
# note that its not clear that the above line should be divided by mean_absolute_fitness
# in the paper, but it is true that the selection gradient is the partial derivative
# of a trait with respect to relative fitness.
