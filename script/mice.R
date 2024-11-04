library(plm)
library(here)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(here)
library(mice)


# load data 

here()

# load dataset from project folder "original"

finaldata <- read.csv(here("original", "final_conflict.csv"), header = TRUE)
finaldata <- as.tibble(finaldata)
finaldata <- select(finaldata, -X.1, -X)
summary(finaldata)

#log transform gdp
finaldata["gdp1000"] <- log(finaldata["gdp1000"])
colnames(finaldata)[9] <- "loggdp"


#visualize missing data
library(naniar)
finaldata |>
  arrange(Year, ISO) |>
  dplyr::select(-country_name, -ISO, -region, -Year) |>
  vis_miss()

#convert ISO to numeric
midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)

#dry run to extract meth and pred
mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)
meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "Maternal.Mortality", "Infant.Mortality", "Neonatal.Mortality", "Under.5.Mortality", "loggdp", "popdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "Maternal.Mortality", "Infant.Mortality", "Neonatal.Mortality", "Under.5.Mortality", "loggdp", "popdens"), "ISOnum"] <- -2


#MI with 10 imputations
mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

#check for convergence
plot(mice.multi.out)

#Run analysis with MI datasets 

preds <- as.formula(" ~ bin_conflict + loggdp + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + total_earthquakes + total_droughts + 
                  ISOnum + as.factor(Year)")

matmormod <- with(mice.multi.out, lm(update.formula(preds, Maternal.Mortality ~ .)))
un5mormod <- with(mice.multi.out, lm(update.formula(preds, Infant.Mortality ~ .)))
infmormod <- with(mice.multi.out, lm(update.formula(preds, Neonatal.Mortality ~ .)))
neomormod <- with(mice.multi.out, lm(update.formula(preds, Under.5.Mortality ~ .)))



#build confidence intervals for each coefficient
ci_results <- list()
for (i in 1:length(models)) {
  ci_paste <- vector("character", length = nrow(summary(models[[i]])$coefficients))
  for (j in 1:length(summary(models[[i]])$coefficients[,1])) {
      coefficients <- summary(models[[i]])$coefficients[j,1]
      se <- summary(models[[i]])$coefficients[j,2]
      z <- qnorm(1-0.05/2)
      lower_bound <- coefficients - z * se
      upper_bound <- coefficients + z * se
      ci_paste[j] <- paste(
            round(coefficients, 2),' ', '[',
            if_else(lower_bound < upper_bound, round(lower_bound, 2), round(upper_bound, 2)),', ',
            if_else(lower_bound > upper_bound, round(lower_bound, 2), round(upper_bound, 2)),']'
            ,sep = "") }
  ci_results[[i]] <- ci_paste
}
ci_df <- as.data.frame(do.call(cbind, ci_results))
colnames(ci_df) <- c("Maternal Mortality per 100,000 live births",
                     "Infant Mortality per 1000 live births",
                     "Neonatal Mortality per 1000 live births",
                     "Under 5 Years Mortality per 1000 live births")
rownames(ci_df) <- c("Binary Conflict", "GDP per 1000*", 
                     "OECD", "Population Density",
                     "Urban Residence", "Age dependency ratio",
                     "Male Education", "Temperature",
                     "Rainfall", "Earthquakes", "Droughts")

#Create table 


ci_df %>% 
  kbl(caption = "Table 2 - Multivariate regression models for Maternal, Infant, Neonatal
      and Under 5 Mortality") %>% 
  kable_classic(full_width = F, html_font = "Times New Roman", font_size = 13) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% 
  footnote(general = "Coefficients and confidence intervals are presented; the latter in parentheses. *GDP1000 coefficient is log-transformed") %>% 
  column_spec(column = 1:3, width = '2in')