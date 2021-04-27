library(dplyr)
library(tidyverse)
library(knitr)
library(broom)
library(nnet)

data <- read_csv("data/all_national_data.csv")
data <- data %>%
  mutate(abortion_status = as.factor(abortion_status))

ellipse::pairs(as.factor(abortion_status) ~ as.factor(state) + fp_perc_need_met16 + fp_num_served16 + fp_num_u20_served16, 
             data = data, lower.panel = NULL) # linear relationship between fp_num_served16 and fp_num_u20_served16
ellipse::pairs(as.factor(abortion_status) ~ fp_num_centers15 + fp_total_expend15 + STATEFP20 + childcare_score + paid_leave_score, 
               data = data, lower.panel = NULL) #linear relatnshp between fp_num_centers15 and fp_total_expend15
ellipse::pairs(as.factor(abortion_status) ~ ad_num_waiting16 + ad_num_adopted16 + ad_num_served16 + ad_prop_adopted16 + parental_leave_score + prop_uninsured16, 
               data = data, lower.panel = NULL) #lin relation btwn ad_num_waiting16 & ad_num_adopted16 and ad_num_waiting16 & ad_num_served16 and ad_num_adopted16 & ad_num_served16 

full <- multinom(as.factor(abortion_status) ~ fp_perc_need_met16 + fp_num_served16 + fp_num_u20_served16 + fp_num_centers15 
            + fp_total_expend15 + ad_num_waiting16 + ad_num_adopted16+ad_num_served16+
              ad_prop_adopted16+parental_leave_score+prop_uninsured16 + fp_num_served16*fp_num_u20_served16 + 
              fp_num_centers15*fp_total_expend15 + ad_num_waiting16*ad_num_adopted16 + ad_num_waiting16*ad_num_served16
            + ad_num_adopted16*ad_num_served16, data = data)

kable(tidy(full), 
      format = "markdown", digits = 3, caption = "Full Model")
kable(confint(full), format = "markdown", digits = 3, caption = "Full Model Confidence Intervals")

regfit_backward <- step(full, direction = "backward")


best_model <- multinom(as.factor(abortion_status) ~ ad_num_waiting16*ad_num_adopted16 + ad_num_adopted16*ad_num_served16 + 
                         ad_num_waiting16:ad_num_served16 + prop_uninsured16 + ad_prop_adopted16 + fp_num_served16*fp_num_u20_served16
                       + fp_total_expend15 + parental_leave_score + fp_perc_need_met16, data = data)

kable(tidy(best_model), 
      format = "markdown", digits = 3, caption = "Best Model") 
kable(confint(best_model), format = "markdown", digits = 3, caption = "Best Model Confidence Intervals")

# prediction of model
pred <- predict(best_model, type = "class")
table <- table(data$abortion_status, pred)
confusion_matrix <- as.data.frame(table)

ggplot(data = confusion_matrix,
       mapping = aes(x = Var1,
                     y = pred)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log")
