library(dplyr)
library(tidyverse)
library(knitr)
library(broom)
library(nnet)

data <- read_csv("data/all_national_data_merged.csv")
data <- data %>%
  mutate(Abortion = as.factor(Abortion))

ellipse::pairs(Abortion ~ FPNeedMet16 + FPServed16 + FPU20Served16 + FPCenters15 + FPExpend15, 
             data = data, lower.panel = NULL) # FPServed16 & FPU20Served16 + FPServed16&FPCenters15 + FPServed16&FPExpend15
                                              # FPU20Served16&FPCenters15 + FPU20Served16&FPExpend15 + FPCenters15&FPExpend15
ellipse::pairs(Abortion ~ Childcare1213 + PaidLeave14 + ADWaiting16 + ADAdopted16 + ADServed16 + ADPropAdopted16, 
               data = data, lower.panel = NULL) # ADWaiting16&ADAdopted16 + ADWaiting16&ADServed16 + ADAdopted16&ADServed16
ellipse::pairs(Abortion ~ ADPropTerm16 + ParentalLeave + PropUninsured16 + MaternalDeaths1216 + ADWaiting16 + ADAdopted16, 
               data = data, lower.panel = NULL) 
ellipse::pairs(Abortion ~ FPNeedMet16 + FPServed16 + PaidLeave14 + ADWaiting16 + ADPropAdopted16 + MaternalDeaths1216, 
               data = data, lower.panel = NULL)


full <- multinom(Abortion ~ FPNeedMet16+FPServed16+FPU20Served16+FPCenters15+FPExpend15+
Childcare1213+PaidLeave14+ADWaiting16+ADAdopted16+ADServed16+ADPropAdopted16+ADPropTerm16+ParentalLeave+PropUninsured16+
  MaternalDeaths1216 + FPServed16*FPU20Served16 + FPServed16*FPCenters15 + FPServed16*FPExpend15 + 
  FPU20Served16*FPCenters15 + FPU20Served16*FPExpend15 + FPCenters15*FPExpend15 + ADWaiting16*ADAdopted16 + 
  ADWaiting16*ADServed16 + ADAdopted16*ADServed16, data = na.omit(data))

kable(tidy(full), format = "markdown", digits = 3, caption = "Full Model")
kable(confint(full), format = "markdown", digits = 3, caption = "Full Model Confidence Intervals")

regfit_backward <- step(full, direction = "backward")


best_model <- multinom(Abortion ~ FPServed16*FPExpend15+MaternalDeaths1216+ADPropTerm16+ParentalLeave+ADAdopted16*ADServed16
                       +Childcare1213+FPNeedMet16+ADWaiting16*ADServed16+ADPropAdopted16+FPU20Served16*FPCenters15+
                         FPServed16*FPCenters15+FPU20Served16*FPExpend15+ADWaiting16*ADAdopted16+FPCenters15*FPExpend15
                       +FPServed16*FPU20Served16+PaidLeave14, data = data)

kable(tidy(best_model), 
      format = "markdown", digits = 3, caption = "Best Model") 
kable(confint(best_model), format = "markdown", digits = 3, caption = "Best Model Confidence Intervals")

# prediction of model
pred <- predict(best_model, type = "class")
table <- table(na.omit(data)$Abortion, pred)
confusion_matrix <- as.data.frame(table)

ggplot(data = confusion_matrix,
       mapping = aes(x = Var1,
                     y = pred)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log")
