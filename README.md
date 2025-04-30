# MUB-Data

#This data set is taking purchasing card transactions during Matthew Clarke's tenure at the MUB -> 11/2021 - 05/2025.

#The goal is to test time series modeling techniques to practice and potentially identify actionable recommendations.

#Please note, this should not be shared.

#The data from 11/29/2021 - 12/2024 is generated from the WebIntelligence applciation's reporting system, while from 12/2024 - 05/2025, this is reported using Workday's.

#Transactions from 06/2022 - 08/2023 is from Maureen Claussen's purchasing cards, whereas from that point onward is under Matthew Clarke's purchasing cards.

#

```r
library(tidyverse)
library(fftw)

pcard_data <- read.csv("Pra Info.csv")

column_na = colsum(is.na(pcard_data))

#Light EDA

summary(pcard_data)

pcard_data_agg = pcard_data %>%
  mutate(Invoice.Date = ymd(Invoice.Date) %>%
  group_by(Invoice.Date) %>%
  summarize(Trans.Amt)

hist(pcard_data$Invoice.Date)
mean(pcard_data$Trans.Amt, na.rm = TRUE)
median(pcard_data$Trans.Amt)

plot(x = pcard_data_agg$Invoice.Date, y = pcard_data_agg$Trans.Amt)
  
#Linear Regression
summary(lm(Trans.Amt ~. , data = pcard_data))

#FFT
fft = FFT(pcard_data_agg$Trans.Amt)

pred = predict(fft)

```
