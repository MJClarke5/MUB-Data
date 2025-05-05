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
library(nixtlar)

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

#FFT - Monthly
fft = FFT(pcard_data_agg$Trans.Amt)

pred = predict(fft)

data_by_month = pcard_data %>%
  group_by(Invoice.Date) %>% 
  summarise(y = sum(Trans.Amt)) %>% 
  mutate(y_std = c(0, diff(y)))

y = data_by_month$y_std
#Plot it to show the hourly by day to find the trends within each day of the week
FFT = fft(y)

#Magnitude of the results
magnitude_fft <-Mod(FFT)
hist(magnitude_fft[2:8689], main = "Magnitude of FFT", xlab = "Magnitude per Index")

#Phase
phase_fft <- Arg(FFT)

#Frequency - FFT
n <- length(FFT)
fs <- 1  # Sampling frequency
freqs <- (0:(n/2))/ n
power <- Mod(FFT[1:(n/2 + 1)])^2  # Power spectrum

# Convert frequencies to periods (inverse of frequency)
periods <- 1 / freqs

# Plot periodogram
data_fft <- tibble(
  period = periods,
  power = power) %>%
  filter(is.finite(period))  # Remove infinite values

ggplot(data_fft, aes(x = period, y = (power) )) +
  geom_line() +
  scale_x_log10() +  # Log scale to better visualize
  labs(title = "Periodogram (FFT)", x = "Period (Days)", y = "Power") +
  theme_minimal()

# Zero out small frequencies to keep only dominant components
threshold <- 0.1 * max(Mod(FFT))  # Keep only significant frequencies
fft_filtered <- ifelse(Mod(FFT) > threshold, FFT, 0)

# Inverse FFT to reconstruct the signal
reconstructed_signal <- Re(fft(fft_filtered, inverse = TRUE) / n)

# convert model fit (on stationary 'diffed' series) to counts
reconstructed_signal[1] = 985
yhat = cumsum(reconstructed_signal)
with(data, plot(date, y, type = "l", ylim = c(-2000, 9000)))+
  lines(x = data$date, y = yhat, col = "blue")
```
