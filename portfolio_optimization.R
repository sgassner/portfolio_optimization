#------------------------------------------------------------------------------#
# Packages
#------------------------------------------------------------------------------#

# Notwendige Pakete laden
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(quadprog)
library(zoo)
library(PortfolioAnalytics)
library(IntroCompFinR)
library(lubridate)

#------------------------------------------------------------------------------#
# Data Cleaning
#------------------------------------------------------------------------------#

# define working directory
wd <- "~/Documents/R/portfolio"

# set working directory
setwd(wd)

# CSVs einlesen
equities <- read.csv("equities.csv")
bonds <- read.csv("bonds.csv")
commodity <- read.csv("commodity.csv")
real_estate <- read.csv("real_estate.csv")
gold <- read.csv("gold.csv")
crypto <- read.csv("crypto.csv")

# Verwende eine Schleife, um die Datumsspalte in jedem Data Frame zu formatieren
equities <- equities %>% mutate(Datum = parse_date_time(Datum, orders = "my"))
bonds <- bonds %>% mutate(Datum = parse_date_time(Datum, orders = "my"))
commodity <- commodity %>% mutate(Datum = parse_date_time(Datum, orders = "my"))
real_estate <- real_estate %>% mutate(Datum = parse_date_time(Datum, orders = "my"))
gold <- gold %>% mutate(Datum = parse_date_time(Datum, orders = "my"))
crypto <- crypto %>% mutate(Datum = parse_date_time(Datum, orders = "my"))

# Datum in einen DataFrame zusammenführen
data <- merge(equities, bonds, by = "Datum", all = FALSE)
data <- merge(data, commodity, by = "Datum", all = FALSE)
data <- merge(data, real_estate, by = "Datum", all = FALSE)
data <- merge(data, gold, by = "Datum", all = FALSE)
data <- merge(data, crypto, by = "Datum", all = FALSE)

# Spaltennamen anpassen
assets <- c("equities", "bonds", "commodity", "real_estate", "gold", "crypto")
colnames(data) <- c("Datum", "equities", "bonds", "commodity", "real_estate", "gold", "crypto")

# Datum in ein passendes Format umwandeln und nach Datum sortieren
data <- data %>% arrange(Datum)

#------------------------------------------------------------------------------#
# Asset Descriptives
#------------------------------------------------------------------------------#

# Daten in das Long Format transformieren
data_long <- data %>% gather("assets", "Price", -Datum)

# Berechnung der monatlichen log returns
data_long <- data_long %>%
  group_by(assets) %>%
  mutate(r_log = log(Price / lag(Price)),  # Logarithmische Monatsrenditen
         r_month = (Price - lag(Price)) / lag(Price)) %>%  # Diskrete Monatsrenditen
  filter(!is.na(r_log))  # Entferne NA-Werte, die durch lag() entstehen

# Berechnung der durchschnittlichen log returns und diskreten Renditen (annualisiert)
assets_descriptives <- data_long %>%
  group_by(assets) %>%
  summarise(
    mean_return_log = mean(r_log),  # Durchschnittliche log Rendite (arithmetisch)
    annual_mean_return_log = mean_return_log * 12,  # Annualisierte log Rendite
    mean_return_arith = mean(r_month),  # Durchschnittliche diskrete Rendite (arithmetisch)
    annual_mean_return_arith = mean_return_arith * 12,  # Annualisierte diskrete Rendite
    sd_log = sd(r_log),  # Standardabweichung der log Rendite (monatlich)
    annual_sd_log = sd_log * sqrt(12),  # Annualisierte Standardabweichung der log Rendite
    sd_arith = sd(r_month),  # Standardabweichung der diskreten Rendite (monatlich)
    annual_sd_arith = sd_arith * sqrt(12)  # Annualisierte Standardabweichung der diskreten Rendite
  ) %>%
  mutate_if(is.numeric, round, digits = 4)

# Ergebnis anzeigen
assets_descriptives

#------------------------------------------------------------------------------#
# Covariance Matrix
#------------------------------------------------------------------------------#

# Daten in das Wide-Format transformieren, um die Kovarianzmatrix zu berechnen
returns_wide <- data_long %>% select(Datum, assets, r_log) %>% spread(key = assets, value = r_log)

# Entferne NA-Werte, die durch die Transformation entstehen
returns_wide <- returns_wide %>% filter(complete.cases(returns_wide))

# Berechnung der Kovarianzmatrix (nur mit log Renditen)
covariance_matrix <- cov(returns_wide %>% select(-Datum))

#------------------------------------------------------------------------------#
# Correlation Analysis
#------------------------------------------------------------------------------#

# Berechnung der Korrelationsmatrix zwischen den Assets
correlation_matrix <- cor(returns_wide %>% select(-Datum))
correlation_matrix

#------------------------------------------------------------------------------#
# Risk Free Rate
#------------------------------------------------------------------------------#

# define risk free rates (angepasst auf realistischere Werte)
rf_1 <- 0  # 2% jährlicher risikofreier Zinssatz

#------------------------------------------------------------------------------#
# Portfolio Optimization
#------------------------------------------------------------------------------#

# Minimum Variance Portfolio für alle Assets
er_all <- assets_descriptives$annual_mean_return_log  # Verwende die annualisierten log Mittelwerte
names(er_all) <- assets_descriptives$assets

gm_all <- globalMin.portfolio(er_all, covariance_matrix, shorts = FALSE)

# Tangency Portfolio für alle Assets
er_all <- assets_descriptives$annual_mean_return_log
er_all[er_all < 0] <- 0
tp_all <- tangency.portfolio(er_all, covariance_matrix, risk.free = rf_1, shorts = FALSE)

#------------------------------------------------------------------------------#
# Efficient frontier
#------------------------------------------------------------------------------#

# Berechnung der Mean-Variance-Efficient Frontier
ef <- efficient.frontier(er_all, covariance_matrix, nport = 100, alpha.min = -10, alpha.max = 10, shorts = FALSE)

# Plot der Mean-Variance Efficient Frontier mit erweiterten Achsenlimits
plot(ef$sd, ef$er, type = "l", col = "blue", lwd = 2, xlab = "Risk (Standard Deviation)", ylab = "Expected Return",
     xlim = c(-0.1, max(ef$sd) * 5), ylim = c(-0.025, max(ef$er) * 5))

# Hinzufügen des globalen Minimum-Varianz-Portfolios
points(gm_all$sd, gm_all$er, col = "green", pch = 16, cex = 2)
text(gm_all$sd, gm_all$er, labels = "GLOBAL MIN", pos = 2, col = "green")

# Hinzufügen des Tangency Portfolios
points(tp_all$sd, tp_all$er, col = "red", pch = 16, cex = 2)
text(tp_all$sd, tp_all$er, labels = "TANGENCY", pos = 2, col = "red")

# Hinzufügen der einzelnen Assets als Punkte im Plot der Efficient Frontier
for (i in 1:nrow(assets_descriptives)) {
  points(assets_descriptives$annual_sd_log[i], assets_descriptives$annual_mean_return_log[i], col = "darkorange", pch = 17)
  text(assets_descriptives$annual_sd_log[i], assets_descriptives$annual_mean_return_log[i], labels = assets_descriptives$assets[i], pos = 4, cex = 0.8, col = "darkorange")
}

# Hinzufügen der Capital Market Line
sr.tan = (tp_all$er - rf_1) / tp_all$sd
abline(a = rf_1, b = sr.tan, col = "red", lwd = 2)

#------------------------------------------------------------------------------#
# Results
#------------------------------------------------------------------------------#

# Ergebnisse anzeigen
optimal_weights <- data.frame(assets = assets_descriptives$assets, optimal_weights = tp_all$weights * 100)
optimal_weights
optimal_portfolio_return <- data.frame(expected_return = tp_all$er, standard_deviation = tp_all$sd)
optimal_portfolio_return

# Sharpe Ratio Berechnungen mit diskreten Renditen
sharpe_ratios <- data.frame(assets = c(assets_descriptives$assets, "optimal_portfolio"),
                            sharpe_ratio = c(((assets_descriptives$annual_mean_return_arith - rf_1) / assets_descriptives$annual_sd_arith), 
                                             (optimal_portfolio_return$expected_return - rf_1) / optimal_portfolio_return$standard_deviation))

# Sharpe Ratios anzeigen
sharpe_ratios
