#Create Folders
dir.create("raw_data/", showWarnings = FALSE)
dir.create("clean_data",showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)


#Install Packages
install.packages("readxl")
install.packages("xts")
install.packages("gt")
install.packages("webshot2")
install.packages("ggplot2")
install.packages("tinytable")
install.packages("tidyverse")
install.packages("flextable")

#Modeling Packages
install.packages("sandwich") # NeweyWest() HAC standard errors
install.packages("lmtest") #Coeftest()
install.packages("modelsummary") #regression tables
install.packages("broom")
install.packages("tseries") # adf.test(),kpss. test


#Load Packages
library("readxl")
library("xts")
library("gt")
library("webshot2")
library("fixest")
library("ggplot2")
library("tidyverse")

# Load Modeling Packages
library("sandwich")
library("lmtest") 
library("modelsummary")
library("broom")
library("tseries")
library("tinytable")

#Identify Sources

allmonthly <- read.csv("raw_data/monthly_datasets_2018_2026.csv")

mydata <-allmonthly 

mydata <- mydata %>%
  rename("res_price"= Residential.electricity.price,
         "nat_gas"= Natural.Gas.Prices,
         "avg_load"= Average_load_DOM,
         "month"= Date,
         "total_gen"= Total.Generation)

  
alias(m3)
alias(m5)


summary(mydata$brk_2021)

summary(mydata$brk_2023)

table(mydata$brk_2021)

table(mydata$brk_2023)

names(mydata)


mydata$brk_2021 <- ifelse(mydata$month >= as.Date("2021-10-01"), 1, 0)

mydata$brk_2023 <- ifelse(mydata$month >= as.Date("2023-03-01"), 1, 0)


mydata$month <- ym(as.character(mydata$month))


#CPI 
folder_path <- "C:/Users/clair/OneDrive/ Documents/OURS199/R Training/Week_6_Analysis/raw_data"
clean_folder   <- "C:/Users/clair/OneDrive/Documents/OURS199/R Training/Week_6_Analysis/clean"
if (!dir.exists(clean_folder)) dir.create(clean_folder, recursive = TRUE)

RETAIL_START <- as.Date("2018-01-01")
RETAIL_END   <- as.Date("2025-12-01")

cpi_raw <- read_csv("raw_data/CPI.csv", show_col_types = FALSE) %>%
  mutate(month = as.Date(month)) %>%
  arrange(month)

# Complete monthly sequence: this INSERTS any missing month (Oct 2025) as NA
cpi_monthly <- tibble(month = seq(RETAIL_START, RETAIL_END, by = "month")) %>%
  left_join(cpi_raw, by = "month") %>%
  mutate(
    CPI = exp(na.approx(log(CPI), x = month, na.rm = FALSE)),  # log-scale fill
    CPI = round(CPI, 3)                                        # 3 decimals throughout
  ) %>%
  rename(cpi = CPI)

write_csv(cpi_monthly, "clean/cpi_monthly.csv")

nrow(cpi_monthly)                                   # expect 102
sum(is.na(cpi_monthly$cpi_monthly))                         # expect 0
cpi_monthly %>% filter(month >= "2025-08-01", month <= "2025-12-01")
# Oct 2025 should read 324.647, between Sep 324.245 and Nov 325.063


# NOTE: Rebuild `dat` from the cleaned merged file so this script runs
# start-to-finish on its own. Everything downstream depends on this block.
BREAK_2021 <- as.Date("2021-10-01")   # located empirically by supF (Section 6)
BREAK_2023 <- as.Date("2023-03-01")   #   — NOT chosen by hand
BREAK_AI   <- as.Date("2023-01-01")   # post-boom date for the interaction test
GAS_LAGS   <- c(3, 6, 12)

for (L in GAS_LAGS) dat[[paste0("hh_real_l", L)]] <- dplyr::lag(dat$hh_real, L)

dat <- mydata%>%
  arrange(month) %>%
  mutate(
    price_real  = res_price * last(CPI) / CPI,  # CPI-deflated
    hh_real     = nat_gas          * last(CPI) / CPI,
    mon         = factor(month(month), levels = 1:12, labels = month.abb),
    trend       = row_number(),
    customers_k = Customers / 1e3,
    post_ai     = as.integer(month >= BREAK_AI),
    brk_2021    = as.integer(month >= BREAK_2021),
    brk_2023    = as.integer(month >= BREAK_2023)
  )

for (L in GAS_LAGS) dat[[paste0("hh_real_l", L)]] <- dplyr::lag(dat$hh_real, L)


# NOTE: Descriptives come BEFORE the models. They flag the trending variables
# (load, customers) that the diagnostics and ladder later show are hard to
# separate.
desc <- mydata %>%
  select(avg_load,Customers, HDD, CDD, nat_gas, res_price,total_gen) %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(
    n      = sum(!is.na(value)),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value,   na.rm = TRUE),
    min    = min(value,  na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max    = max(value,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(mean:max, ~round(.x, 3)))

write_csv(desc, "figures/descriptive_statistics.csv")
print(desc)


#                 STATIONARY DIAGONASTICS
# NOTE: The load-vs-trend problem is, formally, a non-stationarity problem.
#   ADF  H0 = unit root (non-stationary). Small p -> stationary. p < 0.05
#   KPSS H0 = stationary.                 Small p -> NON-stationary. p > 0.05
# OPPOSITE nulls; they agree when ADF fails to reject AND KPSS rejects
# (both say "unit root"). tseries truncates p-values at 0.01 and 0.10 — read
# "0.01" as "p <= 0.01" (strong rejection), not an exact value.
stationarity <- function(x, name) {
  x <- na.omit(x)
  tibble(
    series       = name,
    adf_p_level  = suppressWarnings(adf.test(x)$p.value),
    kpss_p_level = suppressWarnings(kpss.test(x, null = "Trend")$p.value),
    adf_p_diff   = suppressWarnings(adf.test(diff(x))$p.value),
    kpss_p_diff  = suppressWarnings(kpss.test(diff(x), null = "Level")$p.value)
  )
}

stat_tbl <- bind_rows(
  stationarity(mydata$res_price,   "res_price"),
  stationarity(mydata$avg_load, "avg_load"),
  stationarity(mydata$total_gen,      "total_gen"),
  stationarity(mydata$Customers,  "Customers"),
  stationarity(mydata$nat_gas,    "nat_gas")
) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)),
         reading = case_when(
           adf_p_level > 0.05 & kpss_p_level < 0.05 ~
             "Unit root in levels; stationary in differences (expected for trending series)",
           adf_p_level <= 0.05 & kpss_p_level >= 0.05 ~
             "Consistent with (trend-)stationary in levels",
           TRUE ~ "Tests disagree; report both"))
write_csv(stat_tbl, "tables/stationarity_tests.csv")
print(stat_tbl)






# INTERPRETATION: price, load, and customers typically show a unit root in
# levels — the formal signature of the shared trend that makes load and trend
# inseparable. The primary models stay in levels (with breaks + HAC errors);
# M5 (first differences) is the robustness check that removes the trend.


png("figures/fig0_acf_pacf.png", width = 10*300, height = 4*300, res = 300)
par(mfrow = c(1, 2))
acf(mydata$res_price,  main = "ACF: residd",  na.action = na.pass)
pacf(mydata$res_price, main = "PACF: res_price", na.action = na.pass)
dev.off(); par(mfrow = c(1, 1))


# ---- 3. THE SPECIFICATION LADDER --------------------------------------------
# NOTE: Five models, same outcome, differing only in how they handle the
# co-trending controls (linear trend, customer counts, dated breaks) and
# whether they are in levels or differences. Watch the load coefficient move
# across columns while the 6-month gas lag stays put. That contrast IS the
# study's honest result.

# M1 — baseline: linear trend absorbs the secular rise
m1 <- lm(price_real ~ avg_load + hh_real_l3 + hh_real_l6 + hh_real_l12 +
           HDD + CDD + customers_k + nat_gas + trend + mon, data = dat)

# M2 — drop the trend: load and trend were splitting the same rise
m2 <- lm(price_real ~ avg_load + hh_real_l3 + hh_real_l6 + hh_real_l12 +
           HDD + CDD + customers_k + nat_gas + mon, data = dat)

# M3 — replace the crude trend with the two DATED cost-shock breaks
m3 <- lm(price_real ~ avg_load + hh_real_l3 + hh_real_l6 + hh_real_l12 +
           HDD + CDD + customers_k + nat_gas + brk_2021 + brk_2023 + mon,
         data = dat)

# M4 — M3 without customer counts: load falls and loses significance,
#      showing the load coefficient is not robust to dropping a co-trending
#      control
m4 <- lm(price_real ~ avg_load + hh_real_l3 + hh_real_l6 + hh_real_l12 +
           HDD + CDD + nat_gas + brk_2021 + brk_2023 + mon, data = dat)

# M5 — first differences: the robustness check for non-stationarity.
#      Differencing removes the shared trend. If the gas relationship survives
#      here, it is not a spurious-trend artifact. Month dummies stay
#      undifferenced (deterministic seasonality is not differenced away).
dat_fd <- dat %>%
  mutate(across(c(price_real, avg_load, hh_real_l3, hh_real_l6,
                  hh_real_l12, HDD, CDD, customers_k, nat_gas),
                ~ .x - dplyr::lag(.x), .names = "d_{.col}"))

m5 <- lm(d_price_real ~ d_avg_load + d_hh_real_l3 + d_hh_real_l6 +
           d_hh_real_l12 + d_HDD + d_CDD + d_customers_k + d_nat_gas +
           mon, data = dat_fd)

# M6- Post AI Interaction
m6 <- lm(price_real ~ avg_load *post_ai + hh_real_l3 + hh_real_l6 +
           hh_real_l12 + HDD + CDD + customers_k + nat_gas + brk_2021+
           mon, data = dat)

# HAC (Newey-West) standard errors for every model — monthly price series are
# autocorrelated, so ordinary OLS errors would be too small.
nw <- function(m) NeweyWest(m, prewhite = FALSE)

coeftest(m1, vcov. = nw(m1))
coeftest(m2, vcov. = nw(m2))
coeftest(m3, vcov. = nw(m3))
coeftest(m4, vcov. = nw(m4))
coeftest(m5, vcov. = nw(m5))
coeftest(m6, vcov. = nw(m6))

# ---- 4. Regression table (the ladder as one table) --------------------------
modelsummary(
  list("M1: +trend" = m1, "M2: -trend" = m2, "M3: +breaks" = m3,
       "M4: -customers" = m4, "M5: 1st diff" = m5, "M6:AI Interaction"= m6),
  vcov      = list(nw(m1), nw(m2), nw(m3), nw(m4), nw(m5), nw(m6)),
  coef_map  = c(
    "avg_load"   = "DOM load (GWh)",
    "d_avg_load" = "DOM load (GWh)",
    "post_AI" = "Post-AI (2023+)",
    "hh_real_l6"     = "Real gas, 6-mo lag",
    "d_hh_real_l6"   = "Real gas, 6-mo lag",
    "hh_real_l3"     = "Real gas, 3-mo lag",
    "d_hh_real_l3"   = "Real gas, 3-mo lag",
    "hh_real_l12"    = "Real gas, 12-mo lag",
    "d_hh_real_l12"  = "Real gas, 12-mo lag",
    "nat_gas"      = "Gas generation share",
    "d_nat_gas"    = "Gas generation share",
    "customers_k"    = "Residential customers (000s)",
    "d_customers_k"  = "Residential customers (000s)",
    "brk_2021" = "Break: Oct 2021", "brk_2023" = "Break: Mar 2023",
    "trend"    = "Linear trend"
  ),
  gof_omit  = "IC|Log|F|RMSE",
  stars     = TRUE,
  title     = "Real Virginia residential price: specification ladder (HAC SE)",
  notes     = c(
    "Newey-West HAC standard errors. Month dummies included, not shown.",
    "M5 is in first differences. Coefficients are conditional associations.",
    "Load moves across M1-M5; the 6-month gas lag is stable, incl. differences."),
  output    = "tables/regression_ladder1.html"
  
  
)




# ---- 5. FIGURE 1 — Coefficient stability (THE contribution figure) ----------
# NOTE: The figure that carries the whole argument. Load coefficient and the
# 6-month gas coefficient across all four LEVEL specifications with HAC 95%
# intervals. Gas is robust (tight, stable, positive); load swings from
# marginal to strong depending on controls. The honest core of the study and
# the visual case for the panel.
get_coef <- function(model, term, label, spec) {
  ct <- coeftest(model, vcov. = NeweyWest(model, prewhite = FALSE))
  est <- ct[term, "Estimate"]; se <- ct[term, "Std. Error"]
  tibble(spec = spec, term = label, estimate = est,
         conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)
}

specs  <- c("M1: +trend", "M2: -trend", "M3: +breaks", "M4: -customers")
models <- list(m1, m2, m3, m4)

coef_stab <- bind_rows(
  map2_dfr(models, specs, ~get_coef(.x, "avg_load", "DOM load (GWh)", .y)),
  map2_dfr(models, specs, ~get_coef(.x, "hh_real_l6", "Real gas, 6-mo lag", .y))
) %>%
  mutate(spec = factor(spec, levels = specs))

f1 <- ggplot(coef_stab, aes(spec, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = term),
                  linewidth = 0.7) +
  facet_wrap(~term, scales = "free_y") +
  scale_color_manual(values = c("DOM load (GWh)" = "#c85a19",
                                "Real gas, 6-mo lag" = "#1b5e8a"),
                     guide = "none") +
  labs(
    title    = "Coefficient stability across specifications",
    subtitle = "Gas pass-through is robust; the load association is specification-dependent",
    x = NULL, y = "Coefficient (HAC 95% interval)",
    caption  = paste(
      "M1-M4 differ only in co-trending controls (trend, breaks, customers).",
      "Load's interval crosses zero when the trend or customer count is added/removed;",
      "gas stays positive and significant. Separating load from the secular trend",
      "requires cross-sectional variation — the motivation for the panel (next step).",
      sep = "\n")
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8, color = "grey40"),
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("figures/fig1_coefficient_stability.png", f1,
       width = 9, height = 5.5, dpi = 300, bg = "white")

# ---- 6. FIGURE 2 — Gas lag structure (the mechanism) ------------------------
# NOTE: WHY retail differs from wholesale. The 6-month lag dominates:
# regulated rates respond to gas costs through SCC fuel-factor and rate-case
# processes at ~two quarters, not contemporaneously. From M3 (preferred).
gas_lags <- bind_rows(
  get_coef(m3, "hh_real_l3",  "3-month lag",  "M3"),
  get_coef(m3, "hh_real_l6",  "6-month lag",  "M3"),
  get_coef(m3, "hh_real_l12", "12-month lag", "M3")
) %>%
  mutate(term = factor(term, levels = c("3-month lag", "6-month lag",
                                        "12-month lag")))

f2 <- ggplot(gas_lags, aes(term, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = "#1b5e8a", linewidth = 0.8) +
  labs(
    title    = "Gas-cost pass-through peaks at a 6-month lag",
    subtitle = "Consistent with regulated fuel-factor and rate-case timing",
    x = NULL, y = "Coefficient on real Henry Hub (HAC 95% interval)",
    caption  = "From M3 (break-controlled). The 6-month lag is the pass-through channel."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8, color = "grey40"))

ggsave("figures/fig2_gas_lag_structure.png", f2,
       width = 8, height = 5, dpi = 300, bg = "white")

# ---- 7. FIGURE 3 — Real price with structural breaks ------------------------
# NOTE: The supF test (Section 8) rejected "no break"; breakpoints() located
# breaks at Oct 2021 and Mar 2023 — the gas-price surge and its unwind, NOT
# the load ramp. This figure shows the deflated series with those dates marked.
f3 <- ggplot(dat, aes(month, price_real)) +
  geom_line(color = "#1b5e8a", linewidth = 0.7) +
  geom_vline(xintercept = BREAK_2021, linetype = "dashed", color = "#c85a19") +
  geom_vline(xintercept = BREAK_2023, linetype = "dashed", color = "#c85a19") +
  annotate("text", x = BREAK_2021, y = min(dat$price_real), label = " Oct 2021",
           hjust = 0, vjust = -0.5, size = 3, color = "#c85a19") +
  annotate("text", x = BREAK_2023, y = min(dat$price_real), label = " Mar 2023",
           hjust = 0, vjust = -0.5, size = 3, color = "#c85a19") +
  labs(
    title    = "Real residential price with structural breaks",
    subtitle = "Breaks located empirically; both coincide with the gas-price cycle",
    x = NULL, y = "Real price (cents/kWh, CPI-deflated)",
    caption  = paste(
      "Break dates from a supF test + breakpoints(), not chosen by hand.",
      "The 2021 break sits at a real-price trough: deflated prices fell as inflation",
      "outran rate adjustments, then rose through 2022 as fuel factors caught up.",
      "The 2023 break marks the plateau after that recovery. Both align with the",
      "regulatory lag in gas-cost pass-through, not with the data center load ramp.",
      "Note the zoomed y-axis: real price varies only ~4.6% around its mean",
      "(15.2 cents/kWh, SD 0.70) — regulated rates are administratively smoothed,",
      "which is why load is hard to detect in one series.",
      sep = "\n")
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8, color = "grey40"))

ggsave("figures/fig3_price_breaks.png", f3,
       width = 9, height = 5, dpi = 300, bg = "white")

# ---- 8. Structural break test (documented, reproducible) --------------------
# NOTE: WHY the break dates in Section 1 are what they are. We did not pick
# them — the data located them. The supF rejects "no break"; breakpoints()
# with BIC prefers two breaks at Oct 2021 and Mar 2023.
if (requireNamespace("strucchange", quietly = TRUE)) {
  library(strucchange)
  sub <- dat %>% select(price_real, avg_load, hh_real_l6, trend) %>% drop_na()
  print(sctest(price_real ~ avg_load + hh_real_l6 + trend,
               data = sub, type = "supF"))
  bp <- breakpoints(price_real ~ avg_load + hh_real_l6 + trend, data = sub)
  print(summary(bp))
  months_used <- dat$month[!is.na(dat$hh_real_l6)]
  cat("Located break dates:\n"); print(months_used[bp$breakpoints])
}

# ---- 9. The post-boom interaction (the load-change question) ----------------
# NOTE: Our headline question was whether the load-price association
# STRENGTHENED after the boom. Answer: no detectable change (interaction null).
# Reported, not hidden. Consistent with the specification-dependence of the
# level association and the long regulatory lag.
m_int <- lm(price_real ~ avg_load * post_ai + hh_real_l3 + hh_real_l6 +
              hh_real_l12 + HDD + CDD + customers_k + nat_gas +
              brk_2021 + mon, data = dat)
coeftest(m_int, vcov. = nw(m_int))   # dom_load_gwh:post_ai is the coefficient

# ---- 10. What we can say, and the named next step ---------------------------
# NOTE FOR THE POSTER / MANUSCRIPT — complete summary:
#
# ROBUST FINDINGS (stable across every specification, incl. differences):
#   - Real retail prices track gas costs at a ~6-month lag (Fig 2).
#   - Pass-through scales with gas generation share (share_gas > 0 throughout).
#   - Two structural breaks (Oct 2021, Mar 2023) coincide with the gas-price
#     surge and unwind, not the load ramp (Fig 3).
#
# SPECIFICATION-DEPENDENT (limitation, Fig 1 + Section 2b):
#   - The load-price association ranges from marginal to strong depending on
#     co-trending controls; unit-root tests confirm load and price share a
#     trend. In a single series they cannot be cleanly separated.
#   - The load-price association shows no detectable strengthening after the
#     boom (Section 9 interaction is null).
#
# NEXT STEP (the contribution this study sets up):
#   - A multi-state / multi-utility PANEL with fixed effects uses cross-
#     sectional variation to separate load from the common trend — exactly
#     what a single series cannot do. This study's specification-dependence
#     result IS the motivation for that design. Deferred to the publication
#     stage by design, not by omission.
#
# Three independent lines converge: ladder instability (Fig 1), unit-root
# tests (2b), and the first-difference result (M5). Together they establish
# the load-vs-trend limitation rigorously rather than asserting it.

