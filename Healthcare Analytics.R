# ============================================================
# Hospital Patient Analytics — 10 Business Questions
# Author: Oluwatobi A. Alawode, PhD
# Date: May 2026
# Tools: R (tidyverse, ggplot2, rstatix, scales, patchwork)
# Data: Healthcare_Analytics.xlsx
# ============================================================

##Healthcare data analytics project
# Install packages (run once)
install.packages(c('tidyverse', 'readxl', 'rstatix', 'scales', 'ggthemes'))

# Load packages (run every session)
library(tidyverse)
library(readxl)
library(rstatix)
library(scales)
library(ggthemes)

# Load data
df <- read_excel("Healthcare Analytics_Working.xlsx")

# Inspect
glimpse(df)
dim(df)          # Should show 55500 rows, 16 columns
names(df)        # Check column names

# Clean column names (remove spaces)
df <- df %>%
  rename(
    Medical_Condition  = `Medical Condition`,
    Date_Admission     = `Date of Admission`,
    Discharge_Date     = `Discharge Date`,
    Admission_Type     = `Admission Type`,
    Insurance_Provider = `Insurance Provider`,
    Billing_Amount     = `Billing Amount`,
    Test_Results       = `Test Results`
  )

# Create derived variables
df <- df %>%
  mutate(
    Date_Admission  = as.Date(Date_Admission),
    Discharge_Date  = as.Date(Discharge_Date),
    LOS             = as.numeric(Discharge_Date - Date_Admission),
    Billing_Amount  = pmax(Billing_Amount, 0),   # clip negatives
    Age_Group       = cut(Age,
                          breaks = c(0, 34, 49, 64, 89),
                          labels = c('<=34', '35-49', '50-64', '65+'),
                          right  = TRUE)
  )

# Confirm
summary(df$LOS)
summary(df$Billing_Amount)
table(df$Age_Group)

# Q1 & Q2: Billing by Insurance Provider
ins_summary <- df %>%
  group_by(Insurance_Provider) %>%
  summarise(
    Total_Revenue  = sum(Billing_Amount),
    Avg_Billing    = mean(Billing_Amount),
    Median_Billing = median(Billing_Amount),
    Encounters     = n()
  ) %>%
  arrange(desc(Total_Revenue))

print(ins_summary)

# Q1 Chart — Total Revenue
ggplot(ins_summary,
       aes(x = reorder(Insurance_Provider, Total_Revenue),
           y = Total_Revenue / 1e6)) +
  geom_col(fill = '#2E6DA4', alpha = 0.85) +
  geom_text(aes(label = paste0('$', round(Total_Revenue/1e6, 1), 'M')),
            hjust = -0.1, size = 3.5, fontface = 'bold') +
  coord_flip() +
  labs(title    = 'Q1: Total Revenue by Insurance Provider',
       subtitle = '55,500 patient encounters, 2019-2024',
       x = NULL, y = 'Total Billing (USD Millions)') +
  theme_minimal(base_size = 12) +
  expand_limits(y = max(ins_summary$Total_Revenue/1e6) * 1.15)

# Q2 Chart — Average Billing
overall_avg <- mean(df$Billing_Amount)

ggplot(ins_summary,
       aes(x = reorder(Insurance_Provider, Avg_Billing),
           y = Avg_Billing)) +
  geom_col(fill = '#1A7A6B', alpha = 0.85) +
  geom_hline(yintercept = overall_avg, linetype = 'dashed',
             color = 'red', linewidth = 1) +
  geom_text(aes(label = dollar(round(Avg_Billing))),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(title    = 'Q2: Average Billing per Encounter by Insurance Provider',
       subtitle = paste('Dashed line = overall average:', dollar(round(overall_avg))),
       x = NULL, y = 'Avg Billing per Encounter (USD)') +
  theme_minimal(base_size = 12)



# Q3: Billing by Admission Type
adm_billing <- df %>%
  group_by(Admission_Type) %>%
  summarise(
    Mean_Billing   = mean(Billing_Amount),
    Median_Billing = median(Billing_Amount),
    SD_Billing     = sd(Billing_Amount),
    Encounters     = n()
  ) %>%
  arrange(desc(Mean_Billing))

print(adm_billing)

# Pivot longer for grouped bar chart (mean vs median)
adm_long <- adm_billing %>%
  select(Admission_Type, Mean_Billing, Median_Billing) %>%
  pivot_longer(cols = c(Mean_Billing, Median_Billing),
               names_to  = 'Measure',
               values_to = 'Amount')

ggplot(adm_long,
       aes(x = Admission_Type, y = Amount, fill = Measure)) +
  geom_col(position = 'dodge', alpha = 0.85) +
  geom_hline(yintercept = mean(df$Billing_Amount),
             linetype = 'dashed', color = 'red', linewidth = 1) +
  scale_fill_manual(values = c('#2E6DA4', '#6AAED6'),
                    labels = c('Mean', 'Median')) +
  scale_y_continuous(labels = dollar) +
  labs(title    = 'Q3: Billing by Admission Type (Mean vs Median)',
       subtitle = 'Red dashed line = overall average',
       x = NULL, y = 'Billing Amount (USD)', fill = NULL) +
  theme_minimal(base_size = 12)


# Q4: Billing by Medical Condition
cond_billing <- df %>%
  group_by(Medical_Condition) %>%
  summarise(
    Avg_Billing   = mean(Billing_Amount),
    Total_Billing = sum(Billing_Amount),
    Encounters    = n()
  ) %>%
  arrange(desc(Avg_Billing))

print(cond_billing)

cond_colors <- c('#C0392B','#D97B2B','#2E6DA4',
                 '#1A7A6B','#7D3C98','#27AE60')

ggplot(cond_billing,
       aes(x = reorder(Medical_Condition, Avg_Billing),
           y = Avg_Billing,
           fill = Medical_Condition)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_vline(xintercept = mean(df$Billing_Amount),
             linetype = 'dashed', color = 'red', linewidth = 1) +
  geom_text(aes(label = dollar(round(Avg_Billing))),
            hjust = -0.1, size = 3.5, fontface = 'bold') +
  scale_fill_manual(values = cond_colors) +
  scale_y_continuous(labels = dollar) +
  coord_flip() +
  labs(title = 'Q4: Average Billing by Medical Condition',
       x = NULL, y = 'Avg Billing per Encounter (USD)') +
  theme_minimal(base_size = 12) +
  expand_limits(y = max(cond_billing$Avg_Billing) * 1.15)


# Q5: Length of Stay by Admission Type
los_summary <- df %>%
  group_by(Admission_Type) %>%
  summarise(
    Mean_LOS   = mean(LOS),
    Median_LOS = median(LOS),
    SD_LOS     = sd(LOS),
    Min_LOS    = min(LOS),
    Max_LOS    = max(LOS),
    Encounters = n()
  )

print(los_summary)

# Boxplot — best way to show LOS distribution
adm_colors <- c('Elective' = 'slategrey',
                'Emergency' = 'bisque2',
                'Urgent'    = 'grey38')

ggplot(df, aes(x = Admission_Type, y = LOS, fill = Admission_Type)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.2,
               outlier.size = 1) +
  geom_hline(yintercept = mean(df$LOS), linetype = 'dashed',
             color = 'navy', linewidth = 1) +
  scale_fill_manual(values = adm_colors) +
  annotate('text', x = 0.6, y = mean(df$LOS) + 0.5,
           label = paste('Overall avg:', round(mean(df$LOS), 1), 'days'),
           color = 'navy', size = 3.5) +
  labs(title    = 'Length of Stay Distribution by Admission Type',
       subtitle = 'Dashed line = overall average LOS',
       x = NULL, y = 'Length of Stay (Days)') +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'none')


# Q6: Age vs LOS and Billing

# Correlation tests
cor.test(df$Age, df$LOS,            method = 'pearson')
cor.test(df$Age, df$Billing_Amount, method = 'pearson')

# Age group summary
age_summary <- df %>%
  group_by(Age_Group) %>%
  summarise(
    Avg_LOS     = mean(LOS),
    Avg_Billing = mean(Billing_Amount),
    Encounters  = n()
  )

print(age_summary)

# Dual-axis chart: LOS bars + Billing line
# Step 1: get scaling factor
scale_factor <- max(age_summary$Avg_LOS) / max(age_summary$Avg_Billing)

ggplot(age_summary, aes(x = Age_Group)) +
  geom_col(aes(y = Avg_LOS), fill = '#2E6DA4',
           alpha = 0.7, width = 0.5) +
  geom_line(aes(y = Avg_Billing * scale_factor, group = 1),
            color = '#D97B2B', linewidth = 1.8) +
  geom_point(aes(y = Avg_Billing * scale_factor),
             color = '#D97B2B', size = 4) +
  scale_y_continuous(
    name = 'Avg Length of Stay (Days)',
    sec.axis = sec_axis(~ . / scale_factor,
                        name   = 'Avg Billing (USD)',
                        labels = dollar)
  ) +
  labs(title    = 'Q6: LOS and Billing by Age Group',
       subtitle = 'Bars = LOS (left axis)  |  Line = Billing (right axis)',
       x = 'Age Group') +
  theme_minimal(base_size = 12)

##Q6

# ── Compute correlation values first ─────────────────────────
r_los  <- cor.test(df$Age, df$LOS)
r_bill <- cor.test(df$Age, df$Billing_Amount)

# ── Plot 1: Age vs LOS ────────────────────────────────────────
p1 <- ggplot(df, aes(x = Age, y = LOS)) +
  geom_point(alpha = 0.07, color = "#2E6DA4", size = 0.8) +
  geom_smooth(method = "lm", color = "#C0392B",
              linewidth = 1.8, se = TRUE,
              fill = "#F1948A", alpha = 0.3) +
  annotate("text",
           x    = 75,
           y    = max(df$LOS) * 0.95,
           label = paste0("r = ", round(r_los$estimate, 3),
                          "\np = ", round(r_los$p.value, 3)),
           size  = 4.5,
           color = "#C0392B",
           fontface = "bold",
           hjust = 0) +
  labs(
    title    = "Q6a: Are Older Patients Associated with Longer Hospital Stays?",
    subtitle = "Each dot = 1 patient  |  Red line = linear trend  |  Shaded band = 95% confidence interval",
    x        = "Patient Age (Years)",
    y        = "Length of Stay (Days)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1B3A5C"),
    plot.subtitle = element_text(color = "gray50", size = 10)
  )





# ── Plot 2: Age vs Billing ────────────────────────────────────
p2 <- ggplot(df, aes(x = Age, y = Billing_Amount)) +
  geom_point(alpha = 0.07, color = "#1A7A6B", size = 0.8) +
  geom_smooth(method = "lm", color = "#D97B2B",
              linewidth = 1.8, se = TRUE,
              fill = "#FAD7A0", alpha = 0.3) +
  annotate("text",
           x    = 75,
           y    = max(df$Billing_Amount) * 0.95,
           label = paste0("r = ", round(r_bill$estimate, 3),
                          "\np = ", round(r_bill$p.value, 3)),
           size  = 4.5,
           color = "#D97B2B",
           fontface = "bold",
           hjust = 0) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title    = "Q6b: Are Older Patients Associated with Higher Billing Costs?",
    subtitle = "Each dot = 1 patient  |  Orange line = linear trend  |  Shaded band = 95% confidence interval",
    x        = "Patient Age (Years)",
    y        = "Billing Amount (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1B3A5C"),
    plot.subtitle = element_text(color = "gray50", size = 10)
  )

# ── Display both plots together ───────────────────────────────
library(patchwork)
p1 / p2



# Q7: Hospital Emergency Volume

# Top 15 hospitals by emergency volume
hosp_emergency <- df %>%
  filter(Admission_Type == 'Emergency') %>%
  count(Hospital, name = 'Emergency_Count') %>%
  arrange(desc(Emergency_Count)) %>%
  slice_head(n = 15)

print(hosp_emergency)

# Chart: Top 15
ggplot(hosp_emergency,
       aes(x = reorder(Hospital, Emergency_Count),
           y = Emergency_Count)) +
  geom_col(fill = '#C0392B', alpha = 0.80) +
  geom_text(aes(label = Emergency_Count),
            hjust = -0.2, size = 3.5, fontface = 'bold') +
  coord_flip() +
  labs(title    = 'Top 15 Hospitals by Emergency Admission Volume',
       subtitle = paste('Total unique hospitals:', n_distinct(df$Hospital)),
       x = NULL, y = 'Emergency Admissions') +
  theme_minimal(base_size = 12) +
  expand_limits(y = max(hosp_emergency$Emergency_Count) * 1.15)

# Emergency rate distribution across all hospitals
hosp_rates <- df %>%
  group_by(Hospital) %>%
  summarise(
    Total      = n(),
    Emergency  = sum(Admission_Type == 'Emergency'),
    Em_Rate    = Emergency / Total * 100
  ) %>%
  filter(Total >= 3)   # exclude hospitals with <3 encounters

ggplot(hosp_rates, aes(x = Em_Rate)) +
  geom_histogram(fill = '#C0392B', alpha = 0.75,
                 bins = 30, color = 'white') +
  geom_vline(xintercept = mean(hosp_rates$Em_Rate),
             color = 'navy', linetype = 'dashed', linewidth = 1.2) +
  labs(title    = 'Distribution of Emergency Admission Rate Across Hospitals',
       subtitle = 'Dashed = mean emergency rate',
       x = 'Emergency Admission Rate (%)', y = 'Number of Hospitals') +
  theme_minimal(base_size = 12)


##Q8: Test results associated with higher billing ot longer stays?
# Summary statistics
test_summary <- df %>%
  group_by(Test_Results) %>%
  summarise(
    Avg_Billing = mean(Billing_Amount),
    Avg_LOS     = mean(LOS),
    Encounters  = n()
  )

print(test_summary)

# One-way ANOVA: Billing by Test Results
anova_billing <- aov(Billing_Amount ~ Test_Results, data = df)
summary(anova_billing)

# One-way ANOVA: LOS by Test Results
anova_los <- aov(LOS ~ Test_Results, data = df)
summary(anova_los)

# Boxplot: Billing by Test Result
test_colors <- c('Normal'='#27AE60', 'Abnormal'='#C0392B',
                 'Inconclusive'='#D97B2B')

ggplot(df, aes(x = Test_Results, y = Billing_Amount,
               fill = Test_Results)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.15,
               outlier.size = 1) +
  geom_hline(yintercept = mean(df$Billing_Amount),
             linetype = 'dashed', color = 'navy', linewidth = 1) +
  scale_fill_manual(values = test_colors) +
  scale_y_continuous(labels = dollar) +
  labs(title    = 'Billing Amount by Test Result',
       subtitle = paste('ANOVA p-value:',
                        round(summary(anova_billing)[[1]][['Pr(>F)']][1], 3)),
       x = NULL, y = 'Billing Amount (USD)') +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'none')

# ── Step 1: Compute summary statistics ───────────────────────
test_billing <- df %>%
  group_by(Test_Results) %>%
  summarise(
    Avg_Billing = mean(Billing_Amount),
    Median_Billing = median(Billing_Amount),
    Encounters  = n()
  ) %>%
  arrange(desc(Avg_Billing))

# View the numbers first
print(test_billing)

# ── Step 2: Run ANOVA (for annotation on chart) ───────────────
anova_billing <- aov(Billing_Amount ~ Test_Results, data = df)
anova_summary <- summary(anova_billing)
p_val <- round(anova_summary[[1]][["Pr(>F)"]][1], 3)
f_val <- round(anova_summary[[1]][["F value"]][1], 2)

# ── Step 3: Define colors ─────────────────────────────────────
test_colors <- c(
  "Abnormal"     = "gray50",
  "Inconclusive" = "peachpuff2",
  "Normal"       = "wheat3"
)

# ── Step 4: Overall average for reference line ────────────────
overall_avg <- mean(df$Billing_Amount)

# ── Step 5: Build the chart ───────────────────────────────────
p_8a <- ggplot(test_billing,
               aes(x    = reorder(Test_Results, Avg_Billing),
                   y    = Avg_Billing,
                   fill = Test_Results)) +
  geom_col(width = 0.55, alpha = 0.85, show.legend = FALSE) +
  
  # Value labels on top of each bar
  geom_text(aes(label = scales::dollar(round(Avg_Billing))),
            vjust    = -0.5,
            size     = 4.5,
            fontface = "bold",
            color    = "#1B3A5C") +
  
  # Encounter count below bar label
  geom_text(aes(label = paste0("n = ", scales::comma(Encounters))),
            vjust = -2.0,
            size  = 3.5,
            color = "gray50") +
  
  # Overall average reference line
  geom_hline(yintercept = overall_avg,
             linetype   = "dashed",
             color      = "navy",
             linewidth  = 1.2) +
  
  # Annotate the reference line
  annotate("text",
           x     = 0.55,
           y     = overall_avg + 120,
           label = paste0("Overall avg: ",
                          scales::dollar(round(overall_avg))),
           color    = "navy",
           size     = 3.8,
           hjust    = 0,
           fontface = "italic") +
  
  # ANOVA result annotation
  annotate("text",
           x     = 3.45,
           y     = max(test_billing$Avg_Billing) * 0.72,
           label = paste0("ANOVA\nF = ", f_val,
                          "\np = ", p_val),
           color    = "gray40",
           size     = 3.8,
           hjust    = 1,
           fontface = "italic") +
  
  scale_fill_manual(values = test_colors) +
  scale_y_continuous(
    labels = scales::dollar,
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  labs(
    title    = "Average Billing Amount by Test Result",
    subtitle = paste0(
      "Are abnormal test results associated with higher costs?  |  ",
      scales::comma(nrow(df)), " patient encounters, 2019-2024"
    ),
    x        = "Test Result",
    y        = "Average Billing Amount (USD)",
    caption  = "Dashed line = overall average billing across all patients"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1B3A5C", size = 14),
    plot.subtitle = element_text(color = "gray50", size = 10),
    plot.caption  = element_text(color = "gray50", size = 9, hjust = 0),
    axis.text.x   = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank()
  )

# ── Step 6: Display ───────────────────────────────────────────
print(p_8a)


## Display plot 8b
# ── Step 1: Compute summary statistics ───────────────────────
test_los <- df %>%
  group_by(Test_Results) %>%
  summarise(
    Mean_LOS   = mean(LOS),
    Median_LOS = median(LOS),
    SD_LOS     = sd(LOS),
    Min_LOS    = min(LOS),
    Max_LOS    = max(LOS),
    Encounters = n()
  ) %>%
  arrange(desc(Mean_LOS))

# View the numbers first
print(test_los)

# ── Step 2: Run ANOVA (for annotation) ───────────────────────
anova_los     <- aov(LOS ~ Test_Results, data = df)
anova_los_sum <- summary(anova_los)
p_val_los     <- round(anova_los_sum[[1]][["Pr(>F)"]][1], 3)
f_val_los     <- round(anova_los_sum[[1]][["F value"]][1], 2)

# ── Step 3: Define colors ─────────────────────────────────────
test_colors <- c(
  "Abnormal"     = "gray50",
  "Inconclusive" = "peachpuff2",
  "Normal"       = "wheat3"
)

# ── Step 4: Overall average LOS for reference line ────────────
overall_los <- mean(df$LOS)

# ── Step 5: Build the boxplot ─────────────────────────────────
p_8b <- ggplot(df, aes(x    = Test_Results,
                       y    = LOS,
                       fill = Test_Results)) +
  
  # Main boxplot
  geom_boxplot(
    alpha         = 0.75,
    outlier.alpha = 0.10,
    outlier.size  = 0.8,
    outlier.color = "gray60",
    width         = 0.55,
    show.legend   = FALSE
  ) +
  
  # Overlay mean as a diamond point
  stat_summary(
    fun      = mean,
    geom     = "point",
    shape    = 18,
    size     = 5,
    color    = "white",
    show.legend = FALSE
  ) +
  stat_summary(
    fun      = mean,
    geom     = "point",
    shape    = 18,
    size     = 3.5,
    color    = "navy",
    show.legend = FALSE
  ) +
  
  # Overall average reference line
  geom_hline(
    yintercept = overall_los,
    linetype   = "dashed",
    color      = "navy",
    linewidth  = 1.2
  ) +
  
  # Annotate reference line
  annotate(
    "text",
    x     = 0.55,
    y     = overall_los + 0.4,
    label = paste0("Overall avg: ", round(overall_los, 1), " days"),
    color    = "navy",
    size     = 3.8,
    hjust    = 0,
    fontface = "italic"
  ) +
  
  # Annotate mean values above each box
  stat_summary(
    fun      = mean,
    geom     = "text",
    aes(label = paste0("Mean: ", round(after_stat(y), 1), "d")),
    vjust    = -1.2,
    size     = 3.8,
    fontface = "bold",
    color    = "navy"
  ) +
  
  # Annotate median values inside each box
  stat_summary(
    fun      = median,
    geom     = "text",
    aes(label = paste0("Median: ", round(after_stat(y), 0), "d")),
    vjust    = 1.8,
    size     = 3.5,
    color    = "white",
    fontface = "bold"
  ) +
  
  # ANOVA result annotation
  annotate(
    "text",
    x     = 3.45,
    y     = max(df$LOS) * 0.95,
    label = paste0("ANOVA\nF = ", f_val_los,
                   "\np = ", p_val_los),
    color    = "gray40",
    size     = 3.8,
    hjust    = 1,
    fontface = "italic"
  ) +
  
  scale_fill_manual(values = test_colors) +
  scale_y_continuous(
    breaks = seq(0, max(df$LOS) + 5, by = 5),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    title    = "Length of Stay Distribution by Test Result",
    subtitle = paste0(
      "Are abnormal test results associated with longer hospital stays?  |  ",
      scales::comma(nrow(df)), " patient encounters, 2019-2024"
    ),
    x        = "Test Result",
    y        = "Length of Stay (Days)",
    caption  = paste0(
      "Box = IQR (25th-75th percentile)  |  ",
      "Horizontal line inside box = median  |  ",
      "Diamond = mean  |  ",
      "Dots = outliers"
    )
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", color = "#1B3A5C", size = 14),
    plot.subtitle      = element_text(color = "gray50", size = 10),
    plot.caption       = element_text(color = "gray50", size = 9, hjust = 0),
    axis.text.x        = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# ── Step 6: Display ───────────────────────────────────────────
print(p_8b)


## Q9 - Test result by Admission Type
# Cross-tabulation (counts)
ct_counts <- table(df$Admission_Type, df$Test_Results)
print(ct_counts)

# Cross-tabulation (row percentages)
ct_pct <- prop.table(ct_counts, margin = 1) * 100
print(round(ct_pct, 1))

# Chi-square test of independence
chi_result <- chisq.test(ct_counts)
print(chi_result)
# Key output: X-squared, df, p-value

## Visualization

# Grouped bar chart
ct_df <- as.data.frame(ct_pct) %>%
  rename(Admission_Type = Var1,
         Test_Results   = Var2,
         Percentage     = Freq)

ggplot(ct_df,
       aes(x = Admission_Type, y = Percentage,
           fill = Test_Results)) +
  geom_col(position = 'dodge', alpha = 0.85) +
  geom_hline(yintercept = 33.33, linetype = 'dotted',
             color = 'gray50', linewidth = 1) +
  geom_text(aes(label = paste0(round(Percentage, 1), '%')),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 3, fontface = 'bold') +
  scale_fill_manual(values = c('Abnormal'='cadetblue',
                               'Inconclusive'='gray5',
                               'Normal'='khaki3')) +
  labs(title    = 'Test Result Distribution by Admission Type',
       subtitle = paste('Chi-square p-value:',
                        round(chi_result$p.value, 3),
                        '| Dotted = expected equal split (33.3%)'),
       x = NULL, y = '% of Patients', fill = 'Test Result') +
  theme_minimal(base_size = 12) +
  ylim(0, 42)


# Q10: Medical Condition vs Emergency Admission Rate

# Emergency rate by condition
cond_em <- df %>%
  group_by(Medical_Condition) %>%
  summarise(
    Total          = n(),
    Emergency      = sum(Admission_Type == 'Emergency'),
    Emergency_Rate = Emergency / Total * 100
  ) %>%
  arrange(desc(Emergency_Rate))

print(cond_em)


# Chi-square test
ct_cond <- table(df$Medical_Condition, df$Admission_Type)
chi_cond <- chisq.test(ct_cond)
print(chi_cond)

# Overall emergency rate for reference line
overall_em <- mean(df$Admission_Type == 'Emergency') * 100


# Bar chart: Emergency rate by condition
ggplot(cond_em,
       aes(x = reorder(Medical_Condition, Emergency_Rate),
           y = Emergency_Rate,
           fill = Emergency_Rate)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_hline(yintercept = overall_em,
             linetype = 'dashed', color = 'navy', linewidth = 1.2) +
  geom_text(aes(label = paste0(round(Emergency_Rate, 1), '%')),
            hjust = -0.1, size = 3.5, fontface = 'bold') +
  scale_fill_gradient(low = '#6AAED6', high = '#C0392B') +
  coord_flip() +
  labs(title    = 'Emergency Admission Rate by Medical Condition',
       subtitle = paste('Chi-square p =',
                        round(chi_cond$p.value, 3),
                        ' | Dashed = overall rate:',
                        round(overall_em, 1), '%'),
       x = NULL, y = 'Emergency Admission Rate (%)') +
  theme_minimal(base_size = 12) +
  expand_limits(y = max(cond_em$Emergency_Rate) * 1.12)

# Heatmap: Condition x Admission Type
ct_df2 <- as.data.frame(prop.table(ct_cond, margin = 1) * 100) %>%
  rename(Medical_Condition = Var1,
         Admission_Type    = Var2,
         Percentage        = Freq)

ggplot(ct_df2,
       aes(x = Admission_Type,
           y = Medical_Condition,
           fill = Percentage)) +
  geom_tile(color = 'white', linewidth = 0.8) +
  geom_text(aes(label = paste0(round(Percentage, 1), '%')),
            fontface = 'bold', size = 4.5) +
  scale_fill_gradient(low = '#D6EAF8', high = '#1B3A5C',
                      name = '% of Patients') +
  labs(title    = 'Admission Type Heatmap by Medical Condition',
       subtitle = 'Each row sums to 100%',
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12)












