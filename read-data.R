library(haven)
# A takhle z toho "vysochas" jmena a oznaceni sloupcu
# column_names <- colnames(dataset)
# variable_labels <- sapply(dataset, function(x) attr(x, "label"), simplify = "array")
#
# Attributes from the SAV file
# attr(d[[1]]$w1_gen_trust, "label")
# attr(d[[1]]$w1_gen_trust, "labels")

###########################################################################################################
# Trust in the Constitutional Court 
###########################################################################################################
# Řekněte, prosím, jak moc důvěřujete následujícím institucím nebo aktérům:
# Ústavní soud
# 1 Rozhodně důvěřuji
# 2 Spíše důvěřuji
# 3 Spíše nedůvěřuji
# 4 Rozhodně nedůvěřuji
# 98 Nevím
# 99 odmítl/a odpovědět
text.name <- "Trust in the Constitutional Court"
node.name <- paste(text.name, "\n  Strong trust (1), Rather trust (2), Rather distrust (3), Strong distrust (4)", sep="")
states <- c("Strong trust", "Rather trust", "Rather distrust", "Strong distrust")
fnames <- c("../CzechAttitudeBarometer/Vlna 1 (červen a červenec 2024)/PNS 2406 CoRe CAB W1_FINAL.sav", 
            "../CzechAttitudeBarometer/Vlna 2 (září 2024)/PNS 2409 CoRe CAB W2_FINAL.sav", 
            "../CzechAttitudeBarometer/Vlna 3 (listopad 2024)/PNS 2411 CoRe CAB W3_FINAL.sav", 
            "../CzechAttitudeBarometer/Vlna 4 (prosinec 2024)/PNS 2412 CoRe CAB W4_FINAL.sav", 
            "../CzechAttitudeBarometer/Vlna 5 (duben 2025)/PNS 2504 CoRe W5_FINAL.sav",
            "../CzechAttitudeBarometer/Vlna 6 (červenec 2025)/PNS 2507 CoRe CAB W6_FINAL.sav", 
            "../CzechAttitudeBarometer/Vlna 7 (září 2025)/PNS 2509 CoRe CAB W7_FINAL.sav", 
            "../CzechAttitudeBarometer/Vlna 8 (říjen 2025)/PNS 2510 CoRe CAB W8_FINAL.sav")

# General Trust (have 11 states)
# names <- c("w1_gen_trust", NA, "w3_gen_trust", NA, "w5_gen_trust", NA, "w7_gen_trust", "w8_gen_trust")

# President
# names <- c("w1_pol_trust_1",	NA,	"w3_pol_trust_1",	NA,	"w5_pol_trust_1", "w6_pol_trust_1",	"w7_pol_trust_1", "w8_pol_trust_1")

# Government
# names <- c("w1_pol_trust_2",	NA,	"w3_pol_trust_2",	NA,	"w5_pol_trust_2", "w6_pol_trust_2",	"w7_pol_trust_2", "w8_pol_trust_2")

# Chamber of Deputies of the Parliament
# names <- c("w1_pol_trust_3",	NA,	"w3_pol_trust_3",	NA,	"w5_pol_trust_3", "w6_pol_trust_3",	"w7_pol_trust_3", "w8_pol_trust_3")

# Constitutional court
names <- c("w1_pol_trust_5",	NA,	"w3_pol_trust_5",	NA,	"w5_pol_trust_5", "w6_pol_trust_5",	"w7_pol_trust_5", "w8_pol_trust_5")

# Army
# names <- c("w1_pol_trust_7",	NA,	"w3_pol_trust_7",	NA,	"w5_pol_trust_7", "w6_pol_trust_7",	"w7_pol_trust_7", "w8_pol_trust_7")

# Public media (e.g., Czech Television, Czech Radio)
# names <- c("w1_pol_trust_8",	NA,	"w3_pol_trust_8",	NA,	"w5_pol_trust_8", "w6_pol_trust_8",	"w7_pol_trust_8", "w8_pol_trust_8")

# Scientists
# names <- c("w1_pol_trust_9",	NA,	"w3_pol_trust_9",	NA,	"w5_pol_trust_9", "w6_pol_trust_9",	"w7_pol_trust_9", "w8_pol_trust_9")

d <- list()
dates <- list()
tables <- list()
for (i in 1:8){
  d[[i]] <-  read_sav(fnames[i])
  date.name <- paste("w",i,"_DATE",sep="")
  dates[[i]] <- median(d[[i]][[date.name]])
  if (is.na(names[i])){
    tables[[i]] <- NA
  }else{
    # kombinace designové váhy a post-stratifikační váhy, vypočítané pomocí iteračního vážení a zohledňující křížení 
    # velikosti místa bydliště proti NUTS2, a dále věk, vzdělání, pohlaví. 
    # Tuto váhu doporučujeme používat při běžných analýzách.
    weight_trim <- paste("w",i,"_poststrat_weight_trim", sep="")
    f <- as.formula(paste(weight_trim, names[i], sep="~"))
    tables[[i]] <- xtabs(f, data = d[[i]])
    # tables[[i]] <- table(d[[i]][[names[i]]])
  }
}

# Visualization using 

library(ggplot2)
library(tidyr)
library(dplyr)

# Set locale to English for dates
Sys.setlocale("LC_TIME", "C")


# Define states and colors
states <- c("Strong trust", "Rather trust", "Rather distrust", "Strong distrust")
# states <- 0:10
col <- colorRampPalette(c("blue", "cyan", "yellow", "orange", "red"))(length(states))

# Extract non-missing tables and remove value 98
df_long <- lapply(seq_along(tables), function(i) {
  if (!is.na(tables[[i]][1])) {
    # Remove the 98 category
    tab <- tables[[i]][names(tables[[i]]) != "98" & names(tables[[i]]) != "99"] 
    # Calculate percentages
    percentages <- (tab / sum(tab)) * 100
    data.frame(
      date = as.Date(dates[[i]]),
      category = factor(states, levels = states),  # Use custom labels
      percentage = as.numeric(percentages)
    )
  } else {
    NULL
  }
}) %>% 
  bind_rows()

# Create the plot
# You can adjust the size = 3 parameter if you want the labels larger or smaller, and 
# width = 20 if you need different column width.
ggplot(df_long, aes(x = date, y = percentage, fill = category)) +
  geom_col(width = 20) +  # width in days - made slimmer
  geom_text(aes(label = round(percentage),
                color = category), 
            position = position_stack(vjust = 0.5),
            size = 3) +  # Add percentage labels
  scale_fill_manual(values = col) +  # Custom color scheme
  scale_color_manual(values = c("white", "black", "black", "black"), guide = "none") +  # Text colors
  # scale_color_manual(values = c("white", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black"), guide = "none") +  # Text colors
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Trend Over Time",
    x = "Date",
    y = "Percentage (%)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################################################
# An alternative where "I don't know" is included at the center of the scale
# Define states and colors
states <- c("Strong trust", "Rather trust", "I do not know", "Rather distrust", "Strong distrust")
# states <- 0:10
col <- colorRampPalette(c("blue", "cyan", "yellow", "orange", "red"))(length(states))

# Extract non-missing tables and reorder categories
df_long <- lapply(seq_along(tables), function(i) {
  if (!is.na(tables[[i]][1])) {
    # Reorder: 1, 2, 98, 3, 4
    tab <- tables[[i]][c("1", "2", "98", "3", "4")]
    # Calculate percentages
    percentages <- (tab / sum(tab)) * 100
    data.frame(
      date = as.Date(dates[[i]]),
      category = factor(states, levels = states),  # Use custom labels
      percentage = as.numeric(percentages)
    )
  } else {
    NULL
  }
}) %>% 
  bind_rows()

# Create the plot
ggplot(df_long, aes(x = date, y = percentage, fill = category)) +
  geom_col(width = 20) +  # width in days
  geom_text(aes(label = round(percentage),
                color = category), 
            position = position_stack(vjust = 0.5),
            size = 3) +  # Add percentage labels
  scale_fill_manual(values = col) +  # Custom color scheme with gray for "I do not know"
  scale_color_manual(values = c("white", "black", "black", "black", "black"), guide = "none") +  # Text colors
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Trend Over Time",
    x = "Date",
    y = "Percentage (%)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


