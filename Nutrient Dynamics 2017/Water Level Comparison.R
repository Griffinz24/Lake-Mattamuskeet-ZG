AG_path   <- "C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/AG_mass.csv"
MI9_path  <- "C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/MI9_mass.csv"

library(ggplot2)
library(lubridate)

# Read CSVs (handle extra quotes correctly)
AG_df  <- read.csv(AG_path,  stringsAsFactors = FALSE, quote="\"")
MI9_df <- read.csv(MI9_path, stringsAsFactors = FALSE, quote="\"")

# Your file structure:
#   Column 1 = row ID (ignore)
#   Column 2 = Date (already ISO format)
#   Column 3 = WL

# Parse datetime from column 2
AG_df$Time  <- ymd_hms(AG_df[[2]])
MI9_df$Time <- ymd_hms(MI9_df[[2]])

# Extract WL values from column 3
AG_df$Value  <- AG_df[[3]]
MI9_df$Value <- MI9_df[[3]]

# Remove rows where Time failed to parse (should be none)
AG_df  <- AG_df[!is.na(AG_df$Time), ]
MI9_df <- MI9_df[!is.na(MI9_df$Time), ]

# Combine
plot_df <- rbind(
  data.frame(Time = AG_df$Time,  Value = AG_df$Value,  Site = "AG"),
  data.frame(Time = MI9_df$Time, Value = MI9_df$Value, Site = "MI9")
)

# Plot MATHC WITH THE OTHER GRAPHS AI GEN
theme_lm <- theme_bw(base_size = 16) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14, colour = "black"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    plot.margin = margin(10, 20, 10, 10)
  )

#data plot
p <- ggplot(plot_df, aes(x = Time, y = Value, color = Site)) +
  geom_line(linewidth = 1.2) +   # match nitrogen line width
  scale_x_datetime(
    date_breaks = "2 months",
    date_labels = "%m/%d/%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(values = c("AG" = "cornflowerblue", "MI9" = "sienna1")) +
  labs(
    x = "Date",
    y = "Water Level (m)",
    color = ""
  ) +
  theme_lm
p
ggsave("Water Level Comparison RAW.jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)


#Smoothed plot
p <- ggplot(plot_df, aes(x = Time, y = Value, color = Site)) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 20),
    linewidth = 1.2,
    se = FALSE
  ) +
  scale_x_datetime(
    date_breaks = "2 months",
    date_labels = "%m/%d/%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(limits = c(-.10,2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(values = c("AG" = "cornflowerblue", "MI9" = "sienna1")) +
  labs(
    x = "Date",
    y = "Water Level (m)",
    color = ""
  ) +
  theme_lm

p

ggsave("Water Level Comparison Smoothed.jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)


