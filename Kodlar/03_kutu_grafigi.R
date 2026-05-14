# ================================================================
# 03_kutu_grafigi.R
# Buyuksehirlerde Dogalgaz Tuketim Dagilimi
# Kaynak: EPDK (2015-2024)
# Encoding: UTF-8
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# -- Sabitler -----------------------------------------------------
DOSYA <- "03_kutu_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/03_kutu_grafigi.png"
W <- 14; H <- 8; RES <- 300

KUTU_DOLGU <- "#003f88"
KUTU_CIZGI <- "#002a5c"
NOKTA_RENK <- "#e76f00"

# -- Tema ---------------------------------------------------------
tema <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E5E5E5", linewidth = 0.35),
      axis.line.x        = element_line(colour = "#BBBBBB", linewidth = 0.4),
      axis.ticks.x       = element_line(colour = "#BBBBBB", linewidth = 0.4),
      axis.ticks.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_text(size = base - 2, colour = "#444444"),
      axis.text          = element_text(size = base - 1, colour = "#555555"),
      plot.title         = element_text(size = base + 4, face = "bold",
                                        colour = "#111111", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base - 2, colour = "#777777",
                                        hjust = 0.5, margin = margin(b = 14)),
      plot.caption       = element_text(size = base - 3, colour = "#999999",
                                        hjust = 0.5, face = "italic",
                                        margin = margin(t = 10)),
      legend.position    = "none",
      plot.margin        = margin(16, 20, 12, 16)
    )
}

# -- Veri ---------------------------------------------------------
df <- read_excel(DOSYA) |>
  mutate(Yil = as.integer(Yil), across(-Yil, as.numeric)) |>
  filter(!is.na(Yil)) |>
  pivot_longer(-Yil, names_to = "Sehir", values_to = "GWh") |>
  filter(!is.na(GWh)) |>
  mutate(Sehir = recode(Sehir,
    "Izmir"    = "\u0130zmir",
    "Istanbul" = "\u0130stanbul"
  ))

sira <- df |>
  group_by(Sehir) |>
  summarise(med = median(GWh, na.rm = TRUE), .groups = "drop") |>
  arrange(med) |>
  pull(Sehir)

df <- df |> mutate(Sehir = factor(Sehir, levels = sira))

# -- Grafik -------------------------------------------------------
p <- ggplot(df, aes(Sehir, GWh)) +
  geom_boxplot(
    outlier.shape = NA,
    fill          = KUTU_DOLGU,
    colour        = KUTU_CIZGI,
    alpha         = 0.20,
    linewidth     = 0.75,
    width         = 0.58
  ) +
  geom_jitter(
    width  = 0.14,
    height = 0,
    size   = 2.0,
    alpha  = 0.82,
    shape  = 16,
    colour = NOKTA_RENK
  ) +
  scale_y_continuous(
    labels = function(x) formatC(x, format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  labs(
    title    = "B\u00fcy\u00fcks\u00fehirlerde Do\u011falgaz T\u00fcketim Da\u011f\u0131l\u0131m\u0131",
    subtitle = "Y\u0131ll\u0131k t\u00fcketim (GWh)  |  2015\u20132024",
    y        = "T\u00fcketim (GWh)",
    caption  = "Kaynak: EPDK (2015\u20132024)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
