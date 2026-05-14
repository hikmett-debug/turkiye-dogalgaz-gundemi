# ================================================================
# 01_cizgi_grafigi.R
# Elektrik Uretiminde Kaynak Paylarinin Yillik Degisimi
# Kaynak: TEIAS (2017-2025)
# Encoding: UTF-8
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# -- Sabitler -----------------------------------------------------
DOSYA <- "01_cizgi_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/01_cizgi_grafigi.png"
W <- 14; H <- 8; RES <- 300

PALET <- c(
  "Dogal Gaz"   = "#003f88",
  "Ithal Komur" = "#c1121f",
  "Linyit"      = "#e76f00"
)

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
      axis.text          = element_text(size = base - 3, colour = "#555555"),
      plot.title         = element_text(size = base + 4, face = "bold",
                                        colour = "#111111", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base - 2, colour = "#777777",
                                        hjust = 0.5, margin = margin(b = 14)),
      plot.caption       = element_text(size = base - 3, colour = "#999999",
                                        hjust = 0.5, face = "italic",
                                        margin = margin(t = 10)),
      legend.position    = "right",
      legend.title       = element_blank(),
      legend.text        = element_text(size = base - 1),
      legend.key.width   = unit(1.8, "cm"),
      legend.key.height  = unit(0.45, "cm"),
      legend.spacing.y   = unit(0.35, "cm"),
      plot.margin        = margin(16, 10, 12, 16)
    )
}

# -- Veri ---------------------------------------------------------
df <- read_excel(DOSYA) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(Yil)) |>
  pivot_longer(-Yil, names_to = "Kaynak", values_to = "Yuzde") |>
  filter(!is.na(Yuzde)) |>
  mutate(Kaynak = recode(Kaynak,
    "Dogal_gaz_yuzde"   = "Dogal Gaz",
    "Ithal_komur_yuzde" = "Ithal Komur",
    "Linyit_yuzde"      = "Linyit"
  ))

# -- Grafik -------------------------------------------------------
p <- ggplot(df, aes(Yil, Yuzde, colour = Kaynak, group = Kaynak)) +
  geom_line(linewidth = 1.8, lineend = "round") +
  geom_point(size = 3.8, shape = 21, fill = "white", stroke = 2.2) +
  scale_x_continuous(
    breaks = unique(df$Yil),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  scale_colour_manual(
    values = PALET,
    labels = c(
      "Dogal Gaz"   = "Do\u011fal Gaz",
      "Ithal Komur" = "\u0130thal K\u00f6m\u00fcr",
      "Linyit"      = "Linyit"
    )
  ) +
  guides(colour = guide_legend(
    override.aes = list(
      linewidth = 1.8, size = 3.8,
      shape = 21, fill = "white", stroke = 2.2
    )
  )) +
  labs(
    title    = "Elektrik \u00dcretiminde Kaynak Paylar\u0131n\u0131n Y\u0131ll\u0131k De\u011fi\u015fimi",
    subtitle = "Do\u011fal Gaz, \u0130thal K\u00f6m\u00fcr ve Linyit  |  2017\u20132025",
    y        = "\u00dcretim Pay\u0131 (%)",
    caption  = "Kaynak: TE\u0130A\u015e (2017\u20132025)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
