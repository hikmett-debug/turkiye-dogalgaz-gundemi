# ================================================================
# 07_yatay_cubuk_grafigi.R
# Avrupa'da Ulkelere Gore Dogalgaz Depolama Kapasitesi
# Kaynak: Eurostat (2025 Sonu)
# Encoding: UTF-8
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)

# -- Sabitler -----------------------------------------------------
DOSYA <- "07_yatay_cubuk_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/07_yatay_cubuk_grafigi.png"
W <- 14; H <- 8; RES <- 300

PALET <- c(
  "Vurgu"  = "#c1121f",
  "Yuksek" = "#003f88",
  "Orta"   = "#028090",
  "Dusuk"  = "#adb5bd"
)

# -- Tema ---------------------------------------------------------
tema <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "#E5E5E5", linewidth = 0.35),
      axis.ticks         = element_blank(),
      axis.title.y       = element_blank(),
      axis.title.x       = element_text(size = base - 1, colour = "#444444"),
      axis.text.y        = element_text(size = base - 1, colour = "#333333"),
      axis.text.x        = element_text(size = base - 2, colour = "#666666"),
      plot.title         = element_text(size = base + 5, face = "bold",
                                        colour = "#111111", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base - 1, colour = "#777777",
                                        hjust = 0.5, margin = margin(b = 14)),
      plot.caption       = element_text(size = base - 2, colour = "#999999",
                                        hjust = 0.5, face = "italic",
                                        margin = margin(t = 10)),
      legend.position    = "none",
      plot.margin        = margin(16, 55, 12, 16)
    )
}

# -- Veri ---------------------------------------------------------
df <- read_excel(DOSYA) |>
  setNames(c("Ulke", "Deger")) |>
  mutate(
    Ulke  = as.character(trimws(Ulke)),
    Deger = suppressWarnings(as.numeric(Deger))
  ) |>
  filter(!is.na(Ulke), !is.na(Deger), Deger > 0) |>
  arrange(Deger) |>
  mutate(
    Ulke = factor(Ulke, levels = Ulke),
    Grup = case_when(
      grepl("rkiye", Ulke)                          ~ "Vurgu",
      Deger >= quantile(Deger, 0.75, na.rm = TRUE)  ~ "Yuksek",
      Deger >= quantile(Deger, 0.40, na.rm = TRUE)  ~ "Orta",
      TRUE                                          ~ "Dusuk"
    )
  )

x_max <- max(df$Deger, na.rm = TRUE)

# -- Grafik -------------------------------------------------------
p <- ggplot(df, aes(Deger, Ulke, fill = Grup)) +
  geom_col(width = 0.72, alpha = 0.92) +
  geom_text(
    aes(
      x     = Deger + x_max * 0.015,
      label = formatC(round(Deger, 1), format = "f", digits = 1,
                      big.mark = ".", decimal.mark = ",")
    ),
    hjust  = 0,
    size   = 2.9,
    colour = "#444444"
  ) +
  scale_x_continuous(
    labels = function(x) formatC(x, format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.22))
  ) +
  scale_fill_manual(values = PALET) +
  labs(
    title    = "Avrupa'da \u00dclkelere G\u00f6re Do\u011falgaz Depolama Kapasitesi",
    subtitle = "2025 sonu itibar\u0131yla  |  TWh",
    x        = "Depolama Kapasitesi (TWh)",
    caption  = "Kaynak: Eurostat (2025 Sonu)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
