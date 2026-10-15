rm(list = ls())

library(vegan)
library(ggcor)
library(ggplot2)
library(dplyr)



varespec <- read.csv(
  "species.csv",
  header = TRUE,
  row.names = 1,
  check.names = FALSE
)

varechem <- read.csv(
  "chen.csv",
  header = TRUE,
  row.names = 1,
  check.names = FALSE
)


identical(
  rownames(varespec),
  rownames(varechem)
)


colnames(varespec)
colnames(varechem)


# =========================
# Mantel test
# =========================

mantel <- mantel_test(
  varespec,
  varechem,
  spec.select = list(
    RiskI = 3,
    RiskII = 2,
    RiskIII = 4
  )
) %>%
  
  dplyr::mutate(
    
    rd = cut(
      r,
      breaks = c(
        -Inf,
        0.2,
        0.4,
        Inf
      ),
      labels = c(
        "< 0.2",
        "0.2 - 0.4",
        ">= 0.4"
      )
    ),
    
    pd = cut(
      p.value,
      breaks = c(
        -Inf,
        0.01,
        0.05,
        Inf
      ),
      labels = c(
        "< 0.01",
        "0.01 - 0.05",
        ">= 0.05"
      )
    )
  )


mantel



windowsFonts(
  Times = windowsFont("Times New Roman"),
  Arial = windowsFont("Arial")
)


p <- quickcor(
  varechem,
  type = "upper"
) +
  
  geom_square() +
  
  anno_link(
    aes(
      colour = pd,
      size = rd
    ),
    data = mantel
  ) +
  
  scale_size_manual(
    values = c(
      "< 0.2" = 0.5,
      "0.2 - 0.4" = 1,
      ">= 0.4" = 2
    )
  ) +
  
  scale_colour_manual(
    values = c(
      "< 0.01" = "#D95F02",
      "0.01 - 0.05" = "#1B9E77",
      ">= 0.05" = "#A2A2A288"
    )
  ) +
  
  guides(
    
    size = guide_legend(
      title = "Mantel's r",
      override.aes = list(
        colour = "grey35"
      ),
      order = 2
    ),
    
    colour = guide_legend(
      title = "Mantel's p",
      override.aes = list(
        size = 2
      ),
      order = 1
    ),
    
    fill = guide_colorbar(
      title = "Pearson's r",
      order = 3
    )
  ) +
  
  theme(
    text = element_text(
      family = "Times",
      face = "bold",
      size = 20
    ),
    
    legend.text = element_text(
      family = "Times",
      size = 10
    ),
    
    legend.title = element_text(
      family = "Times",
      size = 10
    )
  )

p