#' plot_SOS'
#' @export
#' @examples data %>% plot_SOS()


plot_SOS <- function(genotype_order = c("cest-2.1"),
                     facet_condition = FALSE,
                     ...) {

  library(tidyverse)
  theme_set(theme_classic())
  source('./scripts/genotype_colors.R')
  source('./scripts/plotOrder.R')
  message("Select file in the folder containing SOS dataset you want to analyze")
  file = file.choose()
  folder = dirname(file)
  filename = fs::path_ext_remove(basename(file))

  source(file.path(folder,"genotype_colors.R"))

  message(paste0("Analyzing ", print(basename(file)), " in the ", print(folder), " folder"))

  dataset <- read_csv(file)

  plot_genotypes <- names(genotype_colors)
  plot_genotypes <- plot_genotypes[plot_genotypes %in% unique(dataset$Genotype)]

  print(plot_genotypes)
  ### reorder to put WT first ###
  dataset <- mutate(dataset, Genotype = fct_relevel(Genotype, plot_genotypes))

  #### PLOT FUNCTION #####
  meds <- dataset %>%
    group_by(Genotype, Date, Condition) %>%
    summarise(Response.time = median(Response.time, na.rm = TRUE)) %>%
    mutate(Genotype = fct_relevel(Genotype, plot_genotypes))

  means <- dataset %>%
    group_by(Genotype, Date, Condition) %>%
    summarise(Response.time = mean(Response.time, na.rm = TRUE)) %>%
    mutate(Genotype = fct_relevel(Genotype, plot_genotypes))

  mod <- means %>%
    lm(data = ., formula = Response.time ~ Genotype) %>%
    emmeans::ref_grid() %>%
    emmeans::contrast(method = "pairwise")

p <- means %>% ggplot(aes(x = Genotype, y = Response.time)) +
  stat_summary(geom = "bar", aes(fill = Genotype, alpha = Condition), width= 0.65) +
  labs(fill = "Genotype") +
  ggbeeswarm::geom_quasirandom(alpha = 0.1, width=0.2) +
  ggbeeswarm::geom_quasirandom(alpha = 1, width = 0.2, data = means) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.1) +
  #labs(title = "SOS with N2, cest-2.1, and tbh-1 for 30% octanol avoidance") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0,15)) +
  scale_fill_manual(values = genotype_colors) +
  scale_alpha_manual(values = condition_alpha) +
  labs(y = "Time(sec)") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  guides(fill = "none")

if(facet_condition) {
  p <- p + facet_grid(.~Condition)
}


  ggsave(file.path(folder,paste0(filename,".pdf")),
         plot = p,
         height = 4.5,
         width = 1.5 + length(unique(dataset$Genotype))*1)

  write_csv(as_tibble(mod), file = file.path(folder,paste0(filename,"_stats.csv")))
}
