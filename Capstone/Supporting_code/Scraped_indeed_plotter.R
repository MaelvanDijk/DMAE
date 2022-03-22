

plot_hob_salary_vs_indeed_salary_dist <- function(df_indeed_scraped){
  # bepaal constanten op basis van HoB vacature
  options(scipen=999)
  hob_first_salary <- 2763
  hob_second_salary <- hob_first_salary + 324
  hob_third_salary <- hob_second_salary + 432 
  indeed_median_salary <- median(df_indeed_scraped$salary)
  
  # maak density tables
  #https://stackoverflow.com/questions/41971150/add-vline-to-geom-density-and-shade-confidence-interval-of-mean-r
  dens <- ggplot_build(ggplot(df_indeed_scraped, aes(x= salary)) +
                         geom_density())
  
  
  # Bepaal de density (hoogte) van het 1ste, 2e en 3de salaris door deze te benaderen op basis van x en y verhoudingen
  dens_first_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = hob_first_salary)[[2]])%>%
    .$dens.mean %>% .[1]
  
  dens_second_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = hob_second_salary)[[2]])%>%
    .$dens.mean %>% .[1]
  
  dens_third_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = hob_third_salary)[[2]])%>%
    .$dens.mean %>% .[1] 
  
  dens_indeed_median_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = indeed_median_salary)[[2]])%>%
    .$dens.mean %>% .[1] 
  
  # plot de verdeling inclusief salaris punten
  df_indeed_scraped %>%
    ggplot(aes(x= salary)) +
    geom_density(alpha=0.5) +
    # onderstaande regel vult alles rechts van de gekozen lijn
    geom_area(data=subset(dens$data[[1]], x > hob_first_salary), aes(x=x, y=y), fill="red")+
    geom_segment(
      aes(
        x=hob_first_salary,xend=hob_first_salary,
        y=0, yend= dens_first_salary )
    ) +
    geom_segment(
      aes(
        x=hob_second_salary, xend=hob_second_salary,
        y=0, yend= dens_second_salary )
    ) +
    geom_segment(
      aes(
        x=hob_third_salary, xend=hob_third_salary,
        y=0, yend= dens_third_salary )
    ) +
    geom_segment(
      aes(
        x=indeed_median_salary, xend=indeed_median_salary,
        y=0, yend= dens_indeed_median_salary )
    ) +
    labs(
      title="Verdeling maandsalaris op indeed vs. HoB",
      y="frequentie",
      x="Maandsalaris"
    ) +
    theme(
      plot.title=element_text(hjust=0.5)
    ) +
    annotate(
      "text",
      x= hob_first_salary - 200,
      y = 1e-04,
      label= "HoB salaris 1ste jr.",
      angle=90
      
    ) +
    annotate(
      "text",
      x= hob_second_salary - 200,
      y = 2e-04,
      label= "HoB salaris 2de jr.",
      angle=90
      
    ) +
    annotate(
      "text",
      x= hob_third_salary - 200,
      y = 3e-04,
      label= "HoB salaris 3de jr.",
      angle=90
      
    )
}