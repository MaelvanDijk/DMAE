
library("tidyverse")
library("stringr")

plot_hob_salary_vs_indeed_salary_dist <- function(df_indeed_scraped, first_salary= 2763, second_salary= 3087, third_salary= 3519 ){
  # bepaal constanten op basis van HoB vacature
  options(scipen=999)
  # bepaal constanten op basis van HoB vacature
  hob_first_salary <- first_salary
  hob_second_salary <- second_salary
  hob_third_salary <- third_salary 
  indeed_median_salary <- median(df_indeed_scraped$salary,na.rm = TRUE)
  
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
    ggplot(
      aes(
        x= salary,
        # y= (..count../(sum(..count..)))
        )
      ) +
    geom_density(alpha=0.5) +
    # onderstaande regel vult alles rechts van de gekozen lijn
    # geom_area(data=subset(dens$data[[1]], x > hob_first_salary), aes(x=x, y=y), fill="red")+
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
        y=0, yend= dens_indeed_median_salary,
        linetype= "median loon Indeed"),
      color = "blue",
      size = 1.0
    ) +
    labs(
      title="Verdeling maandsalaris op Indeed vs. HoB",
      y="Frequentie (%)",
      x="Maandsalaris (\u20ac)"
    ) +
    theme(
      plot.title=element_text(hjust=0.5),
      legend.title= element_blank()
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
      x= hob_third_salary + 200,
      y = 3e-04,
      label= "HoB salaris 3de jr.",
      angle=90
      
    )+
    scale_y_continuous(
      breaks= seq(0.0001, 0.0005, 0.0001),
      labels=seq(0.01, 0.05, 0.01)
    )
}

plot_indeed_timeseries_data <- function(df_indeed_skills, skill=""){
  if(skill==""){
    df_indeed_skills %>%
      group_by(listing_date) %>%
      summarise(req_per_day= n(), .groups= "keep") %>%
      ggplot(
        aes(
          x= listing_date,
          y= req_per_day
        )
      ) +
      geom_line() +
      stat_smooth(method = "lm",
                  se= FALSE
      ) +
      scale_x_date(date_labels = "%a\n%d-%m",
                   date_breaks = "week"
      )
  }
  else{
    max_count <- df_indeed_skills_date_filtered %>%
      group_by(listing_date, skills) %>%
      summarise(req_per_day= n(), .groups= "keep") %>%
      .$req_per_day %>%
      max()
    
    df_indeed_skills %>%
      filter(skills == skill) %>%
      group_by(listing_date, skills) %>%
      summarise(req_per_day= n(), .groups= "keep") %>%
      ggplot(
        aes(
          x= listing_date,
          y= req_per_day
            )
      ) +
      geom_line() +
      stat_smooth(method = "lm",
                  se= FALSE
                  ) +
      scale_x_date(date_labels = "%a\n%d-%m",
                   date_breaks = "week"
                   ) +
      ylim(0,
           (max_count * 1.1)
           ) # keep y-axis consistent over different plots
  }
}