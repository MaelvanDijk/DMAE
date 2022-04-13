
library("tidyverse")
library("stringr")

plot_hob_salary_vs_indeed_salary_dist <- function(
    df_indeed_scraped,
    first_salary= 2763,
    second_salary= 3087,
    third_salary= 3519
    ){
  # bepaal constanten op basis van HoB vacature
  options(scipen=999)
  
  # bepaal median indeed salary
  indeed_median_salary <- median(df_indeed_scraped$salary,na.rm = TRUE)
  
  # maak density tables
  #https://stackoverflow.com/questions/41971150/add-vline-to-geom-density-and-shade-confidence-interval-of-mean-r
  dens <- ggplot_build(ggplot(df_indeed_scraped, aes(x= salary)) +
                         geom_density())
  
  
  # Bepaal de density (hoogte) van het 1ste, 2e en 3de salaris door deze te benaderen op basis van x en y verhoudingen
  dens_first_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = first_salary)[[2]])%>%
    .$dens.mean %>% .[1]
  
  dens_second_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = second_salary)[[2]])%>%
    .$dens.mean %>% .[1]
  
  dens_third_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = third_salary)[[2]])%>%
    .$dens.mean %>% .[1] 
  
  dens_indeed_median_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = indeed_median_salary)[[2]])%>%
    .$dens.mean %>% .[1] 
  
  # plot de verdeling inclusief salaris punten
  df_indeed_scraped %>%
    ggplot(
      aes(
        x= salary
        )
      ) +
    geom_density(alpha=0.5) +
    geom_segment(
      aes(
        x=first_salary,xend=first_salary,
        y=0, yend= dens_first_salary,
        linetype= "HoB 1ste jr."
        )
    ) +
    geom_segment(
      aes(
        x=second_salary, xend=second_salary,
        y=0, yend= dens_second_salary,
        linetype= "HoB 2de jr."
        )
    ) +
    geom_segment(
      aes(
        x=third_salary, xend=third_salary,
        y=0, yend= dens_third_salary,
        linetype= "HoB 3de jr."
        )
    ) +
    geom_segment(
      aes(
        x=indeed_median_salary, xend=indeed_median_salary,
        y=0, yend= dens_indeed_median_salary,
        color = "median loon Indeed"),
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
    scale_y_continuous(
      breaks= seq(0.0001, 0.0005, 0.0002),
      labels=seq(0.01, 0.05, 0.02)
    ) +
    theme_classic()+
    scale_color_manual(values=c("median loon Indeed"="#07E597"))
}


plt_hob_vs_indeed_salary_dist_per_experience <- function(
    df_indeed_final,
    first_salary= 2763,
    second_salary= 3087,
    third_salary= 3519
){
  df_indeed_final %>%
    ggplot(aes(x= salary)) +
    geom_density(alpha=0.5) +
    geom_segment(
      aes(
        x =first_salary, xend =first_salary,
        y= 0.0000, yend=0.003,
        linetype= "HoB 1ste jr."
      )
    ) +
    geom_segment(
      aes(
        x =second_salary, xend =second_salary,
        y= 0.0000, yend=0.003,
        linetype= "HoB 2de jr."
      )
    ) +
    geom_segment(
      aes(
        x =third_salary, xend =third_salary,
        y= 0.0000, yend=0.003,
        linetype= "HoB 3de jr."
      )
    ) +
    facet_wrap(vars(cleaned_werkervaring_jaren)) +
    labs(
      title="Verdeling geboden salaris per jaren werkervaring",
      y= "Frequentie (%)",
      x= "Geboden salaris x1.000 (\u20ac)"
    ) +
    theme(
      plot.title=element_text(hjust=0.5))+
    scale_y_continuous(
      breaks= seq(0.0000, 0.004, 0.001),
      labels=seq(0.00, 0.04, 0.01),
      limits= c(0, 0.003)
    ) +
    scale_x_continuous(
      breaks= seq(2000, 6000, 1000),
      labels=seq(2, 6, 1),
      limits = c(2000, 6000)
    ) +
    theme_classic()
}

plot_indeed_median_salary_per_amount_skills <- function(
    df_indeed_final,
    add_ref_line= FALSE,
    ref_salary= 0
){
  if(add_ref_line){
    df_indeed_final %>%
      group_by(skill_count) %>%
      summarise(med= median(salary, na.rm = T)) %>%
      ggplot(
        aes(x=skill_count, y=med)) +
      geom_col(
        aes(
          fill = ifelse(med < ref_salary, '#07E597', '#2C435E')
        )
      )+
      geom_hline(linetype= 1,
                 yintercept = ref_salary
      )+
      labs(
        title = "Mediaan salaris Indeed vs. aantal skills",
        subtitle = "Met gekozen salaris als referentie lijn",
        y = "Mediaan salaris Indeed (\u20ac)",
        x = "Aantal skills"
      ) +
      theme_classic()+
      theme(
        legend.position= "none"
      )+
      scale_fill_manual(values=c('#07E597', '#2C435E'))
  }
  else{
    df_indeed_final %>%
      group_by(skill_count) %>%
      summarise(med= median(salary, na.rm = T)) %>%
      ggplot(
        aes(x=skill_count, y=med)) +
      geom_col(
        aes()
      )+
      labs(
        title = "Mediaan salaris Indeed vs. aantal skills",
        y = "Mediaan salaris Indeed (\u20ac)",
        x = "Aantal skills"
      )+
      theme_classic() +
      theme(
        legend.position= "none"
      )
  }

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