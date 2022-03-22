plot_kaggle_distribution <- function(df_indeed_kaggle){
  # D001
  # maak de grafieken breeder
  options(repr.plot.width=20, repr.plot.height=8)
  
  # visualiseer gevraagde aantal skills verderling
  p1 <- df_indeed_kaggle %>%
    ggplot(aes(x= No_of_Skills)) +
    geom_bar() +
    facet_grid(cols= vars(Job_Type)) +
    labs(
      title="Verdeling benodigde aantal skills per functie type",
      y= "frequentie",
      x= "Aantal gevraagde skills"
    ) +
    theme(plot.title=element_text(hjust=0.5))
  
  # sorteer de salaris kolom
  df_indeed_kaggle$Queried_Salary <- factor(
    df_indeed_kaggle$Queried_Salary,
    levels = c("<80000", "80000-99999", "100000-119999", "120000-139999", "140000-159999", ">160000"))  
  
  # visualiseer salaris groep
p2 <- df_indeed_kaggle %>%
    ggplot(aes(x= Queried_Salary)) +
    geom_bar() +
    facet_grid(cols= vars(Job_Type)) +
    labs(
      title="Verdeling salaris groep per functie type",
      y= "frequentie",
      x= "Salaris groep"
    ) +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x = element_text(angle = 90))


  # visualiseer de skill verdeling op basis van salaris groepen
  p3 <- df_indeed_kaggle %>%
    ggplot(aes(x= No_of_Skills, fill= Queried_Salary)) +
    geom_density(alpha=0.5) +
    facet_grid(rows= vars(Queried_Salary)) +
    labs(
      title="Verdeling gevraagde aantal skills per salaris groep",
      y= "frequentie",
      x= "Aantal gevraagde skills"
    ) +
    theme(
      plot.title=element_text(hjust=0.5))
  
  
  
  return(list(plot(p1), plot(p2), plot(p3)))
}