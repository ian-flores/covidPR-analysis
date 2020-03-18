library(tidyverse)

covid_data <- read_csv('data/march_17.csv')

covid_data %>%
  ggplot(aes(x = Edad)) +
  geom_density() +
  facet_wrap(~ Resultado) +
  theme_minimal()

covid_coded_data <- covid_data %>%
  filter(Resultado != 'Pendiente') %>%
  mutate(Resultado = if_else(Resultado == 'Positivo', 1, 0)) %>%
  mutate(Sexo = if_else(Sexo == 'F', 'Female', 'Male'))

covid_coded_data %>%
  glm(Resultado ~ Edad, family = binomial, data = .)


min(covid_coded_data$Edad)
max(covid_coded_data$Edad)

library(parsnip)

new_data <- data.frame(Edad = rep(1:100, 2), 
                       Sexo = c(rep('Male', 100), rep('Female', 100)))

model_fit <- logistic_reg(mode = 'classification', penalty = NULL, mixture = NULL) %>%
  set_engine("glm", family = binomial) %>%
  fit(as.factor(Resultado) ~ Edad + Sexo, data = covid_coded_data) 

red_line_annot <- data.frame(x = 30, y = 0.65, label = 'The red shade is the 95% \nconfidence interval', Sexo = 'Female')
blue_line_annot <- data.frame(x = 35, y = 0.55, label = 'The blue line is the \npredicted probability', Sexo = 'Male')

model_fit %>%
  predict.model_fit(new_data, type = 'conf_int') %>%
  bind_cols({
    model_fit %>%
      predict.model_fit(new_data, type = 'prob')
  }) %>%
  bind_cols(new_data) %>%
  ggplot(aes(x = Edad)) +
    geom_line(aes(y = .pred_1), colour = 'blue', size = 1.25) +
    geom_ribbon(aes(
      ymin = .pred_lower_1,
      ymax = .pred_upper_1
    ),
    alpha = 0.15,
    fill = 'red') +
    facet_wrap(~ Sexo) +
    xlab('Age') +
    ylab('P(Positive | Testing & 3 symptoms present)') +
    labs(title = 'Probabilities of testing positive to COVID-19 in Puerto Rico (March 17)',
         subtitle = 'Given testing and the presence of the 3 symptoms (Cough, Fever, Difficulty Breathing) \n\nN = 37 patients tested',
         caption = 'Prepared by Ian Flores Siaca (iflores.siaca@pm.me)') +
    theme_light(base_size = 16) +
    geom_label(data = red_line_annot, aes(x = x, y = y, label = label), colour = 'red') +
    geom_label(data = blue_line_annot, aes(x = x, y = y, label = label), colour = 'blue')