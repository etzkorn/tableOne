library(tidyverse)

data <- iris
data$species2 <-  data$Species
tableOne(
     data,
     strata.variable = "Species",
     pretty.labels = c(
     	 species2 = "Type",
         Sepal.Length = "Sepal Length (cm)",
         Sepal.Width = "Sepal Width (cm)",
         Petal.Length = "Petal Length (cm)",
         Petal.Width = "Petal Width (cm)"))
