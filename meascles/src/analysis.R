library(data.table)


meascles <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
setDTthreads(threads = 0)

states_filter <- c( "Colorado", "Connecticut", "Minnesota", "Montana", "New Jersey",
                    "New York", "North Dakota", "Pennsylvania", "South Dakota", "Utah", "Washington")

meascles_filtered <- meascles[! state %in% states_filter & ,.(year,state,overall,mmr,xrel,xmed,xper)]

table(meascles_filtered$state)

