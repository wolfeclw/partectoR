
d1 <- read_partector('8074_141425_p1.txt', sample_col = 'Unit', sample_id = 'Partector 1')
d2 <- read_partector('8219_141424_p2.txt', sample_col = 'Unit', sample_id = 'Partector 2')


d_partector <- circleclust::dt_aggregate(d1, 'Date_Time')

d_agg1 <- circleclust::dt_aggregate(d1, 'Date_Time')
d_agg2 <- circleclust::dt_aggregate(d2, 'Date_Time')

d_colocation <- dplyr::bind_rows(d_agg1, d_agg2)

usethis::use_data(d_partector, overwrite = TRUE)
usethis::use_data(d_colocation, overwrite = TRUE)


