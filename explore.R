# dat <- rbind(a,d,c)
# codes <- dat %>% group_by(NAICS) %>% count() %>% arrange(n) %>% tail(20) %>% head(15) %>% pull(NAICS)
# dat %>% filter(NAICS %in% codes) %>% group_by(NAICS) %>% select(A_MEDIAN, H_MEDIAN, OCC_CODE, OCC_TITLE, NAICS_TITLE)

order.mix <- function (mix) {
    tibble(mu = mix$mu, sigma = mix$sigma) %>% 
        arrange(mu) %>%
        mutate(skill = c('low', 'high'))
}

get.mixes <- function (d) {
    l <- d %>% 
        mutate(A_MEDIAN = as.numeric(A_MEDIAN)) %>%
        filter(!is.na(A_MEDIAN) & !is.na(NAICS)) %>%
        group_by(NAICS) %>% 
        select(A_MEDIAN, H_MEDIAN, OCC_CODE, OCC_TITLE, NAICS_TITLE, NAICS) %>%
        nest()

    map(l$data, function (df) normalmixEM(df$A_MEDIAN)) %>%
        map_df(function (mix) order.mix(mix)) %>% 
        group_by(skill) %>% 
        mutate(NAICS = l$NAICS)
}
