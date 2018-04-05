order.mix <- function (mix) {
    tibble(mu = mix$mu, sigma = mix$sigma) %>% 
        arrange(mu) %>%
        mutate(skill = c('low', 'high'))
}

prep.bls <- function(d) {
    d %>% 
        mutate(A_MEDIAN = as.numeric(A_MEDIAN)) %>%
        filter(!is.na(A_MEDIAN) & !is.na(NAICS)) %>%
        group_by(NAICS) %>% 
        select(A_MEDIAN, H_MEDIAN, OCC_CODE, OCC_TITLE, NAICS_TITLE, NAICS)    
}

get.mixes <- function (d) {
    prep.bls(d) %>% 
    nest() %>%
    map(.$data, function (df) normalmixEM(df$A_MEDIAN)) %>%
        map_df(function (mix) order.mix(mix)) %>% 
        group_by(skill) %>% 
        mutate(NAICS = l$NAICS)
}

adjust <- function (mix, deflators, y) {
    defs <- (deflators %>% filter(year == y) %>% select(price.deflator))    
    p <- (100 / defs) %>% as.numeric() %>% reduce(`*`)
    mix %>% mutate(mu = mu * p, sigma = sigma * p)
}

get.log.skill <- function(d, s) d %>% filter(skill == s) %>% pull(mu) %>% log()

get.spread <- function (d) {
    h <- get.log.skill(d, 'high')
    l <- get.log.skill(d, 'low')
    h - l
}

get.diffs <- function (df) {
    df <- df %>% arrange(year) 
    lows <- get.log.skill(df, 'low')
    highs <-  get.log.skill(df, 'high')
    spread <- df %>% group_by(year) %>% get.spread()
    data.frame(lows = lows, highs = highs, spread = spread) %>% map_df(diff)
}