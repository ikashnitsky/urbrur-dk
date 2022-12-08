#===============================================================================
# 2022-11-22 -- urbrur-dk
# estimate life tables for the urb/rur groups of municipalitis
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

source("src/prepare-session.R")

source("src/fun-topals-fit.R")
source("src/fun-show-topals.R")
source("src/fun-life-table.R")


# data --------------------------------------------------
# municipalities geodata taken from DEGURBA
# https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/degurba

load("~/data/dnk/geodata.rda")

# load prepared data
load("~/data/dnk/deaths.rda")
load("~/data/dnk/expo.rda")
load("~/data/dnk/pop.rda")

# urbrur classifications
load("dat/classifications-urbrur.rda")

# take the standard from mun-dk project
load("~/github/mun-non-surv/dat/pooled_5y.rda")

# HMD standard
hmd_std <- read_csv("https://gist.githubusercontent.com/ikashnitsky/71a6084808c1ac96ee96a2c187588105/raw/179a61e602f895b3feca3b61e56402eacc50863b/hmd-std.csv") %>%
    pivot_longer(f:m, names_to = "sex", values_to = "logmx_std")


# join datasets
df <- type %>%
    left_join(expo %>% select(-name)) %>%
    left_join(deaths %>% select(-name))

# # aggregate by urbrur degurba
# df_degurba <- df %>%
#     group_by(type_degurba, year, sex, age) %>%
#     summarise(
#         expo = expo %>% sum(na.rm = T),
#         death = death %>% sum(na.rm = T)
#     )

# aggregate by urbrur dors
df_dors <- df %>%
    group_by(type_dors, year, sex, age) %>%
    summarise(
        expo = expo %>% sum(na.rm = T),
        death = death %>% sum(na.rm = T)
    ) %>%
    mutate(
        age = age %>% as.integer(),
        year = year %>% as.integer()
    ) %>%
    ungroup()


# join standard and fit TOPALS [TODO: compare standards]
df_fit <- df_dors %>%
    left_join(
        # std_5y %>%
        #     filter(period == "2015-2019") %>%
        #     transmute(age, sex, logmx_smth)
        hmd_std
    ) %>%
    group_by(year, sex, type_dors) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = expo, D = death, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

# calculate full life tables
df_lt <- df_fit %>%
    group_by(year, sex, type_dors) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()


df_lt %>%
    filter(age == 0) %>%
    tibble() %>%
    ggplot(aes(year, ex, color = type_dors))+
    geom_line(size = 1)+
    # stat_smooth(se = F)+
    scale_color_manual(values = pal_3)+
    scale_x_continuous(breaks = c(2010, 2015, 2020))+
    scale_y_continuous(limits = c(76.2, 84.1))+
    facet_wrap(~sex)+
    geom_text(
        data = . %>%
            filter(sex == "m") %>%
            select(sex, type_dors) %>%
            distinct(),
        aes(label = type_dors),
        x = 2010, y = seq(81.5, 80.5, -.5), hjust = 0, size = 5,
        family = font_rc, fontface = 2
    )+
    geom_text(
        data = tibble(
            sex = c("f", "m"),
            label = c("females", "males")
        ),
        aes(label = label),
        x = 2010, y = 83.7, hjust = 0, size = 7,
        family = font_rc, fontface = 2,
        color = c("#b085f5", "#bb4d00")
    )+
    theme(
        strip.text = element_blank()
    )

ggsave(
    "fig/life-exp-dors.png",
    width = 7, height = 4
)


# check the same for degurba ----------------------------------------------


# aggregate by urbrur degurba
df_degurba <- df %>%
    group_by(type_degurba, year, sex, age) %>%
    summarise(
        expo = expo %>% sum(na.rm = T),
        death = death %>% sum(na.rm = T)
    ) %>%
    mutate(
        age = age %>% as.integer(),
        year = year %>% as.integer()
    ) %>%
    ungroup()


# join standard and fit TOPALS [TODO: year specific standards]
df_fit_dg <- df_degurba %>%
    left_join(
        # std_5y %>%
        #     filter(period == "2015-2019") %>%
        #     transmute(age, sex, logmx_smth)
        hmd_std
    ) %>%
    group_by(year, sex, type_degurba) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = expo, D = death, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

# calculate full life tables
df_lt_dg <- df_fit_dg %>%
    group_by(year, sex, type_degurba) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup() %>%
    mutate(
        type_degurba = type_degurba %>%
            as_factor %>%
            lvls_revalue(
                c(
                    "Densely populated",
                    "Intermediate density",
                    "Thinly populated"
                )
            )
    )


df_lt_dg %>%
    filter(age == 0) %>%
    tibble() %>%
    ggplot(aes(year, ex, color = type_degurba))+
    geom_line(size = 1)+
    # stat_smooth(se = F)+
    scale_color_manual(values = pal_3)+
    scale_x_continuous(breaks = c(2010, 2015, 2020))+
    scale_y_continuous(limits = c(76.2, 84.1))+
    facet_wrap(~sex)+
    geom_text(
        data = . %>%
            filter(sex == "m") %>%
            select(sex, type_degurba) %>%
            distinct() ,
        aes(label = type_degurba),
        x = 2010, y = seq(81.5, 80.5, -.5), hjust = 0, size = 5,
        family = font_rc, fontface = 2
    )+
    geom_text(
        data = tibble(
            sex = c("f", "m"),
            label = c("females", "males")
        ),
        aes(label = label),
        x = 2010, y = 83.7, hjust = 0, size = 7,
        family = font_rc, fontface = 2,
        color = c("#b085f5", "#bb4d00")
    )+
    theme(
        strip.text = element_blank()
    )

ggsave(
    "fig/life-exp-degurba.png",
    width = 7, height = 4
)


# uncertainty estimates ---------------------------------------------------

# join standard
tictoc::tic()
df_sim <- df_dors %>%
    left_join(
        # std_5y %>%
        #     filter(period == "2015-2019") %>%
        #     transmute(age, sex, logmx_smth)
        hmd_std
    ) %>%
    group_by(year, sex, type_dors) %>%
    expand_grid(sim_id = 1:5e3) %>%
    group_by(year, sex, type_dors, age) %>%
    mutate(death_sim = rpois(5e3, death)) %>%
    ungroup()
tictoc::toc()


# fit TOPALS to the simulated death counts ~8min (5e3 was 42 min)
tictoc::tic()
df_fit_sim <- df_sim %>%
    group_by(year, sex, type_dors, sim_id) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = expo, D = death_sim, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()
tictoc::toc()
save(df_fit_sim, file = "dat/dors_fit_sim_5e3.rda")


# calculate life tables based on simulated data ~3min
tictoc::tic()
df_lt_sim <- df_fit_sim %>%
    group_by(year, sex, type_dors, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()
save(df_lt_sim, file = "dat/dors_lt_sim.rda")

# life expectancy with 95% CI
df_e0_ci <- df_lt_sim %>%
    filter(age == 0) %>%
    group_by(year, sex, type_dors) %>%
    summarise(
        e0_025 = ex %>% quantile(.025),
        e0_5 = ex %>% quantile(.5),
        e0_975 = ex %>% quantile(.975)
    ) %>%
    left_join(
        df_lt %>%
            filter(age == 0) %>%
            select(year, sex, type_dors, ex)
    )

load("fig/classification-gg.rda")

# plot with CI
df_lt %>%
    filter(age == 0) %>%
    tibble() %>%
    ggplot(aes(year, ex, color = type_dors))+
    # stat_smooth(se = F)+
    geom_line(size = 1)+
    geom_point(size = 1)+
    geom_ribbon(
        data = df_e0_ci,
        aes(ymin = e0_025, ymax = e0_975, fill = type_dors),
        alpha = .3, size = .3
    )+
    scale_color_manual(values = pal_3 %>% clr_darken(.3))+
    scale_fill_manual(values = pal_3)+
    scale_x_continuous(breaks = c(2010, 2015, 2020))+
    facet_wrap(~sex)+
    geom_text(
        data = . %>%
            filter(sex == "m") %>%
            select(sex, type_dors) %>%
            distinct(),
        aes(label = type_dors),
        x = 2010, y = seq(81.5, 80.5, -.5), hjust = 0, size = 5,
        family = font_rc, fontface = 2
    )+
    geom_text(
        data = tibble(
            sex = c("f", "m"),
            label = c("females", "males")
        ),
        aes(label = label),
        x = 2010, y = 83.7, hjust = 0, size = 7,
        family = font_rc, fontface = 2,
        color = c("#b085f5", "#bb4d00")
    )+
    theme(
        strip.text = element_blank()
    )

p_e0_dors <- last_plot()

p_out_dors <- ggdraw(p_e0_dors)+
    draw_plot(p_dors, x = .1, y = .1, height = .4, width = .4)

ggsave(
    "fig/life-exp-dors-ci.png",
    width = 7, height = 4
)
