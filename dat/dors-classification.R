#===============================================================================
# 2022-11-21 -- urbrur-dk
# Municipal classifications: Degurba and DØRS 2015
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

source("src/prepare-session.R")



# degurba -----------------------------------------------------------------

load("~/data/degurba/degurba-2018.rda")

degurba_dk <- degurba %>%
    filter(str_sub(nuts, 1, 2) == "DK") %>%
    ms_simplify(.5)


# dors --------------------------------------------------------------------
# De Økonomiske Råd. (2015). Dansk Økonomi, forår 2015. 401. https://dors.dk/files/media/rapporter/2015/f15/f15.pdf
# Classification is on page 238 -- Tabel IV.1
# Commented out are copy-pastes from the PDF
# next I use datapasta paste as vector and clean
# 4 names need fixing to match the official DST


# Yderkommuner
# Aabenraa, Billund,
# Bornholm, Faxe, Frederikshavn, Guldborgsund,
# Halsnæs, Hjørring, Holstebro, Jammerbugt,
# Kalundborg, Langeland,
# Lemvig, Lolland, Læsø,
# Morsø, Norddjurs, Næstved, Odsherred, Ringkøbing-Skjern, Samsø,
# Skive, Slagelse, Sorø,
# Stevns, Struer, Svendborg, Syddjurs, Sønderborg, Thisted, Tønder,
# Vesthimmerland, Viborg,
# Vordingborg, Ærø

yder <- c("Aabenraa, Billund", "Bornholm, Faxe, Frederikshavn, Guldborgsund", "Halsnæs, Hjørring, Holstebro, Jammerbugt", "Kalundborg, Langeland", "Lemvig, Lolland, Læsø", "Morsø, Norddjurs, Næstved, Odsherred, Ringkøbing-Skjern, Samsø", "Skive, Slagelse, Sorø", "Stevns, Struer, Svendborg, Syddjurs, Sønderborg, Thisted, Tønder", "Vesthimmerland, Viborg", "Vordingborg, Ærø") %>%
    str_replace("Vesthimmerland", "Vesthimmerlands") %>%
    tibble() %>%
    set_colnames("name") %>%
    separate_rows(name, sep = ", ") %>%
    mutate(type_dors = "yderkommuner")


# Bykommuner
# Aarhus, Aalborg, Albertslund, Ballerup,
# Brøndby, Esbjerg, Frederiksberg, Gentofte, Gladsaxe, Glostrup, Greve,
# Helsingør, Herlev, Herning, Horsens, Hvidovre,
# Ishøj, Kolding, København, Lyngby-Taarbæk,
# Odense, Randers, Roskilde, Rødovre, Tårnby,
# Vallensbæk, Vejle

by <- c(c("Aarhus, Aalborg, Albertslund, Ballerup", "Brøndby, Esbjerg, Frederiksberg, Gentofte, Gladsaxe, Glostrup, Greve", "Helsingør, Herlev, Herning, Horsens, Hvidovre", "Ishøj, Kolding, København, Lyngby-Taarbæk", "Odense, Randers, Roskilde, Rødovre, Tårnby", "Vallensbæk, Vejle")
) %>%
    c(., "Solrød") %>% # add the missed one
    tibble() %>%
    set_colnames("name") %>%
    separate_rows(name, sep = ", ") %>%
    mutate(type_dors = "bykommuner")

# Øvrige kommuner
# Allerød, Assens, Bogense, Brønderslev-Dronningelund, Dragør,
# Egedal, Faaborg-Midtfyn, Fanø, Favrskov,
# Frederiksund, Fredensborg, Fredericia, Furesø,
# Gribskov, Haderslev,
# Hedensted, Hillerød,
# Holbæk, Høje-Taastrup,
# Hørsholm, Ikast-Brande,
# Kerteminde, Mariagerfjord, Køge, Lejre, Middelfart, Nyborg, Odder,
# Odsherred, Rebild, Ringsted, Rudersdal, Silkeborg, Skanderborg, Varde, Vejen

over <- c(c("Allerød, Assens, Bogense, Brønderslev-Dronningelund, Dragør", "Egedal, Faaborg-Midtfyn, Fanø, Favrskov", "Frederiksund, Fredensborg, Fredericia, Furesø", "Gribskov, Haderslev", "Hedensted, Hillerød", "Holbæk, Høje-Taastrup", "Hørsholm, Ikast-Brande", "Kerteminde, Mariagerfjord, Køge, Lejre, Middelfart, Nyborg, Odder", "Odsherred, Rebild, Ringsted, Rudersdal, Silkeborg, Skanderborg, Varde, Vejen")
) %>%
    str_replace("Bogense", "Nordfyns") %>%
    str_replace("Brønderslev-Dronningelund", "Brønderslev") %>%
    str_replace("Frederiksund", "Frederikssund") %>%
    tibble() %>%
    set_colnames("name") %>%
    separate_rows(name, sep = ", ") %>%
    mutate(type_dors = "øvrige kommuner")


type <- bind_rows(by, over, yder) %>%
    mutate(type_dors = type_dors %>% as_factor) %>%
    inner_join(
        degurba_dk %>%
            # st_drop_geometry() %>%
            transmute(
                name = lat_nat,
                id = nsi_code,
                nuts3 = nuts,
                type_degurba = dgurba %>%
                    as_factor %>%
                    lvls_revalue(
                        c(
                            "Densely populated",
                            "Intermediate density",
                            "Thinly populated"
                        )
                    )
            ),
        .
    )



# plot --------------------------------------------------------------------



# which municipalities mismatch?
missm <- type %>%
    filter(!as.numeric(type_degurba) == as.numeric(type_dors))

# map degurba
type %>%
    ggplot()+
    geom_sf(aes(fill = type_degurba), color = NA)+
    geom_sf(data = type %>%  ms_innerlines, color = "#ffffff", size = .15)+
    # geom_sf(data = missm, color = "#FF00FF", size = .3, fill = NA)+
    scale_fill_manual(values = pal_3)+
    theme_map(font_family = font_rc)+
    theme(legend.position = c(.55, .7))+
    labs(
        fill = "DEGURBA"
    )

p_degurba <- last_plot()

ggsave(
    "fig/class-degurba.png", p_degurba,
    width = 5, height = 4
)

# map dors
type %>%
    ggplot()+
    geom_sf(aes(fill = type_dors), color = NA)+
    geom_sf(data = type %>% ms_innerlines, color = "#ffffff", size = .15)+
    # geom_sf(data = missm, color = "#FF00FF", size = .3, fill = NA)+
    scale_fill_manual(values = pal_3)+
    theme_map(font_family = font_rc)+
    theme(legend.position = c(.55, .7))+
    labs(fill = "DØRS")

p_dors <- last_plot()

ggsave(
    "fig/class-dors.png", p_dors,
    width = 5, height = 4
)

p_out <- (p_degurba + geom_sf(data = missm, color = "#FF00FF", size = .3, fill = NA)) + (p_dors + geom_sf(data = missm, color = "#FF00FF", size = .3, fill = NA))

ggsave(
    "fig/classifications.png", p_out,
    width = 10, height = 4, bg = "#dadada"
)

save(p_degurba, p_dors, file = "fig/classification-gg.rda")

# save without geometry
type <- type %>% st_drop_geometry()
save(type, file = "classifications-urbrur.rda")
