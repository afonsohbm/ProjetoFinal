library(electionsBR)
library(dplyr)


candidate <- electionsBR::candidate_fed(year = 2018, uf = "DF")
View(candidate)

candidateFiltered <- filter(candidate, SIGLA_PARTIDO == "NOVO")
View(candidateFiltered)

municipios <- electionsBR::details_mun_zone_fed(year = 2018, uf = "DF")
View(municipios)

party_mun_zone_fed <- electionsBR::party_mun_zone_fed(year = 2018, uf = "DF")
View(party_mun_zone_fed)

party_mun_zone_fed_FILTERED <- filter(party_mun_zone_fed, SIGLA_PARTIDO == "NOVO")
View(party_mun_zone_fed_FILTERED)

filter(party_mun_zone_fed_FILTERED, DESCRICAO_CARGO == "Presidente")

finance <- electionsBR::personal_finances_fed(year = 2018, uf = "DF")
View(finance)

affiliation <- electionsBR::voter_affiliation(party  = "NOVO", uf = "DF")

senador <- party_mun_zone_fed[party_mun_zone_fed$DESCRICAO_CARGO == "Senador",]
presidente <- party_mun_zone_fed[party_mun_zone_fed$DESCRICAO_CARGO == "Presidente",]


#BETS
install.packages("BETS")
BETS::BETS.addin_pt()

BETS::dashboard()

