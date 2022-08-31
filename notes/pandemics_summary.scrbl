#lang notes

@block{@block-name{Comparison}
  @block{@block-name{Categories of news portals}
     Medicine/Health
     Vaccine
     Work
     Psychology
     Travel
     Graphs
     Warning-Apps
     Podcasts/Videos
     Newsletter
     Culture
     Lifestyle
     Family
  }

  @block{@block-name{Parameters RKI, Charité Berlin}
     Virus, epidemiology
     Statistics
     Clinical aspects
     Status updates
     Quarantine
     Contacts
     Diagnostic
     7 days incidence
     Infections per day
     All infections per country
  }

  @block{@block-name{Whatsapp Bots:}
    WHO : https://api.whatsapp.com/send?phone=41798931892&text=hi&source=&data=
    Fact check: https://api.whatsapp.com/send/?phone=17272912606&text=hi&app_absent=0
    Bundesministerium für Gesundheit: https://api.whatsapp.com/send?phone=4915162875183&text=Start&source=&data=
    DRK: https://api.whatsapp.com/send/?phone=493085404106&text=Hallo&app_absent=0

    @block{@block-name{WhatsApp Bot (Corona) instructions:}
        https://jd-bots.com/2020/08/01/whatsapp-bot-get-covid-data-daily-on-whatsapp-using-azure-functions-and-twilio/
        https://github.com/ArugaZ/whatsapp-bot last commit 2021-01-30
    }

    @block{@block-name{Source code repositories}
        https://github.com/godrix/whatsapp-bot-covid19
        https://github.com/k1m0ch1/covid-19-api
        https://github.com/Jatin-8898/covid-bot
        https://github.com/edniltonjr/CoronaBOT
        https://github.com/BuildForSDGCohort2/covid19-whatsapp-bot
        https://github.com/alessandrojcm/covid19-helper-bot
    }
  }
}

@block{@block-name{Sources}
  @block{@block-name{Data collections WHO all https://www.who.int/data/collections}
     Explanation (CSV tables) https://covid19.who.int/info/
     Weekly updates https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports#
     Dashboard: https://covid19.who.int/table?tableChartType=heat
  }

  @block{@block-name{Data world population}
     https://datacatalog.worldbank.org/dataset/population-estimates-and-projections
     https://data.worldbank.org/indicator/SP.POP.TOTL
     https://data.world/worldbank/total-population-per-country
     https://unstats.un.org/unsd/mbs/app/DataSearchTable.aspx
  }

  @block{@block-name{Payed datasets}
    Worldometer -> inaccessible, licence needed
    @block{@block-name{Wolfram alpha:}
        Covid Datasets: https://datarepository.wolframcloud.com/search/?i=covid
        Epidemic Datasets: https://datarepository.wolframcloud.com/search/?i=epidemic
    }
  }

  @block{@block-name{Other pandemics/epidemics}
    @block{@block-name{[[https://www.who.int/data/collections][Data collections WHO all]]}
    }

    @block{@block-name{[[id:be9f0fc9-4b3e-4660-afa0-63f7b85c634b][Influenza]]}
    }

    @block{@block-name{RKI https://www.rki.de/DE/Content/InfAZ/InfAZ_marginal_node.html;jsessionid=0A12AD63CDD5C3F2F53221C6FED82A99.internet061}
    }

    @block{@block-name{European Centre for Disease Prevention and Control (ECDC):}
      @block{@block-name{Tools}
           https://www.ecdc.europa.eu/en/tools
           https://www.ecdc.europa.eu/en/threats-and-outbreaks/epidemic-intelligence
           https://www.ecdc.europa.eu/en/threats-and-outbreaks/outbreak-tools
      }

      @block{@block-name{Surveillance, data: https://www.ecdc.europa.eu/en/surveillance-and-disease-data}
      }

      @block{@block-name{Threats and outbreaks: https://www.ecdc.europa.eu/en/threats-and-outbreaks/by-disease-and-country}
      }
    }
  }

  @block{@block-name{Covid pandemic/epidemic:}
    @block{@block-name{ECDC:}
        Download Covid Datasets ECDC https://www.ecdc.europa.eu/en/covid-19/data (XLSX, CSV, JSON, XML)
        Data updates, sources Covid ECDC: https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases
        ECDC data weekly: https://www.ecdc.europa.eu/en/publications-data
    }

    @block{@block-name{WHO:}
        https://covid19.who.int/table
        https://www.who.int/emergencies/diseases/novel-coronavirus-2019
    }

    @block{@block-name{ICU Free Beds (Intensive Care Unit) https://en.wikipedia.org/wiki/List_of_countries_by_hospital_beds}
    }
  }
}

@block{@block-name{EU}
  @block{@block-name{Covid response coordination:}
     https://www.consilium.europa.eu/en/policies/coronavirus/covid-19-public-health/#
     https://ec.europa.eu/info/live-work-travel-eu/coronavirus-response_en
  }

  @block{@block-name{Covid Overview: https://ec.europa.eu/info/live-work-travel-eu/coronavirus-response/overview-commissions-response_en}
  }

  @block{@block-name{European Centre for Disease Prevention and Control (ECDC): https://europa.eu/european-union/about-eu/agencies/ecdc_en}
  }

  @block{@block-name{European Medicines Agency: https://www.ema.europa.eu/en}
    See picture: [[./eu-bureaucracy-sturcture.png][EU Bureaucracy Structure]]
  }
}

@block{@block-name{Disinformation in the media}
  @block{@block-name{German Factchecks:}
     https://correctiv.org/faktencheck/
     https://www.mimikama.at/category/coronavirus-2019-ncov/
     https://www.spiegel.de/netzwelt/web/coronavirus-fake-news-entlarven-anleitung-zum-faktencheck-a-25e5045f-ed20-4d33-838a-9be8aab84c03
     https://projekte.sueddeutsche.de/artikel/wissen/corona-faktencheck-e401112/
  }

  @block{@block-name{Official German sources:}
     https://www.infektionsschutz.de/coronavirus/basisinformationen/verlaessliche-informationen-erkennen.html
     Q&A RKI: https://www.rki.de/SharedDocs/FAQ/NCOV2019/gesamt.html;jsessionid=1F110AC4949D54DAEE4DFF627675AC66.internet061?nn=13490888
     Infos RKI Vaccination: https://www.rki.de/DE/Content/Infekt/Impfen/ImpfungenAZ/COVID-19/COVID-19.html;jsessionid=1F110AC4949D54DAEE4DFF627675AC66.internet061?nn=13490888
  }

  @block{@block-name{Scientific explanation disinformation:}
     https://www.campus.uni-konstanz.de/wortwechsel/desinformation-fehlinformation-und-verschwoerungstheorien-im-umgang-mit-covid-19>
  }

  @block{@block-name{WHO:}
     Mythbusters: https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public/myth-busters
     Advice for the public: https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public
     Video vaccine vs. vaccine myths vs science: https://www.youtube.com/watch?v=B-aaVh0BQSw
  }
}

@block{@block-name{Extras:}
  Unreported cases: https://covid19.dunkelzifferradar.de/ (Code, Data accessible)

  @block{@block-name{Useful?}
    https://github.com/github/covid-19-repo-data
    https://www.ft.com/content/a2901ce8-5eb7-4633-b89c-cbdf5b386938
    https://covid19.figshare.com/
    COVID-19 Data Repository: social, behaviour, health, economic effects: https://www.openicpsr.org/openicpsr/covid19

  @block{@block-name{Government responses to covid}
      https://www.openicpsr.org/openicpsr/project/119061/version/V6/view
      https://www.openicpsr.org/openicpsr/project/120342/version/V1/view
    }
  }
}
