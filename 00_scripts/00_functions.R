###    removeprobablemistakes ########################################

# function to remove probable mistakes, during cleaning of raw data (improving data quality)

# probable mistakes can be of different types:
#   1. entire checklists
#   2. species-checklist combinations
#   3. species-observer combinations
#   4. species-admin unit (state or district) combinations
#   5. species-admin unit-date combinations
#   6. latlong-admin unit (state or district) combinations (or regions)

rm_prob_mistakes <- function(data) {
  
  # creating temporary regions to use in filters
    data <- data %>% 
      mutate(TEMP.REGION = case_when(
        
        COUNTY %in% c("Aurangabad", "Parbhani", "Beed", "Hingoli", "Jalna", "Buldhana", 
                      "Washim", "Dhule", "Solapur", "Ahmednagar", "Latur", "Osmanabad", 
                      "Yavatmal", "Wardha") ~ "MH_plains",
        (COUNTY == "Jalgaon" & LATITUDE < 21.292) |
          (COUNTY == "Amravati" & LATITUDE < 21.209) |
          (COUNTY == "Akola" & LATITUDE < 21.209) |
          (COUNTY == "Nagpur" & LATITUDE < 21.398 & LONGITUDE < 79.400) ~ "MH_plains",
        
        COUNTY %in% c("Thiruvallur", "Ranipet", "Kancheepuram", "Chennai", "Cuddalore", 
                      "Ariyalur", "Puducherry") |
          COUNTY == "Viluppuram" & LONGITUDE > 79.42 ~ "TN_N-plains"
        
        COUNTY %in% c("Karur", "Thiruvarur", "Thanjavur", "Nagapattinam") |
          COUNTY == "Tiruchirappalli" & LATITUDE < 11.138 ~ "TN_C-plains"

        COUNTY %in% c("Sivagangai", "Pudukkottai", "Ramanathapuram", "Thoothukudi") ~ "TN_S-plains",
        (COUNTY == "Virudhunagar" & LONGITUDE > 77.725) |
          (COUNTY == "Tenkasi" & LONGITUDE > 77.351) |
          (COUNTY == "Tirunelveli" & LONGITUDE > 77.572) |
          (COUNTY == "Madurai" & LATITUDE < 10.07 & LONGITUDE > 77.846) ~ "TN_S-plains",
        
        COUNTY %in% c("Vellore", "Tiruvannamalai", "Kallakurichi", "Tirupathur", "Krishnagiri", 
                      "Dharmapuri", "Salem", "Namakkal") ~ "TN_EG-N"

      ))


  # 1. entire checklists
  
  mistake1 <- data %>% 
    filter(group.id %in% c("asd")) %>% 
    distinct(group.id)
  
  
  # 2. species-checklist combinations
  
  mistake2 <- data %>% 
    filter(
      (COMMON.NAME == "Blyth’s Tragopan" & group.id == "S21274851") |
        (COMMON.NAME == "Himalayan Monal" & group.id %in% c(
          "S36449309","S110202874"
          )) |
        (COMMON.NAME == "Kalij Pheasant" & group.id %in% c(
          "S63416231","S64856967"
          )) |
        (COMMON.NAME == "Red Spurfowl" & group.id %in% c(
          "S76110147", "S132265895", "S98984072", "S25573488", "S25762331", "S122211593", 
          "S84162145", "S74797922", "S41925375", "S34004741", "S34201642", "S63349162", 
          "S121904493", "S72249154", "S20388830", "S20388995", "S20840289", "S12275556", 
          "S31592561", "S21501918", "S20437175", "S20399293", "S34358305"
          )) |
        (COMMON.NAME == "Painted Spurfowl" & group.id == "S134340928") |
        (COMMON.NAME == "Mountain Bamboo-Partridge" & group.id == "S21274851") |
        (COMMON.NAME == "Red Junglefowl" & group.id %in% c(
          "S108982385","S67570561"
          )) |
        (COMMON.NAME == "Grey Junglefowl" & group.id %in% c(
          "S98615153", "S127159840", "S34661870", "S23066272", "S41125176", 
          "S132825863", "S26634409", "S65035598", "S63037541", "S41925375"
          )) |
        (COMMON.NAME == "Black Francolin" & group.id == "S60933821") |
        (COMMON.NAME == "Painted Francolin" & group.id == "S57803190") |
        (COMMON.NAME == "Rain Quail" & group.id %in% c(
          "S48472342", "S28738842", "S42271891", "S42637748"
          )) |
        (COMMON.NAME == "Jungle Bush-Quail" & group.id %in% c(
          "S76962924", "S89878061", "S65125519"
          )) |
        (COMMON.NAME == "Rock Bush-Quail" & group.id %in% c(
          "S24991930", "S32453722", "S103366454"
          )) |
        (COMMON.NAME == "Lesser Flamingo" & group.id == "S126888465") |
        (COMMON.NAME == "Ashy Wood-Pigeon" & group.id == "S93699607") |
        (COMMON.NAME == "Spotted Dove" & group.id %in% c(
          "S113162586", "S70796689"
          )) |
        (COMMON.NAME == "Barred Cuckoo-Dove" & group.id == "S133909460") |
        (COMMON.NAME == "Asian Emerald Dove" & group.id %in% c(
          "S126183847", "S126488688"
          )) |
        (COMMON.NAME == "Green Imperial-Pigeon" & group.id == "S20639077") |
        (COMMON.NAME == "Malabar Imperial-Pigeon" & group.id %in% c(
          "S21879421", "S96866144", "S84526237"
          )) |
        (COMMON.NAME == "Mountain Imperial-Pigeon" & group.id %in% c(
          "S55422829", "S137655719"
          )) |
        (COMMON.NAME == "Chestnut-bellied Sandgrouse" & group.id %in% c(
          "S137616888", "S64845421"
          )) |
        (COMMON.NAME == "Greater Coucal" & group.id %in% c(
          "S42608784", "S43386229"
          )) |
        (COMMON.NAME == "Green-billed Malkoha" & group.id == "S125158253") |
        (COMMON.NAME == "Plaintive Cuckoo" & group.id == "S90844755") |
        (COMMON.NAME == "Fork-tailed Drongo-Cuckoo" & group.id %in% c(
          "S96359379", "S24640058"
          )) |
        (COMMON.NAME == "Square-tailed Drongo-Cuckoo" & group.id == "S64379861") |
        (COMMON.NAME == "Common Hawk-Cuckoo" & group.id %in% c(
          "S139777248", "S28823684", "S20984093", "S55301209", "S46323372", "S67672676",
          "S28600623", "S37961563", "S31420463", "S68225437", "S66660738", "S137374061"
        )) |
        (COMMON.NAME == "Hodgson’s Hawk-Cuckoo" & group.id == "S55246155") |
        (COMMON.NAME == "Indian Cuckoo" & group.id %in% c(
          "S97378065", "S107590529", "S49846553", "S61708615", "S131745447"
          )) |
        (COMMON.NAME == "Himalayan Cuckoo" & group.id %in% c(
          "S32601523", "S28511840", "S25364525", "S28511850", "S25364524", 
          "S21535948", "S21504333", "S21503610"
          )) |
        (COMMON.NAME == "White-rumped Needletail" & group.id %in% c(
          "S17625842", "S81990142", "S88790248", "S24265685", "S24265700", 
          "S24868786", "S59681703"
          )) |
        (COMMON.NAME == "Silver-backed Needletail" & group.id %in% c(
          "S57135779", "S68120528", "S55463823", "S55463822", "S55463821"
          )) |
        (COMMON.NAME == "Indian Swiftlet" & group.id %in% c(
          "S21877776", "S22141232", "S23966644", "S23511550", "S22149290", "S82135536"
          )) |
        (COMMON.NAME == "Alpine Swift" & group.id %in% c(
          "S58044659", "S40781715", "S22508834", "S63166306", "S50709313"
          )) |
        (COMMON.NAME == "Dark-rumped Swift" & group.id %in% c(
          "S136596908", "S133953587"
          )) |
        (COMMON.NAME == "Crested Treeswift" & group.id %in% c(
          "S35120686", "S39356909", "S54124046", "S42608562", "S53287943", "S79257174", 
          "S103353216", "S103348496", "S86148123", "S46655701", "S32263134", "S48844071",
          "S91979068"
          )) |
        (COMMON.NAME == "Slaty-breasted Rail" & group.id %in% c(
          "S70765821", "S54062383"
          )) |
        (COMMON.NAME == "Brown Crake" & group.id %in% c(
          "S64000111", "S83604984", "S122810982", "S41982536", "S96928236", "S49771153", 
          "S55009673", "S52121764", "S80442160", "S96270389", "S63661563", "S64807222", 
          "S63667366"
          )) |
        (COMMON.NAME == "Sarus Crane" & group.id %in% c(
          "S83259843", "S118940543", "S99553961"
          )) |
        (COMMON.NAME == "Great Thick-knee" & group.id == "S28771713") |
        (COMMON.NAME == "Eurasian Oystercatcher" & group.id %in% c(
          "S67718859", "S132754026", "S64305472"
          )) |
        (COMMON.NAME == "Black-bellied Plover" & group.id %in% c(
          "S123385002", "S34218448", "S27921471", "S99277147", "S34492614", "S44886609", 
          "S70115622", "S81855275", "S81815220", "S82728052"
          )) |
        (COMMON.NAME == "River Lapwing" & group.id == "S37500970") |
        (COMMON.NAME == "Lesser Sand-Plover" & group.id %in% c(
          "S122169372", "S41043890", "S41021293", "S41021217", "S33010135", "S32999682", 
          "S60810032", "S52467678", "S62946445", "S56307563", "S33712996", "S33712788", 
          "S81515840", "S33982984", "S27014963", "S26186397", "S127063577", "S77074722", 
          "S127143144", "S127142452", "S127142209", "S51075394", "S78637842", "S81568637", 
          "S126692010", "S41864652", "S68908985", "S101336692", "S26976203", "S72989252", 
          "S99684662", "S78454227", "S79001575", "S87147483", "S102669179", "S77475777", 
          "S61442808"
          )) |
        (COMMON.NAME == "Greater Sand-Plover" & group.id %in% c(
          "S125365793", "S88961247", "S128664706", "S128643701", "S128638685", "S32109548", 
          "S88438661", "S96353226", "S45814939", "S101406933", "S101196659", "S126033935", 
          "S126033807", "S125832796", "S94207453", "S136916725", "S131157478", "S124752456", 
          "S124747800", " S124747060", "S129764356", "S98181626", "S97336646", "S28688880", 
          "S28737013", "S50224949", "S47638332", "S49847497", " S60673251", "S41787524", 
          "S41788174", "S128149132", "S125677365", "S125677447", "S125689359", "S125677365", 
          "S125677447", "S125689359", "S128144960", "S129085199", "S96326239", "S57280565", 
          "S57725730", "S68117264", "S66478342"
          )) |
        (COMMON.NAME == "Bronze-winged Jacana" & group.id %in% c(
          "S77327626", "S47898177"
        )) |
        (COMMON.NAME == "Whimbrel" & group.id %in% c(
          "S94207453", "S57280565", "S120580604", "S88961247", "S68954928", "S68803520", 
          "S44758620", "S19732637", "S138631110", "S24292330", "S89528530", "S103331869",
          "S87228508", "S32040641", "S24620534", "S125598879", "S124981679", "S124981422", 
          "S82682321", "S64075567", "S64047969", "S63793146", "S63898127", "S65388753", 
          "S65351309", "S65351251", " S65346803", "S125671676", "S107698669", "S47207126", 
          "S82225746", "S31659725", "S21008563", "S83444263", "S42760427", "S42196928", 
          "S103396838", "S101328387", "S101255522", " S101254510", "S101254421", "S51076043", 
          "S103048651", "S102720263", "S102468635", "S65314482", "S64305472", "S64380910", 
          "S64353757", "S21548228", "S21547843", "S41669336", "S41668652", "S41666492", 
          "S58556628", "S41791079", "S41490557", "S41486253", "S41424753", "S32790587"
        )) |
        (COMMON.NAME == "Bar-tailed Godwit" & group.id %in% c(
          "S129764356", "S35631022", "S127566274", "S127020270", "S126907180", "S125011091", 
          "S99310300", "S115052000", "S98061800", "S123772783", "S131060622", "S125927262", 
          "S121977813"
        )) |
        (COMMON.NAME == "Ruddy Turnstone" & group.id %in% c(
          "S125365793", "S94207453", "S95259238", "S35271945", "S34754628", "S34754627", 
          "S33762033", "S78041623", "S83505365", "S14038496", "S71040766"
        )) |
        (COMMON.NAME == "Great Knot" & group.id %in% c(
          "S96924803", "S126555184"
        )) |
        (COMMON.NAME == "Long-toed Stint" & group.id == "S33777908") |
        (COMMON.NAME == "Sanderling" & group.id %in% c(
          "S127159051", "S99502396", "S127736849", "S125691173", "S127861562", "S136914849", 
          "S99901327", "S124509991", "S26193571", "S97652926"
        )) |
        (COMMON.NAME == "Long-toed Stint" & group.id == "S33777908") |
        (COMMON.NAME == "Terek Sandpiper" & group.id %in% c(
          "S36033025", "S35630666", "S33422535", "S50766480", "S31619331", "S51245796", 
          "S51245795", "S51245793", "S34549167", "S33653238", "S130519567", "S54772905", 
          "S64140254", "S64139483", "S64080154"
        )) |
        (COMMON.NAME == "Small Buttonquail" & group.id %in% c(
          "S61522072", "S89775462"
        )) |
        (COMMON.NAME == "Yellow-legged Buttonquail" & group.id %in% c(
          "S118430336", "S117435760", "S117307799"
        )) |
        (COMMON.NAME == "Crab Plover" & group.id %in% c(
          "S36033025", "S35630666", "S33422535", "S134096062", "S96370806"
        )) |
        (COMMON.NAME == "Indian Courser" & group.id %in% c(
          "S97784644", "S127306334", "S83003400"
        )) |
        (COMMON.NAME == "Little Tern" & group.id == "S140952496") |
        (COMMON.NAME == "Gull-billed Tern" & group.id == "S42719659") |
        (COMMON.NAME == "White-winged Tern" & group.id == "S52672155") |
        (COMMON.NAME == "Common Tern" & group.id %in% c(
          "S63540655", "S54990627", "S50437242", "S54772905", "S95411037", "S102831394", 
          "S123771523", "S129895197", "S120874060", "S120873941", "S120873899", "S103203226", 
          "S103156027", "S41319785", "S123993212", "S24618541", "S51012577", "S33956972", "S12339162"
        )) |
        (COMMON.NAME == "Great Crested Tern" & group.id == "S24618541") |
        (COMMON.NAME == "Sandwich Tern" & group.id %in% c(
          "S62520349", "S94637178", "S67017146", "S86698583", "S75473160", "S74922890"
        )) |
        (COMMON.NAME == "Lesser Crested Tern" & group.id %in% c(
          "S57280565", "S35630802", "S34910005", "S34835793", "S34761087", "S34758889", 
          "S62520349", "S36033025", "S35630666", "S33422535", "S62357295", "S99221178", 
          "S99220121", "S99624848", "S95659171", "S95244418", "S51891676"
        )) |
        (COMMON.NAME == "Asian Woolly-necked Stork" & group.id %in% c(
          "S97227016", "S97194628", "S97186140", "S97185402", "S96872172", "S111302557", "S110664590"
        )) |
        (COMMON.NAME == "White Stork" & group.id %in% c(
          "S43131285", "S100036334", "S99923227", "S61442808", "S38778061", "S38778087", 
          "S42966179", "S43019630", "S55899926", "S55905151", "S43023811", "S54439640", 
          "S88947144", "S127557849", "S67387400", "S97695089", "S98864749"
        )) |
        (COMMON.NAME == "Black-necked Stork" & group.id %in% c(
          "S70264669", "S100076599", "S42512082"
        )) |
        (COMMON.NAME == "Indian Cormorant" & group.id == "S105410896") |
        (COMMON.NAME == "Great White Pelican" & group.id %in% c(
          "S122642372", "S98271096"
        )) |
        (COMMON.NAME == "Red-naped Ibis" & group.id %in% c(
          "S37639532", "S75847942"
        )) |
        (COMMON.NAME == "Jerdon’s Baza" & group.id %in% c(
          "S60606407", "S60606339", "S46292979", "S67613913"
        )) |
        (COMMON.NAME == "Red-headed Vulture" & group.id == "S122331698") |
        (COMMON.NAME == "Slender-billed Vulture" & group.id == "S43496484") |
        (COMMON.NAME == "Crested Serpent-Eagle" & group.id %in% c(
          "S72637664", "S57621212", "S70945094", "S70912816", "S49816263", "S136113707", 
          "S85741691", "S42965305", "S42919344", "S102965818", "S75244233", "S35784656", 
          "S63893280", "S62261189", "S54608475", "S29668389", "S133737611", "S91478425", 
          "S79623482", "S68917585", "S53078026", "S56297908", "S43974086", "S70196805", 
          "S21930328", "S126123586", "S96938135", "S104264106", "S35105786"
        )) |
        (COMMON.NAME == "Changeable Hawk-Eagle" & group.id %in% c(
          "S32263134", "S26605014", "S127071945", "S127072092", "S40389585", "S117494023", 
          "S65809077", "S102965818"
        )) |
        (COMMON.NAME == "Rufous-bellied Eagle" & group.id %in% c(
          "S119816069", "S36134519", "S32915515", "S127982412", "S99308861", "S40436853", 
          "S90210375", "S97691821", "S83785523", "S83785027"
        )) |
        (COMMON.NAME == "Black Eagle" & group.id %in% c(
          "S70196805", "S81052731", "S123237920", "S88725142"
        )) |
        (COMMON.NAME == "Tawny Eagle" & group.id %in% c(
          "S82783539", "S82782788", "S17179271", "S34181478", "S29112916", "S49247113", 
          "S53923521", "S57314082", "S35891258"
        )) |
        (COMMON.NAME == "Pallid Harrier" & group.id == "S51111996") |
        (COMMON.NAME == "Crested Goshawk" & group.id %in% c(
          "S18505248", "S103201363", "S133586111", "S47903089", "S16725097", "S27133439"
        )) |
        (COMMON.NAME == "Gray-headed Fish-Eagle" & group.id %in% c(
          "S37879951", "S58025780", "S31343531", "S53087003", "S53694039", "S53695627", 
          "S133774117", "S103686730", "S61596589", "S61596585", "S11088142"
        )) 
    ) %>% 
    distinct(COMMON.NAME, group.id)
  
  # 3. species-observer combinations
  
  mistake3 <- data %>% 
    filter(
      OBSERVER.ID == "obsr1158032" & COMMON.NAME %in% c(
        "Fulvous Whistling-Duck", "Common Pochard", "Tufted Duck", "Red Spurfowl", 
        "Painted Spurfowl", "Grey Junglefowl", "Rain Quail", "Jungle Bush-Quail", 
        "Lesser Flamingo", "White-rumped Needletail", "Alpine Swift", "Slaty-breasted Rail", 
        "Watercock", "Great Thick-knee", "Black-bellied Plover", "Lesser Sand-Plover", 
        "Greater Sand-Plover", "Ruddy Turnstone", "Common Redshank", "Indian Courser",
        "Little Tern", "Common Tern", "Sandwich Tern", "Lesser Crested Tern", "White Stork",
        "Crested Serpent-Eagle", "Short-toed Snake-Eagle", "Changeable Hawk-Eagle",
        "Black Eagle", "Tawny Eagle", "Eurasian Sparrowhawk"
      ) |
        OBSERVER.ID == "obsr701947" & COMMON.NAME == "Common Tern"
    ) %>% 
    distinct(OBSERVER.ID, COMMON.NAME)
  
  # 4. species-admin unit (state or district) combinations
  
  mistake4 <- data %>% 
    filter(
      (COMMON.NAME == "Knob-billed Duck" & COUNTY == "Tiruppur") | 
        (COMMON.NAME == "Chestnut-breasted Partridge" & COUNTY == "Darjeeling") |
        (COMMON.NAME == "Snow Partridge" & (
          (STATE == "Jammu and Kashmir" & COUNTY == "Baramulla") |
            STATE == "Ladakh"
        )) |
        (COMMON.NAME == "Himalayan Monal" & COUNTY == "Nainital") |
        (COMMON.NAME == "Red Spurfowl" & COUNTY == "Dharmapuri") |
        (COMMON.NAME == "Red Junglefowl" & COUNTY %in% c(
          "North 24 Parganas", "Purba Bardhaman"
        )) |
        (COMMON.NAME == "Grey Junglefowl" & COUNTY %in% c(
          "Sri Potti Sriramulu Nellore", "East Godavari"
        )) |
        (COMMON.NAME == "Rain Quail" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Jungle Bush-Quail" & (
          COUNTY %in% c("Sri Potti Sriramulu Nellore", "Kurnool", "YSR District (Kadapa)") |
            (STATE == "Uttar Pradesh" & COUNTY %in% c(
              "Rampur", "Lakhimpur Kheri", "Shrawasti", "Lucknow", "Ambedkar Nagar"
            ))
        )) |
        (COMMON.NAME == "Rock Bush-Quail" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Hill Pigeon" & (
          COUNTY %in% c("Bageshwar", "Nainital", "Tehri Garhwal", "Pithoragarh") |
            (STATE == "Jammu and Kashmir" & COUNTY %in% c(
              "Anantnag", "Baramulla", "Budgam", "Doda", "Ganderbal", "Kulgam", "Pulwama", 
              "Shopian", "Srinagar"
            ))
        )) |
        (COMMON.NAME == "Oriental Turtle-Dove" & COUNTY %in% c(
          "Jaisalmer", "Jodhpur", "Barmer", "Bikaner"
        )) |
        (COMMON.NAME == "Spotted Dove" & COUNTY %in% c(
          "Beed", "Hingoli", "Jalna", "Washim", "Solapur", "Latur", "Osmanabad"
        )) |
        (COMMON.NAME == "Barred Cuckoo-Dove" & COUNTY %in% c("Darrang", "Golaghat", "Nagaon")) |
        (COMMON.NAME == "Mountain Imperial-Pigeon" & (
          STATE %in% c("Kerala", "Karnataka", "Tamil Nadu") |
            COUNTY == "Cooch Behar"
        )) |
        (COMMON.NAME == "Greater Coucal" & COUNTY == "Kullu") |
        (COMMON.NAME == "Sirkeer Malkoha" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Grey-bellied Cuckoo" & COUNTY == "Sonitpur") |
        (COMMON.NAME == "Common Hawk-Cuckoo" & COUNTY == "Bageshwar") |
        (COMMON.NAME == "Indian Cuckoo" & COUNTY %in% c(
          "Darbhanga", "Madhubani", "Patna", "Bhagalpur"
        )) |
        (COMMON.NAME == "White-rumped Needletail" & COUNTY == "Chittoor") |
        (COMMON.NAME == "Indian Swiftlet" & COUNTY %in% c("Satara", "Thane", "Ratnagiri")) |
        (COMMON.NAME == "Crested Treeswift" & COUNTY %in% c("Kolhapur", "Nashik")) |
        (COMMON.NAME == "Slaty-breasted Rail" & COUNTY == "Madurai") |
        (COMMON.NAME == "Watercock" & COUNTY == "Karur") |
        (COMMON.NAME == "Slaty-legged Crake" & COUNTY == "Kalimpong") |
        (COMMON.NAME == "Brown Crake" & STATE %in% c("Bihar", "Jharkhand")) |
        (COMMON.NAME == "Sarus Crane" & COUNTY %in% c("Jaisalmer", "Churu")) |
        (COMMON.NAME == "Great Thick-knee" & COUNTY == "Churu") |
        (COMMON.NAME == "Lesser Sand-Plover" & COUNTY %in% c("Jalpaiguri", "Nashik")) |
        (COMMON.NAME == "Bronze-winged Jacana" & COUNTY %in% c(
          "Tiruchirappalli", "Ariyalur", "Karur", "Thiruvarur", "Thanjavur", "Nagapattinam", 
          "Sivagangai", "Pudukkottai", "Madurai", "Ramanathapuram", "Virudhunagar", 
          "Solapur", "Osmanabad", "Nanded", "Aurangabad", "Ahmednagar", "Bikaner", "Churu", 
          "Sikar", "Jodhpur", "Jaisalmer", "Sri Ganganagar", "Gurdaspur", "Kapurthala", 
          "Amritsar", "Ferozepur"
        )) |
        (COMMON.NAME == "Barred Buttonquail" & COUNTY %in% c("Jaisalmer", "Churu")) |
        (COMMON.NAME == "Oriental Pratincole" & COUNTY %in% c("Sonitpur", "Golaghat", "Tinsukia")) |
        (COMMON.NAME == "Common Tern" & COUNTY %in% c("Jalpaiguri", "Madurai", "Tirunelveli")) |
        (COMMON.NAME == "Asian Openbill" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Indian Cormorant" & (
          COUNTY %in% c("Jaisalmer", "Barmer", "Bikaner") |
            STATE %in% c("Himachal Pradesh", "Jammu and Kashmir")
        )) |
        (COMMON.NAME == "Red-naped Ibis" & COUNTY %in% c(
          "Hooghly", "Howrah", "Nadia", "Purba Bardhaman", "North 24 Parganas", 
          "South 24 Parganas", "Coimbatore"
        )) |
        (COMMON.NAME == "Crested Serpent-Eagle" & COUNTY %in% c(
          "Barmer", "Bikaner", "Churu", "Jaisalmer", "Solapur", "Parbhani", "Nanded", 
          "Osmanabad", "Beed", "Bhagalpur", "Darbhanga", "Gaya", "Gopalganj", "Katihar", 
          "Nalanda", "Munger", "Saharsa"
        )) |
        (COMMON.NAME == "Changeable Hawk-Eagle" & COUNTY %in% c(
          "Parbhani", "Jaisalmer", "Bikaner", "Jaipur", "Alwar", "Darjeeling", "Kalimpong", "Sikkim"
        )) |
        (COMMON.NAME == "Rufous-bellied Eagle" & COUNTY %in% c(
          "Pithoragarh", "Chamoli", "Rudraprayag", "Uttarkashi"
        )) |
        (COMMON.NAME == "Black Eagle" & COUNTY == "Washim") |
        (COMMON.NAME == "Tawny Eagle" & (
          COUNTY %in% c(
            "Ramanathapuram", "Kurnool", "Parbhani", "Aurangabad", "Jalgaon", "Nashik", 
            "Akola", "Chandrapur", "Nagpur", "East Godavari", "Visakhapatnam", "Bhagalpur"
          ) |
            STATE %in% c("Goa", "Odisha")
        )) |
        (COMMON.NAME == "Montagu’s Harrier" & STATE == "Bihar") |
        (COMMON.NAME == "Crested Goshawk" & (
          COUNTY == "Chittoor" |
            STATE %in% c("Uttarakhand", "Himachal Pradesh")
        )) |
        (COMMON.NAME == "Gray-headed Fish-Eagle" & (
          COUNTY %in% c(
            "Thiruvananthapuram", "Muzaffarnagar", "Tinsukia", "Jalpaiguri", "Alipurduar", 
            "Kalimpong", "Varanasi", "Unnao", "Kanpur Nagar", "Sawai Madhopur"
            ) |
            STATE == "Uttarakhand"
        )) 
    ) %>% 
    distinct(COMMON.NAME, STATE, COUNTY)
  
  # 5. species-admin unit-date combinations
  
  mistake5 <- data %>% 
    filter(
      (COMMON.NAME == "Lesser Sand-Plover" & COUNTY == "Malda" & !(month %in% c(3, 4, 10, 11))) 
    ) %>% 
    distinct(COMMON.NAME, STATE, COUNTY, month) 
  
  # 6. species-latlong-admin unit (state or district) combinations (or regions)
  
  mistake6 <- data %>% 
    filter(
      (COMMON.NAME == "Red Spurfowl" & (
        TEMP.REGION == "MH_plains" |
          (COUNTY == "Ballari" & LATITUDE > 15.216)
        )) |
        (COMMON.NAME == "Painted Spurfowl" & TEMP.REGION == "MH_plains") |
        (COMMON.NAME == "Grey Junglefowl" & (
          TEMP.REGION == "MH_plains" |
            (COUNTY == "Madurai" & LATITUDE > 10.07 & LONGITUDE > 77.846) |
            (COUNTY == "Tiruchirappalli" & LATITUDE > 11.138)
          )) |
        (COMMON.NAME == "Jungle Bush-Quail" & (
          (TEMP.REGION == "MH_plains" & COUNTY != "Yavatmal") |
            TEMP.REGION %in% c("TN_N-plains", "TN_C-plains", "TN_S-plains")
          )) |
        (COMMON.NAME == "Rock Bush-Quail" & (
          (COUNTY == "Pune" & LONGITUDE < 73.60) |
            (COUNTY == "Satara" & LONGITUDE < 73.85)
          )) |
        (COMMON.NAME == "Red-naped Ibis" & TEMP.REGION %in% c(
          "TN_N-plains", "TN_C-plains", "TN_EG-N"
          )) |
        (COMMON.NAME == "Black Eagle" & COUNTY == "Akola" & LATITUDE < 21.209) 
    ) %>% 
    distinct(COMMON.NAME, STATE, COUNTY, LONGITUDE, LATITUDE, TEMP.REGION)
  
    
  
  # removing probable mistakes from data
  
  filtered <- data %>% 
    anti_join(mistake1) %>% 
    anti_join(mistake2) %>% 
    anti_join(mistake3) %>% 
    anti_join(mistake4) %>% 
    anti_join(mistake5) %>% 
    anti_join(mistake6)
  
  return(filtered)
  
}


### 01 readcleanrawdata ########################################

## read and clean raw data and add important columns like group id, seasonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "00_data/ebd_IN_relFeb-2023.txt", 
                            sensitivepath = "00_data/ebd_sensitive_relFeb-2023_IN.txt")
{
  require(lubridate)
  require(tidyverse)
  
  # select only necessary columns
  preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
             "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
             "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
             "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")
  
  # CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
  # SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
  # LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
  # LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
  # TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
  # PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
  # EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
  # ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
  # GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
  # SAMPLING.EVENT.IDENTIFIER - unique checlist ID
  
  nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                   na.strings = c(""," ",NA))
  nms = names(nms)
  nms[!(nms %in% preimp)] = "NULL"
  nms[nms %in% preimp] = NA
  
  # read data from certain columns only
  data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))

  # read sensitive species data
  nms1 = read.delim(sensitivepath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                    na.strings = c(""," ",NA))
  nms1 = names(nms1)
  nms1[!(nms1 %in% preimp)] = "NULL"
  nms1[nms1 %in% preimp] = NA
  

  sesp = read.delim(sensitivepath, colClasses = nms1, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))

  # read sensitive species data
  
  
  # merge both data frames
  data = rbind(data,sesp)
  
  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(REVIEWED == 0 | APPROVED == 1) %>%
    filter(!EXOTIC.CODE %in% c("X")) %>%
    filter(CATEGORY == "species" | CATEGORY == "issf") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  
  write.csv(temp,"00_data/indiaspecieslist.csv", row.names=FALSE)
  
  ## create location file for LULC
  
  locdat = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)
  write.csv(locdat,"00_data/eBird_location_data.csv", row.names=FALSE)
  
  
  ## choosing important columns required for further analyses
  
  imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID", "REVIEWED","APPROVED","EXOTIC.CODE",
          "LOCALITY.TYPE","STATE","COUNTY",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
          "OBSERVER.ID","PROTOCOL.TYPE",
          "DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "ALL.SPECIES.REPORTED","group.id")
  

  # no of days in every month, and cumulative number
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  # create a column "group.id" which can help remove duplicate checklists
  data = data %>%
    mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))
  
  data = data %>%
    # need to combine several closely related species and slashes/spuhs
    # so, first changing their category to species since they will be combined next
    mutate(CATEGORY = case_when(COMMON.NAME %in% c("Green/Greenish Warbler",
                                                   "Siberian/Amur Stonechat",
                                                   "Red-necked/Little Stint",
                                                   "Western/Eastern Yellow Wagtail",
                                                   "Common/Himalayan Buzzard",
                                                   "Eurasian/Eastern Marsh-Harrier",
                                                   "Lesser/Greater Sand-Plover",
                                                   "Baikal/Spotted Bush Warbler",
                                                   "Lemon-rumped/Sichuan Leaf Warbler",
                                                   "Red-rumped/Striated Swallow",
                                                   "Bank Swallow/Pale Sand Martin",
                                                   "Riparia sp.",
                                                   "Greater/Mongolian Short-toed Lark",
                                                   "Taiga/Red-breasted Flycatcher",
                                                   "Tricolored x Chestnut Munia (hybrid)",
                                                   "Little/House Swift",
                                                   "Pin-tailed/Swinhoe's Snipe",
                                                   "Booted/Sykes's Warbler",
                                                   "Iduna sp.") ~ "species",
                                TRUE ~ CATEGORY)) %>%
    # combining species, slashes and spuhs
    mutate(COMMON.NAME = case_when(
      (COMMON.NAME == "Green Warbler" | COMMON.NAME == "Green/Greenish Warbler") ~ 
        "Greenish Warbler",
      (COMMON.NAME == "Amur Stonechat" | COMMON.NAME == "Siberian/Amur Stonechat") ~ 
        "Siberian Stonechat",
      (COMMON.NAME == "Red-necked Stint" | COMMON.NAME == "Red-necked/Little Stint") ~ 
        "Little Stint",
      (COMMON.NAME == "Eastern Yellow Wagtail" | COMMON.NAME == "Western/Eastern Yellow Wagtail") ~ 
        "Western Yellow Wagtail",
      (COMMON.NAME == "Himalayan Buzzard" | COMMON.NAME == "Common/Himalayan Buzzard") ~ 
        "Common Buzzard",
      (COMMON.NAME == "Eastern Marsh-Harrier" | COMMON.NAME == "Eurasian/Eastern Marsh-Harrier") ~ 
        "Eurasian Marsh-Harrier",
      (COMMON.NAME == "Greater Sand-Plover" | COMMON.NAME == "Lesser/Greater Sand-Plover") ~ 
        "Lesser Sand-Plover",
      (COMMON.NAME == "Baikal Bush Warbler" | COMMON.NAME == "Baikal/Spotted Bush Warbler") ~ 
        "Spotted Bush Warbler",
      (COMMON.NAME == "Sichuan Leaf Warbler" | COMMON.NAME == "Lemon-rumped/Sichuan Leaf Warbler") ~ 
        "Lemon-rumped Warbler",
      (COMMON.NAME == "Striated Swallow" | COMMON.NAME == "Red-rumped/Striated Swallow") ~ 
        "Red-rumped Swallow",
      (COMMON.NAME == "Pale Sand Martin" | COMMON.NAME == "Bank Swallow/Pale Sand Martin" | COMMON.NAME == "Riparia sp.") ~ 
        "Gray-throated Martin",
      (COMMON.NAME == "Mongolian Short-toed Lark" | COMMON.NAME == "Greater/Mongolian Short-toed Lark") ~ 
        "Greater Short-toed Lark",
      (COMMON.NAME == "Taiga Flycatcher" | COMMON.NAME == "Taiga/Red-breasted Flycatcher") ~ 
        "Red-breasted Flycatcher",
      (COMMON.NAME == "Chestnut Munia" | COMMON.NAME == "Tricolored x Chestnut Munia (hybrid)") ~ 
        "Tricolored Munia",
      (COMMON.NAME == "House Swift" | COMMON.NAME == "Little/House Swift") ~ 
        "Little Swift",
      (COMMON.NAME == "Swinhoe's Snipe" | COMMON.NAME == "Pin-tailed/Swinhoe's Snipe") ~ 
        "Pin-tailed Snipe",
      (COMMON.NAME == "Sykes's Warbler" | COMMON.NAME == "Booted/Sykes's Warbler" | COMMON.NAME == "Iduna sp.") ~ 
        "Booted Warbler",
      TRUE ~ COMMON.NAME
    ))
  
  # # removing probable mistakes
  # data <- rm_prob_mistakes(data)
  
  
  ## setup eBird data ##
  
  ## filter species, slice by single group ID, remove repetitions
  ## remove repeats by retaining only a single group.id + species combination
  ## set date, add month, year and day columns using package LUBRIDATE
  ## add number of species/list length column (no.sp), for list length analyses (lla)
  
  
  data = data %>%
    group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
    dplyr::select(all_of(imp)) %>%
    mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           month = month(OBSERVATION.DATE),
           day = day(OBSERVATION.DATE) + cdays[month], 
           #week = week(OBSERVATION.DATE),
           #fort = ceiling(day/14),
           cyear = year(OBSERVATION.DATE)) %>%
    dplyr::select(-c("OBSERVATION.DATE")) %>%
    mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
    group_by(group.id) %>% mutate(no.sp = n_distinct(COMMON.NAME)) %>%
    ungroup
  
  data = data %>% filter(year < 2023)
  names(data)[names(data) == "STATE"] = "ST_NM"
  names(data)[names(data) == "COUNTY"] = "DISTRICT"
  
  ## remove probable mistakes
  
  assign("data",data,.GlobalEnv)
  
  # save workspace
  save(data, file="00_data/rawdata.RData")
  rm(data, pos = ".GlobalEnv")
}

### 02 addmapvars ########################################

# Load sf map objects and add to dataset. 
# - admin & PA boundaries;
# - square grids at 5 resolutions (5, 25, 50, 100, 200 km*km), unclipped and clipped to India;
# - queen and rook neighbours info for 4 grids (25, 50, 100, 200)
# See the India Maps repo:
# https://github.com/birdcountindia/india-maps/blob/main/scripts/create_maps_sf.R

addmapvars = function(datapath = "00_data/rawdata.RData", 
                      mappath1 = "00_data/grids_sf_full.RData", 
                      mappath2 = "00_data/grids_g0_sf.RData",
                      mappath3 = "00_data/maps_sf.RData",
                      papath = "00_data/maps_pa_sf.RData",
                      maskspath = "00_data/habmasks_sf.RData")
{
  require(tidyverse)
  require(sf)
  
  load(datapath)
  
  # map details to add to eBird data
  load(mappath1)
  load(mappath2)
  load(mappath3)
  load(papath)
  
  load(maskspath)
  names(habmasks_sf)[1] = "gridg1"
  
  # to later filter pelagics
  india_buff_sf <- india_buff_sf %>% mutate(INLAND = 1)

  
  
  sf_use_s2(FALSE)
  
  temp = data %>%
    distinct(group.id, LONGITUDE, LATITUDE) %>% 
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    # joining map vars to EBD
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
    st_set_crs(st_crs(india_sf)) %>%
    # PAs
    st_join(pa_sf %>% dplyr::select(NAME)) %>%
    # grid cells
    st_join(g0_sf %>% dplyr::select(GRID.G0)) %>% 
    st_join(g1_sf %>% dplyr::select(GRID.G1)) %>% 
    st_join(g2_sf %>% dplyr::select(GRID.G2)) %>% 
    st_join(g3_sf %>% dplyr::select(GRID.G3)) %>% 
    st_join(g4_sf %>% dplyr::select(GRID.G4)) %>% 
    st_join(india_buff_sf %>% dplyr::select(INLAND)) %>% 
    st_drop_geometry()
  
  temp = temp %>% 
    distinct(NAME,GRID.G0,GRID.G1,GRID.G2,GRID.G3,GRID.G4,group.id,X) %>% 
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    magrittr::set_colnames(c("pa.name","gridg0","gridg1","gridg2","gridg3",
                             "gridg4","group.id","INLAND"))
  
  data = data %>% 
    left_join(temp) %>% 
    # removes pelagics
    filter(!is.na(INLAND)) %>% 
    dplyr::select(-INLAND) %>% 
    left_join(habmasks_sf)
  
  ### 
  
  assign("data",data,.GlobalEnv)

  save(data, file="00_data/data.RData")
  rm(data, pos = ".GlobalEnv")
  
}


###    completelistcheck ########################################

## remove all probable errors
## type can be "trends" or "range"
## to use in dataspeciesfilter()

completelistcheck = function(data)
{
  require(tidyverse)
  require(lubridate)

  data = data %>% 
    # create 2 columns from the "TIME.OBSERVATIONS.STARTED' column
    mutate(DATETIME = as_datetime(paste("2023-06-01", # any date, we just need the time
                                        TIME.OBSERVATIONS.STARTED)),
           hr = hour(DATETIME),
           min = minute(DATETIME)) %>% 
    # calculate speed and species/unit time (sut)
    mutate(speed = EFFORT.DISTANCE.KM*60/DURATION.MINUTES, # kmph
           sut = no.sp*60/DURATION.MINUTES, # species per hour
           # calculate hour checklist ended
           end = floor((hr*60 + min + DURATION.MINUTES)/60))
  
  # set thresholds for speed and sut
  vel = 20
  time = 2
  
  
  # exclude any list that may in fact be incomplete ###
  
  # list of on-paper complete lists
  temp = data %>%
    filter(ALL.SPECIES.REPORTED == 1, PROTOCOL.TYPE != "Incidental") %>%
    group_by(group.id) %>% slice(1)
  
  # choose checklists without info on duration with 3 or fewer species
  grp = temp %>%
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    distinct(group.id)
  grp = grp$group.id
  
  # exclude records based on various criteria 
  data = data %>%
    mutate(ALL.SPECIES.REPORTED = case_when(
      # fake complete lists
      ALL.SPECIES.REPORTED == 1 & 
        (EFFORT.DISTANCE.KM > 10 | # remove travelling lists covering >10 km
           group.id %in% grp | # lists without info on duration with 3 or fewer species
           speed > vel | # too fast
           (DURATION.MINUTES < 3) | # too short
           (sut < time & no.sp <= 3) | # species per unit time too slow
           PROTOCOL.TYPE == "Incidental" | # incidental
           (!is.na(hr) & ((hr <= 4 & end <= 4) | # nocturnal filter
                            (hr >= 20 & end <= 28)))) ~ 0, 
      # true incomplete lists
      ALL.SPECIES.REPORTED == 0 ~ 0,
      # true complete lists
      TRUE ~ 1
    )) %>% 
    dplyr::select(-speed,-sut,-hr,-min,-end) 
}

###    removevagrants ########################################

## remove vagrants
## to use in dataspeciesfilter()

removevagrants = function(data)
{
  # mapping of SoIB-species-of-interest to a range of variables/classifications
  # (manually created)
  fullmap = read.csv("00_data/SoIB_mapping_2022.csv")
  
  migspecies = fullmap %>%
    filter(!Migratory.Status.Within.India %in% c("Resident",
                                                 "Resident & Altitudinal Migrant",
                                                 "Resident & Winter Migrant",
                                                 "Resident & Summer Migrant",
                                                 "Resident & Local Migrant",
                                                 "Resident & Localized Summer Migrant",
                                                 "Resident & Within-India Migrant",
                                                 "Resident (Extirpated)")) %>%
    dplyr::select(eBird.English.Name.2022) %>% 
    as.vector() %>% 
    list_c()
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4, month, COMMON.NAME) %>%
    reframe(nyear = n_distinct(year)) %>%
    filter(nyear <= 3) %>% 
    dplyr::select(gridg4, month, COMMON.NAME)
  
  d = left_join(d, data) %>%
    filter(year > 2014)
  
  save(d, file = "00_data/vagrantdata.RData")
  
  data = anti_join(data, d)
  return(data)
}



###    dataspeciesfilter ########################################

# <annotation_pending_AV> elaborate below
# select species for State of India's Birds, and species for historical and recent trends
# includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(
    
  # thresholds for species to be considered in each analysis
  locationlimit = 15, # individual locations
  gridlimit = 4, # grid cells
  listlimit = 50, # checklists
  cur_mask = "none"
  
) {
  
  # ensuring only valid cur_mask names are provided
  if (!(cur_mask %in% unique(analyses_metadata$MASK))) {
    return('Invalid mask! Please provide valid mask name, one of: c("none","woodland","cropland","ONEland","PA").')
  }
  
  
  # preparing data for mask ###
  
  cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
  
  if (cur_mask == "none"){
    data0 = data_base
  } else if (cur_mask == "woodland"){
    data0 = data_base %>% filter(maskWdl == 1)
  } else if (cur_mask == "cropland"){
    data0 = data_base %>% filter(maskCrp == 1)
  } else if (cur_mask == "ONEland"){
    data0 = data_base %>% filter(maskOne == 1)
  } else if (cur_mask == "PA"){
    data0 = data_base %>% filter(!is.na(pa.name))
  } else {
    # states
    data0 = data_base %>% filter(ST_NM == cur_mask)
  } 
  
  
  # processing dataspeciesfilter --------------------------------------------
  
  data = data0 %>% 
    dplyr::select(-CATEGORY,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                  -LOCALITY.TYPE,-LOCALITY.ID,-pa.name,-maskWdl,-maskCrp,-maskOne,
                  -LATITUDE,-LONGITUDE,-PROTOCOL.TYPE,-EXOTIC.CODE,-day,-cyear,
                  -DURATION.MINUTES,-TIME.OBSERVATIONS.STARTED,-EFFORT.DISTANCE.KM)
  
  stats7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
                 "filter 1 usable observations")
  stats8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "filter 2 unique complete checklists")
  stats9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                      data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "pre-2000 checklists")
  
  # summary for each timegroup
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% 
    reframe(lists = n_distinct(group.id), 
            year = round(median(year))) %>%
    arrange(year)
  
  
  # historical data (data from before 2000 onwards), used for long-term trends
  # gives list of species for which we have enough data and this analysis can be done
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf")) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(locs = n_distinct(LOCALITY.ID), 
            cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% 
    dplyr::select(COMMON.NAME, ht)
  
  # recent data (data from 2015 onwards), used for recent trends
  # gives list of species for which we have enough data and this analysis can be done
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf"), 
           year > 2014) %>%
    group_by(COMMON.NAME, year) %>%
    reframe(locs = n_distinct(LOCALITY.ID), 
            cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% 
    dplyr::select(COMMON.NAME, rt)
  
  
  # for other species that don't qualify simple rules above (restricted range)
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf")) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    dplyr::select(COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),
                                                spec_resident),
                            validh = NA_real_)
  
  # if the grids in which species has been reported a few times have sufficient lists
  # from enough years, still consider for analysis
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1) %>%
      left_join(data0) %>%
      group_by(timegroups) %>% 
      reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
    
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf"), 
           year > 2014) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    dplyr::select(COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),
                                                spec_resident),
                            validr = NA_real_)
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1) %>%
      left_join(data0) %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% 
      reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
    
  }
  
  
  # full species list (historical + recent) ###
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  # joining info for normal species (non-range-restricted)
  dataf = dataf %>% 
    left_join(datah, by = c("COMMON.NAME")) %>% 
    left_join(datar, by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    # <annotation_pending_AV> what does each variable mean? (e.g., Essential)
    # what are we finally filtering for?
    filter((Essential == 1 | Endemic.Region != "None" | ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    dplyr::select(COMMON.NAME, ht, rt)
  
  # <annotation_pending_AV> why filtering dataf also? (instead of specieslist above)
  dataf <- dataf %>% 
    mutate(ht = case_when(Breeding.Activity.Period == "Nocturnal" &
                            Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(Breeding.Activity.Period == "Nocturnal" &
                            Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                          TRUE ~ rt))
  
  # ignoring species that are frequently misIDd
  specieslist <- specieslist %>% 
    mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ rt))
  
  
  # <annotation_pending_AV> 
  # why left_joining (then removing misIDd specs) separately again? 
  restrictedspecieslist = data.frame(species = specieslist$COMMON.NAME) %>% 
    left_join(speciesresth) %>% 
    left_join(speciesrestr) %>%
    # valid for at least 1 of 2 analyses
    filter(!is.na(validh) | !is.na(validr)) %>% 
    magrittr::set_colnames(c("COMMON.NAME", "ht", "rt")) %>% 
    mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ rt))  
  
  
  # filtering for only species in certain masks ###
  if (cur_mask == "woodland") {
    
    specieslist <- specieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ rt))
    
    restrictedspecieslist = restrictedspecieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ rt))
    
  } else if (cur_mask %in% c("cropland", "ONEland")) {
    
    specieslist <- specieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ rt))
    
    restrictedspecieslist = restrictedspecieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ rt))
    
  }
  
  
  # <annotation_pending_AV> what exactly are we checking?
  check1 = restrictedspecieslist %>% 
    filter(!is.na(ht)) %>% 
    dplyr::select(COMMON.NAME) %>% as.vector() %>% list_c()
  check2 = restrictedspecieslist %>% 
    filter(!is.na(rt)) %>% 
    dplyr::select(COMMON.NAME) %>% as.vector() %>% list_c()
  
  
  # <annotation_pending_AV> 
  randomcheck_a = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1, 
           COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    filter(n > 7)
  
  randomcheck_b = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1, 
           COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    filter(n <= 7)
  
  
  # <annotation_pending_AV> 
  restrictedspecieslist_a = restrictedspecieslist %>% 
    filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME) %>% 
    mutate(mixed = 1)
  restrictedspecieslist_b = restrictedspecieslist %>% 
    filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME) %>% 
    mutate(mixed = 0)
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  
  
  # <annotation_pending_AV> 
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) &
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  t2 = dataf %>%
    filter((Endemic.Region != "None" | ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | ht == 1 | rt == 1) &
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  
  stats10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  stats11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  stats12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  
  # <annotation_pending_AV> 
  specieslist1 = specieslist %>% 
    mutate(selected = 1) %>% 
    dplyr::select(COMMON.NAME, selected)
  
  dataf = dataf %>%
    dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, ht, rt) %>% 
    left_join(specieslist1) %>% 
    magrittr::set_colnames(c("COMMON.NAME","SCIENTIFIC.NAME",
                             "Long.Term.Analysis","Current.Analysis",
                             "Selected.SOIB")) %>%  
    # converting to report table-style with blanks for NAs and Xs for 1s
    mutate(across(everything(), ~ as.character(.))) %>% 
    mutate(across(everything(), ~ replace_na(., replace = ""))) %>% 
    mutate(across(everything(), ~ str_replace(., pattern = "1", replacement = "X"))) %>% 
    # also including species in checks
    mutate(Long.Term.Analysis = if_else(COMMON.NAME %in% check1, "X", Long.Term.Analysis),
           Current.Analysis = if_else(COMMON.NAME %in% check2, "X", Current.Analysis)) %>%
    dplyr::select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                  "Selected.SOIB")
  
  
  # filtering for only species in certain masks ###
  if (cur_mask == "woodland") {
    
    dataf <- dataf %>% 
      mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Long.Term.Analysis),
             Current.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Current.Analysis),
             Selected.SOIB = if_else(!(COMMON.NAME %in% spec_woodland), "", Selected.SOIB))
    
  } else if (cur_mask %in% c("cropland", "ONEland")) {
    
    dataf <- dataf %>% 
      mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Long.Term.Analysis),
             Current.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Current.Analysis),
             Selected.SOIB = if_else(!(COMMON.NAME %in% spec_openland), "", Selected.SOIB))
    
  }
  
  
  # number of sampled grid cell at each resolution
  sampledcells = c(length(unique(data0$gridg0)),
                   length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),
                   length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(stats1, stats2, stats3, stats4, stats5, stats6,
            stats7, stats8, stats9, stats10, stats11, stats12)
  
  
  
  # additional filtering safeguards - proportion of range sampled during every timegroup
  
  temp = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, 
           ALL.SPECIES.REPORTED == 1)
  
  totalrange = temp %>%
    group_by(COMMON.NAME) %>% 
    reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = temp %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = temp %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = temp %>%
    filter(timegroups %in% as.character(2015:2022)) %>%
    group_by(COMMON.NAME, timegroups) %>% 
    reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = totalrange %>% 
    left_join(proprange2000) %>% 
    left_join(proprange.current) %>% 
    left_join(proprange2022) %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  
  # additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% 
    reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1,
           COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME, gridg1)
  
  samp25km5km = spec25km %>% 
    left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% 
    reframe(mean5km = mean(n), 
            ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km) %>% left_join(samp25km5km)
  
  
  # writing filtered data files ---------------------------------------------
  
  # <annotation_pending_AV> short, about each file saved
  
  
  # <annotation_pending_AV>
  write.csv(dataf, row.names = F, 
            file = cur_metadata$FULLSPECLIST.PATH)
  
  
  # <annotation_pending_AV>
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID, group.id, month, timegroups)
  
  write.csv(locs_write, row.names = F, 
            file = cur_metadata$LOCS.PATH)
  
  
  # <annotation_pending_AV>
  save(specieslist, restrictedspecieslist, databins, 
       file = cur_metadata$SPECLISTDATA.PATH)
  
  save(data, sampledcells, databins, stats, 
       file = cur_metadata$DATA.PATH)
  
}



### expandbyspecies ########################################


## ensure that the working directory has list of India's birds with scientific names 
## (just a safety mechanism for the function to work for small subsets, needs to be enabled if required)
## only need to input data, the species of interest and the complete list of India's bird species
## also groupspecs if required (a dataframe with all relevant list level info), it is defaulted to data

expandbyspecies = function(data, species)
{
  require(tidyverse)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)

  data$timegroups = as.factor(data$timegroups)
  
  ## considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1,gridg2,gridg3,gridg4,
             ALL.SPECIES.REPORTED,OBSERVER.ID,
             #city,
             #DURATION.MINUTES,EFFORT.DISTANCE.KM,
             group.id,month,year,no.sp,timegroups,
             timegroups1)
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  expanded = expanded %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                     "ALL.SPECIES.REPORTED","group.id","year","timegroups1"))
  
  ## deal with NAs
  
  expanded = expanded %>% mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, is.na(OBSERVATION.COUNT), "0"))
  
  
  expanded = expanded %>%
    mutate(OBSERVATION.COUNT=replace(OBSERVATION.COUNT, OBSERVATION.COUNT != "0", "1"))
  
  
  
  expanded$OBSERVATION.COUNT = as.numeric(expanded$OBSERVATION.COUNT)

  return(expanded)
}



### error operations ########################################

# <annotation_pending_AV>
errordiv = function(x1,x2,se1,se2)
{
  r = x1/x2
  t = data.frame(se1/x1,se2/x2)
  ser = r*sqrt(t[,1]^2 + t[,2]^2)
  a = data.frame(freq = numeric(length(r)))
  a$freq = r
  a$se = ser
  return(a)
}

erroradd = function(vec)
{
  err = sqrt(sum(vec^2))
  return(err)
}

# <annotation_pending_AV>
simerrordiv = function(x1, x2, se1, se2)
{
  # takes untransformed (link) mean and SE values, and generates normal dist. from 1000 sims,
  # then transformed 
  # after the function, lower and upper quantiles are selected as limits of 95% CI
  tp = data.frame(num = clogloglink(rnorm(1000, x1, se1), inverse = T), 
                  den = clogloglink(rnorm(1000, x2, se2), inverse = T)) %>%
    reframe(rat = num/den, 
            val = num)

  return(tp)
}






### occupancy ########################################

## occupancy analyses for bird abundance/range, reports area in units of 10000 sq. km.
## Requires tidyverse, reshape2, data.table and unmarked
## type = trivial, null, nosp, nosptime, nb, nosptimenb

occufreq = function(data, species, areag, rerun = F, datatofill)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  load("00_data/neighbours.RData")
  
  migstatus = read.csv("Migratory Status - Migratory Status.csv")

  migstatus = migstatus %>%
    mutate(mig = 
             case_when(!is.na(Summer.Visitor) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Resident) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Summer.Visitor) & !is.na(Resident) ~ "LM",
                       !is.na(Summer.Visitor) ~ "S",
                       !is.na(Winter.Visitor) | !is.na(Strictly.Passage) ~ "W/P",
                       !is.na(Uncertain.Vagrant) & is.na(Resident) ~ "U",
                       TRUE ~ "R")
    ) %>%
    select(eBird.English.Name,mig)
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Himalayan Cuckoo","Common Cuckoo",
                                                    "Watercock")] = "S"
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Indian Skimmer","Black-bellied Tern",
                                                    "Black-capped Kingfisher",
                                                    "Mountain Chiffchaff","Red-rumped Swallow",
                                                    "Fire-capped Tit")] = "R"
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Smoky Warbler","Wallcreeper",
                                                    "Long-billed Pipit")] = "W/P"
  
  if(rerun)
  {
    species = datatofill$species
    
    temp = datatofill %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    
    species = species[species %in% temp$species]
  }
  
  migstatus = migstatus %>%
    filter(eBird.English.Name %in% species)
  
  species = as.character(migstatus$eBird.English.Name)
  speciesf = species
  mig = migstatus$mig
  migf = mig
  
  spec = species[mig == "LM"]
  species = c(species,spec)
  mig[mig == "LM"] = "MS"
  mig = c(mig,rep("MW",length(spec)))
  
  data = data %>%
    filter(year > 2013)
  
  data = data %>%
    mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, !is.na(OBSERVATION.COUNT), "1"))
  
  data$OBSERVATION.COUNT = as.numeric(data$OBSERVATION.COUNT)
  
  # create dataframe to store occupancy and detection proabability 
  # estimates across species and spatial resolutions
  

  est = array(data=NA,dim=c(length(speciesf),11),
              dimnames=list(speciesf,c("detprobB","occB","occB.ci","trivB","sampareaB","detprobM",
                                       "occM","occM.ci","trivM","sampareaM","status")))
  
  if(rerun)
  {
    est[,1] = datatofill$detprobB
    est[,2] = datatofill$occB*10000
    est[,3] = datatofill$occB.ci*10000
    est[,4] = datatofill$trivB*10000
    est[,5] = datatofill$sampareaB*10000
    est[,6] = datatofill$detprobM
    est[,7] = datatofill$occM*10000
    est[,8] = datatofill$occM.ci*10000
    est[,9] = datatofill$trivM*10000
    est[,10] = datatofill$sampareaM*10000
    est[,11] = datatofill$migstatus
    
    if (migstatus == "LM")
    {
      index = 1:length(species)
      x = numeric(0)
      
      for (i in unique(species))
      {
        temp = datatofill %>%
          filter(species == i, migstatus == "LM")
        
        if (length(temp$species) != 0)
        {
          if (!is.na(temp$trivB) & (!is.na(temp$occB) & !is.na(temp$occB.ci)))
          {
            x1 = intersect(index[species == i], index[mig == "MS"])
            x = c(x,index[x1])
          }
          if (!is.na(temp$trivM) & (!is.na(temp$occM) & !is.na(temp$occM.ci)))
          {
            x2 = intersect(index[species == i], index[mig == "MW"])
            x = c(x,index[x2])
          }
        }
      }
      species = species[-x]
      mig = mig[-x]
    }
    

  }

  for(s in 1:length(species))
  {
    if(rerun)
    {
      if (is.na(datatofill$sampareaB[s]) & is.na(datatofill$sampareaM[s]))
        next
    }
    
    if (mig[s] == "S" | mig[s] == "W/P")
    {
      temp1 = data %>%
        filter(COMMON.NAME == species[s]) %>%
        distinct(month)
      
      datac = temp1 %>% left_join(data)
    }
    
    if (mig[s] == "R" | mig[s] == "U")
    {
      datac = data
    }
    
    if (mig[s] == "MS")
    {
      datac = data %>%
        filter(month %in% c(5:8))
    }
    
    if (mig[s] == "MW")
    {
      datac = data %>%
        filter(month %in% c(11:12,1:2))
    }
    
    datay = datac %>%
      group_by(gridg1,group.id) %>% slice(1) %>% ungroup %>%
      group_by(gridg1) %>% reframe(medianlla = median(no.sp)) %>%
      group_by(gridg1) %>%
      reframe(medianlla = round(mean(medianlla)))
    medianlla = datay$medianlla
    
    sampledarea = left_join(datac,areag,by = c("gridg1" = "id"))
    sampledarea = sampledarea %>% distinct(gridg1,area)
    len = length(sampledarea$area)
    sampledarea = sum(sampledarea$area)
    
    if (length(datac$COMMON.NAME[datac$COMMON.NAME == species[s]]) == 0)
    {
      estdf = data.frame(rep(rownames(est)))
      names(estdf) = "species"
      
      estdf$detprobB = NA
      estdf$occB = NA
      estdf$occB.ci = NA
      estdf$trivB = NA
      estdf$sampareaB = NA
      estdf$detprobM = NA
      estdf$occM = NA
      estdf$occM.ci = NA
      estdf$trivM = NA
      estdf$sampareaM = NA
      
      names(migstatus)[2] = "migstatus"
      estdf = left_join(estdf,migstatus,by = c("species" = "eBird.English.Name"))
      
      return(estdf)
    }
    
    selexp = expandbyspecies(datac,species[s])
    
    selexp = selexp[sample(1:nrow(selexp)),]
    
    selexp$month[selexp$month %in% c(11,12,1,2)] = "Win"
    selexp$month[selexp$month %in% c(3,4,5,6)] = "Sum"
    selexp$month[selexp$month %in% c(7,8,9,10)] = "Mon"
    
    nb8g = nb8g1
    lpg = selexp %>%
      group_by(gridg1) %>% summarize(lpg = n())
    listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
    inc = datac %>%
      mutate(gridg = gridg1)
    selexp = selexp %>% 
      arrange(gridg1) %>%
      mutate(gridg = gridg1) %>%
      group_by(gridg) %>% mutate(group.id = 1:n()) %>% ungroup %>%
      left_join(areag,by = c("gridg" = "id"))
      
    
    nbt = selexp %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
    nbt$nb8 = 0
      
    nbti = inc %>%
      filter(COMMON.NAME == species[s]) %>%
      left_join(areag,by = c("gridg" = "id")) %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT), area = mean(area)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
      
    fil = sum(nbti$fl*nbti$area)
      
      
    setDT(selexp)
    
    det = dcast(selexp, gridg ~ group.id, value.var = "OBSERVATION.COUNT")
    cov.month = dcast(selexp, gridg ~ group.id, value.var = "month")
    cov.nosp = dcast(selexp, gridg ~ group.id, value.var = "no.sp")
    
    det = setDF(det)
    cov.month = setDF(cov.month)
    cov.nosp = setDF(cov.nosp)
    
    det = det[,1:listcutoff]
    cov.month = cov.month[,1:listcutoff]
    cov.nosp = cov.nosp[,1:listcutoff]
      
    nbt$gridg = as.character(nbt$gridg)
    nbti$gridg = as.character(nbti$gridg)

    for (i in 1:length(nbt$gridg))
    {
      temp = as.numeric(nb8g[[nbt$gridg[i]]])
      sm = sum(nbti[nbti$gridg %in% temp,]$fl)/length(temp)
      nbt$nb8[i] = sm
    }
        
    nbt$gridg = as.character(nbt$gridg)
    tp = nbt
    tp1 = nbt %>% select(-fl)
    #tp = left_join(nbti,tp1)
    nbt = nbt[,-2]
    
    nbtx = tp[tp$fl != 1,]
    
    detn = data.frame(gridg = det[,1])
    detn= left_join(detn,nbt)

    umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb8g = detn$nb8), 
                            obsCovs = list(cov1 = cov.nosp[,-1], 
                            cov2 = cov.month[,-1]))
    
    
    if (mig[s] == "R")
    {
      occ_det = tryCatch({occu(~log(cov1)*cov2 ~nb8g, data=umf, starts = c(0,0,0,0,0,0,0,0))},
                         error = function(cond){"skip"})
      
      newdat1 = data.frame(cov1=medianlla, cov2=factor(c("Mon","Win","Sum")))
      newdat2 = data.frame(nb8g=nbtx$nb8)
    }
    
    if (mig[s] != "R")
    {
      occ_det = tryCatch({occu(~log(cov1) ~nb8g, data=umf, starts = c(0,0,0,0))},
                         error = function(cond){"skip"})
      
      newdat1 = data.frame(cov1=medianlla)
      newdat2 = data.frame(nb8g=nbtx$nb8)
    }
    

    if (!is.character(occ_det))
    {
      f1 = predict(occ_det, newdata = newdat1, type = "det")
      f1 = mean(f1$Predicted)
      f2 = predict(occ_det, newdata = newdat2, type = "state")
      f2$nb = newdat2$nb8g
      f2$gridg = nbtx$gridg
      f2 = left_join(f2,areag,by = c("gridg" = "id"))
      f2 = f2 %>% filter(!is.na(Predicted))
      f2a = sum(f2$Predicted*f2$area) + fil
      f2b = round((erroradd(f2$SE*f2$area))*1.96)
      
      
      if (mig[s] == "R" | mig[s] == "MS" | mig[s] == "S")
      {
        est[species[s],"detprobB"] =  f1
        est[species[s],"occB"] = f2a
        est[species[s],"occB.ci"] = f2b
      }
      
      if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
      {
        est[species[s],"detprobM"] =  f1
        est[species[s],"occM"] = f2a
        est[species[s],"occM.ci"] = f2b
      }

    }
    
    if (mig[s] == "R" | mig[s] == "MS" | mig[s] == "S")
    {
      est[species[s],"trivB"] = fil
      est[species[s],"sampareaB"] = sampledarea
    }
    
    if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
    {
      est[species[s],"trivM"] = fil
      est[species[s],"sampareaM"] = sampledarea
    }
  }  
  estdf = data.frame(rep(rownames(est)))
  names(estdf) = "species"

  
  estdf$detprobB = round(as.numeric(est[,1]),3)
  estdf$occB = round(as.numeric(est[,2]),3)/10000
  estdf$occB.ci = round(as.numeric(est[,3]),3)/10000
  estdf$trivB = round(as.numeric(est[,4]),3)/10000
  estdf$sampareaB = round(as.numeric(est[,5]),3)/10000
  estdf$detprobM = round(as.numeric(est[,6]),3)
  estdf$occM = round(as.numeric(est[,7]),3)/10000
  estdf$occM.ci = round(as.numeric(est[,8]),3)/10000
  estdf$trivM = round(as.numeric(est[,9]),3)/10000
  estdf$sampareaM = round(as.numeric(est[,10]),3)/10000
  
  names(migstatus)[2] = "migstatus"
  estdf = left_join(estdf,migstatus,by = c("species" = "eBird.English.Name"))

  return(estdf)
}

SoIBoccupancy = function(data,species,areag)
{
  a = occufreq(data,species,areag)
  c = 0
  repeat
  {
    c = c + 1
    temp = a %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    if(length(temp$species) == 0)
      break
    if(c == 10)
      break
    a = occufreq(data,species,areag,rerun=T,datatofill=a)
  }
  return(a)
}



###    create a set of locations ########################################

# <annotation_pending_AV> why do we do this in the first place?

# locs is a data frame with location, group id info

createrandomlocs = function(locs)
{
  require(tidyverse)
  
  locs1 = locs %>% 
    group_by(LOCALITY.ID, month, timegroups) %>% sample_n(1)
  
  return(locs1$group.id)
}


###    singlespeciesrun (run models) ########################################

singlespeciesrun = function(data, species, specieslist, restrictedspecieslist)
{
  require(tidyverse)
  require(merTools)
  
  data1 = data
  
  # <annotation_pending_AV> why this intermediate specieslist1 object? 
  # can't we do with just the second one?
  specieslist1 = specieslist %>% filter(COMMON.NAME %in% species)
  specieslist2 = specieslist1 %>% filter(COMMON.NAME == species)
  
  # three different flags for three different model types that will be run.
  # 0 is normal model, with full random effects. depending on restricted species,
  # model changes slightly.
  flag = 0
  if (species %in% restrictedspecieslist$COMMON.NAME)
  {
    flag = 1
    restrictedlist1 = restrictedspecieslist %>% filter(COMMON.NAME == species)
    specieslist2$ht = restrictedlist1$ht
    specieslist2$rt = restrictedlist1$rt
    
    if (restrictedlist1$mixed == 0) {
      flag = 2
    }
  }
  
  # filters data based on whether the species has been selected for long-term trends (ht) 
  # or short-term trends (rt) 
  # (if only recent, then need to filter for recent years. else, use all years so no filter.)
  
  if (is.na(specieslist2$ht) & !is.na(specieslist2$rt))
  {
    data1 = data1 %>% filter(year >= 2015)
  }
  
  data1 = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data1)
  
  tm = data1 %>% distinct(timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  datay = data1 %>%
    group_by(gridg3, gridg1, group.id) %>% 
    slice(1) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  
  # expand dataframe to include absences as well
  ed = expandbyspecies(data1, species) %>% 
    # converting months to seasons
    mutate(month = as.numeric(month)) %>% 
    mutate(month = case_when(month %in% c(12,1,2) ~ "Win",
                             month %in% c(3,4,5) ~ "Sum",
                             month %in% c(6,7,8) ~ "Mon",
                             month %in% c(9,10,11) ~ "Aut")) %>% 
    mutate(month = as.factor(month))


  # the model ---------------------------------------------------------------
  
  if (flag == 0)
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1), 
               data = ed, family = binomial(link = 'cloglog'), 
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (flag == 1)
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg1), 
               data = ed, family = binomial(link = 'cloglog'), 
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (flag == 2)
  {
    m1 = glm(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups, 
             data = ed, family = binomial(link = 'cloglog'))
  }
  

  # predicting from model ---------------------------------------------------

  # prepare a new data file to predict
  ltemp <- ed %>% 
    group_by(month) %>% 
    reframe(timegroups = unique(tm$timegroups)) %>% 
    mutate(no.sp = medianlla,
           # <annotation_pending_AV> why taking 1st value?
           gridg1 = data1$gridg1[1], 
           gridg3 = data1$gridg3[1])

  f2 <- ltemp %>% 
    dplyr::select(timegroups) %>% 
    # this is not actually needed
    mutate(freq = 0, se = 0)
  
  
  if (flag != 2)
  {
    #pred = predict(m1, newdata = ltemp, type = "response", re.form = NA, allow.new.levels=TRUE)
    pred = predictInterval(m1, newdata = ltemp, which = "fixed",
                           level = 0.48, type = "linear.prediction")
    f2$freqt = pred$fit
    f2$set = pred$fit-pred$lwr
  }
  
  if (flag == 2)
  {
    pred = predict(m1, newdata = ltemp, type = "link", se.fit = T)
    f2$freqt = pred$fit
    f2$set = pred$se.fit
  }
  
  f1 = f2 %>%
    filter(!is.na(freqt) & !is.na(se)) %>%
    group_by(timegroups) %>% 
    reframe(freq = mean(freqt), se = mean(set)) %>% 
    right_join(tm) %>% 
    left_join(databins %>% distinct(timegroups, year)) %>% 
    rename(timegroupsf = timegroups,
           timegroups = year) %>% 
    mutate(timegroupsf = factor(timegroupsf, 
                               levels = c("before 2000","2000-2006","2007-2010",
                                          "2011-2012","2013","2014","2015","2016",
                                          "2017","2018","2019","2020","2021","2022"))) %>% 
    complete(timegroupsf) %>% 
    arrange(timegroupsf)
  
  
  tocomb = c(species, f1$freq, f1$se)
  return(tocomb)
  # each species's tocomb becomes one column in final trends0 output object
  
}
