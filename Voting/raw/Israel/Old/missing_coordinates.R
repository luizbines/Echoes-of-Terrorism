# LUIZ BINES
# February 2024
# This script adds coordinates to complicated google maps locations

# Library
library(dplyr)

# wd
setwd("C:/Users/luizb/Desktop/Dissertation")

# Importing

files = list.files(path = 'Attacks/Cities/',
                   pattern = '*.csv',
                   full.names = T)

data = lapply(files, function(x){
  df = read.csv(x, header = F, stringsAsFactors = F, skip = 1)
df[] = lapply(df, as.character)
return(df)
}) %>% bind_rows()


# geografic boundaries
lat_min = 29.46
lat_max = 33.32
long_min = 34.19
long_max = 35.57

data = data %>%
  rename(
    location = V2,
    latitude = V3,
    longitude = V4
  ) %>%
  mutate(location = gsub(location, pattern = '\\+', replacement = ' '),
         location = gsub(location, pattern = 'Israel', replacement = ''),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         latitude = ifelse(latitude < lat_min | latitude > lat_max |
                             longitude < long_min| longitude > long_max, NA, latitude),
         longitude = ifelse(is.na(latitude), NA, longitude))


missing = data[is.na(data$latitude),] %>% 
  select(-latitude,-longitude)



missing_coords = c(
    'https://www.google.com/maps/place/Abu+Qrenat,+Israel/@31.1031086,34.9130342,14z/data=!3m1!4b1!4m10!1m2!2m1!1z15DXkdeVINen16jXmdeg15DXqiDXldeU16TXlteV16jXlCAgIElzcmFlbA!3m6!1s0x1502440747fa9463:0x858a7cce894f87e3!8m2!3d31.103115!4d34.952003!15sCivXkNeR15Ug16fXqNeZ16DXkNeqINeV15TXpNeW15XXqNeUICAgSXNyYWVskgEMc3VibG9jYWxpdHkx4AEA!16s%2Fm%2F02rv7ds?entry=ttu',
    'https://www.google.com/maps/place/Abu+Qash/@31.9445793,35.1435272,14z/data=!3m1!4b1!4m10!1m2!2m1!1z15DXkdeVINen16kgICBJc3JhZWw!3m6!1s0x151d2af7fb5be5b9:0x8445b09c61c69d0d!8m2!3d31.9434978!4d35.1791208!15sChTXkNeR15Ug16fXqSAgIElzcmFlbJIBFGFkbWluaXN0cmF0aXZlX2FyZWEz4AEA!16s%2Fg%2F11rgvxtxzz?entry=ttu',
    'https://www.google.com/maps/place/%D8%A3%D8%A8%D9%88+%D8%B4%D8%AE%D9%8A%D8%AF%D9%85%E2%80%AD/@31.8942517,34.8144167,9.54z/data=!4m6!3m5!1s0x151d2bb52d24932d:0x1efaead239874f03!8m2!3d31.966531!4d35.170814!16s%2Fg%2F120jxc2k?entry=ttu',
    'https://www.google.com/maps/place/Abu+Talul,+Israel/@31.1430155,34.9087933,15z/data=!4m6!3m5!1s0x150243aeed5cbef3:0xf2e6ed964ac118da!8m2!3d31.142612!4d34.913514!16s%2Fm%2F0w7m49w?entry=ttu',
    'https://www.google.com/maps/place/Avnei+Eitan/@32.823254,35.7663461,15z/data=!3m1!4b1!4m6!3m5!1s0x151c0561e8f4c363:0xd533e954f87e5b5c!8m2!3d32.824808!4d35.765777!16zL20vMGJyZ3hm?entry=ttu',
    'https://www.google.com/maps/place/Avnei+Hefetz/@32.2865635,35.075054,16z/data=!3m1!4b1!4m6!3m5!1s0x151d185b0097bf51:0xec8c3dadbb49832e!8m2!3d32.284628!4d35.074551!16s%2Fm%2F02vr2pw?entry=ttu',
    'https://www.google.com/maps/place/Avnat/@31.6786667,35.4308975,15.57z/data=!4m10!1m2!2m1!1z15DXkdeg16ogICBJc3JhZWw!3m6!1s0x150330899e5e179f:0x57445e639a31c4ce!8m2!3d31.679039!4d35.436945!15sChHXkNeR16DXqiAgIElzcmFlbJIBCGxvY2FsaXR54AEA!16s%2Fm%2F03m9h_x?entry=ttu',
    'https://www.google.com/maps/place/Adora/@31.5520339,35.0186455,16z/data=!3m1!4b1!4m6!3m5!1s0x1502ef49efd25367:0xae90167f3a018f9d!8m2!3d31.552219!4d35.017634!16zL20vMGNsYmJw?entry=ttu',
    'https://www.google.com/maps/place/Ad-Deirat/@31.4454702,35.1649413,16z/data=!3m1!4b1!4m10!1m2!2m1!1z15Ag15PXmdeo15DXqiAgIElzcmFlbA!3m6!1s0x1502e33aac576f23:0x17efe9f2588526fd!8m2!3d31.4436598!4d35.1642817!15sChbXkCDXk9eZ16jXkNeqICAgSXNyYWVskgEIbG9jYWxpdHngAQA!16s%2Fg%2F1tfd95zk?entry=ttu',
    'https://www.google.com/maps/place/%D8%A3%D8%AF%D9%88%D9%84%D8%A9%E2%80%AD/@32.1533726,35.275529,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdef5fcb8ab9b:0x8ce65fe479424f23!8m2!3d32.153373!4d35.275529!16s%2Fm%2F04cspxv?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%95%D7%93%D7%9D%E2%80%AD/@33.194342,35.7490975,16z/data=!3m1!4b1!4m6!3m5!1s0x151eb09b3bc4f96d:0x616d5804a0a5bb05!8m2!3d33.1933716!4d35.7495651!16zL20vMDY5aGRy?entry=ttu',
    'https://www.google.com/maps/place/Ohalo+Manor/@32.7137548,35.5488012,13.52z/data=!4m10!1m2!2m1!1z15DXldeU15zXlQ!3m6!1s0x151c6aa3bec1a023:0xc49cadcd5ef56757!8m2!3d32.714436!4d35.572861!15sCgrXkNeV15TXnNeVkgEIYnVzX3N0b3DgAQA!16s%2Fg%2F12llwv21j?entry=ttu',
    'https://www.google.com/maps/place/Umm+Batin,+Israel/@31.2777331,34.8826857,15z/data=!3m1!4b1!4m6!3m5!1s0x15025e0ad6c21201:0x92b5efa375cd7afb!8m2!3d31.2767437!4d34.8829215!16s%2Fm%2F03mcstx?entry=ttu',
    'https://www.google.com/maps/place/%D8%A3%D9%85+%D8%B5%D9%81%D8%A7%E2%80%AD/@32.0088685,35.165696,15z/data=!3m1!4b1!4m10!1m2!2m1!1z15DXldedINeh16TXkCAgIElzcmFlbA!3m6!1s0x151d2974a1794c39:0x984895304cef4141!8m2!3d32.008869!4d35.165696!15sChbXkNeV150g16HXpNeQICAgSXNyYWVskgEIbG9jYWxpdHngAQA!16s%2Fg%2F11f6y3q5nj?entry=ttu',
    'https://www.google.com/maps/place/Oron,+Kiryat+Gat,+Israel/@31.602053,34.761688,17z/data=!3m1!4b1!4m6!3m5!1s0x1502917735eecec9:0x3f3df2826454f803!8m2!3d31.602053!4d34.761688!16s%2Fg%2F1ymv6j7dp?entry=ttu',
    'https://www.google.com/maps/place/Ortal/@33.087476,35.759199,16z/data=!3m1!4b1!4m6!3m5!1s0x151eaf6bb93a3aa5:0x80b01115e20638c3!8m2!3d33.086236!4d35.758661!16s%2Fm%2F04634b4?entry=ttu',
    'https://www.google.com/maps/place/Oranim,+Yehiam,+Israel/@32.9976495,35.222,15z/data=!3m1!4b1!4m10!1m2!2m1!1z15DXldeo16DXmdedICAgSXNyYWVs!3m6!1s0x151c32a936db38a1:0xf84d0d74ef81b524!8m2!3d32.99765!4d35.222!15sChXXkNeV16jXoNeZ150gICBJc3JhZWySAQxuZWlnaGJvcmhvb2TgAQA!16s%2Fg%2F1ymvkn24p?entry=ttu',
    'https://www.google.com/maps/place/Military+industries+area/@32.542374,35.4856465,14z/data=!4m10!1m2!2m1!1z16rXotep15nXldeqINem15HXkNeZ150!3m6!1s0x151c5c43cbd3824b:0x7f5f1230ddebb35b!8m2!3d32.542374!4d35.503156!15sChfXqtei16nXmdeV16og16bXkdeQ15nXneABAA!16s%2Fg%2F1vbbcxlb?entry=ttu',
    'https://www.google.com/maps/place/Industrial+area+Akhozab+Miloa+T./@33.068374,35.110211,17z/data=!3m1!4b1!4m6!3m5!1s0x151dd199f7831d1d:0x96da2c83e21a85e9!8m2!3d33.068374!4d35.110211!16s%2Fg%2F1td2q0lf?entry=ttu',
    'https://www.google.com/maps/place/Industrial+area/@32.6432734,34.6118639,9.05z/data=!4m10!1m2!2m1!1zIteQ15bXldeoINeq16LXqdeZ15nXlCDXkNec15XXnyDXlNeq15HXldeoICAgSXNyYWVsIg!3m6!1s0x151c45cef88394a1:0xdecbcdf04e6ac4ad!8m2!3d32.686989!4d35.426625!15sCjQi15DXlteV16gg16rXotep15nXmdeUINeQ15zXldefINeU16rXkdeV16ggICBJc3JhZWwiWjIiMNeQ15bXldeoINeq16LXqdeZ15nXlCDXkNec15XXnyDXlNeq15HXldeoIGlzcmFlbJoBI0NoWkRTVWhOTUc5blMwVkpRMEZuU1VOcE9UWnVOa2xSRUFF4AEA!16s%2Fg%2F1vhjwbzc?entry=ttu',
    'https://www.google.com/maps/place/Afek+industrial+park/@32.1060921,34.9687576,17z/data=!3m1!4b1!4m6!3m5!1s0x151d30c142e66321:0xe616e824b91652eb!8m2!3d32.1060921!4d34.9687576!16s%2Fg%2F120qddnm?entry=ttu',
    "https://www.google.com/maps/place/Industrial+area+Be'er+Tovia/@31.7234649,34.7513313,17z/data=!3m1!4b1!4m6!3m5!1s0x1502bdf34cd6626b:0xdb6213a9a1d0b631!8m2!3d31.7234649!4d34.7513313!16s%2Fg%2F1tf18361?entry=ttu",
    'https://www.google.com/maps/place/Bne+Yehuda+Industrial+Zone/@32.8310581,35.6450697,12.83z/data=!4m10!1m2!2m1!1zIteQ15bXldeoINeq16LXqdeZ15nXlCDXkdeg15kg15nXlNeV15PXlCAgIElzcmFlbA!3m6!1s0x151c104c6e1ccf6f:0xb304a1df4ca7fe4f!8m2!3d32.80658!4d35.712292!15sCjEi15DXlteV16gg16rXotep15nXmdeUINeR16DXmSDXmdeU15XXk9eUICAgSXNyYWVskgEIYnVzX3N0b3DgAQA!16s%2Fg%2F11h037c_h6?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%96%D7%95%D7%A8+%D7%AA%D7%A2%D7%A9%D7%99%D7%99%D7%94+%D7%91%D7%A8%D7%95%D7%A9+%D7%9E%D7%A2%D7%A8%D7%91%E2%80%AD/@31.733143,34.959383,17z/data=!3m1!4b1!4m6!3m5!1s0x1502c3f9ae9dde95:0x260ff9352293b0d1!8m2!3d31.733143!4d34.959383!16s%2Fg%2F1tfkyk9q?entry=ttu',
    'https://www.google.com/maps/place/Dimona,+Israel/@31.018723,35.09576,12z/data=!3m1!4b1!4m10!1m2!2m1!1zIteQ15bXldeoINeq16LXqdeZ15nXlCDXk9eZ157Xldeg15QgICBJc3JhZWwi!3m6!1s0x150246012ca6fe53:0x96ba70a020a712d1!8m2!3d31.069419!4d35.033363!15sCi0i15DXlteV16gg16rXotep15nXmdeUINeT15nXnteV16DXlCAgIElzcmFlbCKSAQhsb2NhbGl0eeABAA!16zL20vMDJodHk?entry=ttu',
    'https://www.google.com/maps/place/South+Industry+Zone/@31.6308144,34.5519771,14.77z/data=!4m10!1m2!2m1!1z15DXlteV16gg16rXotep15nXmdeUINeU15PXqNeV157XmSDXkNep16fXnNeV158gICBJc3JhZWwi!3m6!1s0x15029cd8cbf0f629:0x371f5596bf632c62!8m2!3d31.636089!4d34.554535!15sCjnXkNeW15XXqCDXqtei16nXmdeZ15Qg15TXk9eo15XXnteZINeQ16nXp9ec15XXnyAgIElzcmFlbCLgAQA!16s%2Fg%2F1tlv47jd?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%96%D7%95%D7%A8+%D7%AA%D7%A2%D7%A9%D7%99%D7%99%D7%94+%D7%94%D7%A8%D7%98%D7%95%D7%91+%D7%90%E2%80%AD/@31.769382,34.9938233,16z/data=!4m10!1m2!2m1!1zIteQ15bXldeoINeq16LXqdeZ15nXlCDXlNeoINeY15XXkSAtINem16jXoteUICAgSXNyYWVsIg!3m6!1s0x1502c506373e6f99:0xed80ee03d447ba58!8m2!3d31.772753!4d34.99809!15sCjci15DXlteV16gg16rXotep15nXmdeUINeU16gg15jXldeRIC0g16bXqNei15QgICBJc3JhZWwikgEVZWxlY3RyaWNhbF9zdWJzdGF0aW9u4AEA!16s%2Fg%2F1hc1z58p1?entry=ttu',
    'https://www.google.com/maps/place/Modiin+Region+Industrial+Zone/@32.0082401,34.960668,14z/data=!4m10!1m2!2m1!1z15DXlteV16gg16rXotep15nXmdeUINeX15HXnCDXnteV15PXmdei15nXnyAgIElzcmFlbA!3m6!1s0x151d337af42b406b:0xb2f6dd327eb0242a!8m2!3d32.007819!4d34.9606546!15sCjTXkNeW15XXqCDXqtei16nXmdeZ15Qg15fXkdecINee15XXk9eZ16LXmdefICAgSXNyYWVs4AEA!16s%2Fg%2F11dztgwfhr?entry=ttu',
    'https://www.google.com/maps/place/Industrial+Zone%2FHaBanim+Road/@33.0438942,35.4422707,10.28z/data=!4m10!1m2!2m1!1zIteQ15bXldeoINeq16LXqdeZ15nXlCDXl9em15XXqCDXlNeS15zXmdec15nXqiAgIElzcmFlbA!3m6!1s0x151c21f1b4563bf9:0x52c1d644452d1fd6!8m2!3d32.982323!4d35.551868!15sCjci15DXlteV16gg16rXotep15nXmdeUINeX16bXldeoINeU15LXnNeZ15zXmdeqICAgSXNyYWVskgEIYnVzX3N0b3DgAQA!16s%2Fg%2F1q5gkdj0v?entry=ttu',
    'https://www.google.com/maps/place/Jerusal%C3%A9m,+Israel/@31.7963186,35.175359,12z/data=!3m1!4b1!4m10!1m2!2m1!1z15nXqNeV16nXnNeZ150gLSDXnteo15vXliAgIElzcmFlbA!3m6!1s0x1502d7d634c1fc4b:0xd96f623e456ee1cb!8m2!3d31.768319!4d35.21371!15sCiLXmdeo15XXqdec15nXnSAtINee16jXm9eWICAgSXNyYWVsWiAiHteZ16jXldep15zXmdedINee16jXm9eWIGlzcmFlbJIBCGxvY2FsaXR5mgEjQ2haRFNVaE5NRzluUzBWSlEwRm5TVVJWYW5RdFRGaDNFQUXgAQA!16zL20vMDQzMF8?entry=ttu',
    'https://www.google.com/maps/place/Jerusal%C3%A9m,+Israel/@31.7963186,35.175359,12z/data=!3m1!4b1!4m6!3m5!1s0x1502d7d634c1fc4b:0xd96f623e456ee1cb!8m2!3d31.768319!4d35.21371!16zL20vMDQzMF8?entry=ttu',
    'https://www.google.com/maps/place/Jerusal%C3%A9m,+Israel/@31.7963186,35.175359,12z/data=!4m6!3m5!1s0x1502d7d634c1fc4b:0xd96f623e456ee1cb!8m2!3d31.768319!4d35.21371!16zL20vMDQzMF8?entry=ttu',
    'https://www.google.com/maps/place/Jericho/@31.8594722,35.4645078,14z/data=!3m1!4b1!4m10!1m2!2m1!1z15nXqNeZ15fXlSAgIElzcmFlbA!3m6!1s0x151ccc61e6ff8567:0x811428a4685668bb!8m2!3d31.8611058!4d35.4617583!15sChPXmdeo15nXl9eVICAgSXNyYWVskgEIbG9jYWxpdHngAQA!16zL20vMDQzOHQ?entry=ttu',
    'https://www.google.com/maps/place/Merkaz+Omen,+Israel/@32.5634535,35.24248,16z/data=!3m1!4b1!4m6!3m5!1s0x151c54e164af78c5:0x9e24f197a373ee6e!8m2!3d32.564279!4d35.241941!16s%2Fm%2F03gy3y3?entry=ttu',
    'https://www.google.com/maps/place/Merkaz+Yael,+Israel/@32.55137,35.3074255,17z/data=!3m1!4b1!4m6!3m5!1s0x151c569cedc4ff63:0x563071e545de50d0!8m2!3d32.552064!4d35.306548!16s%2Fm%2F03h3m29?entry=ttu',
    'https://www.google.com/maps/place/Yatma/@32.1077465,35.268147,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd92f95c9e09b:0xbd5fbf3b55c1f9eb!8m2!3d32.107747!4d35.268147!16s%2Fm%2F04crsn3?entry=ttu',
    'https://www.google.com/maps/place/Kobar/@31.9860229,35.1385262,15z/data=!3m1!4b1!4m10!1m2!2m1!1z15vXldeR16ggICBJc3JhZWw!3m6!1s0x151d2bf1d89c96d5:0xc54fd8b20361982c!8m2!3d31.9821013!4d35.1436537!15sChHXm9eV15HXqCAgIElzcmFlbJIBCGxvY2FsaXR54AEA!16s%2Fm%2F047sslh?entry=ttu',
    'https://www.google.com/maps/place/%D7%9B%D7%95%D7%9B%D7%91+%D7%94%D7%A9%D7%97%D7%A8%E2%80%AD/@31.9602635,35.349421,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd1a64aa5caab:0x7f5ae629d74af4e3!8m2!3d31.960264!4d35.349421!16zL20vMDczZHJ5?entry=ttu',
    'https://www.google.com/maps/place/Vered+Hagalil+Holiday+Farm/@32.9053416,35.5470553,16.05z/data=!4m17!1m5!2m4!1z15vXldeo15bXmdedINeV16jXkyDXlNeS15zXmdecICAg!5m2!5m1!1s2024-02-10!3m10!1s0x151c22b0c7cb2c7d:0x4f5084cb03ceb302!5m3!1s2024-02-10!4m1!1i2!8m2!3d32.90416!4d35.550785!15sCh7Xm9eV16jXlteZ150g15XXqNeTINeU15LXnNeZ15ySAQVob3RlbOABAA!16s%2Fm%2F0fqrq3n?entry=ttu',
    'https://www.google.com/maps/place/Kifl+Haris/@32.1168895,35.156964,15z/data=!3m1!4b1!4m6!3m5!1s0x151d26f6d5ca8881:0x4fd1a83d66befb4a!8m2!3d32.11689!4d35.156964!16s%2Fm%2F02qsh3z?entry=ttu',
    'https://www.google.com/maps/place/Damon+prison/@32.7332388,35.0233539,17z/data=!3m1!4b1!4m6!3m5!1s0x151da543c4f955bf:0xf44dd12c6609d5c2!8m2!3d32.7332388!4d35.0233539!16s%2Fg%2F1hb_gg8zr?entry=ttu',
    'https://www.google.com/maps/place/Kannot+Youth+Village/@31.4322689,34.4427413,8.96z/data=!4m10!1m2!2m1!1z15vXoNeV16ogICBJc3JhZWw!3m6!1s0x1502bbf4f25026cb:0x1d081b24be241e63!8m2!3d31.803482!4d34.754189!15sChHXm9eg15XXqiAgIElzcmFlbJIBCGJ1c19zdG9w4AEA!16s%2Fg%2F11r8xh5m7z?entry=ttu',
    'https://www.google.com/maps/place/%D7%9B%D7%A0%D7%A3%E2%80%AD/@32.8702395,35.6973271,16z/data=!3m1!4b1!4m6!3m5!1s0x151c1af5911cdf1f:0xbfe30a4536e1b69f!8m2!3d32.870412!4d35.697897!16s%2Fm%2F0463kqb?entry=ttu',
    'https://www.google.com/maps/place/Kuseife,+Israel/@31.2396324,35.0905246,13z/data=!3m1!4b1!4m10!1m2!2m1!1z15vXodeZ15nXpNeUINeV15TXpNeW15XXqNeUICAgSXNyYWVs!3m6!1s0x150256fd70d1bef9:0xeefa5ef1c34cb30c!8m2!3d31.249246!4d35.083681!15sCiTXm9eh15nXmdek15Qg15XXlNek15bXldeo15QgICBJc3JhZWySAQhsb2NhbGl0eeABAA!16zL20vMGJ5cXF4?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Adumim/@31.8224025,35.325381,14z/data=!3m1!4b1!4m6!3m5!1s0x15032ce53f201ea7:0x84492e68eb8c9560!8m2!3d31.827216!4d35.337189!16s%2Fm%2F04glqfs?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Blum,+Israel/@33.172328,35.6066025,15z/data=!3m1!4b1!4m6!3m5!1s0x151ea2e03d2df6ef:0x1c3c0d2877de18bd!8m2!3d33.172527!4d35.607938!16zL20vMDY0bW53?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Giladi,+Israel/@33.2421781,35.5754526,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebd8287014bf9:0x7bf3cc5e991363c7!8m2!3d33.242078!4d35.574142!16zL20vMGcwa2hq?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Ha-Oranim/@31.9196121,35.038281,16z/data=!3m1!4b1!4m6!3m5!1s0x1502cd7d3d791e87:0xa1a02fa13420b642!8m2!3d31.919928!4d35.036911!16s%2Fm%2F04gh58z?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Ha-Oranim/@31.9196121,35.038281,16z/data=!4m6!3m5!1s0x1502cd7d3d791e87:0xa1a02fa13420b642!8m2!3d31.919928!4d35.036911!16s%2Fm%2F04gh58z?entry=ttu',
    'https://www.google.com/maps/place/Yemin+Orde,+Israel/@32.7017975,34.9878,15z/data=!3m1!4b1!4m6!3m5!1s0x151da449dfc3efe5:0x9091d965eb94cc72!8m2!3d32.701798!4d34.9878!16s%2Fg%2F120qdqk8?entry=ttu',
    'https://www.google.com/maps/place/Kfar+HaNassi,+Israel/@32.9764105,35.6008935,16z/data=!3m1!4b1!4m6!3m5!1s0x151c1f224cb39903:0xe747698bab02250a!8m2!3d32.97466!4d35.602308!16s%2Fm%2F02rxv2j?entry=ttu',
    "https://www.google.com/maps/place/Re'em+Junction/@31.7695387,34.7340673,12.24z/data=!4m10!1m2!2m1!1z15vXpNeoINeU16jXmScn16Mg15XXpteV157XqiDXqNeQ150gICBJc3JhZWw!3m6!1s0x1502be9aa1333d39:0x4a6dc81cbe141682!8m2!3d31.761328!4d34.784897!15sCizXm9ek16gg15TXqNeZJyfXoyDXldem15XXnteqINeo15DXnSAgIElzcmFlbJIBCGJ1c19zdG9w4AEA!16s%2Fg%2F12hrbc8_g?entry=ttu",
    'https://www.google.com/maps/place/Kfar+Haruv/@32.7629764,35.6647761,16z/data=!3m1!4b1!4m6!3m5!1s0x151c13f281e650b1:0xace0cd6a9fcbe8aa!8m2!3d32.762357!4d35.66418!16s%2Fm%2F03qcm1x?entry=ttu',
    'https://www.google.com/maps/place/Yuval,+Israel/@33.2432995,35.5989725,14z/data=!3m1!4b1!4m6!3m5!1s0x151ebdba5ef6464b:0x7d6b769db3f22234!8m2!3d33.246577!4d35.597063!16s%2Fm%2F03wfgw_?entry=ttu',
    'https://www.google.com/maps/place/%D9%83%D9%81%D8%B1+%D9%85%D8%A7%D9%84%D9%83%E2%80%AD/@31.9902015,35.310221,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd0d117eff3dd:0xaf01fcecd8e0bac3!8m2!3d31.990202!4d35.310221!16s%2Fm%2F04ctc7_?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Maimon,+Israel/@31.4328755,34.535252,16z/data=!3m1!4b1!4m6!3m5!1s0x15027fa41a35a739:0xf00ec413b088cd7f!8m2!3d31.431401!4d34.53718!16s%2Fm%2F04ghl6s?entry=ttu',
    'https://www.google.com/maps/place/Jordan+River+Village/@32.768294,35.435627,17z/data=!3m1!4b1!4m6!3m5!1s0x151c479d550db1b5:0xde4a5fda3b356ed4!8m2!3d32.768294!4d35.435627!16s%2Fm%2F0ynts5w?entry=ttu',
    'https://www.google.com/maps/place/Adanim,+Israel/@32.1390985,34.906199,16z/data=!3m1!4b1!4m10!1m2!2m1!1z15vXpNeoINeg15XXoteoINei15PXoNeZ150g!3m6!1s0x151d3777a7d44341:0xb69473a15ecbc653!8m2!3d32.138798!4d34.909148!15sChrXm9ek16gg16DXldei16gg16LXk9eg15nXnZIBCGxvY2FsaXR54AEA!16s%2Fm%2F047n_9l?entry=ttu',
    'https://www.google.com/maps/place/Cafarnaum,+Israel/@32.8803295,35.573307,14z/data=!3m1!4b1!4m6!3m5!1s0x151c17fb0f89d5e9:0xa91847e6f9c7b1dc!8m2!3d32.88033!4d35.573307!16zL20vMDF5cmR4?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Szold,+Israel/@33.1950795,35.6574289,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebb60c5e11a71:0x4f059cabaee8b746!8m2!3d33.196035!4d35.657993!16zL20vMGc2ZHBq?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Ezion/@31.64929,35.115063,16z/data=!3m1!4b1!4m6!3m5!1s0x1502dc328631ebf5:0xb5f7ff55a2a6a22f!8m2!3d31.649486!4d35.115234!16zL20vMDY0djg2?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Tapuach/@32.1176191,35.2496959,16z/data=!3m1!4b1!4m6!3m5!1s0x151cd8d22567e71d:0x2bb3c4a7029e3b4c!8m2!3d32.117973!4d35.249829!16zL20vMDNiNTkz?entry=ttu',
    'https://www.google.com/maps/place/Kfar+Tikva/@32.701338,35.115035,17z/data=!3m1!4b1!4m6!3m5!1s0x151dae12ba81f987:0x53474dda876ca208!8m2!3d32.701338!4d35.115035!16s%2Fg%2F122dlh0q?entry=ttu',
    'https://www.google.com/maps/place/Khursa/@31.4409605,34.99744,14z/data=!3m1!4b1!4m6!3m5!1s0x1502f0fe58115453:0xe42dff6e8b6b919c!8m2!3d31.440961!4d34.99744!16s%2Fm%2F0n_833_?entry=ttu',
    'https://www.google.com/maps/place/%D7%9B%D7%A8%D7%99+%D7%93%D7%A9%D7%90,+Israel%E2%80%AD/@32.9267229,35.5516035,12.79z/data=!4m6!3m5!1s0x151c1887d58983c1:0xb363721fd97f3cf!8m2!3d32.928713!4d35.576718!16s%2Fg%2F1v70ywqj?entry=ttu',
    'https://www.google.com/maps/place/Karkom,+Israel/@32.930156,35.6077089,17z/data=!3m1!4b1!4m6!3m5!1s0x151c18e31bb101bf:0x2551d39b47524b1d!8m2!3d32.928271!4d35.606331!16s%2Fm%2F03wf1vn?entry=ttu',
    "https://www.google.com/maps/place/Yeshivat+Kerem+B'Yavneh/@31.8199341,34.7226127,17z/data=!3m1!4b1!4m6!3m5!1s0x1502bb7a3624c029:0x47ae4652d7177138!8m2!3d31.8199341!4d34.7226127!16s%2Fm%2F02pp89r?entry=ttu",
    'https://www.google.com/maps/place/Kerem+Ben+Shemen,+Israel/@31.9586779,34.9339324,18z/data=!3m1!4b1!4m6!3m5!1s0x1502cb6bfd81edc7:0xefb78f58ddd6c469!8m2!3d31.958896!4d34.934104!16s%2Fm%2F04cx0hf?entry=ttu',
    "https://www.google.com/maps/place/Yeshivat+Kerem+B'Yavneh/@31.8199341,34.7226127,17z/data=!4m6!3m5!1s0x1502bb7a3624c029:0x47ae4652d7177138!8m2!3d31.8199341!4d34.7226127!16s%2Fm%2F02pp89r?entry=ttu",
    'https://www.google.com/maps/place/Karmei+Zur/@31.609445,35.101301,16z/data=!3m1!4b1!4m6!3m5!1s0x1502e761a5d26feb:0xfa4eeff4fb9e418d!8m2!3d31.609468!4d35.101273!16s%2Fm%2F02ps_8y?entry=ttu',
    'https://www.google.com/maps/place/Lehavot+HaBashan,+Israel/@33.140549,35.64826,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea378f5fed5db:0x4180a0517b2920ad!8m2!3d33.1412178!4d35.6478714!16s%2Fm%2F04g15yn?entry=ttu',
    'https://www.google.com/maps/place/%D8%A7%D9%84%D9%84%D8%A8%D9%86+%D8%A7%D9%84%D8%B4%D8%B1%D9%82%D9%8A%D8%A9%E2%80%AD/@32.0707445,35.242027,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd843f9907e75:0xff6ab2c4bc1018a!8m2!3d32.070745!4d35.242027!16s%2Fm%2F03ql9y4?entry=ttu',
    'https://www.google.com/maps/place/Latrun/@31.8327215,34.97981,14z/data=!3m1!4b1!4m10!1m2!2m1!1zItec15jXqNeV158gICBJc3JhZWwiICAgICAgICAgICAgICAgIA!3m6!1s0x1502cf13bc48ad49:0xee347ac8045800b!8m2!3d31.832722!4d34.97981!15sChUi15zXmNeo15XXnyAgIElzcmFlbCKSAQhsb2NhbGl0eeABAA!16s%2Fg%2F1ywqfhyn8?entry=ttu',
    'https://www.google.com/maps/place/%D7%9C%D7%A4%D7%99%D7%93%E2%80%AD/@31.9178914,35.0323525,16z/data=!3m1!4b1!4m6!3m5!1s0x1502cd795f227971:0x83d44ded8da44d2a!8m2!3d31.917522!4d35.032191!16s%2Fm%2F04cwg94?entry=ttu',
    'https://www.google.com/maps/place/Laqiya,+Israel/@31.324195,34.8699774,14z/data=!3m1!4b1!4m10!1m2!2m1!1zItec16fXmdeUINeV15TXpNeW15XXqNeUICAgSXNyYWVsIiAgICAgICAgICAgIA!3m6!1s0x15025fd4cbf7a083:0xe2e3b39e8c2cb837!8m2!3d31.324884!4d34.866219!15sCiIi15zXp9eZ15Qg15XXlNek15bXldeo15QgICBJc3JhZWwikgEIbG9jYWxpdHngAQA!16zL20vMGJ5cm1y?entry=ttu',
    'https://www.google.com/maps/place/Meir+Shfeya+Youth+Village/@32.5912213,34.9710703,17z/data=!3m1!4b1!4m6!3m5!1s0x151d09ea233115b3:0xfe1d3848cae3ca7e!8m2!3d32.5912213!4d34.9710703!16s%2Fg%2F12vqq9mr9?entry=ttu',
    'https://www.google.com/maps/place/Mevo+Dotan/@32.4201215,35.1759891,15z/data=!3m1!4b1!4m6!3m5!1s0x151d02fef4c7ac6b:0x37d74a464e00c1d3!8m2!3d32.420772!4d35.173869!16zL20vMGZjNG45?entry=ttu',
    "https://www.google.com/maps/place/Mevo'ot+Yericho/@31.9077979,35.4174179,16z/data=!3m1!4b1!4m6!3m5!1s0x151ccddf29210971:0xef0d623203cbd905!8m2!3d31.9074488!4d35.4155538!16zL20vMGM1cW5f?entry=ttu",
    'https://www.google.com/maps/place/Mevo+Hama/@32.7374839,35.655483,16z/data=!3m1!4b1!4m6!3m5!1s0x151c137dbae8aeeb:0x561d2984b2d98dec!8m2!3d32.736701!4d35.655207!16s%2Fm%2F0411crz?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%92%D7%93%D7%9C%D7%99%D7%9D%E2%80%AD/@32.0904831,35.3419725,17z/data=!3m1!4b1!4m6!3m5!1s0x151cdb8118583733:0x33fa77c688884079!8m2!3d32.089866!4d35.342633!16zL20vMGJybXI4?entry=ttu',
    'https://www.google.com/maps/place/Migdal+Oz/@31.6399354,35.1428584,16z/data=!3m1!4b1!4m6!3m5!1s0x1502de897f660f75:0x8664bb3fb0421152!8m2!3d31.640932!4d35.143139!16s%2Fm%2F02rvztm?entry=ttu',
    'https://www.google.com/maps/place/Majdal+Shams/@33.2690956,35.772122,14z/data=!3m1!4b1!4m6!3m5!1s0x151eb7ba0aca30d1:0xd6ccba06aeebb48d!8m2!3d33.2690961!4d35.772122!16zL20vMDlqZ3Fj?entry=ttu',
    'https://www.google.com/maps/place/%D9%85%D8%AF%D9%85%D8%A7%E2%80%AD/@32.0262999,35.0337983,9.99z/data=!4m10!1m2!2m1!1z157Xk9ee15AgICAgICAgICAgICAgICAgICAgICAgICAg!3m6!1s0x151cdf944f767fc3:0x5fb3d9646ab51d77!8m2!3d32.183929!4d35.233453!15sCgjXnteT157XkJIBCGxvY2FsaXR54AEA!16s%2Fg%2F1235kdmb?entry=ttu',
    "https://www.google.com/maps/place/Ishpro+Center+Modi'in/@31.8898011,34.9628105,17z/data=!3m1!4b1!4m6!3m5!1s0x1502ceaf49d9ca91:0x7715d4d338e3ec48!8m2!3d31.8898011!4d34.9628105!16s%2Fg%2F1yfjs581m?entry=ttu",
    "https://www.google.com/maps/place/Modi'in+Ilit/@31.9329695,35.0473064,14z/data=!3m1!4b1!4m6!3m5!1s0x1502d2a10ff5a55d:0x96d92731eec10bb5!8m2!3d31.932477!4d35.042265!16zL20vMDRjajM5?entry=ttu",
    'https://www.google.com/maps/place/%D8%A7%D9%84%D9%85%D8%BA%D9%8A%D8%B1%E2%80%AD/@32.4217005,35.386433,14z/data=!3m1!4b1!4m10!1m2!2m1!1z157Xldei4oCZ15nXmdeoICAgSXNyYWVs!3m6!1s0x151cf9d93f520b19:0xd9e17dd6e9ea1b99!8m2!3d32.421701!4d35.386433!15sChjXnteV16LigJnXmdeZ16ggICBJc3JhZWySAQhsb2NhbGl0eeABAA!16s%2Fm%2F04ctmst?entry=ttu',
    'https://www.google.com/maps/place/Al+Aroub/@31.6226015,35.138126,14z/data=!3m1!4b1!4m6!3m5!1s0x1502de6d81be5efd:0xdc07de4baed220e8!8m2!3d31.622602!4d35.138126!16s%2Fm%2F03m9j84?entry=ttu',
    'https://www.google.com/maps/place/%D9%85%D8%B2%D8%A7%D8%B1%D8%B9+%D8%A7%D9%84%D9%86%D8%A8%D9%88%D8%A7%D9%86%D9%8A%E2%80%AD/@32.0486075,35.167176,14z/data=!3m1!4b1!4m6!3m5!1s0x151d28f4c5f2d71f:0x28fc2d5f12830761!8m2!3d32.048608!4d35.167176!16s%2Fm%2F04cvfd8?entry=ttu',
    'https://www.google.com/maps/place/%D8%A7%D9%84%D9%85%D8%B2%D8%B1%D8%B9%D8%A9+%D8%A7%D9%84%D9%82%D8%A8%D9%84%D9%8A%D8%A9%E2%80%AD/@31.9516005,35.150742,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2b0b1173e3e7:0x2947b59b108689fd!8m2!3d31.951601!4d35.150742!16s%2Fg%2F113qbtkpm?entry=ttu',
    "https://www.google.com/maps/place/Al-Mazra'a+ash-Sharqiya/@32.0032375,35.276132,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd71993cb5f0f:0x1134a9daf0a6a8af!8m2!3d32.003238!4d35.276132!16s%2Fm%2F04ctdxy?entry=ttu",
    'https://www.google.com/maps/place/Mahanayim,+Israel/@32.9885475,35.5705511,16z/data=!3m1!4b1!4m6!3m5!1s0x151c1f60c7734917:0xed66ec9b2a8cc5d3!8m2!3d32.988845!4d35.570375!16s%2Fm%2F04g1rkz?entry=ttu',
    'https://www.google.com/maps/place/Global+anti-terrorism+Ltd./@31.5123126,34.5545017,17z/data=!3m1!4b1!4m6!3m5!1s0x15029c5141685a15:0x8af2f774fc13aa81!8m2!3d31.5123126!4d34.5545017!16s%2Fg%2F1tdnhgsd?entry=ttu',
    'https://www.google.com/maps/place/Metula,+Israel/@33.269263,35.5760915,14z/data=!3m1!4b1!4m6!3m5!1s0x151ebde2e0f90eeb:0xbd066a600f5d390c!8m2!3d33.277232!4d35.578235!16zL20vMDdxdDhs?entry=ttu',
    'https://www.google.com/maps/place/Mata,+Israel/@31.7177145,35.0632326,16z/data=!3m1!4b1!4m6!3m5!1s0x1502db42813dd429:0x50fbd56a4246cbb9!8m2!3d31.716522!4d35.060571!16s%2Fm%2F04gtq6w?entry=ttu',
    'https://www.google.com/maps/place/al-Midya/@31.9360925,35.004574,14z/data=!3m1!4b1!4m6!3m5!1s0x1502cd1a1ee3ca65:0x77be8a1706b4ed0e!8m2!3d31.936093!4d35.004574!16s%2Fm%2F047m9tr?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%99%D7%A6%D7%A8%E2%80%AD/@32.768473,35.736489,16z/data=!3m1!4b1!4m6!3m5!1s0x151c11e39e5c9f75:0x10e2ad098956ae24!8m2!3d32.768641!4d35.736356!16s%2Fm%2F0463rk9?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%9B%D7%95%D7%A8%D7%94%E2%80%AD/@32.165092,35.4236425,16z/data=!3m1!4b1!4m6!3m5!1s0x151ce8378644e413:0xb19a71e56549c713!8m2!3d32.164159!4d35.423623!16s%2Fm%2F02vw8_b?entry=ttu',
    'https://www.google.com/maps/place/%D7%94%D7%9E%D7%9B%D7%9C%D7%9C%D7%94+%D7%94%D7%90%D7%A7%D7%93%D7%9E%D7%99%D7%AA+%D7%A1%D7%A4%D7%99%D7%A8+-+Sapir+College%E2%80%AD/@31.5090643,34.5944391,17z/data=!3m1!4b1!4m6!3m5!1s0x1502814a96e693ed:0xd2f92addc3834b6c!8m2!3d31.5090643!4d34.5944391!16s%2Fm%2F03h5c86?entry=ttu',
    'https://www.google.com/maps/place/Mamshit+National+Park/@31.025668,35.064281,17z/data=!3m1!4b1!4m6!3m5!1s0x150248f3917ba54b:0xac8688d9204ce521!8m2!3d31.025668!4d35.064281!16s%2Fm%2F02pwsp5?entry=ttu',
    'https://www.google.com/maps/place/Rosh+Pina+Airport/@32.9769392,35.5730341,17z/data=!3m1!4b1!4m6!3m5!1s0x151c1f59584e9b1d:0x2c211bb912822c96!8m2!3d32.9769392!4d35.5730341!16zL20vMGNxeXA0?entry=ttu',
    'https://www.google.com/maps/place/Masada,+Israel/@32.682953,35.5979145,17z/data=!3m1!4b1!4m6!3m5!1s0x151c6a581695aa29:0xcbb81729338a8611!8m2!3d32.682305!4d35.597657!16s%2Fm%2F03h0twx?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%A1%D7%A2%D7%93%D7%94%E2%80%AD/@33.2321154,35.7555991,14z/data=!3m1!4b1!4m6!3m5!1s0x151eb745f0c9b167:0xb5fbe24577cf0afc!8m2!3d33.2321159!4d35.7555991!16s%2Fm%2F03m4tdw?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%A2%D7%92%D7%9F%E2%80%AD/@32.7059065,35.6015434,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6adc9012cbc3:0xf291b568ecbc47d3!8m2!3d32.706344!4d35.600575!16s%2Fm%2F04f3s_j?entry=ttu',
    'https://www.google.com/maps/place/Maon+Tsofia/@31.855517,34.7374628,17z/data=!3m1!4b1!4m6!3m5!1s0x1502ba5a636b8503:0xd16d33bca1fa2467!8m2!3d31.855517!4d34.7374628!16s%2Fg%2F11fvhbqjd8?entry=ttu',
    "https://www.google.com/maps/place/Ma'ayan+Baruch,+Israel/@33.240483,35.6073735,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebdad5260a06f:0x6811fa47a0a345b0!8m2!3d33.239349!4d35.609359!16s%2Fm%2F04g2s76?entry=ttu",
    'NA',
    "https://www.google.com/maps/place/Ma'ale+Adummim/@31.786368,35.324426,12z/data=!3m1!4b1!4m6!3m5!1s0x15032ea9712cfb93:0xb69f0f43f662bd18!8m2!3d31.777369!4d35.297955!16zL20vMDF2dGh5?entry=ttu",
    "https://www.google.com/maps/place/Ma'ale+Efraim/@32.0773655,35.4042746,14z/data=!3m1!4b1!4m6!3m5!1s0x151cc4a7a8c9cb71:0xc7db99238c8c2c23!8m2!3d32.071304!4d35.4039!16zL20vMGNtMWI1?entry=ttu",
    "https://www.google.com/maps/place/Ma'ale+Gamla/@32.887397,35.6826449,15z/data=!3m1!4b1!4m6!3m5!1s0x151c1a4d802367c5:0xac34bb192a2b19df!8m2!3d32.888256!4d35.685966!16s%2Fm%2F0464psj?entry=ttu",
    "https://www.google.com/maps/place/Ma'ale+Levona/@32.0547589,35.2407845,16z/data=!3m1!4b1!4m6!3m5!1s0x151cd822e2617799:0xad2984386e2811d!8m2!3d32.054436!4d35.239503!16zL20vMGRqN3N0?entry=ttu",
    'https://www.google.com/maps/place/%D7%9E%D7%A2%D7%9C%D7%94+%D7%9E%D7%9B%D7%9E%D7%A9%E2%80%AD/@31.8791305,35.305902,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd3591b156e01:0xb29cbaa8f12870cc!8m2!3d31.879131!4d35.305902!16zL20vMGNtMGx3?entry=ttu',
    'https://www.google.com/maps/place/Shavit,+Keisarya,+Israel/@32.4883192,34.9130043,17z/data=!3m1!4b1!4m10!1m2!2m1!1zItee16LXnNeUINep15HXmdeYIg!3m6!1s0x151d0da21751084b:0x804274b2845329d8!8m2!3d32.4883192!4d34.9130043!15sChMi157Xotec15Qg16nXkdeZ15gikgEFcm91dGXgAQA!16s%2Fg%2F1ymx12b3y?entry=ttu',
    NA,
    'https://www.google.com/maps/place/Avshalom+factories/@31.4411991,34.7568468,15.48z/data=!4m10!1m2!2m1!1z157XpNei15zXmSDXkNeR16nXnNeV150!3m6!1s0x1502897721ee88ff:0xf08c7e437351e8b2!8m2!3d31.44131!4d34.760557!15sChfXntek16LXnNeZINeQ15HXqdec15XXneABAA!16s%2Fg%2F1tf0svtf?entry=ttu',
    'https://www.google.com/maps/place/Gilboa+Regional+Council,+Israel/@32.5684382,35.3771315,11z/data=!3m1!4b1!4m10!1m2!2m1!1z15LXnNeR15XXoiIgICAgICAg!3m6!1s0x151c59febd48642d:0xb93842ec3630bfd7!8m2!3d32.5656671!4d35.4124355!15sCgvXktec15HXldeiIpIBFGFkbWluaXN0cmF0aXZlX2FyZWE04AEA!16s%2Fm%2F03d8rb4?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%A2%D7%95%D7%9F%E2%80%AD/@31.413704,35.1648785,16z/data=!3m1!4b1!4m6!3m5!1s0x1502fcddb5398b6f:0x9005074ca4862e5c!8m2!3d31.414592!4d35.163893!16s%2Fg%2F11bc58ppnh?entry=ttu',
    'https://www.google.com/maps/place/Shan+factories/@32.496991,35.518722,17z/data=!3m1!4b1!4m6!3m5!1s0x151c5e897c8cb887:0x461c4bbab82defea!8m2!3d32.496991!4d35.518722!16s%2Fg%2F1tj4_yt6?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%A6%D7%A4%D7%94+%D7%A9%D7%9C%D7%9D%E2%80%AD/@31.5672355,35.4021609,16z/data=!3m1!4b1!4m6!3m5!1s0x15033ccf96defe0f:0x408ead733d6bc94f!8m2!3d31.568875!4d35.400367!16s%2Fm%2F03mb0jl?entry=ttu',
    'https://www.google.com/maps/place/%D9%85%D8%B1%D8%AC+%D9%86%D8%B9%D8%AC%D8%A9%E2%80%AD/@32.1850585,35.538986,14z/data=!3m1!4b1!4m6!3m5!1s0x151ceb61666f2f07:0xb558c923805558ca!8m2!3d32.185059!4d35.538986!16s%2Fg%2F11hzzx9j0n?entry=ttu',
    'https://www.google.com/maps/place/%D9%85%D8%B1%D8%AF%D8%A9%E2%80%AD/@32.1142165,35.195851,14z/data=!3m1!4b1!4m6!3m5!1s0x151d276c7cedf22d:0x893f53cf4cbc1eac!8m2!3d32.114217!4d35.195851!16s%2Fm%2F04cv89z?entry=ttu',
    'https://www.google.com/maps/place/Merom+Golan/@33.1330285,35.7763995,16z/data=!3m1!4b1!4m6!3m5!1s0x151eafd6ee25af91:0x33200ae591142796!8m2!3d33.132583!4d35.777071!16s%2Fm%2F0463pqj?entry=ttu',
    NA,
    "https://www.google.com/maps/place/Mevo'ot+HaHermon+Regional+Council,+Israel/@33.0680505,35.5762141,11z/data=!3m1!4b1!4m10!1m2!2m1!1zItee16jXm9eWINeQ15bXldeo15kg157XkdeV15DXldeqINeX16jXnteV158gICBJc3JhZWwiICAgICA!3m6!1s0x151ea1e33e0a6b79:0xe27c12accc921db6!8m2!3d33.0614193!4d35.5548422!15sCjYi157XqNeb15Yg15DXlteV16jXmSDXnteR15XXkNeV16og15fXqNee15XXnyAgIElzcmFlbCKSARRhZG1pbmlzdHJhdGl2ZV9hcmVhNOABAA!16s%2Fm%2F03c709q?entry=ttu",
    'https://www.google.com/maps/place/Gateway+to+the+Negev/@30.829987,33.9683001,8.53z/data=!4m10!1m2!2m1!1z157XqNeb15Yg15TXoNeS15EgICBJc3JhZWw!3m6!1s0x1502661073ae7ec7:0x45d38ebaee7954c4!8m2!3d31.239029!4d34.7866269!15sChrXnteo15vXliDXlNeg15LXkSAgIElzcmFlbFoaIhjXnteo15vXliDXlNeg15LXkSBpc3JhZWySAQ52aXNpdG9yX2NlbnRlcuABAA!16s%2Fg%2F11gnrm0gbw?entry=ttu',
    'https://www.google.com/maps/place/%D7%9E%D7%96%D7%A8%D7%97+%D7%9E%D7%A2%D7%A8%D7%91+-+%D7%94%D7%9E%D7%A8%D7%9B%D7%96+%D7%9C%D7%9C%D7%99%D7%9E%D7%95%D7%93%D7%99+%D7%A8%D7%A4%D7%95%D7%90%D7%94+%D7%A1%D7%99%D7%A0%D7%99%D7%AA+%D7%A7%D7%9C%D7%90%D7%A1%D7%99%D7%AA.+%D7%9C%D7%A8%D7%90%D7%A9%D7%95%D7%A0%D7%94+%D7%91%D7%99%D7%A9%D7%A8%D7%90%D7%9C,+%D7%9E%D7%A1%D7%9C%D7%95%D7%9C+%D7%AA%D7%9C%D7%AA+%D7%A9%D7%A0%D7%AA%D7%99%E2%80%AD/@31.5410416,34.2772958,8.74z/data=!4m6!3m5!1s0x151d47f3aecbad3d:0x5e212192832bd65a!8m2!3d32.1138193!4d34.8221762!16s%2Fg%2F12q4wk50n?entry=ttu',
    'https://www.google.com/maps/place/Caesarea+Sea+Center/@32.492613,34.89315,17z/data=!3m1!4b1!4m10!3m9!1s0x151d0d6ec9742adf:0x2237cb1853a9e22e!5m3!1s2024-02-10!4m1!1i2!8m2!3d32.492613!4d34.89315!16s%2Fg%2F1v_swpc8?entry=ttu',
    'https://www.google.com/maps/place/Bilu+Center/@31.8648919,34.8166232,17z/data=!3m1!4b1!4m6!3m5!1s0x1502b82a6af1010f:0x1904ecd7baf4b323!8m2!3d31.8648919!4d34.8166232!16s%2Fg%2F11bxfyklms?entry=ttu',
    NA,
    'https://www.google.com/maps/place/Mishmar+HaYarden,+Israel/@33.0050745,35.5997525,16z/data=!3m1!4b1!4m6!3m5!1s0x151c1fa5f9873fed:0x2f4d4e448a594d0d!8m2!3d33.003484!4d35.598271!16s%2Fm%2F03wcyp7?entry=ttu',
    'https://www.google.com/maps/place/Bnei+Darom,+Israel/@31.82118,34.6924049,16z/data=!3m1!4b1!4m10!1m2!2m1!1z157XqteX150g15HXoNeZINeT16jXldedICAgSXNyYWVs!3m6!1s0x1502bb5673e828cb:0xf8c2233173a03442!8m2!3d31.82087!4d34.691609!15sCiHXnteq15fXnSDXkdeg15kg15PXqNeV150gICBJc3JhZWySAQhsb2NhbGl0eeABAA!16s%2Fm%2F04ct3rz?entry=ttu',
    'https://www.google.com/maps/search/%D7%A6%D7%95%D7%9E%D7%AA+%D7%A9%D7%95%D7%A7%D7%AA+%E2%80%AD/@31.3071363,34.90098,18z/data=!3m1!4b1?entry=ttu',
    "https://www.google.com/maps/place/Ne'ot+Golan/@32.7862969,35.690951,15z/data=!3m1!4b1!4m6!3m5!1s0x151c11745cbe5e47:0x629348e5c525a70b!8m2!3d32.787026!4d35.693172!16s%2Fm%2F04654f_?entry=ttu",
    'https://www.google.com/maps/place/Neot+Mordechai,+Israel/@33.15942,35.597509,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea2f6e0a09ba5:0x24f26065cbae64ae!8m2!3d33.159785!4d35.595602!16s%2Fm%2F03ct3l5?entry=ttu',
    'https://www.google.com/maps/place/%D7%A0%D7%95%D7%91%E2%80%AD/@32.8325175,35.7833184,15z/data=!3m1!4b1!4m6!3m5!1s0x151c05763d151fdf:0x3ad8ef1f22f11216!8m2!3d32.832852!4d35.783574!16s%2Fm%2F0463x5f?entry=ttu',
    NA,
    'https://www.google.com/maps/place/Neve+Ativ/@33.2626711,35.7420415,16z/data=!3m1!4b1!4m6!3m5!1s0x151eb9d589bbbf91:0x53f72ff17e890546!8m2!3d33.26202!4d35.740574!16s%2Fm%2F026_58s?entry=ttu',
    'https://www.google.com/maps/place/Neve+Herzog/@31.8233,34.687861,17z/data=!3m1!4b1!4m6!3m5!1s0x1502bb54fc851b59:0x7ee0e20483cfd7d3!8m2!3d31.8233!4d34.687861!16s%2Fg%2F122rpfc1?entry=ttu',
    'https://www.google.com/maps/place/Neve+Shalom/@31.8177324,34.9787761,17z/data=!3m1!4b1!4m6!3m5!1s0x1502cf65d53bace7:0x90586cd0088439b4!8m2!3d31.817427!4d34.978335!16zL20vMGhjenI?entry=ttu',
    "https://www.google.com/maps/place/Nu'eima/@31.8825285,35.459793,14z/data=!3m1!4b1!4m6!3m5!1s0x151ccc16918adf09:0xb85f53f1397dac2a!8m2!3d31.882529!4d35.459793!16s%2Fg%2F1thpz1zz?entry=ttu",
    "https://www.google.com/maps/place/Nu'eima/@31.8825285,35.459793,14z/data=!3m1!4b1!4m6!3m5!1s0x151ccc16918adf09:0xb85f53f1397dac2a!8m2!3d31.882529!4d35.459793!16s%2Fg%2F1thpz1zz?entry=ttu",
    NA,
    'https://www.google.com/maps/place/Nokdim/@31.646125,35.2511805,15z/data=!3m1!4b1!4m6!3m5!1s0x150320ccc2197177:0x83512ba69944dbd2!8m2!3d31.645331!4d35.244064!16zL20vMGNuYmd2?entry=ttu',
    'https://www.google.com/maps/place/Nahal+EliSha/@31.8738715,35.477965,14z/data=!3m1!4b1!4m6!3m5!1s0x151ccc0a79c6f761:0xdfbafceeccc85edb!8m2!3d31.873872!4d35.477965!16s%2Fg%2F120sppxg?entry=ttu',
    'https://www.google.com/maps/place/%D7%A0%D7%98%D7%95%D7%A8%E2%80%AD/@32.8535895,35.7523995,16z/data=!3m1!4b1!4m6!3m5!1s0x151c0539f66e893b:0xecc85b2b52015062!8m2!3d32.854068!4d35.752574!16s%2Fm%2F0463d8t?entry=ttu',
    'https://www.google.com/maps/place/Neta,+Israel/@31.4776453,34.9263696,17z/data=!3m1!4b1!4m6!3m5!1s0x1502f2479b039213:0x2bf0c11da12cf7d2!8m2!3d31.477893!4d34.927774!16s%2Fg%2F12q4zjvg0?entry=ttu',
    "https://www.google.com/maps/place/Neta'im,+Israel/@31.9448125,34.7734194,15z/data=!3m1!4b1!4m6!3m5!1s0x1502b6ad20c91d0d:0xe37f49f4a1794107!8m2!3d31.945689!4d34.775145!16zL20vMDY5aDk3?entry=ttu",
    'https://www.google.com/maps/place/Nataf,+Israel/@31.833001,35.0680665,16z/data=!3m1!4b1!4m6!3m5!1s0x1502d1ba8252d425:0xd50e17cb71344f61!8m2!3d31.831276!4d35.06958!16s%2Fm%2F03gxpkt?entry=ttu',
    'https://www.google.com/maps/place/%D9%86%D9%8A%D9%84%D9%8A%E2%80%AD/@31.9626236,35.0480745,16z/data=!3m1!4b1!4m6!3m5!1s0x151d2d5c888c7d1f:0x2ed05915b0b514a3!8m2!3d31.963415!4d35.04727!16s%2Fm%2F02qnq1s?entry=ttu',
    'https://www.google.com/maps/place/Nein,+Israel/@32.63478,35.3526654,15z/data=!3m1!4b1!4m6!3m5!1s0x151c50338298583d:0x4b91e73f6fa2302a!8m2!3d32.630986!4d35.349399!16s%2Fm%2F03c4stq?entry=ttu',
    "https://www.google.com/maps/place/Ni'lin/@31.9468425,35.021461,14z/data=!3m1!4b1!4m6!3m5!1s0x1502cd48ba2e45a9:0x4f602ff86b74ff1a!8m2!3d31.946843!4d35.021461!16s%2Fm%2F03nxpt2?entry=ttu",
    'https://www.google.com/maps/place/%D7%A0%D7%99%D7%A6%D7%A0%D7%94+%D7%9B%D7%A4%D7%A8+%D7%A0%D7%95%D7%A2%D7%A8%E2%80%AD/@30.8864847,34.4215222,17z/data=!3m1!4b1!4m6!3m5!1s0x14fdfe778de637ab:0x299ba3c7a04fa55d!8m2!3d30.8864847!4d34.4215222!16s%2Fg%2F12jsycx1x?entry=ttu',
    NA,
    'https://www.google.com/maps/place/%D7%A0%D7%9E%D7%A8%D7%95%D7%93%E2%80%AD/@33.244941,35.7514855,17z/data=!3m1!4b1!4m6!3m5!1s0x151eb766326a89e3:0x24a69a869e476b65!8m2!3d33.244978!4d35.750842!16s%2Fm%2F026_4hb?entry=ttu',
    NA,
    NA,
    'https://www.google.com/maps/place/Netiv+HaGdud/@31.9886349,35.4463625,15z/data=!3m1!4b1!4m6!3m5!1s0x151cc5851076deb7:0x4d18c6cf6164f555!8m2!3d31.989412!4d35.443281!16s%2Fm%2F04gtppp?entry=ttu',
    'https://www.google.com/maps/place/As-Sawiya/@32.0851645,35.256761,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd8fe775bca2d:0x7db4d85a68ad0275!8m2!3d32.085165!4d35.256761!16s%2Fm%2F04cqy8h?entry=ttu',
    'https://www.google.com/maps/place/Susya/@31.3912906,35.1143225,15z/data=!3m1!4b1!4m10!1m2!2m1!1zIteh15XXodeZ15QgICBJc3JhZWwi!3m6!1s0x1502fb9378356c59:0x6273aa0915b7bef5!8m2!3d31.391051!4d35.110978!15sChUi16HXldeh15nXlCAgIElzcmFlbCKSAQhsb2NhbGl0eeABAA!16s%2Fm%2F04gsh2t?entry=ttu',
    'https://www.google.com/maps/place/%D8%B3%D8%B1%D8%AF%D8%A7%E2%80%AD/@31.9417515,35.20318,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2a6330a88229:0xff0a2826e12b80a4!8m2!3d31.941752!4d35.20318!16s%2Fm%2F04cwyb4?entry=ttu',
    NA,
    'https://www.google.com/maps/place/Silwad/@31.9793895,35.261865,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd68b2b5964c9:0x9d3eb3847195b22!8m2!3d31.97939!4d35.261865!16zL20vMGJ5a2Yx?entry=ttu',
    'https://www.google.com/maps/place/%D8%B3%D9%86%D8%AC%D9%84%E2%80%AD/@31.9793895,35.261865,14z/data=!4m6!3m5!1s0x151cd77eac80503d:0x86390871fdd5cf65!8m2!3d32.033607!4d35.263935!16s%2Fm%2F03m6qz_?entry=ttu',
    'https://www.google.com/maps/place/Cinema+City+Glilot/@32.1463519,34.8040703,17z/data=!3m1!4b1!4m6!3m5!1s0x151d4849fd90c6c7:0x6b3c93051b7ffa1e!8m2!3d32.1463519!4d34.8040703!16s%2Fg%2F120scx1r?entry=ttu',
    'https://www.google.com/maps/place/%D8%B3%D9%84%D9%81%D9%8A%D8%AA%E2%80%AD/@32.0850935,35.180832,14z/data=!3m1!4b1!4m10!1m2!2m1!1zIteh15zXpNeZ16ogICBJc3JhZWw!3m6!1s0x151d27b7e69fecff:0x37b8a0567622a0eb!8m2!3d32.085094!4d35.180832!15sChQi16HXnNek15nXqiAgIElzcmFlbJIBCGxvY2FsaXR54AEA!16s%2Fm%2F0260yq_?entry=ttu',
    'https://www.google.com/maps/place/Mulada,+Israel/@31.2610705,34.975564,14z/data=!3m1!4b1!4m6!3m5!1s0x150259688713a281:0xedcd601e2fe802ef!8m2!3d31.261071!4d34.975564!16s%2Fm%2F046643m?entry=ttu',
    NA,
    'https://www.google.com/maps/place/%D7%A1%D7%A8%D7%9B%D7%99%D7%A1,+Qiryat+Atta,+Israel%E2%80%AD/@32.8061945,35.144518,14z/data=!3m1!4b1!4m6!3m5!1s0x151db414158e8029:0xee5239e80f11c8a2!8m2!3d32.806195!4d35.144518!16s%2Fg%2F1ymwsdmfh?entry=ttu',
    'https://www.google.com/maps/place/Aboud/@32.0165385,35.068807,14z/data=!3m1!4b1!4m10!1m2!2m1!1z16LXkNeR15XXkyAgIElzcmFlbA!3m6!1s0x151d2dd976c7754f:0x233fd5d2efa05f45!8m2!3d32.016539!4d35.068807!15sChPXoteQ15HXldeTICAgSXNyYWVskgEIbG9jYWxpdHngAQA!16s%2Fm%2F02plp5q?entry=ttu',
    'https://www.google.com/maps/place/%D8%B9%D8%B7%D9%88%D9%81%E2%80%AD/@32.2636175,35.437368,14z/data=!3m1!4b1!4m6!3m5!1s0x151cee5456a0fef9:0x51dde5208e45d0ca!8m2!3d32.263618!4d35.437368!16s%2Fg%2F12cq1zy3s?entry=ttu',
    NA,
    'https://www.google.com/maps/place/%D8%B9%D8%AC%D9%88%D9%84%E2%80%AD/@32.0230355,35.180439,14z/data=!3m1!4b1!4m10!1m2!2m1!1z16LXkuKAmdeV15wgICBJc3JhZWw!3m6!1s0x151d29ad0cbb0259:0xaffca3c56ed4f470!8m2!3d32.023036!4d35.180439!15sChTXoteS4oCZ15XXnCAgIElzcmFlbJIBCGxvY2FsaXR54AEA!16s%2Fm%2F04cyfyr?entry=ttu',
    'https://www.google.com/maps/place/Ghajar/@33.269621,35.631424,15z/data=!3m1!4b1!4m6!3m5!1s0x151ebe9a5a0d68f5:0x28ab57f12baf393c!8m2!3d33.272607!4d35.623637!16zL20vMGcyemM2?entry=ttu',
    'https://www.google.com/maps/place/Ad+Halom/@31.7508697,34.5995159,11.95z/data=!4m10!1m2!2m1!1z16LXkyDXlNec15XXnSAgIElzcmFlbA!3m6!1s0x1502a2d94dd7f5d1:0xf59aad509551634!8m2!3d31.7820648!4d34.6694006!15sChbXoteTINeU15zXldedICAgSXNyYWVskgEGYnJpZGdl4AEA!16zL20vMGZ2bXht?entry=ttu',
    'https://www.google.com/maps/place/Al-Auja/@31.9480645,35.471164,14z/data=!3m1!4b1!4m6!3m5!1s0x151ccf36c88999cf:0xc266a52799050ae!8m2!3d31.948065!4d35.471164!16s%2Fm%2F047fhys?entry=ttu',
    NA,
    'https://www.google.com/maps/place/%D7%90%D7%AA%D7%92%D7%A8%D7%99%D7%AA+%D7%A2%D7%95%D7%98%D7%A3+%D7%A2%D7%96%D7%94+%D7%98%D7%A8%D7%99%D7%90%D7%AA%D7%9C%D7%95%D7%9F%E2%80%AD/@31.4894389,34.4095857,11.13z/data=!4m10!1m2!2m1!1z16LXldeY16Mg16LXlteUIA!3m6!1s0x1502810179e1c611:0xb754dbd207f5cd73!8m2!3d31.502206!4d34.561674!15sCg_XoteV15jXoyDXoteW15SSAQ1hdGhsZXRpY19jbHVi4AEA!16s%2Fg%2F11gy1y5tw8?entry=ttu',
    'https://www.google.com/maps/place/%D8%B9%D8%B3%D8%A7%D8%B1%D9%8A%D9%86%E2%80%AD/@32.1249285,35.309237,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdc223be290bd:0xc1550ddc0ff5edea!8m2!3d32.124929!4d35.309237!16s%2Fm%2F04cw2qv?entry=ttu',
    'https://www.google.com/maps/place/Urif/@32.1587015,35.224177,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdf6513c44989:0xfa751e7cbebfbe81!8m2!3d32.158702!4d35.224177!16s%2Fm%2F04cw0qx?entry=ttu',
    'https://www.google.com/maps/place/%D8%B9%D8%B2%D9%85%D9%88%D8%B7%E2%80%AD/@32.2244195,35.309351,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce6d666246405:0xa3344c1c528c18ba!8m2!3d32.22442!4d35.309351!16s%2Fm%2F047mbnk?entry=ttu',
    'https://www.google.com/maps/place/%D8%B9%D8%B7%D8%A7%D8%B1%D8%A9%E2%80%AD/@32.0020795,35.206358,14z/data=!3m1!4b1!4m6!3m5!1s0x151d29db7e3c5053:0x29fd26f6dfb0955c!8m2!3d32.00208!4d35.206358!16s%2Fm%2F03ymn_m?entry=ttu',
    'https://www.google.com/maps/place/%D7%A2%D7%98%D7%A8%D7%AA%E2%80%AD/@32.0009865,35.1779365,15z/data=!3m1!4b1!4m6!3m5!1s0x151d299bfcdd0123:0x219cd2f12e7ea12c!8m2!3d32.001207!4d35.176982!16zL20vMGd2dHE0?entry=ttu',
    'https://www.google.com/maps/place/al-Eizariya/@31.7691345,35.271533,14z/data=!3m1!4b1!4m6!3m5!1s0x1503291045d015dd:0x19f1d8546a889975!8m2!3d31.769135!4d35.271533!16s%2Fm%2F0czd4kn?entry=ttu',
    'https://www.google.com/maps/place/Einabus/@32.1459395,35.242889,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdf1672471747:0x8746760eacbd1d33!8m2!3d32.14594!4d35.242889!16s%2Fm%2F047mmw0?entry=ttu',
    'https://www.google.com/maps/place/Ein+Al-Matwi/@32.0825735,35.133017,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2603d290201d:0xab563f7337318280!8m2!3d32.082574!4d35.133017!16s%2Fg%2F1td_7dz8?entry=ttu',
    'https://www.google.com/maps/place/Ein+Al+Sultan/@31.8795465,35.447437,14z/data=!3m1!4b1!4m6!3m5!1s0x151ccc4799c42bc9:0xc159d1de0d3e5013!8m2!3d31.879547!4d35.447437!16s%2Fm%2F04dz9l1?entry=ttu',
    'https://www.google.com/maps/place/%D7%A2%D7%99%D7%9F+%D7%92%D7%91%E2%80%AD/@32.783385,35.6383561,16z/data=!4m6!3m5!1s0x151c141c8f7ed7ef:0x994301b7e6697abe!8m2!3d32.783393!4d35.640119!16zL20vMDc4aF9r?entry=ttu',
    'https://www.google.com/maps/place/Ein+Zivan/@33.0964391,35.7957564,16z/data=!3m1!4b1!4m6!3m5!1s0x151eae5a6b8ea6d5:0x267e4b73b2d803ff!8m2!3d33.09645!4d35.796313!16s%2Fm%2F0462p0z?entry=ttu',
    'https://www.google.com/maps/place/%D8%B9%D9%8A%D9%86+%D9%8A%D8%A8%D8%B1%D9%88%D8%AF%E2%80%AD/@31.9532585,35.249852,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd5cfd9d496e3:0x8a6cda3ddbf6aa5c!8m2!3d31.953259!4d35.249852!16s%2Fm%2F03c75px?entry=ttu',
    "https://www.google.com/maps/place/%D7%A2%D7%99%D7%9F+%D7%9E%D7%A2'%D7%A8%E2%80%AD/@32.0584745,35.34674,14z/data=!3m1!4b1!4m10!1m2!2m1!1z16LXmdefINee16LigJnXqCAgIElzcmFlbA!3m6!1s0x151cdb0762f655db:0xa55e002e44276a81!8m2!3d32.058475!4d35.34674!15sChnXoteZ158g157XouKAmdeoICAgSXNyYWVskgEIbG9jYWxpdHngAQA!16s%2Fg%2F1vq1x5y5?entry=ttu",
    'https://www.google.com/maps/place/%D8%B9%D9%8A%D9%86+%D8%B3%D9%8A%D9%86%D9%8A%D8%A7%E2%80%AD/@31.9722385,35.227966,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd608501446d5:0x9f27d07351efc5ee!8m2!3d31.972239!4d35.227966!16s%2Fm%2F04_00yt?entry=ttu',
    'https://www.google.com/maps/place/Ein+Qiniyye/@33.2366797,35.7313062,14z/data=!3m1!4b1!4m6!3m5!1s0x151eb9faec61099d:0xd3f6a4ebfcb5242a!8m2!3d33.2366802!4d35.7313062!16s%2Fm%2F0411d50?entry=ttu',
    'https://www.google.com/maps/place/Ein+Qiniyye/@33.2366797,35.7313062,14z/data=!3m1!4b1!4m6!3m5!1s0x151eb9faec61099d:0xd3f6a4ebfcb5242a!8m2!3d33.2366802!4d35.7313062!16s%2Fm%2F0411d50?entry=ttu',
    'https://www.google.com/maps/place/%D8%B9%D8%B1%D8%A7%D9%82+%D8%A8%D9%88%D8%B1%D9%8A%D9%86%E2%80%AD/@32.2024085,35.239118,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdfde516f2d83:0xc71a01ac5f086453!8m2!3d32.202409!4d35.239118!16s%2Fg%2F11f6y3p388?entry=ttu',
    'https://www.google.com/maps/place/Acre,+Israel/@32.916975,35.0880969,13z/data=!3m1!4b1!4m6!3m5!1s0x151dc8fe03b29c25:0x709859e5804dc329!8m2!3d32.933052!4d35.082678!16zL20vMGZnamw?entry=ttu',
    'https://www.google.com/maps/place/%D7%A2%D7%9C%D7%99%E2%80%AD/@32.0711699,35.2687815,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd9aede54c7fd:0x30e73dfe2a1f5eb5!8m2!3d32.071177!4d35.266859!16zL20vMGMxNDUx?entry=ttu',
    'https://www.google.com/maps/place/%D7%A2%D7%9E%D7%95%D7%A8%D7%99%D7%94,+Rahat,+Israel%E2%80%AD/@31.3791312,34.735355,17z/data=!3m1!4b1!4m6!3m5!1s0x1502625be5bb26f9:0x95b9fd6aa3ff1aaf!8m2!3d31.3791312!4d34.735355!16s%2Fg%2F11hfhsft_m?entry=ttu',
    'https://www.google.com/maps/place/Amir,+Israel/@33.1778901,35.6207559,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea330489bc4d9:0xa7fb52b5c4d069eb!8m2!3d33.1777261!4d35.6198305!16s%2Fm%2F04f_xpx?entry=ttu',
    'https://www.google.com/maps/place/Immanuel/@32.1585666,35.1422255,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2152f1307211:0x551b081983b1fa5b!8m2!3d32.160688!4d35.136036!16zL20vMGM0bGp5?entry=ttu',
    'https://www.google.com/maps/place/Emek+HaYarden+St+11,+Kiryat+Ono,+Israel/@32.0638782,34.846286,17z/data=!3m1!4b1!4m5!3m4!1s0x151d4a6ed2439053:0x29e770613e210865!8m2!3d32.0638782!4d34.846286?entry=ttu',
    'https://www.google.com/maps/place/Askar/@32.2145605,35.303685,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce72d58deb36d:0xc49c794e54da43b5!8m2!3d32.214561!4d35.303685!16zL20vMGN0OW1n?entry=ttu',
    'https://www.google.com/maps/place/Ofra/@31.9533086,35.2620901,16z/data=!3m1!4b1!4m6!3m5!1s0x151cd42b602dc9d7:0xa405b1ca4798bda6!8m2!3d31.955101!4d35.260324!16zL20vMGI0eXEx?entry=ttu',
    'https://www.google.com/maps/place/Aqabat+Jabr/@31.8420475,35.445564,14z/data=!3m1!4b1!4m6!3m5!1s0x151ccce806208f63:0xa68eceebd10dbb92!8m2!3d31.842048!4d35.445564!16s%2Fm%2F047lzkz?entry=ttu',
    'https://www.google.com/maps/place/Aqraba/@32.1275855,35.342735,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdc5420917a17:0x23068472990b8035!8m2!3d32.127586!4d35.342735!16s%2Fm%2F03m87dz?entry=ttu',
    'https://www.google.com/maps/place/Bani+Zeid+al-Sharqiya/@32.0407085,35.173878,14z/data=!3m1!4b1!4m6!3m5!1s0x151d28591ee9e305:0x528dfe53c2ccf577!8m2!3d32.040709!4d35.173878!16s%2Fm%2F05p9qfy?entry=ttu',
    "https://www.google.com/maps/place/Far'a+el-Giftlik/@32.1436505,35.49702,14z/data=!3m1!4b1!4m6!3m5!1s0x151cea3e342a9ddb:0xa7b365403b15c8eb!8m2!3d32.143651!4d35.49702!16s%2Fg%2F1v2sjbjw?entry=ttu",
    'https://www.google.com/maps/place/Park+Tzora/@31.761916,34.9738314,16z/data=!4m10!1m2!2m1!1z16TXkNeo16cg16bXqNei15QgICA!3m6!1s0x1502c44e423adb65:0xa4e383c580fbec82!8m2!3d31.7611789!4d34.9807319!15sChHXpNeQ16jXpyDXpteo16LXlJIBFGNvbnN0cnVjdGlvbl9jb21wYW554AEA!16s%2Fg%2F1hc3zphb7?entry=ttu',
    'https://www.google.com/maps/place/Palmakhim+Industries+Park/@31.934509,34.718443,17z/data=!3m1!4b1!4m6!3m5!1s0x1502b1b2738e6357:0xbed56fe8c38ce741!8m2!3d31.934509!4d34.718443!16s%2Fg%2F11b7hjx1bq?entry=ttu',
    'https://www.google.com/maps/place/%D7%A4%D7%90%D7%A8%D7%A7+%D7%AA%D7%A2%D7%A9%D7%99%D7%95%D7%AA+%D7%A8%D7%90%D7%9D%E2%80%AD/@31.7905415,34.7528642,16z/data=!4m10!1m2!2m1!1zItek15DXqNenINeq16LXqdeZ15nXlCDXqNeQ150gICBJc3JhZWwi!3m6!1s0x1502bc0ddeb54a87:0x9caec81e199b6719!8m2!3d31.7905415!4d34.7572416!15sCici16TXkNeo16cg16rXotep15nXmdeUINeo15DXnSAgIElzcmFlbCLgAQA!16s%2Fg%2F11h51k3v4y?entry=ttu',
    'https://www.google.com/maps/place/Laqiya,+Israel/@31.324195,34.8699774,14z/data=!3m1!4b1!4m10!1m2!2m1!1zItek15bXldeo15Qg15zXp9eZ15QgICBJc3JhZWw!3m6!1s0x15025fd4cbf7a083:0xe2e3b39e8c2cb837!8m2!3d31.324884!4d34.866219!15sCh0i16TXlteV16jXlCDXnNen15nXlCAgIElzcmFlbJIBCGxvY2FsaXR54AEA!16zL20vMGJ5cm1y?entry=ttu',
    'https://www.google.com/maps/place/Ein+Kerem,+Jerusal%C3%A9m,+Israel/@31.7671312,35.1619863,16z/data=!3m1!4b1!4m6!3m5!1s0x1502d772018ac7c9:0xf405fd7ba09f94f4!8m2!3d31.7671028!4d35.1623714!16zL20vMDdiOTh4?entry=ttu',
    'https://www.google.com/maps/place/%D9%81%D8%B5%D8%A7%D9%8A%D9%84%E2%80%AD/@32.0253975,35.444264,14z/data=!3m1!4b1!4m6!3m5!1s0x151cc4521e9b1511:0x822d487cf1adab1!8m2!3d32.025398!4d35.444264!16s%2Fm%2F047r5zf?entry=ttu',
    'https://www.google.com/maps/place/Psagot/@31.8999835,35.223923,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd5504fa3e0ab:0x8948c566b4bbf3c7!8m2!3d31.899984!4d35.223923!16s%2Fm%2F04glvcl?entry=ttu',
    'https://www.google.com/maps/place/%D7%A4%D7%A6%D7%90%D7%9C%E2%80%AD/@32.0443941,35.442084,15z/data=!3m1!4b1!4m6!3m5!1s0x151cc4664971414d:0xf1e3cfad7b627e16!8m2!3d32.043492!4d35.443753!16s%2Fm%2F04gm1xk?entry=ttu',
    'https://www.google.com/maps/place/Furush+Beit+Dajan/@32.1952773,35.4473388,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce90d34a0e91b:0xf34936df8a282c3e!8m2!3d32.1832589!4d35.4563073!16s%2Fm%2F0464y42?entry=ttu',
    'https://www.google.com/maps/place/%D9%81%D8%B1%D8%AE%D8%A9%E2%80%AD/@32.0687425,35.150876,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2898cff15969:0xaf2a3584f1d2571b!8m2!3d32.068743!4d35.150876!16s%2Fm%2F04cwkt1?entry=ttu',
    'https://www.google.com/maps/place/Tzohar,+Israel/@31.2351375,34.426986,16z/data=!3m1!4b1!4m6!3m5!1s0x14fd8b8cbe02faf7:0x9657aa37d494a4eb!8m2!3d31.23717!4d34.426444!16s%2Fm%2F047fzdw?entry=ttu',
    NA,
    'https://www.google.com/maps/place/%D7%A6%D7%95%D7%A4%D7%99%D7%94%E2%80%AD/@31.877958,34.7306943,15z/data=!4m10!1m2!2m1!1z16bXldek15nXlA!3m6!1s0x1502bb9ac5621093:0x7691f515802f0ffc!8m2!3d31.877958!4d34.739449!15sCgrXpteV16TXmdeUkgEKZm91bmRhdGlvbuABAA!16s%2Fg%2F11fl3ym4df?entry=ttu',
    'https://www.google.com/maps/place/%D7%A7%D7%91%D7%95%D7%A2%D7%94,+Israel%E2%80%AD/@31.2250865,35.214754,14z/data=!3m1!4b1!4m6!3m5!1s0x1502552d4ae02769:0xba48ab35ad958e75!8m2!3d31.225087!4d35.214754!16s%2Fg%2F1ymswz7f2?entry=ttu',
    'https://www.google.com/maps/place/%D9%82%D8%A8%D9%84%D8%A7%D9%86%E2%80%AD/@32.1018195,35.287729,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd94f15a3f3a5:0xdca117d52ff140a2!8m2!3d32.10182!4d35.287729!16s%2Fm%2F047qxfx?entry=ttu',
    'https://www.google.com/maps/place/Kdumim/@32.2180506,35.161528,14z/data=!3m1!4b1!4m6!3m5!1s0x151d21c79ebfc461:0x9e230e9468ecb39b!8m2!3d32.21239!4d35.157388!16zL20vMGI5c3c1?entry=ttu',
    'https://www.google.com/maps/place/%D7%A7%D7%93%D7%9E%D7%AA+%D7%A6%D7%91%D7%99%E2%80%AD/@33.030075,35.6978959,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea7a74d742889:0xae1119cf7c51251e!8m2!3d33.029485!4d35.697744!16s%2Fm%2F0463689?entry=ttu',
    'https://www.google.com/maps/place/%D9%82%D9%88%D8%B2%D8%A9%E2%80%AD/@32.1383905,35.253164,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdf23d6798225:0x458d45fa583882cb!8m2!3d32.138391!4d35.253164!16s%2Fg%2F1ttrv1kw?entry=ttu',
    'https://www.google.com/maps/place/Qusra/@32.0845745,35.330037,14z/data=!3m1!4b1!4m6!3m5!1s0x151cdb922af77857:0x34778239ad7816ed!8m2!3d32.084575!4d35.330037!16s%2Fm%2F04ctk9r?entry=ttu',
    'https://www.google.com/maps/place/Dan,+Israel/@33.239794,35.6540716,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebbf5fcaa439f:0xca83690662ab782b!8m2!3d33.239637!4d35.653953!16zL20vMDljbnQy?entry=ttu',
    NA,
    'https://www.google.com/maps/place/%D7%A7%D7%9C%D7%99%D7%94%E2%80%AD/@31.7510134,35.466779,16z/data=!3m1!4b1!4m6!3m5!1s0x150333ee0c351bdf:0xfeee6df838a53c03!8m2!3d31.749896!4d35.466131!16s%2Fm%2F02r4px2?entry=ttu',
    "https://www.google.com/maps/place/Bruchim+Qela'+Alon/@33.131223,35.6868805,16z/data=!3m1!4b1!4m10!1m2!2m1!1z16fXnNeiIGlz!3m6!1s0x151ea4fb971c847b:0xe612e0c9a2c38578!8m2!3d33.131601!4d35.69039!15sCgnXp9ec16IgaXOSAQhsb2NhbGl0eeABAA!16s%2Fm%2F0464k7f?entry=ttu",
    'https://www.google.com/maps/place/Qasr+al-Sir,+Israel/@31.0833285,34.979579,14z/data=!3m1!4b1!4m6!3m5!1s0x1502469bf2b5cb15:0xab793b37e4e5736d!8m2!3d31.083329!4d34.979579!16s%2Fm%2F0462tfn?entry=ttu',
    'https://www.google.com/maps/place/%D7%A7%D7%A6%D7%A8%D7%99%D7%9F%E2%80%AD/@32.994181,35.6954075,14z/data=!3m1!4b1!4m6!3m5!1s0x151c1d9916d01bdf:0x90d62c7747edc8d2!8m2!3d32.990998!4d35.689865!16zL20vMDF2MGd0?entry=ttu',
    'https://www.google.com/maps/place/%D7%A7%D7%A8%D7%99%D7%AA+%D7%97%D7%99%D7%A0%D7%95%D7%9A+%D7%9E%D7%A8%D7%97%D7%91%D7%99%D7%9D%E2%80%AD/@31.3165805,34.578694,14z/data=!4m10!1m2!2m1!1z16fXqNeZ15nXqiDXl9eZ16DXldeaINee16jXl9eR15nXnSA!3m6!1s0x15027988110e80d1:0xbd6f9d20c421725a!8m2!3d31.3165805!4d34.5962035!15sCiLXp9eo15nXmdeqINeX15nXoNeV15og157XqNeX15HXmdedkgEXbG9naWNhbF90cmFuc2l0X3N0YXRpb27gAQA!16s%2Fg%2F11cs137pzw?entry=ttu',
    'https://www.google.com/maps/place/Qiryat-Chemon%C3%A1,+Israel/@33.2110794,35.5762589,14z/data=!3m1!4b1!4m6!3m5!1s0x151ebd5410adb449:0x396449c6726cbbad!8m2!3d33.207933!4d35.570246!16zL20vMDI5ZGZr?entry=ttu',
    'https://www.google.com/maps/place/Kiryat+Arba/@31.524928,35.134732,13z/data=!3m1!4b1!4m10!1m2!2m1!1z16fXqNeZ16og15DXqNeR16IgICBJc3JhZWw!3m6!1s0x1502e6a7d695b0ed:0x515b17caf43e90a5!8m2!3d31.529326!4d35.115625!15sChrXp9eo15nXqiDXkNeo15HXoiAgIElzcmFlbJIBCGxvY2FsaXR54AEA!16zL20vMDI5YzAz?entry=ttu',
    'https://www.google.com/maps/place/%D7%A7%D7%A8%D7%99%D7%AA+%D7%97%D7%99%D7%A0%D7%95%D7%9A+%D7%9E%D7%A8%D7%97%D7%91%D7%99%D7%9D%E2%80%AD/@31.3165805,34.578694,14z/data=!4m10!1m2!2m1!1z16fXqNeZ16og15fXmdeg15XXmiDXnteo15fXkdeZ150!3m6!1s0x15027988110e80d1:0xbd6f9d20c421725a!8m2!3d31.3165805!4d34.5962035!15sCiDXp9eo15nXqiDXl9eZ16DXldeaINee16jXl9eR15nXnZIBF2xvZ2ljYWxfdHJhbnNpdF9zdGF0aW9u4AEA!16s%2Fg%2F11cs137pzw?entry=ttu',
    'https://www.google.com/maps/place/Educational+Center+Sdot+Negev/@31.4310279,34.5906993,17z/data=!4m10!1m2!2m1!1z16fXqNeZ16og15fXmdeg15XXmiDXqdeT15XXqiDXoNeS15E!3m6!1s0x15027e0b730afd4b:0xd4c7ab19b98f1d8d!8m2!3d31.4310279!4d34.592888!15sCiPXp9eo15nXqiDXl9eZ16DXldeaINep15PXldeqINeg15LXkZIBCWVkdWNhdGlvbuABAA!16s%2Fg%2F1wz3bc03?entry=ttu',
    'https://www.google.com/maps/place/Kiryat+Shlomo+Hospital/@32.2342394,34.8533295,15z/data=!4m10!1m2!2m1!1z16fXqNeZ16og16nXnNee15Q!3m6!1s0x151d38a0e41dbca5:0x4caa9682193d781f!8m2!3d32.2342394!4d34.8620842!15sChHXp9eo15nXqiDXqdec157XlJIBCGhvc3BpdGFs4AEA!16s%2Fg%2F1tsw59qb?hl=pt-BR&entry=ttu',
    'https://www.google.com/maps/place/Qiryat-Chemon%C3%A1,+Israel/@33.2110795,35.5762589,14z/data=!3m1!4b1!4m6!3m5!1s0x151ebd5410adb449:0x396449c6726cbbad!8m2!3d33.207933!4d35.570246!16zL20vMDI5ZGZr?entry=ttu',
    'https://www.google.com/maps/place/Karnei+Shomron/@32.1707625,35.1053194,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2389c5aa79ed:0x81df70acafdf89b0!8m2!3d32.171553!4d35.097462!16zL20vMDlmbTY0?entry=ttu',
    'https://www.google.com/maps/place/%D7%A7%D7%A9%D7%AA%E2%80%AD/@32.9791,35.8087065,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea9e853e86981:0x38f5e7846ea807e3!8m2!3d32.980388!4d35.808484!16s%2Fm%2F027qxj2?entry=ttu',
    'https://www.google.com/maps/place/%D7%A8%D7%90%D7%9D%E2%80%AD/@30.979013,34.72362,17z/data=!4m6!3m5!1s0x150211a5cd84e66d:0xc96fce9182f705e8!8m2!3d30.979013!4d34.72362!16s%2Fg%2F1tg4hr4j?entry=ttu',
    'https://www.google.com/maps/place/Rosh+Tzurim/@31.6683276,35.1247681,16z/data=!3m1!4b1!4m6!3m5!1s0x1502dc1e6a67fa55:0xedd9c8a1add56c3f!8m2!3d31.667791!4d35.125288!16s%2Fm%2F02psgv3?entry=ttu',
    'https://www.google.com/maps/place/%D7%A8%D7%91%D7%91%D7%94%E2%80%AD/@32.118986,35.1284665,16z/data=!3m1!4b1!4m6!3m5!1s0x151d267facba7837:0x489c482daa1f1a21!8m2!3d32.11871!4d35.129583!16zL20vMGM1ZG5i?entry=ttu',
    'https://www.google.com/maps/place/Rahat,+Israel/@31.3881151,34.7506795,13z/data=!3m1!4b1!4m10!1m2!2m1!1z16jXlNeYINeV15TXpNeW15XXqNeUIA!3m6!1s0x1502620b62e38399:0x77e47b908a9cac48!8m2!3d31.394548!4d34.753934!15sChXXqNeU15gg15XXlNek15bXldeo15SSAQhsb2NhbGl0eeABAA!16zL20vMDF2dHRr?entry=ttu',
    'https://www.google.com/maps/place/Rujeib/@32.1916275,35.291606,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce0a627d73d6d:0xb73c0d341b27a286!8m2!3d32.191628!4d35.291606!16s%2Fm%2F04crd37?entry=ttu',
    'https://www.google.com/maps/place/%D7%A8%D7%95%D7%A2%D7%99,+Tefahot,+Israel%E2%80%AD/@32.8706875,35.419893,14z/data=!3m1!4b1!4m6!3m5!1s0x151c3a3b1749949b:0x3dd769fa6148be38!8m2!3d32.870688!4d35.419893!16s%2Fg%2F1ymtth_3c?entry=ttu',
    'https://www.google.com/maps/place/%D7%A8%D7%97%D7%9C%D7%99%D7%9D%E2%80%AD/@32.1024595,35.257756,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd8df9e07c84f:0xdd937ec2705b9cc8!8m2!3d32.10246!4d35.257756!16zL20vMGNzZnI5?entry=ttu',
    'https://www.google.com/maps/place/Givat+Shemesh,+Israel/@31.7741885,34.95055,14z/data=!3m1!4b1!4m6!3m5!1s0x1502c681fd0c2bb7:0x4686a1be56c4df5e!8m2!3d31.774189!4d34.95055!16s%2Fm%2F04gqbvx?entry=ttu',
    'https://www.google.com/maps/place/Rimonim/@31.9353815,35.340494,14z/data=!3m1!4b1!4m6!3m5!1s0x151cd17de591d7c3:0x5c3d7883c812dc59!8m2!3d31.935382!4d35.340494!16s%2Fm%2F02vv2l6?entry=ttu',
    'https://www.google.com/maps/place/Ramallah/@31.9073508,35.205882,14z/data=!3m1!4b1!4m6!3m5!1s0x1502d54cda2d58d1:0xbf6d4d17cc8b2c76!8m2!3d31.9037641!4d35.2034184!16zL20vMGMxMWw?entry=ttu',
    'https://www.google.com/maps/place/%D7%93%D7%A8%D7%9A+%D7%A8%D7%9E%D7%95%D7%9F,+Mitzpe+Ramon,+Israel%E2%80%AD/@30.6108887,34.8009348,17z/data=!3m1!4b1!4m10!1m2!2m1!1z16jXnteV158!3m6!1s0x1501f248a7e84641:0x55155c264a48a218!8m2!3d30.6108887!4d34.8009348!15sCgjXqNee15XXn5IBBXJvdXRl4AEA!16s%2Fg%2F11sghpj5hb?entry=ttu',
    'https://www.google.com/maps/place/Ramat+HaNadiv/@32.547972,34.8742092,12z/data=!4m10!1m2!2m1!1zICLXqNee16og15TXoNeT15nXkSAgIElzcmFlbA!3m6!1s0x151d0be4bc79dc65:0xec75c23765d98553!8m2!3d32.547972!4d34.944247!15sChsi16jXnteqINeU16DXk9eZ15EgICBJc3JhZWySAQRwZWFr4AEA!16s%2Fg%2F119wrtcnw?entry=ttu',
    'https://www.google.com/maps/place/Ramat+Magshimim/@32.845356,35.807777,16z/data=!3m1!4b1!4m6!3m5!1s0x151c05c9d3ef49d9:0x23e0a4a25b1e2752!8m2!3d32.844066!4d35.80658!16s%2Fm%2F0465fq7?entry=ttu',
    'https://www.google.com/maps/place/%D8%B1%D9%81%D9%8A%D8%AF%D9%8A%D8%A7%E2%80%AD/@32.2275216,35.223183,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce019aa5e6325:0x4e23c91aabe2792e!8m2!3d32.227522!4d35.223183!16s%2Fm%2F05mskr6?entry=ttu',
    "https://www.google.com/maps/place/She'ar+Yashuv,+Israel/@33.2261512,35.6461108,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebc771fe89f7b:0x1749bd4e0c91a331!8m2!3d33.2263492!4d35.6468357!16s%2Fm%2F03wfh15?entry=ttu",
    'https://www.google.com/maps/place/%D7%A9%D7%91%D7%95%D7%AA+%D7%A8%D7%97%D7%9C%E2%80%AD/@32.0545385,35.310703,14z/data=!3m1!4b1!4m6!3m5!1s0x151cda2f74c73edb:0x1920bd383116cceb!8m2!3d32.054539!4d35.310703!16s%2Fm%2F02w1zdj?entry=ttu',
    'https://www.google.com/maps/place/Shaqib+al-Salam,+Israel/@31.2017005,34.8390906,14z/data=!3m1!4b1!4m10!1m2!2m1!1z16nXkteRINep15zXldedINeV15TXpNeW15XXqNeU!3m6!1s0x1502686cb7590cb3:0x61af8b9d736833c4!8m2!3d31.198361!4d34.839665!15sCh7XqdeS15Eg16nXnNeV150g15XXlNek15bXldeo15SSAQhsb2NhbGl0eeABAA!16zL20vMGJ5cm50?entry=ttu',
    'https://www.google.com/maps/place/Sde+Nehemia,+Israel/@33.1882845,35.624589,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebccd02dbce95:0xc79240c8fd4087ca!8m2!3d33.188234!4d35.62307!16s%2Fm%2F03h3bc7?entry=ttu',
    NA,
    "https://www.google.com/maps/place/Carmiel,+Israel/@32.9112925,35.3115249,13z/data=!3m1!4b1!4m6!3m5!1s0x151c33e664765f2b:0x1b61c5431d415446!8m2!3d32.914671!4d35.292417!16zL20vMDF0eXBj?entry=ttu",
    'https://www.google.com/maps/place/Gilboa+factories/@32.6732367,33.9605051,7.59z/data=!4m10!1m2!2m1!1zICDXnteR15XXkNeV16og15TXktec15HXldeiICAgSXNyYWVs!3m6!1s0x151c5a0356572b45:0x8b3db1567d0113b3!8m2!3d32.554399!4d35.400988!15sCiLXnteR15XXkNeV16og15TXktec15HXldeiICAgSXNyYWVs4AEA!16s%2Fg%2F1tc_ncd3?entry=ttu',
    'https://www.google.com/maps/place/Nir+Etzion,+Israel/@32.6980296,34.992115,16z/data=!3m1!4b1!4m6!3m5!1s0x151da4357255b11b:0xfeafa6062e1cdc01!8m2!3d32.699125!4d34.992526!16s%2Fm%2F04cxy4y?entry=ttu',
    'https://www.google.com/maps/place/%D7%A4%D7%90%D7%A8%D7%A7+%D7%A0.%D7%A2.%D7%9E%E2%80%AD/@31.4234933,34.6001209,18z/data=!3m1!4b1!4m6!3m5!1s0x15027ddee3bb13d1:0x91ce2052c6caeabc!8m2!3d31.4234933!4d34.6001209!16s%2Fg%2F11h52r_n2z?entry=ttu',
    "https://www.google.com/maps/place/Sha'ar+Na'aman+Industrial+Zone/@32.893398,35.091562,17z/data=!3m1!4b1!4m6!3m5!1s0x151dc9d0f91757f5:0xc28b2e14165e2433!8m2!3d32.893398!4d35.091562!16s%2Fg%2F1tgldctg?entry=ttu",
    'https://www.google.com/maps/place/%D7%9E%D7%A4%D7%A2%D7%9C+%D7%A0%D7%A9%D7%A8+%D7%A8%D7%9E%D7%9C%D7%94%E2%80%AD/@31.9164883,34.8964425,17z/data=!3m1!4b1!4m6!3m5!1s0x1502ca28130ebd91:0x69d3b708fd577062!8m2!3d31.9164883!4d34.8964425!16s%2Fg%2F120n2khf?entry=ttu',
    "https://www.google.com/maps/place/Ad+Halom+Park/@31.7832605,34.6631285,14z/data=!4m10!1m2!2m1!1z16LXkyDXlNec15XXnQ!3m6!1s0x1502a3251e7ec9d3:0x3046be3c291dd8b8!8m2!3d31.7853204!4d34.668603!15sCg3XoteTINeU15zXldedkgEEcGFya-ABAA!16s%2Fg%2F1tlfs3ns?entry=ttu",
    'https://www.google.com/maps/place/Idan+HaNegev+Industrial+Park/@31.380758,34.785969,17z/data=!3m1!4b1!4m6!3m5!1s0x150261e5387bf30f:0x4a306e3482dfa4f6!8m2!3d31.380758!4d34.785969!16s%2Fm%2F0ll4yf4?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%96%D7%95%D7%A8+%D7%AA%D7%A2%D7%A9%D7%99%D7%94+%D7%A6%D7%97%22%D7%A8%E2%80%AD/@33.1830866,30.6452143,6.5z/data=!4m10!1m2!2m1!1z16rXotep15nXmdeUINemLteXLteo!3m6!1s0x151c21ffec458805:0xe1c1601e93f082ac!8m2!3d32.9705521!4d35.5646455!15sChXXqtei16nXmdeZ15Qg16Yu15cu16iSARBjb3Jwb3JhdGVfb2ZmaWNl4AEA!16s%2Fg%2F11hd1wtj50?entry=ttu',
    'https://www.google.com/maps/place/Tsemah+Regional+industries/@32.7144324,35.5727785,14z/data=!4m10!1m2!2m1!1z16rXotep15nXmdeUINem157Xlw!3m6!1s0x151c6a94c5a48505:0xe0ac5ca54198c91d!8m2!3d32.70327!4d35.586875!15sChPXqtei16nXmdeZ15Qg16bXnteXWhUiE9eq16LXqdeZ15nXlCDXptee15eaASNDaFpEU1VoTk1HOW5TMFZKUTBGblNVUXRlRjlQTmtObkVBReABAA!16s%2Fg%2F1vfhk69y?entry=ttu',
    'https://www.google.com/maps/place/North+Industry+Zone/@31.664701,34.59809,17z/data=!3m1!4b1!4m6!3m5!1s0x15029b8a6ec1b245:0x35cd56589664ea98!8m2!3d31.664701!4d34.59809!16s%2Fg%2F1w9p5fn2?entry=ttu',
    'https://www.google.com/maps/place/Cesareia,+Israel/@32.496276,34.9199935,13z/data=!3m1!4b1!4m6!3m5!1s0x151d0cf83a10b203:0x4ac5521593cfcdcb!8m2!3d32.519016!4d34.904544!16zL20vMDlqdDFs?entry=ttu',
    'https://www.google.com/maps/place/Qiryat+Gat,+Israel/@31.6095861,34.7769395,13z/data=!3m1!4b1!4m6!3m5!1s0x15029166b2e7a63b:0xcf6958d0335368fd!8m2!3d31.611148!4d34.768459!16zL20vMDF2dHFt?entry=ttu',
    'https://www.google.com/maps/place/Regavim,+Israel/@32.5224645,35.0342945,16z/data=!3m1!4b1!4m6!3m5!1s0x151d08a6ad5fa91b:0xf22cad4d2e198637!8m2!3d32.52344!4d35.033987!16s%2Fm%2F04gk769?entry=ttu',
    'https://www.google.com/maps/place/Ramat+Dalton/@33.021944,35.4680556,15z/data=!3m1!4b1!4m6!3m5!1s0x151c26c812503e0b:0x4841608eefc5569e!8m2!3d33.0219444!4d35.4680556!16s%2Fg%2F11cs00nsnh?entry=ttu',
    'https://www.google.com/maps/place/Shkhoret+Industrial+Zone/@29.597881,34.9718911,17z/data=!3m1!4b1!4m6!3m5!1s0x15006e8aaf9b0fe9:0x759ba1c62d6dc13a!8m2!3d29.597881!4d34.9718911!16s%2Fg%2F1yfjjd1__?entry=ttu',
    "https://www.google.com/maps/place/Sha'ar+Binyamin+industrial+zone/@31.865575,35.261645,11z/data=!3m1!4b1!4m6!3m5!1s0x15032b26d7b4b1df:0xee49d8d8d5b41267!8m2!3d31.865575!4d35.261645!16s%2Fm%2F0j445wm?entry=ttu",
    "https://www.google.com/maps/place/Sha'ar+Na'aman+Industrial+Zone/@32.893398,35.091562,17z/data=!3m1!4b1!4m6!3m5!1s0x151dc9d0f91757f5:0xc28b2e14165e2433!8m2!3d32.893398!4d35.091562!16s%2Fg%2F1tgldctg?entry=ttu",
    'https://www.google.com/maps/place/Timorim,+Israel/@31.7151415,34.7605895,16z/data=!3m1!4b1!4m6!3m5!1s0x1502961d1490e0c9:0xcd036335f56ee4c0!8m2!3d31.716336!4d34.761356!16s%2Fm%2F0462f18?entry=ttu',
    'https://www.google.com/maps/place/Ayelet+HaShahar,+Israel/@33.023077,35.5778794,15z/data=!3m1!4b1!4m6!3m5!1s0x151c1f927555e7ff:0x847a5745e12b394e!8m2!3d33.021115!4d35.576732!16s%2Fm%2F03cn238?entry=ttu',
    'https://www.google.com/maps/place/Iskaka/@32.1029205,35.224549,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd89a8b17d95f:0x3ba5f7b8973bdacb!8m2!3d32.102921!4d35.224549!16s%2Fm%2F04crfj8?entry=ttu',
    'https://www.google.com/maps/place/Itamar,+Israel/@32.0898021,34.8068353,17z/data=!3m1!4b1!4m6!3m5!1s0x151d4bcf7d47a753:0x92e2cba529a42efd!8m2!3d32.0898021!4d34.8068353!16s%2Fg%2F1ymwgywyd?entry=ttu',
    'https://www.google.com/maps/place/Al-Lubban+Al-Gharbi/@32.0357755,35.038365,15z/data=!3m1!4b1!4m6!3m5!1s0x151d31fdfa415d19:0x7b17d7c45853d2ca!8m2!3d32.035776!4d35.038365!16s%2Fm%2F03y7sq1?entry=ttu',
    'https://www.google.com/maps/place/Alon+HaGalil,+Israel/@32.7570006,35.220052,16z/data=!3m1!4b1!4m6!3m5!1s0x151c4cade21b5b55:0x87732d9803ac2242!8m2!3d32.756874!4d35.220294!16s%2Fm%2F04f0lzz?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%9C%D7%95%D7%A0%D7%99+%D7%94%D7%91%D7%A9%D7%9F%E2%80%AD/@33.0438561,35.8383879,17z/data=!3m1!4b1!4m6!3m5!1s0x151eac0583e84b1d:0x187becc53a77ac84!8m2!3d33.044087!4d35.838507!16s%2Fm%2F0462czm?entry=ttu',
    'https://www.google.com/maps/place/Elon+Moreh/@32.2272506,35.3492156,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce6ecbea86371:0x444acb83e64d0fbf!8m2!3d32.234418!4d35.331201!16s%2Fm%2F025v07j?entry=ttu',
    'https://www.google.com/maps/place/Alon+Shvut/@31.6643275,35.114553,14z/data=!3m1!4b1!4m6!3m5!1s0x1502dc27c3c10157:0x986c0b293ebf3a76!8m2!3d31.654871!4d35.125256!16zL20vMGM5NHRs?entry=ttu',
    'https://www.google.com/maps/place/al-Khader/@31.6948105,35.170745,15z/data=!3m1!4b1!4m6!3m5!1s0x1502d91dff3cd261:0xcda3ff2a00c0e3ad!8m2!3d31.694811!4d35.170745!16s%2Fm%2F03cp3xd?entry=ttu',
    'https://www.google.com/maps/place/Eliad/@32.8049809,35.734976,15z/data=!3m1!4b1!4m6!3m5!1s0x151c10266aabe675:0xc65a892e84761bd4!8m2!3d32.806583!4d35.736575!16s%2Fm%2F0466l68?entry=ttu',
    'https://www.google.com/maps/place/Almagor,+Israel/@32.9126025,35.6032754,16z/data=!3m1!4b1!4m6!3m5!1s0x151c185130dbdf2d:0x8d4af75fcab6d76a!8m2!3d32.912709!4d35.60231!16s%2Fm%2F04dz6k1?entry=ttu',
    'https://www.google.com/maps/place/Almog/@31.7879765,35.4617375,16z/data=!3m1!4b1!4m6!3m5!1s0x1503336e9efb28cb:0x306d57d5e1070811!8m2!3d31.790034!4d35.461968!16s%2Fm%2F03m9jkf?entry=ttu',
    'https://www.google.com/maps/place/AlSayid+Tribe,+Israel/@31.2817185,34.916298,15z/data=!3m1!4b1!4m6!3m5!1s0x15025ebee46d22bb:0xa4b4699b86f312f3!8m2!3d31.281719!4d34.916298!16s%2Fm%2F0464xdc?entry=ttu',
    'https://www.google.com/maps/place/Shila/@32.0881787,34.7735754,17z/data=!3m1!4b1!4m6!3m5!1s0x151d4c7605ef42fb:0x7c5ef15c8e987661!8m2!3d32.0881787!4d34.7735754!16s%2Fg%2F11j00s33yf?entry=ttu',
    'https://www.google.com/maps/place/Nablus/@32.2243095,35.2476793,14z/data=!3m1!4b1!4m6!3m5!1s0x151ce0f650425697:0x7f0ba930bd153d84!8m2!3d32.2226678!4d35.2621461!16zL20vMDFoeHg2?entry=ttu',
    'https://www.google.com/maps/place/%D7%A6%D7%99%D7%9E%D7%A8+%D7%A9%D7%9C%D7%95%D7%95%D7%94+%D7%91%D7%9E%D7%93%D7%91%D7%A8%E2%80%AD/@30.767942,35.2772213,17z/data=!3m1!4b1!4m6!3m5!1s0x1503cd9302679be9:0x6baeee9db8822428!8m2!3d30.767942!4d35.2772213!16s%2Fg%2F11c54w9pwd?entry=ttu',
    'https://www.google.com/maps/place/Shamir,+Israel/@33.166208,35.6607399,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea4af2057c9e9:0x55b0a34168977dcd!8m2!3d33.166754!4d35.659949!16zL20vMGQwYjVs?entry=ttu',
    'https://www.google.com/maps/place/%D7%A9%D7%A0%D7%99%D7%A8%E2%80%AD/@33.2419235,35.678258,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebbddbb29bc3b:0x43c0103d13ba5d2e!8m2!3d33.240338!4d35.677015!16s%2Fm%2F04g00kj?entry=ttu',
    "https://www.google.com/maps/place/Se'orim,+Israel/@32.6968275,35.417603,15z/data=!3m1!4b1!4m6!3m5!1s0x151c45dc067a42c9:0xacb882b072125a30!8m2!3d32.696828!4d35.417603!16s%2Fg%2F1vfhk69f?entry=ttu",
    "https://www.google.com/maps/place/Sha'al/@33.1171664,35.7180195,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea50ab351b827:0xe51c8cec8f0d4542!8m2!3d33.11653!4d35.718816!16s%2Fm%2F0465807?entry=ttu",
    "https://www.google.com/maps/place/Sha'ar+HaGolan,+Israel/@32.6869705,35.6042804,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6afc0ddc86c1:0xa45a3d826aa96367!8m2!3d32.686919!4d35.604656!16zL20vMDhnd3gw?entry=ttu",
    "https://www.google.com/maps/place/Sha'ar+Menashe/@32.4438777,35.0065985,16z/data=!4m10!1m2!2m1!1z16nXoteoINee16DXqdeUICAg!3m6!1s0x151d0fc1a8d3c851:0xb710878770d0fdbf!8m2!3d32.4438777!4d35.0109759!15sCg_Xqdei16gg157XoNep15SSAQ5tZWRpY2FsX2NsaW5pY-ABAA!16s%2Fm%2F04gp4sj?entry=ttu",
    'https://www.google.com/maps/place/%D7%AA%D7%95%D7%9E%D7%A8%E2%80%AD/@32.018984,35.4392885,16z/data=!3m1!4b1!4m6!3m5!1s0x151cc5ab8a0deb57:0xc0e47458f0757a9a!8m2!3d32.018587!4d35.4398!16s%2Fm%2F04gwb27?entry=ttu',
    'https://www.google.com/maps/place/%D8%AA%D8%B1%D9%85%D8%B3%D8%B9%D9%8A%D8%A7%E2%80%AD/@32.0327255,35.288375,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd9e251c69f71:0xe7f940c128f86ff3!8m2!3d32.032726!4d35.288375!16s%2Fm%2F027n6nt?entry=ttu',
    'https://www.google.com/maps/place/Rosh+HaAyin+North/@32.1176427,34.9291971,14.79z/data=!4m10!1m2!2m1!1z16rXl9eg16og16jXm9eR16og16jXkNepINeU16LXmdef!3m6!1s0x151d3738ce003e7d:0xc253df44b985fd8e!8m2!3d32.120865!4d34.934528!15sCiHXqteX16DXqiDXqNeb15HXqiDXqNeQ16kg15TXoteZ15-SAQ10cmFpbl9zdGF0aW9u4AEA!16s%2Fm%2F04ybwx4?entry=ttu',
    'https://www.google.com/maps/place/Telavive,+Israel/@32.0879315,34.797246,12z/data=!3m1!4b1!4m6!3m5!1s0x151d4ca6193b7c1f:0xc1fb72a2c0963f90!8m2!3d32.0852999!4d34.7817676!16zL20vMDdxenY?entry=ttu',
    'https://www.google.com/maps/place/Tel-Hay,+Kfar+Saba,+Israel/@32.1769088,34.9148474,17z/data=!3m1!4b1!4m6!3m5!1s0x151d39b04d530c79:0xe933289d2b6445f2!8m2!3d32.1769088!4d34.9148474!16s%2Fg%2F1ymw793xd?entry=ttu',
    'https://www.google.com/maps/place/%D7%A4%D7%96%D7%95%D7%A8%D7%94+%D7%AA%D7%9C+%D7%A2%D7%A8%D7%93%E2%80%AD/@31.2532467,35.1116338,17z/data=!3m1!4b1!4m6!3m5!1s0x150257a8d10b3aa5:0xafa2ba91706c3dea!8m2!3d31.2532467!4d35.1116338!16s%2Fg%2F11h1m4y674?entry=ttu',
    'https://www.google.com/maps/place/%D8%AA%D9%84%D8%A8%D9%8A%D9%88%D8%AA+%D9%81%D9%8A+%D9%85%D8%B1%D9%83%D8%B2+%D8%A7%D9%84%D8%B3%D8%A7%D9%85%D8%B1%D8%A9%E2%80%AD/@32.0872965,35.291506,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd96879bb80d7:0x86e55e9e1c4109db!8m2!3d32.087297!4d35.291506!16s%2Fg%2F1td6jq7_?entry=ttu',
    'https://www.google.com/maps/place/%D7%AA%D7%9C+%D7%A7%D7%A6%D7%99%D7%A8%E2%80%AD/@32.7057485,35.6167419,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6b2f8043ed4f:0xa3a5c7e6fa32e16b!8m2!3d32.706396!4d35.61786!16s%2Fm%2F04f028h?entry=ttu',
    "https://www.google.com/maps/place/Taasi'on+ha-Sharon/@32.11378,34.917535,17z/data=!3m1!4b1!4m6!3m5!1s0x151d3704b2baaba3:0x7ed50207b0f4839b!8m2!3d32.11378!4d34.917535!16s%2Fg%2F1v27401c?entry=ttu",
    'https://www.google.com/maps/place/%D7%AA%D7%A2%D7%A9%D7%99%D7%95%D7%9F+%D7%97%D7%A6%D7%91%E2%80%AD/@32.07173,34.954425,17z/data=!3m1!4b1!4m6!3m5!1s0x151d314b898c6907:0x30a790efe3d6e63c!8m2!3d32.07173!4d34.954425!16s%2Fg%2F1td8gbvx?entry=ttu',
    'https://www.google.com/maps/place/%D7%AA%D7%A2%D7%A9%D7%99%D7%95%D7%9F+%D7%A6%D7%A8%D7%99%D7%A4%D7%99%D7%9F%E2%80%AD/@31.963734,34.8463718,17z/data=!4m10!1m2!2m1!1z16rXotep15nXldefINem16jXmdek15nXnw!3m6!1s0x1502b59ba0b3e765:0xae9ba7aed45138b6!8m2!3d31.963734!4d34.8485605!15sChnXqtei16nXmdeV158g16bXqNeZ16TXmdefkgEXbG9naWNhbF90cmFuc2l0X3N0YXRpb27gAQA!16s%2Fg%2F11cs12_c34?entry=ttu',
    'https://www.google.com/maps/place/Industrial+zone+u+h+k/@32.4735311,35.1781932,17z/data=!3m1!4b1!4m6!3m5!1s0x151d019b3c477a31:0x8c6c523423b67f61!8m2!3d32.4735311!4d35.1781932!16s%2Fg%2F1q5bl0zh6?entry=ttu',
    "https://www.google.com/maps/place/Yizre'am+Farm,+Israel/@31.4429626,34.571844,15z/data=!3m1!4b1!4m6!3m5!1s0x15027e3cf8b65f0b:0xb04a9d33a4c84398!8m2!3d31.442963!4d34.571844!16s%2Fg%2F1trl82ys?entry=ttu",
    'https://www.google.com/maps/place/Tirabin+al-Sana,+Israel/@31.3445964,34.7375071,15z/data=!3m1!4b1!4m6!3m5!1s0x150263a2eedcdf9d:0xb5a2390aca03037f!8m2!3d31.345481!4d34.739126!16s%2Fm%2F046473d?entry=ttu',
    'https://www.google.com/maps/place/Tradyon/@32.8584422,35.266592,14.26z/data=!4m6!3m5!1s0x151c344cd19350b1:0x9a36e9ffdfabff3!8m2!3d32.867686!4d35.273122!16s%2Fg%2F1tm9djk0?entry=ttu',
    "https://www.google.com/maps/place/%D7%90%D7%9C+%D7%A4%D7%95%D7%A8%D7%A2%D7%94+%D7%90'%E2%80%AD/@31.2085214,34.7884828,9.73z/data=!4m6!3m5!1s0x150255bcf5947571:0x426d85847274ae7f!8m2!3d31.2506527!4d35.1571925!16s%2Fg%2F11b7sycmyf?entry=ttu",
    'https://www.google.com/maps/place/Elkana,+Jerusalem,+Israel/@31.2085214,34.7884828,9.73z/data=!4m6!3m5!1s0x1502d6206f483d8d:0x8de2b5640b3e99a2!8m2!3d31.7939179!4d35.2141166!16s%2Fg%2F1ymvfcnxh?entry=ttu',
    'https://www.google.com/maps/place/%D8%A7%D9%84%D8%B1%D9%88%D9%85%E2%80%AD/@33.1794486,35.770733,16z/data=!3m1!4b1!4m6!3m5!1s0x151eb0f6e5e6940f:0x60d48fb10ff53993!8m2!3d33.180519!4d35.770095!16zL20vMDduMWNf?entry=ttu',
    'https://www.google.com/maps/place/Amnun,+Israel/@32.9050934,35.571802,16z/data=!3m1!4b1!4m6!3m5!1s0x151c180b8ac2f063:0xa55398e0a3282073!8m2!3d32.904989!4d35.571136!16s%2Fm%2F03nx181?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%A0%D7%99%D7%A2%D7%9D%E2%80%AD/@32.9567375,35.7406115,16z/data=!3m1!4b1!4m6!3m5!1s0x151c1d2b58a77043:0x9bafc8d6ec0be4aa!8m2!3d32.956638!4d35.739718!16s%2Fm%2F0465d51?entry=ttu',
    'https://www.google.com/maps/place/%D7%90-%D7%A1%D7%9C%D7%91%D7%94%E2%80%AD/@32.9293945,35.712766,15z/data=!3m1!4b1!4m6!3m5!1s0x151c1c9a2523f0ef:0x787e245753a5e039!8m2!3d32.929395!4d35.712766!16s%2Fg%2F1tdgxn9_?entry=ttu',
    'https://www.google.com/maps/place/Metzad/@31.587002,35.1901589,16z/data=!3m1!4b1!4m6!3m5!1s0x1502dfdf33bc717b:0xaad0a5ff6302cc68!8m2!3d31.586152!4d35.187594!16s%2Fm%2F02psgmy?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%A4%D7%99%D7%A7%E2%80%AD/@32.7797635,35.7018325,16z/data=!3m1!4b1!4m6!3m5!1s0x151c119b78792e1f:0x762f7e33babcd864!8m2!3d32.779428!4d35.702994!16zL20vMDY0enA0?entry=ttu',
    'https://www.google.com/maps/place/Afikim,+Israel/@32.6802345,35.5777715,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6a6544ac7e97:0x88b5c4b60b0fadb0!8m2!3d32.680564!4d35.57773!16zL20vMDZicF8w?entry=ttu',
    'https://www.google.com/maps/place/Efrat/@31.6656128,35.1633174,14z/data=!3m1!4b1!4m6!3m5!1s0x1502decffed40ac3:0x5fcf186a4abbd39a!8m2!3d31.653589!4d35.149934!16zL20vMDNjOTA0?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%A8%D7%92%D7%9E%D7%9F%E2%80%AD/@32.1722275,35.5222129,16z/data=!3m1!4b1!4m6!3m5!1s0x151ceb0aa6644d6b:0x87af03d2307d315f!8m2!3d32.17334!4d35.522285!16s%2Fm%2F04gjc_h?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%A8%D7%99%D7%90%D7%9C%E2%80%AD/@31.8244063,34.1567282,8.38z/data=!4m6!3m5!1s0x151d270b0797feeb:0xe8ae03cbd935baad!8m2!3d32.104637!4d35.174514!16zL20vMDJ2MDJ3?entry=ttu',
    NA,
    "https://www.google.com/maps/place/Ashdot+Ya'akov+Ihud,+Israel/@32.6580961,35.5798165,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6a1a63b4a74b:0x36c1b6494f179a90!8m2!3d32.65877!4d35.579637!16s%2Fm%2F04f0sy9?entry=ttu",
    "https://www.google.com/maps/place/Ashdot+Ya'akov+Meuhad,+Israel/@32.6627285,35.5835955,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6a1472c3b9e9:0xd6d8c9cddb1e17c9!8m2!3d32.664808!4d35.582769!16s%2Fm%2F04f5np4?entry=ttu",
    'https://www.google.com/maps/place/Ascal%C3%A3o,+Israel/@31.6677251,34.5646541,13z/data=!3m1!4b1!4m6!3m5!1s0x15029c5141685e73:0xeb72211d3092dc0f!8m2!3d31.6687885!4d34.5742523!16zL20vMGZkazQ?entry=ttu',
    "https://www.google.com/maps/place/Asaf+Duda'im+Waste+Site/@31.3195802,34.7286289,17z/data=!4m6!3m5!1s0x15026487b1d3aff3:0xb544c30c1c4f3dd!8m2!3d31.3195802!4d34.7286289!16s%2Fg%2F120m0pjt?entry=ttu",
    "https://www.google.com/maps/place/Be'er+Sheva+North/@31.262089,34.809288,17z/data=!3m1!4b1!4m6!3m5!1s0x150266f879d0c3e1:0x2e58887a229204b5!8m2!3d31.262089!4d34.809288!16s%2Fm%2F02z5lxg?entry=ttu",
    "https://www.google.com/maps/place/Buq'ata/@33.2026567,35.7767723,15z/data=!3m1!4b1!4m6!3m5!1s0x151eb0d9f98ff651:0xf47cbdefa09843ba!8m2!3d33.2026572!4d35.7767723!16zL20vMDdtd212?entry=ttu",
    NA,
    'https://www.google.com/maps/place/%D8%A8%D9%88%D8%B1%D9%87%D8%A7%D9%85%E2%80%AD/@31.9884775,35.175441,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2980c699bed9:0x1a8eea10438f0e21!8m2!3d31.988478!4d35.175441!16s%2Fm%2F04y7bpn?entry=ttu',
    'https://www.google.com/maps/place/%D8%A8%D9%88%D8%B1%D9%8A%D9%86%E2%80%AD/@32.1846585,35.249229,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdfc8f5583db9:0xb5827497e09b7b5f!8m2!3d32.184659!4d35.249229!16s%2Fm%2F04csn_m?entry=ttu',
    'https://www.google.com/maps/place/Birzeit/@31.9753135,35.196042,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2a192cf4ce57:0x27b74f88cf9c7db5!8m2!3d31.975314!4d35.196042!16zL20vMDloMmNm?entry=ttu',
    'https://www.google.com/maps/place/Beit+Ur+al-Fauqa/@31.8861805,35.114256,15z/data=!3m1!4b1!4m6!3m5!1s0x1502d39d689eebe5:0x45421590564d85ac!8m2!3d31.886181!4d35.114256!16s%2Fm%2F04cyzdl?entry=ttu',
    'https://www.google.com/maps/place/%D7%91%D7%99%D7%AA+%D7%90%D7%9C%E2%80%AD/@31.9413695,35.2279204,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd585602cfba5:0xc2cac9586dcc7d4e!8m2!3d31.9416!4d35.222734!16zL20vMDdzZnA1?entry=ttu',
    'https://www.google.com/maps/place/Beit+Aryeh-Ofarim/@32.03295,35.0476606,14z/data=!3m1!4b1!4m6!3m5!1s0x151d2e1a7135a8b3:0x8c608536dc015bdd!8m2!3d32.040059!4d35.049472!16s%2Fm%2F04gpp35?entry=ttu',
    'https://www.google.com/maps/place/Beit+Hillel,+Israel/@33.2090245,35.6037705,14z/data=!3m1!4b1!4m6!3m5!1s0x151ebd210127cd35:0xc5e860cf100151a1!8m2!3d33.208782!4d35.606515!16s%2Fm%2F03nx1cx?entry=ttu',
    'https://www.google.com/maps/place/Beit+HaArava/@31.808859,35.486759,15z/data=!3m1!4b1!4m6!3m5!1s0x15033355c6c15b95:0x2cda876fee2e953e!8m2!3d31.807762!4d35.476411!16s%2Fm%2F02zbdl5?entry=ttu',
    'https://www.google.com/maps/place/Beit+Zera,+Israel/@32.6888225,35.5735085,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6a7d09cb364b:0x6777b09b243d61f9!8m2!3d32.689296!4d35.574062!16s%2Fm%2F03cdr0f?entry=ttu',
    'https://www.google.com/maps/place/%D8%A8%D9%8A%D8%AA%D9%8A%D9%86%E2%80%AD/@31.9264805,35.236911,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd50ab568223d:0xbb3731e7319c2bf7!8m2!3d31.926481!4d35.236911!16s%2Fm%2F0462gql?entry=ttu',
    'https://www.google.com/maps/place/%D7%91%D7%99%D7%AA+%D7%99%D7%A8%D7%97,+Israel%E2%80%AD/@32.7123605,35.574715,15z/data=!3m1!4b1!4m6!3m5!1s0x151c6aa2e550bb51:0x15793b7216d34f58!8m2!3d32.712361!4d35.574715!16s%2Fg%2F1tjp4lzh?entry=ttu',
    'https://www.google.com/maps/place/Hasharon+Prison/@32.2408422,34.8848165,17z/data=!3m1!4b1!4m6!3m5!1s0x151d3f358680e1fd:0x15259a504b32fa78!8m2!3d32.2408422!4d34.8848165!16s%2Fg%2F1235qkhn?entry=ttu',
    'https://www.google.com/maps/place/Megiddo+Prison/@32.5708622,35.1898259,17z/data=!3m1!4b1!4m6!3m5!1s0x151daae75db12c8d:0xe77b43d2a0de55ad!8m2!3d32.5708622!4d35.1898259!16s%2Fg%2F1237d864?entry=ttu',
    'https://www.google.com/maps/place/%D7%90%D7%95%D7%A8%D7%98+%D7%91%D7%A0%D7%99%D7%9E%D7%99%D7%A0%D7%94+%D7%97%D7%98%D7%99%D7%91%D7%AA+%D7%94%D7%91%D7%99%D7%A0%D7%99%D7%99%D7%9D%E2%80%AD/@32.5497387,34.9528783,17z/data=!4m10!1m2!2m1!1z15HXmdeqINeh16TXqCDXkNeV16jXmCDXkdeg15nXnteZ16DXlA!3m6!1s0x151d09bcce100d95:0x70d943aea02e51c5!8m2!3d32.5497387!4d34.955067!15sCiXXkdeZ16og16HXpNeoINeQ15XXqNeYINeR16DXmdee15nXoNeUIgOIAQGSAQZzY2hvb2zgAQA!16s%2Fg%2F11fj32k4kr?entry=ttu',
    'https://www.google.com/maps/place/The+Mount+Meron+Field+School/@33.0114872,35.3922459,17z/data=!3m1!4b1!4m6!3m5!1s0x151c2449333574d9:0x456dda74afaa3d47!8m2!3d33.0114872!4d35.3922459!16s%2Fg%2F1vl0bpv1?entry=ttu',
    'https://www.google.com/maps/place/Tel+Regev+cemetery/@32.7672379,35.1243861,17z/data=!3m1!4b1!4m6!3m5!1s0x151db16352cdf831:0x7836dd0d5d97d25c!8m2!3d32.7672379!4d35.1243861!16s%2Fg%2F120r4kml?entry=ttu',
    'https://www.google.com/maps/place/%D8%A8%D9%8A%D8%AA+%D9%81%D9%88%D8%B1%D9%8A%D9%83%E2%80%AD/@32.1754555,35.336484,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce77d387b5c77:0xc22dccc04e65bcb6!8m2!3d32.175456!4d35.336484!16s%2Fm%2F03h0vl4?entry=ttu',
    'https://www.google.com/maps/place/Beit+Zvi+School+for+the+Performing+Arts/@32.0784145,34.8217109,17z/data=!4m6!3m5!1s0x151d4a358657bc15:0xd6d8dd96cd7f02a2!8m2!3d32.0784145!4d34.8217109!16s%2Fm%2F09gf6gq?entry=ttu',
    NA,
    'https://www.google.com/maps/place/Betar+Illit/@31.7019045,35.1121105,14z/data=!3m1!4b1!4m6!3m5!1s0x1502dbfa57125a4f:0x2d45eaaf6ebc1a0b!8m2!3d31.7010023!4d35.1119254!16zL20vMDF2dDli?entry=ttu',
    'https://www.google.com/maps/place/Balata/@32.2072275,35.285928,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce0bed3d4a49f:0x3bfe9f57600a9c56!8m2!3d32.207228!4d35.285928!16zL20vMDJ6NnB2?entry=ttu',
    'https://www.google.com/maps/place/Bnei+Yehuda/@32.79845,35.6900459,16z/data=!3m1!4b1!4m6!3m5!1s0x151c110f29085e7d:0x4b8c8c216257ae9!8m2!3d32.798262!4d35.69107!16s%2Fm%2F0463qtl?entry=ttu',
    "https://www.google.com/maps/place/Giv'at+Yo'av/@32.7980686,35.6820396,16z/data=!3m1!4b1!4m10!1m2!2m1!1z15HXoNeZINeZ15TXldeT15Qg15XXkteR16LXqiDXmdeV15DXkQ!3m6!1s0x151c11112bdb163b:0xac5abafcabc1d572!8m2!3d32.801217!4d35.680911!15sCiXXkdeg15kg15nXlNeV15PXlCDXldeS15HXoteqINeZ15XXkNeRkgEIbG9jYWxpdHngAQA!16s%2Fm%2F04657dj?entry=ttu",
    'https://www.google.com/maps/place/Hazera+Seeds+-+Brurim+Farm/@31.7680463,34.7807623,17z/data=!3m1!4b1!4m6!3m5!1s0x1502be9eb2c1d259:0x3381de29179db720!8m2!3d31.7680463!4d34.7807623!16s%2Fg%2F121vrflb?entry=ttu',
    'https://www.google.com/maps/place/Har+Brakha/@32.1928741,35.260194,15z/data=!3m1!4b1!4m6!3m5!1s0x151cde295d69d2fb:0x1e4b0e54d0574a1a!8m2!3d32.193264!4d35.264885!16s%2Fm%2F026553k?entry=ttu',
    'https://www.google.com/maps/place/%D7%91%D7%AA+%D7%A2%D7%99%D7%9F%E2%80%AD/@31.6575564,35.101987,15z/data=!3m1!4b1!4m6!3m5!1s0x1502dc496c79dbfb:0xca3c0b7b262cc184!8m2!3d31.657653!4d35.101101!16zL20vMDYycXJx?entry=ttu',
    'https://www.google.com/maps/place/Jalud/@32.0695305,35.31549,15z/data=!4m6!3m5!1s0x151cdbcfb15e58bf:0x1891863dbe9a2a7f!8m2!3d32.069531!4d35.31549!16s%2Fm%2F0j3cswc?entry=ttu',
    'https://www.google.com/maps/place/%D8%AC%D8%A7%D9%86%D9%8A%D8%A9%E2%80%AD/@31.9384185,35.123984,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2ca6d51b1b23:0x561968aca762e94b!8m2!3d31.938419!4d35.123984!16s%2Fg%2F11b5z79fq1?entry=ttu',
    'https://www.google.com/maps/place/%D7%92%D7%91%D7%A2%D7%AA+%D7%90%D7%A1%D7%A3%E2%80%AD/@31.9416122,35.2051307,14z/data=!4m10!1m2!2m1!1z15LXkdei16og15DXodejICA!3m6!1s0x151cd510e0f42b65:0x2ec64331066235b3!8m2!3d31.9416122!4d35.2226402!15sCg_XkteR16LXqiDXkNeh16OSAQ9ob3VzaW5nX2NvbXBsZXjgAQA!16s%2Fg%2F11ks1hynlv?entry=ttu',
    'https://www.google.com/maps/place/Givat+Wolfson/@32.7202415,35.0170555,17z/data=!3m1!4b1!4m6!3m5!1s0x151da50d26058c4b:0xe76cce81ef559c2f!8m2!3d32.7202415!4d35.0170555!16s%2Fg%2F121hg34j?entry=ttu',
    'https://www.google.com/maps/place/Givat+Haviva/@32.457665,35.0178723,17z/data=!4m10!1m2!2m1!1z15LXkdei16og15fXkdeZ15HXlA!3m6!1s0x151d0f9430e18aff:0x6858ba9307ca9ffc!8m2!3d32.457665!4d35.020061!15sChPXkteR16LXqiDXl9eR15nXkdeUkgEQZWR1Y2F0aW9uX2NlbnRlcuABAA!16s%2Fg%2F121db_24?entry=ttu',
    "https://www.google.com/maps/place/Giv'at+Yo'av/@32.7980686,35.6820396,16z/data=!3m1!4b1!4m6!3m5!1s0x151c11112bdb163b:0xac5abafcabc1d572!8m2!3d32.801217!4d35.680911!16s%2Fm%2F04657dj?entry=ttu",
    "https://www.google.com/maps/place/Giv'at+Sne+Ya'akov/@32.1779165,35.263443,15z/data=!3m1!4b1!4m6!3m5!1s0x151cde35f7288ef3:0xfddc6c53545ed13!8m2!3d32.177917!4d35.263443!16s%2Fg%2F1ymwn4hz0?entry=ttu",
    'https://www.google.com/maps/place/%D7%92%D7%93%D7%95%D7%AA%E2%80%AD/@33.0186075,35.6171239,16z/data=!3m1!4b1!4m6!3m5!1s0x151c1e30626b5db1:0x7cbbb48e14cb4c3f!8m2!3d33.018208!4d35.619547!16s%2Fm%2F047n7fc?entry=ttu',
    'https://www.google.com/maps/place/al-Judeira/@31.8566045,35.197686,15z/data=!3m1!4b1!4m6!3m5!1s0x1502d59ea2d3b9c7:0xce5d9b43f6d82098!8m2!3d31.856605!4d35.197686!16s%2Fm%2F04cvr6c?entry=ttu',
    'https://www.google.com/maps/place/%D7%92%D7%95%D7%9C%D7%9F+4%E2%80%AD/@32.7069618,35.3157504,14.54z/data=!4m6!3m5!1s0x151c4f460d9f3565:0x43a3b4b10c8d7cb0!8m2!3d32.7108383!4d35.3253767!16s%2Fg%2F11k46jrddh?entry=ttu',
    'https://www.google.com/maps/place/%D8%AC%D9%86%D9%8A%D8%AF%E2%80%AD/@32.2260755,35.218249,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce01c3073a9d7:0x325f6394557650b6!8m2!3d32.226076!4d35.218249!16s%2Fg%2F1tsr5fsr?entry=ttu',
    'https://www.google.com/maps/place/Gonen,+Israel/@33.1241971,35.6457011,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea38c084f0af7:0x1438d35bc64f1cc4!8m2!3d33.1240353!4d35.6461519!16s%2Fm%2F04fznhb?entry=ttu',
    'https://www.google.com/maps/place/%D8%AC%D9%88%D8%A7%D8%B1%D9%8A%D8%B4%E2%80%AD/@32.1023295,35.321847,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdc0ba9c07111:0x33c39ba4e4a29155!8m2!3d32.10233!4d35.321847!16s%2Fm%2F04cw4t5?entry=ttu',
    'https://www.google.com/maps/place/Al+Jib/@31.8509886,35.183103,15z/data=!3m1!4b1!4m6!3m5!1s0x1502d5ba165da79b:0x27d2f47770a1c77a!8m2!3d31.850989!4d35.183103!16s%2Fm%2F02r301p?entry=ttu',
    'https://www.google.com/maps/place/%D8%AC%D9%8A%D8%A8%D9%8A%D8%A7%E2%80%AD/@31.9971395,35.161111,15z/data=!3m1!4b1!4m6!3m5!1s0x151d297b42dbaa3d:0x92d6edbb258937dd!8m2!3d31.99714!4d35.161111!16s%2Fg%2F11b6gnqd_5?entry=ttu',
    'https://www.google.com/maps/place/%D8%AC%D9%84%D8%AC%D9%8A%D9%84%D9%8A%D8%A7%E2%80%AD/@32.0313635,35.223285,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd7dd7e4eb09b:0x1e9734e3c31abd0!8m2!3d32.031364!4d35.223285!16s%2Fg%2F121v6_bh?entry=ttu',
    "https://www.google.com/maps/place/Gitit,+Modi'in+Makabim-Re'ut,+Israel/@31.8861104,35.0109342,17z/data=!3m1!4b1!4m6!3m5!1s0x1502cddb89a546e3:0xd18a70082457b369!8m2!3d31.8861104!4d35.0109342!16s%2Fg%2F1ymss455h?entry=ttu",
    'https://www.google.com/maps/place/Gilgal/@32.0000155,35.444643,15z/data=!3m1!4b1!4m6!3m5!1s0x151cc5981aa999cb:0xabed8852ab9b7d2f!8m2!3d32.000016!4d35.444643!16s%2Fm%2F04gqj40?entry=ttu',
    'https://www.google.com/maps/place/Jalazone/@31.9516115,35.212517,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2a75b6185231:0x142f17ea43302d3b!8m2!3d31.951612!4d35.212517!16s%2Fm%2F03y9ph1?entry=ttu',
    "https://www.google.com/maps/place/Jamma'in/@32.1320796,35.203342,15z/data=!3m1!4b1!4m6!3m5!1s0x151d274e0700adc3:0x9c13f02beae09540!8m2!3d32.13208!4d35.203342!16s%2Fm%2F03d54kf?entry=ttu",
    'https://www.google.com/maps/place/Ganei+Yohanan,+Israel/@31.8580225,34.8402214,15z/data=!3m1!4b1!4m6!3m5!1s0x1502b7f32ef8e49f:0x73efcda8d027d256!8m2!3d31.859924!4d34.840827!16s%2Fm%2F047bjb4?entry=ttu',
    'https://www.google.com/maps/place/Jifna/@31.9634625,35.215092,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd5f7e169aa8f:0x1f1a65e52f925b3!8m2!3d31.963463!4d35.215092!16s%2Fm%2F03y0jhl?entry=ttu',
    'https://www.google.com/maps/place/%D7%92%D7%A9%D7%95%D7%A8%E2%80%AD/@32.817636,35.718622,15z/data=!3m1!4b1!4m6!3m5!1s0x151c1041ae95d595:0x797b255b3a84b041!8m2!3d32.819941!4d35.715708!16s%2Fm%2F03hhc4q?entry=ttu',
    'https://www.google.com/maps/place/Degania+Alef,+Israel/@32.7072299,35.574352,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6a99bebb30f1:0x7fae19ea94aadcf0!8m2!3d32.706938!4d35.575062!16zL20vMDVmc2Nt?entry=ttu',
    'https://www.google.com/maps/place/Degania+Bet,+Israel/@32.6998335,35.57463,16z/data=!3m1!4b1!4m6!3m5!1s0x151c6a8fef844c8d:0xc1daad2efa21eef5!8m2!3d32.69907!4d35.577551!16s%2Fm%2F04f4k_5?entry=ttu',
    'https://www.google.com/maps/place/Dheisha/@31.6935155,35.184562,15z/data=!3m1!4b1!4m6!3m5!1s0x1502d8efb324e2ef:0x19c589f487c0824d!8m2!3d31.693516!4d35.184562!16zL20vMGNfMWhy?entry=ttu',
    'https://www.google.com/maps/place/ad-Dhahiriya/@31.4096375,34.975508,15z/data=!3m1!4b1!4m6!3m5!1s0x1502f6cb1fef813b:0x864f08fa0f052772!8m2!3d31.409638!4d34.975508!16s%2Fm%2F03cq0xl?entry=ttu',
    'https://www.google.com/maps/place/Dolev/@31.9262485,35.133591,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2b5142e1a8f3:0xd863cf7fc32e09de!8m2!3d31.926249!4d35.133591!16s%2Fm%2F04gl1lk?entry=ttu',
    'https://www.google.com/maps/place/%D8%AF%D9%88%D9%85%D8%A7%E2%80%AD/@32.0556365,35.367338,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdb17f2fb921f:0x13ab051f4cc837fc!8m2!3d32.055637!4d35.367338!16s%2Fm%2F047rsth?entry=ttu',
    'https://www.google.com/maps/place/shomron+tourist+and+study+center/@32.2624745,35.185667,17z/data=!3m1!4b1!4m6!3m5!1s0x151d1e4ce66e0bdb:0x995f2cc9c44eef4!8m2!3d32.2624745!4d35.185667!16s%2Fg%2F11g6wdqm0h?entry=ttu',
    'https://www.google.com/maps/place/%D8%AF%D9%88%D8%B1%D8%A7+%D8%A7%D9%84%D9%82%D8%B1%D8%B9%E2%80%AD/@31.9585185,35.225893,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd5ee2ab3bfa3:0x2d30d3f344dafdbb!8m2!3d31.958519!4d35.225893!16s%2Fm%2F0464rl8?entry=ttu',
    'https://www.google.com/maps/place/Deir+Istiya/@32.1307615,35.140136,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2690e4499cc9:0x46b56c7fc2e23397!8m2!3d32.130762!4d35.140136!16s%2Fm%2F04cv8rc?entry=ttu',
    'https://www.google.com/maps/place/Deir+al-Hatab/@32.2168815,35.319464,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce720d5ebe565:0xf1e024a256079482!8m2!3d32.216882!4d35.319464!16s%2Fm%2F047b5vn?entry=ttu',
    'https://www.google.com/maps/place/%D8%AF%D9%8A%D8%B1+%D8%A7%D9%84%D8%B3%D9%88%D8%AF%D8%A7%D9%86%E2%80%AD/@32.0318825,35.148975,15z/data=!3m1!4b1!4m6!3m5!1s0x151d291f62f466b3:0xedfba1fb4daa064f!8m2!3d32.031883!4d35.148975!16s%2Fm%2F04cyv4j?entry=ttu',
    'https://www.google.com/maps/place/%D8%AF%D9%8A%D8%B1+%D8%AC%D8%B1%D9%8A%D8%B1%E2%80%AD/@31.9656115,35.293776,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd6b717f0af67:0xe8cf54bdb1e0d51d!8m2!3d31.965612!4d35.293776!16s%2Fm%2F043lk4v?entry=ttu',
    'https://www.google.com/maps/place/%D8%AF%D9%8A%D8%B1+%D8%AF%D8%A8%D9%88%D8%A7%D9%86%E2%80%AD/@31.9112045,35.268988,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd4fa6d15e9c7:0xcda714b22da85142!8m2!3d31.911205!4d35.268988!16s%2Fm%2F02wcqk1?entry=ttu',
    NA,
    'https://www.google.com/maps/place/Bnei+Dan+St+159,+Tel+Aviv-Yafo,+Israel/@32.0963712,34.7920356,17z/data=!3m1!4b1!4m5!3m4!1s0x151d4be8eaad3f63:0x98b30609e054327e!8m2!3d32.0963712!4d34.7920356?entry=ttu',
    'https://www.google.com/maps/place/Bnei+Dan+St+156,+Tel+Aviv-Yafo,+Israel/@32.096755,34.7937803,17z/data=!3m1!4b1!4m5!3m4!1s0x151d4be87bfaab07:0x8b5eafba9bc581d5!8m2!3d32.096755!4d34.7937803?entry=ttu',
    'https://www.google.com/maps/place/Dafna,+Israel/@33.2300325,35.6385991,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebc6f6759460d:0x33efd074ba38dd7!8m2!3d33.23075!4d35.638268!16s%2Fm%2F03c71zm?entry=ttu',
    'https://www.google.com/maps/place/Sefel%C3%A1,+Israel/@31.7278699,34.936689,11z/data=!3m1!4b1!4m10!1m2!2m1!1z15PXqNeV150g15TXqdek15zXlA!3m6!1s0x1502c4e9bfa07779:0x623893e126955c31!8m2!3d31.7255977!4d34.9286096!15sChPXk9eo15XXnSDXlNep16TXnNeUkgEUYWRtaW5pc3RyYXRpdmVfYXJlYTPgAQA!16zL20vMDJ6Mjh4?entry=ttu',
    'https://www.google.com/maps/place/HaOn/@32.7264595,35.6229476,16z/data=!3m1!4b1!4m6!3m5!1s0x151c14c50302a33d:0xf7dc695aaf2f5f00!8m2!3d32.726748!4d35.622665!16zL20vMGc1Ymx6?entry=ttu',
    'https://www.google.com/maps/place/HaGoshrim,+Israel/@33.2201709,35.6230576,16z/data=!3m1!4b1!4m6!3m5!1s0x151ebcf726fc9241:0x1db36086381ae3b!8m2!3d33.221485!4d35.623028!16s%2Fm%2F03c7hjr?entry=ttu',
    'https://www.google.com/maps/place/%D7%94%D7%99%D7%9C%D7%94,+Israel%E2%80%AD/@33.035967,35.244291,16z/data=!3m1!4b1!4m6!3m5!1s0x151c2ce2508f87ab:0x211e9b948ef415e5!8m2!3d33.036175!4d35.244582!16s%2Fg%2F1vc8lp_g?entry=ttu',
    NA,
    "https://www.google.com/maps/place/Mount+Gerizim/@32.2005551,35.2733333,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce09bbd631073:0x3904ca6c2022b5c6!8m2!3d32.2005556!4d35.2733333!16zL20vMDM3dms1?entry=ttu",
    'https://www.google.com/maps/place/Har+Amasa,+Israel/@31.3432165,35.1011545,17z/data=!3m1!4b1!4m6!3m5!1s0x1502f91772fe1057:0x7de7f6a6a7d8ee6d!8m2!3d31.342717!4d35.101599!16s%2Fm%2F02vq5_v?entry=ttu',
    'https://www.google.com/maps/place/Herzliya,+Israel/@32.1731045,34.8280035,13z/data=!3m1!4b1!4m6!3m5!1s0x151d480dc125e56f:0x46a023cb144ac1c4!8m2!3d32.162413!4d34.844675!16zL20vMDF0bDJx?entry=ttu',
    'https://www.google.com/maps/place/Wadi+el-Khuweikh,+Ein+Mahil,+Israel/@32.7199065,35.349218,15z/data=!3m1!4b1!4m10!1m2!2m1!1z15XXkNeT15kg15DXnCDXoNei150!3m6!1s0x151c4f2a2d0600c5:0x5d2cecfee6198efc!8m2!3d32.719907!4d35.349218!15sChTXldeQ15PXmSDXkNecINeg16LXnZIBDG5laWdoYm9yaG9vZOABAA!16s%2Fg%2F1ttxf4k9?entry=ttu',
    'https://www.google.com/maps/place/Nahal+Yoav/@31.4073442,34.7574287,10z/data=!4m10!1m2!2m1!1z15XXkNeT15kg15DXnCDXoNei150!3m6!1s0x150290e429d81b2f:0xa443ddf63fdd7b02!8m2!3d31.62794!4d34.714341!15sChTXldeQ15PXmSDXkNecINeg16LXnZIBDnNlYXNvbmFsX3JpdmVy4AEA!16s%2Fg%2F120mbhff?entry=ttu',
    'https://www.google.com/maps/place/%D7%95%D7%A8%D7%93%D7%99%D7%9D+%D7%9E%D7%95%D7%A1%D7%93+%D7%97%D7%99%D7%A0%D7%95%D7%9B%D7%99%E2%80%AD/@32.2187629,34.8560346,17z/data=!3m1!4b1!4m6!3m5!1s0x151d476490f8a8af:0xb9d56d613fb2dd0!8m2!3d32.2187629!4d34.8560346!16s%2Fg%2F1tqd32td?entry=ttu',
    'https://www.google.com/maps/place/Vered+Yeriho/@31.826984,35.4322771,16z/data=!3m1!4b1!4m6!3m5!1s0x151ccd2ad19dbf01:0x4236e5c837966efd!8m2!3d31.826192!4d35.432139!16s%2Fm%2F03m9j3p?entry=ttu',
    'https://www.google.com/maps/place/Al+Zubeidat/@32.1759163,35.5337419,15z/data=!3m1!4b1!4m6!3m5!1s0x151ceb6e0fc40b41:0xb43d6587bbde5157!8m2!3d32.177201!4d35.5356866!16s%2Fm%2F04dzr6y?entry=ttu',
    'https://www.google.com/maps/place/Zawata/@32.2459715,35.227112,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce0326734551b:0xf6e1d6c90e26129b!8m2!3d32.245972!4d35.227112!16s%2Fm%2F04cy6cx?entry=ttu',
    'https://www.google.com/maps/place/Zimrat,+Israel/@31.4472805,34.553848,16z/data=!3m1!4b1!4m6!3m5!1s0x15027fccc8e68585:0x2968c1e08cf8676!8m2!3d31.447853!4d34.552288!16s%2Fm%2F04gps10?entry=ttu',
    'https://www.google.com/maps/place/%D8%AD%D8%A7%D8%B1%D8%B3%E2%80%AD/@32.1134145,35.143716,15z/data=!3m1!4b1!4m6!3m5!1s0x151d26617c7f0b79:0x31f5be572db92e8e!8m2!3d32.113415!4d35.143716!16s%2Fm%2F04ct61l?entry=ttu',
    'https://www.google.com/maps/place/%D7%97%D7%93+%D7%A0%D7%A1%E2%80%AD/@32.9321431,35.6398625,15z/data=!3m1!4b1!4m6!3m5!1s0x151c19432741ec23:0x7727988d3fecf6d5!8m2!3d32.927514!4d35.641408!16s%2Fm%2F046428q?entry=ttu',
    'https://www.google.com/maps/place/Hadera+West/@32.4382307,34.8993371,17z/data=!3m1!4b1!4m6!3m5!1s0x151d12fb917bdbc7:0xb8a24b6d3159c5a!8m2!3d32.4382307!4d34.8993371!16zL20vMGZ6NGZy?entry=ttu',
    'https://www.google.com/maps/place/Hadera,+Israel/@32.44064,34.9238166,13z/data=!3m1!4b1!4m6!3m5!1s0x151d124252d125bb:0x3abe13857b8fe43d!8m2!3d32.4340458!4d34.9196518!16zL20vMDF2dGJm?entry=ttu',
    'https://www.google.com/maps/place/%D8%AD%D9%88%D8%A7%D8%B1%D8%A9%E2%80%AD/@32.1513125,35.256799,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdf02e86eaac7:0xf2f9d86667fa8db7!8m2!3d32.151313!4d35.256799!16s%2Fm%2F03b_c4g?entry=ttu',
    'https://www.google.com/maps/place/Hulata,+Israel/@33.0513845,35.610094,16z/data=!3m1!4b1!4m6!3m5!1s0x151ea06003520d0b:0x232a652eedb0291!8m2!3d33.052851!4d35.609244!16s%2Fm%2F04g29cy?entry=ttu',
    'https://www.google.com/maps/place/Yotvata+Hai-Bar+Nature+Reserve/@29.8461746,35.0294647,17z/data=!3m1!4b1!4m6!3m5!1s0x1500501088ca25a7:0x4edcf5087bb5c9e2!8m2!3d29.8461746!4d35.0294647!16s%2Fm%2F027hjxx?entry=ttu',
    'https://www.google.com/maps/place/%D8%AE%D8%B1%D8%A8%D8%A9+%D8%A3%D8%A8%D9%88+%D9%81%D9%84%D8%A7%D8%AD%E2%80%AD/@32.0136305,35.302612,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd74cd56c9a67:0xb2191be30d4131b0!8m2!3d32.013631!4d35.302612!16s%2Fm%2F04csx5_?entry=ttu',
    'https://www.google.com/maps/place/%D8%AE%D8%B1%D8%A8%D8%A9+%D8%A7%D9%84%D8%B7%D9%8A%D8%B1%D8%A9%E2%80%AD/@31.9107905,35.18164,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2ad1fc5bba19:0x6a688a16ec5ef768!8m2!3d31.910791!4d35.18164!16s%2Fg%2F1vc8lq13?entry=ttu',
    'https://www.google.com/maps/place/%D8%AE%D8%B1%D8%A8%D8%A9+%D8%B5%D8%A7%D9%81%D8%A7%E2%80%AD/@31.6426165,35.100808,15z/data=!3m1!4b1!4m6!3m5!1s0x1502ddb769f21df1:0x52bd93cc87070547!8m2!3d31.642617!4d35.100808!16s%2Fm%2F04cvygz?entry=ttu',
    'https://www.google.com/maps/place/%D8%AE%D8%B1%D8%A8%D8%A9+%D9%82%D9%8A%D8%B3%E2%80%AD/@32.0631465,35.177798,15z/data=!3m1!4b1!4m6!3m5!1s0x151d28791f9d08a3:0x7a4a27b84d5d585f!8m2!3d32.063147!4d35.177798!16s%2Fg%2F122gwd4q?entry=ttu',
    'https://www.google.com/maps/place/Halet+al-Fule/@32.1559635,35.470916,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce98e9deccd7d:0x2939b3a2c11f0293!8m2!3d32.155964!4d35.470916!16s%2Fg%2F1xgc0xzy?entry=ttu',
    'https://www.google.com/maps/place/%D7%97%D7%9E%D7%93%D7%AA%E2%80%AD/@32.2516965,35.5269735,16z/data=!3m1!4b1!4m6!3m5!1s0x151ced74ad9a581b:0x2db7d96d75f0461f!8m2!3d32.252028!4d35.526843!16zL20vMGd0a3px?entry=ttu',
    'https://www.google.com/maps/place/%D7%97%D7%9E%D7%A8%D7%94%E2%80%AD/@32.1992761,35.4355605,15z/data=!3m1!4b1!4m6!3m5!1s0x151ce91f091f143b:0x722fae62a70e60a8!8m2!3d32.199688!4d35.43735!16s%2Fm%2F04css_k?entry=ttu',
    'https://www.google.com/maps/place/%D7%97%D7%9E%D7%AA+%D7%92%D7%93%D7%A8%E2%80%AD/@32.6833581,35.6647332,15z/data=!3m1!4b1!4m6!3m5!1s0x151c6cbc94f14eef:0xae6972668252a252!8m2!3d32.6833586!4d35.6647332!16zL20vMGJoMGpu?entry=ttu',
    'https://www.google.com/maps/place/Haspin/@32.8452054,35.793082,15z/data=!3m1!4b1!4m6!3m5!1s0x151c05a666ad49cd:0x7cd2a6a0d99979a7!8m2!3d32.8456305!4d35.7929907!16s%2Fm%2F0462d32?entry=ttu',
    "https://www.google.com/maps/place/Derech+Hefer+148,+Beit+Yitshak+Sha'ar+Hefer,+Israel/@32.3417119,34.8984018,17z/data=!3m1!4b1!4m5!3m4!1s0x151d150fa40285c5:0x2d3b8a7d536099e6!8m2!3d32.3417119!4d34.8984018?entry=ttu",
    'https://www.google.com/maps/place/Harmala/@31.6612464,35.2218201,16z/data=!3m1!4b1!4m6!3m5!1s0x150327572c6d47e1:0xfc6381a428393224!8m2!3d31.6610022!4d35.2219894!16s%2Fg%2F1trl7hkb?entry=ttu',
    'https://www.google.com/maps/place/Tuba-Zangariyye,+Israel/@32.964856,35.589887,15z/data=!3m1!4b1!4m6!3m5!1s0x151c1f36a30cfa87:0xf190a021932c1f65!8m2!3d32.96621!4d35.592018!16s%2Fm%2F02qhtkr?entry=ttu',
    'https://www.google.com/maps/place/%D8%A7%D9%84%D8%B7%D9%8A%D8%A8%D8%A9%E2%80%AD/@31.9581135,35.298483,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd6b20b6e7627:0xb1c21e6d63f548d0!8m2!3d31.958114!4d35.298483!16s%2Fm%2F027j70z?entry=ttu',
    'https://www.google.com/maps/place/%D7%98%D7%9C%D7%9E%D7%95%D7%9F%E2%80%AD/@31.9394746,35.133131,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2b5de8694bf1:0xc6c82f2cf8fe8850!8m2!3d31.939475!4d35.133131!16zL20vMGJ3cnI3?entry=ttu',
    'https://www.google.com/maps/place/%D9%8A%D8%A7%D9%86%D9%88%D9%86%E2%80%AD/@32.1453345,35.355599,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdd0da591390d:0xf0edf1c8fe410d90!8m2!3d32.145335!4d35.355599!16s%2Fm%2F04cwhrx?entry=ttu',
    'https://www.google.com/maps/place/%D9%8A%D8%A7%D8%B3%D9%88%D9%81%E2%80%AD/@32.1093705,35.239715,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd8c11fa449f1:0xf733a0b9115edfa8!8m2!3d32.109371!4d35.239715!16s%2Fm%2F04cxr3r?entry=ttu',
    'https://www.google.com/maps/place/%D9%8A%D8%A8%D8%B1%D9%88%D8%AF%E2%80%AD/@31.9752865,35.243822,15z/data=!3m1!4b1!4m6!3m5!1s0x151cd66e20661143:0xa75ab84b72b20795!8m2!3d31.975287!4d35.243822!16s%2Fm%2F05nzw_1?entry=ttu',
    'https://www.google.com/maps/place/Yehuda,+Ofra/@31.9512505,35.2660476,17z/data=!3m1!4b1!4m6!3m5!1s0x151cd42ef38d9a79:0xdfbdae3895520b35!8m2!3d31.9512505!4d35.2660476!16s%2Fg%2F1ymt1f34p?entry=ttu',
    'https://www.google.com/maps/place/Yuval,+Israel/@33.2432995,35.5989725,14z/data=!3m1!4b1!4m6!3m5!1s0x151ebdba5ef6464b:0x7d6b769db3f22234!8m2!3d33.246577!4d35.597063!16s%2Fm%2F03wfgw_?entry=ttu',
    'https://www.google.com/maps/place/%D7%99%D7%95%D7%A0%D7%AA%D7%9F%E2%80%AD/@32.93903,35.794893,17z/data=!3m1!4b1!4m6!3m5!1s0x151c024e21afb387:0xcb062c969a55ad1a!8m2!3d32.93966!4d35.795468!16s%2Fm%2F03nvpws?entry=ttu',
    'https://www.google.com/maps/place/%D7%99%D7%99%D7%98%22%D7%91%E2%80%AD/@31.948263,35.4229755,16z/data=!3m1!4b1!4m6!3m5!1s0x151cce4d43e25c53:0x1ee8866ad43f8e03!8m2!3d31.947215!4d35.422375!16s%2Fm%2F04f7c1t?entry=ttu',
    'https://www.google.com/maps/place/Yakhini,+Israel/@31.4827316,34.6011976,15z/data=!3m1!4b1!4m6!3m5!1s0x1502872c9fa7a76b:0xf6591f488ea7d689!8m2!3d31.482205!4d34.598634!16zL20vMDdmNGY4?entry=ttu',
    'https://www.google.com/maps/place/Mar+Morto/@31.5371797,35.4902883,10z/data=!3m1!4b1!4m6!3m5!1s0x15033c2eaf9fbba1:0xf38cff48a0c15882!8m2!3d31.5590287!4d35.4731895!16zL20vMDJjbnA?entry=ttu',
    "https://www.google.com/maps/place/Yesud+HaMa'ala,+Israel/@33.0609239,35.6046765,13z/data=!3m1!4b1!4m6!3m5!1s0x151ea06cc20f3477:0xcaac36c76aca5eef!8m2!3d33.05358!4d35.589013!16zL20vMGc2ZHNu?entry=ttu",
    NA,
    'https://www.google.com/maps/place/Carmel+Forest/@32.7161091,34.9965169,14z/data=!4m10!1m2!2m1!1z15nXoteo15XXqiDXlNeb16jXntec!3m6!1s0x151da53cfaa5d3c3:0x7cdf6e3ce402e925!8m2!3d32.737985!4d35.007888!15sChXXmdei16jXldeqINeU15vXqNee15ySAQ9uYXRpb25hbF9mb3Jlc3TgAQA!16s%2Fg%2F11c1wwlhvj?entry=ttu',
    'https://www.google.com/maps/place/%D7%99%D7%A4%D7%99%D7%AA%E2%80%AD/@32.0631289,35.4747324,16z/data=!3m1!4b1!4m6!3m5!1s0x151cc3e30c44258d:0xb2c51eb73428c3ff!8m2!3d32.062687!4d35.474317!16s%2Fm%2F04ghnvn?entry=ttu',
    'https://www.google.com/maps/place/%D7%99%D7%A6%D7%94%D7%A8%E2%80%AD/@32.1649466,35.2350119,15z/data=!3m1!4b1!4m6!3m5!1s0x151cdf9f9a573403:0x8935573c0e7a880a!8m2!3d32.168529!4d35.235944!16zL20vMGY0bjh0?entry=ttu',
    'https://www.google.com/maps/place/%D7%99%D7%A7%D7%99%D7%A8%E2%80%AD/@32.1482875,35.1100115,15z/data=!3m1!4b1!4m6!3m5!1s0x151d2417487a6607:0xa20f264f71adee38!8m2!3d32.149831!4d35.114631!16zL20vMGM0MzM5?entry=ttu',
    'https://www.google.com/maps/place/Atarot+industrial+zone/@31.85502,35.218686,11z/data=!3m1!4b1!4m6!3m5!1s0x15032a8094bbd053:0xd43580323d95308b!8m2!3d31.85502!4d35.218686!16s%2Fg%2F1yw9kvx_3?entry=ttu'
  ) %>% as.data.frame() 

colnames(missing_coords) = 'link'


missing_coords = missing_coords %>%
  mutate(link = str_extract_all(link, "@(.*)"),
         latitude = str_extract_all(link, "@(.*?),", ) %>% 
           sub(pattern = '@', replacement = '') %>%
           sub(pattern  = ',', replacement = '') %>%
           as.numeric(),
         longitude = str_extract_all(link, ",(.*?)z") %>%
           substr(start = 2, stop = nchar(link)) %>%
           str_replace(pattern = ",.*$", replacement = "") %>%
           as.numeric()) %>%
  select(-link)


missing = missing %>% cbind(missing_coords)
rm(missing_coords)

data = data %>%
  merge(missing, by = 'location', all.x = T) %>%
  mutate(
    lat = coalesce(latitude.x, latitude.y),
    long = coalesce(longitude.x,longitude.y)) %>%
  select(c(location, lat, long))



# NEW ROWS BASED ON ELECTORAL LOCALITIES
locations <- c(
  "כנרת (קבוצה)", "כנרת (מושבה)", "חירות", "אשדות יעקב  (מאוח",
  "אשדות יעקב  (איחו", "יקנעם עילית", "יקנעם (מושבה)", "ניר דוד (תל עמל)",
  "מעין צבי", "דן", "גת (קיבוץ)", "גלעד (אבן יצחק)", "מעין ברוך",
  "אבו סנאן", "בענה", "ג'ולס", "טייבה (בעמק)", "כאוכב אבו אל-היג'",
  "כפר מצר", "ניין", "פקיעין (בוקייעה)", "ריחאניה", "כפר חושן", "ג'ת",
  "בית חירות", "שבלי - אום אל-גנם", "סואעד (חמרייה)", "הוזייל (שבט)",
  "עוקבי (בנו עוקבה)", "אבו עבדון (שבט)", "אסד (שבט)", "אבו רוקייק (שבט)",
  "אעצם (שבט)", "קודייראת א-צאנע(ש", "אטרש (שבט)", "אבו רובייעה (שבט)",
  "אבו ג'ווייעד (שבט", "אבו קורינאת (שבט)", "עטאוונה (שבט)",
  "תראבין א-צאנע (שב", "קוואעין (שבט)", "ג'נאביב (שבט)",
  "כעביה-טבאש-חג'אג'", "ח'ואלד (שבט)", "רומת הייב", "נצאצרה (שבט)",
  "כרם יבנה (ישיבה)", "כפר רוזנואלד (זרע", "הוואשלה (שבט)", "סייד (שבט)",
  "מודיעין-מכבים-רעו", "קבועה (שבט)", "שני", "ג'דיידה-מכר", "כסרא-סמיע",
  "כדיתה", "אבו קרינאת (יישוב", "תראבין א-צאנע(ישו", "קצר א-סר",
  "מחנה תל נוף", "מחנה מרים", "מחנה יפה", "מחנה יוכבד", "מחנה עדי",
  "מחנה טלי", "בן שמן (מושב)", "עופרה", "נעמ\"ה", "בוקעאתא", "באקה אל-גרביה"
)

# Criando o dataframe
df <- data.frame(
  location = locations,
  lat = rep(NA, length(locations)),
  long = rep(NA, length(locations))
)


df[1,2:3] = c(32.7133725,35.5614085)
df[2,2:3] = c(32.7396056,35.5578436)
df[3,2:3] = c(32.2403081,34.91468)
df[4,2:3] = c(32.6627285,35.5835955)
df[5,2:3] = c(32.6580961,35.5798166)
df[6,2:3] = c(32.655368,35.0884011)
df[7,2:3] = c(32.6571359,35.1153181)
df[8,2:3] = c(32.503684,35.4575585)
df[9,2:3] = c(32.5668855,34.940559)
df[10,2:3] = c(NA,NA)
df[11,2:3] = c(31.627332,34.7921815)
df[12,2:3] = c(32.557258,35.0756085)
df[13,2:3] = c(33.240483,35.6073735)
df[14,2:3] = c(32.9667965,35.1556505)
df[15,2:3] = c(32.9293434,35.2790699)
df[16,2:3] = c(32.9413535,35.1803245)
df[17,2:3] = c(32.6030136,35.4457815)
df[18,2:3] = c(32.829475,35.2509515)
df[19,2:3] = c(32.6433485,35.420842)
df[20,2:3] = c(32.63478,35.3526654)
df[21,2:3] = c(32.976773,35.323924)
df[22,2:3] = c(33.0499625,35.4869756)
df[23,2:3] = c(33.0106885,35.438397)
df[24,2:3] = c(32.40044,35.0299183)
df[25,2:3] = c(32.3794988,34.8705486)
df[26,2:3] = c(32.6841665,35.396944)
df[27,2:3] = c(32.7669035,35.168324)
df[28,2:3] = c(31.4177383,34.7597942)
df[29,2:3] = c(31.2832105,34.930794)
df[30,2:3] = c(31.3013965,34.841655)
df[31,2:3] = c(31.3321635,34.860437)
df[32,2:3] = c(31.2595395,34.863897)
df[33,2:3] = c(31.2958155,34.903102)
df[34,2:3] = c(31.2931065,35.10564)
df[35,2:3] = c(31.2641606,34.948716)
df[36,2:3] = c(31.2308106,35.064209)
df[37,2:3] = c(31.1806625,35.072843)
df[38,2:3] = c(31.2138516,35.084855)
df[39,2:3] = c(31.3018816,34.926949)
df[40,2:3] = c(31.3445965,34.7375071)
df[41,2:3] = c(31.3233025,35.026031)
df[42,2:3] = c(31.2871555,35.108132)
df[43,2:3] = c(32.7480155,35.171925)
df[44,2:3] = c(32.7588259,35.15831)
df[45,2:3] = c(32.778279,35.3048614)
df[46,2:3] = c(31.2470325,35.032332)
df[47,2:3] = c(31.8199341,34.7226127)
df[48,2:3] = c(33.1002685,35.2886345)
df[49,2:3] = c(31.0922265,34.979069)
df[50,2:3] = c(31.2817185,34.916298)
df[51,2:3] = c(31.8952745,34.98807)
df[52,2:3] = c(31.2250865,35.214754)
df[53,2:3] = c(NA,NA)
df[54,2:3] = c(32.931315,35.1422211)
df[55,2:3] = c(32.9717,35.2954015)
df[56,2:3] = c(33.0059535,35.468309)
df[57,2:3] = c(31.1031145,34.952003)
df[58,2:3] = c(31.1669285,34.820923)
df[59,2:3] = c(31.0833285,34.979579)
df[60,2:3] = c(31.829526,34.8014565)
df[61,2:3] = c(NA,NA)
df[62,2:3] = c(NA,NA)
df[63,2:3] = c(29.952075,34.934344)
df[64,2:3] = c(33.2253981,35.5495681)
df[65,2:3] = c(NA,NA)
df[66,2:3] = c(31.9590185,34.927831)
df[67,2:3] = c(31.9533085,35.2620901)
df[68,2:3] = c(31.9070159,35.4676824)
df[69,2:3] = c(33.2026567,35.7767723)
df[70,2:3] = c(32.4191113,35.0357895)


all_cities_coordinates = rbind(data,df)

write.csv(all_cities_coordinates, 'raw/Israel/all_cities_coordinates.csv')
