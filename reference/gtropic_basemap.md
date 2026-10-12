# Natural Earth basemaps cached for the current R session

Countries use the locally installed Natural Earth data package. First
use of an administrative-level-1 layer requires the optional, non-CRAN
`rnaturalearthhires` package. No layer is downloaded by this function.

## Usage

``` r
gtropic_basemap(scale = "medium", what = c("countries", "admin1", "none"))
```

## Arguments

- scale:

  Country resolution: `"small"` (110) or `"medium"` (50). Large (10)
  country layers are deliberately unsupported. Ignored for admin1, which
  uses the resolution supplied by
  [`rnaturalearth::ne_states()`](https://docs.ropensci.org/rnaturalearth/reference/ne_states.html).

- what:

  One of `"countries"`, `"admin1"`, or `"none"`.

## Value

An sf object, or NULL for `what = "none"`. Layers are cached by
resolution and type until the R session ends.

## Examples

``` r
# \donttest{
gtropic_basemap(what = "none")
#> NULL
if (requireNamespace("rnaturalearth", quietly = TRUE) &&
  requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  gtropic_basemap()
}
#> Simple feature collection with 242 features and 168 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -180 ymin: -89.99893 xmax: 180 ymax: 83.59961
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>         featurecla scalerank labelrank                     sovereignt sov_a3
#> 1  Admin-0 country         1         3                       Zimbabwe    ZWE
#> 2  Admin-0 country         1         3                         Zambia    ZMB
#> 3  Admin-0 country         1         3                          Yemen    YEM
#> 4  Admin-0 country         3         2                        Vietnam    VNM
#> 5  Admin-0 country         5         3                      Venezuela    VEN
#> 6  Admin-0 country         6         6                        Vatican    VAT
#> 7  Admin-0 country         1         4                        Vanuatu    VUT
#> 8  Admin-0 country         1         3                     Uzbekistan    UZB
#> 9  Admin-0 country         1         4                        Uruguay    URY
#> 10 Admin-0 country         3         6 Federated States of Micronesia    FSM
#>    adm0_dif level              type tlc                          admin adm0_a3
#> 1         0     2 Sovereign country   1                       Zimbabwe     ZWE
#> 2         0     2 Sovereign country   1                         Zambia     ZMB
#> 3         0     2 Sovereign country   1                          Yemen     YEM
#> 4         0     2 Sovereign country   1                        Vietnam     VNM
#> 5         0     2 Sovereign country   1                      Venezuela     VEN
#> 6         0     2 Sovereign country   1                        Vatican     VAT
#> 7         0     2 Sovereign country   1                        Vanuatu     VUT
#> 8         0     2 Sovereign country   1                     Uzbekistan     UZB
#> 9         0     2 Sovereign country   1                        Uruguay     URY
#> 10        0     2 Sovereign country   1 Federated States of Micronesia     FSM
#>    geou_dif                        geounit gu_a3 su_dif
#> 1         0                       Zimbabwe   ZWE      0
#> 2         0                         Zambia   ZMB      0
#> 3         0                          Yemen   YEM      0
#> 4         0                        Vietnam   VNM      0
#> 5         0                      Venezuela   VEN      0
#> 6         0                        Vatican   VAT      0
#> 7         0                        Vanuatu   VUT      0
#> 8         0                     Uzbekistan   UZB      0
#> 9         0                        Uruguay   URY      0
#> 10        0 Federated States of Micronesia   FSM      0
#>                           subunit su_a3 brk_diff       name
#> 1                        Zimbabwe   ZWE        0   Zimbabwe
#> 2                          Zambia   ZMB        0     Zambia
#> 3                           Yemen   YEM        0      Yemen
#> 4                         Vietnam   VNM        0    Vietnam
#> 5                       Venezuela   VEN        0  Venezuela
#> 6                         Vatican   VAT        0    Vatican
#> 7                         Vanuatu   VUT        0    Vanuatu
#> 8                      Uzbekistan   UZB        0 Uzbekistan
#> 9                         Uruguay   URY        0    Uruguay
#> 10 Federated States of Micronesia   FSM        0 Micronesia
#>                         name_long brk_a3   brk_name brk_group abbrev postal
#> 1                        Zimbabwe    ZWE   Zimbabwe      <NA>  Zimb.     ZW
#> 2                          Zambia    ZMB     Zambia      <NA> Zambia     ZM
#> 3                           Yemen    YEM      Yemen      <NA>   Yem.     YE
#> 4                         Vietnam    VNM    Vietnam      <NA>  Viet.     VN
#> 5                       Venezuela    VEN  Venezuela      <NA>   Ven.     VE
#> 6                         Vatican    VAT    Vatican      <NA>   Vat.      V
#> 7                         Vanuatu    VUT    Vanuatu      <NA>   Van.     VU
#> 8                      Uzbekistan    UZB Uzbekistan      <NA>   Uzb.     UZ
#> 9                         Uruguay    URY    Uruguay      <NA>   Ury.     UY
#> 10 Federated States of Micronesia    FSM Micronesia      <NA> F.S.M.    FSM
#>                           formal_en                          formal_fr
#> 1              Republic of Zimbabwe                               <NA>
#> 2                Republic of Zambia                               <NA>
#> 3                 Republic of Yemen                               <NA>
#> 4     Socialist Republic of Vietnam                               <NA>
#> 5  Bolivarian Republic of Venezuela República Bolivariana de Venezuela
#> 6         State of the Vatican City                               <NA>
#> 7               Republic of Vanuatu                               <NA>
#> 8            Republic of Uzbekistan                               <NA>
#> 9      Oriental Republic of Uruguay                               <NA>
#> 10   Federated States of Micronesia                               <NA>
#>                         name_ciawf note_adm0 note_brk
#> 1                         Zimbabwe      <NA>     <NA>
#> 2                           Zambia      <NA>     <NA>
#> 3                            Yemen      <NA>     <NA>
#> 4                          Vietnam      <NA>     <NA>
#> 5                        Venezuela      <NA>     <NA>
#> 6          Holy See (Vatican City)      <NA>     <NA>
#> 7                          Vanuatu      <NA>     <NA>
#> 8                       Uzbekistan      <NA>     <NA>
#> 9                          Uruguay      <NA>     <NA>
#> 10 Micronesia, Federated States of      <NA>     <NA>
#>                          name_sort name_alt mapcolor7 mapcolor8 mapcolor9
#> 1                         Zimbabwe     <NA>         1         5         3
#> 2                           Zambia     <NA>         5         8         5
#> 3                      Yemen, Rep.     <NA>         5         3         3
#> 4                          Vietnam     <NA>         5         6         5
#> 5                    Venezuela, RB     <NA>         1         3         1
#> 6               Vatican (Holy See) Holy See         1         3         4
#> 7                          Vanuatu     <NA>         6         3         7
#> 8                       Uzbekistan     <NA>         2         3         5
#> 9                          Uruguay     <NA>         1         2         2
#> 10 Micronesia, Federated States of     <NA>         5         2         4
#>    mapcolor13  pop_est pop_rank pop_year gdp_md gdp_year
#> 1           9 14645468       14     2019  21440     2019
#> 2          13 17861030       14     2019  23309     2019
#> 3          11 29161922       15     2019  22581     2019
#> 4           4 96462106       16     2019 261921     2019
#> 5           4 28515829       15     2019 482359     2014
#> 6           2      825        2     2019    -99     2019
#> 7           3   299882       10     2019    934     2019
#> 8           4 33580650       15     2019  57921     2019
#> 9          10  3461734       12     2019  56045     2019
#> 10         13   113815        9     2019    401     2018
#>                       economy              income_grp fips_10 iso_a2 iso_a2_eh
#> 1     5. Emerging region: G20           5. Low income      ZI     ZW        ZW
#> 2   7. Least developed region  4. Lower middle income      ZA     ZM        ZM
#> 3   7. Least developed region  4. Lower middle income      YM     YE        YE
#> 4     5. Emerging region: G20  4. Lower middle income      VM     VN        VN
#> 5     5. Emerging region: G20  3. Upper middle income      VE     VE        VE
#> 6  2. Developed region: nonG7 2. High income: nonOECD      VT     VA        VA
#> 7   7. Least developed region  4. Lower middle income      NH     VU        VU
#> 8        6. Developing region  4. Lower middle income      UZ     UZ        UZ
#> 9     5. Emerging region: G20  3. Upper middle income      UY     UY        UY
#> 10       6. Developing region  4. Lower middle income      FM     FM        FM
#>    iso_a3 iso_a3_eh iso_n3 iso_n3_eh un_a3 wb_a2 wb_a3   woe_id woe_id_eh
#> 1     ZWE       ZWE    716       716   716    ZW   ZWE 23425004  23425004
#> 2     ZMB       ZMB    894       894   894    ZM   ZMB 23425003  23425003
#> 3     YEM       YEM    887       887   887    RY   YEM 23425002  23425002
#> 4     VNM       VNM    704       704   704    VN   VNM 23424984  23424984
#> 5     VEN       VEN    862       862   862    VE   VEN 23424982  23424982
#> 6     VAT       VAT    336       336   336   -99   -99 23424986  23424986
#> 7     VUT       VUT    548       548   548    VU   VUT 23424907  23424907
#> 8     UZB       UZB    860       860   860    UZ   UZB 23424980  23424980
#> 9     URY       URY    858       858   858    UY   URY 23424979  23424979
#> 10    FSM       FSM    583       583   583    FM   FSM 23424815  23424815
#>                      woe_note adm0_iso adm0_diff adm0_tlc adm0_a3_us adm0_a3_fr
#> 1  Exact WOE match as country      ZWE      <NA>      ZWE        ZWE        ZWE
#> 2  Exact WOE match as country      ZMB      <NA>      ZMB        ZMB        ZMB
#> 3  Exact WOE match as country      YEM      <NA>      YEM        YEM        YEM
#> 4  Exact WOE match as country      VNM      <NA>      VNM        VNM        VNM
#> 5  Exact WOE match as country      VEN      <NA>      VEN        VEN        VEN
#> 6  Exact WOE match as country      VAT      <NA>      VAT        VAT        VAT
#> 7  Exact WOE match as country      VUT      <NA>      VUT        VUT        VUT
#> 8  Exact WOE match as country      UZB      <NA>      UZB        UZB        UZB
#> 9  Exact WOE match as country      URY      <NA>      URY        URY        URY
#> 10 Exact WOE match as country      FSM      <NA>      FSM        FSM        FSM
#>    adm0_a3_ru adm0_a3_es adm0_a3_cn adm0_a3_tw adm0_a3_in adm0_a3_np adm0_a3_pk
#> 1         ZWE        ZWE        ZWE        ZWE        ZWE        ZWE        ZWE
#> 2         ZMB        ZMB        ZMB        ZMB        ZMB        ZMB        ZMB
#> 3         YEM        YEM        YEM        YEM        YEM        YEM        YEM
#> 4         VNM        VNM        VNM        VNM        VNM        VNM        VNM
#> 5         VEN        VEN        VEN        VEN        VEN        VEN        VEN
#> 6         VAT        VAT        VAT        VAT        VAT        VAT        VAT
#> 7         VUT        VUT        VUT        VUT        VUT        VUT        VUT
#> 8         UZB        UZB        UZB        UZB        UZB        UZB        UZB
#> 9         URY        URY        URY        URY        URY        URY        URY
#> 10        FSM        FSM        FSM        FSM        FSM        FSM        FSM
#>    adm0_a3_de adm0_a3_gb adm0_a3_br adm0_a3_il adm0_a3_ps adm0_a3_sa adm0_a3_eg
#> 1         ZWE        ZWE        ZWE        ZWE        ZWE        ZWE        ZWE
#> 2         ZMB        ZMB        ZMB        ZMB        ZMB        ZMB        ZMB
#> 3         YEM        YEM        YEM        YEM        YEM        YEM        YEM
#> 4         VNM        VNM        VNM        VNM        VNM        VNM        VNM
#> 5         VEN        VEN        VEN        VEN        VEN        VEN        VEN
#> 6         VAT        VAT        VAT        VAT        VAT        VAT        VAT
#> 7         VUT        VUT        VUT        VUT        VUT        VUT        VUT
#> 8         UZB        UZB        UZB        UZB        UZB        UZB        UZB
#> 9         URY        URY        URY        URY        URY        URY        URY
#> 10        FSM        FSM        FSM        FSM        FSM        FSM        FSM
#>    adm0_a3_ma adm0_a3_pt adm0_a3_ar adm0_a3_jp adm0_a3_ko adm0_a3_vn adm0_a3_tr
#> 1         ZWE        ZWE        ZWE        ZWE        ZWE        ZWE        ZWE
#> 2         ZMB        ZMB        ZMB        ZMB        ZMB        ZMB        ZMB
#> 3         YEM        YEM        YEM        YEM        YEM        YEM        YEM
#> 4         VNM        VNM        VNM        VNM        VNM        VNM        VNM
#> 5         VEN        VEN        VEN        VEN        VEN        VEN        VEN
#> 6         VAT        VAT        VAT        VAT        VAT        VAT        VAT
#> 7         VUT        VUT        VUT        VUT        VUT        VUT        VUT
#> 8         UZB        UZB        UZB        UZB        UZB        UZB        UZB
#> 9         URY        URY        URY        URY        URY        URY        URY
#> 10        FSM        FSM        FSM        FSM        FSM        FSM        FSM
#>    adm0_a3_id adm0_a3_pl adm0_a3_gr adm0_a3_it adm0_a3_nl adm0_a3_se adm0_a3_bd
#> 1         ZWE        ZWE        ZWE        ZWE        ZWE        ZWE        ZWE
#> 2         ZMB        ZMB        ZMB        ZMB        ZMB        ZMB        ZMB
#> 3         YEM        YEM        YEM        YEM        YEM        YEM        YEM
#> 4         VNM        VNM        VNM        VNM        VNM        VNM        VNM
#> 5         VEN        VEN        VEN        VEN        VEN        VEN        VEN
#> 6         VAT        VAT        VAT        VAT        VAT        VAT        VAT
#> 7         VUT        VUT        VUT        VUT        VUT        VUT        VUT
#> 8         UZB        UZB        UZB        UZB        UZB        UZB        UZB
#> 9         URY        URY        URY        URY        URY        URY        URY
#> 10        FSM        FSM        FSM        FSM        FSM        FSM        FSM
#>    adm0_a3_ua adm0_a3_un adm0_a3_wb     continent region_un          subregion
#> 1         ZWE        -99        -99        Africa    Africa     Eastern Africa
#> 2         ZMB        -99        -99        Africa    Africa     Eastern Africa
#> 3         YEM        -99        -99          Asia      Asia       Western Asia
#> 4         VNM        -99        -99          Asia      Asia South-Eastern Asia
#> 5         VEN        -99        -99 South America  Americas      South America
#> 6         VAT        -99        -99        Europe    Europe    Southern Europe
#> 7         VUT        -99        -99       Oceania   Oceania          Melanesia
#> 8         UZB        -99        -99          Asia      Asia       Central Asia
#> 9         URY        -99        -99 South America  Americas      South America
#> 10        FSM        -99        -99       Oceania   Oceania         Micronesia
#>                     region_wb name_len long_len abbrev_len tiny homepart
#> 1          Sub-Saharan Africa        8        8          5  -99        1
#> 2          Sub-Saharan Africa        6        6          6  -99        1
#> 3  Middle East & North Africa        5        5          4  -99        1
#> 4         East Asia & Pacific        7        7          5    2        1
#> 5   Latin America & Caribbean        9        9          4  -99        1
#> 6       Europe & Central Asia        7        7          4    4        1
#> 7         East Asia & Pacific        7        7          4    2        1
#> 8       Europe & Central Asia       10       10          4    5        1
#> 9   Latin America & Caribbean        7        7          4  -99        1
#> 10        East Asia & Pacific       10       30          6  -99        1
#>    min_zoom min_label max_label   label_x    label_y      ne_id wikidataid
#> 1         0       2.5       8.0  29.92544 -18.911640 1159321441       Q954
#> 2         0       3.0       8.0  26.39530 -14.660804 1159321439       Q953
#> 3         0       3.0       8.0  45.87438  15.328226 1159321425       Q805
#> 4         0       2.0       7.0 105.38729  21.715416 1159321417       Q881
#> 5         0       2.5       7.5 -64.59938   7.182476 1159321411       Q717
#> 6         0       5.0      10.0  12.45342  41.903323 1159321407       Q237
#> 7         0       4.0       9.0 166.90876 -15.371530 1159321421       Q686
#> 8         0       3.0       8.0  64.00543  41.693603 1159321405       Q265
#> 9         0       3.0       8.0 -55.96694 -32.961127 1159321353        Q77
#> 10        0       5.0      10.0 158.23402   6.887553 1159320691       Q702
#>                      name_ar              name_bn
#> 1                   زيمبابوي             জিম্বাবুয়ে
#> 2                     زامبيا              জাম্বিয়া
#> 3                      اليمن               ইয়েমেন
#> 4                     فيتنام             ভিয়েতনাম
#> 5                    فنزويلا            ভেনেজুয়েলা
#> 6                  الفاتيكان        ভ্যাটিকান সিটি
#> 7                    فانواتو               ভানুয়াতু
#> 8                  أوزبكستان           উজবেকিস্তান
#> 9                 الأوروغواي                উরুগুয়ে
#> 10 ولايات ميكرونيسيا المتحدة মাইক্রোনেশিয়া যুক্তরাজ্য
#>                               name_de                        name_en
#> 1                            Simbabwe                       Zimbabwe
#> 2                              Sambia                         Zambia
#> 3                               Jemen                          Yemen
#> 4                             Vietnam                        Vietnam
#> 5                           Venezuela                      Venezuela
#> 6                        Vatikanstadt                   Vatican City
#> 7                             Vanuatu                        Vanuatu
#> 8                          Usbekistan                     Uzbekistan
#> 9                             Uruguay                        Uruguay
#> 10 Föderierte Staaten von Mikronesien Federated States of Micronesia
#>                            name_es  name_fa                     name_fr
#> 1                         Zimbabue زیمبابوه                    Zimbabwe
#> 2                           Zambia   زامبیا                      Zambie
#> 3                            Yemen      یمن                       Yémen
#> 4                          Vietnam   ویتنام                    Viêt Nam
#> 5                        Venezuela  ونزوئلا                   Venezuela
#> 6              Ciudad del Vaticano  واتیکان             Cité du Vatican
#> 7                          Vanuatu  وانواتو                     Vanuatu
#> 8                       Uzbekistán ازبکستان                 Ouzbékistan
#> 9                          Uruguay  اروگوئه                     Uruguay
#> 10 Estados Federados de Micronesia میکرونزی États fédérés de Micronésie
#>                                 name_el       name_he                  name_hi
#> 1                            Ζιμπάμπουε      זימבבואה                  ज़िम्बाब्वे
#> 2                                Ζάμπια         זמביה                  ज़ाम्बिया
#> 3                                Υεμένη          תימן                      यमन
#> 4                               Βιετνάμ       וייטנאם                  वियतनाम
#> 5                            Βενεζουέλα       ונצואלה                   वेनेज़ुएला
#> 6                              Βατικανό קריית הוותיקן                वैटिकन नगर
#> 7                             Βανουάτου        ונואטו                    वानूआटू
#> 8                          Ουζμπεκιστάν     אוזבקיסטן                उज़्बेकिस्तान
#> 9                            Ουρουγουάη     אורוגוואי                     उरुग्वे
#> 10 Ομόσπονδες Πολιτείες της Μικρονησίας     מיקרונזיה माइक्रोनेशिया के संघीकृत राज्य
#>                           name_hu    name_id                      name_it
#> 1                        Zimbabwe   Zimbabwe                     Zimbabwe
#> 2                          Zambia     Zambia                       Zambia
#> 3                           Jemen      Yaman                        Yemen
#> 4                         Vietnám    Vietnam                      Vietnam
#> 5                       Venezuela  Venezuela                    Venezuela
#> 6                         Vatikán    Vatikan           Città del Vaticano
#> 7                         Vanuatu    Vanuatu                      Vanuatu
#> 8                     Üzbegisztán Uzbekistan                   Uzbekistan
#> 9                         Uruguay    Uruguay                      Uruguay
#> 10 Mikronéziai Szövetségi Államok Mikronesia Stati Federati di Micronesia
#>             name_ja           name_ko      name_nl    name_pl     name_pt
#> 1        ジンバブエ          짐바브웨     Zimbabwe   Zimbabwe    Zimbábue
#> 2          ザンビア            잠비아       Zambia     Zambia      Zâmbia
#> 3          イエメン              예멘        Jemen      Jemen       Iémen
#> 4          ベトナム            베트남      Vietnam    Wietnam    Vietname
#> 5        ベネズエラ        베네수엘라    Venezuela  Wenezuela   Venezuela
#> 6          バチカン       바티칸 시국 Vaticaanstad    Watykan    Vaticano
#> 7          バヌアツ          바누아투      Vanuatu    Vanuatu     Vanuatu
#> 8    ウズベキスタン      우즈베키스탄  Oezbekistan Uzbekistan Uzbequistão
#> 9        ウルグアイ          우루과이      Uruguay    Urugwaj     Uruguai
#> 10 ミクロネシア連邦 미크로네시아 연방   Micronesia Mikronezja  Micronésia
#>       name_ru                   name_sv    name_tr    name_uk      name_ur
#> 1    Зимбабве                  Zimbabwe   Zimbabve   Зімбабве      زمبابوے
#> 2      Замбия                    Zambia    Zambiya     Замбія       زیمبیا
#> 3       Йемен                     Jemen      Yemen       Ємен          یمن
#> 4     Вьетнам                   Vietnam    Vietnam    В'єтнам       ویتنام
#> 5   Венесуэла                 Venezuela  Venezuela  Венесуела    وینیزویلا
#> 6     Ватикан             Vatikanstaten    Vatikan    Ватикан   ویٹیکن سٹی
#> 7     Вануату                   Vanuatu    Vanuatu    Вануату      وانواتو
#> 8  Узбекистан                Uzbekistan Özbekistan Узбекистан     ازبکستان
#> 9     Уругвай                   Uruguay    Uruguay    Уругвай     یوراگوئے
#> 10 Микронезия Mikronesiska federationen Mikronezya Мікронезія مائیکرونیشیا
#>          name_vi          name_zh         name_zht      fclass_iso tlc_diff
#> 1       Zimbabwe         津巴布韦           辛巴威 Admin-0 country     <NA>
#> 2         Zambia           赞比亚           尚比亞 Admin-0 country     <NA>
#> 3          Yemen             也门             葉門 Admin-0 country     <NA>
#> 4       Việt Nam             越南             越南 Admin-0 country     <NA>
#> 5      Venezuela         委内瑞拉         委內瑞拉 Admin-0 country     <NA>
#> 6  Thành Vatican           梵蒂冈           梵蒂岡 Admin-0 country     <NA>
#> 7        Vanuatu         瓦努阿图           萬那杜 Admin-0 country     <NA>
#> 8     Uzbekistan     乌兹别克斯坦         烏茲別克 Admin-0 country     <NA>
#> 9        Uruguay           乌拉圭           烏拉圭 Admin-0 country     <NA>
#> 10    Micronesia 密克罗尼西亚联邦 密克羅尼西亞聯邦 Admin-0 country     <NA>
#>         fclass_tlc fclass_us fclass_fr fclass_ru fclass_es fclass_cn fclass_tw
#> 1  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 2  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 3  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 4  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 5  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 6  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 7  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 8  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 9  Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 10 Admin-0 country      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#>    fclass_in fclass_np fclass_pk fclass_de fclass_gb fclass_br fclass_il
#> 1       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 2       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 3       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 4       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 5       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 6       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 7       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 8       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 9       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 10      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#>    fclass_ps fclass_sa fclass_eg fclass_ma fclass_pt fclass_ar fclass_jp
#> 1       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 2       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 3       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 4       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 5       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 6       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 7       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 8       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 9       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 10      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#>    fclass_ko fclass_vn fclass_tr fclass_id fclass_pl fclass_gr fclass_it
#> 1       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 2       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 3       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 4       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 5       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 6       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 7       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 8       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 9       <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#> 10      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>      <NA>
#>    fclass_nl fclass_se fclass_bd fclass_ua                       geometry
#> 1       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((31.28789 -2...
#> 2       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((30.39609 -1...
#> 3       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((53.08564 16...
#> 4       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((104.064 10....
#> 5       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((-60.82119 9...
#> 6       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((12.43916 41...
#> 7       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((166.7458 -1...
#> 8       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((70.94678 42...
#> 9       <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((-53.37061 -...
#> 10      <NA>      <NA>      <NA>      <NA> MULTIPOLYGON (((162.9832 5....
# }
```
