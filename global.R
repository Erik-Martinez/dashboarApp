# valores datos UI
year_list <-list("Serie temporal"=77, "2005"=2005, "2006"=2006, "2007"=2007, 
                 "2008"=2008, "2009"=2009, "2010"=2010, "2011"=2011, "2012"=2012,
                 "2013"=2013, "2014"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
                 "2018"=2018, "2019"=2019, "2020"=2020, "2021"=2021, "2022"=2022)
year_listMod <-list("2005"=2005, "2006"=2006, "2007"=2007, 
                    "2008"=2008, "2009"=2009, "2010"=2010, "2011"=2011, "2012"=2012,
                    "2013"=2013, "2014"=2014, "2015"=2015, "2016"=2016, "2017"=2017,
                    "2018"=2018, "2019"=2019, "2020"=2020, "2021"=2021, "2022"=2022)
trim_list <- list("Anual"=77,"T1"=1,"T2"=2,"T3"=3,"T4"=4)
prov_list <- list("Nacional"=77, "Araba/Álava"=1, "Albacete"=2, 
                  "Alicante/Alacant"=3, "Almería"=4,"Ávila"=5, "Badajoz"=6, 
                  "Balears, Illes"=7, "Barcelona"=8, "Burgos"=9, "Cáceres"=10, 
                  "Cádiz"=11, "Castellón/Castelló"=12, "Ciudad Real"=13, 
                  "Córdoba"=14, "Coruña, A"=15, "Cuenca"=16, "Girona"=17, 
                  "Granada"=18, "Guadalajara"=19, "Gipuzkoa"=20, "Huelva"=21, 
                  "Huesca"=22, "Jaén"=23, "León"=24, "Lleida"=25, "Rioja, La"=26, 
                  "Lugo"=27, "Madrid"=28, "Málaga"=29, "Murcia"=30, "Navarra"=31, 
                  "Ourense"=32, "Asturias"=33, "Palencia"=34, "Palmas, Las"=35, 
                  "Pontevedra"=36, "Salamanca"=37, "Santa Cruz de Tenerife"=38, 
                  "Cantabria"=39, "Segovia"=40, "Sevilla"=41, "Soria"=42, 
                  "Tarragona"=43, "Teruel"=44, "Toledo"=45,"Valencia/València"=46,
                  "Valladolid"=47, "Bizkaia"=48, "Zamora"=49, "Zaragoza"=50, 
                  "Ceuta"=51, "Melilla"=52)
prov_list <- list("Nacional"=77, "Araba", "Albacete", "Alacant", "Almería", "Ávila", "Badajoz", 
                  "Illes Balears", "Barcelona", "Burgos", "Cáceres", "Cádiz", "Castelló", 
                  "Ciudad Real", "Córdoba", "A Coruña", "Cuenca", "Girona", "Granada", 
                  "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", 
                  "Lleida", "La Rioja", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", 
                  "Ourense", "Asturias", "Palencia", "Las Palmas", "Pontevedra", "Salamanca", 
                  "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla", "Soria", 
                  "Tarragona", "Teruel", "Toledo", "València", "Valladolid", "Bizkaia", 
                  "Zamora", "Zaragoza", "Ceuta", "Melilla")
prov_list_mod <- list("Nacional", "Araba", "Albacete", "Alacant", "Almería", "Ávila", "Badajoz", 
                      "Illes Balears", "Barcelona", "Burgos", "Cáceres", "Cádiz", "Castelló", 
                      "Ciudad Real", "Córdoba", "A Coruña", "Cuenca", "Girona", "Granada", 
                      "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", 
                      "Lleida", "La Rioja", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", 
                      "Ourense", "Asturias", "Palencia", "Las Palmas", "Pontevedra", "Salamanca", 
                      "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla", "Soria", 
                      "Tarragona", "Teruel", "Toledo", "València", "Valladolid", "Bizkaia", 
                      "Zamora", "Zaragoza", "Ceuta", "Melilla")
list_vari <- list("---"=999,"Grupos de Edad(5 años)"="EDAD5", "Sexo"="SEXO1", "Nacionalidad"="NAC1", 
                  "Nivel de estudios"="NFORMA")
list_act_ampliado <- list( "Total","AGRICULTURA, GANADERÍA, SILVICULTURA Y PESCA", 
                           "INDUSTRIAS EXTRACTIVAS", "INDUSTRIA MANUFACTURERA", 
                           "SUMINISTRO DE ENERGÍA ELÉCTRICA, GAS, VAPOR Y AIRE ACONDICIONADO", 
                           "SUMINISTRO DE AGUA, ACTIVIDADES DE SANEAMIENTO. GESTIÓN DE RESIDUOS Y DESCONTAMINACIÓN", 
                           "CONSTRUCCIÓN", 
                           "COMERCIO AL POR MAYOR Y AL POR MENOR; REPARACIÓN DE VEHÍCULOS DE MOTOR Y MOTOCICLETAS", 
                           "TRANSPORTE Y ALMACENAMIENTO", "HOSTELERÍA", "INFORMACIÓN Y COMUNICACIONES", 
                           "ACTIVIDADES FINANCIERAS Y DE SEGUROS", "ACTIVIDADES INMOBILIARIAS", 
                           "ACTIVIDADES PROFESIONALES, CIENTÍFICAS Y TÉCNICAS", 
                           "ACTIVIDADES ADMINISTRATIVAS Y SERVICIOS AUXILIARES", 
                           "ADMINISTRACIÓN PÚBLICA Y DEFENSA; SEGURIDAD SOCIAL OBLIGATORIA", 
                           "EDUCACIÓN", "ACTIVIDADES SANITARIAS Y DE SERVICIOS SOCIALES", 
                           "ACTIVIDADES ARTÍSTICAS, RECREATIVAS Y DE ENTRETENIMIENTO", 
                           "OTROS SERVICIOS", "ACTIVIDADES DE LOS HOGARES COMO EMPLEADORES", 
                           "ORGANISMOS EXTRATERRITORIALES")

#---------------------------------------------------------------#

#valores datos server
cate_ocu <- c("Ocupaciones militares. Fuerzas armadas",
              "Directores y gerentes. Dirección de las empresas y de las  Administraciones Públicas",
              "Técnicos y Profesionales científicos e intelectuales",
              "Técnicos y Profesionales de apoyo",
              "Empleados contables, administrativos y otros empleados de oficina. Empleados de tipo administrativo",
              "Trabajadores de servicios de restauración, personales, protección y vendedores de comercio",
              "Trabajadores cualificados en el sector agrícola, ganadero, forestal y pesquero. 
                Trabajadores cualificados en la agricultura y en la pesca",
              "Artesanos y trabajadores cualificados de las industrias manufactureras y la construcción. 
                Artesanos y trabajadores cualificados de las industrias manufactureras, la construcción, 
                y la minería, excepto operadores de instalaciones y maquinaria" ,
              "Operadores de instalaciones y maquinaria, y montadores",
              "Ocupaciones elementales. Trabajadores no cualificados")
cate_act <- c("Agricultura, ganadería, silvicultura y pesca",
              "Industria de la alimentación, textil, cuero, madera y papel",
              "Industrias extractivas, refino de petróleo, industria química, 
                 farmacéutica, industria del caucho y materias plásticas, 
                 suministro energía eléctrica, gas, vapor y aire acondicionado, 
                 suministro de agua, gestión de residuos. Metalurgia",
              "Construcción de maquinaria, equipo eléctrico y material de transporte. 
                 Instalación y reparación industrial",
              "Construcción",
              "Comercio al por mayor y al por menor y sus instalaciones y reparaciones. 
                 Reparación de automóviles, hostelería",
              "Transporte y almacenamiento. Información y comunicaciones",
              "Intermediación financiera, seguros, actividades inmobiliarias, 
                 servicios profesionales, científicos, administrativos y otros",
              "Administración Pública, educación y actividades sanitarias",
              "Otros servicios.")
cate_situ <- c("Empresario con asalariados",
               "Trabajador independiente o empresario sin asalariados", 
               "Miembro de una cooperativa", 
               "Ayuda en la empresa o negocio familiar", 
               "Asalariado sector público",
               "Asalariado sector privado",
               "Otra situación")

age <- c("0 a 4 años", "5 a 9 años", "10 a 15 años", "16 a 19 años", 
         "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años", 
         "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años", 
         "60 a 64 años", "65 o más años")

estudios <-c("Analfabetos", "Educación primaria incompleta", "Educación primaria", 
             "Primera etapa de educación secundaria", 
             "Segunda etapa de educación secundaria. Orientación general", 
             "Segunda etapa de educación secundaria. Orientación profesional (incluye educación postsecundaria no superior)", 
             "Educación superior")

provincia <- c("Araba", "Albacete", "Alacant", "Almería", "Ávila", "Badajoz", 
               "Illes Balears", "Barcelona", "Burgos", "Cáceres", "Cádiz", "Castelló", 
               "Ciudad Real", "Córdoba", "A Coruña", "Cuenca", "Girona", "Granada", 
               "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", "Jaén", "León", 
               "Lleida", "La Rioja", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", 
               "Ourense", "Asturias", "Palencia", "Las Palmas", "Pontevedra", "Salamanca", 
               "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla", "Soria", 
               "Tarragona", "Teruel", "Toledo", "València", "Valladolid", "Bizkaia", 
               "Zamora", "Zaragoza", "Ceuta", "Melilla")

prov_gali <- c("Galicia", "A Coruña", "Lugo", "Ourense", "Pontevedra")
prov_gali_sin <- c("A Coruña", "Lugo", "Ourense", "Pontevedra")

admi <- c("No sabe", "Administración central", "Administración de la Seguridad Social",
          "Administración de Comunidad Autónoma", "Administración local",
          "Empresas públicas e Instituciones financieras públicas", "Otro tipo")


#afiliados

cate_act_ampliado <- c("AGRICULTURA, GANADERÍA, SILVICULTURA Y PESCA", 
                       "INDUSTRIAS EXTRACTIVAS", "INDUSTRIA MANUFACTURERA", 
                       "SUMINISTRO DE ENERGÍA ELÉCTRICA, GAS, VAPOR Y AIRE ACONDICIONADO", 
                       "SUMINISTRO DE AGUA, ACTIVIDADES DE SANEAMIENTO. GESTIÓN DE RESIDUOS Y DESCONTAMINACIÓN", 
                       "CONSTRUCCIÓN", 
                       "COMERCIO AL POR MAYOR Y AL POR MENOR; REPARACIÓN DE VEHÍCULOS DE MOTOR Y MOTOCICLETAS", 
                       "TRANSPORTE Y ALMACENAMIENTO", "HOSTELERÍA", "INFORMACIÓN Y COMUNICACIONES", 
                       "ACTIVIDADES FINANCIERAS Y DE SEGUROS", "ACTIVIDADES INMOBILIARIAS", 
                       "ACTIVIDADES PROFESIONALES, CIENTÍFICAS Y TÉCNICAS", 
                       "ACTIVIDADES ADMINISTRATIVAS Y SERVICIOS AUXILIARES", 
                       "ADMINISTRACIÓN PÚBLICA Y DEFENSA; SEGURIDAD SOCIAL OBLIGATORIA", 
                       "EDUCACIÓN", "ACTIVIDADES SANITARIAS Y DE SERVICIOS SOCIALES", 
                       "ACTIVIDADES ARTÍSTICAS, RECREATIVAS Y DE ENTRETENIMIENTO", 
                       "OTROS SERVICIOS", "ACTIVIDADES DE LOS HOGARES COMO EMPLEADORES", 
                       "ORGANISMOS EXTRATERRITORIALES", "Total")
cate_act_PIB <- c("Agricultura, ganadería, silvicultura y pesca", "Industria", 
                   "Construcción", "Comercio, transporte y hostelería", 
                  "Información y comunicaciones", "Actividades financieras y de seguros", 
                  "Actividades inmobiliarias", 
                  "Actividades profesionales, científicas y técnicas y otras", 
                  "Administración pública, educación y sanidad", "
                  Actividades artísticas, recreativas y otros servicios", 
                  "TODAS LAS SECCIONES", "Secciones no incorporadas")
#------------------------------------------------------------------------#

#funciones de cambio de botones

restaurar_selectores <- function(session){
  updateSelectInput(session, "select_prov", "Provincia", prov_list)
  updateSelectInput(session, "select_prov1", "Provincia", prov_list)
  updateSelectInput(session, "select_prov3", "Provincia", prov_list)
  updatePickerInput(session, "select_prov_af1", "Provincia", prov_list_mod)
  updateSelectInput(session, "select_prov_af3", "Provincia", prov_list_mod)
}


