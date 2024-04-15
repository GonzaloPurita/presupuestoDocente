sueldoDocente :: String -> Float -> Float -> Float -> Float
sueldoDocente cargo horas antiguedad integrantes = (canastaBasica integrantes) * 1.71 - (cargoDocente cargo * antiguedadFacultad antiguedad * horasTrabajadas horas) * 1.22

cargoDocente :: String -> Float
cargoDocente "titular" = 149000
cargoDocente "adjunto" = 116000
cargoDocente "ayudante" = 66000

antiguedadFacultad :: Float -> Float
antiguedadFacultad antiguedad
                        | antiguedad >= 3 && antiguedad <5 = 1.2
                        | antiguedad >= 5 && antiguedad < 10 = 1.3
                        | antiguedad >= 10 && antiguedad < 24 = 1.5
                        | antiguedad >= 24 = 2.2
                        | otherwise = 1

horasTrabajadas :: Float -> Float
horasTrabajadas horas 
                    | horas >= 5 && horas < 15 = 1
                    | horas >= 15 && horas < 25 = 2
                    | horas >= 25 && horas < 35 = 3
                    | horas >= 35 && horas < 45 = 4
                    | horas >= 45 && horas <= 50 = 5
                    | otherwise = 0

canastaBasica :: Float -> Float
canastaBasica integrantes 
                    | integrantes == 1 = 126000
                    | integrantes == 3 = 310000
                    | integrantes == 4 = 390000
                    | integrantes == 5 = 410000