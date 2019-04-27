let map1 =
  List.map Port.boat_of_string ["A2H12"]

let map2 =
  List.map Port.boat_of_string ["A2H12"; "B3V52"; "C2V41"]

let map3 =
  List.map Port.boat_of_string ["A2H12"; "B2V51"]

let map4 =
  List.map Port.boat_of_string ["A2H02"; "B3V50"; "C3H33"]

let map5 =
  List.map Port.boat_of_string ["A2H12"; "B2V02"; "C2H10"; "D3V23"; "E3V30"; "F2H33"; "G2H35"; "H3V51"]

let map6 =
  List.map Port.boat_of_string ["B2H00"; "C2H20"; "D2V40"; "E2V21"; "F3V51"; "G2V12"; "A2H32"; "I2H33"; "J3V03"; "K2H15"; "L2H44"; "M2H45"; "N2V34"]

let map7 =
  List.map Port.boat_of_string ["A2H12"; "B2H00"; "C3V01"; "D2V04"; "E3H13"; "F3V30"; "G3V41"; "H2H44"; "I2H45"; "J2V50"; "K2V52"]

let map8 =
  List.map Port.boat_of_string ["A2H12"; "B3V00"; "C2V23"; "D3H25"; "E3V30"; "F3H33"; "G2V54"]

let map9 =
  List.map Port.boat_of_string ["A2H12"; "B2H00"; "C2V01"; "D2V03"; "E2H05"; "F3H11"; "G2V13"; "H2H20"; "I2V32"; "J3H34"; "K2V40"; "L2H43"; "M3V50"]

let map10 =
  List.map Port.boat_of_string ["A2H22"; "B3H00"; "C2V01"; "D2H03"; "E2H11"; "F2V14"; "G2V23"; "H2H25"; "I2V30"; "J3V40"; "K2H44"; "L2H45"; "M3V50"]

let map11 =
  List.map Port.boat_of_string ["A2H02"; "B2V00"; "C2H11"; "D2V22"; "E2V24"; "F3H30"; "G2V31"; "H2H34"; "I3H35"; "J3V52"]

let map12 =
  List.map Port.boat_of_string ["A2H02"; "B3V03"; "C2V10"; "D3H13"; "E2H20"; "F2V24"; "G2V31"; "H2H40"; "I2H41"; "J3V42"; "K2V52"; "L2V54"]

let map13 =
  List.map Port.boat_of_string ["A2H12"; "B2H01"; "C2H21"; "D2H10"; "E2H30"; "F2H05"; "G2H44"; "H2H45"; "I2V02"; "J2V23"; "K2V32"; "L2V34"; "M2V41"; "N2V50"]

let map14 =
  List.map Port.boat_of_string ["A2H22"; "B2V01"; "C2V11"; "D2V30"; "E2V42"; "F2V24"; "G3V51"; "H2H40"; "I3H13"; "J3H34"]

let all_maps = [|map1; map2; map3; map4; map5; map6; map7; map8; map9; map10; map11; map12; map13; map14|]

let colors = [|"#FF0000"; "#FF8C00"; "#FFFF00"; "#32CD32"; "#40E0D0"; "#1E90FF"; "#6A5ACD"; "#FF69B4"; "#696969"; "#8B4513"; "#4B0082"; "#00FFFF"; "#808000"; "#ADFF2F"; "#FF7F50"|]
