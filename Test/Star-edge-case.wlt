

(* Star-edge-case.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Star-edge-case.nb"
]

VerificationTest[
    Get["Yurie`Cluster`"]
    ,
    Null
    ,
    TestID->"[1] Star-edge-case.nb"
]

VerificationTest[
    ClearAll[cluster1]
    ,
    Null
    ,
    TestID->"[2] Star-edge-case.nb"
]

VerificationTest[
    clusterInit[{"cluster1", "Global`"}, {planet1, planet2, planet3, planet4}, {{a[1]}, {b[1]}, {c[1]}, {d[1]}}, {{a[0]}, {b[0]}, {c[0]}, {d[0]}}, {Join, Sort @* Join, Union, Sort @* Union}]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[3] Star-edge-case.nb"
]

VerificationTest[
    starDefine[cluster1, {}]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[4] Star-edge-case.nb"
]

VerificationTest[
    starDefault[cluster1, {}]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[5] Star-edge-case.nb"
]

VerificationTest[
    starMerge[cluster1, {}, Association[planet1 -> {a[11]}, planet2 -> {b[11]}, planet3 -> {c[11]}, planet4 -> {d[11]}]]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[6] Star-edge-case.nb"
]

VerificationTest[
    starChange[cluster1, {}, Association[planet2 -> {b[11]}], {planet2 -> Complement}]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[7] Star-edge-case.nb"
]

VerificationTest[
    starReset[cluster1, {}]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[8] Star-edge-case.nb"
]

VerificationTest[
    starUnset[cluster1, {}]; 
    cluster1["Data"]
    ,
    Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
    ,
    TestID->"[9] Star-edge-case.nb"
]

VerificationTest[
    ClearAll[cluster1]
    ,
    Null
    ,
    TestID->"[10] Star-edge-case.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Star-edge-case.nb"
]