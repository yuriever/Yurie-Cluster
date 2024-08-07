

(*Star.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-Star.nb"
]

VerificationTest[
	Get["Yurie`Cluster`"]
	,
	Null
	,
	TestID->"1-Star.nb"
]

VerificationTest[
	ClearAll[cluster1]
	,
	Null
	,
	TestID->"2-Star.nb"
]

VerificationTest[
	clusterInit[{"cluster1", "Global`"}, {planet1, planet2, planet3, planet4}, {{a[1]}, {b[1]}, {c[1]}, {d[1]}}, {{a[0]}, {b[0]}, {c[0]}, {d[0]}}, {Join, Sort @* Join, Union, Sort @* Union}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
	,
	TestID->"3-Star.nb"
]

VerificationTest[
	starDefine[cluster1, {star1, star2}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], star2 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}]], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
	,
	TestID->"4-Star.nb"
]

VerificationTest[
	starDefine[cluster2, {star1}]
	,
	starDefine[cluster2, {star1}]
	,
	TestID->"5-Star.nb"
]

VerificationTest[
	starDefine[cluster1, {star1}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], star2 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}]], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
	,
	{Yurie`Cluster`cluster::stardef}
	,
	TestID->"6-Star.nb"
]

VerificationTest[
	starDefault[cluster1, {star3}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], star2 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}]], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
	,
	{Yurie`Cluster`cluster::starundef}
	,
	TestID->"7-Star.nb"
]

VerificationTest[
	starDefault[cluster1, {star1, star2}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], star2 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}]], "StarDefaultList" -> {star1, star2}, "StarDefaultData" -> Association[planet1 -> {a[0], a[1], a[1]}, planet2 -> {b[0], b[1], b[1]}, planet3 -> {c[0], c[1]}, planet4 -> {d[0], d[1]}]]
	,
	TestID->"8-Star.nb"
]

VerificationTest[
	starMerge[cluster1, {star1, star2}, Association[planet1 -> {a[11]}, planet2 -> {b[11]}, planet3 -> {c[11]}, planet4 -> {d[11]}]]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1], a[11]}, planet2 -> {b[1], b[11]}, planet3 -> {c[1], c[11]}, planet4 -> {d[1], d[11]}], star2 -> Association[planet1 -> {a[1], a[11]}, planet2 -> {b[1], b[11]}, planet3 -> {c[1], c[11]}, planet4 -> {d[1], d[11]}]], "StarDefaultList" -> {star1, star2}, "StarDefaultData" -> Association[planet1 -> {a[0], a[1], a[11], a[1], a[11]}, planet2 -> {b[0], b[1], b[1], b[11], b[11]}, planet3 -> {c[0], c[1], c[11]}, planet4 -> {d[0], d[1], d[11]}]]
	,
	TestID->"9-Star.nb"
]

VerificationTest[
	starChange[cluster1, {star1, star2}, Association[planet2 -> {b[11]}], {planet2 -> Complement}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1], a[11]}, planet2 -> {b[1]}, planet3 -> {c[1], c[11]}, planet4 -> {d[1], d[11]}], star2 -> Association[planet1 -> {a[1], a[11]}, planet2 -> {b[1]}, planet3 -> {c[1], c[11]}, planet4 -> {d[1], d[11]}]], "StarDefaultList" -> {star1, star2}, "StarDefaultData" -> Association[planet1 -> {a[0], a[1], a[11], a[1], a[11]}, planet2 -> {b[0], b[1], b[1]}, planet3 -> {c[0], c[1], c[11]}, planet4 -> {d[0], d[1], d[11]}]]
	,
	TestID->"10-Star.nb"
]

VerificationTest[
	starReset[cluster1, {star1}]; 
	starReset[cluster1, {star2}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {star1, star2}, "StarData" -> Association[star1 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], star2 -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}]], "StarDefaultList" -> {star1, star2}, "StarDefaultData" -> Association[planet1 -> {a[0], a[1], a[1]}, planet2 -> {b[0], b[1], b[1]}, planet3 -> {c[0], c[1]}, planet4 -> {d[0], d[1]}]]
	,
	TestID->"11-Star.nb"
]

VerificationTest[
	starUnset[cluster1, {star1}]; 
	starUnset[cluster1, {star2}]; 
	cluster1["Data"]
	,
	Association["ClusterName" -> "cluster1", "PlanetList" -> {planet1, planet2, planet3, planet4}, "PlanetCommonData" -> Association[planet1 -> {a[1]}, planet2 -> {b[1]}, planet3 -> {c[1]}, planet4 -> {d[1]}], "PlanetExtraData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}], "PlanetMergeData" -> Association[planet1 -> Join, planet2 -> Sort @* Join, planet3 -> Union, planet4 -> Sort @* Union], "StarList" -> {}, "StarData" -> Association[], "StarDefaultList" -> {}, "StarDefaultData" -> Association[planet1 -> {a[0]}, planet2 -> {b[0]}, planet3 -> {c[0]}, planet4 -> {d[0]}]]
	,
	{Yurie`Cluster`cluster::rmdefault,Yurie`Cluster`cluster::rmdefault}
	,
	TestID->"12-Star.nb"
]

VerificationTest[
	ClearAll[cluster1]
	,
	Null
	,
	TestID->"13-Star.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Star.nb"
]