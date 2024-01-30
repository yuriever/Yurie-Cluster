(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Star`"];


Needs["Yurie`Cluster`"];
Needs["Yurie`Cluster`Common`"];
Needs["Yurie`Cluster`Cluster`"];


(* ::Section:: *)
(*Public*)


starDefine::usage = 
    "define the stars.";

starDefault::usage = 
    "set the stars into default.";

starReset::usage = 
    "reset the stars.";

starUnset::usage = 
    "unset the stars, and update the default star list.";

starMerge::usage = 
    "merge planet data to the stars.";

starChange::usage = 
    "change planet data to the stars by the functions.";
    
starPreIntercept::usage = 
    "reserved function to modify the pre-process of star methods.";

starPostIntercept::usage = 
    "reserved function to modify the post-process of star methods.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Messages and attributes*)


starDefineQ::usage = 
    "check whether the star is defined, or split the list of stars into defined and undefined.";

starDefineCheck::usage = 
    "check the input before calling public methods.";

starUpdateDefault::usage = 
    "update starDefaultData by starDefaultList after starDefault, starReset, starUnset, starMerge and starChange.";

starUpdateDefaultWhenUnset::usage =
    "remove the stars both in the input and the default star list, used by starUnset.";

starMergeKernel::usage = 
    "kernel function.";

starChangeKernel::usage = 
    "kernel function.";


starDefineCheck::starundef =
    "the star `` is undefined.";
starDefineCheck::stardef =
    "the star `` has been defined.";
starDefineCheck::memundef =
    "the planet `` is undefined.";

starUnset::rmdefault =
    "the following stars `` have been removed from default.";


SetAttributes[{
    starDefineQ,
    starDefineCheck,
    starUpdateDefault,
    starPreIntercept,
    starPostIntercept,
    starDefine,
    starDefault,
    starReset,
    starUnset,
    starUpdateDefaultWhenUnset,
    starMerge,
    starMergeKernel,
    starChange,
    starChangeKernel
},HoldFirst];


(* ::Subsection:: *)
(*starDefineQ*)


starDefineQ[cluster_,star_] :=
    MemberQ[clusterPropGet[cluster,"starList"],star];

starDefineQ[cluster_,starList_List] :=
    <|
        True->Intersection[
            starList,
            clusterPropGet[cluster,"starList"]
        ],
        False->Complement[
            starList,
            clusterPropGet[cluster,"starList"]
        ]
    |>;


(* ::Subsection:: *)
(*starDefineCheck*)


starDefineCheck[cluster_,"starReportUndefAndReturnDef",starList_] :=
    Module[ {starIfExist},
        starIfExist = starDefineQ[cluster,starList];
        If[ starIfExist[False]=!={},
            messageHideContext[starDefineCheck::starundef,starIfExist[False]]
        ];
        starIfExist[True]
    ];

starDefineCheck[cluster_,"starReportDefAndReturnUndef",starList_] :=
    Module[ {starIfExist},
        starIfExist = starDefineQ[cluster,starList];
        If[ starIfExist[True]=!={},
            messageHideContext[starDefineCheck::stardef,starIfExist[True]]
        ];
        starIfExist[False]
    ];
    
starDefineCheck[cluster_,"planetAbortUndef",planetList_] :=
    Module[ {planetUndefList},
        planetUndefList = Complement[
            planetList,
            clusterPropGet[cluster,"planetList"]
        ];
        If[ planetUndefList=!={},
            messageHideContext[starDefineCheck::memundef,planetUndefList];
            Abort[]
        ];
    ];


(* ::Subsection:: *)
(*starDefaultUpdate*)


starUpdateDefault[cluster_] :=
    Module[ {defaultStar},
        (*construct the default values from extra and input.*)
        defaultStar = 
            clusterDataMerge[clusterPropGet[cluster,"planetMergeData"]]@Join[
                {clusterPropGet[cluster,"planetExtraData"]},
                clusterPropGet[cluster,"starData"]/@clusterPropGet[cluster,"starDefaultList"]
            ];
        (*update to the default star.*)
        starPreIntercept[cluster,"starUpdateDefault",defaultStar];
        clusterPropSet[cluster,"starDefaultData"->defaultStar];
        starPostIntercept[cluster,"starUpdateDefault",defaultStar];
    ];


(* ::Subsection:: *)
(*starDefine*)


starDefine[cluster_Symbol?clusterQ,starList_List] :=
    Module[ {starUndefList,newStarList,newStarData},
        (*check existence of stars.*)
        starUndefList = 
            starDefineCheck[cluster,"starReportDefAndReturnUndef",starList];
        (*build the new star list and data.*)
        newStarList = Join[
            clusterPropGet[cluster,"starList"],
            starUndefList
        ];
        newStarData = <|
            clusterPropGet[cluster,"starData"],
            AssociationMap[clusterPropGet[cluster,"planetCommonData"]&,starUndefList]
        |>; 
        (*define the new stars.*)
        starPreIntercept[cluster,"starDefine",starList];
        clusterPropSet[cluster,{"starList"->newStarList,"starData"->newStarData}];
        starPostIntercept[cluster,"starDefine",starList];
    ];


(* ::Subsection:: *)
(*starDefault*)


starDefault[cluster_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList},
        (*check existence of stars.*)
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*set the default star.*)
        starPreIntercept[cluster,"starDefault",starDefList];
        clusterPropSet[cluster,"starDefaultList"->starDefList];
        starPostIntercept[cluster,"starDefault",starDefList];
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


(* ::Subsection:: *)
(*starReset*)


starReset[cluster_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList,newStarData},
        (*check existence of stars.*)
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*build the data of reset stars.*)
        newStarData = <|
            clusterPropGet[cluster,"starData"],
            AssociationMap[clusterPropGet[cluster,"planetCommonData"]&,starDefList]
        |>; 
        (*reset the stars.*)
        starPreIntercept[cluster,"starReset",starDefList];
        clusterPropSet[cluster,"starData"->newStarData];
        starPostIntercept[cluster,"starReset",starDefList];
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


(* ::Subsection:: *)
(*starUnset*)


starUnset[cluster_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList,newStarList,newStarData},
        (*check existence of stars.*)
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*build the new star list and data.*)
        newStarList = Complement[
            clusterPropGet[cluster,"starList"],
            starDefList
        ];
        newStarData = KeyDrop[
            clusterPropGet[cluster,"starData"],
            starDefList
        ]; 
        (*unset the stars.*)
        starPreIntercept[cluster,"starUnset",starDefList];
        clusterPropSet[cluster,{"starList"->newStarList,"starData"->newStarData}];
        starPostIntercept[cluster,"starUnset",starDefList];
        (*remove the stars in both the input and default star list.*)
        starUpdateDefaultWhenUnset[cluster,starDefList];
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


starUpdateDefaultWhenUnset[cluster_,starList_] :=
    Module[ {removedDefaultList,leftDefaultList},
        removedDefaultList = Intersection[
            clusterPropGet[cluster,"starDefaultList"],
            starList
        ];
        leftDefaultList = Complement[
            clusterPropGet[cluster,"starDefaultList"],
            starList
        ];
        If[ removedDefaultList=!={},
            Message[starUnset::rmdefault,removedDefaultList]
        ];
        clusterPropSet[cluster,"starDefaultList"->leftDefaultList];
    ];


(* ::Subsection:: *)
(*starMerge*)


starMerge[cluster_Symbol?clusterQ,starList_List,planetData_] :=
    Module[ {planetAssoc,planetList,starDefList},
        planetAssoc = 
            Association[planetData];
        planetList = 
            Keys@planetAssoc;        
        (*check existence of stars and planets.*)
        starDefineCheck[cluster,"planetAbortUndef",planetList];
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*kernel*)
        starMergeKernel[cluster,#,planetAssoc]&/@starDefList;
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


starMergeKernel[cluster_,star_,planetAssoc_] :=
    Module[ {starData,newStarData,newPlanetData},
        starData =
            clusterPropGet[cluster,"starData"];
        newPlanetData =
            clusterDataMerge[clusterPropGet[cluster,"planetMergeData"]]@{starData[star],planetAssoc};
        newStarData =
            <|starData,star->newPlanetData|>;
        (*add to the star.*)
        starPreIntercept[cluster,"starMerge",star,newPlanetData];
        clusterPropSet[cluster,"starData"->newStarData];
        starPostIntercept[cluster,"starMerge",star,newPlanetData];
    ];


(* ::Subsection:: *)
(*starChange*)


starChange[cluster_Symbol?clusterQ,starList_List,planetData_,planetFunctionData_] :=
    Module[ {planetFunctionAssoc,planetAssoc,planetList,starDefList},
        planetFunctionAssoc =
            Association[planetFunctionData];
        planetAssoc = 
            Association[planetData];
        planetList = 
            Keys@planetAssoc;        
        (*check existence of stars and planets.*)
        starDefineCheck[cluster,"planetAbortUndef",planetList];
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*kernel*)
        starChangeKernel[cluster,#,planetAssoc,planetFunctionAssoc]&/@starDefList;
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


starChangeKernel[cluster_,star_,planetAssoc_,planetFunctionAssoc_] :=
    Module[ {starData,newStarData,newPlanetData},
        starData =
            clusterPropGet[cluster,"starData"];
        newPlanetData =
            clusterDataMerge[planetFunctionAssoc,Apply[Identity]]@{starData[star],planetAssoc};
        newStarData =
            <|starData,star->newPlanetData|>;
        (*add to the star.*)
        starPreIntercept[cluster,"starChange",star,newPlanetData];
        clusterPropSet[cluster,"starData"->newStarData];
        starPostIntercept[cluster,"starChange",star,newPlanetData];
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
