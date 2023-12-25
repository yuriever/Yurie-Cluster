(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Star`"];


Needs["Yurie`Cluster`"];
Needs["Yurie`Cluster`Common`"];
(*`Star` depends on `Cluster`.*)
Needs["Yurie`Cluster`Cluster`"];


(* ::Section:: *)
(*Public*)


starDefineQ;
starDefine;
starDefault;
starReset;
starUnset;
starMerge;
starChange;
starPreIntercept;
starPostIntercept;


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


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Attributes and messages*)


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


starDefineCheck::starundef =
    "the star `` is undefined.";
starDefineCheck::stardef =
    "the star `` has been defined.";
starDefineCheck::memundef =
    "the planet `` is undefined.";

starUnset::rmdefault =
    "the following stars `` have been removed from default.";


(* ::Subsection:: *)
(*starDefineQ*)


starDefineQ[cluster_,star_] :=
    MemberQ[clusterPropGetUnsafe[cluster,"starList"],star];

starDefineQ[cluster_,starList_List] :=
    <|
        True->Intersection[
            starList,
            clusterPropGetUnsafe[cluster,"starList"]
        ],
        False->Complement[
            starList,
            clusterPropGetUnsafe[cluster,"starList"]
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
            clusterPropGetUnsafe[cluster,"planetList"]
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
            mergeByKey[clusterPropGetUnsafe[cluster,"planetMergeData"]]@Join[
                {clusterPropGetUnsafe[cluster,"planetExtraData"]},
                clusterPropGetUnsafe[cluster,"starData"]/@clusterPropGetUnsafe[cluster,"starDefaultList"]
            ];
        (*update to the default star.*)
        starPreIntercept[cluster,"starUpdateDefault",defaultStar];
        clusterPropSetUnsafe[cluster,"starDefaultData"->defaultStar];
        starPostIntercept[cluster,"starUpdateDefault",defaultStar];
    ];


(* ::Subsection:: *)
(*starDefine*)


(*star property has not been defined.*)

starDefine[cluster_Symbol?clusterValidQ,starList_List] :=
    Module[ {starUndefList,newStarList,newStarData},
        (*check existence of stars.*)
        starUndefList = 
            starDefineCheck[cluster,"starReportDefAndReturnUndef",starList];
        (*build the new star list and data.*)
        newStarList = Join[
            clusterPropGetUnsafe[cluster,"starList"],
            starUndefList
        ];
        newStarData = <|
            clusterPropGetUnsafe[cluster,"starData"],
            AssociationMap[clusterPropGetUnsafe[cluster,"planetCommonData"]&,starUndefList]
        |>; 
        (*define the new stars.*)
        starPreIntercept[cluster,"starDefine",starList];
        clusterPropSetUnsafe[cluster,{"starList"->newStarList,"starData"->newStarData}];
        starPostIntercept[cluster,"starDefine",starList];
    ];


(* ::Subsection:: *)
(*starDefault*)


starDefault[cluster_Symbol?clusterValidQ,starList_List] :=
    Module[ {starDefList},
        (*check existence of stars.*)
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*set the default star.*)
        starPreIntercept[cluster,"starDefault",starDefList];
        clusterPropSetUnsafe[cluster,"starDefaultList"->starDefList];
        starPostIntercept[cluster,"starDefault",starDefList];
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


(* ::Subsection:: *)
(*starReset*)


starReset[cluster_Symbol?clusterValidQ,starList_List] :=
    Module[ {starDefList,newStarData},
        (*check existence of stars.*)
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*build the data of reset stars.*)
        newStarData = <|
            clusterPropGetUnsafe[cluster,"starData"],
            AssociationMap[clusterPropGetUnsafe[cluster,"planetCommonData"]&,starDefList]
        |>; 
        (*reset the stars.*)
        starPreIntercept[cluster,"starReset",starDefList];
        clusterPropSetUnsafe[cluster,"starData"->newStarData];
        starPostIntercept[cluster,"starReset",starDefList];
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


(* ::Subsection:: *)
(*starUnset*)


starUnset[cluster_Symbol?clusterValidQ,starList_List] :=
    Module[ {starDefList,newStarList,newStarData},
        (*check existence of stars.*)
        starDefList = 
            starDefineCheck[cluster,"starReportUndefAndReturnDef",starList];
        (*build the new star list and data.*)
        newStarList = Complement[
            clusterPropGetUnsafe[cluster,"starList"],
            starDefList
        ];
        newStarData = KeyDrop[
            clusterPropGetUnsafe[cluster,"starData"],
            starDefList
        ]; 
        (*unset the stars.*)
        starPreIntercept[cluster,"starUnset",starDefList];
        (*KeyDropFrom[clusterPropGetUnsafe[cluster,"starProperty"],starDefList];*)
        clusterPropSetUnsafe[cluster,{"starList"->newStarList,"starData"->newStarData}];
        starPostIntercept[cluster,"starUnset",starDefList];
        (*remove the stars in both the input and default star list.*)
        starUpdateDefaultWhenUnset[cluster,starDefList];
        (*update to the default star.*)
        starUpdateDefault[cluster];
    ];


starUpdateDefaultWhenUnset[cluster_,starList_] :=
    Module[ {removedDefaultList,leftDefaultList},
        removedDefaultList = Intersection[
            clusterPropGetUnsafe[cluster,"starDefaultList"],
            starList
        ];
        leftDefaultList = Complement[
            clusterPropGetUnsafe[cluster,"starDefaultList"],
            starList
        ];
        If[ removedDefaultList=!={},
            Message[starUnset::rmdefault,removedDefaultList]
        ];
        clusterPropSetUnsafe[cluster,"starDefaultList"->leftDefaultList];
    ];


(* ::Subsection:: *)
(*starMerge*)


starMerge[cluster_Symbol?clusterValidQ,starList_List,planetData_] :=
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
            clusterPropGetUnsafe[cluster,"starData"];
        newPlanetData =
            mergeByKey[clusterPropGetUnsafe[cluster,"planetMergeData"]]@{starData[star],planetAssoc};
        newStarData =
            <|starData,star->newPlanetData|>;
        (*add to the star.*)
        starPreIntercept[cluster,"starMerge",star,newPlanetData];
        clusterPropSetUnsafe[cluster,"starData"->newStarData];
        starPostIntercept[cluster,"starMerge",star,newPlanetData];
    ];


(* ::Subsection:: *)
(*starChange*)


starChange[cluster_Symbol?clusterValidQ,starList_List,planetData_,planetFunctionData_] :=
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
            clusterPropGetUnsafe[cluster,"starData"];
        newPlanetData =
            mergeByKey[planetFunctionAssoc,Apply[Identity]]@{starData[star],planetAssoc};
        newStarData =
            <|starData,star->newPlanetData|>;
        (*add to the star.*)
        starPreIntercept[cluster,"starChange",star,newPlanetData];
        clusterPropSetUnsafe[cluster,"starData"->newStarData];
        starPostIntercept[cluster,"starChange",star,newPlanetData];
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
