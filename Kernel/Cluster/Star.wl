(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Star`"];


Needs["Yurie`Cluster`"];


(* ::Section:: *)
(*Public*)


starDefine::usage =
    "define the stars.";

starDefault::usage =
    "set the stars to default.";

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
(*Helper*)


(* ::Subsubsection:: *)
(*clusterDataMerge*)


clusterDataMerge::usage =
    "merge a list of associations using different merge functions according to keys.";

clusterDataMerge[ruleAssoc_,default_:Identity][assocList:{___Association}] :=
    mergeByKeyKernel[
        assocList,
        ruleAssoc//Map[Apply]//Normal,
        default
    ];


mergeByKeyKernel[{<||>...},_,_] :=
    <||>;

mergeByKeyKernel[assocList_,{},Identity] :=
    (*in this case queryRuleList=={}, and Query[{}][...] will unexpectedly return an empty association.*)
    getTransposedAssocListAndKeyList[assocList,{}]//First;

mergeByKeyKernel[assocList_,ruleList_,default_] :=
    Module[ {keyList,dataMerged,queryRuleList},
        {dataMerged,keyList} =
            getTransposedAssocListAndKeyList[assocList,ruleList];
        queryRuleList =
            prepareQueryRuleList[ruleList,keyList,default];
        Query[queryRuleList]@dataMerged
    ];


getTransposedAssocListAndKeyList[assocList_,ruleList_] :=
    Module[ {keyList,keyListList,dataPadded,dataMerged,missing},
        keyListList =
            Keys[assocList];
        (*pad the list of associations by the placeholder missing if necessary.*)
        If[ SameQ@@keyListList,
            keyList =
                First@keyListList;
            dataMerged =
                AssociationThread[
                    keyList,
                    Transpose@Values[assocList]
                ],
            (*Else*)
            dataPadded =
                KeyUnion[assocList,missing&];
            keyList =
                Keys@First@dataPadded;
            dataMerged =
                AssociationThread[
                    keyList,
                    DeleteCases[Transpose@Values[dataPadded],missing,{2}]
                ];
        ];
        {dataMerged,Key/@keyList}
    ];


prepareQueryRuleList[ruleList_,keyList_,default_] :=
    DeleteCases[
        Thread[
            keyList->Lookup[ruleList,keyList,default]
        ],
        _->Identity
    ];


(* ::Subsubsection:: *)
(*listComplement*)


listComplement[list1_List,list2_List] :=
    DeleteCases[list1,Alternatives@@list2];


(* ::Subsubsection:: *)
(*listIntersection*)


listIntersection[list1_List,list2_List] :=
    Cases[list1,Alternatives@@list2];


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*Attribute*)


SetAttributes[{
    starDefineSplit,starDefineCheck,starUpdateDefault,starUpdateDefaultWhenUnset,
    starPreIntercept,starPostIntercept,
    starDefine,starDefault,starReset,starUnset,starMerge,starMergeKernel,starChange,starChangeKernel
},HoldFirst];


(* ::Subsubsection:: *)
(*Message*)


starDefineSplit::usage =
    "split the list of stars into defined and undefined.";

starDefineCheck::usage =
    "check the input before calling public methods.";

starUpdateDefault::usage =
    "update starDefaultData by starDefaultList after starDefault, starReset, starUnset, starMerge and starChange.";

starUpdateDefaultWhenUnset::usage =
    "remove the stars in both the input and the default star list, used by starUnset.";


cluster::starundef =
    "the star `2` in `1` is undefined.";

cluster::stardef =
    "the star `2` in `1` has been defined.";

cluster::plannetundef =
    "the planet `2` in `1` is undefined.";

cluster::rmdefault =
    "the star `2` in `1` has been removed from default.";


(* ::Subsubsection:: *)
(*starDefineSplit*)


starDefineSplit[cl_,starList_List] :=
    <|
        (*keep the ordering of "StarList".*)
        True->listIntersection[
            clusterPropGet[cl,"StarList"],
            starList
        ],
        False->listComplement[
            starList,
            clusterPropGet[cl,"StarList"]
        ]
    |>;


(* ::Subsubsection:: *)
(*starDefineCheck*)


starDefineCheck[cl_,"starReportUndefAndReturnDef",starList_] :=
    Module[ {starIfExist},
        starIfExist =
            starDefineSplit[cl,starList];
        If[ starIfExist[False]=!={},
            Message[cluster::starundef,clusterPropGet[cl,"ClusterName"],starIfExist[False]]
        ];
        starIfExist[True]
    ];

starDefineCheck[cl_,"starReportDefAndReturnUndef",starList_] :=
    Module[ {starIfExist},
        starIfExist =
            starDefineSplit[cl,starList];
        If[ starIfExist[True]=!={},
            Message[cluster::stardef,clusterPropGet[cl,"ClusterName"],starIfExist[True]]
        ];
        starIfExist[False]
    ];

starDefineCheck[cl_,"planetAbortUndef",planetList_] :=
    Module[ {planetUndefList},
        planetUndefList =
            listComplement[
                planetList,
                clusterPropGet[cl,"PlanetList"]
            ];
        If[ planetUndefList=!={},
            Message[cluster::plannetundef,clusterPropGet[cl,"ClusterName"],planetUndefList];
            Abort[]
        ];
    ];


(* ::Subsubsection:: *)
(*starUpdateDefault*)


starUpdateDefault[cl_] :=
    Module[ {defaultStar},
        (*construct the default values from extra and input.*)
        defaultStar =
            Join[
                {clusterPropGet[cl,"PlanetExtraData"]},
                clusterPropGet[cl,"StarData"]/@clusterPropGet[cl,"StarDefaultList"]
            ]//clusterDataMerge[clusterPropGet[cl,"PlanetMergeData"]];
        (*update to the default star.*)
        starPreIntercept[cl,"starUpdateDefault",defaultStar];
        clusterPropSet[cl,"StarDefaultData"->defaultStar];
        starPostIntercept[cl,"starUpdateDefault",defaultStar];
    ];


(* ::Subsubsection:: *)
(*starUpdateDefaultWhenUnset*)


starUpdateDefaultWhenUnset[cl_,starList_] :=
    Module[ {removedDefaultList,leftDefaultList},
        removedDefaultList =
            listIntersection[
                clusterPropGet[cl,"StarDefaultList"],
                starList
            ];
        leftDefaultList =
            listComplement[
                clusterPropGet[cl,"StarDefaultList"],
                starList
            ];
        If[ removedDefaultList=!={},
            Message[cluster::rmdefault,clusterPropGet[cl,"ClusterName"],removedDefaultList]
        ];
        clusterPropSet[cl,"StarDefaultList"->leftDefaultList];
    ];


(* ::Subsubsection:: *)
(*starDefine*)


starDefine[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starUndefList,newStarList,newStarData},
        (*check existence of stars.*)
        starUndefList =
            starDefineCheck[cl,"starReportDefAndReturnUndef",starList];
        (*build the new star list and data.*)
        newStarList =
            Join[
                clusterPropGet[cl,"StarList"],
                starUndefList
            ];
        newStarData =
            <|
                clusterPropGet[cl,"StarData"],
                AssociationMap[clusterPropGet[cl,"PlanetCommonData"]&,starUndefList]
            |>;
        (*define the new stars.*)
        starPreIntercept[cl,"starDefine",starList];
        clusterPropSet[cl,{"StarList"->newStarList,"StarData"->newStarData}];
        starPostIntercept[cl,"starDefine",starList];
    ];


(* ::Subsubsection:: *)
(*starDefault*)


starDefault[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList},
        (*check existence of stars.*)
        starDefList =
            starDefineCheck[cl,"starReportUndefAndReturnDef",starList];
        (*set the default star.*)
        starPreIntercept[cl,"starDefault",starDefList];
        clusterPropSet[cl,"StarDefaultList"->starDefList];
        starPostIntercept[cl,"starDefault",starDefList];
        (*update to the default star.*)
        starUpdateDefault[cl];
    ];


(* ::Subsubsection:: *)
(*starReset*)


starReset[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList,newStarData},
        (*check existence of stars.*)
        starDefList =
            starDefineCheck[cl,"starReportUndefAndReturnDef",starList];
        (*build the data of reset stars.*)
        newStarData =
            <|
                clusterPropGet[cl,"StarData"],
                AssociationMap[clusterPropGet[cl,"PlanetCommonData"]&,starDefList]
            |>; 
        (*reset the stars.*)
        starPreIntercept[cl,"starReset",starDefList];
        clusterPropSet[cl,"StarData"->newStarData];
        starPostIntercept[cl,"starReset",starDefList];
        (*update to the default star.*)
        starUpdateDefault[cl];
    ];


(* ::Subsubsection:: *)
(*starUnset*)


starUnset[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList,newStarList,newStarData},
        (*check existence of stars.*)
        starDefList =
            starDefineCheck[cl,"starReportUndefAndReturnDef",starList];
        (*build the new star list and data.*)
        newStarList =
            listComplement[
                clusterPropGet[cl,"StarList"],
                starDefList
            ];
        newStarData =
            KeyDrop[
                clusterPropGet[cl,"StarData"],
                starDefList
            ];
        (*unset the stars.*)
        starPreIntercept[cl,"starUnset",starDefList];
        clusterPropSet[cl,{"StarList"->newStarList,"StarData"->newStarData}];
        starPostIntercept[cl,"starUnset",starDefList];
        (*remove the stars in both the input and default star list.*)
        starUpdateDefaultWhenUnset[cl,starDefList];
        (*update to the default star.*)
        starUpdateDefault[cl];
    ];


(* ::Subsubsection:: *)
(*starMerge*)


starMerge[cl_Symbol?clusterQ,starList_List,planetData_] :=
    Module[ {planetAssoc,planetList,starDefList},
        planetAssoc =
            Association[planetData];
        planetList =
            Keys@planetAssoc;
        (*check existence of stars and planets.*)
        starDefineCheck[cl,"planetAbortUndef",planetList];
        starDefList =
            starDefineCheck[cl,"starReportUndefAndReturnDef",starList];
        (*kernel*)
        starMergeKernel[cl,#,planetAssoc]&/@starDefList;
        (*update to the default star.*)
        starUpdateDefault[cl];
    ];


starMergeKernel[cl_,star_,planetAssoc_] :=
    Module[ {starData,newStarData,newPlanetData},
        starData =
            clusterPropGet[cl,"StarData"];
        newPlanetData =
            {starData[star],planetAssoc}//clusterDataMerge[clusterPropGet[cl,"PlanetMergeData"]];
        newStarData =
            <|starData,star->newPlanetData|>;
        (*add to the star.*)
        starPreIntercept[cl,"starMerge",star,newPlanetData];
        clusterPropSet[cl,"StarData"->newStarData];
        starPostIntercept[cl,"starMerge",star,newPlanetData];
    ];


(* ::Subsubsection:: *)
(*starChange*)


starChange[cl_Symbol?clusterQ,starList_List,planetData_,planetFunctionData_] :=
    Module[ {planetFunctionAssoc,planetAssoc,planetList,starDefList},
        planetFunctionAssoc =
            Association[planetFunctionData];
        planetAssoc =
            Association[planetData];
        planetList =
            Keys@planetAssoc;
        (*check existence of stars and planets.*)
        starDefineCheck[cl,"planetAbortUndef",planetList];
        starDefList =
            starDefineCheck[cl,"starReportUndefAndReturnDef",starList];
        (*kernel*)
        starChangeKernel[cl,#,planetAssoc,planetFunctionAssoc]&/@starDefList;
        (*update to the default star.*)
        starUpdateDefault[cl];
    ];


starChangeKernel[cl_,star_,planetAssoc_,planetFunctionAssoc_] :=
    Module[ {starData,newStarData,newPlanetData},
        starData =
            clusterPropGet[cl,"StarData"];
        newPlanetData =
            {starData[star],planetAssoc}//clusterDataMerge[planetFunctionAssoc,Apply[Identity]];
        newStarData =
            <|starData,star->newPlanetData|>;
        (*add to the star.*)
        starPreIntercept[cl,"starChange",star,newPlanetData];
        clusterPropSet[cl,"StarData"->newStarData];
        starPostIntercept[cl,"starChange",star,newPlanetData];
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
