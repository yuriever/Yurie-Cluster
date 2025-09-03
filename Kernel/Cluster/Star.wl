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

starDefineReset::usage =
    "define the stars and reset the existing ones.";


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
    (* In this case queryRuleList=={}, and Query[{}][...] will unexpectedly return an empty association. *)
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
        (* Pad the list of associations by the placeholder missing if necessary. *)
        If[ SameQ@@keyListList,
            keyList =
                First@keyListList;
            dataMerged =
                AssociationThread[
                    keyList,
                    Transpose@Values[assocList]
                ],
            (* Else *)
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


(* ::Subsubsection:: *)
(*formattedMessage*)


formattedMessage//Attributes = {
    HoldFirst
};


formattedMessage[msg_,args___] :=
    Message[msg,Sequence@@Map[messageArgForm,{args}]];


messageArgForm[list_List] :=
    Row[Map[Style[#,StandardBlue]&,list],","];

messageArgForm[expr_] :=
    Style[expr,StandardBlue];


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*Attribute*)


SetAttributes[{
    starDefineSplit,starDefineCheck,starUpdateDefault,starUpdateDefaultWhenUnset,
    starPreIntercept,starPostIntercept,
    starDefine,starDefault,starReset,starUnset,starDefineReset,
    starMerge,starMergeKernel,
    starChange,starChangeKernel
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


General::StarUndef =
    "`2` in `1` is undefined.";

General::StarDef =
    "`2` in `1` has been defined.";

General::StarRemoveDefault =
    "`2` in `1` has been removed from default.";

General::PlanetUndef =
    "`2` in `1` is undefined.";


(* ::Subsubsection:: *)
(*starDefineSplit*)


starDefineSplit[cl_,starList_List] :=
    With[ {
            starList1 = clusterGet[cl,"StarList"]
        },
        <|
            (* Keep the order of "StarList". *)
            True->listIntersection[starList1,starList],
            False->listComplement[starList,starList1]
        |>
    ];


(* ::Subsubsection:: *)
(*starDefineCheck*)


starDefineCheck[cl_,"StarReportUndefAndReturnDef",starList_] :=
    With[ {
            starIfExist = starDefineSplit[cl,starList]
        },
        If[ starIfExist[False]=!={},
            formattedMessage[General::StarUndef,clusterGet[cl,"ClusterName"],starIfExist[False]]
        ];
        starIfExist[True]
    ];

starDefineCheck[cl_,"StarReportDefAndReturnUndef",starList_] :=
    With[ {
            starIfExist = starDefineSplit[cl,starList]
        },
        If[ starIfExist[True]=!={},
            formattedMessage[General::StarDef,clusterGet[cl,"ClusterName"],starIfExist[True]]
        ];
        starIfExist[False]
    ];


starDefineCheck[cl_,"PlanetThrowUndef",planetList_] :=
    With[ {
            planetUndefList = listComplement[planetList,clusterGet[cl,"PlanetList"]]
        },
        If[ planetUndefList=!={},
            formattedMessage[General::PlanetUndef,clusterGet[cl,"ClusterName"],planetUndefList];
            Throw[$Failed]
        ];
    ];


(* ::Subsubsection:: *)
(*starUpdateDefault*)


starUpdateDefault[cl_] :=
    With[ {
            (* Construct the default values from extra and input. *)
            defaultStar =
                clusterDataMerge[clusterGet[cl,"PlanetMergeData"]]@{
                    clusterGet[cl,"PlanetExtraData"],
                    Splice@Lookup[clusterGet[cl,"StarData"],clusterGet[cl,"StarDefaultList"]]
                }
        },
        (* Update to the default star. *)
        starPreIntercept[cl,"starUpdateDefault",defaultStar];
        clusterSet[cl,"StarDefaultData"->defaultStar];
        starPostIntercept[cl,"starUpdateDefault",defaultStar];
    ];


(* ::Subsubsection:: *)
(*starUpdateDefaultWhenUnset*)


starUpdateDefaultWhenUnset[cl_,starList_] :=
    With[ {
            removedDefaultList = listIntersection[clusterGet[cl,"StarDefaultList"],starList],
            leftDefaultList = listComplement[clusterGet[cl,"StarDefaultList"],starList]
        },
        If[ removedDefaultList=!={},
            formattedMessage[General::StarRemoveDefault,clusterGet[cl,"ClusterName"],removedDefaultList]
        ];
        clusterSet[cl,"StarDefaultList"->leftDefaultList];
    ];


(* ::Subsubsection:: *)
(*starDefine*)


starDefine[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starUndefList,newStarList,newStarData},
        (* Check existence of stars. *)
        starUndefList =
            starDefineCheck[cl,"StarReportDefAndReturnUndef",starList];
        (* Build the new star list and data. *)
        newStarList =
            Join[
                clusterGet[cl,"StarList"],
                starUndefList
            ];
        newStarData =
            <|
                clusterGet[cl,"StarData"],
                AssociationMap[clusterGet[cl,"PlanetCommonData"]&,starUndefList]
            |>;
        (* Define the new stars. *)
        starPreIntercept[cl,"starDefine",starList];
        clusterSet[cl,{"StarList"->newStarList,"StarData"->newStarData}];
        starPostIntercept[cl,"starDefine",starList];
    ];


(* ::Subsubsection:: *)
(*starDefault*)


starDefault[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList},
        (* Check existence of stars. *)
        starDefList =
            starDefineCheck[cl,"StarReportUndefAndReturnDef",starList];
        (* Set the default star. *)
        starPreIntercept[cl,"starDefault",starDefList];
        clusterSet[cl,"StarDefaultList"->starDefList];
        starPostIntercept[cl,"starDefault",starDefList];
        (* Update to the default star. *)
        starUpdateDefault[cl];
    ];


(* ::Subsubsection:: *)
(*starReset*)


starReset[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList,newStarData},
        (* Check existence of stars. *)
        starDefList =
            starDefineCheck[cl,"StarReportUndefAndReturnDef",starList];
        (* Build the data of reset stars. *)
        newStarData =
            <|
                clusterGet[cl,"StarData"],
                AssociationMap[clusterGet[cl,"PlanetCommonData"]&,starDefList]
            |>;
        (* Reset the stars. *)
        starPreIntercept[cl,"starReset",starDefList];
        clusterSet[cl,"StarData"->newStarData];
        starPostIntercept[cl,"starReset",starDefList];
        (* Update to the default star. *)
        starUpdateDefault[cl];
    ];


(* ::Subsubsection:: *)
(*starUnset*)


starUnset[cl_Symbol?clusterQ,starList_List] :=
    Module[ {starDefList,newStarList,newStarData},
        (* Check existence of stars. *)
        starDefList =
            starDefineCheck[cl,"StarReportUndefAndReturnDef",starList];
        (* Build the new star list and data. *)
        newStarList =
            listComplement[
                clusterGet[cl,"StarList"],
                starDefList
            ];
        newStarData =
            KeyDrop[
                clusterGet[cl,"StarData"],
                starDefList
            ];
        (* Unset the stars. *)
        starPreIntercept[cl,"starUnset",starDefList];
        clusterSet[cl,{"StarList"->newStarList,"StarData"->newStarData}];
        starPostIntercept[cl,"starUnset",starDefList];
        (* Remove the stars in both the input and default star list. *)
        starUpdateDefaultWhenUnset[cl,starDefList];
        (* Update to the default star. *)
        starUpdateDefault[cl];
    ];


(* ::Subsubsection:: *)
(*starDefineReset*)


starDefineReset[cl_Symbol?clusterQ,starList_List] :=
    With[ {
            starDefUndef = starDefineSplit[cl,starList]
        },
        starDefine[cl,starDefUndef[False]];
        starReset[cl,starDefUndef[True]];
    ];


(* ::Subsubsection:: *)
(*starMerge*)


starMerge[cl_Symbol?clusterQ,starList_List,planetData_] :=
    Module[ {planetAssoc,planetList,starDefList},
        planetAssoc =
            Association[planetData];
        planetList =
            Keys@planetAssoc;
        (* Check existence of stars and planets. *)
        starDefineCheck[cl,"PlanetThrowUndef",planetList];
        starDefList =
            starDefineCheck[cl,"StarReportUndefAndReturnDef",starList];
        (* Kernel. *)
        starMergeKernel[cl,#,planetAssoc]&/@starDefList;
        (* Update to the default star. *)
        starUpdateDefault[cl];
    ]//Catch;


starMergeKernel[cl_,star_,planetAssoc_] :=
    Module[ {starData,newStarData,newPlanetData},
        starData =
            clusterGet[cl,"StarData"];
        newPlanetData =
            {starData[star],planetAssoc}//clusterDataMerge[clusterGet[cl,"PlanetMergeData"]];
        newStarData =
            <|starData,star->newPlanetData|>;
        (* Add to the star. *)
        starPreIntercept[cl,"starMerge",star,newPlanetData];
        clusterSet[cl,"StarData"->newStarData];
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
        (* Check existence of stars and planets. *)
        starDefineCheck[cl,"PlanetThrowUndef",planetList];
        starDefList =
            starDefineCheck[cl,"StarReportUndefAndReturnDef",starList];
        (* Kernel. *)
        starChangeKernel[cl,#,planetAssoc,planetFunctionAssoc]&/@starDefList;
        (* Update to the default star. *)
        starUpdateDefault[cl];
    ]//Catch;


starChangeKernel[cl_,star_,planetAssoc_,planetFunctionAssoc_] :=
    Module[ {starData,newStarData,newPlanetData},
        starData =
            clusterGet[cl,"StarData"];
        newPlanetData =
            {starData[star],planetAssoc}//clusterDataMerge[planetFunctionAssoc,Apply[Identity]];
        newStarData =
            <|starData,star->newPlanetData|>;
        (* Add to the star. *)
        starPreIntercept[cl,"starChange",star,newPlanetData];
        clusterSet[cl,"StarData"->newStarData];
        starPostIntercept[cl,"starChange",star,newPlanetData];
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
