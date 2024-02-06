(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Cluster`"];


Needs["Yurie`Cluster`"];
Needs["Yurie`Cluster`Common`"];


(* ::Section:: *)
(*Public*)


cluster::usage = 
    "head of cluster.";

clusterQ::usage = 
    "check whether the input is a cluster.";

clusterInit::usage = 
    "initiate cluster and bind to the symbol \"context`clusterName\".";

clusterPropGet::usage = 
    "get property of the cluster.";

clusterPropSet::usage = 
    "set property of the cluster.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$clusterKeyList = {
    "clusterName",
    "planetList",
    "planetCommonData",
    "planetExtraData",
    "planetMergeData",
    "starList",
    "starData",
    "starDefaultList",
    "starDefaultData"
};

$clusterPropList = {
    Splice@$clusterKeyList,
    "property",
    "data"
};


(* ::Subsection:: *)
(*clusterInit*)


clusterInit[
    {clusterName_String,context_String},
    planetList_List,
    planetCommonDataList_List,
    planetExtraDataList_List,
    planetMergeDataList_List
] :=
    With[ {
            clean = ClearAll@Evaluate[context<>clusterName],
            self = ToExpression[context<>clusterName],
            data = AssociationThread[$clusterKeyList->{
                (*"clusterName"*)
                clusterName,
                (*"planetList"*)
                planetList,
                (*"planetCommonData"*)
                AssociationThread[planetList->planetCommonDataList],
                (*"planetExtraData"*)
                AssociationThread[planetList->planetExtraDataList],
                (*"planetMergeData"*)
                AssociationThread[planetList->planetMergeDataList],
                (*"starList"*)
                {},
                (*"starData"*)
                <||>,
                (*"starDefaultList"*)
                {},
                (*"starDefaultData"*)
                AssociationThread[planetList->planetExtraDataList]
            }]
        },
        self = cluster[data]
    ];


(* ::Subsection:: *)
(*cluster*)


cl_cluster[property_] :=
    clusterStrip[cl,property];


(* ::Subsection:: *)
(*clusterStrip*)


clusterStrip[_cluster,"property"] :=
    $clusterPropList;

clusterStrip[cluster[data_],"data"] :=
    data;

clusterStrip[cluster[data_],keyOrItsList_] :=
    data[[keyOrItsList]];


(* ::Subsection:: *)
(*clusterQ*)


clusterQ//Attributes = 
    {HoldFirst};

clusterQ[self_Symbol] :=
    Head@self===cluster;


(* ::Subsection:: *)
(*clusterPropGet*)


clusterPropGet//Attributes = 
    {HoldFirst};

clusterPropGet[self_Symbol,propertyOrItsList_] :=
    clusterStrip[self,propertyOrItsList];


(* ::Subsection:: *)
(*clusterPropSet*)


clusterPropSet//Attributes = 
    {HoldFirst};

clusterPropSet[self_Symbol,keyValueOrItsList_] :=
    With[ {data = clusterStrip[self,"data"]},
        (*the symbol should be rebound to the new cluster.*)
        self = cluster[<|data,keyValueOrItsList|>]
    ];


(* ::Subsection:: *)
(*Atomization*)


cl_cluster/;System`Private`HoldNotValidQ[cl] :=
    (
        System`Private`HoldSetValid[cl];
        System`Private`HoldSetNoEntry[cl]
    );


(* ::Subsection:: *)
(*Format*)


cluster/:MakeBoxes[cl_cluster,format:StandardForm]:=
    With[ {},
        BoxForm`ArrangeSummaryBox[
            (*head*)
            clusterStrip[cl,"clusterName"],
            (*data*)
            cl,
            (*icon*)
            "",
            (*always visible*)
            {
                {
                    BoxForm`SummaryItem[{"Default stars: ",clusterStrip[cl,"starDefaultList"]}]
                },
                {
                    BoxForm`SummaryItem[{"Planet: ",clusterStrip[cl,"planetList"]}]
                }
            },
            (*sometimes visible*)
            {
                {
                    BoxForm`SummaryItem[{"Common: ",clusterStrip[cl,"planetCommonData"]}]
                }
            },
            format,
            "Interpretable"->Automatic
        ]
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
