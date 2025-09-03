(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Cluster`"];


Needs["Yurie`Cluster`"];


(* ::Section:: *)
(*Public*)


cluster::usage =
    "head of cluster.";

clusterQ::usage =
    "check whether the input is a cluster.";

clusterInit::usage =
    "initiate the cluster and bind it to the symbol \"context`clusterName\".";

clusterGet::usage =
    "get property of the cluster.";

clusterSet::usage =
    "set property of the cluster.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$clusterKeyList = {
    "ClusterName",
    "PlanetList",
    "PlanetCommonData",
    "PlanetExtraData",
    "PlanetMergeData",
    "StarList",
    "StarData",
    "StarDefaultList",
    "StarDefaultData"
};


$clusterPropList = {
    Splice@$clusterKeyList,
    "Property",
    "Data"
};


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*cluster*)


cl_cluster[property_] :=
    clusterStrip[cl,property];


(* Atomization *)

cl_cluster/;System`Private`HoldNotValidQ[cl] :=
    (
        System`Private`HoldSetValid[cl];
        System`Private`HoldSetNoEntry[cl]
    );


(* Format *)

cluster/:MakeBoxes[cl_cluster,format:StandardForm]:=
    BoxForm`ArrangeSummaryBox[
        (* Head *)
        clusterStrip[cl,"ClusterName"],
        (* Data *)
        cl,
        (* Icon *)
        "",
        (* Always visible *)
        {
            {
                BoxForm`SummaryItem[{"Default stars: ",clusterStrip[cl,"StarDefaultList"]}]
            },
            {
                BoxForm`SummaryItem[{"Planet: ",clusterStrip[cl,"PlanetList"]}]
            }
        },
        (* Sometimes visible *)
        {
            {
                BoxForm`SummaryItem[{"Common: ",clusterStrip[cl,"PlanetCommonData"]}]
            }
        },
        format,
        "Interpretable"->Automatic
    ];


(* ::Subsubsection:: *)
(*clusterQ*)


clusterQ//Attributes =
    {HoldFirst};

clusterQ[self_Symbol] :=
    Head[self]===cluster;


(* ::Subsubsection:: *)
(*clusterInit*)


clusterInit[
    clusterName_String,
    planetList_List,
    planetCommonDataList_List,
    planetExtraDataList_List,
    planetMergeDataList_List
] :=
    With[ {
            data = AssociationThread[$clusterKeyList->{
                (* "ClusterName" *)
                clusterName,
                (* "PlanetList" *)
                planetList,
                (* "PlanetCommonData" *)
                AssociationThread[planetList->planetCommonDataList],
                (* "PlanetExtraData" *)
                AssociationThread[planetList->planetExtraDataList],
                (* "PlanetMergeData" *)
                AssociationThread[planetList->planetMergeDataList],
                (* "StarList" *)
                {},
                (* "StarData" *)
                <||>,
                (* "StarDefaultList" *)
                {},
                (* "StarDefaultData" *)
                AssociationThread[planetList->planetExtraDataList]
            }]
        },
        cluster[data]
    ];


(* ::Subsubsection:: *)
(*clusterGet*)


clusterGet//Attributes =
    {HoldFirst};

clusterGet[self_Symbol,propertyOrItsList_] :=
    clusterStrip[self,propertyOrItsList];


(* ::Subsubsection:: *)
(*clusterSet*)


clusterSet//Attributes =
    {HoldFirst};

clusterSet[self_Symbol,keyValueOrItsList_] :=
    With[ {data = clusterStrip[self,"Data"]},
        (*the symbol should be rebound to the new cluster.*)
        self = cluster[<|data,keyValueOrItsList|>]
    ];


(* ::Subsection:: *)
(*Helper*)


(* ::Subsubsection:: *)
(*clusterStrip*)


clusterStrip[_cluster,"Property"] :=
    $clusterPropList;

clusterStrip[cluster[data_],"Data"] :=
    data;

clusterStrip[cluster[data_],keyOrItsList_] :=
    data[[keyOrItsList]];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
