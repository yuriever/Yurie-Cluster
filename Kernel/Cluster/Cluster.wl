(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Cluster`"];


Needs["Yurie`Cluster`"];
Needs["Yurie`Cluster`Common`"];


(* ::Section:: *)
(*Public*)


cluster;
clusterInit;
clusterPropGet;
clusterPropSet;


clusterValidQ::usage = 
    "whether the input is a valid cluster.";

clusterPropGetUnsafe::usage = 
	"clusterPropGet without clusterValidQ.";

clusterPropSetUnsafe::usage = 
	"clusterPropSet without clusterValidQ.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Messages*)


clusterValidQ::undef = 
	"`` is not a valid cluster."


(* ::Subsection:: *)
(*Constants*)


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

$clusterKeySortedList = 
	Sort@$clusterKeyList;

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
(*clusterPropGet*)


cl_cluster[property_]:=
    clusterStrip[cl,property];


clusterStrip[_cluster,"property"] :=
    $clusterPropList;

clusterStrip[cluster[data_],"data"] :=
    data;

clusterStrip[cluster[data_],keyOrItsList_] :=
    data[[keyOrItsList]];


clusterValidQ//Attributes = 
    {HoldFirst};

clusterValidQ[self_Symbol] :=
	Head@self===cluster;

(*clusterValidQ[self_Symbol] :=
    Module[ {data},
        Head@self===cluster&&(
        	data = clusterStrip[self,"data"];
            Sort@Keys@data===$clusterKeySortedList&&Sort@data["starList"]===Sort@Keys@data["starData"]
    	)
    ];*)


clusterPropGetUnsafe//Attributes = 
    {HoldFirst};

clusterPropGetUnsafe[self_,propertyOrItsList_] :=
    clusterStrip[self,propertyOrItsList];


clusterPropSetUnsafe//Attributes = 
    {HoldFirst};

(*the symbol should be rebound to the new cluster.*)
clusterPropSetUnsafe[self_,keyValueOrItsList_] :=
    With[ {data = clusterStrip[self,"data"]},
        self = cluster[<|data,keyValueOrItsList|>]
    ];


clusterPropGet//Attributes = 
    {HoldFirst};

clusterPropGet[self_Symbol?clusterValidQ,propertyOrItsList_]:=
	clusterStrip[self,propertyOrItsList];


clusterPropSet//Attributes = 
    {HoldFirst};

(*the symbol should be rebound to the new cluster.*)
clusterPropSet[self_Symbol?clusterValidQ,keyValueOrItsList_] :=
    With[ {data = clusterStrip[self,"data"]},
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
