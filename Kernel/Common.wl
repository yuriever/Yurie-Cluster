(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Common`"];


(*clear the state-dependent definitions.*)
(*ClearAll["`*"];*)


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Symbols*)


(* ::Subsection:: *)
(*Utilities*)


messageHideContext::usage = 
    "hide the context in messages.";

clusterDataMerge::usage =
    "merge a list of associations using different merge functions according to keys.";


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Symbols*)


(* ::Subsection:: *)
(*Utilities*)


messageHideContext//Attributes = 
    {HoldFirst};

messageHideContext[args__] :=
    Block[ {Internal`$ContextMarks = False},
        Message[args]
    ];


clusterDataMerge[ruleAssoc_,default:_:Identity][data_List] :=
    clusterDataMergeKernel[data,Normal[Apply/@ruleAssoc],default];

clusterDataMergeKernel[data_,ruleList_,default_] :=
    Module[ {missingToken,assoc,keys,queryRules,mergeRules},
        (*missingToken: unique symbol that is used for identifying where the undefined keys were after transposing the association *)
        mergeRules = 
            Replace[
                Flatten@Replace[
                    ruleList,
                    Verbatim[Rule][list_List,fun_]:>Thread[list->fun],
                    {1}
                ],
                Verbatim[Rule][Key[k_],fun_]:>Rule[k,fun],
                {1}
            ];
        (*avoid KeyUnion if it's not necessary.*)
        If[ SameQ@@Keys[data],
            assoc = data,
            assoc = KeyUnion[DeleteCases[data,<||>],missingToken&]
        ];
        keys = Keys@First@assoc;
        (*this is essentially how GeneralUtilities`AssociationTranspose works.*)
        assoc = 
            AssociationThread[
                keys,
                If[ SameQ@@Keys[data],
                    Transpose@Values[assoc],
                    DeleteCases[Transpose@Values[assoc],missingToken,{2}]
                ]
            ];
        keys = Key/@keys;
        queryRules = 
            DeleteCases[
                Thread[
                    keys->Lookup[mergeRules,keys,default]
                ],
                _->Identity
            ];
        If[ MatchQ[queryRules,{__Rule}],
            Query[queryRules]@assoc,
            assoc
        ]
    ];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
