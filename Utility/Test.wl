(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Test`"];


Needs["Yurie`Cluster`Info`"];


(* ::Section:: *)
(*Public*)


getReport;


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Functions*)


getReport[] :=
    Module[ {report,selectedData},
        report = 
            TestReport@FileNames["*.wlt",$thisTestDir];
        selectedData = 
            report["ResultsDataset"]//
	            If[Normal@#!={},
		            #[All,<|
		                "input"->ToString[#Input],
		                "starList"->Enclose[ConfirmMatch[ReleaseHold[#ActualOutput]["starList"],_List],Missing[]&],
		                "starDefaultList"->Enclose[ConfirmMatch[ReleaseHold[#ActualOutput]["starDefaultList"],_List],Missing[]&],
		                "starDefaultData"->Enclose[ConfirmMatch[ReleaseHold[#ActualOutput]["starDefaultData"],_Association],Missing[]&]
		            |>&]//Dataset[#,MaxItems->{All,All,4}]&
	            ]&;
        CellPrint@{
            ExpressionCell[report["ResultsByOutcome"]//Map[Column]//TabView,"Output"],
            ExpressionCell[selectedData,"Output"]
        };
    ];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];


getReport[];