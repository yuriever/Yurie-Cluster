(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Cluster`Info`"];


(* ::Section:: *)
(*Public*)


$thisPacletDir;
$thisKernelDir;
$thisTestDir;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


$thisPaclet = 
    PacletObject["Yurie/Cluster"];
    
$thisPacletDir = 
    $thisPaclet["Location"];

$thisKernelDir = 
    FileNameJoin@{$thisPacletDir,"Kernel"};

$thisTestDir = 
    $thisPaclet["AssetLocation","Test"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
