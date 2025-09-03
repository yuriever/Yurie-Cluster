(* Cluster.wl *)

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


(* Star.wl *)

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