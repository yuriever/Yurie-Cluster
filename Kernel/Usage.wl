

(*Cluster.wl*)

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


(*Star.wl*)

starDefine::usage =
	"define the stars.";

starDefault::usage =
	"set the stars into default.";

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