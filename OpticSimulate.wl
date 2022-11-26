(* ::Package:: *)

BeginPackage["OpticSimulate`"];

BasicMirror::usage = "BasicMirror[x,y,theta,scale] creates a basic, flat mirror at the specified position & size.";

SimulPhoton::usage = "SimulPhoton[elements, coords] simulates one iteration step of a photon's lifespan";
SimulBeam::usage = "SimulBeam[dims, elements, coords, execLimt] simulates the entire lifespan of a single proton";
GenerateASTs::usage = "GenerateASTs[elements] takes the elements and generates the ASTs for them; this is required before using either Simul- command, but not the Optic- commands.";

Begin["`Private`"];

(* ------ Optics Elements: ------ *)
BasicMirror[x_,y_,theta_,scale_]:=Module[{check, update,  render},
	check[pos_] := Sign[pos[[2]]] != Sign[pos[[2]] + pos[[4]]];
	update[pos_] := Module[{res},
		res = pos;
		res[[4]] = -res[[4]];
		Return[res]
	];
	render := Module[{},
		Return[{
			Black, Thickness[0.01],
			Line[{
				{(-scale Cos[theta]) + x, (-scale Sin[theta]) + y}, {(scale Cos[theta]) + x, (scale Sin[theta]) + y}
			}]
		}]
	];
	
	Return[<|
		"position"->{elX, elY, elTheta, elScale},
		"check"-> check,
		"update"-> update,
		"graphics" -> render
	|>]
]


(* ------ Engine Functions: ------ *)

(* Simulates a single photon *)
SimulPhoton[elements_, coords_] := Module[{pos, elLoc, local},
	pos = coords;
	
	Do[
		(* If we aren't in the radial distance of the element, skip the element. *)
		If[!DistanceCheck[pos, entry["position"]], Continue[]];
		(* Transform to the local space using the proper function *)
		local = entry["g2l"][pos];
		(* Check the element's check function... *)
		If[!entry["check"][local], Continue[]];
		(* Apply the update function to the particle... *)
		local = entry["update"][local];
		(* Convert back to global coordinates. *)
		pos = entry["l2g"][local],
	{entry, elements}];
	
	Return[pos]
]

(* Simulates an entire photon beam *)
SimulBeam[dims_, elements_, coords_, execLimit_]:=Module[{result, photon, execs},
	result = {};
	photon = coords;
	execs = 0;
	
	While[execs < execLimit,
		(* Advance the photon by running the simulation once *)
		photon = SimulPhoton[elements, photon];
		(* Apply the velocity of the photon *)
		photon[[1]]=photon[[1]]+photon[[3]];
		photon[[2]]=photon[[2]]+photon[[4]];
		(* If it's out of bounds, break *)
		If[photon[[1]]<-dims[[1]]/2, Break[]];
		If[photon[[1]]>dims[[1]]/2, Break[]];
		If[photon[[2]]<-dims[[2]]/2, Break[]];
		If[photon[[2]]>dims[[2]]/2, Break[]];
		(* Append the resulting photon's position *)
		AppendTo[result, {photon[[1]],photon[[2]]}];
		(* Increment the execution counter *)
		execs = execs + 1;
	];
	
	Return[result]
]

(* ------ Utility Functions: ------ *)

(* This function creates the matrices representing the global-to-local space transformations, as linear transformations. *)
LocalSpaceTransform[coords_] := Module[{xBase={1,0}, yBase={0,1}, rot,l2g, g2l},
	(* Multiply the basis vectors by the scale *)
	xBase = xBase * coords[[4]];
	yBase = yBase * coords[[4]];
	(* Create the rotation matrix *)
	rot = RotationMatrix[coords[[3]]];
	(* Rotate the basis vectors by theta *)
	xBase = rot . xBase;
	yBase = rot . yBase;
	(* Create the matrices and return them *)
	l2g = Transpose[{xBase, yBase}];
	g2l = Inverse[l2g];
	Return[{l2g, g2l}]
]

(* Get Affine Space Transform: This function returns an expression that can be evaluated at coordinates, which uses the local space transformation matrices at an offset to perform the affine space transformation.*)
GetAST[element_] := Module[{elPos,linears, l2g, g2l},
	elPos = element["position"];
	linears = LocalSpaceTransform[elPos];
	(* Hooray, first-class citizens! *)
	(* Create a matrix whose first column is the offset coordinates and second column is the velocity vector, then apply the g2l matrix, and transpose + flatten the matrix to get {x, y, xVel, yVel}. *)
	g2l[coords_] := Flatten[Transpose[
		linears[[2]] . {{coords[[1]]-elPos[[1]],coords[[3]]},{coords[[2]]-elPos[[2]],coords[[4]]}}
	]];
	(* Reshape the list into a 2x2 matrix, and transpose it so the left column is the position, right column is the velocity, apply the l2g matrix, and then again transpose and flatten to {get x, y, xVel, yVel}, and finally add in the offset. *)
	l2g[coords_] := {elPos[[1]],elPos[[2]],0,0} + Flatten[Transpose[linears[[1]] . Transpose[ArrayReshape[coords, {2,2}]]]];
	Return[{l2g, g2l}]
]

(* Generate the transforms for several elements and insert them into the elements. *)
GenerateASTs[elements_] := Module[{gen, result},
	result={};

	(* Generate the ASTs for each entry and append it to the result. *)
	Do[
		gen=GetAST[entry];
		entry["l2g"]=gen[[1]];
		entry["g2l"]=gen[[2]];
		AppendTo[result, entry];
	, {entry, elements}];

	Return[result]
]

(* A function to do a quick check to see if two points are within a given distance of each other *)
DistanceCheck[coords_, elPos_] := (((coords[[1]]-elPos[[1]]) ^ 2) + ((coords[[2]]-elPos[[2]]) ^ 2)) <= (elPos[[4]] ^ 2)

End[];
EndPackage[];
