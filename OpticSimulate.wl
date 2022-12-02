(* ::Package:: *)

BeginPackage["OpticSimulate`"]

OpticRenderStatic::usage = "OpticRenderStatic[bounds, elements] - Performs a static final-state simulation in a specified area with the specified elements"
BasicMirror::usage = "BasicMirror[x,y,theta,scale] - Creates an element representing a flat mirror of angle theta from the x-axis, with a given scaling factor (where the default mirror is of length 2)"
ConvexLens::usage= "ConvexLens[x,y,theta,scale]- Creates an element represneting a convex mirror"
SimulBeam::usage = "SimulBeam - simulates a potato."
SimulPhoton::usage = "bagels"
GenerateASTs::usage = "fizz"

Begin["`Private`"]

(* ------- Utility Functions ------- *)
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
	g2l[coords_] :=Flatten[Transpose[
		linears[[2]] . {{coords[[1]]-elPos[[1]],coords[[3]]},{coords[[2]]-elPos[[2]],coords[[4]]}}
	]];
	(* Reshape the list into a 2x2 matrix, and transpose it so the left column is the position, right column is the velocity, apply the l2g matrix, and then again transpose and flatten to {get x, y, xVel, yVel}, and finally add in the offset. *)
	l2g[coords_]:={elPos[[1]],elPos[[2]],0,0} +Flatten[Transpose[linears[[1]] . Transpose[ArrayReshape[coords, {2,2}]]]];
	Return[{l2g, g2l}]
]

(* Generate the transforms for several elements and insert them into the elements. *)
GenerateASTs[elements_] := Module[{gen, result},
	result = {};
	
	Do[
		gen = GetAST[entry];
		entry["l2g"] = gen[[1]];
		entry["g2l"] = gen[[2]];
		AppendTo[result, entry];
	, {entry, elements}];
	
	Return[result]
]

(* A function to do a quick check to see if two points are within a given distance of each other *)
DistanceCheck[coords_, elPos_] := (((coords[[1]]-elPos[[1]]) ^ 2) + ((coords[[2]]-elPos[[2]]) ^ 2)) <= (elPos[[4]] ^ 2)

(* Generates a line object given an expression. *)
(* Example: ExpLine[Sin[#] &, {-1, 1}] would create the line from -1 to 1 of Sin[x].*)
ExpLine[exp_, bounds_] := Module[{step, range, points},
    step = (bounds[[2]] - bounds[[1]]) / 1000;
    range = Range[bounds[[1]], bounds[[2]] - step, step];
    points = {{#, exp[#]}, {# + step, exp[# + step]}} & /@ range;
    Return[Line[points]]
]

(* ------- Engine Implementation Functions -------*)
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

(* ------- Public Engine Functions ------- *)
OpticRenderStatic[bounds_List, elements_List, OptionsPattern[{ExecLimit->10000,Sources->{{-2,1,-3Pi/32,0.02, {Blue, 0.01}}}, CanvasColor->LightGray}]] := Module[{realElems, canvas, source, res, graphics, velX, velY, tempPos},
	realElems = GenerateASTs[elements];
	canvas = {OptionValue[CanvasColor], Rectangle[{-(bounds[[1]])/2, -(bounds[[2]])/2}, {bounds[[1]]/2, bounds[[2]]/2}]};
	
	source = OptionValue[Sources][[1]];
	velX = source[[4]]Cos[source[[3]]];
	velY = source[[4]] Sin[source[[3]]];
	source[[3]] = velX;
	source[[4]] = velY;
	
	res = SimulBeam[bounds, realElems, source, OptionValue[ExecLimit]];
	
	graphics = {canvas, source[[5]][[1]], PointSize[source[[5]][[2]]], Point[res]};
	Do[
		tempPos = elem["position"];
		AppendTo[graphics, Translate[Scale[Rotate[elem["graphics"], tempPos[[3]]], tempPos[[4]]],{tempPos[[1]],tempPos[[2]]}]]
	,{elem, realElems}];
	
	Return[Graphics[graphics]];
]

(* ------- Element Functions ------- *)
BasicMirror[elX_,elY_,elTheta_,elScale_]:=Module[{check, update,  render},
	check[pos_] := Sign[pos[[2]]]!=Sign[pos[[2]] + pos[[4]]];
	update[pos_] := Module[{res},
		res = pos;
		res[[4]] = -res[[4]];
		Return[res]
	];
	Return[<|
		"position"->{elX, elY, elTheta, elScale},
		"check"-> check,
		"update"-> update,
		"graphics" -> {Black, Thickness[0.01 / elScale], Line[{{-1,0},{1,0}}]} 
	|>]
]
ConvexLens[elX_,elY_,elTheta_,elScale_,rad_]:=Module[{check, update,  render},
	check[pos_] := Module[{exp1, exp2},
		exp1[x_] =  Sqrt[rad^2 -(x)^2] - Sqrt[rad^2-1^2];
		exp2[x_] = -Sqrt[rad^2 -(x)^2] + Sqrt[rad^2-1^2];
		Return[
			Sign[pos[[2]]-exp1[pos[[1]]]]!=Sign[pos[[2]]+pos[[4]]-exp1[pos[[1]]+pos[[3]]]]
			||Sign[pos[[2]]-exp2[pos[[1]]]]!=Sign[pos[[2]]+pos[[4]]-exp2[pos[[1]]+pos[[3]]]
		]]
	];
    update[pos_] := Module[{res},
		res = pos;
		\[Theta]1=ArcTan[res[[4]]/res[[3]]];
		\[Theta]2=ArcTan[1/D[Sqrt[rad^2-(res[[1]])^2]-Sqrt[rad^2-1^2]]];
		\[Theta]i=\[Theta]1-\[Theta]2;
		\[Theta]r=\[Theta]i/1.8;
		res[[3]]=Sqrt[(res[[3]])^2+(res[[4]])^2]*Cos[\[Theta]r];
		res[[4]]=Sqrt[(res[[3]])^2+(res[[4]])^2]*Sin[\[Theta] r];
		Return[res]
	];

	Return[<|
		"position"->{elX, elY, elTheta, elScale},
		"check"-> check,
		"update"-> update,
		"graphics" -> {Black, Thickness[0.01 / elScale], ExpLine[Sqrt[rad^2-#^2]-Sqrt[rad^2-elScale^2]&,{-elScale,elScale}],ExpLine[-Sqrt[rad^2-#^2]+Sqrt[rad^2-elScale^2]&,{-elScale,elScale}]}
	|>]
]
Mirrors[elX_,elY_,elTheta_,elScale_,rad_]:= Module[{check, update, render},
	check[pos_]:= Module[{f1, f2}, 
		exp[x_] = Sqrt[rad^2 - x^2] - Sqrt[rad^2 - 1];
		Return[
			Sign[pos[[2]] - exp[pos[[1]]]] != 
			Sign[pos[[2]] + pos[[4]] - exp[pos[[1]] + pos[[3]]]]
		]
	];
	update[pos_] := Module[{}
	
	]
]


End[]

EndPackage[]






