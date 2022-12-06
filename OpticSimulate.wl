(* ::Package:: *)

BeginPackage["OpticSimulate`"]

OpticRenderStatic::usage = "OpticRenderStatic[bounds, elements] - Performs a static final-state simulation in a specified area with the specified elements"
OpticRenderAnimate::usage = "OpticRenderStatic[bounds, elements] - Creates an animation of a specified area with the specified elements"
OpticSimulateResult::usage = "OpticSimulateResult[bounds, elements, sources] - Returns the final position of the particles specified by the sources"
BasicMirror::usage = "BasicMirror[x,y,theta,scale] - Creates an element representing a flat mirror of angle theta from the x-axis, with a given scaling factor (where the default mirror is of length 2)"
ConvexLens::usage = "ConvexLens[x,y,theta,scale,radiusofcurvature]- Creates an element representing a convex mirror"
ConcaveLens::usage = "ConcaveLens[x,y,theta,scale,radiusofcurvature]- Creates an element representing a concave mirror"
CurvedMirror::usage = "CurvedMirror[x,y,theta,scale,radiusofcurvature]- Creates an element representing a curved mirror"

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

CropLists[beams_,t_] := Module[{newbeams},
	newbeams={};
	For[i = 1, i <= Length[beams], i++,
		If[t < Length[beams[[i]]],
			AppendTo[newbeams, Drop[beams[[i]],(t-Length[beams[[i]]])]],
			AppendTo[newbeams, beams[[i]]]
		];
	];
	Return[newbeams];
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

Render[bounds_List, sources_List, beams_List, elements_List] := Module[{graphics},
	graphics = {};
	
	Do[
	
		AppendTo[graphics, sources[[i]][[5]][[1]]];
		AppendTo[graphics, PointSize[sources[[i]][[5]][[2]]]];
		AppendTo[graphics, Point[beams[[i]]]], 
		{i, Length[sources]}
	];
	
	Do[
		tempPos = elem["position"];
		AppendTo[graphics, Translate[Scale[Rotate[elem["graphics"], tempPos[[3]], {0,0}], tempPos[[4]]],{tempPos[[1]],tempPos[[2]]}]],
		{elem, elements}
	];
	
	Return[Graphics[graphics,Frame->True,FrameTicks->Automatic,GridLines->Automatic,PlotRange->{{-(bounds[[1]])/2, (bounds[[1]])/2}, {-bounds[[2]]/2, bounds[[2]]/2}}, PlotRangeClipping->True]];
]

(* ------- Public Engine Functions ------- *)
OpticRenderStatic[bounds_List, elements_List, OptionsPattern[{ExecLimit -> 10000, Sources -> {{-2,1,-3Pi/32,0.02, {Blue, 0.007}}}, CanvasColor -> RGBColor[0.95,0.95,0.95]}]] := Module[{realElems, canvas, source, graphics, velX, velY, tempPos, beams},
	realElems = GenerateASTs[elements];
	
	beams = {};
	
	Do[
		source = sourceInput;
		velX = source[[4]]Cos[source[[3]]];
		velY = source[[4]] Sin[source[[3]]];
		source[[3]] = velX;
		source[[4]] = velY;
		
		AppendTo[beams, SimulBeam[bounds, realElems, source, OptionValue[ExecLimit]]], 
		{sourceInput, OptionValue[Sources]}
	];
	
	Return[Render[bounds, OptionValue[Sources], beams, realElems]];
]

OpticRenderAnimate[bounds_List, elements_List, OptionsPattern[{ExecLimit -> 10000, Sources -> {{-2,1,-3Pi/32,0.02, {Blue, 0.007}}}, CanvasColor -> RGBColor[0.95,0.95,0.95]}]] := Module[{realElems, canvas, source, graphics, velX, velY, tempPos, beams, max},
	realElems = GenerateASTs[elements];
	
	beams = {};
	
	Do[
		source = sourceInput;
		velX = source[[4]]Cos[source[[3]]];
		velY = source[[4]] Sin[source[[3]]];
		source[[3]] = velX;
		source[[4]] = velY;
		
		AppendTo[beams, SimulBeam[bounds, realElems, source, OptionValue[ExecLimit]]], 
		{sourceInput, OptionValue[Sources]}
	];
	
	max = Max[Length /@ beams];
	Print[max];
	Print[CropLists[beams,15]];
	
	Return[Animate[Render[bounds, OptionValue[Sources], CropLists[beams, Floor[t]], realElems], {t, 0, max}, AnimationRate->60]];
]

OpticSimulateResult[bounds_List, elements_List, sources_List, OptionsPattern[{ExecLimit -> 100000}]] := Module[{res, source, velX, velY},
    realElems = GenerateASTs[elements];
    res = {};
    
    Do[
        source = sourceInput;
        velX = source[[4]] Cos[source[[3]]]/100;
        velY = source[[4]] Sin[source[[3]]]/100;
        source[[3]] = velX;
        source[[4]] = velY;
        
        AppendTo[res, SimulBeam[bounds, realElems, source, OptionValue[ExecLimit]][[-1]]],
        {sourceInput, sources}
    ];
    
    Return[res];
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

ConvexLens[elX_,elY_,elTheta_,elScale_,rad_]:=Module[{check, update,  render, upper, lower,dupper,dlower},
	upper[x_] =  Sqrt[rad^2 -(x)^2] - Sqrt[rad^2-1^2];
	lower[x_] = -Sqrt[rad^2 -(x)^2] + Sqrt[rad^2-1^2];
	dupper[x_]  = x / Sqrt[rad^2 - x^2];
	dlower[x_] = -x / Sqrt[rad^2 - x^2];
	
	check[pos_] := Module[{},
		Return[
			Sign[pos[[2]] - upper[pos[[1]]]] != Sign[pos[[2]] + pos[[4]] - upper[pos[[1]] + pos[[3]]]] (* If it crosses the upper *)
			||Sign[pos[[2]] - lower[pos[[1]]]] != Sign[pos[[2]] + pos[[4]] - lower[pos[[1]] + pos[[3]]]](* If it crosses the lower *)
		]
	];
	
    update[pos_] := Module[{res, crossUpper, angle, rotMat, rotVel, mag, \[Theta]i, \[Theta]r, newVel},
		res = pos;
		(* find angle at that x value *)
		crossUpper = Sign[pos[[2]] - upper[pos[[1]]]]!=Sign[pos[[2]] + pos[[4]] - upper[pos[[1]] + pos[[3]]]];
		angle = ArcTan[If[crossUpper, dupper, dlower][pos[[1]] + pos[[3]]/2]];
		
		(* rotate the velocity vetor by that angle *)
		rotMat = RotationMatrix[-angle];
		rotVel= rotMat . {res[[3]], res[[4]]};
		mag = Norm[rotVel];
		
		(* find angle of incidence and angle of refraction *)
		\[Theta]i = ArcCos[Dot[rotVel,{0, Sign[rotVel[[2]]]}]/Norm[rotVel]];
		\[Theta]r = ArcSin[If[crossUpper, Sin[\[Theta]i]/1.8, Sin[\[Theta]i*1.8]]];
		
		(* create new velocity vector *)
		newVel = {mag * Sin[\[Theta]r] * Sign[rotVel[[1]]], mag * Cos[\[Theta]r]*Sign[rotVel[[2]]]};
		newVel = RotationMatrix[angle] . newVel;
		
		(* insert new velocity into pos and return it *)
		res[[3]] = newVel[[1]];
		res[[4]] = newVel[[2]];
		
		Return[res];
	];

	Return[<|
		"position"->{elX, elY, elTheta, elScale},
		"check"-> check,
		"update"-> update,
		"graphics" -> {Black, Thickness[0.01 / elScale], ExpLine[upper[#] &,{-1,1}],ExpLine[lower[#] &,{-1,1}]}
	|>]
]

ConcaveLens[elX_,elY_,elTheta_,elScale_,rad_]:=Module[{check, update,  render, upper, lower,dupper,dlower},
	upper[x_] =  -Sqrt[rad^2 -(x)^2] + Sqrt[rad^2-1^2] + 0.5;
	lower[x_] = Sqrt[rad^2 -(x)^2] - Sqrt[rad^2-1^2] - 0.5;
	dupper[x_]  = -x / Sqrt[rad^2 - x^2];
	dlower[x_] = x / Sqrt[rad^2 - x^2];
	
	check[pos_] := Module[{},
		Return[
			Sign[pos[[2]] - upper[pos[[1]]]] != Sign[pos[[2]] + pos[[4]] - upper[pos[[1]] + pos[[3]]]] (* If it crosses the upper *)
			||Sign[pos[[2]] - lower[pos[[1]]]] != Sign[pos[[2]] + pos[[4]] - lower[pos[[1]] + pos[[3]]]](* If it crosses the lower *)
		]
	];
	
    update[pos_] := Module[{res, crossUpper, angle, rotMat, rotVel, mag, \[Theta]i, \[Theta]r, newVel},
		res = pos;
		(* find angle at that x value *)
		crossUpper = Sign[pos[[2]] - upper[pos[[1]]]]!=Sign[pos[[2]] + pos[[4]] - upper[pos[[1]] + pos[[3]]]];
		angle = ArcTan[If[crossUpper, dupper, dlower][pos[[1]] + pos[[3]]/2]];
		
		(* rotate the velocity vetor by that angle *)
		rotMat = RotationMatrix[-angle];
		rotVel= rotMat . {res[[3]], res[[4]]};
		mag = Norm[rotVel];
		
		(* find angle of incidence and angle of refraction *)
		\[Theta]i = ArcCos[Dot[rotVel,{0, Sign[rotVel[[2]]]}]/Norm[rotVel]];
		\[Theta]r = ArcSin[If[crossUpper, Sin[\[Theta]i]/1.8, Sin[\[Theta]i*1.8]]];
		
		(* create new velocity vector *)
		newVel = {mag * Sin[\[Theta]r] * Sign[rotVel[[1]]], mag * Cos[\[Theta]r]*Sign[rotVel[[2]]]};
		newVel = RotationMatrix[angle] . newVel;
		
		(* insert new velocity into pos and return it *)
		res[[3]] = newVel[[1]];
		res[[4]] = newVel[[2]];
		
		Return[res];
	];

	Return[<|
		"position"->{elX, elY, elTheta, elScale},
		"check"-> check,
		"update"-> update,
		"graphics" -> {Black, Thickness[0.01 / elScale], ExpLine[upper[#] &,{-1,1}], ExpLine[lower[#] &,{-1,1}], Line[{{-1,upper[-1]},{-1,lower[-1]}}], Line[{{1,upper[1]},{1,lower[1]}}]}
	|>]
]

CurvedMirror[elX_,elY_,elTheta_,elScale_,rad_]:= Module[{check, update, render, expr, dexp},
	(* expressions for lens and derivative of lens *)
	expr[x_] =  Sqrt[(rad^2) -((x)^2)] - Sqrt[(rad^2)-(1^2)];
	dexp[x_] = x / Sqrt[(rad^2) - (x^2)];
	
	(* checks the position of the partice to see if it has crossed over the mirror *)
	check[pos_] := Module[{result, before, after}, 
	    before = pos[[2]] - expr[pos[[1]]];
	    after = pos[[2]] + pos[[4]] - expr[pos[[1]]+pos[[3]]];
	    result = (Sign[before] != Sign[after]);
		Return[result]
	];
	
	(* reflects the partice, if hits the mirror, gets angle, rotates axis, flips y coord, flips axis back + updates pos *)
	update[pos_] := Module[{xnew, \[Theta], velNew, res},
	
	    (*takes the avg of the xvalues, to get the new one, finds the angle between that vector and the x axis*)
	    xnew = pos[[1]]+(pos[[3]]/2);
		\[Theta] = ArcTan[dexp[xnew]];
		
		(*gets new velocity, by rotating the entire graph about theta (found above), reversing the y-coordinate, and rotating the graph back*)
		velNew=RotationMatrix[\[Theta]] . {pos[[3]], pos[[4]]};
		velNew[[2]]=-velNew[[2]];
		velNew=RotationMatrix[-\[Theta]] . velNew;
		
		(*updtaes the position of the particle*)
		res=pos;
		res[[3]]=velNew[[1]];
		res[[4]]=velNew[[2]];
		
		Return[res]
	];
	
	Return[<|
		"position"->{elX, elY, elTheta, elScale},
		"check"-> check,
		"update"-> update,
		"graphics" -> {Black, Thickness[0.01 / elScale], ExpLine[expr[#]&,{-1,1}]}
	|>]
]

End[]

EndPackage[]









