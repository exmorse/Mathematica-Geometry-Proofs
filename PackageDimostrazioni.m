(***  Universita' di Bologna, Corsi di Laurea Magistrale in Informatica e Matematica  ***)
(***  Progetto di Matematica Computazionale e Calcolo Numerico e Software Didattico  ***)
(***  Anno Accademico 2016/2017 ***)
(***  Autori: Anna Giuffre', Filippo Morselli, Gloria Teggi  ***)

(*
	Il sistema e' stato sviluppato principalmente usando Mathematica 10.4 su un sistema Ubuntu 14.10 64 bit, CPU Intel i5-3230M, 8 GB RAM
	Il corretto funzionamento e' stato testato anche con Mathematica 11 e 10.3 su sistemi Windows e Mac
*)



BeginPackage["Dimostrazioni`"]

InitPackage::usage =
	"Esegue l'inizializzazione delle strutture necessarie al package per le dimostrazioni."
CreateProblem::usage =
	"Mostra l'interfaccia per la creazione di un problema e la sua successiva dimostrazione.\nE' necessario invocare prima l'inizializzazione, ossia InitPackage[]."


Begin["Private`"]



(* Le seguenti funzioni gestiscono le strutture base usate in tutto il package: punti, segmenti, angoli e triangoli *)

(*** PUNTI ***)
(* Schema della struttura dei punti  -->  {"Nome", {CoordX, CoordY}} *)

(* Dato un punto, ne restituisce il nome *)
GetPointLabel[{Label_, {x_, y_}}]:=
	Label;

(* Dato un punto, ne restituisce un elemento Text, con il nome del punto e le coordinate
		in cui il nome dovra' essere disegnato nella figura *)
GetPointLabelAsText[{Label_, {x_, y_}}]:=
	Module[
		(* graphicRange indica la dimensione di disegno della figura, da -graphicRange a +graphicRange sia su X che su Y*)
		{graphicRange = 10, lx, ly},
		(* La posizione del nome e' proporzionale alle coordinate del punto stesso *)
		(* Incremento se e' troppo vicino allo (0,0) *)
		If[
			-graphicRange/2 < x < graphicRange/2,
			If[
				x >= 0,
				lx = x + 0.1 * x + 0.5,
				lx = x + 0.1 * x - 0.5
			],
			lx = 0.1 * x + x;
		];
		If[
			-graphicRange/2 < y < graphicRange/2,
			If[
				y >= 0,
				ly = y + 0.1 * y + 0.5,
				ly = y + 0.1 * y - 0.5
			],
			ly = 0.1 * y + y;
		];
		Text[Label, {lx, ly}]
	]

(* Dato un punto, ne restituisce le coordinate *)
GetPointCoordinates[{Label_, {x_, y_}}] :=
	{x, y};



(*** SEGMENTI ***)
(* Schema della struttura dei segmenti  -->  {{"A", {1, 1}}, {"B", {0,0}}}  *)
(* E' una lista di esattamente due strutture punti *)

(* Dato un segmento, ne restituisce il nome, ossia la concatenazione dei nomi dei punti *)
GetSegmentLabel[{{Label1_, {x1_, y1_}}, {Label2_, {x2_, y2_}}}]:=
	StringJoin[Label1, Label2];

(* Dato un segmento, ne restituisce un elemento Line, che sara' poi disegnato nella figura *)
GetSegmentLine[{{Label1_, {x1_, y1_}}, {Label2_, {x2_, y2_}}}] :=
	Line[{{x1, y1}, {x2, y2}}];

(* Dato un segmento, ne restituisce la lunghezza *)
GetSegmentLength[{{Label1_, {x1_, y1_}}, {Label2_, {x2_, y2_}}}] :=
	Sqrt[Abs[x1-x2]^2  +  Abs[y1-y2]^2];

(* Dati due segmenti, la funzione restituisce un punto in comune fra gli estremi dei segmenti;
	restituisce False nel caso non ci siano estremi in comune *)
PointInCommon[{p1_, p2_}, {p1_, t2_}] := p1;
PointInCommon[{p1_, p2_}, {t1_, p1_}] := p1;
PointInCommon[{p1_, p2_}, {p2_, t2_}] := p2;
PointInCommon[{p1_, p2_}, {t1_, p2_}] := p2;
PointInCommon[{p1_, p2_}, {t1_, t2_}] = False;

(* Riordina alfabeticamente le lettere che compongono il nome del segmento *)
OrderSegmentLabel[l_ /; StringLength[l]==2] :=
	StringJoin[Sort[Characters[l]]];



(*** ANGOLI ***)
(* Schema della struttura dei segmenti  -->  {{{"A", {1, 1}}, {"B", {0,0}}}, {{"A", {1, 1}}, {"C", {3,2}}}}  *)
(* E' una lista di esattamente due strutture segmenti *)

(* Dato un angolo, restituisce il nome dell'angolo *)
(* Il nome dell'angolo e' dato dai nomi dei tre punti coinvolti *)
GetAngleLabel[{seg1_, seg2_}] :=
	Module[
		{pm, p1, p2, pf1, pf2},

		(* pm e' il punto in comune fra i due segmenti che formano l'angolo *)
		(* p1 e p2 sono gli altri due punti *)
		pm = PointInCommon[seg1, seg2];
		p1 = Select[Not[MatchQ[PointInCommon[seg1, seg2],#]]&][seg1][[1]];
		p2 = Select[Not[MatchQ[PointInCommon[seg1, seg2],#]]&][seg2][[1]];

		(* Ordino i nomi degli altri due punti *)
		{pf1, pf2} = Sort[{GetPointLabel[p1], GetPointLabel[p2]}];
		pf1 ~~ GetPointLabel[pm] ~~ pf2
];

(* Dato il nome di un angolo lo riordina *)
OrderAngleLabel[label_] :=
	Module[
		{p1, p2, p3, chars},

		chars = Characters[label];
		If[
			Length[chars] != 3,
			label,
			(* Tiene fisso il punto di mezzo e riordino gli altri due *)
			{p1, p2, p3} = chars;
			{p1, p3} = Sort[{p1, p3}
		];
		p1~~p2~~p3
		]
];

(* Dato un angolo la funzione ritorna l'elemento grafico che indica l'angolo in figura.
	 Questa funzione sara' chiamata all'interno di una Graphics.
	 Ritorna un arco della stessa ampiezza dell'angolo in input *)
GetDrawingAngle[{seg1_, seg2_}] :=
	Module[
		{a,b,c,phi1,phi2, pm, p1, p2, pml, pl1, pl2},

		(* pm e' il punto in comune fra i due segmenti che formano l'angolo *)
		(* p1 e p2 sono gli altri due punti *)
		pm = PointInCommon[seg1, seg2];
		p1 = Select[Not[MatchQ[PointInCommon[seg1, seg2],#]]&][seg1][[1]];
		p2 = Select[Not[MatchQ[PointInCommon[seg1, seg2],#]]&][seg2][[1]];

		(* Ottiene i nomi dei punti coinvolti *)
		pml = GetPointLabel[pm];
		pl1 = GetPointLabel[p1];
		pl2 = GetPointLabel[p2];

		{a,b,c} = Take[
			RotateLeft[
				{
					GetPointCoordinates[pm],
		 			GetPointCoordinates[p1],
		 			GetPointCoordinates[p2]
				},
				-1
			],
			3
		];

		(* Calcolo dell'angolo in cui inizare e terminare il disegno dell'arco *)
		{phi1,phi2} = Sort@N[{ArcTan@@(a-b),ArcTan@@(c-b)}];
		If[phi2-phi1>Pi,phi1+=2 Pi];

		{
			(* Se sono presenti condizioni di equivalenza sull'angolo lo si disegna del colore corrispondente,
			 	 altrimenti si colora di nero *)
			If[
				MatchQ[Position[angleEqualityStruct, pl1~~pml~~pl2], {}],
				Black,
				colorAngleList[[Position[angleEqualityStruct, pl1~~pml~~pl2][[1, 1]]]]
			],
			AbsoluteThickness[3],
			Circle[
				b,
				(* Se l'angolo e' di 180 gradi non lo disegno *)
				If[
					Abs[
						(* Con VectorAngle ottengo l'ampiezza dell'angolo, aggiustando le coordinate degli altri due punti in base alla posizione del punto in comune,
						 		in modo da traslarli in riferimento all'origine *)
						VectorAngle[
							{GetPointCoordinates[p1][[1]] - GetPointCoordinates[pm][[1]], GetPointCoordinates[p1][[2]] - GetPointCoordinates[pm][[2]]},
							{GetPointCoordinates[p2][[1]] - GetPointCoordinates[pm][[1]], GetPointCoordinates[p2][[2]] - GetPointCoordinates[pm][[2]]}
						] - Pi
					] < 10^(-2),
					0,
					(* Rendo il raggio del semicerchio proporzionale all'ampiezza dell'angolo *)
					0.2 * (VectorAngle[
						{GetPointCoordinates[p1][[1]] - GetPointCoordinates[pm][[1]], GetPointCoordinates[p1][[2]] - GetPointCoordinates[pm][[2]]},
						{GetPointCoordinates[p2][[1]] - GetPointCoordinates[pm][[1]], GetPointCoordinates[p2][[2]] - GetPointCoordinates[pm][[2]]}
					])  +  0.5
				],
				(* Estremi del disegno dell'arco *)
				{phi2,phi1}
			]
		}
	];



(*** TRIANGOLI ***)
(* Lo schema della struttura dei segmenti e' una lista di esattamente tre strutture segmenti *)

(* Funzione che ritorna la lista di tutti i triangoli in figura dopo la generazione della figura corretta *)
GetTriangles[] :=
	Module[
		{segSubsets, ptsLabels, triangles},

		triangles = {};
		(* Insieme di tutti gli insiemi di tre segmenti in figura *)
		segSubsets = Subsets[finalSegments[[All, 1]], {3}];

		For[
			i = 1,
			i <= Length[segSubsets],
			i++,
			If[
				(* Seleziono solo gli insiemi con segmenti distinti fra loro *)
				Length[Union[Flatten@segSubsets[[i]][[All, All, 1]]]] == 3,
				If[
					(* Se vale anche la disuguaglianza triangolare allora lo aggiungo alla lista di triangoli *)
					CheckTriangleInequality[segSubsets[[i]]],
					triangles = Append[triangles, segSubsets[[i]]],
					Null
					],
					Null
			]
		];
		triangles
	];

(* Dato un triangolo ritorno il nome del triangolo in input *)
GetTriangleLabel[{seg1_, seg2_, seg3_}] :=
	Module[
		{},
		(* Si esegue l'unione di tutti i punti coinvolti *)
		StringJoin[
			Union[
				Characters[GetSegmentLabel[seg1]],
				Characters[GetSegmentLabel[seg2]],
				Characters[GetSegmentLabel[seg3]]
			]
		]
	];

(* Data una coppia di segmenti restituisce il nome dell'angolo che essi formano.
 	 Ritorna una lista vuota se non formano un angolo *)
GetAngleLabelBetweenSegments[sl1_, sl2_] :=
	Module[
		{c1, c2, pic, p1, p2},
		c1 = Characters[sl1];
		c2 = Characters[sl2];
		(* Cerca punti in comune *)
		pic = Intersection[c1, c2];

		(* Se non ci sono punti in comune ritorna l'insieme vuoto *)
		If[MatchQ[pic, {}], Return[{}]];

		(* Ottengo gli altri due punti *)
		p1 = Complement[c1, pic];
		p2 = Complement[c2, pic];

		(* Ordino i punti e restituisco *)
		OrderAngleLabel[StringJoin[{p1, pic, p2}]]
	];

(* Verifica che valga la disuguaglianza triangolare fra i tre segmenti che prende in input *)
(* Usato per determinare gli effettivi triangoli in figura *)
CheckTriangleInequality[{seg1_, seg2_, seg3_}]:=
	Module[
		{l1, l2, l3, threshold},

		(* Soglia di errore, per tollerare approssimazioni *)
		threshold =  10^(-6);

		(* Calcolo le lunghezze dei segmenti *)
		l1 = GetSegmentLength[seg1];
		l2 = GetSegmentLength[seg2];
		l3 = GetSegmentLength[seg3];

		(* Ritorna True se la condizione e' verificata, altrimenti False *)
		(l1 + l2 - l3) > threshold && (l2 + l3 - l1) >threshold && (l1 + l3 - l2) > threshold
	];



(*** AUSILIARIE ***)
(* Estrae il contenuto di una StringExpression *)
(* Usata per la conversione in stringa *)
ExtractFromStringExpression[StringExpression[x_]] :=
	x;



(*** GESTIONE IPOTESI DEL PROBLEMA ***)

(* Le seguenti funzioni hanno il compito di analizzare le stringhe di input che rappresentano i dati del problema di geometria *)
(* Quando si verifica un match con uno dei pattern previsti viene generata la corrispondente condizione da imporre sui punti coinvolti,
 		che verra' tenuta in considerazione durante la creazione della figura corretta del problema *)

(* Condizone di Punto Medio --- "P Punto Medio AB" *)
ParseHypothesis[hp_String] /; StringMatchQ[hp, pm_ ~~ "PuntoMedio" ~~ p1_ ~~ p2_]:=
	Module[
		{},
		(* Chiamata alla creazione del vincolo di punto medio *)
		SetSegmentMidPoint[
			ExtractFromStringExpression[
				StringReplace[
					hp,
					pm_ ~~ "PuntoMedio" ~~ p1_ ~~ p2_ -> {pm, p1, p2}
				]
			]
		]
	];

(* Condizione di uguaglianza fra angoli, con eventuali moltiplicatori --- "2*ABC = DEF" *)
ParseHypothesis[hp_String] /; StringMatchQ[
	hp,
 	Shortest[((m1__~~"*")|"")] ~~ p1_  ~~ p2_ ~~p3_~~"="~~Shortest[((m2__~~"*")|"")]~~ p4_~~ p5_ ~~ p6_ /;
		Not[StringMatchQ[p1, DigitCharacter]] && Not[StringMatchQ[p4, DigitCharacter]]
]:=
	Module[
		{},
		(* Chiamata alla creazione del vincolo di uguaglianza fra angoli *)
		SetAngleEqualityCondition[
			ExtractFromStringExpression[
				StringReplace[
					hp,
				 	Shortest[((am1__~~"*")|"")] ~~ p1_  ~~ p2_ ~~p3_~~"="~~Shortest[((am2__~~"*")|"")]~~ p4_~~ p5_ ~~ p6_ /;
						Not[StringMatchQ[p1, DigitCharacter]] && Not[StringMatchQ[p4, DigitCharacter]]
						-> {am1,{p1, p2, p3}, am2,{p4,p5, p6}}
				]
			]
		]
	];

(* Condizione di uguaglianza fra segmenti, con eventuali moltiplicatori --- "2*AB = CD" *)
ParseHypothesis[hp_String] /; StringMatchQ[
	hp,
 	Shortest[((m1__~~"*")|"")] ~~ p1_  ~~ p2_ ~~"="~~Shortest[((m2__~~"*")|"")]~~ p3_~~ p4_ /;
		Not[StringMatchQ[p3, DigitCharacter]] && Not[StringMatchQ[p1, DigitCharacter]]
]:=
	Module[
		{},
		(* Chiamata alla creazione del vincolo di uguaglianza fra segmenti *)
		SetSegmentEqualityCondition[
			ExtractFromStringExpression[
				StringReplace[
					hp,
				 	Shortest[((sm1__~~"*")|"")] ~~ p1_  ~~ p2_ ~~"="~~Shortest[((sm2__~~"*")|"")]~~ p3_~~ p4_ /;
						Not[StringMatchQ[p3, DigitCharacter]] && Not[StringMatchQ[p1, DigitCharacter]]
						-> {sm1,{p1, p2}, sm2,{p3, p4}}
				]
			]
		]
	];

(* Condizione di appartenenza di un punto a un segmento --- "P Appartiene AB" *)
ParseHypothesis[hp_String] /; StringMatchQ[
	hp,
 	pm_ ~~ "Appartiene" ~~ p1_ ~~ p2_
]:=
	Module[
		{},
		(* Chiamata alla creazione del vincolo di appartenenza *)
		SetSegmentBelongsCondition[
			ExtractFromStringExpression[
				StringReplace[
					hp,
				 	pm_ ~~ "Appartiene" ~~ p1_ ~~ p2_ -> {pm, p1, p2}
				]
			]
		]
	];

(* Condizione di perpendicolarita' fra segmenti--- "AB Perpendicolare CD" *)
ParseHypothesis[hp_String] /; StringMatchQ[
	hp,
 	p1_ ~~ p2_~~ "Perpendicolare" ~~ p3_ ~~ p4_
]:=
	Module[
		{},
		(* Chiamata alla creazione del vincolo di perpendicolarita' *)
		SetPerpendicularAngleCondition[
			ExtractFromStringExpression[
				StringReplace[
					hp,
	 				p1_ ~~ p2_~~ "Perpendicolare" ~~ p3_ ~~ p4_ -> {p1, p2, p3, p4}
				]
			]
		]
	];

(* Condizione di uguaglianza all'angolo retto --- "ABC Retto" *)
ParseHypothesis[hp_String] /; StringMatchQ[
	hp,
 	p1_ ~~ p2_~~ p3_ ~~ "Retto"
]:=
	Module[
		{},
	  (* Chiamata alla creazione del vincolo di uguaglianza all'angolo retto *)
		SetRightAngleCondition[
			ExtractFromStringExpression[
				StringReplace[
					hp,
	 				p1_ ~~ p2_~~ p3_ ~~ "Retto" -> {p1, p2, p3}
				]
			]
		]
	];

(* Condizione di uguaglianza all'angolo piatto --- "ABC Piatto" *)
ParseHypothesis[hp_String] /; StringMatchQ[
	hp,
 	p1_ ~~ p2_~~ p3_ ~~ "Piatto"
]:=
	Module[
		{},
		(* Chiamata alla creazione del vincolo di uguaglianza all'angolo piatto *)
		SetFlatAngleCondition[
			ExtractFromStringExpression[
				StringReplace[
					hp,
	 			p1_ ~~ p2_~~ p3_ ~~ "Piatto" -> {p1, p2, p3}
				]
			]
		]
	];

(* In tutti gli altri casi si fa match con questo caso, che genera un errore di istruzione non riconosciuta *)
ParseHypothesis[hp_String] :=
	Module[
		{},
		(* Eseguo la funzione di istruzione non riconosciuta *)
		InstructionNotRecognized["Istruzione " ~~ hp]
	];



(*** CREAZIONE DEI VINCOLI SULLA FIGURA ***)

(* Le seguenti funzioni creano i vincoli sui punti finali della figura.
	 problemPoints --- sono i punti che l'utente ha inserito durante la creazione del problema
	 finalPoints --- sono i punti della figura finale, uno per ogni problemPoints
	 Le condizioni sono imposte sui finalPoints.

	 La funzione di creazione della figura finale impone i vincoli sui finalPoints, minimizzando
	 la distanza dai corrispettivi problemPoints *)

(* Dati tre nomi di punti, impone il vincolo di pml punto medio del segmento ptl1 ptl2 *)
SetSegmentMidPoint[{pml_, ptl1_, ptl2_}] :=
	Module[
		{pm, p1, p2,vpm, vp1, vp2, newpm, lineConditions, midConditions},

		(* Aggiunge ai segmenti del problema i due che si formano aggiungendo il punto medio *)
		problemSegmentsCheck = Union[problemSegmentsCheck, {OrderSegmentLabel[ptl1<>pml], OrderSegmentLabel[ptl2<>pml]}];

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che pml esista *)
				(pm = Select[MatchQ[pml, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Punto " ~~ pml]]
			]
		];

		Quiet[
			Check[
				(* Verifica che gli altri due punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Segmento " ~~ ptl1 ~~ ptl2]]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vpm = Select[finalPoints, MatchQ[#[[1]], pml]&][[1]];
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];

		(* Rendo m e q variabili fresche *)
		m = Unique[m];
		q = Unique[q];

		(* Condizioni per m e q, che definiscano la retta corrispondente al segmento *)
		lineConditions = {
			GetPointCoordinates[vp1][[2]]==m*GetPointCoordinates[vp1][[1]]+q,
			GetPointCoordinates[vp2][[2]]==m*GetPointCoordinates[vp2][[1]]+q
		};

		(* Condizione sul punto medio *)
		midConditions = {
			GetPointCoordinates[vpm][[2]] == m*GetPointCoordinates[vpm][[1]]+ q,
			(Sqrt[Abs[GetPointCoordinates[vp1][[1]] - GetPointCoordinates[vpm][[1]]]^2 + Abs[GetPointCoordinates[vp1][[2]]-GetPointCoordinates[vpm][[2]]]^2])
				==
			(Sqrt[Abs[GetPointCoordinates[vp2][[1]] - GetPointCoordinates[vpm][[1]]]^2 + Abs[GetPointCoordinates[vp2][[2]]-GetPointCoordinates[vpm][[2]]]^2])
		} ;

		(* Aggiungo l'uguaglianza fra i due nuovi segmenti *)
		AddSegmentEquality[ptl1~~pml, pml~~ptl2];

		(* Aggiugo m e q alle variabili da considerare *)
		otherVars= Append[otherVars, m];
		otherVars= Append[otherVars, q];
		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Join[solveConditions, lineConditions];
		solveConditions = Join[solveConditions, midConditions];
	];

(* Dati due segmenti ed eventuali moltiplicatori impone la condizione di uguaglianza *)
SetSegmentEqualityCondition[{ml1_,{ptl1_, ptl2_}, ml2_,{ptl3_, ptl4_}}] :=
	Module[
		{p1, p2, p3, p4,vp1, vp2, vp3, vp4,condition, m1, m2},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Segmento " ~~ ptl1 ~~ ptl2]]
			]
		];

		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p3 = Select[MatchQ[ptl3, #[[1]]]&][problemPoints][[1]];
				p4 = Select[MatchQ[ptl4, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Segmento " ~~ ptl3 ~~ ptl4]]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];
		vp3 = Select[finalPoints, MatchQ[#[[1]], ptl3]&][[1]];
		vp4 = Select[finalPoints, MatchQ[#[[1]], ptl4]&][[1]];

		(* Trasforma la stringa del moltiplicatore in espressione,
		 	 se non e' assegnato lo trasforma in 1 *)
		If[NumericQ[ToExpression[ml1]],{m1 = ToExpression[ml1]}, {m1 = 1}];
		If[NumericQ[ToExpression[ml2]],{m2 = ToExpression[ml2]}, {m2 = 1}];

		(* Condizione sull'uguaglianza della lunghezza *)
		condition = (m1 * Sqrt[
		  Abs[GetPointCoordinates[vp1][[1]]-GetPointCoordinates[vp2][[1]]]^2
		 		+
		  Abs[GetPointCoordinates[vp1][[2]]-GetPointCoordinates[vp2][[2]]]^2
			]
				==
			m2 * Sqrt[
		   	Abs[GetPointCoordinates[vp3][[1]]-GetPointCoordinates[vp4][[1]]]^2
				+
				Abs[GetPointCoordinates[vp3][[2]]-GetPointCoordinates[vp4][[2]]]^2]
			);

		(* In caso di stesso moltiplicatore aggiunge alla lista di uguaglianze *)
		If[
			m1 == m2,
			AddSegmentEquality[ptl1~~ptl2, ptl3~~ptl4],
			Null
		];

		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, condition];
	];

(* Dati due angoli ed eventuali moltiplicatori impone la condizione di uguaglianza *)
SetAngleEqualityCondition[{ml1_,{ptl1_, ptl2_, ptl3_}, ml2_,{ptl4_, ptl5_, ptl6_}}] :=
	Module[
		{p1, p2,p3, p4, p5, p6, vp1, vp2,vp3, vp4, vp5, vp6, m1, m2,npl1, npl3, npl4, npl6, condition},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];
				p3 = Select[MatchQ[ptl3, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Angolo " ~~ ptl1 ~~ ptl2 ~~ ptl3]]
			]
		];

		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p4 = Select[MatchQ[ptl4, #[[1]]]&][problemPoints][[1]];
				p5 = Select[MatchQ[ptl5, #[[1]]]&][problemPoints][[1]];
				p6 = Select[MatchQ[ptl6, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Angolo " ~~ ptl4 ~~ ptl5~~ ptl6]]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];
		vp3 = Select[finalPoints, MatchQ[#[[1]], ptl3]&][[1]];
		vp4 = Select[finalPoints, MatchQ[#[[1]], ptl4]&][[1]];
		vp5 = Select[finalPoints, MatchQ[#[[1]], ptl5]&][[1]];
		vp6 = Select[finalPoints, MatchQ[#[[1]], ptl6]&][[1]];

		(* Trasforma la stringa del moltiplicatore in espressione,
		 	 se non e' assegnato lo trasforma in 1 *)
		If[MatchQ[StringLength[ml1],0],{m1 = 1}, {m1 = ToExpression[ml1]}];
		If[MatchQ[StringLength[ml2],0],{m2 = 1}, {m2 = ToExpression[ml2]}];

		(* Condizione sull'uguaglianza *)
		condition =
			m1 *VectorAngle[
			 	{GetPointCoordinates[vp1][[1]] - GetPointCoordinates[vp2][[1]], GetPointCoordinates[vp1][[2]] - GetPointCoordinates[vp2][[2]]},
		 		{GetPointCoordinates[vp3][[1]] - GetPointCoordinates[vp2][[1]], GetPointCoordinates[vp3][[2]] - GetPointCoordinates[vp2][[2]]}]
				==
			m2 * VectorAngle[
				{GetPointCoordinates[vp4][[1]] - GetPointCoordinates[vp5][[1]], GetPointCoordinates[vp4][[2]] - GetPointCoordinates[vp5][[2]]},
		 		{GetPointCoordinates[vp6][[1]] - GetPointCoordinates[vp5][[1]], GetPointCoordinates[vp6][[2]] - GetPointCoordinates[vp5][[2]]}];

		(* Ordino i punti per crearne il nome *)
		{npl1, npl3} = Sort[{ptl1, ptl3}];
		{npl4, npl6} = Sort[{ptl4, ptl6}];

		(* In caso di stesso moltiplicatore aggiunge alla lista di uguaglianze *)
		If[
			m1 == m2,
			AddAngleEquality[npl1~~ptl2~~npl3, npl4~~ptl5~~npl6]
		];

		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, condition];
	];

(* Dati tre nomi di punti, impone il vincolo di pml appartenente al segmento ptl1 ptl2 *)
SetSegmentBelongsCondition[{pml_, ptl1_, ptl2_}] :=
	Module[
		{pm, p1, p2,vpm, vp1, vp2, newpm, lineConditions},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(pm = Select[MatchQ[pml, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Punto " ~~ pml]]
			]
		];

		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Segmento " ~~ ptl1 ~~ ptl2]]
			]
		];

		(* Assegno i finalPoints corrisponenti *)
		vpm = Select[finalPoints, MatchQ[#[[1]], pml]&][[1]];
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];

		(* Rendo m e q variabili fresche *)
		m = Unique[m];
		q = Unique[q];

		(* Aggiungo ai segmenti del problema quelli generati dal nuovo punto *)
		problemSegmentsCheck = Union[problemSegmentsCheck, {OrderSegmentLabel[ptl1<>pml], OrderSegmentLabel[ptl2<>pml]}];

		lineConditions = {
			GetPointCoordinates[vp1][[2]]==m*GetPointCoordinates[vp1][[1]]+q,
			GetPointCoordinates[vp2][[2]]==m*GetPointCoordinates[vp2][[1]]+q,
			GetPointCoordinates[vpm][[2]]==m*GetPointCoordinates[vpm][[1]]+q
		};

		otherVars= Append[otherVars, m];
		otherVars= Append[otherVars, q];
		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, lineConditions];
	];

(* Dati due segmenti ne impone la loro perpendicolarita' *)
SetPerpendicularAngleCondition[{ptl1_, ptl2_, ptl3_,ptl4_}] :=
	Module[
		{p1, p2, p3, p4, vp1, vp2, vp3, vp4,condition},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Segmento " ~~ ptl1 ~~ ptl2]]
			]
		];

		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p3 = Select[MatchQ[ptl3, #[[1]]]&][problemPoints][[1]];
				p4 = Select[MatchQ[ptl4, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Segmento " ~~ ptl3 ~~ ptl4]]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];
		vp3 = Select[finalPoints, MatchQ[#[[1]], ptl3]&][[1]];
		vp4 = Select[finalPoints, MatchQ[#[[1]], ptl4]&][[1]];

		(* Rendo m e q variabili fresche *)
		m = Unique[m];
		q = Unique[q];

		(* Condizioni di perpendicolarita' *)
		condition =
		{
			GetPointCoordinates[vp1][[2]]==m*GetPointCoordinates[vp1][[1]]+q,
			GetPointCoordinates[vp2][[2]]==m*GetPointCoordinates[vp2][[1]]+q,
			GetPointCoordinates[vp3][[2]]==-1/m*GetPointCoordinates[vp3][[1]]+q,
			GetPointCoordinates[vp4][[2]]==-1/m*GetPointCoordinates[vp4][[1]]+q
		};

		otherVars= Append[otherVars, m];
		otherVars= Append[otherVars, q];
		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, condition];
	];

(* Impone che un punto sia intersezione di due segmenti anche nella figura finale *)
SetIntersectionConstraint[
	{
		{pml_,cp_List},
		{
			{{ptl1_,c1_List},{ptl2_,c2_List}},
			{{ptl3_,c3_List},{ptl4_,c4_List}}
		}
	}
] :=
	Module[
		{pm, p1, p2, p3, p4,vpm, vp1, vp2,vp3, vp4, lineConditions},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(pm = Select[MatchQ[pml, #[[1]]]&][problemPoints][[1]];
				p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];
				p3 = Select[MatchQ[ptl3, #[[1]]]&][problemPoints][[1]];
				p4 = Select[MatchQ[ptl4, #[[1]]]&][problemPoints][[1]];),
				ErrorMessageInHypothesis[]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vpm = Select[finalPoints, MatchQ[#[[1]], pml]&][[1]];
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];
		vp3 = Select[finalPoints, MatchQ[#[[1]], ptl3]&][[1]];
		vp4 = Select[finalPoints, MatchQ[#[[1]], ptl4]&][[1]];

		(* Rendo m e q variabili fresche *)
		m1 = Unique[m1];
		q1 = Unique[q1];
		m2 = Unique[m2];
		q2 = Unique[q2];

		(* Condizioni di appartenenza a entrambi i segmenti *)
		lineConditions = {
			GetPointCoordinates[vp1][[2]]==m1*GetPointCoordinates[vp1][[1]]+q1,
			GetPointCoordinates[vp2][[2]]==m1*GetPointCoordinates[vp2][[1]]+q1,
			GetPointCoordinates[vp3][[2]]==m2*GetPointCoordinates[vp3][[1]]+q2,
			GetPointCoordinates[vp4][[2]]==m2*GetPointCoordinates[vp4][[1]]+q2,
			GetPointCoordinates[vpm][[2]]==m1*GetPointCoordinates[vpm][[1]]+q1,
			GetPointCoordinates[vpm][[2]]==m2*GetPointCoordinates[vpm][[1]]+q2
		};

		otherVars= Append[otherVars, m1];
		otherVars= Append[otherVars, q1];
		otherVars= Append[otherVars, m2];
		otherVars= Append[otherVars, q2];

		(* Aggiungo i nuovi segmenti a quelli totali del problema *)
		problemSegmentsCheck = Union[problemSegmentsCheck, {
			StringJoin[Characters[ptl1~~pml]],
			StringJoin[Characters[ptl2~~pml]],
			StringJoin[Characters[ptl3~~pml]],
			StringJoin[Characters[ptl4~~pml]]
		}];

		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, lineConditions];
	];

(* Impone la condizione di angolo retto *)
SetRightAngleCondition[{ptl1_, ptl2_, ptl3_}] :=
	Module[
		{p1, p2, p3, vp1, vp2, vp3, npl1, npl3, npl4, npl6, condition},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];
				p3 = Select[MatchQ[ptl3, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Angolo " ~~ ptl1 ~~ ptl2 ~~ ptl3]]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];
		vp3 = Select[finalPoints, MatchQ[#[[1]], ptl3]&][[1]];

		(* Condizione di angolo retto *)
		condition =
			(VectorAngle[
			 	{GetPointCoordinates[vp1][[1]] - GetPointCoordinates[vp2][[1]], GetPointCoordinates[vp1][[2]] - GetPointCoordinates[vp2][[2]]},
		 		{GetPointCoordinates[vp3][[1]] - GetPointCoordinates[vp2][[1]], GetPointCoordinates[vp3][[2]] - GetPointCoordinates[vp2][[2]]}
				]	== (Pi / 2));

		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, condition];
	];

(* Impone la condizione di angolo piatto *)
SetFlatAngleCondition[{ptl1_, ptl2_, ptl3_}] :=
	Module[
		{p1, p2, p3, vp1, vp2, vp3, condition},

		(* Silenzio eventuali messaggi di errore, gestendolo con un dialog apposito *)
		Quiet[
			Check[
				(* Verifica che i punti esistano *)
				(p1 = Select[MatchQ[ptl1, #[[1]]]&][problemPoints][[1]];
				p2 = Select[MatchQ[ptl2, #[[1]]]&][problemPoints][[1]];
				p3 = Select[MatchQ[ptl3, #[[1]]]&][problemPoints][[1]];),
				Return[ErrorMessageInHypothesis["Angolo " ~~ ptl1 ~~ ptl2 ~~ ptl3]]
			]
		];

		(* Assegno i finalPoints corrispondenti *)
		vp1 = Select[finalPoints, MatchQ[#[[1]], ptl1]&][[1]];
		vp2 = Select[finalPoints, MatchQ[#[[1]], ptl2]&][[1]];
		vp3 = Select[finalPoints, MatchQ[#[[1]], ptl3]&][[1]];

		(* Condizione di angolo piatto *)
		condition =
			(VectorAngle[
				{GetPointCoordinates[vp1][[1]] - GetPointCoordinates[vp2][[1]], GetPointCoordinates[vp1][[2]] - GetPointCoordinates[vp2][[2]]},
				{GetPointCoordinates[vp3][[1]] - GetPointCoordinates[vp2][[1]], GetPointCoordinates[vp3][[2]] - GetPointCoordinates[vp2][[2]]}]
				==
			Pi);

		(* Aggiungo le condizioni ottenute alle solveConditions *)
		solveConditions = Append[solveConditions, condition];
	];



(*** INTERSEZIONI ***)
(* Funzione che rileva eventuali intersezioni fra due segmenti *)
(* Se ci sono punti in comune ritorna la lista vuota *)
FindIntersection[
	{
		{{Label1_, {x1_, y1_}}, {Label2_, {x2_, y2_}}},
		{{Label3_, {x3_, y3_}}, {Label4_, {x4_, y4_}}}
	} ]/;
	PointInCommon[
	{{Label1, {x1, y1}}, {Label2, {x2, y2}}},
	{{Label3, {x3, y3}}, {Label4, {x4, y4}}}
	] != {}	:= {};

(* Ritorna eventuali punti di intersezione *)
FindIntersection[
	{
		{{Label1_, {x1_, y1_}}, {Label2_, {x2_, y2_}}},
		{{Label3_, {x3_, y3_}}, {Label4_, {x4_, y4_}}}
	}
	]:=
	Module[
		{fm1, fm2, fq1, fq2, sol, sol1, sol2, x, y, cond1, cond2, intPoint},

		(* Retta passante per il primo segmento *)
		sol1 = Solve[
			{
				y1==m1*x1+q1,
				y2==m1*x2+q1
			},
			{m1, q1},
			Reals
		];

		(* Retta passante per il secondo segmento *)
		sol2 = Solve[
			{
				y3==m2*x3+q2,
				y4==m2*x4+q2
			},
			{m2, q2},
			Reals
		];

		(* Caso di segmento verticale *)
		If[
			x1 == x2,
			cond1 = (x == x1),
			cond1 =(y == m1 *x + q1) /. sol1 [[1]]
		];

		(* Caso di segmento verticale *)
		If[
			x3 == x4,
			cond2 = (x == x3),
			cond2 =(y == m2 *x + q2) /. sol2 [[1]]
		];

		(* Risolvo il sistema, imponendo che il punto di intersezione debba essere all'interno dei segmenti;
		 	per evitare che venga individuato su un prolungamento dei segmenti *)
		sol = NSolve[
			{
				cond1,
				cond2,
				GetSegmentLength[{{"new",{x, y}}, {Label1, {x1, y1}}}] <= GetSegmentLength[{{Label1, {x1, y1}},  {Label2, {x2, y2}}}],
				GetSegmentLength[{{"new",{x, y}}, {Label2, {x2, y2}}}] <= GetSegmentLength[{{Label1, {x1, y1}},  {Label2, {x2, y2}}}],
				GetSegmentLength[{{"new",{x, y}}, {Label3, {x3, y3}}}] <= GetSegmentLength[{{Label3, {x3, y3}},  {Label4, {x4, y4}}}],
				GetSegmentLength[{{"new",{x, y}}, {Label4, {x4, y4}}}] <= GetSegmentLength[{{Label3, {x3, y3}},  {Label4, {x4, y4}}}]
			},
			{x, y},
			Reals
		];

		(* Se non ha soluzione ritorno la lista vuota *)
		If[
			sol == {},
			intPoint = {{}},
			intPoint =
				{
					({x, y} /.sol)[[1]],
					{
						{{Label1, {x1, y1}}, {Label2, {x2, y2}}},
						{{Label3, {x3, y3}}, {Label4, {x4, y4}}}
					}
				}
		];

		(* Ignoro il punto di intersezione se e' troppo vicino a uno degli estremi dei segmenti *)
		If[
			GetSegmentLength[{{"plchldr", intPoint[[1]]}, {Label1, {x1, y1}}}] < 0.3
				||
			GetSegmentLength[{{"plchldr", intPoint[[1]]}, {Label2, {x2, y2}}}] < 0.3
				||
			GetSegmentLength[{{"plchldr", intPoint[[1]]}, {Label3, {x3, y3}}}] < 0.3
				||
			GetSegmentLength[{{"plchldr", intPoint[[1]]}, {Label4, {x4, y4}}}] < 0.3,
			intPoint = {{}}
		];

		intPoint
	];



(* FUNZIONE DA MINIMIZZARE *)
(* Ritorna il valore che va minimizzato nella creazione della figura finale *)
(* E' la somma delle distanze fra i punti inseriti dall'utente e quelli finali (ossia quelli su cui sono definiti i vincoli) *)
(* Lo scopo e' infatti quello di creare una figura in cui i vincoli siano rispettati che sia il piu' simile possibile a quella disegnata *)
GenerateMinimizeDistance[] :=
	Module[
		{el = 0},
		(* Per ogni punto *)
		For[
			i=1,
			i <= Length[problemPoints],
			i++,
			(* Sommo la distanza al parametro da minimizzare *)
			el = el + Sqrt[
				Abs[GetPointCoordinates[problemPoints[[i]]][[1]] - GetPointCoordinates[finalPoints[[i]]][[1]]]^2 + Abs[GetPointCoordinates[problemPoints[[i]]][[2]] - GetPointCoordinates[finalPoints[[i]]][[2]]]^2
			]^2;
		];
		el
	];



(* GESTIONE DELLE STRUTTURE DI UGUAGLIANZA *)
(* Ognuna delle strutture gestite e' una lista di liste di uguaglianze, dove
 	 ciascuna sottolista equivale all'uguaglianze degli elementi che contiene *)
(* ex: { {AB, BC}, {CD, EF} } significa che i segmenti AB e BC sono della stessa lunghezza, e lo stesso per CD e EF *)

(* Dati due nomi di segmenti, aggiungo una relazione di uguaglianza fra essi *)
AddSegmentEquality[sl1_, sl2_] :=
	Module[
	{s1, s2},

	(* Ordino alfabeticamente il nome *)
	s1 = StringJoin[Sort[Characters[sl1]]];
	s2 = StringJoin[Sort[Characters[sl2]]];

	If[
		(* Se nessuno dei due e' gia' nella struttura aggiungo un nuovo elemento con loro due *)
		MatchQ[Position[segmentEqualityStruct,s1], {}] && MatchQ[Position[segmentEqualityStruct,s2] ,{}] ,
		segmentEqualityStruct = Append[segmentEqualityStruct, {s1, s2}],
		Null
	];

	If[
		(* Se solo s2 e' nella struttura allora aggiungo s1 nella sua stessa sottolista *)
		MatchQ[Position[segmentEqualityStruct,s1], {}] && Not[MatchQ[Position[segmentEqualityStruct,s2] ,{}] ],
		segmentEqualityStruct = segmentEqualityStruct /. s2 -> Sequence[s1, s2],
		Null
	];

	If[
		(* Se solo s1 e' nella struttura allora aggiungo s2 nella sua stessa sottolista *)
		Not[MatchQ[Position[segmentEqualityStruct,s1], {}]] && MatchQ[Position[segmentEqualityStruct,s2] ,{}],
		segmentEqualityStruct = segmentEqualityStruct /. s1 -> Sequence[s1, s2],
		Null
	];

	If[
		(* Se sono entrambi gia' nella struttura... *)
		Not[MatchQ[Position[segmentEqualityStruct,s1], {}]] && Not[MatchQ[Position[segmentEqualityStruct,s2] ,{}]],
		If[
			Position[segmentEqualityStruct,s1][[1]][[1]]== Position[segmentEqualityStruct,s2][[1]][[1]],
			(* ... se sono nello stessa sottolista non devo fare niente *)
			Null,
			(* ... se sono in sottoliste diverse unisco le sottoliste *)
			grp1 = segmentEqualityStruct[[Position[segmentEqualityStruct,s1][[1]][[1]]]];
			grp2 = segmentEqualityStruct[[Position[segmentEqualityStruct,s2][[1]][[1]]]];
			newgrp = Join[grp1, grp2];
			segmentEqualityStruct =
				Drop[segmentEqualityStruct, {Position[segmentEqualityStruct,s1][[1]][[1]]}];
			segmentEqualityStruct =
				Drop[segmentEqualityStruct, {Position[segmentEqualityStruct,s2][[1]][[1]]}];
			segmentEqualityStruct = Append[segmentEqualityStruct, newgrp];
		],
		Null
	];

	segmentEqualityStruct
	];

(* Dati due nomi di angoli, aggiungo una relazione di uguaglianza fra essi *)
AddAngleEquality[al1_, al2_] :=
	Module[
		{a1, a2},

		a1 = al1;
		a2 = al2;

		If[
			(* Se nessuno dei due e' gia' nella struttura aggiungo un nuovo elemento con loro due *)
			MatchQ[Position[angleEqualityStruct ,a1], {}] && MatchQ[Position[angleEqualityStruct ,a2] ,{}] ,
			angleEqualityStruct  = Append[angleEqualityStruct , {a1, a2}],
			Null
		];

		If[
			(* Se solo a2 e' nella struttura allora aggiungo a1 nella sua stessa sottolista *)
			MatchQ[Position[angleEqualityStruct ,a1], {}] && Not[MatchQ[Position[angleEqualityStruct ,a2] ,{}] ],
			angleEqualityStruct  = angleEqualityStruct  /. a2 -> Sequence[a1, a2],
			Null
		];

		If[
			(* Se solo a1 e' nella struttura allora aggiungo a2 nella sua stessa sottolista *)
			Not[MatchQ[Position[angleEqualityStruct ,a1], {}]] && MatchQ[Position[angleEqualityStruct ,a2] ,{}],
			angleEqualityStruct  = angleEqualityStruct  /. a1 -> Sequence[a1, a2],
			Null
		];

		If[
			(* Se sono entrambi gia' nella struttura... *)
			Not[MatchQ[Position[angleEqualityStruct ,a1], {}]] && Not[MatchQ[Position[angleEqualityStruct ,a2] ,{}]] ,
			If[
				Position[angleEqualityStruct ,a1][[1]][[1]]== Position[angleEqualityStruct ,a2][[1]][[1]],
				(* ... se sono nello stessa sottolista non devo fare niente *)
				Null,
				(* ... se sono in sottoliste diverse unisco le sottoliste *)
				grp1 = angleEqualityStruct [[Position[angleEqualityStruct ,a1][[1]][[1]]]];
				grp2 = angleEqualityStruct [[Position[angleEqualityStruct ,a2][[1]][[1]]]];
				newgrp = Join[grp1, grp2];
				angleEqualityStruct  =
					Drop[segmentEqualityStruct, {Position[angleEqualityStruct ,a1][[1]][[1]]}];
				angleEqualityStruct  =
					Drop[segmentEqualityStruct, {Position[angleEqualityStruct ,a2][[1]][[1]]}];
				angleEqualityStruct  = Append[angleEqualityStruct , newgrp];
			],
			Null
		];

		angleEqualityStruct
	];

(* Dati due nomi di triangoli, aggiungo una relazione di uguaglianza fra essi *)
AddTriangleCongruence[tl1_, tl2_] :=
	Module[
	{t1, t2},

	t1 = StringJoin[Sort[Characters[tl1]]];
	t2 = StringJoin[Sort[Characters[tl2]]];

	If[
		(* Se nessuno dei due e' gia' nella struttura aggiungo un nuovo elemento con loro due *)
		MatchQ[Position[triangleCongruenceStruct, t1], {}] && MatchQ[Position[triangleCongruenceStruct, t2] ,{}] ,
		triangleCongruenceStruct = Append[triangleCongruenceStruct, {t1, t2}],
		Null
	];

	If[
		(* Se solo t2 e' nella struttura allora aggiungo t1 nella sua stessa sottolista *)
		MatchQ[Position[triangleCongruenceStruct, t1], {}] && Not[MatchQ[Position[triangleCongruenceStruct, t2] ,{}] ],
		triangleCongruenceStruct = triangleCongruenceStruct /. t2 -> Sequence[t1, t2],
		Null
	];

	If[
		(* Se solo t1 e' nella struttura allora aggiungo t2 nella sua stessa sottolista *)
		Not[MatchQ[Position[triangleCongruenceStruct, t1], {}]] && MatchQ[Position[triangleCongruenceStruct, t2] ,{}],
		triangleCongruenceStruct = triangleCongruenceStruct /. t1 -> Sequence[t1, t2],
		Null
	];

	If[
		(* Se sono entrambi gia' nella struttura... *)
		Not[MatchQ[Position[triangleCongruenceStruct, t1], {}]] && Not[MatchQ[Position[triangleCongruenceStruct, t2] ,{}]],
		If[
			Position[triangleCongruenceStruct, t1][[1]][[1]] == Position[triangleCongruenceStruct, t2][[1]][[1]],
			(* ... se sono nello stessa sottolista non devo fare niente *)
			Null,
			(* ... se sono in sottoliste diverse unisco le sottoliste *)
			grp1 = triangleCongruenceStruct[[Position[triangleCongruenceStruct, t1][[1]][[1]]]];
			grp2 = triangleCongruenceStruct[[Position[triangleCongruenceStruct, t2][[1]][[1]]]];
			newgrp = Join[grp1, grp2];
			triangleCongruenceStruct =
				Drop[triangleCongruenceStruct, {Position[triangleCongruenceStruct,t1][[1]][[1]]}];
			triangleCongruenceStruct =
				Drop[triangleCongruenceStruct, {Position[triangleCongruenceStruct,t2][[1]][[1]]}];
			triangleCongruenceStruct = Append[triangleCongruenceStruct, newgrp];
		],
		Null
	];

	triangleCongruenceStruct
	];



(*** STESSO ANGOLO CON PIU' NOMI ***)
(* Per ogni angolo nella lista delle uguaglianze aggiungo un' uguaglianza con
		angoli uguali ma con nomi differenti *)
FixAngleEqualityStruct[] :=
	Module[
		{angList, j},
		(* Lista di angoli nella struttura di equivalenze *)
		angList = Flatten[angleEqualityStruct];
		For[
			j = 1,
			j <= Length[angList],
			j++,
			(* Chiamata all'aggiunta di angoli *)
			AddSameAngleEquality[Characters[angList[[j]]]];
		];
	];

(* Dati i nomi di tre punti, aggiunge alla strutture delle uguaglianze tutti gli angoli uguali *)
AddSameAngleEquality[{pl1_, plm_, pl2_}] :=
	Module[
		{i, p1, p2, pm},

		(* Ottengo i punti corrispondenti ai nomi in input *)
		p1 = Select[problemFinalPoints, MatchQ[#[[1]], pl1]&][[1]];
		p2 = Select[problemFinalPoints, MatchQ[#[[1]], pl2]&][[1]];
		pm = Select[problemFinalPoints, MatchQ[#[[1]], plm]&][[1]];

		(* Per ciascun punto esistente... *)
		For[
			i = 1,
			i <= Length[problemFinalPoints],
			i++,
			If[
				(* Verifico che sia sulla stessa retta di p1 e pm e nel caso aggiungo *)
				OnSameLine[p1, pm, problemFinalPoints[[i]]],
				AddAngleEquality[
					GetPointLabel[p1] ~~ GetPointLabel[pm] ~~ GetPointLabel[p2],
					GetPointLabel[problemFinalPoints[[i]]] ~~ GetPointLabel[pm] ~~ GetPointLabel[p2]
				];
			];
			If[
					(* Verifico che sia sulla stessa retta di p2 e pm e nel caso aggiungo *)
				OnSameLine[p2, pm, problemFinalPoints[[i]]],
				AddAngleEquality[
					GetPointLabel[p1] ~~ GetPointLabel[pm] ~~ GetPointLabel[p2],
					GetPointLabel[p1] ~~ GetPointLabel[pm] ~~ GetPointLabel[problemFinalPoints[[i]]]
				];
			];
		];
	];

(* Verifica che tre punti siano sulla stessa retta
 		Funzione ausiliaria per AddSameAngleEquality[] *)
OnSameLine[p1_, p2_, p3_] :=
	Module[
		{pl1, pl2, pl3, m, q, sol},

		(* Nomi dei punti *)
		pl1 = GetPointLabel[p1];
		pl2 = GetPointLabel[p2];
		pl3 = GetPointLabel[p3];

		If[
			(* Se ci sono punti non distinti ritorno False *)
			MatchQ[pl1, pl3] || MatchQ[pl2, pl3],
			Return[False]
		];

		(* Calcolo la retta passante per due punti *)
		sol = Solve[
			{
				GetPointCoordinates[p1][[2]] == m * GetPointCoordinates[p1][[1]] + q,
				GetPointCoordinates[p2][[2]] == m * GetPointCoordinates[p2][[1]] + q
			},
			{m, q}
		][[1]];

		{m, q} = {m, q} /. sol;

		If[
			(* Controllo che anche il terzo appartenga alla stessa retta *)
			Abs[GetPointCoordinates[p3][[2]] - (m * GetPointCoordinates[p3][[1]] + q)] < 0.1,
			Return[True]
		];

		False
	]



(*** ESPORTO IL PROBLEMA ***)
(* Chiamata quando l'utente clicca "Vai alla dimostrazione" *)
(* Effettua la valutazione di tutti i punti dati in input dall'utente, di tutte le condizioni di ipotesi e tesi.
 	 Genera quindi la figura finale del problema, su cui effettuare la dimostrazione.
	 Ritorna False se si incontrano problemi o errori durante la valutazione. *)
ExportProblem[hps_,thes_,pts_, ptsIntStruct_, segcheck_, size_] :=
	Module[
		{index},

		(* La seguente istruzione non e' supportata su versioni di matematica <= 10.3, uso un for per ovviare *)
		(* problemHypothesis = Map[StringReplace[" " -> ""], hps]; *)
		problemHypothesis = {};
		For[
			index = 1,
			index <= Length[hps],
			index++,
			{
				problemHypothesis = Append[problemHypothesis, StringReplace[hps[[index]], " " -> ""]];
			}
		];

		problemThesis = thes;
		problemPoints = Sort[pts];
		problemSegmentsCheck = Map[OrderSegmentLabel,segcheck];
		problemWidgetSize = size;
		problemPointIntStruct = ptsIntStruct;
		(* Etichette dei punti *)
		problemPointLabels :=
			Map[GetPointLabelAsText, problemPoints];
		problemSegments :=
			Select[ContainsAll[problemSegmentsCheck,{GetSegmentLabel[#]}]&][ Subsets[problemPoints, {2}]];
		(* Segmenti che devono essere disegnati nella figura *)
		problemDrawingSegments :=
			Map[GetSegmentLine,Select[ContainsAll[problemSegmentsCheck,{GetSegmentLabel[#]}]&][ problemSegments]];
		(* Angoli della figura *)
		problemAngles := Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[problemSegments, {2}]];

		(* Creo la struttura dei finalPoints, su cui verranno imposti i vincoli *)
		(* Per ogni punto del problema aggiungo un elemento *)
		finalPoints = {};
		For[
			i=1,
			i <= Length[problemPoints],
			i++, {
			AppendTo[finalPoints, {problemPoints[[i]][[1]], {Unique[x], Unique[y]}}]
		}];

		(* Lista di colori per segmenti e angoli *)
		colorAngleList = {Red, Blue, Cyan, Magenta, Green, Gray, Orange, Brown, Pink, Purple};
		colorSegmentList := ColorData[3];

		(* Inizializzo le strutture di uguaglianza *)
		segmentEqualityStruct = {};
		angleEqualityStruct = {};
		triangleCongruenceStruct = {};

		(* Inizializzo lista di variabili e condizioni del problema *)
		Clear[solveConditions];
		Clear[otherVars];
		solveConditions = {};
		otherVars = {};

		(* Genero i vincoli per le intersezioni *)
		Clear[substFun];
		substFun = {};
		(* substFun e' una funzione di sostituzione ausiliaria per la gestione delle intersezioni *)
		For[
			i = 1,
			i <= Length[problemPoints],
			i++,
			substFun = Append[substFun,{problemPoints[[i]][[2]], s_List} -> {problemPoints[[i]], s}]
		];
		problemPointIntStruct = problemPointIntStruct /. substFun;
		For[
			i = 1,
			i <= Length[problemPointIntStruct],
			i++,
			SetIntersectionConstraint[problemPointIntStruct[[i]]]
		];

		(* Aggiungo i vincoli di appartenenza alla regione di piano disegnata in figura,
		 		ossia da -10 a +10 sia per X che per Y *)
		Clear[regConstraints];
		regConstraints = {};
		flatVars = Flatten[finalPoints[[All, 2]]];
		For[
			i=1,
			i <= Length[flatVars],
			i++,
			{regConstraints = Append[regConstraints, -10<flatVars[[i]]< 10]}
		];

		(* Scorro tutte le ipotesi, le interpreto e aggiungo i vincoli corrispondenti *)
		errorInHypothesisParsing = False;
		For[
			i=1,
			i <= Length[problemHypothesis],
			i++,
			ParseHypothesis[problemHypothesis[[i]]]
		];

		(* Se l'analisi dei dati ha riscontrato errori ritorno False *)
		If[
			errorInHypothesisParsing,
			Return[False]
		];

		(* Creo la funzione da minimizzare *)
		minDist := GenerateMinimizeDistance[];

		(* Eseguo la minimizzazione, imponendo tutti i vincoli creati in precedenza *)
		finalSubst = Minimize[
			{minDist, Union[solveConditions, regConstraints]},
			Union[Flatten[finalPoints[[All, 2]]], otherVars ],
			Reals
		];

		(* In problemFinalPoints ho i punti del problema finale dopo la minimizzazione *)
		problemFinalPoints = finalPoints /. finalSubst[[2]];

		(* Ricalcolo etichette e segmenti *)
		finalPointLabels :=
			Map[GetPointLabelAsText, problemFinalPoints];
		finalSegments =
			Select[ContainsAll[problemSegmentsCheck,{GetSegmentLabel[#]}]&][ Subsets[problemFinalPoints, {2}]];
		For[
			i = 1,
			i <= Length[finalSegments],
			i++,
			finalSegments[[i]] = finalSegments[[i]] /.  {{ptl1_, c1_List}, {ptl2_, c2_List}} -> {{{ptl1, c1}, {ptl2, c2}}, StringJoin[ptl1, ptl2]}
		];

		(* Creo le strutture segmenti da disegnare, usando i colori se appartententi alla struttura di equivalenza *)
		finalDrawingSegments := finalSegments /. {{{l1_,{c11_,c12_}},{l2_,{c21_,c22_}}},sl_} :>
			{
				AbsoluteThickness[3],
				If[
					MatchQ[Position[segmentEqualityStruct,sl], {}],
					Black,
					colorSegmentList[
					Position[segmentEqualityStruct, sl]
					[[1, 1]] * 2
					]
				],
				Line[{{c11, c12}, {c21, c22}}]
		};

		(* Ricalcolo gli angoli *)
		finalAngles := Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[finalSegments[[All, 1]], {2}]];

		(* Aggiusto le strutture di equivalenza *)
		FixAngleEqualityStruct[];

		(* Inizializzo le strutture per la parte di dimostrazione *)
		proofSteps = Table[{"", False}, 25];
		verifiedProofSteps := Select[proofSteps, MatchQ[#[[2]], True]&];

		(* Inizializzo le strutture per il ripristino *)
		backupSegmentEqualityStruct = segmentEqualityStruct;
		backupAngleEqualityStruct = angleEqualityStruct;
		backupTriangleCongruenceStruct = triangleCongruenceStruct;

		(* Analizzo la tesi, e creo le corrispondenti condizioni da verificare *)
		ParseThesis[StringReplace[problemThesis, " " -> ""]];

		(* Se sono arrivato fino a questo punto ritorno True *)
		True
	]



(*** DIMOSTRAZIONE ***)
(* Le seguenti funzioni sono usate nella parte di dimostrazione per verificare nuove condizioni inserite dall'utente *)

(* Dati due nomi di angoli verifica che siano opposti al vertice, e nel caso ne deduce la loro uguaglianze *)
CheckOppositeAngles[{al1_, al2_}] :=
	Module[
		{a1, a2, pc1, pc2, p1, p2, p3, p4, pl1, pl2, pl3, pl4, isOpposite},

		(* Assegno gli angoli corrispondenti ai nomi *)
		a1 = Select[finalAngles, MatchQ[GetAngleLabel[#], OrderAngleLabel[al1]]&][[1]];
		a2 = Select[finalAngles, MatchQ[GetAngleLabel[#], OrderAngleLabel[al2]]&][[1]];

		(* Silenzio eventuali messaggi di errore *)
		Quiet[
			Check[
				(* Controllo l'esistenza di tutti i punti coinvolti *)
				(pc1 = PointInCommon[a1[[1]], a1[[2]]];
				pc2 = PointInCommon[a2[[1]], a2[[2]]];

				p1 = Select[Not[MatchQ[PointInCommon[a1[[1]], a1[[2]]],#]]&][a1[[1]]][[1]];
				p2 = Select[Not[MatchQ[PointInCommon[a1[[1]], a1[[2]]],#]]&][a1[[2]]][[1]];

				p3 = Select[Not[MatchQ[PointInCommon[a2[[1]], a2[[2]]],#]]&][a2[[1]]][[1]];
				p4 = Select[Not[MatchQ[PointInCommon[a2[[1]], a2[[2]]],#]]&][a2[[2]]][[1]];

				pl1 = GetPointLabel[p1];
				pl2 = GetPointLabel[p2];
				pl3 = GetPointLabel[p3];
				pl4 = GetPointLabel[p4];),
				(* Eventualmente lancio un messaggio di errore *)
				ProofStepNotVerified[];
				Return[False]
			]
		];

		(* Prendo i nomi dei segmenti coinvolti *)
		segLabels = finalSegments[[All, 2]];

		(* Verifico che sia effettivamente opposto al vertice controllando l'esistenza dei vari segmenti *)
		isOpposite = MatchQ[pc1, pc2] &&
			((Not[MatchQ[Position[segLabels, StringJoin[Sort[Characters[pl1~~pl3]]]], {}]] && Not[MatchQ[Position[segLabels, StringJoin[Sort[Characters[pl2~~pl4]]]], {}]])
			||
			(Not[MatchQ[Position[segLabels,  StringJoin[Sort[Characters[pl1~~pl4]]]], {}]] && Not[MatchQ[Position[segLabels,  StringJoin[Sort[Characters[pl2~~pl3]]]], {}]]));

		(* Aggiungo l'uguaglianza *)
		If[isOpposite, AddAngleEquality[OrderAngleLabel[al1], OrderAngleLabel[al2]]];

		isOpposite
	];

(* Dati due nomi di segmenti controllo che siano equivalenti *)
CheckCongruentSegments[{sl1_, sl2_}] :=
	Module[
		{os1, os2},

		(* Riordino i nomi dei segmenti *)
		os1 = StringJoin[Sort[Characters[sl1]]];
		os2 = StringJoin[Sort[Characters[sl2]]];

		If[
			(* Se sono nella stessa sottolista della struttura *)
			MatchQ[Position[segmentEqualityStruct, os1], {}] || MatchQ[Position[segmentEqualityStruct, os2], {}],
			False,
			Position[segmentEqualityStruct, os1][[1, 1]] ==Position[segmentEqualityStruct, os2][[1, 1]]
		]
	];

(* Dati due nomi di angoli controllo che siano equivalenti *)
CheckCongruentAngles[{al1_, al2_}] :=
	Module[
		{oa1, oa2},

		(* Riordino i nomi degli angoli *)
		oa1 =OrderAngleLabel[al1];
		oa2 =OrderAngleLabel[al2];

		If[
			(* Se sono nella stessa sottolista della struttura *)
			MatchQ[Position[angleEqualityStruct, oa1], {}] || MatchQ[Position[angleEqualityStruct, oa2], {}],
			False,
			Position[angleEqualityStruct, oa1][[1, 1]] == Position[angleEqualityStruct, oa2][[1, 1]]
		]
	];

(* Dati due nomi di triangoli controllo che siano equivalenti *)
CheckCongruentTriangles[{tl1_, tl2_}] :=
	Module[
		{ot1, ot2},

		(* Riordino i nomi dei triangoli *)
		ot1 = StringJoin[Sort[Characters[tl1]]];
		ot2 = StringJoin[Sort[Characters[tl2]]];

		If[
			(* Se sono nella stessa sottolista della struttura *)
			MatchQ[Position[triangleCongruenceStruct, ot1], {}] || MatchQ[Position[triangleCongruenceStruct, ot2], {}],
			False,
			Position[triangleCongruenceStruct, ot1][[1, 1]] == Position[triangleCongruenceStruct, ot2][[1, 1]]
		]
	];

(*CheckSumOfCongruentSegments[{sl1_, sl2_}, {sl3_, sl4_}] :=
	Module[
		{},
		(
			(
				CheckCongruentSegments[{sl1, sl3}] && CheckCongruentSegments[{sl2, sl4}]
			)
				||
			(
				CheckCongruentSegments[{sl1, sl4}] && CheckCongruentSegments[{sl2, sl3}]
			)
		)
	];

CheckSumOfCongruentAngles[{al1_, al2_}, {al3_, al4_}] :=
	Module[
		{},
		(
			(
				CheckCongruentAngles[{al1, al3}] && CheckCongruentAngles[{al2, al4}]
			)
			||
			(
				CheckCongruentAngles[{al1, al4}] && CheckCongruentAngles[{al2, al3}]
			)
		)
	];*)



(*** CRITERI DI CONGRUENZA ***)

(* Dati i nomi di due triangoli, due coppie di segmenti e una coppia di angoli verifica se vale il primo criterio di congruenza *)
CheckFirstCongruenceCriterion[tlA_, tlB_, {sl1A_, sl1B_}, {sl2A_, sl2B_}, {alA_, alB_}] :=
	Module[
		{tA, tB, triangles, tASegLabels, tBSegLabels, osl1A, osl1B, osl2A, osl2B,
		tAAngles, tBAngles, tAAngLabels, tBAngLabels, oalA, oalB,
		isCongruent,thirdSegmentA, thirdSegmentB},

		(* Prendo i triangoli associati ai nomi *)
		triangles = GetTriangles[];
		tA = Select[triangles, MatchQ[GetTriangleLabel[#], StringJoin[Sort[Characters[tlA]]]]&] ;
		tB = Select[triangles, MatchQ[GetTriangleLabel[#], StringJoin[Sort[Characters[tlB]]]]&] ;

		(* Controllo l'esistenza dei triangoli *)
		If[
			MatchQ[tA, {}] || MatchQ[tB, {}],
			Return[False]
		];

		(* La soluzione ha un solo triangolo *)
		tA = tA[[1]];
		tB = tB[[1]];

		(* Estraggo e riordino i nomi dei segmenti *)
		tASegLabels = Map[GetSegmentLabel, tA];
		tBSegLabels = Map[GetSegmentLabel, tB];
		osl1A = OrderSegmentLabel[sl1A];
		osl2A = OrderSegmentLabel[sl2A];
		osl1B = OrderSegmentLabel[sl1B];
		osl2B = OrderSegmentLabel[sl2B];

		(* Controllo che i segmenti appartengano ai triangoli *)
		If[
			Length[Intersection[tASegLabels, {osl1A, osl2A}]] != 2|| Length[Intersection[tBSegLabels, {osl1B, osl2B}]] != 2,
			Return[False]
		];

		(* Genero gli angoli di ciascun triangolo *)
		tAAngles =  Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[tA, {2}]];

		tBAngles =  Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[tB, {2}]];

		(* Estraggo e riordino i nome degli angoli *)
		tAAngLabels = Map[GetAngleLabel, tAAngles];
		tBAngLabels = Map[GetAngleLabel, tBAngles];
		oalA = OrderAngleLabel[alA];
		oalB = OrderAngleLabel[alB];

		(* Controllo che gli angoli appartengano ai triangoli *)
		If[
			Length[Intersection[tAAngLabels, {oalA}]] != 1|| Length[Intersection[tBAngLabels, {oalB}]] != 1,
			Return[False]
		];

		(* Controllo che l'angolo passato sia quello compreso fra i due segmenti *)
		If[
			{Characters[oalA][[2]]} != Intersection[Characters[osl1A], Characters[osl2A]] || {Characters[oalB][[2]]} != Intersection[Characters[osl1B], Characters[osl2B]],
			Return[False]
		];

		(* Controllo la congruenza di ciascuna coppia di elementi *)
		isCongruent = (CheckCongruentSegments[{osl1A, osl1B}] || MatchQ[osl1A, osl1B])
				&&
			(CheckCongruentSegments[{osl2A, osl2B}] || MatchQ[osl2A, osl2B])
				&&
			(CheckCongruentAngles[{oalA, oalB}]);

		(* In caso positivo aggiungo tutte le nuove uguaglianze dedotte dalla congruenza *)
		If[
			isCongruent,
			{
				thirdSegmentA = OrderSegmentLabel[Complement[tASegLabels, {osl1A,osl2A}][[1]]];
				thirdSegmentB = OrderSegmentLabel[Complement[tBSegLabels, {osl1B,osl2B}][[1]]];
				AddSegmentEquality[thirdSegmentA, thirdSegmentB];
				AddAngleEquality[
					GetAngleLabelBetweenSegments[osl1A,thirdSegmentA],
					GetAngleLabelBetweenSegments[osl1B,thirdSegmentB]
				];
				AddAngleEquality[
					GetAngleLabelBetweenSegments[osl2A,thirdSegmentA],
					GetAngleLabelBetweenSegments[osl2B,thirdSegmentB]
				];
				AddTriangleCongruence[tlA, tlB];
				FixAngleEqualityStruct[];
			}
		];

		isCongruent
	]

(* Dati i nomi di due triangoli, due coppie di angoli e una coppia di segmenti verifica se vale il secondo criterio di congruenza *)
CheckSecondCongruenceCriterion[tlA_, tlB_, {slA_, slB_}, {al1A_, al1B_}, {al2A_, al2B_}] :=
	Module[
		{tA, tB, triangles, tASegLabels, tBSegLabels, oslA, oslB,
		tAAngles, tBAngles, tAAngLabels, tBAngLabels, oal1A, oal1B,  oal2A, oal2B,
		isCongruent, thirdAngleA, thirdAngleB},

		(* Prendo i triangoli associati ai nomi *)
		triangles= GetTriangles[];
		tA = Select[triangles, MatchQ[GetTriangleLabel[#], StringJoin[Sort[Characters[tlA]]]]&];
		tB = Select[triangles, MatchQ[GetTriangleLabel[#], StringJoin[Sort[Characters[tlB]]]]&];

		(* Controllo l'esistenza dei triangoli *)
		If[
			MatchQ[tA, {}] || MatchQ[tB, {}],
			Return[False]
		];

		(* La soluzione ha un solo triangolo *)
		tA = tA[[1]];
		tB = tB[[1]];

		(* Estraggo e riordino i nomi dei segmenti *)
		tASegLabels = Map[GetSegmentLabel, tA];
		tBSegLabels = Map[GetSegmentLabel, tB];
		oslA = OrderSegmentLabel[slA];
		oslB = OrderSegmentLabel[slB];

		(* Controllo che i segmenti appartengano ai triangoli *)
		If[
			Length[Intersection[tASegLabels, {oslA}]] != 1|| Length[Intersection[tBSegLabels, {oslB}]] != 1,
			Return[False]
		];

		(* Genero gli angoli *)
		tAAngles =  Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[tA, {2}]];

		tBAngles =  Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[tB, {2}]];

		(* Genero e riordino le etichette *)
		tAAngLabels = Map[GetAngleLabel, tAAngles];
		tBAngLabels = Map[GetAngleLabel, tBAngles];
		oal1A = OrderAngleLabel[al1A];
		oal2A = OrderAngleLabel[al2A];
		oal1B = OrderAngleLabel[al1B];
		oal2B = OrderAngleLabel[al2B];

		(* Controllo che gli angoli appartengano ai triangoli *)
		If[
			Length[Intersection[tAAngLabels, {oal1A, oal2A}]] != 2|| Length[Intersection[tBAngLabels, {oal1B, oal2B}]] != 2,
			Return[False]
		];

		(* Controllo che il segmento sia fra i due angoli *)
		If[
			Not[
				MatchQ[
					oslA,
					StringJoin[Sort[{Characters[oal1A][[2]], Characters[oal2A][[2]]}]]
				]
			] ||
			Not[
				MatchQ[
					oslB,
					StringJoin[Sort[{Characters[oal1B][[2]], Characters[oal2B][[2]]}]]
				]
			],
			Return[False]
		];

		(* Controllo la congruenza di ciascuna coppia *)
		isCongruent = (CheckCongruentSegments[{oslA, oslB}] || MatchQ[oslA, oslB])
			&&
			(CheckCongruentAngles[{oal1A, oal1B}])
			&&
			(CheckCongruentAngles[{oal2A, oal2B}]);

		(* In caso positivo aggiungo tutte le nuove uguaglianze dedotte dalla congruenza *)
		If[
			isCongruent,
			thirdAngleA = Complement[tAAngLabels, {oal1A, oal2A}][[1]];
			thirdAngleB = Complement[tBAngLabels, {oal1B, oal2B}][[1]];
			AddAngleEquality[thirdAngleA, thirdAngleB];
			AddSegmentEquality[
				OrderSegmentLabel[StringJoin[Characters[thirdAngleA][[2]], Characters[oal1A][[2]]]],
				OrderSegmentLabel[StringJoin[Characters[thirdAngleB][[2]], Characters[oal1B][[2]]]]
			];
			AddSegmentEquality[
				OrderSegmentLabel[StringJoin[Characters[thirdAngleA][[2]], Characters[oal2A][[2]]]],
				OrderSegmentLabel[StringJoin[Characters[thirdAngleB][[2]], Characters[oal2B][[2]]]]
			];
			AddTriangleCongruence[tlA, tlB];
			FixAngleEqualityStruct[];
		];

		isCongruent
	]

(* Dati i nomi di due triangoli, tre coppie di segmenti verifica se vale il terzo criterio di congruenza *)
CheckThirdCongruenceCriterion[tlA_, tlB_, {sl1A_, sl1B_}, {sl2A_, sl2B_}, {sl3A_, sl3B_}] :=
	Module[
		{tA, tB, triangles, tASegLabels, tBSegLabels, osl1A, osl1B, osl2A, osl2B, osl3A, osl3B, isCongruent},

		(* Prendo i triangoli associati ai nomi *)
		triangles= GetTriangles[];
		tA = Select[triangles, MatchQ[GetTriangleLabel[#], StringJoin[Sort[Characters[tlA]]]]&];
		tB = Select[triangles, MatchQ[GetTriangleLabel[#], StringJoin[Sort[Characters[tlB]]]]&];

		(* Controllo l'esistenza dei triangoli *)
		If[
			MatchQ[tA, {}] || MatchQ[tB, {}],
			(Print["Non nei triangoli"];Return[False])
		];

		(* La soluzione ha un solo triangolo *)
		tA = tA[[1]];
		tB = tB[[1]];

		(* Estraggo e riordino i nomi dei segmenti *)
		tASegLabels = Map[GetSegmentLabel, tA];
		tBSegLabels = Map[GetSegmentLabel, tB];
		osl1A = OrderSegmentLabel[sl1A];
		osl2A = OrderSegmentLabel[sl2A];
		osl3A = OrderSegmentLabel[sl3A];
		osl1B = OrderSegmentLabel[sl1B];
		osl2B = OrderSegmentLabel[sl2B];
		osl3B = OrderSegmentLabel[sl3B];

		(* Controllo che i segmenti appartengano ai triangoli *)
		If[
			Length[Intersection[tASegLabels, {osl1A, osl2A, osl3A}]] != 3|| Length[Intersection[tBSegLabels, {osl1B, osl2B, osl3B}]] != 3,
			(Print["Segmenti non nei triangoli"];Return[False])
		];

		(* Controllo la congruenza *)
		isCongruent = (CheckCongruentSegments[{osl1A, osl1B}] || MatchQ[osl1A, osl1B])
			&&
			(CheckCongruentSegments[{osl2A, osl2B}] || MatchQ[osl2A, osl2B])
			&&
			(CheckCongruentSegments[{osl3A, osl3B}] || MatchQ[osl3A, osl3B]) ;

		(* In caso positivo aggiungo tutte le nuove uguaglianze dedotte dalla congruenza *)
		If[
			isCongruent,
			{
				AddAngleEquality[
					GetAngleLabelBetweenSegments[osl1A,osl2A],
					GetAngleLabelBetweenSegments[osl1B,osl2B]
				];
				AddAngleEquality[
					GetAngleLabelBetweenSegments[osl2A,osl3A],
					GetAngleLabelBetweenSegments[osl2B,osl3B]
				];
				AddAngleEquality[
					GetAngleLabelBetweenSegments[osl1A,osl3A],
					GetAngleLabelBetweenSegments[osl1B,osl3B]
				];
				AddTriangleCongruence[tlA, tlB];
				FixAngleEqualityStruct[];
			}
		];

isCongruent
]



(*** ISTRUZIONI DI DIMOSTRAZIONE ***)
(* Le seguenti funzioni hanno il compito di analizzare le stringhe di dimostrazione del problema *)
(* Quando si verifica un match con uno dei pattern previsti viene controllata la condizione corrispondente,
 		che, se verificata, corrispondera' a un passo di dimostrazione. *)

(* Angoli opposti al vertice --- ABC Opposto DBE *)
ParseProofStep[ps_]/; StringMatchQ[ps, p1_ ~~ p2_ ~~ p3_ ~~ "Opposto" ~~ p4_ ~~ p5_~~p6_]:=
	Module[
		{res},
		(* Chiamata per verificare la condizione *)
		res = CheckOppositeAngles[
			ExtractFromStringExpression[
				StringReplace[
					ps,
					p1_ ~~ p2_ ~~ p3_ ~~ "Opposto" ~~ p4_ ~~ p5_~~p6_ -> {p1~~p2~~p3, p4~~p5~~p6}
				]
			]
		];

		If[
			(* Se la condizione non e' verificata mostro un dialog di errore *)
			Not[res],
			ProofStepNotVerified[]
		];

		res
	];

(* In tutti gli altri casi si fa match con questo caso, che genera un errore di istruzione non riconosciuta *)
ParseProofStep[ps_] :=
	Module[
		{},
		(* Istruzione non riconosciuta *)
		InstructionNotRecognized["Istruzione " ~~ ps]
	];

(* Funzione che crea un dialog di errore in caso di passo di dimostrazione non verificato *)
ProofStepNotVerified[] :=
	CreateDialog[
		Column[
			{
				Spacer[50],
				Style["Condizione non verificata", Black, Bold, FontSize->16],
				Spacer[50],
				Spacer[50],
				CancelButton["Chiudi", DialogReturn[]]
			},
		Alignment->Center
		],
		Modal->True,
		WindowTitle->"Non verificata"
	];



(*** DIALOG PER I CRITERI DI CONGRUENZA ***)

(* Crea un dialog per semplificare l'inserimento di una verifica del primo criterio *)
createFirstCriterionDialog[i_] :=
	DynamicModule[
		{tlA, tlB , sl1A, sl2A, sl1B, sl2B, alA, alB, critResult, aux},
		critResult = False;
		CreateDialog[
			Column[
				{
				Style[
					Grid[
						{
							{Spacer[10]},
							{"Triangolo:",InputField[Dynamic[tlA],String, FieldSize->6],"=", InputField[Dynamic[tlB],String, FieldSize->6]},
							{"Segmento:",InputField[Dynamic[sl1A],String, FieldSize->6], "=", InputField[Dynamic[sl1B],String, FieldSize->6]},
							{"Segmento:",InputField[Dynamic[sl2A],String, FieldSize->6], "=",InputField[Dynamic[sl2B],String, FieldSize->6]},
							{"Angolo:",InputField[Dynamic[alA],String, FieldSize->6], "=",InputField[Dynamic[alB],String, FieldSize->6]},
							{Spacer[10]},
							{
								"",
								CancelButton["Annulla", DialogReturn[proofSteps[[Length[verifiedProofSteps]+1, 2]] = False]],
								"",
								DefaultButton["Applica", DialogReturn[
									(* Chiamata all' effettiva verifica del criterio *)
									aux = CheckFirstCongruenceCriterion[tlA, tlB, {sl1A, sl1B}, {sl2A, sl2B}, {alA, alB}];
								];
								If[
									aux,
									(* Aggiunge il passo di dimostrazione *)
									(proofSteps[[Length[verifiedProofSteps]+1, 1]] = "Primo Criterio su "~~tlA ~~ " e " ~~ tlB;
									If[
										(* Controllo se la tesi e' verificata in seguito alle nuove informazioni *)
										CheckThesis[],
										CreateProblemSolvedDialog[]
									];),
									ProofStepNotVerified[]
								];
								proofSteps[[Length[verifiedProofSteps]+1, 2]] = aux
								]
							}
						},
						Spacings->{1,Automatic},
						Alignment->{Center}
					],
				Large
				]
				}
			],
			Modal->True,
			WindowTitle->"Primo Criterio di Congruenza"
		];
		aux
	];

(* Crea un dialog per semplificare l'inserimento di una verifica del secondo criterio *)
createSecondCriterionDialog[i_] :=
	DynamicModule[
		{tlA, tlB , slA, slB,al1A, al1B, al2A, al2B, critResult, aux},
		critResult = False;
		CreateDialog[
			Column[
				{
					Style[
						Grid[
							{
								{Spacer[10]},
								{"Triangolo:",InputField[Dynamic[tlA],String, FieldSize->6],"=", InputField[Dynamic[tlB],String, FieldSize->6]},
								{"Segmento:",InputField[Dynamic[slA],String, FieldSize->6], "=", InputField[Dynamic[slB],String, FieldSize->6]},
								{"Angolo:",InputField[Dynamic[al1A],String, FieldSize->6], "=",InputField[Dynamic[al1B],String, FieldSize->6]},
								{"Angolo:",InputField[Dynamic[al2A],String, FieldSize->6], "=",InputField[Dynamic[al2B],String, FieldSize->6]},
								{Spacer[10]},
								{
									"",
									CancelButton["Annulla", DialogReturn[proofSteps[[Length[verifiedProofSteps]+1, 2]] = False]],
									"",
									DefaultButton[
										"Applica",
										DialogReturn[
											(* Chiamata all' effettiva verifica del criterio *)
											aux = CheckSecondCongruenceCriterion[tlA, tlB, {slA, slB}, {al1A, al1B}, {al2A, al2B}];
										];
										If[
											aux,
											(* Aggiunge il passo di dimostrazione *)
											(proofSteps[[Length[verifiedProofSteps]+1, 1]] = "Secondo Criterio su "~~tlA ~~ " e " ~~ tlB;
											If[
												(* Controllo se la tesi e' verificata in seguito alle nuove informazioni *)
												CheckThesis[],
												CreateProblemSolvedDialog[]
											];),
											ProofStepNotVerified[]
										];
										proofSteps[[Length[verifiedProofSteps]+1, 2]] = aux
									]
								}
							},
						Spacings->{1,Automatic},
						Alignment->{Center}
						],
					Large
					]
				}
			],
			Modal->True,
			WindowTitle->"Secondo Criterio di Congruenza"
		];
		aux
	];

(* Crea un dialog per semplificare l'inserimento di una verifica del terzo criterio *)
createThirdCriterionDialog[i_] :=
	DynamicModule[
		{tlA, tlB , sl1A, sl2A,sl3A, sl1B, sl2B,sl3B, critResult, aux},
		critResult = False;
		CreateDialog[
			Column[
				{
					Style[
						Grid[
						{
							{Spacer[10]},
							{"Triangolo:",InputField[Dynamic[tlA],String, FieldSize->6],"=", InputField[Dynamic[tlB],String, FieldSize->6]},
							{"Segmento:",InputField[Dynamic[sl1A],String, FieldSize->6], "=", InputField[Dynamic[sl1B],String, FieldSize->6]},
							{"Segmento:",InputField[Dynamic[sl2A],String, FieldSize->6], "=",InputField[Dynamic[sl2B],String, FieldSize->6]},
							{"Segmento:",InputField[Dynamic[sl3A],String, FieldSize->6], "=",InputField[Dynamic[sl3B],String, FieldSize->6]},
							{Spacer[10]},
							{
								"",
								CancelButton["Annulla", DialogReturn[proofSteps[[Length[verifiedProofSteps]+1, 2]] = False]],
								"",
								DefaultButton[
									"Applica",
									DialogReturn[
										(* Chiamata all' effettiva verifica del criterio *)
										aux = CheckThirdCongruenceCriterion[tlA, tlB, {sl1A, sl1B}, {sl2A, sl2B}, {sl3A, sl3B}]
									];
									If[
										aux,
										(* Aggiunge il passo di dimostrazione *)
										(proofSteps[[Length[verifiedProofSteps]+1, 1]] = "Terzo Criterio su "~~tlA ~~ " e " ~~ tlB;
										If[
											(* Controllo se la tesi e' verificata in seguito alle nuove informazioni *)
											CheckThesis[],
											CreateProblemSolvedDialog[]
										];),
										ProofStepNotVerified[]
									];
									proofSteps[[Length[verifiedProofSteps]+1, 2]] = aux
								]
							}
						},
						Spacings->{1,Automatic},
						Alignment->{Center}
						],
					Large
					]
				}
			],
			Modal->True,
			WindowTitle->"Terzo Criterio di Congruenza"
		];
		aux
	];



(*** ALTRI DIALOG ***)

(* Dialog che mostra tutti i triangoli in figura in modo separato *)
CreateTriangleDialog[] :=
	DynamicModule[
		{selectedTriangleLbl, selectedTriangle, drawingSegments, trianglePoints, triangleAngles},

		If[
			(* Nessun triangolo in figura *)
			MatchQ[GetTriangles[], {}],
			CreateDialog[
				Column[
					{
						Spacer[50],
						Style["Nessun triangolo in figura", Black, Bold, FontSize->16],
						Spacer[50],
						Spacer[50],
						CancelButton["Chiudi", DialogReturn[]]
					},
				Alignment->Center
				],
				Modal->True,
				WindowTitle->"Triangoli in figura"
			];
			Return[False]
		];

		(* Nomi dei triangoli e elementi da disegnare *)
		selectedTriangleLbl = GetTriangleLabel[GetTriangles[][[1]]];
		selectedTriangle := Select[GetTriangles[], MatchQ[selectedTriangleLbl, GetTriangleLabel[#]]&];
		trianglePoints := Union[Flatten[selectedTriangle[[1]], 1]];
		triangleAngles := Select[
			Not[
				MatchQ[
					PointInCommon[
					{# [[1]][[1]],# [[1]][[2]]},
					{# [[2]][[1]],# [[2]][[2]]}
					],
					False
				]
			]
		&][Subsets[selectedTriangle[[1]], {2}]];

		(* Segmenti da disegnare del triangolo selezionato *)
		drawingSegments := selectedTriangle /. {{l1_,{c11_,c12_}},{l2_,{c21_,c22_}}} :>
			{
				AbsoluteThickness[3],
				If[
					MatchQ[Position[segmentEqualityStruct,sl], {}],
					Black,
					colorSegmentList[
						Position[segmentEqualityStruct, sl]
						[[1, 1]] * 2
					]
				],
				Line[{{c11, c12}, {c21, c22}}
				]
			};

		CreateDialog[
			Column[
				{
					Row[
						{
							Row[
								Join[
									(* PopupMenu in cui scegliere il triangolo da disegnare *)
									{Style["Triangolo: ", Black, Bold, FontSize->16]},
									{PopupMenu[Dynamic[selectedTriangleLbl], Map[GetTriangleLabel,GetTriangles[]], FieldSize->Medium]}
								]
							],
							Spacer[100],
							(* Parte grafica in cui disegnare il triangolo *)
							Dynamic@Graphics[
								{
									{PointSize[Large],Map[Point,Map[GetPointCoordinates,trianglePoints]]},
									Map[GetPointLabelAsText, trianglePoints],
									drawingSegments,
									(Map[GetDrawingAngle, triangleAngles] ) /. {col_, thick_, circle_Circle} -> {Black, thick, circle}
								},
								PlotRange-> Automatic,
								ImageSize-> 400
							],
							Spacer[20]
						}
					],
					Row[
						{Button["Chiudi", DialogReturn[]]},
						Alignment->Center
					]
				}
			],
			Modal->True,
			WindowTitle->"Triangoli in figura"
		];
	];

(* Dialog che mostra la lista dei triangoli equivalenti *)
CreateCongruenceTriangleDialog[] :=
	Module[
		{i, j, columnContent, rowContent},

		If[
			(* Nessuna sottolista di triangoli congruenti *)
			MatchQ[triangleCongruenceStruct, {}],
			CreateDialog[
				Column[
					{
						Spacer[50],
						Style["Nessuna congruenza di triangoli", Black, Bold, FontSize->16],
						Spacer[50],
						Spacer[50],
						CancelButton["Chiudi", DialogReturn[]]
					},
				Alignment->Center
				],
				Modal->True,
				WindowTitle->"Congruenze di triangoli"
			];
			Return[False]
		];

		columnContent = {Spacer[10]};
		(* Stampo tante righe quante sono le sottoliste di congruenza *)
		For[
			i = 1,
			i <= Length[triangleCongruenceStruct],
			i++,
			rowContent = "";
			For[
				(* Aggiungo tutti i triangoli appartententi alla sottolista *)
				j = 1,
				j <= Length[triangleCongruenceStruct[[i]]],
				j++,
				rowContent = StringJoin[rowContent, "  " <> triangleCongruenceStruct[[i]][[j]] <> "  "];
			];

			columnContent = Append[
				columnContent,
				Row[Join[
					{Style[i, 20, Bold]},
					{Style[") ", 20, Bold]},
					{Style[rowContent, 18]}
				]]
			];
			columnContent = Append[
				columnContent,
				Spacer[10]
			];
		];

		columnContent = Append[
			columnContent,
				Row[
					{Button["Chiudi", DialogReturn[]]},
					Alignment->Center
				]
		];

		CreateDialog[
			Column[
				columnContent
			],
			Modal->True,
			WindowTitle->"Lista di congruenze"
		];
	];

(* Dialog che segnala la corretta soluzione del problema *)
CreateProblemSolvedDialog[] :=
	Module[
		{},
		CreateDialog[
			Column[
				{
					Spacer[50],
					Style["Tesi del problema verificata", Black, Bold, FontSize->16],
					Spacer[50],
					Spacer[50],
					CancelButton["Chiudi", DialogReturn[]]
				},
			Alignment->Center
			],
			Modal->False,
			WindowTitle->"Problema risolto"
		];
	]

(* Dialog che segnala un errore nell'analisi di istruzioni di ipotesi *)
ErrorMessageInHypothesis[msg_] :=
	Module[
		{},

		errorInHypothesisParsing = True;

		CreateDialog[
			Column[
				{
					Spacer[50],
					Style["Errore nei dati del problema", Black, Bold, FontSize->16],
					Spacer[50],
					Spacer[50],
					Style[msg ~~ " non presente.", Black, Bold, FontSize->14],
					Spacer[50],
					Spacer[50],
					CancelButton["Chiudi", DialogReturn[]]
				},
			Alignment->Center
			],
			Modal->True,
			WindowTitle->"Errore nei dati"
		];
	]

(* Dialog che segnala un errore nell'analisi di istruzioni di dimostrazione *)
InstructionNotRecognized[msg_] :=
	Module[
		{},
		errorInHypothesisParsing = True;

		CreateDialog[
			Column[
				{
					Spacer[50],
					Style["Errore nell'interpretazione", Black, Bold, FontSize->16],
					Spacer[50],
					Spacer[50],
					Style[msg , Black, Bold, FontSize->14],
					Spacer[50],
					Spacer[50],
					CancelButton["Chiudi", DialogReturn[]]
				},
			Alignment->Center
			],
			Modal->True,
			WindowTitle->"Istruzione non riconosciuta"
		];
	]



(*** CONTROLLI SULLA TESI ***)

(* Verifica che i tre punti passati in input formino un triangolo isoscele *)
CheckIsoscelesTriangle[{pl1_, pl2_, pl3_}] :=
	Module[
		{},
		CheckCongruentSegments[{pl1 ~~ pl2, pl2 ~~ pl3}] || CheckCongruentSegments[{pl1 ~~ pl3, pl2 ~~ pl3}] || CheckCongruentSegments[{pl1 ~~ pl2, pl1 ~~ pl3}]
			||
		CheckCongruentAngles[{pl1 ~~ pl2 ~~ pl3, pl2 ~~ pl1 ~~ pl3}] || CheckCongruentAngles[{pl1 ~~ pl2 ~~ pl3, pl1 ~~ pl3 ~~ pl2}] || CheckCongruentAngles[{pl2 ~~ pl1 ~~ pl3, pl1 ~~ pl3 ~~ pl2}]
	]

(* Funzione che verifica se le condizioni della tesi sono verificate *)
CheckThesis[] :=
	Module[
		{thesisVerified},
		thesisVerified = True;
		For[
			i = 1,
			i <= Length[problemThesisConditions],
			i++,
			If[
				problemThesisConditions[[i]] == False,
				thesisVerified = False
			];
		];
		thesisVerified
	]

(* Le seguenti funzioni analizzano la stringa di tesi, e verificano il match con i pattern specificati,
 	 associando alla tesi una condizione da verificare *)

(* Uguaglianza fra segmenti *)
ParseThesis[thstr_] /; StringMatchQ[thstr, p1_ ~~ p2_ ~~ "=" ~~ p3_ ~~ p4_] :=
	Module[
		{},
		problemThesisConditions :=
			{
				CheckCongruentSegments[
					ExtractFromStringExpression[
						StringReplace[
							thstr,
							p1_ ~~ p2_ ~~ "=" ~~ p3_ ~~ p4_ -> {p1 ~~ p2, p3 ~~ p4}
						]
					]
				]
			};
	]

(* Uguaglianza fra angoli *)
ParseThesis[thstr_] /; StringMatchQ[thstr, p1_ ~~ p2_ ~~ p3_ ~~ "=" ~~ p4_ ~~ p5_ ~~ p6_] :=
	Module[
		{},
		problemThesisConditions :=
			{
				CheckCongruentAngles[
					ExtractFromStringExpression[
						StringReplace[
							thstr,
							p1_ ~~ p2_ ~~ p3_ ~~ "=" ~~ p4_ ~~ p5_ ~~ p6_ -> {p1 ~~ p2 ~~ p3, p4 ~~ p5 ~~ p6}
						]
					]
				]
			};
	]

(* Congruenza di triangoli *)
ParseThesis[thstr_] /; StringMatchQ[thstr, p1_ ~~ p2_ ~~ p3_ ~~ "Congruente" ~~ p4_ ~~ p5_ ~~ p6_ ] :=
	Module[
		{},
		problemThesisConditions :=
			{
				CheckCongruentTriangles[
					ExtractFromStringExpression[
						StringReplace[
							thstr,
							p1_ ~~ p2_ ~~ p3_ ~~ "Congruente" ~~ p4_ ~~ p5_ ~~ p6_ -> {p1 ~~ p2 ~~ p3, p4 ~~ p5 ~~ p6}
						]
					]
				]
			};
	]

(* Triangolo isoscele *)
ParseThesis[thstr_] /; StringMatchQ[thstr, p1_ ~~ p2_ ~~ p3_ ~~ "Isoscele"] :=
	Module[
		{},
		problemThesisConditions :=
			{
				CheckIsoscelesTriangle[
					ExtractFromStringExpression[
						StringReplace[
							thstr,
							p1_ ~~ p2_ ~~ p3_ ~~ "Isoscele" -> {p1, p2, p3}
						]
					]
				]
			};
	]

(* Caso di default, se non ha fatto match con nessun altro caso *)
ParseThesis[thstr_] :=
	Module[
		{},
		(* Ritorna False, non potra' mai essere verificata *)
		problemThesisConditions := {False}
	]

(* Resetta i passi di dimostrazione fatti *)
ResetProofSteps[] :=
	Module[
		{},
		segmentEqualityStruct = backupSegmentEqualityStruct;
		angleEqualityStruct = backupAngleEqualityStruct;
		triangleCongruenceStruct = backupTriangleCongruenceStruct;
		proofSteps = Table[{"", False}, 25];
	]



(* Disattiva alcuni messaggi di errore *)
Off[Solve::ifun, NSolve::ratnz, StringLength::string, StringJoin::string, SetDelayed::write, Attributes::locked]



(*** FUNZIONI PUBBLICHE ***)

(* Inizializza alcune variabili necessarie per la creazione dei widget di generazione e risoluzione dei problemi*)
InitPackage[] :=
	Module[
		{},
		(* Appearance dei punti *)
		locatorAppearance = Graphics[{Black,Table[Disk[{0,0},i],{i,2}]}, ImageSize->10];
		(* Dimensioni del widget *)
		widgetSize = {Full, 460};
	]

(* Crea l'interfaccia per la creazione di problemi *)
CreateProblem[] :=
	Module[
	{dialog},
	CellPrint[ExpressionCell[
			Framed[Manipulate[
				(* Appearance dei punti *)
				locatorAppearance = Graphics[{Black,Table[Disk[{0,0},i],{i,2}]}, ImageSize->10];
				(* Dimensioni del widget *)
				widgetSize = {Full, 460};
				(* Plot range *)
				plotRange = 10;

				(* Griglie di sfondo *)
				backgroundGridVersion1 =
					{
						Thin,
						Append[
							Table[Line[{{i,10},{i, -10}}], {i, -plotRange, plotRange}],
							Table[Line[{{10,i},{-10,i}}], {i, -plotRange, plotRange}]
							]
					};
				backgroundGridVersion2 =
					{
						{
							RGBColor[0,0.75,0.95,0.48],
							Table[Polygon[{{-10, i}, {10,i}, {10, i+1}, {-10, i+1}}], {i, -(plotRange-1), plotRange, 2}]
						},
						backgroundGridVersion1
					};

				(* Strutture temporanee, a cui si devono aggiungere i punti derivanti da intersezioni *)
				(* Lista di punti inseriti, generata dai locatorPoints *)
				tpoints:=
					Table[{myAlphabet[[Position[locatorPoints, pt]/. {{el_}} -> el]], pt}, {pt, locatorPoints}];
				(* Ogni possibile segmento; sottoinsiemi di points di due elementi *)
				tallSegments := Subsets[tpoints, {2}];
				(* Segmenti *)
				tsegments :=
					Select[ContainsAll[checkBoxes,{GetSegmentLabel[#]}]&][tallSegments];
				(* Segmenti da disegnare *)
				trealSegments :=
					Map[GetSegmentLine,Select[ContainsAll[checkBoxes,{GetSegmentLabel[#]}]&][tallSegments]];

				(* Punti di intersezione *)
				tsegspairs :=
					Select[ContainsAll[checkBoxes,{GetSegmentLabel[#]}]&][ Subsets[tpoints, {2}]];
				pointsIntStruct:=
					Map[FindIntersection, Subsets[tsegspairs, {2}]];
				If[
					MatchQ[Flatten[pointsIntStruct], {}],
					pointsInt := {},
					pointsInt:= (pointsIntStruct //.{}->Sequence[])[[All, 1]]
				];

				(* Ricalcolo aggiungendo i punti derivanti da intersezioni *)
				points :=
					Table[{myAlphabet[[Position[Join[locatorPoints,pointsInt], pt]/. {{el_}} -> el]], pt}, {pt, Join[locatorPoints,pointsInt]}];
				allSegments := Subsets[points, {2}];
				segments :=
					Select[ContainsAll[checkBoxes,{GetSegmentLabel[#]}]&][allSegments];
				realSegments :=
					Map[GetSegmentLine,Select[ContainsAll[checkBoxes,{GetSegmentLabel[#]}]&][allSegments]];


				(* Lista di etichette di punti *)
				pointLabels :=
					Map[GetPointLabelAsText, points];
				(* Angoli da disegnare *)
				angles :=
				Select[
					Not[
						MatchQ[
							PointInCommon[
							{# [[1]][[1]],# [[1]][[2]]},
							{# [[2]][[1]],# [[2]][[2]]}
							],
							False
						]
					]
				&][Subsets[segments, {2}]];

				(* Istruzioni dei dati *)
				contentDataInstructions :=
					Select[Not[MatchQ[#, ""]]&][dataInstructions];

				(* Parte grafica *)
				Graphics[
					{
						{Black,AbsoluteThickness[3], realSegments},
						(Map[GetDrawingAngle, angles] ) /. {col_, thick_, circle_Circle} -> {Black, thick, circle},
						Switch[backgroundGrid, "Tipo 1", backgroundGridVersion1, "Tipo 2", backgroundGridVersion2, "Nessuno", {}],
						pointLabels,
						{PointSize[Large],Map[Point,pointsInt]}
					},
					PlotRange-> plotRange + 0.1,
					ImageSize-> Full
				],

				(* Variabili della Manipulate *)
				{{locatorPoints,{{-6, 0}, {6,0}}}, Locator, LocatorAutoCreate-> All, Appearance-> locatorAppearance},
				{{backgroundGrid,"Tipo 1", Style["Griglia in sfondo:", Bold]}, {"Tipo 1", "Tipo 2", "Nessuno"}},
				Spacer[10],
				{myAlphabet, {{"A", "B", "C", "D", "E", "F", "G", "H", "I", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "Z"}}, ControlType->None},
				{dataInstructions, {{"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""}}, ControlType->None},
				{thesis, {""}, ControlType->None},

				(* Per ogni possibile segmento da disegnare creo una checkbox corrispondente *)
				Dynamic[If[Not[MatchQ[allSegments, {}]], Identity, Invisible]@
		  		Control[
					{{checkBoxes, {}, Style["Segmenti da disegnare:", Bold]},
		    	Dynamic[Map[GetSegmentLabel, allSegments]],
		    	ControlType -> CheckboxBar}]
				],
				Spacer[10],
				(* Per ogni punto creo un campo di input in cui modificarne il nome *)
				Dynamic@Row[
					Join[
						{Style["\t Nomi dei punti: ", Bold]},
						Table[With[{i=j},InputField[Dynamic[myAlphabet[[i]]],FieldSize->4]],{j,Length[points]}]
					]
				],
				Spacer[10],
				(* Elenco dei campi di input per i dati *)
				Dynamic@Row[
					Join[
						Join[
							{Style["\  Dati del problema: ", Blue, Bold]},
							{
								Grid[
									Table[
										With[
											{i=j},
											{InputField[Dynamic[dataInstructions[[i]]],String,FieldSize->22]}
										],
							      {j,Length[contentDataInstructions] + 1}
									]
					   		]
					 		}
					 ],
					 {Spacer[40]},
					 (* Campo di input per la tesi *)
					 Join[{Style["\  Tesi : ", Blue, Bold]},{InputField[Dynamic[thesis], String, FieldSize->22]}],
					 {Spacer[40]},
					 {Button["Vai alla Dimostrazione",
							(*dialog = CreateDialog[
										Column[{Dynamic@ProgressIndicator[Appearance -> "Indeterminate"]}],
										WindowTitle -> "Creazione della figura...", Modal->True];*)

							(* Effettuo l'export del problema, generando la nuova figura corretta *)
							returnval = ExportProblem[
							 contentDataInstructions,
							 thesis,
							 points,
							 pointsIntStruct,
							 checkBoxes,
							 widgetSize
							];
							(*NotebookClose[dialog];*)
							If[
								MatchQ[returnval, True],
								(
									(* Se non si sono riscontrati problemi si passa alla parte di dimostrazione *)
									SolveProblem[];
									NotebookFind[InputNotebook[], "createProblem", All, CellTags, AutoScroll -> True];
						 			NotebookDelete[];
								)
							];
						, BaseStyle->{FontSize->16}]}
					]
				],
				 ContentSize->widgetSize,
				 LabelStyle->Directive[Medium]
			], RoundingRadius -> 6, FrameStyle -> Directive[RGBColor[0.17, 0.39, 0.58], Thickness[4]]]
			, CellTags -> "createProblem"]]
		]

(* Crea l'interfaccia per la risoluzione di problemi *)
SolveProblem[] :=
Module[
{},
	CellPrint[ExpressionCell[
		Framed[Dynamic@Row[
			{
				Framed[Column[
					{
						Style["Tesi: " ~~ problemThesis, Bold, FontSize->16],
						Spacer[20],
						Spacer[20],
						Spacer[20],
						Spacer[20],
						Style["Dimostrazione:" , Bold, FontSize->16],
						(* Elenco di tutti i passi di dimostrazione *)
						Grid[
							Table[
								With[
									{i=j},
									Join[
										{InputField[Dynamic[proofSteps[[i, 1]]],String,FieldSize->18]},
										{Import[FileNameJoin[{NotebookDirectory[], "tick.png"}], ImageSize-> 26]}
									]
								],
								{j,Length[verifiedProofSteps]}
							]
						],
						Spacer[20],
						Column[
							{
							(* Elenco dei bottoni per la chiamata ai criteri di congruenza *)
							Row[
								{
									Button["Primo Crit.", proofSteps[[Length[verifiedProofSteps]+1, 2]] = createFirstCriterionDialog[i], BaseStyle->{FontSize->14}],
									Spacer[19],
									Button["Secondo Crit.", proofSteps[[Length[verifiedProofSteps]+1, 2]] = createSecondCriterionDialog[i], BaseStyle->{FontSize->14}],
									Spacer[19],
									Button["Terzo Crit.", proofSteps[[Length[verifiedProofSteps]+1, 2]] = createThirdCriterionDialog[i], BaseStyle->{FontSize->14}]
								}
							],
							(* Campo di input in cui inserire passi di dimostrazione *)
							Row[
								{
									InputField[Dynamic[proofSteps[[Length[verifiedProofSteps]+1, 1]]],String,FieldSize->18],
									Spacer[12],
									Button[
										" Verifica  ",
										(proofSteps[[Length[verifiedProofSteps]+1, 2]] =
											ParseProofStep[
												StringReplace[proofSteps[[Length[verifiedProofSteps]+1, 1]], " " -> ""]
											];
											If[
												(* Controllo sulla tesi *)
												CheckThesis[],
												CreateProblemSolvedDialog[]
											];),
											BaseStyle->{FontSize->14}
									]
								}
							],
							Spacer[100],
							Spacer[100],
							Spacer[100],
							Spacer[100],

							Grid[
								 {
								  {Spacer[70]},
								  {
									   Spacer[10],
										 (* Bottone per aprire il dialog dei triangoli in figura *)
									   Button[" Triangoli in figura ", CreateTriangleDialog[]],
									   Spacer[10]
								   },
									 {
									   Spacer[10],
										 (* Bottone per aprire il dialog delle equivalenze *)
									   Button[" Congruenza triangoli ", CreateCongruenceTriangleDialog[]],
									   Spacer[10]
									   },
									 {
									   Spacer[10],
									   Spacer[10],
									   Spacer[10]
									   },
									  {
									   Spacer[10],
										 (* Bottone per resettare i passi di dimostrazione *)
									   Button["  Reset Dimostrazione ", ResetProofSteps[]],
									   Spacer[10]
									   },
									 	{
									   Spacer[10],
										  (* Bottone per tornare alla creazione di problemi *)
									 	 Button[
												 "  Torna alla creazione di problema  ",
												 (
													 CreateProblem[];
													 NotebookFind[InputNotebook[], "solveProblem", All, CellTags, AutoScroll -> False];
													 NotebookDelete[];
												 )
									 		],
									   Spacer[10]
									  }

								  }, Spacings -> {1, Automatic}, Alignment -> {Center}
							]
							}
						]
					}
				], RoundingRadius -> 6, FrameStyle -> Directive[RGBColor[0.37, 0.59, 0.63], Thickness[3]], FrameMargins -> 16],
				Spacer[10],
				Spacer[10],
				Spacer[10],
				Spacer[10],
				(* Parte grafica per mostrare la figura aggiornata *)
				Graphics[
					{
						Map[GetDrawingAngle, finalAngles],
						finalDrawingSegments,
						{PointSize[Large],Map[Point,Map[GetPointCoordinates,problemFinalPoints]]},
						finalPointLabels
					},
					PlotRange-> 10 + 2 + 0.1,
					ImageSize-> {550, 550 * 0.9}
				]
			},
			ImageSize-> {Full, 560}
		], RoundingRadius -> 6, FrameStyle -> Directive[RGBColor[0.17, 0.39, 0.58], Thickness[4]], FrameMargins -> 16]
	, CellTags -> "solveProblem"]];

	NotebookFind[InputNotebook[], "createProblem", All, CellTags, AutoScroll -> False];
	NotebookDelete[];
	If[
		(* Controllo la tesi alla creazione *)
		CheckThesis[],
		CreateProblemSolvedDialog[]
	];
]


SetAttributes[InitPackage, {Protected, Locked}];
SetAttributes[CreateProblem, {Protected, Locked}];

End[];

EndPackage[];
