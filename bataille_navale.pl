%%%%%%%%% matrice 10 * 10 %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% positionnement %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bateaux joueur 1 (id, ligne, colonne) : humain

% bateau 1 : 2 cases
%bateau_joueur1(0, 4, 5).
%bateau_joueur1(0, 4, 6).

% bateau 2 : 3 cases
%bateau_joueur1(1, 3, 4).
%bateau_joueur1(1, 3, 5).
%bateau_joueur1(1, 3, 6).

% bateau 3 : 3 cases
%bateau_joueur1(2, 2, 7).
%bateau_joueur1(2, 3, 7).
%bateau_joueur1(2, 4, 7).

% bateau 4 : 4 cases
%bateau_joueur1(3, 5, 4).
%bateau_joueur1(3, 5, 5).
%bateau_joueur1(3, 5, 6).
%bateau_joueur1(3, 5, 7).

% bateau 5 : 5 cases
%bateau_joueur1(4, 3, 8).
%bateau_joueur1(4, 4, 8).
%bateau_joueur1(4, 5, 8).
%bateau_joueur1(4, 6, 8).
%bateau_joueur1(4, 7, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bateaux joueur 2 (id, l, c) : ordi 

% bateau 1 : 2 cases
%bateau_joueur2(0, 1, 3).
%bateau_joueur2(0, 2, 3).

% bateau 2 : 3 cases
%bateau_joueur2(1, 4, 5).
%bateau_joueur2(1, 5, 5).
%bateau_joueur2(1, 6, 5).

% bateau 3 : 3 cases
%bateau_joueur2(2, 6, 1).
%bateau_joueur2(2, 7, 1).
%bateau_joueur2(2, 8, 1).

% bateau 4 : 4 cases
%bateau_joueur2(3, 2, 7).
%bateau_joueur2(3, 2, 8).
%bateau_joueur2(3, 2, 9).
%bateau_joueur2(3, 2, 10).

% bateau 5 : 5 cases
%bateau_joueur2(4, 10, 6).
%bateau_joueur2(4, 10, 7).
%bateau_joueur2(4, 10, 8).
%bateau_joueur2(4, 10, 9).
%bateau_joueur2(4, 10, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% placement du joueur et de l'ordinateur
positionner :- positionner_tousJ1, !, write('\n'), positionner_tousJ2, !.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% choix du placement des bateaux par l utilisateur

% bateaux joueur 1 (id, ligne, colonne) : humain
:- dynamic(bateau_joueur1/3).

positionner_tousJ1:-positionner_t2,positionner_t3,positionner_t3Bis,positionner_t4,positionner_t5.

positionner_t2:-write('Bateau de taille 2\n'),write('Dans quelle direction voulez vous placer le bateau Verticale/Horizontale(1/0)?\n'),read(Rep),Rep==0,positionner_bateauH(2,1).
positionner_t2:-positionner_bateauV(2,1).

positionner_t3:-write('Bateau de taille 3\n'),write('Dans quelle direction voulez vous placer le bateau Verticale/Horizontale(1/0)?\n'),read(Rep),Rep==0,positionner_bateauH(3,2).
positionner_t3:-positionner_bateauV(3,2).

positionner_t3Bis:-write('Bateau de taille 3 2eme\n'),write('Dans quelle direction voulez vous placer le bateau Verticale/Horizontale(1/0)?\n'),read(Rep),Rep==0,positionner_bateauH(3,3).
positionner_t3Bis:-positionner_bateauV(3,3).

positionner_t4:-write('Bateau de taille 4\n'),write('Dans quelle direction voulez vous placer le bateau Verticale/Horizontale(1/0)?\n'),read(Rep),Rep==0,positionner_bateauH(4,4).
positionner_t4:-positionner_bateauV(4,4).

positionner_t5:-write('Bateau de taille 5\n'),write('Dans quelle direction voulez vous placer le bateau Verticale/Horizontale(1/0)?\n'),read(Rep),Rep==0,positionner_bateauH(5,5).
positionner_t5:-positionner_bateauV(5,5).

%%%%% Positonnement horizontal d un bateau
positionner_bateauH(T,Id):-write('Sur quelle ligne voulez vous placer le bateau ?\n'),read(L),write('Sur quelle colonne?\n'),read(C),Cbis is C + -1,positionner_pt_bateauH(Id,L,Cbis,T).
positionner_bateauH(T,Id):-bateau_joueur1(Id,_,_),retract(bateau_joueur1(Id,_,_)),positionner_bateauH(T,Id).
positionner_bateauH(T,Id):-positionner_bateauH(T,Id).

positionner_pt_bateauH(_,_,_,T):-T<1.
positionner_pt_bateauH(Id,X,Y,T):-Ybis is Y+1,T2 is T-1, Ybis<11,not(bateau_joueur1(_,X,Ybis)),assert(bateau_joueur1(Id,X,Ybis)),positionner_pt_bateauH(Id,X,Ybis,T2).
positionner_pt_bateauH(_,_,Y,_):-Ybis is Y+1,not(Ybis<11),write('Placement hors du terrain\n'),false.
positionner_pt_bateauH(_,X,Y,_):-Ybis is Y+1,bateau_joueur1(_,X,Ybis),write('Il y a deja un bateau a cet endroit\n'),false.

%%%%% Positonnement vertical d un bateau
positionner_bateauV(T,Id):-write('Sur quelle colonne voulez vous placer le bateau ?\n'),read(C),write('Sur quelle ligne?\n'),read(L),Lbis is L + -1,positionner_pt_bateauV(Id,Lbis,C,T).
positionner_bateauV(T,Id):-bateau_joueur1(Id,_,_),retract(bateau_joueur1(Id,_,_)),positionner_bateauV(T,Id).
positionner_bateauV(T,Id):-positionner_bateauV(T,Id).

positionner_pt_bateauV(_,_,_,T):-T<1.
positionner_pt_bateauV(Id,X,Y,T):-Xbis is X+1,T2 is T-1, Xbis<11,not(bateau_joueur1(_,Xbis,Y)),assert(bateau_joueur1(Id,Xbis,Y)),positionner_pt_bateauV(Id,Xbis,Y,T2).
positionner_pt_bateauV(_,X,_,_):-Xbis is X+1,not(Xbis<11),write('Placement hors du terrain\n'),false.
positionner_pt_bateauV(_,X,Y,_):-Xbis is X+1,bateau_joueur1(_,Xbis,Y),write('Il y a deja un bateau a cet endroit\n'),false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% placement des bateaux de l'ordinateur

:- dynamic(bateau_joueur2/3).

positionner_tousJ2:-write('Placement des bateaux de l ordi'),positionner_t2J2,positionner_t3J2,positionner_t3BisJ2,positionner_t4J2,positionner_t5J2.

positionner_t2J2:-random_between(0,1,Rep),Rep==0,positionner_bateauHJ2(2,1).
positionner_t2J2:-positionner_bateauVJ2(2,1).

positionner_t3J2:-random_between(0,1,Rep),Rep==0,positionner_bateauHJ2(3,2).
positionner_t3J2:-positionner_bateauVJ2(3,2).

positionner_t3BisJ2:-random_between(0,1,Rep),Rep==0,positionner_bateauHJ2(3,3).
positionner_t3BisJ2:-positionner_bateauVJ2(3,3).

positionner_t4J2:-random_between(0,1,Rep),Rep==0,positionner_bateauHJ2(4,4).
positionner_t4J2:-positionner_bateauVJ2(4,4).

positionner_tJ2:-random_between(0,1,Rep),Rep==0,positionner_bateauHJ2(5,5).
positionner_t5J2:-positionner_bateauVJ2(5,5).

%%%%% Positonnement horizontal d un bateau
positionner_bateauHJ2(T,Id):-random_between(1,10,L),random_between(1,10,C),Cbis is C + -1,positionner_pt_bateauHJ2(Id,L,Cbis,T).
positionner_bateauHJ2(T,Id):-bateau_joueur2(Id,_,_),retract(bateau_joueur2(Id,_,_)),positionner_bateauHJ2(T,Id).
positionner_bateauHJ2(T,Id):-positionner_bateauHJ2(T,Id).


positionner_pt_bateauHJ2(_,_,_,T):-T<1.
positionner_pt_bateauHJ2(Id,X,Y,T):-Ybis is Y+1,T2 is T-1, Ybis<11,not(bateau_joueur2(_,X,Ybis)),assert(bateau_joueur2(Id,X,Ybis)),positionner_pt_bateauHJ2(Id,X,Ybis,T2).
positionner_pt_bateauHJ2(_,_,Y,_):-Ybis is Y+1,not(Ybis<11),false.
positionner_pt_bateauHJ2(_,X,Y,_):-Ybis is Y+1,bateau_joueur2(_,X,Ybis),false.


%%%%% Positonnement vertical d un bateau
positionner_bateauVJ2(T,Id):-random_between(1,10,C),random_between(1,10,L),Lbis is L + -1,positionner_pt_bateauVJ2(Id,Lbis,C,T).
positionner_bateauVJ2(T,Id):-bateau_joueur2(Id,_,_),retract(bateau_joueur2(Id,_,_)),positionner_bateauVJ2(T,Id).
positionner_bateauVJ2(T,Id):-positionner_bateauVJ2(T,Id).

positionner_pt_bateauVJ2(_,_,_,T):-T<1.
positionner_pt_bateauVJ2(Id,X,Y,T):-Xbis is X+1,T2 is T-1, Xbis<11,not(bateau_joueur2(_,Xbis,Y)),assert(bateau_joueur2(Id,Xbis,Y)),positionner_pt_bateauVJ2(Id,Xbis,Y,T2).
positionner_pt_bateauVJ2(_,X,_,_):-Xbis is X+1,not(Xbis<11),false.
positionner_pt_bateauVJ2(_,X,Y,_):-Xbis is X+1,bateau_joueur2(_,Xbis,Y),false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% J1 : Humain %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mise en mémoire des coups tirés
:- dynamic(coups_tires_joueur1/2).

tirer_J1(X, _) :- X > 10, write('Tir invalide en X').
tirer_J1(_, Y) :- Y > 10, write('Tir invalide en Y').
tirer_J1(X, Y) :- coups_tires_joueur1(X, Y), write('Coup deja joue !').
tirer_J1(X, Y) :- X < 11, Y < 11, not(coups_tires_joueur1(X, Y)), assert(coups_tires_joueur1(X, Y)), 
bateau_joueur2(Id, X, Y), write('Touche !'), couler_joueur1(Id), write('\nCoule !\n'), partie_terminee.

% on récupère tous les points du bateau grâce à son id et on vérifie si ils sont tous dans la liste des coups_tires
couler_joueur1(Id) :- forall(bateau_joueur2(Id, X, Y), coups_tires_joueur1(X, Y)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% J2 : Ordi %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mise en mémoire des coups tirés et des bateaux touchés 
:- dynamic(coups_tires_joueur2/2).
:- dynamic(bateaux_touches_joueur2/3).

% construit la liste des points adjacents à X et Y sous la forme [[X+1, Y], [X-1, Y], [X, Y+1], [X-1, Y-1]]
adjacent(X, Y, [[XPlus, Y], [XMinus, Y], [X, YPlus], [X, YMinus]]) :- XMinus is X - 1, XPlus is X + 1, YMinus is Y - 1, YPlus is Y + 1.

% inutile de tirer dans une case esseulée
evaluer_tir_alea([_, _], []) :- tirer_J2([]).

evaluer_tir_alea([X, Y], [_|_]) :- tirer_J2(X, Y).

% on récupère tous les points du bateau grâce à son id et on vérifie si ils sont tous dans la liste des coups_tires
couler_joueur2(Id) :- forall(bateau_joueur1(Id, X, Y), coups_tires_joueur2(X, Y)).

% on stocke tous les prédicats "bateaux_touches_joueur2" dans une liste
tirer_J2 :- findall([X, Y], bateaux_touches_joueur2(_, X, Y), R), tirer_J2(R).

% si la liste est vide on tire au hasard dans une case qui possède au moins une case adjacente où on n'a pas tiré 
tirer_J2([]) :- random_between(1, 10, X), random_between(1, 10, Y), 
adjacent(X, Y, LAdj), findall([XTires, YTires], coups_tires_joueur2(XTires, YTires), LCoupsTires),
subtract(LAdj, [[0, _], [_, 0], [11, _], [_, 11]|LCoupsTires], LPossibles),
evaluer_tir_alea([X, Y], LPossibles).

% autrement on applique une stratégie
tirer_J2([T|Q]) :- strategie_tir([T|Q]).

tirer_J2(X, Y) :- coups_tires_joueur2(X, Y), tirer_J2.

tirer_J2(X, Y) :- not(coups_tires_joueur2(X, Y)), assert(coups_tires_joueur2(X, Y)), 
write('Joueur 2 : tir en '), write(X), write(' * '), write(Y), 
bateau_joueur1(Id, X, Y), assert(bateaux_touches_joueur2(Id, X, Y)), write('\nTouche !'), 
couler_joueur2(Id), write('\nCoule !\n'), retractall(bateaux_touches_joueur2(Id, _, _)), partie_terminee.

%%%%%%%%%%%%%%%%%%%%%%%%%%

% lorsqu'il n'y a qu'un seul élément touché, on prend les points adjacents
% on supprime les points qui se situent hors du terrain et les coups déjà tirés
strategie_tir([[X, Y]]) :- adjacent(X, Y, LAdj), 
findall([XTires, YTires], coups_tires_joueur2(XTires, YTires), LCoupsTires), 
subtract(LAdj, [[0, _], [_, 0], [11, _], [_, 11]|LCoupsTires], [[XFinal, YFinal]|_]),
tirer_J2(XFinal, YFinal).

% lorsqu'il y en a plusieurs, on prend le premier et on cherche un alignement par rapport à ce point
strategie_tir([[X, Y], [_, _]|_]) :- findall(XDiff, (bateaux_touches_joueur2(_, XDiff, Y), XDiff\==X), XTouches), findall(YDiff, (bateaux_touches_joueur2(_, X, YDiff), YDiff\==Y), YTouches),
evaluer_alignement([X, Y], XTouches, YTouches).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% trouve les extrémités verticales à partir des coordonnées en X des bateaux touchés
extremites_verticales([X, Y], XTouches, XTrouves) :- max_list([X|XTouches], XMax), min_list([X|XTouches], XMin), XSup is XMax + 1, XInf is XMin - 1,
findall(X2, coups_tires_joueur2(X2, Y), XTires),
subtract([XSup,XInf], [0, 11 | XTires], XTrouves).

% trouve les extrémités horizontales à partir des coordonnées en Y des bateaux touchés
extremites_horizontales([X, Y], YTouches, YTrouves) :- max_list([Y|YTouches], YMax), min_list([Y|YTouches], YMin), YSup is YMax + 1, YInf is YMin - 1,
findall(Y2, coups_tires_joueur2(X, Y2), YTires),
subtract([YSup,YInf], [0, 11 | YTires], YTrouves).
 
evaluer_alignement([X, Y], [], []) :- strategie_tir([[X, Y]]).

% alignement vertical : on essaye de tirer aux des extrémités verticales
evaluer_alignement([X, Y], [T1|Q1], []) :- extremites_verticales([X, Y], [T1|Q1], XTrouves),
evaluer_tir([X, Y], XTrouves, []).

% alignement horizontal : on essaye de tirer aux extrémités horizontales
evaluer_alignement([X, Y], [], [T2|Q2]) :- extremites_horizontales([X, Y], [T2|Q2], YTrouves),
evaluer_tir([X, Y], [], YTrouves).

% cas particulier : double alignement
evaluer_alignement([X, Y], [T1|Q1], [T2|Q2]) :- extremites_verticales([X, Y], [T1|Q1], XTrouves), extremites_horizontales([X, Y], [T2|Q2], YTrouves),
evaluer_tir([X, Y], XTrouves, YTrouves).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% évaluation des tirs

evaluer_tir([X, Y], [], []) :- strategie_tir([[X, Y]]).

evaluer_tir([_, Y], [XChoisi|_], []) :- tirer_J2(XChoisi, Y).

evaluer_tir([X, _], [], [YChoisi|_]) :- tirer_J2(X, YChoisi).

evaluer_tir([_, Y], [XChoisi|_], [_|_]) :- tirer_J2(XChoisi, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%

% partie terminée

partie_terminee :- findall([X, Y], coups_tires_joueur1(X, Y), TirsJ1), findall([X1, Y1], bateau_joueur2(_, X1, Y1), BateauxJ2), subtract(BateauxJ2, TirsJ1, ResteJ1),
findall([X2, Y2], coups_tires_joueur2(X2, Y2), TirsJ2), findall([X3, Y3], bateau_joueur1(_, X3, Y3), BateauxJ1), subtract(BateauxJ1, TirsJ2, ResteJ2), 
partie_terminee(ResteJ1, ResteJ2).

partie_terminee([], [_|_]) :- write('Partie terminee\nLe joueur 1 a gagne\n').
partie_terminee([_|_], []) :- write('Partie terminee\nLe joueur 2 a gagne\n').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Affichage %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Utilitaires affichage
premelement([Prem|_],Prem).
reste([_|Reste],Reste).

substituer(_,_ ,[] ,[]).
substituer(X,Y,[X|L1],[Y|L2]):- substituer(X,Y,L1,L2).
substituer(X,Y,[T|L1],[T|L2]):- substituer(X,Y,L1,L2), X \= T.

substituer_non_Bat(A,List):-A>10,write(List),nl.
substituer_non_Bat(A,List):-substituer(A,'~',List,L2),X1 is A+1,substituer_non_Bat(X1,L2).

%%%%%%%% Affichage de tout

afficher :- write('Bateaux J1 : \n'), afficher_bateauJ1, !, 
write('\n\Coups tires J1 : \n'), afficher_tire, !, 
write('\n\nBateaux J2 : \n'), afficher_bateauJ2, !.

%%%%%%%% Affichage map joueur1

construire_map_J1(Res,Bat,T,_):-T==1,Bat==[],substituer_non_Bat(1,Res).
construire_map_J1(Res,_,T,L):-T==0,findall(Y,bateau_joueur1(_,L,Y),Bat2),Bat2\==[],premelement(Bat2,Batprem),reste(Bat2,Reste),substituer(Batprem,'X',[1,2,3,4,5,6,7,8,9,10],Res),construire_map_J1(Res,Reste,1,L).
construire_map_J1(Res,Bat,_,L):-Bat\==[],premelement(Bat,Batprem),reste(Bat,Reste),substituer(Batprem,'X',Res,Res2),construire_map_J1(Res2,Reste,1,L).
construire_map_J1(_,Bat,T,_):-T==0,Bat==[],write(['~','~','~','~','~','~','~','~','~','~']),nl.


%afficher(L):-L<10,construire_map_J1(_,[],0,L),L2 is L+1,afficher(L2).
afficher_bateauJ1:-construire_map_J1(_,[],0,1),construire_map_J1(_,[],0,2),construire_map_J1(_,[],0,3),construire_map_J1(_,[],0,4),construire_map_J1(_,[],0,5),construire_map_J1(_,[],0,6),construire_map_J1(_,[],0,7),construire_map_J1(_,[],0,8),construire_map_J1(_,[],0,9),construire_map_J1(_,[],0,10).

%%%%%%%% Affichage map joueur2

afficher_bateauJ2:-construire_map_J2(_,[],0,1),construire_map_J2(_,[],0,2),construire_map_J2(_,[],0,3),construire_map_J2(_,[],0,4),construire_map_J2(_,[],0,5),construire_map_J2(_,[],0,6),construire_map_J2(_,[],0,7),construire_map_J2(_,[],0,8),construire_map_J2(_,[],0,9),construire_map_J2(_,[],0,10).

construire_map_J2(Res,Bat,T,_):-T==1,Bat==[],substituer_non_Bat(1,Res).
construire_map_J2(Res,_,T,L):-T==0,findall(Y,bateau_joueur2(_,L,Y),Bat2),Bat2\==[],premelement(Bat2,Batprem),reste(Bat2,Reste),substituer(Batprem,'X',[1,2,3,4,5,6,7,8,9,10],Res),construire_map_J2(Res,Reste,1,L).
construire_map_J2(Res,Bat,_,L):-Bat\==[],premelement(Bat,Batprem),reste(Bat,Reste),substituer(Batprem,'X',Res,Res2),construire_map_J2(Res2,Reste,1,L).
construire_map_J2(_,Bat,T,_):-T==0,Bat==[],write(['~','~','~','~','~','~','~','~','~','~']),nl.

%%%%%%%% Affichage map tir et touché J1

construire_tire(Res,Pt,T,_):-T==1,Pt==[],substituer_non_Bat(1,Res).

construire_tire(_,_,T,L):-T==0,findall(Y,coups_tires_joueur1(L,Y),Pt_tire),premelement(Pt_tire,Pt_prem),reste(Pt_tire,_),not(bateau_joueur2(_,L,Pt_prem)),substituer(Pt_prem,'O',[1,2,3,4,5,6,7,8,9,10],Res_sub),construire_tire(Res_sub,Pt_tire,1,L).

construire_tire(_,_,T,L):-T==0,findall(Y,coups_tires_joueur1(L,Y),Pt_tire),premelement(Pt_tire,Pt_prem),reste(Pt_tire,Reste),bateau_joueur2(_,L,Pt_prem),substituer(Pt_prem,'T',[1,2,3,4,5,6,7,8,9,10],Res_sub),construire_tire(Res_sub,Reste,1,L).

construire_tire(Res,Pt,T,L):-T==1,premelement(Pt,Pt_prem),reste(Pt,Reste),not(bateau_joueur2(_,L,Pt_prem)),substituer(Pt_prem,'O',Res,Res_sub),construire_tire(Res_sub,Reste,1,L).
construire_tire(Res,Pt,T,L):-T==1,premelement(Pt,Pt_prem),reste(Pt,Reste),bateau_joueur2(_,L,Pt_prem),substituer(Pt_prem,'O',Res,Res_sub),construire_tire(Res_sub,Reste,1,L).
construire_tire(_,Pt,T,_):-T==0,Pt==[],write(['~','~','~','~','~','~','~','~','~','~']),nl.

afficher_tire:-write([1,2,3,4,5,6,7,8,9,10]),nl,construire_tire(_,[],0,1),construire_tire(_,[],0,2),construire_tire(_,[],0,3),construire_tire(_,[],0,4),construire_tire(_,[],0,5),construire_tire(_,[],0,6),construire_tire(_,[],0,7),construire_tire(_,[],0,8),construire_tire(_,[],0,9),construire_tire(_,[],0,10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Autres : %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fonction présente dans la version 6 de SWI-Prolog, mais pas dans la version 5

random_between(L, U, R) :-
integer(L), integer(U), !,
U >= L,
R is L+random((U+1)-L).
random_between(L, U, _) :-
must_be(integer, L),
must_be(integer, U).
