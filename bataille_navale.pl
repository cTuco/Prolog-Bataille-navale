% matrice 10 * 10 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Humain%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bateaux joueur 1 (id, ligne, colonne) : humain

% bateau 1 : 2 cases
bateau_joueur1(0, 4, 5).
bateau_joueur1(0, 4, 6).

% bateau 2 : 3 cases
bateau_joueur1(1, 3, 4).
bateau_joueur1(1, 3, 5).
bateau_joueur1(1, 3, 6).

% bateau 3 : 3 cases
bateau_joueur1(2, 2, 7).
bateau_joueur1(2, 3, 7).
bateau_joueur1(2, 4, 7).

% bateau 4 : 4 cases
bateau_joueur1(3, 5, 4).
bateau_joueur1(3, 5, 5).
bateau_joueur1(3, 5, 6).
bateau_joueur1(3, 5, 7).

% bateau 5 : 5 cases
bateau_joueur1(4, 3, 8).
bateau_joueur1(4, 4, 8).
bateau_joueur1(4, 5, 8).
bateau_joueur1(4, 6, 8).
bateau_joueur1(4, 7, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%choix du placement des bateaux par l utilisateur

% bateaux joueur 1 (id, ligne, colonne) : humain
%:- dynamic(bateau_joueur1/3).

positionner_tous:-positionner_t2,positionner_t3,positionner_t3Bis,positionner_t4,positionner_t5.

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
positionner_bateauH(T,Id):-retract(bateau_joueur1(Id,X,Y)),positionner_bateauH(T,Id).

positionner_pt_bateauH(Id,X,Y,T):-T<1.
positionner_pt_bateauH(Id,X,Y,T):-Ybis is Y+1,T2 is T-1, Ybis<11,assert(bateau_joueur1(Id,X,Ybis)),positionner_pt_bateauH(Id,X,Ybis,T2).
positionner_pt_bateauH(Id,X,Y,T):-write('Placement hors du terrain\n'),false.


%%%%% Positonnement vertical d un bateau
positionner_bateauV(T,Id):-write('Sur quelle colonne voulez vous placer le bateau ?\n'),read(C),write('Sur quelle ligne?\n'),read(L),Lbis is L + -1,positionner_pt_bateauV(Id,Lbis,C,T).
positionner_bateauV(T,Id):-retract(bateau_joueur1(Id,X,Y)),positionner_bateauV(T,Id).

positionner_pt_bateauV(Id,X,Y,T):-T<1.
positionner_pt_bateauV(Id,X,Y,T):-Xbis is X+1,T2 is T-1, Xbis<11,not(bateau_joueur1(_,Xbis,Y)),assert(bateau_joueur1(Id,Xbis,Y)),positionner_pt_bateauV(Id,Xbis,Y,T2).
positionner_pt_bateauV(Id,X,Y,T):-Xbis is X+1,not(Xbis<11),write('Placement hors du terrain\n'),false.
positionner_pt_bateauV(Id,X,Y,T):-Xbis is X+1,bateau_joueur1(_,Xbis,Y),write('Il y a deja un bateau a cet endroit\n'),false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mise en mémoire des coups tirés
:- dynamic(coups_tires_joueur1/2).

tirer_joueur1(X, _) :- X > 10, write('Tir invalide en X').
tirer_joueur1(_, Y) :- Y > 10, write('Tir invalide en Y').
tirer_joueur1(X, Y) :- coups_tires_joueur1(X, Y), write('Coup deja joue !').
tirer_joueur1(X, Y) :- X < 11, Y < 11, not(coups_tires_joueur1(X, Y)), assert(coups_tires_joueur1(X, Y)), 
bateau_joueur2(Id, X, Y), write('Touche !'), couler_joueur1(Id), write('\nCoule !').

% on récupère tous les points du bateau grâce à son id et on vérifie si ils sont tous dans la liste des coups_tires
couler_joueur1(Id) :- forall(bateau_joueur2(Id, X, Y), coups_tires_joueur1(X, Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Ordi%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bateaux joueur 2 (id, l, c) : ordi 

% bateau 1 : 2 cases
bateau_joueur2(0, 1, 3).
bateau_joueur2(0, 2, 3).

% bateau 2 : 3 cases
bateau_joueur2(1, 4, 5).
bateau_joueur2(1, 5, 5).
bateau_joueur2(1, 6, 5).

% bateau 3 : 3 cases
bateau_joueur2(2, 6, 1).
bateau_joueur2(2, 7, 1).
bateau_joueur2(2, 8, 1).

% bateau 4 : 4 cases
bateau_joueur2(3, 2, 7).
bateau_joueur2(3, 2, 8).
bateau_joueur2(3, 2, 9).
bateau_joueur2(3, 2, 10).

% bateau 5 : 5 cases
bateau_joueur2(4, 10, 6).
bateau_joueur2(4, 10, 7).
bateau_joueur2(4, 10, 8).
bateau_joueur2(4, 10, 9).
bateau_joueur2(4, 10, 10).

% mise en mémoire des coups tirés et des bateaux touchés 
:- dynamic(coups_tires_joueur2/2).
:- dynamic(bateaux_touches_joueur2/3).

% on stocke tous les prédicats "bateaux_touches_joueur2" dans une liste
tirer_joueur2 :- findall([X, Y], bateaux_touches_joueur2(_, X, Y), R), tirer_joueur2(R).

% si la liste est vide on tire au hasard 
tirer_joueur2([]) :- random(1, 11, X), random(1, 11, Y), tirer_joueur2(X, Y).

% autrement on applique une stratégie
tirer_joueur2([T|Q]) :- strategie_tir([T|Q]).

tirer_joueur2(X, Y) :- coups_tires_joueur2(X, Y), tirer_joueur2.

tirer_joueur2(X, Y) :- not(coups_tires_joueur2(X, Y)), assert(coups_tires_joueur2(X, Y)), 
write('Joueur 2 : tir en '), write(X), write(' * '), write(Y), 
bateau_joueur1(Id, X, Y), assert(bateaux_touches_joueur2(Id, X, Y)), write('\nTouche !'), 
couler_joueur2(Id), write('\nCoule !'), retractall(bateaux_touches_joueur2(Id, _, _)).

% on récupère tous les points du bateau grâce à son id et on vérifie si ils sont tous dans la liste des coups_tires
couler_joueur2(Id) :- forall(bateau_joueur1(Id, X, Y), coups_tires_joueur2(X, Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% construit la liste des points adjacents à X et Y sous la forme [[X+1, Y], [X-1, Y], [X, Y+1], [X-1, Y-1]]
adjacent(X, Y, [[XPlus, Y], [XMinus, Y], [X, YPlus], [X, YMinus]]) :- XMinus is X - 1, XPlus is X + 1, YMinus is Y - 1, YPlus is Y + 1.

% lorsqu'il n'y a qu'un seul élément touché, on prend les points adjacents
% on supprime les points qui se situent hors du terrain et les coups déjà tirés
strategie_tir([[X, Y]]) :- adjacent(X, Y, LAdj), 
findall([XTires, YTires], coups_tires_joueur2(XTires, YTires), LCoupsTires), 
subtract(LAdj, [[0, _], [_, 0], [11, _], [_, 11]|LCoupsTires], [[XFinal, YFinal]|_]),
tirer_joueur2(XFinal, YFinal).

% lorsqu'il y en a plusieurs, on prend le premier et on cherche un alignement par rapport à ce point
strategie_tir([[X, Y], [_, _]|_]) :- findall(XDiff, (bateaux_touches_joueur2(_, XDiff, Y), XDiff\==X), XTouches), findall(YDiff, (bateaux_touches_joueur2(_, X, YDiff), YDiff\==Y), YTouches),
evaluer_alignement([X, Y], XTouches, YTouches).

%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluer_alignement([X, Y], [], []) :- strategie_tir([[X, Y]]).

% alignement vertical : on essaye de tirer au delà des extrémités verticales
evaluer_alignement([X, Y], [T1|Q1], []) :- max_list([X|[T1|Q1]], XMax), min_list([X|[T1|Q1]], XMin), XSup is XMax + 1, XInf is XMin - 1,
findall(X2, coups_tires_joueur2(X2, Y), XTires),
subtract([XSup,XInf], [0, 11 | XTires], XTrouves),
evaluer_tir([X, Y], XTrouves, []).

% alignement horizontal : on essaye de tirer au delà des extrémités horizontales
evaluer_alignement([X, Y], [], [T2|Q2]) :- max_list([Y|[T2|Q2]], YMax), min_list([Y|[T2|Q2]], YMin), YSup is YMax + 1, YInf is YMin - 1,
findall(Y2, coups_tires_joueur2(X, Y2), YTires),
subtract([YSup,YInf], [0, 11 | YTires], YTrouves),
evaluer_tir([X, Y], [], YTrouves).

% cas particulier : double alignement
evaluer_alignement([X, Y], [T1|Q1], [T2|Q2]) :- max_list([X|[T1|Q1]], XMax), min_list([X|[T1|Q1]], XMin), XSup is XMax + 1, XInf is XMin - 1,
findall(X2, coups_tires_joueur2(X2, Y), XTires),
subtract([XSup,XInf], [0, 11 | XTires], XTrouves),
max_list([Y|[T2|Q2]], YMax), min_list([Y|[T2|Q2]], YMin), YSup is YMax + 1, YInf is YMin - 1,
findall(Y2, coups_tires_joueur2(X, Y2), YTires),
subtract([YSup,YInf], [0, 11 | YTires], YTrouves),
evaluer_tir([X, Y], XTrouves, YTrouves).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% évaluation des tirs

evaluer_tir([X, Y], [], []) :- strategie_tir([[X, Y]]).

evaluer_tir([_, Y], [XChoisi|_], []) :- tirer_joueur2(XChoisi, Y).

evaluer_tir([X, _], [], [YChoisi|_]) :- tirer_joueur2(X, YChoisi).

evaluer_tir([_, Y], [XChoisi|_], [_|_]) :- tirer_joueur2(XChoisi, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%

% partie terminée

partie_terminee :- findall([X, Y], coups_tires_joueur1(X, Y), TirsJ1), findall([X1, Y1], bateau_joueur2(_, X1, Y1), BateauxJ2), subtract(BateauxJ2, TirsJ1, ResteJ1),
findall([X2, Y2], coups_tires_joueur2(X2, Y2), TirsJ2), findall([X3, Y3], bateau_joueur1(_, X3, Y3), BateauxJ1), subtract(BateauxJ1, TirsJ2, ResteJ2), 
partie_terminee(ResteJ1, ResteJ2).

partie_terminee([], [_|_]) :- write('Partie terminee\nLe joueur 1 a gagne\n').
partie_terminee([_|_], []) :- write('Partie terminee\nLe joueur 2 a gagne\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_between(L, U, R) :-
integer(L), integer(U), !,
U >= L,
R is L+random((U+1)-L).
random_between(L, U, _) :-
must_be(integer, L),
must_be(integer, U).
