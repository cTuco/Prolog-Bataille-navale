% matrice 10 * 10 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Humain%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bateaux joueur 1 (id, ligne, colonne) : humain

/*% bateau 1 : 2 cases
bateau_joueur1(0, 5, 5).
bateau_joueur1(0, 5, 6).

% bateau 2 : 3 cases
bateau_joueur1(1, 7, 4).
bateau_joueur1(1, 7, 5).
bateau_joueur1(1, 7, 6).

% bateau 3 : 3 cases
bateau_joueur1(2, 8, 10).
bateau_joueur1(2, 9, 10).
bateau_joueur1(2, 10, 10).

% bateau 4 : 4 cases
bateau_joueur1(3, 2, 2).
bateau_joueur1(3, 3, 2).
bateau_joueur1(3, 4, 2).
bateau_joueur1(3, 5, 2).

% bateau 5 : 5 cases
bateau_joueur1(4, 6, 4).
bateau_joueur1(4, 6, 5).
bateau_joueur1(4, 6, 6).
bateau_joueur1(4, 6, 7).
bateau_joueur1(4, 6, 8).*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%choix du placement des bateaux par l utilisateur

% bateaux joueur 1 (id, ligne, colonne) : humain
:- dynamic(bateau_joueur1/3).

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
% mise en m�moire des coups tir�s
:- dynamic(coups_tires_joueur1/2).

tirer_joueur1(X, _) :- X > 10, write('Tir invalide en X').
tirer_joueur1(_, Y) :- Y > 10, write('Tir invalide en Y').
tirer_joueur1(X, Y) :- coups_tires_joueur1(X, Y), write('Coup d�j� jou� !').
tirer_joueur1(X, Y) :- X < 11, Y < 11, not(coups_tires_joueur1(X, Y)), assert(coups_tires_joueur1(X, Y)), 
bateau_joueur2(Id, X, Y), write('Touch� !'), couler_joueur1(Id), write('\nCoul� !').

% on r�cup�re tous les points du bateau gr�ce � son id et on v�rifie si ils sont tous dans la liste des coups_tires
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

% mise en m�moire des coups tir�s et des bateaux touch�s 
:- dynamic(coups_tires_joueur2/2).
:- dynamic(bateaux_touches_joueur2/3).

% on stocke tous les pr�dicats "bateaux_touches_joueur2" dans une liste
tirer_joueur2 :- findall([X, Y], bateaux_touches_joueur2(_, X, Y), R), tirer_joueur2(R).

% si la liste est vide on tire au hasard 
tirer_joueur2([]) :- random(1, 11, X), random(1, 11, Y), tirer_joueur2(X, Y).

% autrement on applique une strat�gie
tirer_joueur2([T|Q]) :- strategie_tir([T|Q]).

tirer_joueur2(X, Y) :- coups_tires_joueur2(X, Y), tirer_joueur2.

tirer_joueur2(X, Y) :- not(coups_tires_joueur2(X, Y)), assert(coups_tires_joueur2(X, Y)), 
write('Joueur 2 : tir en '), write(X), write(' * '), write(Y), 
bateau_joueur1(Id, X, Y), assert(bateaux_touches_joueur2(Id, X, Y)), write('\nTouch� !'), 
couler_joueur2(Id), write('\nCoul� !'), retractall(bateaux_touches_joueur2(Id, _, _)).

% on r�cup�re tous les points du bateau gr�ce � son id et on v�rifie si ils sont tous dans la liste des coups_tires
couler_joueur2(Id) :- forall(bateau_joueur1(Id, X, Y), coups_tires_joueur2(X, Y)).

% construit la liste des points adjacents � X et Y sous la forme [[X+1, Y], [X-1, Y], [X, Y+1], [X-1, Y-1]]
adjacent(X, Y, [[XPlus, Y], [XMinus, Y], [X, YPlus], [X, YMinus]]) :- XMinus is X - 1, XPlus is X + 1, YMinus is Y - 1, YPlus is Y + 1.

% lorsqu'il n'y a qu'un seul �l�ment touch�, on prend les points adjacents
% on supprime les points qui se situent hors du terrain et les coups d�j� tir�s
strategie_tir([[X, Y]]) :- adjacent(X, Y, LAdj), 
findall([XTires, YTires], coups_tires_joueur2(XTires, YTires), LCoupsTires), 
subtract(LAdj, [[0, _], [_, 0], [11, _], [_, 11]|LCoupsTires], [[XFinal, YFinal]|_]),
tirer_joueur2(XFinal, YFinal).

% lorsqu'il y en a plusieurs, on prend le premier et on cherche un alignement par rapport � ce point
strategie_tir([[X, Y]|_]) :- findall(XDiff, (bateaux_touches_joueur2(_, XDiff, Y), XDiff\==X), XTouches), findall(YDiff, (bateaux_touches_joueur2(_, X, YDiff), YDiff\==Y), YTouches),
evaluer_alignement([X, Y], XTouches, YTouches).

% alignement vertical : on essaye de tirer au del� des extr�mit�s verticales
evaluer_alignement([X, Y], XTouches, []) :- max_list([X|XTouches], XMax), min_list([X|XTouches], XMin), XSup is XMax + 1, XInf is XMin - 1,
findall(X2, coups_tires_joueur2(X2, Y), XTires),
subtract([XSup,XInf], [0, 11 | XTires], XTrouves),
evaluer_tir([X, Y], XTrouves, []).

% alignement horizontal : on essaye de tirer au del� des extr�mit�s horizontales
evaluer_alignement([X, Y], [], YTouches) :- max_list([Y|YTouches], YMax), min_list([Y|YTouches], YMin), YSup is YMax + 1, YInf is YMin - 1,
findall(Y2, coups_tires_joueur2(X, Y2), YTires),
subtract([YSup,YInf], [0, 11 | YTires], YTrouves),
evaluer_tir([X, Y], [], YTrouves).

% cas particulier : double alignement
evaluer_alignement(Point, [_|_], [_|_]) :- write('both : TODO').

% pas de tirs possibles aux extr�mit�s des alignements, on applique la 1�re strat�gie
evaluer_tir([X, Y], [], []) :- strategie([[X, Y]]).

evaluer_tir([_, Y], [XChoisi|_], []) :- tirer_joueur2(XChoisi, Y).

evaluer_tir([X, _], [], [YChoisi|_]) :- tirer_joueur2(X, YChoisi).

evaluer_tir([_, Y], [XChoisi|_], [_|_]) :- tirer_joueur2(XChoisi, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_between(L, U, R) :-
integer(L), integer(U), !,
U >= L,
R is L+random((U+1)-L).
random_between(L, U, _) :-
must_be(integer, L),
must_be(integer, U).
