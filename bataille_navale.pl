% matrice 10 * 10 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% bateaux joueur 1 (id, ligne, colonne) : humain

% bateau 1 : 2 cases
bateau_joueur1(0, 5, 4).
bateau_joueur1(0, 5, 5).

% bateau 2 : 3 cases
bateau_joueur1(1, 7, 5).
bateau_joueur1(1, 7, 6).
bateau_joueur1(1, 7, 7).

% bateau 3 : 3 cases
bateau_joueur1(2, 7, 4).
bateau_joueur1(2, 8, 4).
bateau_joueur1(2, 9, 4).

% bateau 4 : 4 cases
bateau_joueur1(3, 3, 3).
bateau_joueur1(3, 4, 3).
bateau_joueur1(3, 5, 3).
bateau_joueur1(3, 6, 3).

% bateau 5 : 5 cases
bateau_joueur1(4, 6, 4).
bateau_joueur1(4, 6, 5).
bateau_joueur1(4, 6, 6).
bateau_joueur1(4, 6, 7).
bateau_joueur1(4, 6, 8).

% mise en mémoire des coups tirés
:- dynamic(coups_tires_joueur1/2).

tirer_joueur1(X, _) :- X > 10, write('Tir invalide en X').
tirer_joueur1(X, _) :- X < 1, write('Tir invalide en X').
tirer_joueur1(_, Y) :- Y > 10, write('Tir invalide en Y').
tirer_joueur1(_, Y) :- Y < 1, write('Tir invalide en Y').
tirer_joueur1(X, Y) :- coups_tires_joueur1(X, Y), write('Coup déjà joué !').
tirer_joueur1(X, Y) :- not(coups_tires_joueur1(X, Y)), assert(coups_tires_joueur1(X, Y)), 
bateau_joueur2(Id, X, Y), write('Touché !'), 
couler_joueur1(Id), write('\nCoulé !').

% on récupère tous les points du bateau grâce à son id et on vérifie si ils sont tous dans la liste des coups_tires
couler_joueur1(Id) :- forall(bateau_joueur2(Id, X, Y), coups_tires_joueur1(X, Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
bateau_joueur1(Id, X, Y), assert(bateaux_touches_joueur2(Id, X, Y)), write('\nTouché !'), 
couler_joueur2(Id), write('\nCoulé !'), retractall(bateaux_touches_joueur2(Id, _, _)).

% on récupère tous les points du bateau grâce à son id et on vérifie si ils sont tous dans la liste des coups_tires
couler_joueur2(Id) :- forall(bateau_joueur1(Id, X, Y), coups_tires_joueur2(X, Y)).

% construit la liste des points adjacents à X et Y sous la forme [[X+1, Y], [X-1, Y], [X, Y+1], [X-1, Y-1]]
adjacent(X, Y, [[XPlus, Y], [XMinus, Y], [X, YPlus], [X, YMinus]]) :- XMinus is X - 1, XPlus is X + 1, YMinus is Y - 1, YPlus is Y + 1.

% lorsqu'il n'y a qu'un seul élément touché, on prend les points adjacents
% on supprime les points qui se situent hors du terrain et les coups déjà tirés
strategie_tir([[X, Y]]) :- adjacent(X, Y, LAdj), 
findall([XTires, YTires], coups_tires_joueur2(XTires, YTires), LCoupsTires), 
subtract(LAdj, [[0, _], [_, 0], [11, _], [_, 11]|LCoupsTires], [[XFinal, YFinal]|_]),
tirer_joueur2(XFinal, YFinal).

% lorsqu'il y en a plusieurs, on prend le premier et on cherche un alignement par rapport à ce point
strategie_tir([[X, Y]|_]) :- findall(XDiff, (bateaux_touches_joueur2(_, XDiff, Y), XDiff\==X), XTouches), findall(YDiff, (bateaux_touches_joueur2(_, X, YDiff), YDiff\==Y), YTouches),
evaluer_alignement([X, Y], XTouches, YTouches).

% alignement vertical : on essaye de tirer au delà des extrémités verticales
evaluer_alignement([X, Y], XTouches, []) :- max_list([X|XTouches], XMax), min_list([X|XTouches], XMin), XSup is XMax + 1, XInf is XMin - 1,
findall(X2, coups_tires_joueur2(X2, Y), XTires),
subtract([XSup,XInf], [0, 11 | XTires], XTrouves),
evaluer_tir([X, Y], XTrouves, []).

% alignement horizontal : on essaye de tirer au delà des extrémités horizontales
evaluer_alignement([X, Y], [], YTouches) :- max_list([Y|YTouches], YMax), min_list([Y|YTouches], YMin), YSup is YMax + 1, YInf is YMin - 1,
findall(Y2, coups_tires_joueur2(X, Y2), YTires),
subtract([YSup,YInf], [0, 11 | YTires], YTrouves),
evaluer_tir([X, Y], [], YTrouves).

% cas particulier : double alignement
evaluer_alignement(Point, [_|_], [_|_]) :- write('both : TODO').

% pas de tirs possibles aux extrémités des alignements, on applique la 1ère stratégie
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

