% Islands is a list of islands on the Board that form a connected
% set starting from a certain island.
% Visited is used to prevent visiting islands multiple times.
% This predicate is called recursively for each islands that
% has not been visited yet
% Take it's neighbors and check if they are visited or not.
fill_set_visit(Board, X, Y, Islands, Visited) :-
    island_neighbors(Board, X,Y, Neighbors),
    length(Neighbors, N),

    ( for(I,1,N), param(Board, Islands, Visited, Neighbors) do
        nth1(I, Neighbors, [X1,Y1, _, _]),
        nth1(Pos, Islands, [X1,Y1]),
        nth1(Pos, Visited, HasVisited),

        % If it is still a var, we haven't visited this islands
        (var(HasVisited) ->
            HasVisited is 1,
            fill_set_visit(Board, X1, Y1, Islands, Visited)
        ;
            true
        )
    ),
    !.
