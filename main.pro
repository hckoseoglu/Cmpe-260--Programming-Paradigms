% name surname
% id
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.

% My custom predicates

% Predicate for finding the length of a list
% Base case: an empty list has length 0
list_length([], 0).

% Recursive case: the length of a list is 1 plus the length of its tail
list_length([_|Tail], Length) :-
    list_length(Tail, TailLength),
    Length is TailLength + 1.

% Function for concatting two lists
% Base case: if the first list is empty, the result is the second list
concat_lists([], List, List).
% Recursive case: concatenate the head of the first list with the result of concatenating the tail of the first list with the second list
concat_lists([Head|Tail1], List2, [Head|Result]) :-
  concat_lists(Tail1, List2, Result).


% Predicate for finding the elements in agents dictionary
% Get the number of elements in an agent_dict
agent_dict_length(AgentDict, Length) :-
  dict_pairs(AgentDict, _, Pairs),
  list_length(Pairs, Length).


% Predicate for finding the second least element inside a list
second_min_list(List, SecondMin) :-
  sort(List, Sorted), nth0(1, Sorted, SecondMin).

%Predicate for finding the distance between an agent and object
agent_object_distance(Agent, Object, Distance) :-
  Distance is abs(Agent.x - Object.x) + abs(Agent.y - Object.y).

%Predicate for finding if there is any agent at given coordinates
agent_at_coordinates(AgentDict, X, Y, Agent) :-
  dict_pairs(AgentDict, _, AgentPairs),
  pairs_values(AgentPairs, AgentsValues),
  member(Agent, AgentsValues),
  Agent = agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y}.

%Predicate for checking if at the given location there is an agent
agent_at_coordinates(AgentDict, X, Y) :-
  agent_at_coordinates(AgentDict, X, Y, agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y}).

%Predicate for checking if the given coordinate does not contain any wolf
no_wolf_at_coordinates(AgentDict, X, Y) :-
  \+ agent_at_coordinates(AgentDict, X, Y, agents{children:_, energy_point:_, subtype:wolf, type:_, x:X, y:Y}).

%Predicate for finding the subtype of agent at given coordinates
agent_subtype_at_coordinates(State, X, Y, Subtype) :-
  State = [AgentDict, _, _, _],
  dict_pairs(AgentDict, _, AgentPairs),
  pairs_values(AgentPairs, AgentsValues),
  member(Agent, AgentsValues),
  Agent = agents{children:_, energy_point:_, subtype:Subtype, type:_, x:X, y:Y}.

% Project Predicates

% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance) :-
  Distance is abs(Agent1.x - Agent2.x) + abs(Agent1.y - Agent2.y).

% 2- number_of_agents(+State, -NumberOfAgents)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], number_of_agents(State, NumberOfAgents).

number_of_agents(State, NumberOfAgents) :- 
  State = [AgentsDict, _, _, _],
  dict_keys(AgentsDict, Ids),   
  list_length(Ids, NumberOfAgents).

% 3- value_of_farm(+State, -Value)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], value_of_farm(State, Value).

value_of_farm(State, Value) :-
  State = [AgentsDict, ObjectsDict, _, _],
  agent_dict_values(AgentsDict, AgentValues),
  object_dict_values(ObjectsDict, ObjectValues),
  sum_list(AgentValues, AgentValueSum),
  sum_list(ObjectValues, ObjectValueSum),
  Value is AgentValueSum + ObjectValueSum.
% Helper predicates

agent_dict_values(AgentDict, Values) :-
  dict_pairs(AgentDict, _, AgentsPairs),
  pairs_values(AgentsPairs, AgentsValues),
   maplist(calculate_agent_value, AgentsValues, Values).

object_dict_values(ObjectsDict, Values) :-
  dict_pairs(ObjectsDict, _, ObjectPairs),
  pairs_values(ObjectPairs, ObjectsValues),
  maplist(calculate_object_value, ObjectsValues, Values).

calculate_agent_value(Agent, Value) :-
  Agent = agents{children:_, energy_point:_, subtype:Subtype, type:_, x:_, y:_},
  (Subtype = wolf -> Value = 0; value(Subtype, Value)).

calculate_object_value(Object, Value) :-
    Object = object{subtype:Subtype, type:_, x:_, y:_},
    value(Subtype, Value).


% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder],find_food_coordinates(State, 1, Coordinates).
find_food_coordinates(State, AgentId, Coordinates) :-
  State = [AgentsDict, ObjectsDict, _, _],
  % Get agent with the given agent id
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:_, energy_point:_, subtype:AgentType, type:_, x:_, y:_},
  dict_pairs(ObjectsDict, _, ObjectPairs),
  pairs_values(ObjectPairs, ObjectsValues),
  dict_pairs(AgentsDict, _, AgentsPairs),
  pairs_values(AgentsPairs, AgentsValues),
  findall([X, Y], (member(Object, ObjectsValues), Object = object{subtype:Subtype, type:_, x:X, y:Y}, can_eat(AgentType, Subtype)), ObjectCoordinates),
  findall([X, Y], (member(AgentToEat, AgentsValues), AgentToEat = agents{children:_, energy_point:_, subtype:EatenType, type:_, x:X, y:Y}, can_eat(AgentType, EatenType)), AgentCoordinates),
  concat_lists(ObjectCoordinates, AgentCoordinates, Coordinates),
  Coordinates \= [].


% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder],find_nearest_agent(State, 1, Coordinates, NearestAgent).
find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-
  State = [AgentsDict, _, _, _],
  get_dict(AgentId, AgentsDict, Agent),
  
  dict_pairs(AgentsDict, _, AgentsPairs),
  pairs_values(AgentsPairs, AgentsValues),
  findall([X, Y], (member(AgentToControl, AgentsValues), AgentToControl = agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y}), AgentCoordinates),
  findall(Distance, (member([X, Y], AgentCoordinates), agents_distance(Agent, agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y}, Distance)), Distances),
  second_min_list(Distances, MinDistance),
  nth0(Index, Distances, MinDistance),
  nth0(Index, AgentCoordinates, Coordinates),
  nth0(Index, AgentsValues, NearestAgent).

% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder],find_nearest_food(State, 1, Coordinates, FoodType, Distance).
find_nearest_food(State, AgentId, Coordinates, FoodType, Distance) :-
  State = [AgentsDict, ObjectsDict, _, _],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:_, energy_point:_, subtype:AgentType, type:_, x:_, y:_},
  dict_pairs(ObjectsDict, _, ObjectPairs),
  pairs_values(ObjectPairs, ObjectsValues),
  dict_pairs(AgentsDict, _, AgentPairs),
  pairs_values(AgentPairs, AgentsValues),
  findall([X, Y, Subtype], (member(Object, ObjectsValues), Object = object{subtype:Subtype, type:_, x:X, y:Y}, can_eat(AgentType, Subtype)), ObjectEatableCoordinates),
  findall([X, Y, Subtype], (member(AgentToEat, AgentsValues), AgentToEat = agents{subtype:Subtype, energy_point:_, children:_, type:_, x:X, y:Y}, can_eat(AgentType, Subtype)), AgentEatableCoordinates),
  concat_lists(ObjectEatableCoordinates, AgentEatableCoordinates, AllCoordinates),
  AllCoordinates \= [],
  % Even though in the list there are coordinates for object, since we calculate distance it doesnt really matter
  findall(Distance, (member([X, Y, _], AllCoordinates), agents_distance(Agent, agents{subtype:_, type:_, energy_point:_, children:_,
     x:X, y:Y}, Distance)), Distances),
  min_list(Distances, MinDistance),
  nth0(Index, Distances, MinDistance),
  nth0(Index, AllCoordinates, [FoundX, FoundY, FoodType]),
  Coordinates = [FoundX, FoundY].

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], move_to_coordinate(State, 0,1,1, CowActionList, 5).

% Predicate for determining if the given agent can make the given move
can_make_move(State, AgentId, Move) :-
  get_dict(AgentId, State, Agent),
  Agent = agents{children:_, energy_point:_, subtype:Subtype, type:_, x:X, y:Y},
  can_move(Subtype, Move),
  % Define new coordinates based on the move
  (Move = move_up -> NewX is X, NewY is Y - 1),
  (Move = move_down -> NewX is X, NewY is Y + 1),
  (Move = move_left -> NewX is X - 1, NewY is Y),
  (Move = move_right -> NewX is X + 1, NewY is Y),
  (Move = move_up_right -> NewX is X + 1, NewY is Y - 1),
  (Move = move_up_left -> NewX is X - 1, NewY is Y - 1),
  (Move = move_down_right -> NewX is X + 1, NewY is Y + 1),
  (Move = move_down_left -> NewX is X - 1, NewY is Y + 1),
  (agent_at_coordinates(State, NewX, NewY) -> agent_subtype_at_coordinates(State, NewX, NewY, SubtypeAtLocation), 
  can_eat(Subtype, SubtypeAtLocation)),
  % Check if the new coordinates are within the boundaries of the map
  width(Width), height(Height),
  NewX > 0, NewX < (Width - 1), NewY > 0, NewY < (Height - 1).

move_to_coordinates_helper(State, AgentId, X, Y, PrevMoves, PrevCoordinates, ActionList, DepthLimit) :-
  State = [AgentsDict, _, _, _],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:_, energy_point:_, subtype:Subtype, type:_, x:CurrentX, y:CurrentY},
  DistanceToDest is abs(CurrentX - X) + abs(CurrentY - Y),
  (DistanceToDest = 0 -> ActionList = []; findall(NextMove,
    (can_make_move(State, AgentId, NextMove),
     move(State, AgentId, NextMove, NewState),
     get_dict(AgentId, NewState = [_, _, _, _], NewAgent),
     NewX is NewAgent.x,
     NewY is NewAgent.y,
     NewDistance is abs(X - NewX) + abs(Y - NewY),
     DepthLimit > 0,
     move_to_coordinate_cow(NewState, AgentId, X, Y, RestOfActions, DepthLimit1),
     DepthLimit1 is DepthLimit - 1),
    PossibleMoves),

    % Check if any valid neighboring move leads closer to the target
    PossibleMoves \= [] ->
    nth0(Index, PossibleMoves, Move),
    append([Move], RestOfActions, ActionList);

    % No valid move found, return empty list
    ActionList = []).  

move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) :-
  move_to_coordinates_helper(State, AgentId, X, Y, [], [], ActionList, DepthLimit).



% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)

