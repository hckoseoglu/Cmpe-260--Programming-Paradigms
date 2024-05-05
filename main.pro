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

% Predicate for reversing a list
% Base case: the reverse of an empty list is an empty list
reverse_list([], []).
% Recursive case: the reverse of a list is the reverse of its tail concatenated with the head
reverse_list([Head|Tail], Reversed) :-
  reverse_list(Tail, ReversedTail),
  concat_lists(ReversedTail, [Head], Reversed).

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
agent_at_coordinates(AgentsDict, X, Y, Agent) :-
  dict_pairs(AgentsDict, _, AgentPairs),
  pairs_values(AgentPairs, AgentsValues),
  member(Agent, AgentsValues),
  Agent = agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y}.

%Predicate for checking if at the given location there is an agent
agent_at_coordinates(AgentsDict, X, Y) :-
  agent_at_coordinates(AgentsDict, X, Y, agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y}).


%Predicate for finding the subtype of agent at given coordinates
agent_subtype_at_coordinates(AgentDict, X, Y, Subtype) :-
  dict_pairs(AgentDict, _, AgentPairs),
  pairs_values(AgentPairs, AgentsValues),
  member(Agent, AgentsValues),
  Agent = agents{children:_, energy_point:_, subtype:Subtype, type:_, x:X, y:Y}.

% Predicate for updating state after one move
update_state_after_move(State, AgentId, Move, NewState) :-
  State = [AgentsDict, ObjectsDict, Time, TurnOrder],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:Children, energy_point:EnergyPoint, subtype:Subtype, type:Type, x:X, y:Y},
  new_coordinates_after_move(Move, X, Y, NewX, NewY),
  % Without checking the leaglity of move just create the new state by updating agents dictionary
  put_dict(AgentId, AgentsDict, agents{children:Children, energy_point:EnergyPoint, subtype:Subtype, type:Type, x:NewX, y:NewY}, NewAgentsDict),
  NewState = [NewAgentsDict, ObjectsDict, Time, TurnOrder].

% Custom predicate pairs_values that extracts values from a list of pairs
custom_pairs_values(Pairs, Values) :-
  custom_pairs_values(Pairs, [], Values).

custom_pairs_values([], Values, Values).

custom_pairs_values([HeadPair | TailPairs], AccumulatedValues, Values) :-
  HeadPair = _ - Value,
  append(AccumulatedValues, [Value], NewAccumulatedValues),
  custom_pairs_values(TailPairs, NewAccumulatedValues, Values).


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
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], move_to_coordinate(State, 1, 3, 4, ActionList, 4).

% Predicate for finding the new coordinates after each move
new_coordinates_after_move(move_down, X, Y, NewX, NewY) :- NewX is X, NewY is Y + 1.
new_coordinates_after_move(move_up, X, Y, NewX, NewY) :- NewX is X, NewY is Y - 1.
new_coordinates_after_move(move_left, X, Y, NewX, NewY) :- NewX is X - 1, NewY is Y.
new_coordinates_after_move(move_right, X, Y, NewX, NewY) :- NewX is X + 1, NewY is Y.
new_coordinates_after_move(move_up_right, X, Y, NewX, NewY) :- NewX is X + 1, NewY is Y - 1.
new_coordinates_after_move(move_up_left, X, Y, NewX, NewY) :- NewX is X - 1, NewY is Y - 1.
new_coordinates_after_move(move_down_right, X, Y, NewX, NewY) :- NewX is X + 1, NewY is Y + 1.
new_coordinates_after_move(move_down_left, X, Y, NewX, NewY) :- NewX is X - 1, NewY is Y + 1.


% Predicate for determining if the given agent can make the given move
can_make_move(State, X, Y, AgentId, Move, NewX, NewY) :-
  State = [AgentsDict, _, _, _],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:_, energy_point:_, subtype:Subtype, type:_, x:_, y:_},
  can_move(Subtype, Move),
  % Define new coordinates based on the move
  new_coordinates_after_move(Move, X, Y, NewX, NewY),
  % Check if the new coordinates are within the boundaries of the map
  width(Width), height(Height),
  NewX > 0, NewX < (Width - 1), NewY > 0, NewY < (Height - 1),
  (agent_at_coordinates(AgentsDict, NewX, NewY) -> 
  agent_subtype_at_coordinates(AgentsDict, NewX, NewY, SubtypeAtLocation), 
  can_eat(Subtype, SubtypeAtLocation);
  true).

isValidMapBoundaries(X, Y) :-
  width(Width), height(Height),
  X > 0, X < (Width - 1), Y > 0, Y < (Height - 1).


move_to_coordinate_helper(State, AgentId, [[X, Y, Actions] | RestQueue], TargetX, TargetY, DepthLimit, FinalActionList) :-
  % Check if the target is reached
  (X = TargetX, Y = TargetY -> FinalActionList = Actions;
  % Explore neighboring nodes
  findall([NewX, NewY, [Move | Actions]],
          (
            list_length(Actions, ActionsLength),
            DepthLimit > ActionsLength,
            can_make_move(State, X, Y, AgentId, Move, NewX, NewY),
            \+ member([NewX, NewY, _], RestQueue)
          ),
          PossibleMoves),

  % Append possible next nodes to the queue
  append(RestQueue, PossibleMoves, NewQueue),

  % Continue BFS with the updated queue
  move_to_coordinate_helper(State, AgentId, NewQueue, TargetX, TargetY, DepthLimit, FinalActionList)).

move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) :-
  State = [AgentsDict, _, _, _],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:_, energy_point:_, subtype:_, type:_, x:CurrentX, y:CurrentY},

  % Calculate the Manhattan distance between current and target coordinates
  DistanceToDest is abs(X - CurrentX) + abs(Y - CurrentY),

  % Base case: reached the target coordinates, return empty list
  (DistanceToDest = 0 -> ActionList = [];

  % Initialize BFS queue with the starting node
  Queue = [[CurrentX, CurrentY, []]],

  % BFS loop
  move_to_coordinate_helper(State, AgentId, Queue, X, Y, DepthLimit, ReverseActionList)),
  reverse_list(ReverseActionList, ActionList).


% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], move_to_nearest_food(State, 11, ActionList, 20).
move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :-
  find_nearest_food(State, AgentId, Coordinates, _, _),
  Coordinates = [X, Y],
  move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit).

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)

% Predicate for retrieving the number of children of an agent
number_of_children(AgentId, State, NumberOfChildren) :-
  State = [AgentsDict, _, _, _],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:NumberOfChildren, energy_point:_, subtype:_, type:_, x:_, y:_}.


% Predicate for updating the location of agent with agent id and updating state
change_location_of_agent(State, AgentId, X, Y, NewState) :-
  State = [AgentsDict, ObjectsDict, Time, TurnOrder],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:Children, energy_point:EnergyPoint, subtype:AgentSubtype, type:Type, x:_, y:_},
  put_dict(AgentId, AgentsDict, agents{children:Children, energy_point:EnergyPoint, subtype:AgentSubtype, type:Type, x:X, y:Y}, NewAgentsDict),
  NewState = [NewAgentsDict, ObjectsDict, Time, TurnOrder].


% Consume single food at given location and update the state afterwards
% First detect if given food at location is object or agent
% Than delete the object or agent from the state

consume_food_at_location(State, AgentId, X, Y, NewState) :-
  State = [AgentsDict, ObjectsDict, Time, TurnOrder],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:Children, energy_point:EnergyPoint, subtype:AgentSubtype, type:Type, x:AgentX, y:AgentY},
  % Check if there is an object at the given location
  (get_dict(EatenObjectId, ObjectsDict, Object), Object = object{subtype:Subtype, type:_, x:X, y:Y} -> 
  (
    % Update the state by deleting the object
    del_dict(EatenObjectId, ObjectsDict, _, NewObjectsDict),
    % Increase the energy point of agent
    energy_point(Subtype, NewEnergy),
    NewEnergyPoint is EnergyPoint + NewEnergy,
    put_dict(AgentId, AgentsDict, agents{children:Children, energy_point:NewEnergyPoint, subtype:AgentSubtype, type:Type, x:AgentX, y:AgentY}, NewAgentsDict),
    NewState = [NewAgentsDict, NewObjectsDict, Time, TurnOrder]
  );
  % Check if there is an agent at the given location
  (get_dict(EatenAgentId, AgentsDict, EatenAgent), EatenAgent = agents{children:_, energy_point:_, subtype:EatenSubtype, type:_, x:X, y:Y} -> 
  (
    % Update the state by deleting the agent
    del_dict(EatenAgentId, AgentsDict, _, NewAgentsDict),
    % Increase the energy point of agent
    energy_point(EatenSubtype, NewEnergy),
    NewEnergyPoint is EnergyPoint + NewEnergy,
    put_dict(AgentId, NewAgentsDict, agents{children:Children, energy_point:NewEnergyPoint, subtype:AgentSubtype, type:Type, x:AgentX, y:AgentY}, FinalAgentsDict),
    NewState = [FinalAgentsDict, ObjectsDict, Time, TurnOrder]
  ))).

% Predicate for increasing agent's number of children if have enough energy point
increase_children(State, AgentId, NewState) :-
  State = [AgentsDict, ObjectsDict, Time, TurnOrder],
  get_dict(AgentId, AgentsDict, Agent),
  Agent = agents{children:Children, energy_point:EnergyPoint, subtype:AgentSubtype, type:Type, x:AgentX, y:AgentY},
  reproduction_ep(AgentSubtype, ReproductionEnergy),
  (EnergyPoint >= ReproductionEnergy -> 
    (
      NewEnergyPoint is EnergyPoint - ReproductionEnergy,
      NewChildren is Children + 1,
      put_dict(AgentId, AgentsDict, agents{children:NewChildren, energy_point:NewEnergyPoint, subtype:AgentSubtype, type:Type, x:AgentX, y:AgentY}, NewAgentsDict),
      NewState = [NewAgentsDict, ObjectsDict, Time, TurnOrder]
    );
    NewState = State).


% Predicate for adding object or agent at given location to temporary agents dict or objects dict
% First check if there is an agent at given location
% If there is an agent, add it to temporary agents dict
% If there is an object, add it to temporary objects dict
% If there is neither, return the same state
add_agent_or_object(State, X, Y, TempAgentsDict, TempObjectsDict, NewTempAgentsDict, NewTempObjectsDict) :-
  State = [AgentsDict, ObjectsDict, _, _],
  (get_dict(AgentId, AgentsDict, Agent), Agent = agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y} -> 
  (put_dict(AgentId, TempAgentsDict, Agent, NewTempAgentsDict), NewTempObjectsDict = TempObjectsDict);
  (get_dict(ObjectId, ObjectsDict, Object), Object = object{subtype:_, type:_, x:X, y:Y} -> 
  (put_dict(ObjectId, TempObjectsDict, Object, NewTempObjectsDict), NewTempAgentsDict = TempAgentsDict))).

% Predicate for removing agent or object from State
% First check if there is an agent at given location
% If there is an agent, delete it from agents dict
% If there is an object, delete it from objects dict
% If there is neither, return the same state
remove_agent_or_object(State, X, Y, NewState) :-
  State = [AgentsDict, ObjectsDict, Time, TurnOrder],
  (get_dict(AgentId, AgentsDict, Agent), Agent = agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y} -> 
  del_dict(AgentId, AgentsDict, _, NewAgentsDict),
  NewState = [NewAgentsDict, ObjectsDict, Time, TurnOrder];
  (get_dict(ObjectId, ObjectsDict, Object), Object = object{subtype:_, type:_, x:X, y:Y} -> 
  del_dict(ObjectId, ObjectsDict, _, NewObjectsDict),
  NewState = [AgentsDict, NewObjectsDict, Time, TurnOrder])).

% Predicate for merging temporary agents dict and objects dict with the state
% First merge temporary agents dict with the state agents dict
% Then merge temporary objects dict with the state objects dict
merge_temp_dicts_with_state(State, TempAgentsDict, TempObjectsDict, NewState) :-
  State = [AgentsDict, ObjectsDict, Time, TurnOrder],
  dict_pairs(TempAgentsDict, _, TempAgentsPairs),
  dict_pairs(AgentsDict, _, AgentsPairs),
  append(TempAgentsPairs, AgentsPairs, NewAgentsPairs),
  dict_pairs(NewAgentsDict, _, NewAgentsPairs),
  dict_pairs(TempObjectsDict, _, TempObjectsPairs),
  dict_pairs(ObjectsDict, _, ObjectsPairs),
  append(TempObjectsPairs, ObjectsPairs, NewObjectsPairs),
  dict_pairs(NewObjectsDict, _, NewObjectsPairs),
  NewState = [NewAgentsDict, NewObjectsDict, Time, TurnOrder].


consume_all_helper(State, AgentId, NumberOfMoves, CurNumberOfMoves, Value, NumberOfChildren, DepthLimit, TempAgentsDict, TempObjectsDict, FinalState) :-
  (find_nearest_food(State, AgentId, Coordinates, _, _) -> 
    ( Coordinates = [X, Y],
      move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) -> 
      (
        print(ActionList),
        list_length(ActionList, ActionListLength),
        NewCurNumberOfMoves is CurNumberOfMoves + ActionListLength,
        consume_food_at_location(State, AgentId, X, Y, StateAfterConsume),
        change_location_of_agent(StateAfterConsume, AgentId, X, Y, StateAfterMove),
        increase_children(StateAfterMove, AgentId, StateAfterReproduction),
        merge_temp_dicts_with_state(StateAfterReproduction, TempAgentsDict, TempObjectsDict, StateAfterMerge),
        consume_all_helper(StateAfterMerge, AgentId, NumberOfMoves, NewCurNumberOfMoves, Value, NumberOfChildren, DepthLimit, _{}, _{}, FinalState)
      );
      (
        Coordinates = [X, Y],
        add_agent_or_object(State, X, Y, TempAgentsDict, TempObjectsDict, NewTempAgentsDict, NewTempObjectsDict),
        remove_agent_or_object(State, X, Y, StateAfterRemove),
        consume_all_helper(StateAfterRemove, AgentId, NumberOfMoves, CurNumberOfMoves, Value, NumberOfChildren, DepthLimit, NewTempAgentsDict, NewTempObjectsDict, FinalState)
      ) 
    );
    (
      merge_temp_dicts_with_state(State, TempAgentsDict, TempObjectsDict, StateAfterMerge),
      % Calculate the value of the farm
      FinalState = StateAfterMerge,
      value_of_farm(StateAfterMerge, Value),
      number_of_children(AgentId, StateAfterMerge, NumberOfChildren),
      NumberOfMoves is CurNumberOfMoves
    )
  ).
% state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], consume_all(State, 0, NumberOfMovements, Value, NumberOfChildren, 1).
consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit, FinalState) :-
  consume_all_helper(State, AgentId, NumberOfMoves, 0, Value, NumberOfChildren, DepthLimit, _{}, _{}, FinalState).
