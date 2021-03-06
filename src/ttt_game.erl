-module(ttt_game).
-author("Jonathon Mah").
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1, make_move/3, get_board/1]).
-export([print_board/1, board_to_iolist/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-type piece() :: 'o' | 'x'.
-type square() :: piece() | 'u'.
-type board() :: [[square(),...],...].

start({{_OPid,_ORef},{_XPid,_XRef}}=PlayerInfo) ->
    gen_server:start(?MODULE, PlayerInfo, []).

-spec make_move(any(), reference(), board()) -> ok | {error, any()}.
make_move(ServerRef, ClientRef, NewBoard) ->
    case is_valid_board(NewBoard) of
        true ->
            gen_server:call(ServerRef, {make_move, ClientRef, NewBoard});
        false ->
            {error, invalid_board}
    end.

-spec get_board(any()) -> board().
get_board(ServerRef) ->
    gen_server:call(ServerRef, get_board, 5000).

-spec print_board(board()) -> ok.
print_board(Board) ->
    io:fwrite(board_to_iolist(Board)).

-spec board_to_iolist(board()) -> iolist().
board_to_iolist(Board) ->
    BoardChars = lists:flatmap(fun(Row) -> lists:map(fun square_to_char/1, Row) end, Board),
    io_lib:format("~c|~c|~c~n"
                  "-+-+-~n"
                  "~c|~c|~c~n"
                  "-+-+-+~n"
                  "~c|~c|~c~n", BoardChars).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

-record(player, { pid :: pid(), ref :: reference(), monitor :: reference(), piece :: piece() }).
-record(st, { players :: [#player{}], board=[[u,u,u],[u,u,u],[u,u,u]] :: board() }).

init({{OPid,ORef},{XPid,XRef}}) ->
    OPlayer = #player{pid=OPid, ref=ORef, monitor=monitor(process, OPid), piece=o},
    XPlayer = #player{pid=XPid, ref=XRef, monitor=monitor(process, XPid), piece=x},
    NewState = #st{players=[OPlayer, XPlayer]},
    notify_player(OPlayer, {your_move, OPlayer#player.piece, NewState#st.board}),
    {ok, NewState}.

handle_call(get_board, _From, State) ->
    {reply, State#st.board, State};

handle_call({make_move, Ref, NewBoard}, From, State) ->
    [ThisPlayer, NextPlayer] = State#st.players,

    % Verify ref and that the move was legal
    try validate_move(ThisPlayer, State#st.board, Ref, NewBoard) of
        ok ->
            NewState = State#st{players=[NextPlayer, ThisPlayer], board=NewBoard},
            % Reply happens here!
            gen_server:reply(From, ok),

            case board_state(NewBoard) of
                {winner, Piece} when Piece == ThisPlayer#player.piece ->
                    % Only current player can win
                    notify_player(ThisPlayer, {you_win, NewBoard}),
                    notify_player(NextPlayer, {you_lose, NewBoard}),
                    {stop, normal, NewState};

                tie ->
                    notify_player(ThisPlayer, {tie, NewBoard}),
                    notify_player(NextPlayer, {tie, NewBoard}),
                    {stop, normal, NewState};

                ongoing ->
                    notify_player(NextPlayer, {your_move, NextPlayer#player.piece, NewBoard}),
                    {noreply, NewState}
            end
    catch
        throw:{error, _}=Error -> {reply, Error, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Monitor, process, _, _}, State) ->
    {stop, player_died, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    lists:map(fun(#player{monitor=Monitor}) -> demonitor(Monitor, [flush]) end, State#st.players),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec square_to_char(square()) -> char().
square_to_char(o) -> $O;
square_to_char(x) -> $X;
square_to_char(u) -> $ .

-spec is_valid_board(board()) -> boolean().
is_valid_board([_,_,_]=Rows) ->
    lists:foldl(fun(Row, Valid) -> Valid andalso is_valid_row(Row) end, true, Rows);
is_valid_board(_) ->
    false.

-spec is_valid_row([piece(),...]) -> boolean().
is_valid_row([_,_,_]=Squares) ->
    lists:foldl(fun(Square, Valid) -> Valid andalso is_valid_square(Square) end, true, Squares);
is_valid_row(_) ->
    false.

-spec is_valid_square(square()) -> boolean().
is_valid_square(o) -> true;
is_valid_square(x) -> true;
is_valid_square(u) -> true;
is_valid_square(_) -> false.

-spec validate_move(#player{}, board(), reference(), board()) -> 'ok'.
validate_move(Player, OldBoard, Ref, NewBoard) ->
    #player{ref = PlayerRef, piece = PlayerPiece} = Player,
    if
        Ref /= PlayerRef -> throw({error, invalid_ref});
        true -> continue
    end,
    case is_valid_board(NewBoard) of
        false -> throw({error, invalid_board});
        _ -> continue
    end,
    case pieces_added(OldBoard, NewBoard) of
        [PlayerPiece] -> continue;
        _ -> throw({error, illegal_move})
    end,
    ok.

-spec pieces_added(board(), board()) -> [piece()].
pieces_added(OldBoard, NewBoard) ->
    Delta = lists:zipwith(fun(OldRow, NewRow) ->
                lists:zipwith(fun(OldSquare, NewSquare) ->
                            case {OldSquare, NewSquare} of
                                {u, o} -> [o];
                                {u, x} -> [x];
                                {A, A} -> []
                            end
                    end, OldRow, NewRow)
        end, OldBoard, NewBoard),
    lists:flatten(Delta).

-spec board_state(board()) -> {'winner', piece()} | 'tie' | 'ongoing'.
board_state(Board) ->
    case lists:member(u, lists:flatten(Board)) of
        false -> tie;
        _ ->
            case board_winner(Board) of
                [P] -> {winner, P};
                _ -> ongoing
            end
    end.

-spec board_winner(board()) -> [piece()].
board_winner(Board) ->
    case diag_winner(Board) of
        [P] -> [P];
        _ ->
            case lists:flatmap(fun row_winner/1, Board) of
                [P] -> [P];
                _ ->
                    Transposed = apply(fun lists:zipwith3/4, [fun(A,B,C) -> [A,B,C] end | Board]),
                    lists:flatmap(fun row_winner/1, Transposed)
            end
    end.

-spec row_winner([square(),...]) -> [piece()].
row_winner([P,P,P]) when P /= u -> [P];
row_winner([_,_,_]) -> [].

-spec diag_winner(board()) -> [piece()].
diag_winner([[P,_,_],[_,P,_],[_,_,P]]) when P /= u -> [P];
diag_winner([[_,_,P],[_,P,_],[P,_,_]]) when P /= u -> [P];
diag_winner(_) -> [].

-spec notify_player(#player{}, any()) -> ok.
notify_player(#player{pid=Pid}, Msg) ->
    Pid ! {ttt_game, self(), Msg},
    ok.
