-module(ttt_game).
-author("Jonathon Mah").
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, print_board/1, board_to_iolist/1]).
-export([pieces_added/2, board_state/1]).

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

start_link({{_OPid,_ORef},{_XPid,_XRef}}=PlayerInfo) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, PlayerInfo, []).

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

-record(player, { pid :: pid(), ref :: reference(), piece :: piece() }).
-record(st, { players :: [#player{}], board=[[u,u,u],[u,u,u],[u,u,u]] :: board() }).

init({{OPid,ORef},{XPid,XRef}}) ->
    % TODO: Monitor
    OPlayer = #player{pid=OPid, ref=ORef, piece=o},
    XPlayer = #player{pid=XPid, ref=XRef, piece=x},
    notify_player(OPlayer, your_turn),
    {ok, #st{players=[OPlayer, XPlayer]}}.

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

handle_cast({move, NewBoard, Ref}, State) ->
    [#player{piece = ThisPiece}=ThisPlayer, NextPlayer] = State#st.players,

    Ref = ThisPlayer#player.ref,
    [ThisPiece] = pieces_added(State#st.board, NewBoard),

    NewState = State#st{players=[NextPlayer, ThisPlayer], board=NewBoard},

    case board_state(NewBoard) of
        {winner, ThisPiece} ->
            % Only current player can win
            notify_player(ThisPlayer, {you_win, NewBoard}),
            notify_player(NextPlayer, {you_lose, NewBoard}),
            {stop, normal, NewState};

        tie ->
            notify_player(ThisPlayer, {tie, NewBoard}),
            notify_player(NextPlayer, {tie, NewBoard}),
            {stop, normal, NewState};

        ongoing ->
            notify_player(NextPlayer, your_turn),
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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

-spec notify_player(#player{}, term()) -> ok.
notify_player(#player{pid=Pid}, Msg) ->
    Pid ! Msg,
    ok.
