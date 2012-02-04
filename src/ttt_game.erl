-module(ttt_game).
-author("Jonathon Mah").
-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, print_board/1, board_to_iolist/1, is_valid_step/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-type piece() :: 'o' | 'x'.
-type square() :: piece() | 'u'.
-type board() :: [[square(),...],...].

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

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

-spec is_valid_step(board(), board()) -> boolean().
is_valid_step(OldBoard, NewBoard) ->
    Delta = lists:zipwith(fun(OldRow, NewRow) ->
                lists:zipwith(fun(OldSquare, NewSquare) ->
                            case {OldSquare, NewSquare} of
                                {u, o} -> 1;
                                {u, x} -> 1;
                                {A, A} -> 0;
                                _      -> 0
                            end
                    end, OldRow, NewRow)
        end, OldBoard, NewBoard),
    lists:sum(lists:flatten(Delta)) == 1.

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

-record(st, {o_ref, x_ref, board=[[u,u,u],[u,u,u],[u,u,u]]}).

init(_Args) ->
    {ok, initial_state_name, initial_state}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec square_to_char(square()) -> char().
square_to_char(o) -> $O;
square_to_char(x) -> $X;
square_to_char(u) -> $ .
