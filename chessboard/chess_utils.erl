-module(chess_utils).

-export([init_board/0]).

init_board() ->
    Columns = lists:seq(0, 7),
    BlackPieces = [{black, rook}, {black, knight}, {black, bishop}, {black, queen},
                   {black, king}, {black, bishop}, {black, knight}, {black, rook}],
    WhitePieces = [{white, rook}, {white, knight}, {white, bishop}, {white, queen},
                   {white, king}, {white, bishop}, {white, knight}, {white, rook}],
    Row1 = [{{C, 1}, {black, pawn}} || C <- Columns ],
    Row6 = [{{C, 6}, {white, pawn}} || C <- Columns ],
    Row0 = [{{C, 0}, lists:nth(C + 1, BlackPieces)} || C <- Columns ],
    Row7 = [{{C, 7}, lists:nth(C + 1, WhitePieces)} || C <- Columns ],
    maps:from_list(Row0 ++ Row1 ++ Row6 ++ Row7).
