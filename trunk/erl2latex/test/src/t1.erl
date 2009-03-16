%% @_
%% \title{t1}
%% \author{Ulf $<$ulf@wiger.net$>$}
%% \maketitle
%% _@
-module(t1).

-export([f/1]).

-erl2latex([{documentclass, auto}]).

%%% @__ One-liner latex

%%% not latex
f(X) ->
    X.

%% @hide

%% This comment is hidden
hidden_function() ->
    foo.

%% @show

visible_function() ->
    foo.

