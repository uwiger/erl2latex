%% The MIT License
%%
%% Copyright (c) 2008 Ulf Wiger <ulf@wiger.net>,
%%                    John Hughes <john.hughes@quviq.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
-module(erl2latex).

-export([file/1, file/2]).


file(F) ->
    file(F, latex_target(F)).

file(F, Target) ->
    case file:read_file(F) of
        {ok, Bin} ->
            Parts = split_input(binary_to_list(Bin)),
            Latex =
                case binary_to_list(
                       list_to_binary([convert_part(P) || P <- Parts])) of
                    "\\document" ++ _ = Latex0 ->
                        {Preamble,Doc} = get_preamble(Latex0),
                        [Preamble, source_listing_setup(Preamble),
                         Doc, end_doc()];
                    Latex0 ->
                        [default_preamble(),
                         "\\begin{document}\n",
                         Latex0, end_doc()]
                end,
            output(Latex, Target);
        Err ->
            Err
    end.

get_preamble(Str) ->
    get_preamble(Str, []).

get_preamble("\\begin{document}" ++ Rest, Acc) ->
    {lists:reverse("\n" ++ Acc), "\\begin{document}\n" ++ Rest};
get_preamble([H|T], Acc) ->
    get_preamble(T, [H|Acc]).

default_preamble() ->
    ["\\documentclass[a4paper,12pt]{article}\n",
     source_listing_setup()].

source_listing_setup(Preamble) ->
    case regexp:first_match(Preamble, "begin{mylisting}") of
        {match,_,_} ->
            [];
        nomatch ->
            source_listing_setup()
    end.

source_listing_setup() ->
    ("\\newenvironment{mylisting}\n"
     "{\\begin{list}{}{\\setlength{\\leftmargin}{1em}}"
     "\\item\\scriptsize\\bfseries}\n"
     "{\\end{list}}\n"
     "\n"
     "\\newenvironment{mytinylisting}\n"
     "{\\begin{list}{}{\\setlength{\\leftmargin}{1em}}"
     "\\item\\tiny\\bfseries}\n"
     "{\\end{list}}\n").


end_doc() ->
    "\n\\end{document}\n".


split_input(Txt) ->
    Lines = string:tokens(Txt, "\n"),
    group([wrap(L) || L <- Lines]).

wrap("%" ++ S) ->
    {comment, strip_comment(S)};
wrap(S) ->
    {code, S}.

 
group([{T,C}|Tail]) ->
    {More,Rest} = lists:splitwith(fun({T1,_C1}) -> T1 == T end, Tail),
    [{T,[C|[C1 || {_,C1} <- More]]} | group(Rest)];
group([]) ->
    [].






convert_part({code,Lines}) ->
    ["\\begin{mylisting}\n"
     "\\begin{verbatim}\n",
     [[expand(L),"\n"] || L <- Lines],
     "\\end{verbatim}\n"
     "\\end{mylisting}\n\n"];
convert_part({comment,Lines}) ->
    [[[L,"\n"] || L <- Lines],"\n"].

expand(Line) ->
    lists:map(fun($\t) -> ["    "];
                 (C) -> C
              end, Line).
                       

escape(Lines) ->
    [[[escape_char(C) || C <- L],"\n"] || L <- Lines].

escape_char(C) ->
    case lists:member(C, "#$%&_{}") of
        true ->
            [$\\,C];
        false ->
            if C==$\\ -> "\\char92";
               C==$\^ -> "\\char94";
               C==$~  -> "\\char126";
               true -> C
            end
    end.

strip_comment(C) ->
    string:strip(lists:dropwhile(fun($\%) -> true; (_) -> false end, C), left).


latex_target(F) ->
    filename:basename(F,".erl") ++ ".tex".

output(Data, F) ->
    file:write_file(F, list_to_binary(Data)).
