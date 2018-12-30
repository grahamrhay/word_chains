-module(prop_word_chains).

-include_lib("proper/include/proper.hrl").

prop_word_chains() ->
    ?FORALL(_, term(),
        begin
            true
        end).
