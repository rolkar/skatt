%% Berakna skatt inkomstår 2012

-module(skatt).

-export([skatt/1]).

%% API -----------------------------------------------------------

skatt(Inkomst) ->
    TaxInkomst = rounddown(Inkomst),
    Grundavdrag = round(grundavdrag(TaxInkomst)),
    NetInkomst = erlang:max(TaxInkomst - Grundavdrag, 0),

    Statlig = round(statlig(NetInkomst)),
    Kommunal = round(kommunal(NetInkomst)),
    Pensionsavgift = round(pensionsavgift(TaxInkomst, Kommunal)),
    Pensionsavdrag = round(pensionsavdrag(Pensionsavgift, Kommunal)),
    Jobbavdrag = round(jobbavdrag(TaxInkomst, Grundavdrag, Pensionsavdrag)),

    Skatt = round(Statlig + Kommunal - Jobbavdrag + Pensionsavgift - Pensionsavdrag),
    Arbetsgivaravgift = round(arbetsgivaravgift(Inkomst)),
    BruttoInkomst = round(Inkomst + Arbetsgivaravgift),
    Kvar = Inkomst - Skatt,

    io:format("~n", []),
    io:format("~-17s = ~10B~n", ["Inkomst", Inkomst]),
    io:format("~-17s = ~10B~n", ["TaxInkomst", TaxInkomst]),
    io:format("~-17s = ~10B~n", ["Grundavdrag", Grundavdrag]),
    io:format("~-17s = ~10B~n", ["NetInkomst", NetInkomst]),
    io:format("~-17s = ~10B~n", ["Statlig", Statlig]),
    io:format("~-17s = ~10B~n", ["Kommunal", Kommunal]),
    io:format("~-17s = ~10B~n", ["Pensionsavgift", Pensionsavgift]),
    io:format("~-17s = ~10B~n", ["Pensionsavdrag", Pensionsavdrag]),
    io:format("~-17s = ~10B~n", ["Jobbavdrag", Jobbavdrag]),
    io:format("~-17s = ~10B (~p %)~n", ["Skatt",
                                        Skatt,
                                        percent(Skatt, Inkomst)]),
    io:format("~-17s = ~10B (~p %)~n", ["Arbetsgivaravgift",
                                        Arbetsgivaravgift,
                                        percent(Arbetsgivaravgift, Inkomst)]),
    io:format("~-17s = ~10B~n", ["BruttoInkomst", BruttoInkomst]),
    io:format("~-17s = ~10B (~p %) (~p % brutto)~n",
              ["Kvar",
               Kvar,
               percent(Kvar, Inkomst),
               percent(Kvar, BruttoInkomst)]),
    io:format("~n", []),

    {Inkomst, Skatt, Arbetsgivaravgift, BruttoInkomst}.


%% delberakningar ------------------------------------------------

grundavdrag(Inkomst) ->
    PBB = prisbasbelopp(),
    G =
        if
            Inkomst < 0.99*PBB -> 0.423*PBB;
            Inkomst < 2.72*PBB -> 0.225*PBB + 0.2*Inkomst;
            Inkomst < 3.11*PBB -> 0.770*PBB;
            Inkomst < 7.88*PBB -> 1.081*PBB - 0.1*Inkomst;
            true               -> 0.293*PBB
        end,
    roundup(G).

pensionsavgift(_Inkomst, Kommunal) when Kommunal == 0 ->
    0;
pensionsavgift(Inkomst, _Kommunal) ->
    IBB = inkomstbasbelopp(),
    P = 
        if
            Inkomst < 0.423*IBB -> 0;
            Inkomst < 8.070*IBB -> 0.07*Inkomst;
            true                -> 0.07*8.07*IBB
        end,
    100*round(P/100).

pensionsavdrag(Pensionsavgift, Kommunal) ->
    erlang:min(Pensionsavgift, Kommunal).

jobbavdrag(Inkomst, Grundavdrag, Pensionsavdrag) ->
    PBB = prisbasbelopp(),
    J =
        if
            Inkomst =< Grundavdrag -> 0;
            Inkomst =< 0.91*PBB    -> kommunal(Inkomst - Grundavdrag);
            Inkomst =< 2.72*PBB    -> kommunal(0.910*PBB + (Inkomst-0.91*PBB)*0.304 - Grundavdrag);
            Inkomst =< 7.00*PBB    -> kommunal(1.461*PBB + (Inkomst-2.72)*0.095     - Grundavdrag);
            true                   -> kommunal(1.868*PBB                            - Grundavdrag)
        end,

    K = kommunal(erlang:max(Inkomst - Grundavdrag, 0)) - Pensionsavdrag,
    erlang:min(K, J).

kommunal(NetInkomst) ->
    kommunalskattesats() * NetInkomst.

%% 20% statlig skatt plus 5% extra varnskatt vid tva brytpunkter
statlig(NetInkomst) ->
    BP1 = brytpunkt1(),
    BP2 = brytpunkt2(),
    [I1, I2, I3] = split(NetInkomst, [BP1, BP2-BP1]),
    I1 * 0.0 + I2 * 0.2 + I3 * 0.25.

%% 2012
arbetsgivaravgift(Inkomst) ->
    Inkomst * arbetsgivarskattesats().


%% help ----------------------------------------------------------

split(NetInkomst, Brytpunkter) ->
    split(NetInkomst, Brytpunkter, []).

split(NetInkomst, [], Acc) ->
    Acc ++ [NetInkomst];
split(NetInkomst, [Brytpunkt|Bs], Acc) ->
    if
        NetInkomst > Brytpunkt ->
            split(NetInkomst - Brytpunkt, Bs, Acc ++ [Brytpunkt]);
        true ->
            split(0, Bs, Acc ++ [NetInkomst])
    end.

roundup(Value) ->
    100 * ceiling(Value/100).

rounddown(Value) ->
    100 * floor(Value/100).

percent(Skatt, Inkomst) ->
    round(100*Skatt/Inkomst).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


%% Yearly settings -----------------------------------------------

%% 2012
prisbasbelopp() ->
    44000.
%% 2013
%% prisbasbelopp() ->
%%     44500.

%% 2012
inkomstbasbelopp() ->
    54500.
%% 2013
%% inkomstbasbelopp() ->
%%     56500.

%% 2012
brytpunkt1() ->
    401100.

%% 2012
brytpunkt2() ->
    574300.

%% 2012
arbetsgivarskattesats() ->
    0.3142.


%% Your settings -------------------------------------------------

%% Din kommun (kommun + lan)
kommunalskattesats() ->
    0.316. %% genomsnitt 2012
