
-module(geonames_import).

-export([build_db/0]).


-define(DB,geonames_mnesia).

-include("geonames_log.hrl").



-define(ADMIN_1, "admin1CodesASCII.txt").
-define(ADMIN_2, "admin2Codes.txt").
-define(CITIES, "cities1000.txt").
-define(TZ, "timeZones.txt").
-define(COUNTRIES, "countryInfo.txt").

    
build_db() -> build_db(geonames_mnesia).


build_db(DBModule) ->
    
    Import = application:get_env(geonames,import, all),
    
    ImportList = import_rules(Import),
    
    lists:map(fun( {Parser, Type, File } ) ->
        parse_file( DBModule, Parser, Type, File )
    end, ImportList ),
    ?DB:wait_tables().
   
    

import_rules(all) -> import_rules( [  timezones,countries,admin1,admin2,cities ] );
import_rules(List) when is_list(List) -> lists:map( fun( K ) -> import_rule(K) end,List  );
import_rules(Atom) when is_atom(Atom) ->
    [ import_rule(Atom) ].

import_rule( admin1 ) -> { parse_admin1, admin1, ?ADMIN_1 };
import_rule( admin2 ) -> { parse_admin2, admin2, ?ADMIN_2 };
import_rule( cities ) -> { parse_geonames, city, ?CITIES };
import_rule( countries ) -> { parse_countries, country, ?COUNTRIES };
import_rule( timezones ) -> { parse_time_zones, timezone, ?TZ }.


now_gs() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()).
    
parse_file( DBModule, ParserMethod, Type, FileName ) ->
    DBDirectory = application:get_env(geonames,data_dir,"data"),
    FullFile = filename:join( [ DBDirectory, FileName ] ),
     case filelib:is_file(FullFile) of
        true ->
            NowGs = now_gs(),
            ?MSG("Parsing file: ~s", [ FullFile ] ),
            Res = geonames_parser:ParserMethod(FullFile, fun(Rec) ->
                case Rec of
                    header -> ok;
                    ignore -> ok;
                    _ when is_list(Rec) ->
                        DBModule:insert( Type,  Rec )
                end
            end ),
            Time = now_gs() - NowGs,
            ?MSG("Result ~p: took ~p", [ Res, Time ] ),
            Res;
        _ ->
            ?WARN("Invalid ~s file: ~s", [FileName,FullFile] ),
            {error, { invalid_file, FullFile } }
    end.
    