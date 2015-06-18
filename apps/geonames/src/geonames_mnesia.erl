-module(geonames_mnesia).

-include("geonames_log.hrl").

-define(TIMEZONE, geonames_timezone).
-define(CITY,geonames_city).
-define(COUNTRY_AREAS,geonames_country_areas).
-define(COUNTRY_DETAILS,geonames_country_details).
-define(NEIGHBOURS,geonames_country_neighbours).
-define(CONTINENT,geonames_country_continent).
-define(LANGUAGES,geonames_languages).
-define(ADMIN_1,geonames_admin_codes_1).
-define(ADMIN_2,geonames_admin_codes_2).

-define(DEFAULT_ADMIN_CODE,<<>>).

-export([insert/2,init/1,wait_tables/0,delete_tables/0]).

-export([read/2,tables_exist/0]).

-record(?CITY, {
    id, 
    name, 
    ascii_name, 
    aliases, 
    coords_latlon, 
    country_code,
    adminCode1=?DEFAULT_ADMIN_CODE, 
    adminCode2=?DEFAULT_ADMIN_CODE, 
    adminCode3=?DEFAULT_ADMIN_CODE, 
    adminCode4=?DEFAULT_ADMIN_CODE, 
    country_name,
    admin1_name,
    admin2_name,
    adminName,
    population, 
    elevation, 
    dem, 
    timezone_id, 
    timezone_offset, 
    modification_date
} ).



-record(?TIMEZONE, {
    id,
    country_code,
    offset
}).

-record(?COUNTRY_AREAS,{
    country_code,
    admin1,
    admin2,
    admin3,
    admin4,
    admin1Name,
    admin2Name
}).

-record(?ADMIN_1,{
    key, %{Country, code}
    country,
    code,
    name,
    ascii_name,
    geoname_id

}).

-record(?ADMIN_2,{
    key, %{Country, admin1,code}
    country,
    admin1_code,
    code,
    name,
    ascii_name,
    geoname_id
}).

-record(?COUNTRY_DETAILS,{
    iso,
    iso3,
    iso_numeric,
    fips,
    name,
    capital,
    area_sqkm,
    population,
    continent,
    tld,
    currency_code,
    currency_name,
    phone_code,
    postal_code_format,
    postal_code_regex,
    languages,
    geoname_id,
    neighbours

}).

-record(?NEIGHBOURS,{
    country_code,
    country_name,
    neighbour
} ).

-record(?CONTINENT,{
    name,
    country_name,
    country_code
} ).

-record(?LANGUAGES,{
    language,
    country_code,
    country_name
} ).

wait_tables() -> mnesia:wait_for_tables(tabs(),60000).
    
delete_tables() ->
    [ mnesia:delete_table(T) || T <- tabs() ].
    
tables_exist() ->
    Res = lists:map( fun( T ) ->
        try 
            mnesia:table_info(T, size ) > 0
        catch 
            _ ->
                false
        end
    end, tabs() ),
    
    not lists:member( false, Res ).
                

read(neighbours, Code ) -> key_read(?NEIGHBOURS, safe_binary(Code) );
read(continent, all ) -> mnesia:dirty_all_keys(?CONTINENT);
read(continent, Name ) -> key_read(?CONTINENT, safe_binary(Name) );
read(country, { tld,TLD} ) -> index_read(?COUNTRY_DETAILS, safe_binary(TLD),#?COUNTRY_DETAILS.tld );
read(country, { currency,Name} ) -> index_read(?COUNTRY_DETAILS, safe_binary(Name),#?COUNTRY_DETAILS.currency_code );
read(country, { name,Name} ) -> index_read(?COUNTRY_DETAILS, safe_binary(Name),#?COUNTRY_DETAILS.name );
read(country, { continent, Name } ) ->
    unique(mnesia:dirty_select(?CONTINENT, countries_in_continent_ms(Name) ) );
read(country,  Code ) when is_binary(Code),byte_size(Code)==2  -> key_read(?COUNTRY_DETAILS,Code);
read(country,  Code ) when is_binary(Code),byte_size(Code)==3  -> index_read(?COUNTRY_DETAILS,Code, #?COUNTRY_DETAILS.iso3);
read(country,  Name ) when is_binary(Name) -> index_read(?COUNTRY_DETAILS,Name, #?COUNTRY_DETAILS.name);
read(country,  CountryQuery) -> 
    case country_query_ms(CountryQuery) of
        {ok, MS } ->
            select(?COUNTRY_DETAILS, MS );
        _ ->
            read(country,safe_binary(CountryQuery) )
    end;
read(country_codes,_) -> mnesia:dirty_all_keys(?COUNTRY_DETAILS);
read(cities, {by_area,AreaPList} ) when is_list(AreaPList) ->
    GV = fun( K ) -> 
        safe_binary(proplists:get_value(K,AreaPList,?DEFAULT_ADMIN_CODE))
    end,
    select( ?CITY, city_query_ms({
        by_area, 
        GV(country_code), 
        GV(admin1),
        GV(admin2),
        GV(admin3),
        GV(admin4) } ) );
read(cities, Query ) ->
    select(?CITY, city_query_ms(Query) );
read(country_areas,{Code,AdminLevels}) -> 
    unique(mnesia:dirty_select(?COUNTRY_AREAS, country_area_ms({admin_level, safe_binary(Code),AdminLevels } ) ) );

read(country_areas,{sub_areas,_CountryCode, _Admin1}=Q) -> 
    unique(mnesia:dirty_select(?COUNTRY_AREAS, country_area_ms(Q) ) );
read(country_areas,Code) -> key_read(?COUNTRY_AREAS, safe_binary(Code) );
read(country_cities,CC) ->
    index_read(?CITY, safe_binary(CC),#?CITY.country_code ).
    
select( Tab, MS) ->
    to_plist(mnesia:dirty_select(Tab,MS) ).
    
    
unique(List) -> sets:to_list(sets:from_list(List)).
    
    
countries_in_continent_ms(Name) ->
    [ { #?CONTINENT{ 
        name=safe_binary(Name),
        country_name='$2',
        country_code='$1' },
        [],
        [ {{'$1','$2'}}]
        }
    ].
    
country_area_ms( {sub_areas, CountryCode, Admin1} ) ->
    [
        {
        #?COUNTRY_AREAS{
            country_code=safe_binary(CountryCode),
            admin1=safe_binary(Admin1),
            admin2='$1',
            admin2Name='$2',
            _='_'
        },
        [],
        [ {{'$1', '$2'}} ] 
        }
    ];
country_area_ms({admin_level,Code, 1} ) ->
    [
        {
        #?COUNTRY_AREAS{
            country_code=safe_binary(Code),
            admin1='$1',
            admin1Name='$2',
            _='_'
        },
        [],
        [ {{'$1', '$2'}} ] 
        }
    ];
country_area_ms({admin_level,Code, 2} ) ->
    [
        {
        #?COUNTRY_AREAS{
            country_code=safe_binary(Code),
            admin1='$1',
            admin2='$2',
            admin1Name='$3',
            admin2Name='$4',
            _='_'
        },
        [],
        [ [ {{'$1','$3'}}, {{'$2','$4'}} ]  ]
        }
    ];
country_area_ms({admin_level,Code, 3} ) ->
    [
        {
        #?COUNTRY_AREAS{
            country_code=safe_binary(Code),
            admin1='$1',
            admin2='$2',
            admin3='$3',
            admin1Name='$4',
            admin2Name='$5',
            _='_'
        },
        [],
        [ [ {{'$1', '$4'}}, {{'$2', '$5'}}, '$3' ] ]
        }
    ];
country_area_ms({admin_level,Code, 4} ) ->
    [
        {
        #?COUNTRY_AREAS{
            country_code=safe_binary(Code),
            admin1='$1',
            admin2='$2',
            admin3='$3',
            admin4='$4',
            admin1Name='$5',
            admin2Name='$6'
        },
        [],
        [ [ {{'$1', '$5'}}, {{'$2', '$6'}}, '$3' , '$4' ] ]
        }
    ].
    
    

    

country_query_ms({min_population, Population}) ->
    [ { #?COUNTRY_DETAILS{ population='$1', _='_' }, 
        [ {'>', '$1', Population } ],
        [ '$_' ]
    }].



city_query_ms(QueryList) when is_list(QueryList) ->
    city_query_ms(QueryList, undefined);
city_query_ms({by_area, CountryCode, Code1} ) ->
    [ {
        #?CITY{ 
            country_code=safe_binary(CountryCode),
            adminCode1=safe_binary(Code1),
            _='_'},
        [],
        ['$_'] 
    }];
city_query_ms({by_area, CountryCode, Code1,Code2} ) ->
    [ {
        #?CITY{ 
            country_code=safe_binary(CountryCode),
            adminCode1=safe_binary(Code1),
            adminCode2=safe_binary(Code2),
            _='_'},
        [],
        ['$_'] 
    }];
city_query_ms({by_area, CountryCode, Code1,Code2,Code3} ) ->
    [ {
        #?CITY{ 
            country_code=safe_binary(CountryCode),
            adminCode1=safe_binary(Code1),
            adminCode2=safe_binary(Code2),
            adminCode3=safe_binary(Code3),
            _='_'},
        [],
        ['$_'] 
    }];
city_query_ms({by_area, CountryCode, Code1,Code2,Code3,Code4 } ) ->
    [ {
        #?CITY{ 
            country_code=safe_binary(CountryCode),
            adminCode1=safe_binary(Code1),
            adminCode2=safe_binary(Code2),
            adminCode3=safe_binary(Code3),
            adminCode4=safe_binary(Code4),
            _='_'},
        [],
        ['$_'] 
    }];
city_query_ms({min_population, Num} ) ->
    [ { #?CITY{ population='$1', _='_' }, [ {'>',  '$1', Num }], [ '$_' ] } ].
    

city_query_ms([],Current) -> Current;
city_query_ms([H|QueryList], Current) ->
    MS = city_query_ms(H),
    city_query_ms(QueryList, merge_ms(MS,Current) ).
    
merge_ms( MS, undefined ) -> MS;    
merge_ms([ {H,G,R} ] , [ {H2,G2,R} ] ) when element(1,H) == element(1,H2) ->
    NH = combine_match_head(H,H2),
    NG = combine_guards(G,G2),
    [ { NH, NG, R } ].

combine_match_head(H,H2) ->
    PList1 = to_plist(H),
    PList2 = to_plist(H2),
    Zipped = lists:zipwith(fun( {K,V}, { K,V2} ) ->
        case V of
            '_' -> {K,V2};
            _ -> {K,V}
        end
    end, PList1,PList2),
     {_,Values} = lists:unzip(Zipped),
    list_to_tuple([ element(1,H) |  Values ] ).
    
 combine_guards([],G2) -> G2;
 combine_guards(G1,[]) -> G1;
 combine_guards([G1],[G2]) -> [ { 'and', G1, G2 } ].
 
    
    
key_read(Tab,Key) ->
    to_plist(mnesia:dirty_read(Tab,Key) ).
    
index_read(Tab,Match,Index) -> to_plist(mnesia:dirty_index_read(Tab,Match,Index) ).

to_plist(List) when is_list(List) -> lists:map( fun to_plist/1, List );
to_plist(Rec) ->
    [H|T] = tuple_to_list(Rec),
    Fields = field_info(H),
    lists:zip(Fields,T).

tabs() -> [
            ?CITY,
            ?COUNTRY_AREAS,
            ?TIMEZONE,
            ?COUNTRY_DETAILS,
            ?NEIGHBOURS,
            ?CONTINENT,
            ?ADMIN_1,
            ?ADMIN_2
        ].
        
init(MnesiaOptions) ->
    [ create_table(Tab,MnesiaOptions) ||
        Tab <-  tabs() ].

table_type(?COUNTRY_AREAS) -> bag;
table_type(?NEIGHBOURS) -> bag;
table_type(?CONTINENT) -> bag;
table_type(_) -> set.


field_info(?ADMIN_1) -> record_info(fields,?ADMIN_1);
field_info(?ADMIN_2) -> record_info(fields,?ADMIN_2);
field_info(?CONTINENT) -> record_info(fields,?CONTINENT);
field_info(?NEIGHBOURS) -> record_info(fields,?NEIGHBOURS);
field_info(?COUNTRY_DETAILS) -> record_info(fields,?COUNTRY_DETAILS);
field_info(?COUNTRY_AREAS) -> record_info(fields,?COUNTRY_AREAS);
field_info(?CITY) -> record_info(fields,?CITY);
field_info(?TIMEZONE) -> record_info(fields,?TIMEZONE).


indices( ?COUNTRY_DETAILS ) -> [ { index,[ name,currency_code,phone_code,tld] } ];
indices( ?CITY ) -> [ { index,[ country_code ] } ];
indices( ?ADMIN_1 ) -> [ { index,[ country ] } ];
indices( ?ADMIN_2 ) -> [ { index,[ country,admin1_code ] } ];
indices(_) -> [].

create_table(Table,MnesiaOptions) ->
    case mnesia:create_table(Table,[
        { attributes, field_info(Table) }
    ]++MnesiaOptions ++ [ {type,table_type(Table)} ] ++ indices(Table)  ) of
        {atomic,ok} -> 
            ?MSG("Table created: ~p",[Table]);
        {aborted,Error} ->
            ?ERROR("Failed to create table: ~p -> ~p", [ Table, Error] )
    end.
    
-define(GV(K), K=proplists:get_value(K,Proplist,undefined) ).


insert( country, Proplist ) ->
    {_,Values} = lists:unzip(Proplist),
    Rec = list_to_tuple([ ?COUNTRY_DETAILS |  Values ] ),
    mnesia:dirty_write(Rec),
    Neighbours = Rec#?COUNTRY_DETAILS.neighbours,
    Code = Rec#?COUNTRY_DETAILS.iso,
    Name = Rec#?COUNTRY_DETAILS.name,
    [
        mnesia:dirty_write(#?NEIGHBOURS{ country_name=Name,country_code=Code,neighbour=Neighbour} )
        || Neighbour <- Neighbours
    ],
    Continent = Rec#?COUNTRY_DETAILS.continent,
    mnesia:dirty_write( #?CONTINENT{ 
        name=Continent, 
        country_name=Name,
        country_code=Code
    } );
insert( city, Proplist ) ->
    TimeZoneId = proplists:get_value(timezone_id,Proplist),
    CountryCode = proplists:get_value(country_code,Proplist),
    Admin1 = proplists:get_value(adminCode1,Proplist),
    Admin2 = proplists:get_value(adminCode2,Proplist),
    Admin3 = proplists:get_value(adminCode3,Proplist),
    Admin4 = proplists:get_value(adminCode4,Proplist),
    
    
    AdminName = get_admin_name(CountryCode,Admin1,Admin2), 

    { CountryName,AdminName1,AdminName2} = AdminName,
    
    mnesia:dirty_write(#?COUNTRY_AREAS{ 
        country_code=CountryCode,
        admin1=Admin1,
        admin2=Admin2,
        admin3=Admin3,
        admin4=Admin4,
        admin1Name=AdminName1,
        admin2Name=AdminName2
    } ),
    
     [ TZRec ] = mnesia:dirty_read(?TIMEZONE,TimeZoneId ),
     TimeZoneOffset =  TZRec#?TIMEZONE.offset,
     
    
    CityRec = #?CITY{
        ?GV(id), 
        ?GV(name), 
        ?GV(ascii_name), 
        ?GV(aliases), 
        ?GV(coords_latlon), 
        ?GV(country_code),
        adminCode1=Admin1,
        adminCode2=Admin2,
        adminCode3=Admin3,
        adminCode4=Admin4,
        adminName=AdminName,
        admin1_name=AdminName1,
        admin2_name=AdminName2,
        country_name=CountryName,
        ?GV(population), 
        ?GV(elevation), 
        ?GV(dem), 
        ?GV(timezone_id), 
        timezone_offset=TimeZoneOffset,
        ?GV(modification_date)
    },
    
   
    
    mnesia:dirty_write(CityRec);

insert( timezone, Proplist ) ->
    Rec = #?TIMEZONE{
        ?GV(id),
        ?GV(country_code),
        ?GV(offset)
    },
    mnesia:dirty_write(Rec);
insert( admin1, Proplist ) ->
    {_,Values} = lists:unzip(Proplist),
    
    Rec = list_to_tuple([ ?ADMIN_1,no_key_yet|  Values ] ),
    
    mnesia:dirty_write( Rec#?ADMIN_1{ 
        key={ 
            Rec#?ADMIN_1.country, 
            Rec#?ADMIN_1.code
        } } );
insert( admin2, Proplist ) ->
    {_,Values} = lists:unzip(Proplist),
    Rec = list_to_tuple([ ?ADMIN_2,no_key_yet|  Values ] ),
    mnesia:dirty_write( Rec#?ADMIN_2{ 
        key={ 
            Rec#?ADMIN_2.country, 
            Rec#?ADMIN_2.admin1_code,
            Rec#?ADMIN_2.code
        } } ).

raw_read(T,Key) -> mnesia:dirty_read(T,Key).
    
get_admin_name(CountryCode,Admin1Code,Admin2Code) ->
    case raw_read(?COUNTRY_DETAILS, CountryCode) of
        [Country] ->
            case raw_read(?ADMIN_1,{CountryCode,Admin1Code}) of
                [Admin1] ->
                    case raw_read(?ADMIN_2,{CountryCode,Admin1Code,Admin2Code}) of
                        [Admin2] -> 
                            { Country#?COUNTRY_DETAILS.name, Admin1#?ADMIN_1.ascii_name, Admin2#?ADMIN_2.ascii_name };
                        [] ->
                            { Country#?COUNTRY_DETAILS.name, Admin1#?ADMIN_1.ascii_name, undefined }
                    end;
                [] ->
                    { Country#?COUNTRY_DETAILS.name, undefined, undefined }
            end;
        [] ->
            { undefined, undefined, undefined }
    end.
        
        
safe_binary(Bin) when is_binary(Bin) -> Bin;
safe_binary(Bin) when is_list(Bin) -> list_to_binary(Bin);
safe_binary(Other) -> term_to_binary(Other).