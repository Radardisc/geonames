

-module(geonames_parser).

-export([parse_geonames/2,parse_time_zones/2,parse_countries/2,parse_admin1/2,parse_admin2/2]).

-ifdef(TEST).
-export([chomp/1]).
-endif.

-include("geonames_log.hrl").

parse_time_zones(File,OnRecord ) ->
    ok_guard(rfile( File, fun timezone_parser/1, OnRecord )).

parse_geonames(File, OnRecord) ->
    ok_guard(rfile( File, fun geoname_parser/1, OnRecord )).
   
parse_countries(File, OnRecord) ->
    ok_guard(rfile( File, fun country_parser/1, OnRecord ) ).
    
parse_admin1(File,OnRecord) ->
    ok_guard(rfile( File, fun admin1_parser/1, OnRecord ) ).
    

parse_admin2(File,OnRecord) ->
    ok_guard(rfile( File, fun admin2_parser/1, OnRecord ) ).
   
ok_guard({ok, _ }) -> ok;
ok_guard(Err) -> Err.


admin1_parser(Line) ->
    SplitLine = re:split(Line,"\t"),
    
    case SplitLine of
        [
        FQN,
        LocalName,
        AsciiName,
        GeoNameId
        ] ->
           
            [Country, Code] = split(FQN,"\\."),
            
            [
                { country,Country},
                { code, Code},
                { name,LocalName},
                { ascii_name, AsciiName },
                { geoname_id, chomp(GeoNameId) } 
            ];
        _Other ->
            ?ERROR("Invalid admin1 line: ~p",[ Line ] ),
            ignore
    end.
    
admin2_parser(Line) ->
    SplitLine = re:split(Line,"\t"),
    
    [
        FQN,
        LocalName,
        AsciiName,
        GeoNameId
    ]=SplitLine,
    [Country, Admin1Code,Code] = split(FQN,"\\."),
    
    [
        { country,Country},
        {admin1_code,Admin1Code},
        { code, Code},
        { name,LocalName},
        { ascii_name, AsciiName },
        { geoname_id, chomp(GeoNameId) }
    ].
timezone_parser(Line ) ->
    SplitLine = re:split(Line,"\t"),
    
    case SplitLine of
        [ <<"CountryCode">>| _ ] ->
            header;
        [ CountryCode, TimeZoneId, GMTOffset,_,_] ->
            [ 
                { country_code, CountryCode },
                { id, TimeZoneId },
                { offset, bin_to_float(GMTOffset) }
            ]
    end.

geoname_parser( Line ) ->
    SplitLine = re:split(Line,"\t"),
    [
        BID,
        RealName,
        AsciiName,
        Aliases,
        BLat,
        BLong,
        <<"P">>,
        _,
        CC,
        _CC2,
        AdminCode1,
        AdminCode2,
        AdminCode3,
        AdminCode4,
        Population,
        Elevation,
        DigitalElevation,
        TimeZone,
        ModificationDate] = SplitLine,
    Lat = bin_to_float(BLat),
    Long = bin_to_float(BLong),

    [ 
        { id, BID },
        { name, RealName },
        { ascii_name, AsciiName},
        { aliases, re:split(Aliases,",")},
        { coords_latlon, {Lat, Long } },
        { country_code, CC },
        { adminCode1, AdminCode1 },
        { adminCode2, AdminCode2 },
        { adminCode3, AdminCode3 },
        { adminCode4, AdminCode4 },
        { population, bin_to_int(Population) },
        { elevation, bin_to_int(Elevation) },
        { dem, bin_to_int(DigitalElevation) },
        { timezone_id, TimeZone },
        { modification_date, to_date(chomp(ModificationDate)) }
    ].
    
    
to_date(Bin) ->
    list_to_tuple([ bin_to_int(S) || S <- split(Bin,"-") ] ).
    
    
chomp(Bin) ->
    [Head|_] = binary:split(Bin,<<"\n">>),
    Head.
    

country_parser( Line ) ->
    SplitLine = re:split(Line,"\t"),
    [
    Viso,
    Viso3,
    Viso_numeric,
    Vfips,
    Vname,
    Vcapital,
    Varea_sqkm,
    Vpopulation,
    Vcontinent,
    Vld,
    Vcurrency_code,
    Vcurrency_name,
    Vphone_code,
    Vpostal_code_format,
    Vpostal_code_regex,
    Vlanguages,
    Vgeoname_id,
    Vneighbours
    |_ ] = SplitLine,
    
    
    case re:run( Viso, "\\#" ) of
        {match,_} -> header;
        nomatch ->
            [
                { iso, Viso },
                { iso3,Viso3},
                { iso_numeric, Viso_numeric },
                { fips, Vfips },
                { name,  Vname },
                { capital, Vcapital },
                { area_sqkm, Varea_sqkm },
                { population, Vpopulation },
                { continent, Vcontinent },
                { tld,Vld },
                { currency_code, Vcurrency_code },
                { currency_name, Vcurrency_name },
                { phone_code, Vphone_code },
                { postal_code_format, Vpostal_code_format },
                { postal_code_regex,  Vpostal_code_regex },
                { languages, Vlanguages },
                { geoname_id, Vgeoname_id },
                { neighbours, split(Vneighbours,",") }
            ]
    end.


split(Target,Match) ->
    re:split(Target,Match).
    
bin_to_int([]) -> 0;
bin_to_int(Num) when is_binary(Num)->
    list_to_integer_(binary_to_list(Num) ).
  
list_to_integer_([]) -> 0;
list_to_integer_(T) -> list_to_integer(T).
  
bin_to_float(B) ->
  case re:run(B,"\\.") of
    nomatch -> float(list_to_integer(binary_to_list(B)));
    _       -> list_to_float(binary_to_list(B))
  end.
  
  
  rfile(File,LineParser,LineConsumer) ->
    
    case file:open(File, read) of
         {ok, Stream} ->   
                Results = handle(Stream, 1, [], LineParser,LineConsumer,[]),
                file:close(Stream),
                {ok,lists:reverse(Results) };
        Other ->
            Other
    end.
    
    
handle(Stream, LineNo, Buffer, LineParser,LineConsumer,Results) ->
  
  case io:get_line(Stream,"") of
    eof ->  
	       Results;
    Line -> 
	ConsumedLine = LineConsumer(LineParser(Line)),
	handle(Stream, LineNo+1, Buffer, LineParser,LineConsumer,[ConsumedLine|Results])
  end.