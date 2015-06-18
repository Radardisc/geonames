
-define( MSG(Fmt,Args), geonames_log:msg( "[~p] " ++ Fmt, [?MODULE|Args] ) ).
-define( WARN(Fmt,Args), geonames_log:warn( "[~p] " ++ Fmt, [?MODULE|Args] ) ).
-define( ERROR(Fmt,Args), geonames_log:error( "[~p] " ++ Fmt, [?MODULE|Args] ) ).


